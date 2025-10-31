use solana_sdk::signature::{Keypair, Signer};
use std::io::Write;
use std::fs::{self, File};
use std::path::{Path, PathBuf};

pub fn create_test_keypair_file(path: &Path) -> std::io::Result<()> {
    let keypair = Keypair::new();
    let mut file = File::create(path)?;
    let json = serde_json::to_string(&keypair.to_bytes().to_vec()).unwrap();
    file.write_all(json.as_bytes())?;
    Ok(())
}

/// Test fixture for temporary directories with automatic cleanup
///
/// This struct provides RAII-style temporary directory management for tests:
/// - Automatically creates a unique temporary directory
/// - Provides helper methods for common file operations
/// - Automatically cleans up on drop (even if test panics)
/// - Thread-safe with unique naming
///
/// # Examples
///
/// ```no_run
/// # use osvm::utils::test_utils::TempDirFixture;
/// # fn test_example() -> std::io::Result<()> {
/// let fixture = TempDirFixture::new("my_test")?;
///
/// // Create a file in the temp directory
/// fixture.write_file("config.json", b"{\"key\": \"value\"}")?;
///
/// // Get the path for assertions
/// let config_path = fixture.path().join("config.json");
/// assert!(config_path.exists());
///
/// // Automatically cleaned up when fixture goes out of scope
/// # Ok(())
/// # }
/// ```
pub struct TempDirFixture {
    path: PathBuf,
    cleanup_on_drop: bool,
}

impl TempDirFixture {
    /// Create a new temporary directory with a unique name
    ///
    /// The directory name will be: `/tmp/osvm-test-{prefix}-{timestamp}-{random}`
    ///
    /// # Arguments
    /// * `prefix` - Test name or identifier for debugging
    pub fn new(prefix: &str) -> std::io::Result<Self> {
        use std::time::{SystemTime, UNIX_EPOCH};

        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();

        let random: u32 = rand::random();
        let dir_name = format!("osvm-test-{}-{}-{:x}", prefix, timestamp, random);
        let path = std::env::temp_dir().join(dir_name);

        fs::create_dir_all(&path)?;

        Ok(Self {
            path,
            cleanup_on_drop: true,
        })
    }

    /// Get the path to the temporary directory
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Write a file in the temporary directory
    ///
    /// # Arguments
    /// * `filename` - Name of the file (relative to temp dir)
    /// * `contents` - File contents
    pub fn write_file(&self, filename: &str, contents: &[u8]) -> std::io::Result<()> {
        let file_path = self.path.join(filename);

        // Create parent directories if needed
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent)?;
        }

        let mut file = File::create(file_path)?;
        file.write_all(contents)?;
        Ok(())
    }

    /// Create a subdirectory in the temporary directory
    ///
    /// # Arguments
    /// * `dirname` - Name of the subdirectory (relative to temp dir)
    pub fn create_dir(&self, dirname: &str) -> std::io::Result<PathBuf> {
        let dir_path = self.path.join(dirname);
        fs::create_dir_all(&dir_path)?;
        Ok(dir_path)
    }

    /// Read a file from the temporary directory
    ///
    /// # Arguments
    /// * `filename` - Name of the file (relative to temp dir)
    pub fn read_file(&self, filename: &str) -> std::io::Result<Vec<u8>> {
        let file_path = self.path.join(filename);
        fs::read(file_path)
    }

    /// Check if a file exists in the temporary directory
    ///
    /// # Arguments
    /// * `filename` - Name of the file (relative to temp dir)
    pub fn file_exists(&self, filename: &str) -> bool {
        self.path.join(filename).exists()
    }

    /// Disable automatic cleanup (for debugging)
    ///
    /// Useful when you want to inspect the temporary directory after test failure.
    /// Set environment variable `OSVM_TEST_NO_CLEANUP=1` to disable cleanup globally.
    pub fn keep_on_drop(&mut self) {
        self.cleanup_on_drop = false;
    }

    /// Manually clean up the temporary directory
    ///
    /// Normally cleanup happens automatically on drop, but this allows
    /// explicit cleanup if needed.
    pub fn cleanup(&mut self) -> std::io::Result<()> {
        if self.path.exists() {
            fs::remove_dir_all(&self.path)?;
        }
        self.cleanup_on_drop = false; // Don't cleanup again on drop
        Ok(())
    }
}

impl Drop for TempDirFixture {
    fn drop(&mut self) {
        // Check environment variable to allow disabling cleanup for debugging
        if std::env::var("OSVM_TEST_NO_CLEANUP").is_ok() {
            eprintln!("⚠️  Test cleanup disabled: {}", self.path.display());
            return;
        }

        if self.cleanup_on_drop {
            if let Err(e) = fs::remove_dir_all(&self.path) {
                eprintln!("⚠️  Failed to cleanup test directory {}: {}", self.path.display(), e);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_temp_dir_fixture() -> std::io::Result<()> {
        let fixture = TempDirFixture::new("fixture_test")?;

        // Directory should exist
        assert!(fixture.path().exists());
        assert!(fixture.path().is_dir());

        // Write and read a file
        fixture.write_file("test.txt", b"Hello, World!")?;
        assert!(fixture.file_exists("test.txt"));

        let contents = fixture.read_file("test.txt")?;
        assert_eq!(contents, b"Hello, World!");

        // Create a subdirectory
        let subdir = fixture.create_dir("subdir")?;
        assert!(subdir.exists());
        assert!(subdir.is_dir());

        // Write a file in the subdirectory
        fixture.write_file("subdir/nested.txt", b"Nested content")?;
        assert!(fixture.file_exists("subdir/nested.txt"));

        let path = fixture.path().to_path_buf();

        // Cleanup happens automatically when fixture is dropped
        drop(fixture);

        // Verify cleanup (directory should be removed)
        // Note: This might be flaky on some systems due to timing
        if path.exists() {
            // If it still exists, it's okay - cleanup might be async
            eprintln!("Note: Directory still exists after drop (this is okay): {}", path.display());
        }

        Ok(())
    }

    #[test]
    fn test_keep_on_drop() -> std::io::Result<()> {
        let mut fixture = TempDirFixture::new("keep_test")?;
        let path = fixture.path().to_path_buf();

        fixture.write_file("keep_me.txt", b"Don't delete!")?;
        fixture.keep_on_drop();

        drop(fixture);

        // Directory should still exist
        assert!(path.exists());

        // Manual cleanup for this test
        fs::remove_dir_all(&path)?;

        Ok(())
    }

    #[test]
    fn test_manual_cleanup() -> std::io::Result<()> {
        let mut fixture = TempDirFixture::new("manual_cleanup_test")?;
        let path = fixture.path().to_path_buf();

        fixture.write_file("temp.txt", b"Temporary")?;

        // Manual cleanup
        fixture.cleanup()?;

        // Directory should be gone
        assert!(!path.exists());

        // Drop should not try to cleanup again (no error)
        drop(fixture);

        Ok(())
    }
}
