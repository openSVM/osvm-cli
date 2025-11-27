//! Investigation database for persistent forensic analysis tracking

use anyhow::{Result, Context};
use rusqlite::{Connection, params, OptionalExtension};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use chrono::{DateTime, Utc};

/// Investigation record with metadata and findings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Investigation {
    pub id: Option<i64>,
    pub wallet_address: String,
    pub started_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
    pub risk_score: f64,
    pub risk_level: String,
    pub behavior_type: String,
    pub node_count: usize,
    pub edge_count: usize,
    pub depth_reached: usize,
    pub alerts: Vec<String>,
    pub reasons: Vec<String>,
    pub notes: Option<String>,
}

/// Alert record for high-priority findings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Alert {
    pub id: Option<i64>,
    pub investigation_id: i64,
    pub alert_type: String,  // "rapid_transfer", "circular_flow", "mixer_detected"
    pub severity: String,     // "Critical", "High", "Medium", "Low"
    pub message: String,
    pub timestamp: DateTime<Utc>,
    pub metadata: Option<String>,  // JSON for additional data
}

/// Wallet history entry for longitudinal analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WalletHistory {
    pub wallet_address: String,
    pub first_seen: DateTime<Utc>,
    pub last_investigated: DateTime<Utc>,
    pub investigation_count: usize,
    pub current_risk_score: f64,
    pub current_behavior: String,
    pub risk_trend: String,  // "increasing", "decreasing", "stable"
}

pub struct InvestigationDB {
    conn: Connection,
}

impl InvestigationDB {
    /// Open or create the investigation database
    pub fn open() -> Result<Self> {
        let db_path = Self::db_path()?;

        // Ensure parent directory exists
        if let Some(parent) = db_path.parent() {
            std::fs::create_dir_all(parent)
                .context("Failed to create database directory")?;
        }

        let conn = Connection::open(&db_path)
            .context("Failed to open investigation database")?;

        let mut db = Self { conn };
        db.init_schema()?;

        Ok(db)
    }

    /// Get database file path
    fn db_path() -> Result<PathBuf> {
        let home = dirs::home_dir()
            .context("Failed to get home directory")?;

        Ok(home.join(".osvm").join("investigations.db"))
    }

    /// Open investigation database at a specific path (useful for testing)
    #[cfg(test)]
    pub fn open_path(path: &std::path::Path) -> Result<Self> {
        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .context("Failed to create database directory")?;
        }

        let conn = Connection::open(path)
            .context("Failed to open investigation database")?;

        let mut db = Self { conn };
        db.init_schema()?;

        Ok(db)
    }

    /// Initialize database schema
    fn init_schema(&mut self) -> Result<()> {
        self.conn.execute_batch(
            r#"
            CREATE TABLE IF NOT EXISTS investigations (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                wallet_address TEXT NOT NULL,
                started_at TEXT NOT NULL,
                completed_at TEXT,
                risk_score REAL NOT NULL,
                risk_level TEXT NOT NULL,
                behavior_type TEXT NOT NULL,
                node_count INTEGER NOT NULL,
                edge_count INTEGER NOT NULL,
                depth_reached INTEGER NOT NULL,
                alerts TEXT,  -- JSON array
                reasons TEXT,  -- JSON array
                notes TEXT
            );

            CREATE TABLE IF NOT EXISTS alerts (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                investigation_id INTEGER NOT NULL,
                alert_type TEXT NOT NULL,
                severity TEXT NOT NULL,
                message TEXT NOT NULL,
                timestamp TEXT NOT NULL,
                metadata TEXT,
                FOREIGN KEY (investigation_id) REFERENCES investigations(id)
            );

            CREATE TABLE IF NOT EXISTS wallet_history (
                wallet_address TEXT PRIMARY KEY,
                first_seen TEXT NOT NULL,
                last_investigated TEXT NOT NULL,
                investigation_count INTEGER NOT NULL,
                current_risk_score REAL NOT NULL,
                current_behavior TEXT NOT NULL,
                risk_trend TEXT NOT NULL
            );

            CREATE INDEX IF NOT EXISTS idx_investigations_wallet
                ON investigations(wallet_address);
            CREATE INDEX IF NOT EXISTS idx_investigations_risk
                ON investigations(risk_level, risk_score DESC);
            CREATE INDEX IF NOT EXISTS idx_investigations_started
                ON investigations(started_at DESC);
            CREATE INDEX IF NOT EXISTS idx_alerts_investigation
                ON alerts(investigation_id);
            CREATE INDEX IF NOT EXISTS idx_alerts_severity
                ON alerts(severity, timestamp DESC);
            "#
        ).context("Failed to initialize database schema")?;

        Ok(())
    }

    /// Save a new investigation
    pub fn save_investigation(&mut self, inv: &Investigation) -> Result<i64> {
        let alerts_json = serde_json::to_string(&inv.alerts)?;
        let reasons_json = serde_json::to_string(&inv.reasons)?;

        self.conn.execute(
            r#"
            INSERT INTO investigations
            (wallet_address, started_at, completed_at, risk_score, risk_level,
             behavior_type, node_count, edge_count, depth_reached, alerts, reasons, notes)
            VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12)
            "#,
            params![
                inv.wallet_address,
                inv.started_at.to_rfc3339(),
                inv.completed_at.map(|dt| dt.to_rfc3339()),
                inv.risk_score,
                inv.risk_level,
                inv.behavior_type,
                inv.node_count as i64,
                inv.edge_count as i64,
                inv.depth_reached as i64,
                alerts_json,
                reasons_json,
                inv.notes,
            ]
        )?;

        let id = self.conn.last_insert_rowid();

        // Update wallet history
        self.update_wallet_history(&inv.wallet_address, inv.risk_score, &inv.behavior_type)?;

        Ok(id)
    }

    /// Save an alert
    pub fn save_alert(&mut self, alert: &Alert) -> Result<i64> {
        self.conn.execute(
            r#"
            INSERT INTO alerts
            (investigation_id, alert_type, severity, message, timestamp, metadata)
            VALUES (?1, ?2, ?3, ?4, ?5, ?6)
            "#,
            params![
                alert.investigation_id,
                alert.alert_type,
                alert.severity,
                alert.message,
                alert.timestamp.to_rfc3339(),
                alert.metadata,
            ]
        )?;

        Ok(self.conn.last_insert_rowid())
    }

    /// Get investigation history for a wallet
    pub fn get_wallet_investigations(&self, wallet: &str) -> Result<Vec<Investigation>> {
        let mut stmt = self.conn.prepare(
            r#"
            SELECT id, wallet_address, started_at, completed_at, risk_score, risk_level,
                   behavior_type, node_count, edge_count, depth_reached, alerts, reasons, notes
            FROM investigations
            WHERE wallet_address = ?1
            ORDER BY started_at DESC
            "#
        )?;

        let invs = stmt.query_map([wallet], |row| {
            let alerts_json: String = row.get(10)?;
            let reasons_json: String = row.get(11)?;

            Ok(Investigation {
                id: Some(row.get(0)?),
                wallet_address: row.get(1)?,
                started_at: DateTime::parse_from_rfc3339(&row.get::<_, String>(2)?)
                    .unwrap().with_timezone(&Utc),
                completed_at: row.get::<_, Option<String>>(3)?
                    .and_then(|s: String| DateTime::parse_from_rfc3339(&s).ok())
                    .map(|dt| dt.with_timezone(&Utc)),
                risk_score: row.get(4)?,
                risk_level: row.get(5)?,
                behavior_type: row.get(6)?,
                node_count: row.get::<_, i64>(7)? as usize,
                edge_count: row.get::<_, i64>(8)? as usize,
                depth_reached: row.get::<_, i64>(9)? as usize,
                alerts: serde_json::from_str(&alerts_json).unwrap_or_default(),
                reasons: serde_json::from_str(&reasons_json).unwrap_or_default(),
                notes: row.get(12)?,
            })
        })?;

        invs.collect::<Result<Vec<_>, _>>().map_err(Into::into)
    }

    /// Get all high-risk wallets
    pub fn get_high_risk_wallets(&self, min_score: f64) -> Result<Vec<Investigation>> {
        let mut stmt = self.conn.prepare(
            r#"
            SELECT id, wallet_address, started_at, completed_at, risk_score, risk_level,
                   behavior_type, node_count, edge_count, depth_reached, alerts, reasons, notes
            FROM investigations
            WHERE risk_score >= ?1
            ORDER BY risk_score DESC, started_at DESC
            "#
        )?;

        let invs = stmt.query_map([min_score], |row| {
            let alerts_json: String = row.get(10)?;
            let reasons_json: String = row.get(11)?;

            Ok(Investigation {
                id: Some(row.get(0)?),
                wallet_address: row.get(1)?,
                started_at: DateTime::parse_from_rfc3339(&row.get::<_, String>(2)?)
                    .unwrap().with_timezone(&Utc),
                completed_at: row.get::<_, Option<String>>(3)?
                    .and_then(|s: String| DateTime::parse_from_rfc3339(&s).ok())
                    .map(|dt| dt.with_timezone(&Utc)),
                risk_score: row.get(4)?,
                risk_level: row.get(5)?,
                behavior_type: row.get(6)?,
                node_count: row.get::<_, i64>(7)? as usize,
                edge_count: row.get::<_, i64>(8)? as usize,
                depth_reached: row.get::<_, i64>(9)? as usize,
                alerts: serde_json::from_str(&alerts_json).unwrap_or_default(),
                reasons: serde_json::from_str(&reasons_json).unwrap_or_default(),
                notes: row.get(12)?,
            })
        })?;

        invs.collect::<Result<Vec<_>, _>>().map_err(Into::into)
    }

    /// Update wallet history
    fn update_wallet_history(&mut self, wallet: &str, risk_score: f64, behavior: &str) -> Result<()> {
        let now = Utc::now();

        // Get previous investigation for trend analysis
        let prev_score: Option<f64> = self.conn.query_row(
            "SELECT risk_score FROM investigations WHERE wallet_address = ?1 ORDER BY started_at DESC LIMIT 1 OFFSET 1",
            [wallet],
            |row| row.get(0)
        ).ok();

        let risk_trend = if let Some(prev) = prev_score {
            if risk_score > prev + 10.0 {
                "increasing"
            } else if risk_score < prev - 10.0 {
                "decreasing"
            } else {
                "stable"
            }
        } else {
            "new"
        };

        // Upsert wallet history
        self.conn.execute(
            r#"
            INSERT INTO wallet_history
            (wallet_address, first_seen, last_investigated, investigation_count,
             current_risk_score, current_behavior, risk_trend)
            VALUES (?1, ?2, ?2, 1, ?3, ?4, ?5)
            ON CONFLICT(wallet_address) DO UPDATE SET
                last_investigated = ?2,
                investigation_count = investigation_count + 1,
                current_risk_score = ?3,
                current_behavior = ?4,
                risk_trend = ?5
            "#,
            params![wallet, now.to_rfc3339(), risk_score, behavior, risk_trend]
        )?;

        Ok(())
    }

    /// Get wallet history summary
    pub fn get_wallet_history(&self, wallet: &str) -> Result<Option<WalletHistory>> {
        self.conn.query_row(
            r#"
            SELECT wallet_address, first_seen, last_investigated, investigation_count,
                   current_risk_score, current_behavior, risk_trend
            FROM wallet_history
            WHERE wallet_address = ?1
            "#,
            [wallet],
            |row| {
                Ok(WalletHistory {
                    wallet_address: row.get(0)?,
                    first_seen: DateTime::parse_from_rfc3339(&row.get::<_, String>(1)?)
                        .unwrap().with_timezone(&Utc),
                    last_investigated: DateTime::parse_from_rfc3339(&row.get::<_, String>(2)?)
                        .unwrap().with_timezone(&Utc),
                    investigation_count: row.get::<_, i64>(3)? as usize,
                    current_risk_score: row.get(4)?,
                    current_behavior: row.get(5)?,
                    risk_trend: row.get(6)?,
                })
            }
        ).optional().map_err(Into::into)
    }

    /// Get recent investigations summary
    pub fn get_recent_summary(&self, limit: usize) -> Result<Vec<Investigation>> {
        let mut stmt = self.conn.prepare(
            r#"
            SELECT id, wallet_address, started_at, completed_at, risk_score, risk_level,
                   behavior_type, node_count, edge_count, depth_reached, alerts, reasons, notes
            FROM investigations
            ORDER BY started_at DESC
            LIMIT ?1
            "#
        )?;

        let invs = stmt.query_map([limit as i64], |row| {
            let alerts_json: String = row.get(10)?;
            let reasons_json: String = row.get(11)?;

            Ok(Investigation {
                id: Some(row.get(0)?),
                wallet_address: row.get(1)?,
                started_at: DateTime::parse_from_rfc3339(&row.get::<_, String>(2)?)
                    .unwrap().with_timezone(&Utc),
                completed_at: row.get::<_, Option<String>>(3)?
                    .and_then(|s: String| DateTime::parse_from_rfc3339(&s).ok())
                    .map(|dt| dt.with_timezone(&Utc)),
                risk_score: row.get(4)?,
                risk_level: row.get(5)?,
                behavior_type: row.get(6)?,
                node_count: row.get::<_, i64>(7)? as usize,
                edge_count: row.get::<_, i64>(8)? as usize,
                depth_reached: row.get::<_, i64>(9)? as usize,
                alerts: serde_json::from_str(&alerts_json).unwrap_or_default(),
                reasons: serde_json::from_str(&reasons_json).unwrap_or_default(),
                notes: row.get(12)?,
            })
        })?;

        invs.collect::<Result<Vec<_>, _>>().map_err(Into::into)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn setup_test_db() -> (InvestigationDB, TempDir) {
        let tmp_dir = TempDir::new().expect("Failed to create temp dir");
        let db_path = tmp_dir.path().join("test_investigations.db");
        let db = InvestigationDB::open_path(&db_path).expect("Failed to open test db");
        (db, tmp_dir)
    }

    #[test]
    fn test_database_operations() {
        let (mut db, _tmp_dir) = setup_test_db();

        let inv = Investigation {
            id: None,
            wallet_address: "TestWallet123".to_string(),
            started_at: Utc::now(),
            completed_at: Some(Utc::now()),
            risk_score: 75.0,
            risk_level: "High".to_string(),
            behavior_type: "Bot".to_string(),
            node_count: 50,
            edge_count: 150,
            depth_reached: 3,
            alerts: vec!["Test alert".to_string()],
            reasons: vec!["Test reason".to_string()],
            notes: Some("Test notes".to_string()),
        };

        let id = db.save_investigation(&inv).unwrap();
        assert!(id > 0);

        let history = db.get_wallet_investigations("TestWallet123").unwrap();
        assert_eq!(history.len(), 1);
        assert_eq!(history[0].risk_score, 75.0);
    }
}
