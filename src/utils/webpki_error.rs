//! Wrapper for webpki::Error to implement std::error::Error trait

use std::fmt;

/// Wrapper for webpki::Error to implement std::error::Error trait
#[derive(Debug)]
pub struct WebPkiError(pub webpki::Error);

impl fmt::Display for WebPkiError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "WebPKI error: {:?}", self.0)
    }
}

impl std::error::Error for WebPkiError {}

impl From<webpki::Error> for WebPkiError {
    fn from(err: webpki::Error) -> Self {
        WebPkiError(err)
    }
}
