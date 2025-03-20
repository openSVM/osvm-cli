// A wrapper around webpki::Error to implement Debug, Display, and Error traits
#[derive(Debug)]
pub struct WebPkiError(pub webpki::Error);

impl std::fmt::Display for WebPkiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "WebPkiError: {:?}", self.0)
    }
}

impl std::error::Error for WebPkiError {}

// Conversion from webpki::Error to WebPkiError
impl From<webpki::Error> for WebPkiError {
    fn from(error: webpki::Error) -> Self {
        WebPkiError(error)
    }
}
