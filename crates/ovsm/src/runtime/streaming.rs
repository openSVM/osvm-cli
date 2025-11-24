/// Streaming support for OVSM LISP
///
/// This module provides built-in functions for real-time blockchain event streaming via WebSocket:
/// - `(stream-connect url :programs ["pumpfun"] :tokens ["USDC"])` - Connect to WebSocket stream
/// - `(stream-poll stream-id :limit 50)` - Poll buffered events (non-blocking)
/// - `(stream-wait stream-id :timeout 30)` - Wait for next event (blocking with timeout)
/// - `(stream-close stream-id)` - Close WebSocket connection
///
/// Example usage:
/// ```lisp
/// ;; Connect to Pump.fun event stream via WebSocket
/// (define stream (stream-connect "ws://localhost:8080/ws" :programs ["pumpfun"]))
///
/// ;; Poll for events in a loop
/// (while true
///   (define events (stream-poll stream :limit 50))
///   (for (event events)
///     (if (= (get event "type") "token_transfer")
///         (log :message "Transfer:" :value (get event "amount"))
///         null)))
/// ```

use crate::error::{Error, Result};
use crate::runtime::Value;
use futures_util::{SinkExt, StreamExt};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use tokio_tungstenite::{connect_async, tungstenite::protocol::Message};

/// Stream connection handle
#[derive(Clone, Debug)]
pub struct StreamHandle {
    pub id: String,
    pub url: String,
    pub filters: StreamFilters,
    pub event_buffer: Arc<Mutex<Vec<JsonValue>>>,
    pub is_connected: Arc<Mutex<bool>>,
}

/// Stream filtering options
#[derive(Clone, Debug, Default)]
pub struct StreamFilters {
    pub programs: Vec<String>,
    pub tokens: Vec<String>,
    pub accounts: Vec<String>,
    pub event_types: Vec<String>,
    pub success_only: bool,
}

lazy_static::lazy_static! {
    /// Global stream registry (stores active streams)
    static ref STREAM_REGISTRY: Arc<Mutex<HashMap<String, StreamHandle>>> =
        Arc::new(Mutex::new(HashMap::new()));
}

/// Generate unique stream ID
fn generate_stream_id() -> String {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("stream_{}", id)
}

/// Connect to WebSocket streaming server
///
/// Syntax: `(stream-connect url &key programs tokens accounts event-types success-only)`
///
/// Parameters:
/// - `url`: WebSocket URL (e.g., "ws://localhost:8080/ws")
/// - `:programs` (optional): Array of program aliases or IDs
/// - `:tokens` (optional): Array of token symbols or mint addresses
/// - `:accounts` (optional): Array of account addresses
/// - `:event-types` (optional): Array of event type strings
/// - `:success-only` (optional): Boolean, filter only successful transactions
///
/// Returns: Stream ID string for use with other stream-* functions
pub fn stream_connect(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::runtime(
            "stream-connect requires at least URL argument".to_string(),
        ));
    }

    // Extract URL
    let url = match &args[0] {
        Value::String(s) => s.clone(),
        _ => return Err(Error::runtime("stream-connect: URL must be a string".to_string())),
    };

    // Parse keyword arguments
    let mut filters = StreamFilters::default();
    let mut i = 1;
    while i < args.len() {
        if let Value::String(key) = &args[i] {
            if key.starts_with(':') {
                if i + 1 >= args.len() {
                    return Err(Error::runtime(format!(
                        "stream-connect: missing value for keyword argument {}",
                        key
                    )));
                }

                let value = &args[i + 1];
                match key.as_str() {
                    ":programs" => {
                        filters.programs = extract_string_array(value)?;
                    }
                    ":tokens" => {
                        filters.tokens = extract_string_array(value)?;
                    }
                    ":accounts" => {
                        filters.accounts = extract_string_array(value)?;
                    }
                    ":event-types" => {
                        filters.event_types = extract_string_array(value)?;
                    }
                    ":success-only" => {
                        filters.success_only = value.is_truthy();
                    }
                    _ => {
                        return Err(Error::runtime(format!(
                            "stream-connect: unknown keyword argument {}",
                            key
                        )))
                    }
                }
                i += 2;
            } else {
                i += 1;
            }
        } else {
            i += 1;
        }
    }

    // Create stream handle
    let stream_id = generate_stream_id();
    let event_buffer = Arc::new(Mutex::new(Vec::new()));
    let is_connected = Arc::new(Mutex::new(true));

    let handle = StreamHandle {
        id: stream_id.clone(),
        url: url.clone(),
        filters: filters.clone(),
        event_buffer: event_buffer.clone(),
        is_connected: is_connected.clone(),
    };

    // Register stream
    {
        let mut registry = STREAM_REGISTRY.lock().unwrap();
        registry.insert(stream_id.clone(), handle.clone());
    }

    // Start WebSocket connection in background thread
    let url_clone = url.clone();
    let buffer_clone = event_buffer.clone();
    let connected_clone = is_connected.clone();
    let filters_clone = filters.clone();

    thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async move {
            if let Err(e) = websocket_client_loop(
                &url_clone,
                buffer_clone,
                connected_clone,
                filters_clone,
            )
            .await
            {
                eprintln!("WebSocket error: {}", e);
            }
        });
    });

    // Wait a bit for connection to establish
    thread::sleep(Duration::from_millis(500));

    Ok(Value::String(stream_id))
}

/// WebSocket client loop (runs in background)
async fn websocket_client_loop(
    url: &str,
    event_buffer: Arc<Mutex<Vec<JsonValue>>>,
    is_connected: Arc<Mutex<bool>>,
    filters: StreamFilters,
) -> Result<()> {
    let (ws_stream, _) = connect_async(url)
        .await
        .map_err(|e| Error::runtime(format!("WebSocket connection failed: {}", e)))?;

    let (_write, mut read) = ws_stream.split();

    while let Some(message) = read.next().await {
        match message {
            Ok(Message::Text(text)) => {
                // Parse JSON event
                if let Ok(json_value) = serde_json::from_str::<JsonValue>(&text) {
                    // Apply filters
                    if filter_event(&json_value, &filters) {
                        // Add to buffer
                        let mut buffer = event_buffer.lock().unwrap();
                        buffer.push(json_value);

                        // Limit buffer size to prevent memory issues
                        if buffer.len() > 10000 {
                            buffer.drain(0..5000); // Remove oldest 5000 events
                        }
                    }
                }
            }
            Ok(Message::Close(_)) => {
                let mut connected = is_connected.lock().unwrap();
                *connected = false;
                break;
            }
            Err(e) => {
                eprintln!("WebSocket read error: {}", e);
                let mut connected = is_connected.lock().unwrap();
                *connected = false;
                break;
            }
            _ => {}
        }
    }

    Ok(())
}

/// Poll for new events (non-blocking)
///
/// Syntax: `(stream-poll stream-id &key limit)`
///
/// Parameters:
/// - `stream-id`: Stream ID returned from stream-connect
/// - `:limit` (optional): Maximum number of events to return (default: 100)
///
/// Returns: Array of event objects
pub fn stream_poll(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::runtime("stream-poll requires stream-id argument".to_string()));
    }

    let stream_id = match &args[0] {
        Value::String(s) => s.clone(),
        _ => return Err(Error::runtime("stream-poll: stream-id must be a string".to_string())),
    };

    // Parse limit keyword argument
    let mut limit = 100;
    if args.len() >= 3 {
        if let Value::String(key) = &args[1] {
            if key == ":limit" {
                match &args[2] {
                    Value::Int(n) => limit = *n as usize,
                    Value::Float(f) => limit = *f as usize,
                    _ => {}
                }
            }
        }
    }

    // Get stream handle
    let handle = {
        let registry = STREAM_REGISTRY.lock().unwrap();
        registry
            .get(&stream_id)
            .cloned()
            .ok_or_else(|| Error::runtime(format!("stream-poll: stream not found: {}", stream_id)))?
    };

    // Check if still connected
    {
        let connected = handle.is_connected.lock().unwrap();
        if !*connected {
            return Err(Error::runtime("stream-poll: WebSocket connection closed".to_string()));
        }
    }

    // Drain events from buffer
    let events = {
        let mut buffer = handle.event_buffer.lock().unwrap();
        let drain_count = buffer.len().min(limit);
        buffer.drain(0..drain_count).collect::<Vec<_>>()
    };

    // Convert events to OVSM Value array
    let event_values: Vec<Value> = events
        .into_iter()
        .map(|json_val| json_to_value(&json_val))
        .collect();

    Ok(Value::Array(Arc::new(event_values)))
}

/// Wait for next event (blocking with timeout)
///
/// Syntax: `(stream-wait stream-id &key timeout)`
///
/// Parameters:
/// - `stream-id`: Stream ID returned from stream-connect
/// - `:timeout` (optional): Timeout in seconds (default: 30)
///
/// Returns: Event object or null if timeout
pub fn stream_wait(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::runtime("stream-wait requires stream-id argument".to_string()));
    }

    let stream_id = match &args[0] {
        Value::String(s) => s.clone(),
        _ => return Err(Error::runtime("stream-wait: stream-id must be a string".to_string())),
    };

    // Parse timeout keyword argument
    let mut timeout_secs = 30;
    if args.len() >= 3 {
        if let Value::String(key) = &args[1] {
            if key == ":timeout" {
                match &args[2] {
                    Value::Int(n) => timeout_secs = *n as u64,
                    Value::Float(f) => timeout_secs = *f as u64,
                    _ => {}
                }
            }
        }
    }

    // Get stream handle
    let handle = {
        let registry = STREAM_REGISTRY.lock().unwrap();
        registry
            .get(&stream_id)
            .cloned()
            .ok_or_else(|| Error::runtime(format!("stream-wait: stream not found: {}", stream_id)))?
    };

    // Wait for event with timeout
    let start = std::time::Instant::now();
    let timeout_duration = Duration::from_secs(timeout_secs);

    while start.elapsed() < timeout_duration {
        // Check buffer
        {
            let mut buffer = handle.event_buffer.lock().unwrap();
            if !buffer.is_empty() {
                let event = buffer.remove(0);
                return Ok(json_to_value(&event));
            }
        }

        // Check if still connected
        {
            let connected = handle.is_connected.lock().unwrap();
            if !*connected {
                return Err(Error::runtime("stream-wait: WebSocket connection closed".to_string()));
            }
        }

        // Sleep briefly before checking again
        thread::sleep(Duration::from_millis(100));
    }

    // Timeout - return null
    Ok(Value::Null)
}

/// Close streaming connection
///
/// Syntax: `(stream-close stream-id)`
///
/// Parameters:
/// - `stream-id`: Stream ID returned from stream-connect
///
/// Returns: Boolean indicating success
pub fn stream_close(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::runtime("stream-close requires stream-id argument".to_string()));
    }

    let stream_id = match &args[0] {
        Value::String(s) => s.clone(),
        _ => return Err(Error::runtime("stream-close: stream-id must be a string".to_string())),
    };

    // Remove from registry
    let removed = {
        let mut registry = STREAM_REGISTRY.lock().unwrap();
        registry.remove(&stream_id).is_some()
    };

    Ok(Value::Bool(removed))
}

/// Helper: Extract string array from Value
fn extract_string_array(value: &Value) -> Result<Vec<String>> {
    match value {
        Value::Array(arr) => {
            let mut strings = Vec::new();
            for item in arr.iter() {
                match item {
                    Value::String(s) => strings.push(s.clone()),
                    _ => {
                        return Err(Error::runtime(
                            "stream-connect: array elements must be strings".to_string(),
                        ))
                    }
                }
            }
            Ok(strings)
        }
        _ => Err(Error::runtime(
            "stream-connect: filter value must be an array".to_string(),
        )),
    }
}

/// Helper: Filter event based on StreamFilters
fn filter_event(event: &JsonValue, filters: &StreamFilters) -> bool {
    // Filter by event type
    if !filters.event_types.is_empty() {
        if let Some(event_type) = event.get("type").and_then(|v| v.as_str()) {
            if !filters.event_types.iter().any(|t| t == event_type) {
                return false;
            }
        } else {
            return false;
        }
    }

    // Filter by success_only
    if filters.success_only {
        if let Some(success) = event.get("success").and_then(|v| v.as_bool()) {
            if !success {
                return false;
            }
        }
    }

    // Note: Program/token/account filtering should be done server-side
    // These filters are for client-side double-checking if needed

    true
}

/// Helper: Convert serde_json::Value to OVSM Value
fn json_to_value(json: &JsonValue) -> Value {
    match json {
        JsonValue::Null => Value::Null,
        JsonValue::Bool(b) => Value::Bool(*b),
        JsonValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::Null
            }
        }
        JsonValue::String(s) => Value::String(s.clone()),
        JsonValue::Array(arr) => {
            let values: Vec<Value> = arr.iter().map(json_to_value).collect();
            Value::Array(Arc::new(values))
        }
        JsonValue::Object(obj) => {
            let mut map = HashMap::new();
            for (k, v) in obj.iter() {
                map.insert(k.clone(), json_to_value(v));
            }
            Value::Object(Arc::new(map))
        }
    }
}
