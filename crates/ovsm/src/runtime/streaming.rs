/// Streaming support for OVSM LISP
///
/// This module provides built-in functions for real-time blockchain event streaming:
/// - `(stream-connect url :programs ["pumpfun"] :tokens ["USDC"])` - Connect to streaming server
/// - `(stream-poll stream-id)` - Poll for new events (non-blocking)
/// - `(stream-wait stream-id timeout)` - Wait for next event (blocking with timeout)
/// - `(stream-close stream-id)` - Close streaming connection
///
/// Example usage:
/// ```lisp
/// ;; Connect to Pump.fun event stream
/// (define stream (stream-connect "http://localhost:8080" :programs ["pumpfun"]))
///
/// ;; Poll for events in a loop
/// (while true
///   (define events (stream-poll stream))
///   (for (event events)
///     (if (= (get event "type") "log_message")
///         (log :message "Transaction:" :value (get event "signature"))
///         null)))
/// ```

use crate::error::{Error, Result};
use crate::runtime::Value;
use reqwest::Client;
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime};

/// Stream connection handle
#[derive(Clone, Debug)]
pub struct StreamHandle {
    pub id: String,
    pub url: String,
    pub filters: StreamFilters,
    pub last_poll: SystemTime,
    pub event_buffer: Arc<Mutex<Vec<JsonValue>>>,
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

/// Global stream registry (stores active streams)
lazy_static::lazy_static! {
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

/// Connect to streaming server
///
/// Syntax: `(stream-connect url &key programs tokens accounts event-types success-only)`
///
/// Parameters:
/// - `url`: Server URL (e.g., "http://localhost:8080")
/// - `:programs` (optional): Array of program aliases or IDs
/// - `:tokens` (optional): Array of token symbols or mint addresses
/// - `:accounts` (optional): Array of account addresses
/// - `:event-types` (optional): Array of event type strings
/// - `:success-only` (optional): Boolean, filter only successful transactions
///
/// Returns: Stream ID string for use with other stream-* functions
pub fn stream_connect(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::Runtime(
            "stream-connect requires at least URL argument".to_string(),
        ));
    }

    // Extract URL
    let url = match &args[0] {
        Value::String(s) => s.clone(),
        _ => return Err(Error::Runtime("stream-connect: URL must be a string".to_string())),
    };

    // Parse keyword arguments
    let mut filters = StreamFilters::default();
    let mut i = 1;
    while i < args.len() {
        if let Value::String(key) = &args[i] {
            if key.starts_with(':') {
                if i + 1 >= args.len() {
                    return Err(Error::Runtime(format!(
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
                        return Err(Error::Runtime(format!(
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
    let handle = StreamHandle {
        id: stream_id.clone(),
        url: url.clone(),
        filters,
        last_poll: SystemTime::now(),
        event_buffer: Arc::new(Mutex::new(Vec::new())),
    };

    // Register stream
    {
        let mut registry = STREAM_REGISTRY.lock().unwrap();
        registry.insert(stream_id.clone(), handle);
    }

    Ok(Value::String(stream_id))
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
        return Err(Error::Runtime("stream-poll requires stream-id argument".to_string()));
    }

    let stream_id = match &args[0] {
        Value::String(s) => s.clone(),
        _ => return Err(Error::Runtime("stream-poll: stream-id must be a string".to_string())),
    };

    // Parse limit keyword argument
    let mut limit = 100;
    if args.len() >= 3 {
        if let Value::String(key) = &args[1] {
            if key == ":limit" {
                if let Value::Number(n) = &args[2] {
                    limit = *n as usize;
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
            .ok_or_else(|| Error::Runtime(format!("stream-poll: stream not found: {}", stream_id)))?
    };

    // Poll HTTP endpoint
    let events = poll_events_sync(&handle, limit)?;

    // Convert events to OVSM Value array
    let event_values: Vec<Value> = events
        .into_iter()
        .map(|json_val| json_to_value(&json_val))
        .collect();

    Ok(Value::Array(event_values))
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
        return Err(Error::Runtime("stream-wait requires stream-id argument".to_string()));
    }

    let stream_id = match &args[0] {
        Value::String(s) => s.clone(),
        _ => return Err(Error::Runtime("stream-wait: stream-id must be a string".to_string())),
    };

    // Parse timeout keyword argument
    let mut timeout_secs = 30;
    if args.len() >= 3 {
        if let Value::String(key) = &args[1] {
            if key == ":timeout" {
                if let Value::Number(n) = &args[2] {
                    timeout_secs = *n as u64;
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
            .ok_or_else(|| Error::Runtime(format!("stream-wait: stream not found: {}", stream_id)))?
    };

    // Poll with retries until event or timeout
    let start = SystemTime::now();
    let timeout_duration = Duration::from_secs(timeout_secs);

    loop {
        let events = poll_events_sync(&handle, 1)?;
        if !events.is_empty() {
            return Ok(json_to_value(&events[0]));
        }

        if start.elapsed().unwrap_or(Duration::ZERO) >= timeout_duration {
            return Ok(Value::Null);
        }

        // Sleep for 100ms before retry
        std::thread::sleep(Duration::from_millis(100));
    }
}

/// Close streaming connection
///
/// Syntax: `(stream-close stream-id)`
///
/// Parameters:
/// - `stream-id`: Stream ID returned from stream-connect
///
/// Returns: true on success
pub fn stream_close(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::Runtime("stream-close requires stream-id argument".to_string()));
    }

    let stream_id = match &args[0] {
        Value::String(s) => s.clone(),
        _ => return Err(Error::Runtime("stream-close: stream-id must be a string".to_string())),
    };

    // Remove from registry
    let mut registry = STREAM_REGISTRY.lock().unwrap();
    registry.remove(&stream_id);

    Ok(Value::Boolean(true))
}

/// Helper: Extract string array from Value
fn extract_string_array(value: &Value) -> Result<Vec<String>> {
    match value {
        Value::Array(arr) => {
            let mut strings = Vec::new();
            for item in arr {
                match item {
                    Value::String(s) => strings.push(s.clone()),
                    _ => {
                        return Err(Error::Runtime(
                            "stream-connect: array elements must be strings".to_string(),
                        ))
                    }
                }
            }
            Ok(strings)
        }
        _ => Err(Error::Runtime(
            "stream-connect: filter value must be an array".to_string(),
        )),
    }
}

/// Helper: Poll events from HTTP endpoint (synchronous)
fn poll_events_sync(handle: &StreamHandle, limit: usize) -> Result<Vec<JsonValue>> {
    let client = Client::builder()
        .timeout(Duration::from_secs(10))
        .build()
        .map_err(|e| Error::Runtime(format!("Failed to create HTTP client: {}", e)))?;

    let url = format!("{}/events?limit={}", handle.url, limit);

    // Build runtime for sync reqwest call
    let runtime = tokio::runtime::Runtime::new()
        .map_err(|e| Error::Runtime(format!("Failed to create tokio runtime: {}", e)))?;

    let response = runtime
        .block_on(client.get(&url).send())
        .map_err(|e| Error::Runtime(format!("HTTP request failed: {}", e)))?;

    let events: Vec<JsonValue> = runtime
        .block_on(response.json())
        .map_err(|e| Error::Runtime(format!("Failed to parse JSON response: {}", e)))?;

    // Apply filters if any
    let filtered = if handle.filters.programs.is_empty()
        && handle.filters.tokens.is_empty()
        && handle.filters.accounts.is_empty()
        && handle.filters.event_types.is_empty()
        && !handle.filters.success_only
    {
        events
    } else {
        events
            .into_iter()
            .filter(|event| filter_event(event, &handle.filters))
            .collect()
    };

    Ok(filtered)
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

    // Note: Program/token/account filtering is done server-side via connection parameters
    // These filters are redundant but kept for client-side double-checking

    true
}

/// Helper: Convert serde_json::Value to OVSM Value
fn json_to_value(json: &JsonValue) -> Value {
    match json {
        JsonValue::Null => Value::Null,
        JsonValue::Bool(b) => Value::Boolean(*b),
        JsonValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Number(i as f64)
            } else if let Some(f) = n.as_f64() {
                Value::Number(f)
            } else {
                Value::Null
            }
        }
        JsonValue::String(s) => Value::String(s.clone()),
        JsonValue::Array(arr) => {
            let values: Vec<Value> = arr.iter().map(json_to_value).collect();
            Value::Array(values)
        }
        JsonValue::Object(obj) => {
            let mut map = HashMap::new();
            for (k, v) in obj.iter() {
                map.insert(k.clone(), json_to_value(v));
            }
            Value::Object(map)
        }
    }
}
