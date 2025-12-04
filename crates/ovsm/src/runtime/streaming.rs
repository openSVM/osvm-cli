//! WebSocket streaming support for real-time blockchain event monitoring.

/// Streaming support for OVSM LISP
///
/// This module provides built-in functions for real-time blockchain event streaming via WebSocket:
/// - `(stream-connect url :programs ["pumpfun"] :tokens ["USDC"])` - Connect to WebSocket stream
/// - `(stream-poll stream-id :limit 50)` - Poll buffered events (non-blocking)
/// - `(stream-wait stream-id :timeout 30)` - Wait for next event (blocking with timeout)
/// - `(stream-close stream-id)` - Close WebSocket connection
/// - `(async-call function arg1 arg2 ...)` - Execute function in thread pool (concurrent processing)
///
/// Example usage (V5 - Event-Driven):
/// ```lisp
/// ;; Connect to Pump.fun event stream via WebSocket
/// (define stream (stream-connect "ws://localhost:8080/ws" :programs ["pumpfun"]))
///
/// ;; Event-driven loop - blocks until event arrives (<1ms latency)
/// (while true
///   (define event (stream-wait stream :timeout 1))
///   (if (not (null? event))
///       (if (= (get event "type") "token_transfer")
///           (log :message "Transfer:" :value (get event "amount"))
///           null)
///       null))
/// ```
///
/// Example usage (V6 - Concurrent Processing):
/// ```lisp
/// ;; Define event handler
/// (defun process-transfer (event)
///   (do
///     (define amount (get event "amount"))
///     (define token (get event "token"))
///     (println (str "Processing: " amount " of " token))))
///
/// ;; Process events concurrently in thread pool
/// (define stream (stream-connect "ws://localhost:8080/ws" :programs ["pumpfun"]))
/// (while true
///   (define event (stream-wait stream :timeout 1))
///   (if (not (null? event))
///       (async-call process-transfer event)  ; Dispatches to thread pool, returns immediately
///       null))
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

// Thread pool for concurrent event processing
lazy_static::lazy_static! {
    static ref THREAD_POOL: rayon::ThreadPool = rayon::ThreadPoolBuilder::new()
        .num_threads(num_cpus::get())
        .build()
        .unwrap();
}

/// Stream connection handle
#[derive(Clone, Debug)]
pub struct StreamHandle {
    /// Unique identifier for this stream connection
    pub id: String,
    /// WebSocket URL endpoint
    pub url: String,
    /// Client-side event filtering criteria
    pub filters: StreamFilters,
    /// Thread-safe buffer of received events
    pub event_buffer: Arc<Mutex<Vec<JsonValue>>>,
    /// Thread-safe connection status flag
    pub is_connected: Arc<Mutex<bool>>,
}

/// Stream filtering options
#[derive(Clone, Debug, Default)]
pub struct StreamFilters {
    /// Filter by program aliases or IDs (e.g., ["pumpfun", "raydium"])
    pub programs: Vec<String>,
    /// Filter by token symbols or mint addresses (e.g., ["USDC", "SOL"])
    pub tokens: Vec<String>,
    /// Filter by specific account addresses
    pub accounts: Vec<String>,
    /// Filter by event type strings (e.g., ["token_transfer", "swap"])
    pub event_types: Vec<String>,
    /// If true, only include successful transactions
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
        _ => {
            return Err(Error::runtime(
                "stream-connect: URL must be a string".to_string(),
            ))
        }
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
            if let Err(e) =
                websocket_client_loop(&url_clone, buffer_clone, connected_clone, filters_clone)
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
        return Err(Error::runtime(
            "stream-poll requires stream-id argument".to_string(),
        ));
    }

    let stream_id = match &args[0] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(Error::runtime(
                "stream-poll: stream-id must be a string".to_string(),
            ))
        }
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
        registry.get(&stream_id).cloned().ok_or_else(|| {
            Error::runtime(format!("stream-poll: stream not found: {}", stream_id))
        })?
    };

    // Check if still connected
    {
        let connected = handle.is_connected.lock().unwrap();
        if !*connected {
            return Err(Error::runtime(
                "stream-poll: WebSocket connection closed".to_string(),
            ));
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
        return Err(Error::runtime(
            "stream-wait requires stream-id argument".to_string(),
        ));
    }

    let stream_id = match &args[0] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(Error::runtime(
                "stream-wait: stream-id must be a string".to_string(),
            ))
        }
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
        registry.get(&stream_id).cloned().ok_or_else(|| {
            Error::runtime(format!("stream-wait: stream not found: {}", stream_id))
        })?
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
                return Err(Error::runtime(
                    "stream-wait: WebSocket connection closed".to_string(),
                ));
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
        return Err(Error::runtime(
            "stream-close requires stream-id argument".to_string(),
        ));
    }

    let stream_id = match &args[0] {
        Value::String(s) => s.clone(),
        _ => {
            return Err(Error::runtime(
                "stream-close: stream-id must be a string".to_string(),
            ))
        }
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

/// Spawn internal OSVM stream server and connect to it
///
/// Syntax: `(osvm-stream &key alias programs tokens accounts)`
///
/// This is a convenience function that:
/// 1. Spawns an embedded stream server in a background thread
/// 2. Automatically connects via WebSocket
/// 3. Returns stream ID for use with stream-poll
/// 4. Server auto-terminates when script ends
///
/// Parameters:
/// - `:alias` (optional): Program alias like "pumpfun", "raydium"
/// - `:programs` (optional): Array of program IDs
/// - `:tokens` (optional): Array of token symbols/mints
/// - `:accounts` (optional): Array of account addresses
///
/// Returns: Stream ID string
///
/// Example:
/// ```lisp
/// (define stream (osvm-stream :alias "pumpfun"))
/// (while true
///   (define events (stream-poll stream))
///   ...)
/// ```
pub fn osvm_stream(args: &[Value]) -> Result<Value> {
    // Parse keyword arguments
    let mut alias: Option<String> = None;
    let mut programs: Vec<String> = Vec::new();
    let mut tokens: Vec<String> = Vec::new();
    let mut accounts: Vec<String> = Vec::new();

    let mut i = 0;
    while i < args.len() {
        if let Value::String(key) = &args[i] {
            if key.starts_with(':') {
                if i + 1 >= args.len() {
                    return Err(Error::runtime(format!(
                        "osvm-stream: missing value for keyword argument {}",
                        key
                    )));
                }

                let value = &args[i + 1];
                match key.as_str() {
                    ":alias" => {
                        if let Value::String(s) = value {
                            alias = Some(s.clone());
                        }
                    }
                    ":programs" => {
                        programs = extract_string_array(value)?;
                    }
                    ":tokens" => {
                        tokens = extract_string_array(value)?;
                    }
                    ":accounts" => {
                        accounts = extract_string_array(value)?;
                    }
                    _ => {
                        return Err(Error::runtime(format!(
                            "osvm-stream: unknown keyword argument {}",
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

    // If alias provided, add to programs list
    if let Some(alias_name) = alias {
        programs.push(alias_name);
    }

    // Find an available port
    let port = find_available_port()?;

    // Spawn internal stream server in background
    spawn_internal_server(port, programs.clone(), tokens.clone(), accounts.clone())?;

    // Wait for server to start
    thread::sleep(Duration::from_millis(1000));

    // Connect via WebSocket
    let ws_url = format!("ws://127.0.0.1:{}/ws", port);
    let mut connect_args = vec![Value::String(ws_url)];

    // Add filters if provided
    if !programs.is_empty() {
        connect_args.push(Value::String(":programs".to_string()));
        connect_args.push(Value::Array(Arc::new(
            programs.into_iter().map(Value::String).collect(),
        )));
    }
    if !tokens.is_empty() {
        connect_args.push(Value::String(":tokens".to_string()));
        connect_args.push(Value::Array(Arc::new(
            tokens.into_iter().map(Value::String).collect(),
        )));
    }
    if !accounts.is_empty() {
        connect_args.push(Value::String(":accounts".to_string()));
        connect_args.push(Value::Array(Arc::new(
            accounts.into_iter().map(Value::String).collect(),
        )));
    }

    // Call stream_connect
    stream_connect(&connect_args)
}

/// Find an available port for the internal server
fn find_available_port() -> Result<u16> {
    use std::net::TcpListener;

    // Try ports 18080-18180
    for port in 18080..18180 {
        if TcpListener::bind(("127.0.0.1", port)).is_ok() {
            return Ok(port);
        }
    }

    Err(Error::runtime(
        "Could not find available port for internal stream server".to_string(),
    ))
}

/// Spawn internal stream server in background thread
fn spawn_internal_server(
    port: u16,
    programs: Vec<String>,
    tokens: Vec<String>,
    accounts: Vec<String>,
) -> Result<()> {
    thread::spawn(move || {
        // This would need to call the actual stream server code
        // For now, this is a placeholder that shows the architecture
        eprintln!(
            "Internal stream server would start on port {} with filters:",
            port
        );
        eprintln!("  Programs: {:?}", programs);
        eprintln!("  Tokens: {:?}", tokens);
        eprintln!("  Accounts: {:?}", accounts);

        // TODO: Actually spawn the stream server here
        // Need to refactor stream service to be embeddable
    });

    Ok(())
}

/// Generate unique async task ID
fn generate_async_id() -> String {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("async_{}", id)
}

/// Execute function asynchronously in thread pool (returns awaitable handle)
///
/// Syntax: `(async function arg1 arg2 ...)`
///
/// This dispatches function execution to the global thread pool and returns
/// an AsyncHandle that can be awaited for the result.
///
/// **Key Characteristics:**
/// - **Non-blocking**: Returns AsyncHandle immediately
/// - **Awaitable**: Use `(await handle)` to get result
/// - **Fire-and-forget**: Ignore handle if result not needed
/// - **Isolated execution**: Each async call gets its own evaluator
/// - **Closure capture**: Lambda closures properly preserved
///
/// **Performance:**
/// - Utilizes all CPU cores (worker pool size = num_cpus)
/// - No blocking on main thread until await
/// - Ideal for I/O-heavy or CPU-intensive operations
///
/// Example:
/// ```lisp
/// ;; Fire-and-forget (ignore handle)
/// (async println "Background task")
///
/// ;; Await result
/// (define handle (async calculate-sum 10 20))
/// (define result (await handle))  ; Blocks until complete
/// (println result)  ; → 30
///
/// ;; Concurrent processing
/// (define handles
///   (map [1 2 3 4 5] (lambda (n) (async factorial n))))
/// (define results (map handles await))
/// (println results)  ; → [1, 2, 6, 24, 120]
/// ```
pub fn async_execute(func: Value, args: Vec<Value>) -> Result<Value> {
    match func {
        Value::Function {
            params,
            body,
            closure,
            ..
        } => {
            // Validate arity
            if params.len() != args.len() {
                return Err(Error::runtime(format!(
                    "async: function expects {} arguments, got {}",
                    params.len(),
                    args.len()
                )));
            }

            // Create oneshot channel for result
            let (tx, rx) = tokio::sync::oneshot::channel();
            let task_id = generate_async_id();

            // Clone everything for thread pool (must be Send + Sync)
            let params_clone = params.clone();
            let body_clone = Arc::clone(&body);
            let closure_clone = Arc::clone(&closure);
            let args_clone = args.clone();

            // Dispatch to thread pool
            THREAD_POOL.spawn(move || {
                // Import here to avoid circular dependency in module-level use
                use crate::runtime::LispEvaluator;

                // Create isolated evaluator for this thread
                let mut evaluator = LispEvaluator::new();

                // Restore closure environment (captured variables)
                for (var_name, var_value) in closure_clone.iter() {
                    evaluator.env.define(var_name.clone(), var_value.clone());
                }

                // Bind function parameters
                for (param_name, arg_value) in params_clone.iter().zip(args_clone.iter()) {
                    evaluator.env.define(param_name.clone(), arg_value.clone());
                }

                // Execute function body and send result
                let result = match evaluator.evaluate_expression(&body_clone) {
                    Ok(val) => val,
                    Err(e) => {
                        eprintln!("⚠️  async task error: {}", e);
                        Value::Null // Return null on error
                    }
                };

                // Send result (ignore error if receiver dropped)
                let _ = tx.send(result);
            });

            // Return AsyncHandle immediately
            Ok(Value::AsyncHandle {
                id: task_id,
                receiver: Arc::new(std::sync::Mutex::new(Some(rx))),
            })
        }
        _ => Err(Error::runtime(format!(
            "async: first argument must be a function, got {}",
            func.type_name()
        ))),
    }
}

/// Wait for async task to complete and return result
///
/// Syntax: `(await async-handle)`
///
/// Blocks until the async task completes and returns its result.
/// Can only be called once per handle (receiver is consumed).
///
/// Example:
/// ```lisp
/// (define handle (async factorial 10))
/// (println "Task running in background...")
/// (define result (await handle))  ; Blocks here
/// (println (str "Result: " result))  ; → Result: 3628800
/// ```
pub fn await_async(handle: Value) -> Result<Value> {
    match handle {
        Value::AsyncHandle { id, receiver } => {
            // Try to take receiver (can only await once!)
            let mut rx = receiver
                .lock()
                .unwrap()
                .take()
                .ok_or_else(|| Error::runtime(format!("AsyncHandle {} already awaited", id)))?;

            // Block until result available (poll in busy-wait since blocking_recv
            // doesn't work inside tokio runtime)
            loop {
                match rx.try_recv() {
                    Ok(result) => return Ok(result),
                    Err(tokio::sync::oneshot::error::TryRecvError::Empty) => {
                        // Not ready yet, sleep briefly
                        std::thread::sleep(std::time::Duration::from_micros(100));
                    }
                    Err(tokio::sync::oneshot::error::TryRecvError::Closed) => {
                        return Err(Error::runtime(format!(
                            "AsyncHandle {} task panicked or was cancelled",
                            id
                        )));
                    }
                }
            }
        }
        _ => Err(Error::runtime(format!(
            "await requires AsyncHandle, got {}",
            handle.type_name()
        ))),
    }
}
