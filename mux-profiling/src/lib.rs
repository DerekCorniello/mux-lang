use serde::Serialize;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};
use std::thread;
use std::time::Instant;

#[derive(Clone, Serialize)]
struct SpanRecord {
    id: u64,
    parent_id: Option<u64>,
    thread: String,
    name: String,
    start_ms: f64,
    end_ms: f64,
}

#[derive(Default)]
struct RecorderState {
    spans: Vec<SpanRecord>,
}

struct Recorder {
    start: Instant,
    state: Mutex<RecorderState>,
    output_prefix: PathBuf,
    next_id: AtomicU64,
}

impl Recorder {
    fn new(output_prefix: PathBuf) -> Self {
        Self {
            start: Instant::now(),
            state: Mutex::new(RecorderState::default()),
            output_prefix,
            next_id: AtomicU64::new(1),
        }
    }

    fn next_span_id(&self) -> u64 {
        self.next_id.fetch_add(1, Ordering::Relaxed)
    }

    fn push_span(&self, record: SpanRecord) {
        if let Ok(mut state) = self.state.lock() {
            state.spans.push(record);
        }
    }

    fn spans(&self) -> Vec<SpanRecord> {
        self.state
            .lock()
            .map(|state| state.spans.clone())
            .unwrap_or_default()
    }

    fn write_reports(&self) {
        let spans = self.spans();
        if spans.is_empty() {
            return;
        }

        let summary_path = self.output_prefix.with_extension("summary.json");
        let speedscope_path = self.output_prefix.with_extension("speedscope.json");

        if let Some(parent) = summary_path.parent() {
            let _ = fs::create_dir_all(parent);
        }

        let mut totals: BTreeMap<String, SummaryEntry> = BTreeMap::new();
        for span in &spans {
            let entry = totals.entry(span.name.clone()).or_default();
            entry.count += 1;
            entry.total_ms += span.end_ms - span.start_ms;
            entry.max_ms = entry.max_ms.max(span.end_ms - span.start_ms);
        }

        let mut totals_vec: Vec<SummaryRow> = totals
            .into_iter()
            .map(|(name, entry)| SummaryRow {
                name,
                count: entry.count,
                total_ms: entry.total_ms,
                max_ms: entry.max_ms,
            })
            .collect();
        totals_vec.sort_by(|a, b| b.total_ms.total_cmp(&a.total_ms));

        let summary = SummaryReport {
            generated_by: "mux-profiling",
            total_duration_ms: self.start.elapsed().as_secs_f64() * 1000.0,
            span_count: spans.len(),
            totals: totals_vec,
            spans,
        };

        if let Ok(json) = serde_json::to_string_pretty(&summary) {
            let _ = fs::write(&summary_path, json);
        }

        if let Ok(json) = serde_json::to_string_pretty(&build_speedscope_report(&summary.spans)) {
            let _ = fs::write(&speedscope_path, json);
        }
    }
}

#[derive(Default)]
struct SummaryEntry {
    count: usize,
    total_ms: f64,
    max_ms: f64,
}

#[derive(Serialize)]
struct SummaryRow {
    name: String,
    count: usize,
    total_ms: f64,
    max_ms: f64,
}

#[derive(Serialize)]
struct SummaryReport<'a> {
    generated_by: &'a str,
    total_duration_ms: f64,
    span_count: usize,
    totals: Vec<SummaryRow>,
    spans: Vec<SpanRecord>,
}

#[derive(Serialize)]
struct SpeedscopeFile<'a> {
    #[serde(rename = "$schema")]
    schema: &'a str,
    shared: SpeedscopeShared,
    profiles: Vec<SpeedscopeProfile>,
    active_profile_index: usize,
}

#[derive(Serialize)]
struct SpeedscopeShared {
    frames: Vec<SpeedscopeFrame>,
}

#[derive(Serialize)]
struct SpeedscopeFrame {
    name: String,
}

#[derive(Serialize)]
struct SpeedscopeProfile {
    #[serde(rename = "type")]
    profile_type: &'static str,
    name: String,
    unit: &'static str,
    start_value: f64,
    end_value: f64,
    events: Vec<SpeedscopeEvent>,
}

#[derive(Serialize)]
struct SpeedscopeEvent {
    #[serde(rename = "type")]
    event_type: &'static str,
    at: f64,
    frame: usize,
}

thread_local! {
    static SPAN_STACK: RefCell<Vec<u64>> = const { RefCell::new(Vec::new()) };
}

static COMPILER_RECORDER: OnceLock<Recorder> = OnceLock::new();
static RUNTIME_RECORDER: OnceLock<Recorder> = OnceLock::new();
static ATEXIT_REGISTERED: OnceLock<()> = OnceLock::new();

fn output_prefix_from_env(kind: &str) -> Option<PathBuf> {
    let key = match kind {
        "compiler" => "MUX_COMPILER_PROFILE_OUTPUT",
        "runtime" => "MUX_RUNTIME_PROFILE_OUTPUT",
        _ => return None,
    };
    env::var(key).ok().map(PathBuf::from)
}

fn recorder(kind: &str) -> Option<&'static Recorder> {
    match kind {
        "compiler" => {
            let prefix = output_prefix_from_env(kind)?;
            Some(COMPILER_RECORDER.get_or_init(|| Recorder::new(prefix)))
        }
        "runtime" => {
            let prefix = output_prefix_from_env(kind)?;
            Some(RUNTIME_RECORDER.get_or_init(|| Recorder::new(prefix)))
        }
        _ => None,
    }
}

fn register_atexit() {
    if ATEXIT_REGISTERED.get().is_some() {
        return;
    }

    extern "C" fn flush() {
        if let Some(recorder) = COMPILER_RECORDER.get() {
            recorder.write_reports();
        }
        if let Some(recorder) = RUNTIME_RECORDER.get() {
            recorder.write_reports();
        }
    }

    unsafe {
        libc::atexit(flush);
    }

    let _ = ATEXIT_REGISTERED.set(());
}

pub struct ProfileGuard {
    recorder: Option<&'static Recorder>,
    span_id: Option<u64>,
    name: &'static str,
    start: Instant,
}

impl Drop for ProfileGuard {
    fn drop(&mut self) {
        let Some(recorder) = self.recorder else {
            return;
        };
        let Some(span_id) = self.span_id else {
            return;
        };

        let end = recorder.start.elapsed().as_secs_f64() * 1000.0;
        let start = self.start.duration_since(recorder.start).as_secs_f64() * 1000.0;
        let thread = thread::current()
            .name()
            .map(str::to_string)
            .unwrap_or_else(|| format!("thread-{:?}", thread::current().id()));
        let parent_id = SPAN_STACK.with(|stack| {
            let mut stack = stack.borrow_mut();
            let _ = stack.pop();
            stack.last().copied()
        });

        recorder.push_span(SpanRecord {
            id: span_id,
            parent_id,
            thread,
            name: self.name.to_string(),
            start_ms: start,
            end_ms: end,
        });
    }
}

fn scope_for(kind: &'static str, name: &'static str) -> ProfileGuard {
    let start = Instant::now();
    let Some(recorder) = recorder(kind) else {
        return ProfileGuard {
            recorder: None,
            span_id: None,
            name,
            start,
        };
    };

    register_atexit();

    let span_id = recorder.next_span_id();
    SPAN_STACK.with(|stack| stack.borrow_mut().push(span_id));

    ProfileGuard {
        recorder: Some(recorder),
        span_id: Some(span_id),
        name,
        start,
    }
}

pub fn compiler_scope(name: &'static str) -> ProfileGuard {
    scope_for("compiler", name)
}

pub fn runtime_scope(name: &'static str) -> ProfileGuard {
    scope_for("runtime", name)
}

fn build_speedscope_report(spans: &[SpanRecord]) -> SpeedscopeFile<'_> {
    let mut frames = Vec::new();
    let mut frame_map = BTreeMap::new();
    for span in spans {
        if frame_map.contains_key(&span.name) {
            continue;
        }
        let idx = frames.len();
        frame_map.insert(span.name.clone(), idx);
        frames.push(SpeedscopeFrame {
            name: span.name.clone(),
        });
    }

    let mut threads: BTreeMap<String, Vec<&SpanRecord>> = BTreeMap::new();
    for span in spans {
        threads.entry(span.thread.clone()).or_default().push(span);
    }

    let profiles = threads
        .into_iter()
        .map(|(thread_name, thread_spans)| {
            let mut events = Vec::with_capacity(thread_spans.len() * 2);
            let mut min_start = f64::INFINITY;
            let mut max_end: f64 = 0.0;
            for span in thread_spans {
                let Some(&frame) = frame_map.get(&span.name) else {
                    debug_assert!(false, "missing frame for span name: {}", span.name);
                    continue;
                };
                events.push(SpeedscopeEvent {
                    event_type: "O",
                    at: span.start_ms,
                    frame,
                });
                events.push(SpeedscopeEvent {
                    event_type: "C",
                    at: span.end_ms,
                    frame,
                });
                min_start = min_start.min(span.start_ms);
                max_end = max_end.max(span.end_ms);
            }
            events.sort_by(|a, b| {
                a.at.total_cmp(&b.at)
                    .then_with(|| a.event_type.cmp(b.event_type))
            });

            SpeedscopeProfile {
                profile_type: "evented",
                name: thread_name,
                unit: "milliseconds",
                start_value: if min_start.is_finite() {
                    min_start
                } else {
                    0.0
                },
                end_value: max_end,
                events,
            }
        })
        .collect();

    SpeedscopeFile {
        schema: "https://www.speedscope.app/file-format-schema.json",
        shared: SpeedscopeShared { frames },
        profiles,
        active_profile_index: 0,
    }
}
