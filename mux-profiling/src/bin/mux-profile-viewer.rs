use eframe::egui;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::PathBuf;

#[derive(Debug, Clone, Deserialize)]
struct SpanRecord {
    id: u64,
    parent_id: Option<u64>,
    thread: String,
    name: String,
    start_ms: f64,
    end_ms: f64,
}

#[derive(Debug, Clone, Deserialize)]
struct SummaryRow {
    name: String,
    count: usize,
    total_ms: f64,
    max_ms: f64,
}

#[derive(Debug, Clone, Deserialize)]
struct SummaryReport {
    generated_by: String,
    total_duration_ms: f64,
    span_count: usize,
    totals: Vec<SummaryRow>,
    spans: Vec<SpanRecord>,
}

struct ProfileViewerApp {
    summary_path: Option<PathBuf>,
    summary: Option<SummaryReport>,
    errors: Vec<String>,
    selected_span: Option<u64>,
}

impl ProfileViewerApp {
    fn new(summary_path: Option<PathBuf>) -> Self {
        let mut app = Self {
            summary_path,
            summary: None,
            errors: Vec::new(),
            selected_span: None,
        };
        app.reload();
        app
    }

    fn reload(&mut self) {
        self.errors.clear();
        self.summary = None;

        if let Some(path) = &self.summary_path {
            match fs::read_to_string(path) {
                Ok(text) => match serde_json::from_str::<SummaryReport>(&text) {
                    Ok(report) => self.summary = Some(report),
                    Err(err) => self.errors.push(format!(
                        "failed to parse summary {}: {}",
                        path.display(),
                        err
                    )),
                },
                Err(err) => self.errors.push(format!(
                    "failed to read summary {}: {}",
                    path.display(),
                    err
                )),
            }
        }
    }

    fn palette(name: &str) -> egui::Color32 {
        let mut hash = 0u32;
        for byte in name.bytes() {
            hash = hash.wrapping_mul(33).wrapping_add(byte as u32);
        }
        let r = 80 + (hash & 0x7f) as u8;
        let g = 80 + ((hash >> 7) & 0x7f) as u8;
        let b = 80 + ((hash >> 14) & 0x7f) as u8;
        egui::Color32::from_rgb(r, g, b)
    }

    fn span_duration(span: &SpanRecord) -> f64 {
        (span.end_ms - span.start_ms).max(0.0)
    }

    fn depth_for(&self, span: &SpanRecord, by_id: &BTreeMap<u64, &SpanRecord>) -> usize {
        let mut depth = 0usize;
        let mut current = span.parent_id;
        while let Some(parent_id) = current {
            let Some(parent) = by_id.get(&parent_id) else {
                break;
            };
            if parent.thread != span.thread {
                break;
            }
            depth += 1;
            current = parent.parent_id;
        }
        depth
    }

    fn render_header(&self, ui: &mut egui::Ui) {
        if let Some(summary) = &self.summary {
            ui.horizontal_wrapped(|ui| {
                ui.label(format!("Source: {}", summary.generated_by));
                ui.separator();
                ui.label(format!("Total: {:.2} ms", summary.total_duration_ms));
                ui.separator();
                ui.label(format!("Spans: {}", summary.span_count));
            });

            ui.add_space(8.0);
            ui.heading("Hotspots");
            let max_total = summary
                .totals
                .iter()
                .map(|row| row.total_ms)
                .fold(0.0_f64, f64::max)
                .max(1.0);
            for row in summary.totals.iter().take(8) {
                let pct = row.total_ms / max_total;
                ui.horizontal(|ui| {
                    ui.label(&row.name);
                    let bar_width = (ui.available_width() * 0.45).max(120.0);
                    let (rect, response) =
                        ui.allocate_exact_size(egui::vec2(bar_width, 18.0), egui::Sense::hover());
                    let painter = ui.painter_at(rect);
                    painter.rect_filled(rect, 3.0, egui::Color32::from_gray(36));
                    let fill = egui::Rect::from_min_max(
                        rect.left_top(),
                        egui::pos2(rect.left() + rect.width() * pct as f32, rect.bottom()),
                    );
                    painter.rect_filled(fill, 3.0, Self::palette(&row.name));
                    painter.rect_stroke(
                        rect,
                        3.0,
                        egui::Stroke::new(1.0, egui::Color32::BLACK),
                        egui::StrokeKind::Outside,
                    );
                    response.on_hover_text(format!(
                        "{}\ncount: {}\ntotal: {:.2} ms\nmax: {:.2} ms",
                        row.name, row.count, row.total_ms, row.max_ms
                    ));
                    ui.label(format!("{:.2} ms", row.total_ms));
                });
            }
            ui.add_space(12.0);
        }
    }

    fn render_flamegraph(&mut self, ui: &mut egui::Ui) {
        let Some(summary) = &self.summary else {
            ui.label("No summary loaded.");
            return;
        };

        let span_by_id: BTreeMap<u64, &SpanRecord> =
            summary.spans.iter().map(|span| (span.id, span)).collect();
        let total_duration = summary.total_duration_ms.max(1.0);
        let lane_width = ui.available_width().max(800.0);
        let row_height = 24.0;
        let title_height = 22.0;
        let v_spacing = 14.0;

        let mut threads: BTreeMap<String, Vec<&SpanRecord>> = BTreeMap::new();
        for span in &summary.spans {
            threads.entry(span.thread.clone()).or_default().push(span);
        }

        for (thread_name, spans) in threads {
            let mut max_depth = 0usize;
            for span in &spans {
                max_depth = max_depth.max(self.depth_for(span, &span_by_id));
            }

            let canvas_height = title_height + ((max_depth + 1) as f32 * row_height) + 8.0;
            let (rect, _) =
                ui.allocate_exact_size(egui::vec2(lane_width, canvas_height), egui::Sense::hover());
            let painter = ui.painter_at(rect);

            painter.text(
                egui::pos2(rect.left(), rect.top() + 2.0),
                egui::Align2::LEFT_TOP,
                thread_name.clone(),
                egui::FontId::monospace(13.0),
                egui::Color32::WHITE,
            );

            painter.rect_stroke(
                egui::Rect::from_min_max(
                    egui::pos2(rect.left(), rect.top() + title_height),
                    rect.right_bottom(),
                ),
                3.0,
                egui::Stroke::new(1.0, egui::Color32::DARK_GRAY),
                egui::StrokeKind::Outside,
            );

            for span in spans {
                let depth = self.depth_for(span, &span_by_id);
                let bar_top = rect.top() + title_height + depth as f32 * row_height + 2.0;
                let bar_bottom = bar_top + row_height - 4.0;
                let x0 = rect.left() + ((span.start_ms / total_duration) as f32) * rect.width();
                let x1 = rect.left() + ((span.end_ms / total_duration) as f32) * rect.width();
                let bar = egui::Rect::from_min_max(
                    egui::pos2(x0, bar_top),
                    egui::pos2(x1.max(x0 + 3.0), bar_bottom),
                );
                let color = Self::palette(&span.name);
                painter.rect_filled(bar, 3.0, color);
                painter.rect_stroke(
                    bar,
                    3.0,
                    egui::Stroke::new(1.0, egui::Color32::BLACK),
                    egui::StrokeKind::Outside,
                );
                let response = ui.interact(bar, egui::Id::new(span.id), egui::Sense::click());
                if response.hovered() || response.clicked() {
                    self.selected_span = Some(span.id);
                }
                response.on_hover_ui(|ui| {
                    ui.label(&span.name);
                    ui.label(format!("Thread: {}", span.thread));
                    ui.label(format!("Start: {:.2} ms", span.start_ms));
                    ui.label(format!("End: {:.2} ms", span.end_ms));
                    ui.label(format!("Duration: {:.2} ms", Self::span_duration(span)));
                    if let Some(parent) = span.parent_id {
                        ui.label(format!("Parent: {}", parent));
                    }
                });
            }

            ui.add_space(v_spacing);
        }
    }

    fn render_selected(&self, ui: &mut egui::Ui) {
        let Some(summary) = &self.summary else {
            return;
        };

        ui.separator();
        ui.heading("Selected");
        if let Some(id) = self.selected_span {
            if let Some(span) = summary.spans.iter().find(|span| span.id == id) {
                ui.label(format!("Name: {}", span.name));
                ui.label(format!("Thread: {}", span.thread));
                ui.label(format!("Start: {:.2} ms", span.start_ms));
                ui.label(format!("End: {:.2} ms", span.end_ms));
                ui.label(format!("Duration: {:.2} ms", Self::span_duration(span)));
                if let Some(parent) = span.parent_id {
                    ui.label(format!("Parent: {}", parent));
                }
            }
        } else {
            ui.label("Click a block to inspect it.");
        }
    }
}

impl eframe::App for ProfileViewerApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("header").show(ctx, |ui| {
            ui.horizontal(|ui| {
                if ui.button("Reload").clicked() {
                    self.reload();
                }
                ui.separator();
                ui.label("Mux Profile Viewer");
                if let Some(path) = &self.summary_path {
                    ui.separator();
                    ui.label(path.display().to_string());
                }
            });
            if !self.errors.is_empty() {
                for error in &self.errors {
                    ui.colored_label(egui::Color32::RED, error);
                }
            }
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            egui::ScrollArea::vertical().show(ui, |ui| {
                self.render_header(ui);
                self.render_flamegraph(ui);
                self.render_selected(ui);
            });
        });
    }
}

fn main() -> eframe::Result<()> {
    let args: Vec<PathBuf> = env::args_os().skip(1).map(PathBuf::from).collect();
    let summary_path = args.first().cloned();

    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "Mux Profile Viewer",
        native_options,
        Box::new(move |_cc| Ok(Box::new(ProfileViewerApp::new(summary_path)))),
    )
}
