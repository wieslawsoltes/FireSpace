mod flame;
mod scan;
mod widgets;

use std::env;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};

use xilem::core::{MessageProxy, NoElement, ViewSequence, fork};
use xilem::masonry::dpi::LogicalSize;
use xilem::masonry::properties::Padding;
use xilem::masonry::properties::types::{AsUnit, CrossAxisAlignment, MainAxisAlignment};
use xilem::style::Style as _;
use xilem::tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use xilem::view::{
    AnyFlexChild, FlexExt as _, FlexSpacer, button, checkbox, flex_col, flex_row, label, sized_box,
    text_button, text_input, worker_raw,
};
use xilem::{EventLoop, EventLoopBuilder, WidgetView, WindowOptions, Xilem, winit};

use crate::flame::{format_bytes, grouped_color};
use crate::scan::{FsNode, FsTree};
use crate::widgets::flame_graph::{FlameGraphAction, FlameGraphMode, flame_graph};
use crate::widgets::hover_clear::hover_clear;
use crate::widgets::hover_wrap::{HoverWrapAction, hover_wrap};

const APP_BG: xilem::Color = xilem::Color::from_rgb8(45, 48, 58);
const APP_BG_DARK: xilem::Color = xilem::Color::from_rgb8(37, 40, 50);
const APP_BG_DARKER: xilem::Color = xilem::Color::from_rgb8(30, 33, 42);
const TITLE_BAR_BG: xilem::Color = xilem::Color::from_rgb8(49, 52, 62);
const CHIP_BG: xilem::Color = xilem::Color::from_rgb8(60, 64, 76);
const CHIP_BG_ACTIVE: xilem::Color = xilem::Color::from_rgb8(58, 113, 208);
const NAV_BG: xilem::Color = xilem::Color::from_rgb8(54, 57, 68);

#[derive(Debug)]
struct ScanRequest {
    path: PathBuf,
    cancel: Arc<AtomicBool>,
}

#[derive(Debug)]
struct ScanResponse {
    path: PathBuf,
    duration: Duration,
    result: ScanResult,
}

#[derive(Debug)]
enum ScanResult {
    Completed(Result<FsTree, String>),
    Cancelled,
}

#[derive(Debug, Default)]
struct ScanSummary {
    path: PathBuf,
    duration: Duration,
    total_bytes: u64,
    nodes: usize,
    errors: usize,
    skipped: usize,
}

struct AppState {
    path_input: String,
    scanning: bool,
    scan_sender: Option<UnboundedSender<ScanRequest>>,
    cancel_flag: Option<Arc<AtomicBool>>,
    tree: Option<Arc<FsTree>>,
    focus: Option<usize>,
    hovered: Option<usize>,
    panel_hovered: Option<usize>,
    selected: Option<usize>,
    view_mode: FlameGraphMode,
    zoom_on_select: bool,
    last_scan: Option<ScanSummary>,
    error: Option<String>,
    auto_scan_pending: bool,
}

impl AppState {
    fn new() -> Self {
        let default_path = default_scan_path();
        Self {
            path_input: default_path.display().to_string(),
            scanning: false,
            scan_sender: None,
            cancel_flag: None,
            tree: None,
            focus: None,
            hovered: None,
            panel_hovered: None,
            selected: None,
            view_mode: FlameGraphMode::Radial,
            zoom_on_select: true,
            last_scan: None,
            error: None,
            auto_scan_pending: false,
        }
    }

    fn set_scan_sender(&mut self, sender: UnboundedSender<ScanRequest>) {
        self.scan_sender = Some(sender);
        if self.auto_scan_pending {
            self.auto_scan_pending = false;
            self.start_scan();
        }
    }

    fn start_scan(&mut self) {
        if self.scanning {
            return;
        }
        let path = normalize_input_path(&self.path_input);
        if path.as_os_str().is_empty() {
            return;
        }
        let Some(sender) = self.scan_sender.as_ref() else {
            self.auto_scan_pending = true;
            return;
        };
        self.auto_scan_pending = false;
        let cancel_flag = Arc::new(AtomicBool::new(false));
        self.scanning = true;
        self.error = None;
        self.cancel_flag = Some(cancel_flag.clone());
        let _ = sender.send(ScanRequest { path, cancel: cancel_flag });
    }

    fn stop_scan(&mut self) {
        if let Some(cancel_flag) = &self.cancel_flag {
            cancel_flag.store(true, Ordering::Relaxed);
        }
    }

    fn handle_scan_response(&mut self, response: ScanResponse) {
        self.scanning = false;
        self.cancel_flag = None;
        match response.result {
            ScanResult::Completed(Ok(tree)) => {
                let summary = ScanSummary {
                    path: response.path,
                    duration: response.duration,
                    total_bytes: tree.total_bytes,
                    nodes: tree.nodes.len(),
                    errors: tree.errors,
                    skipped: tree.skipped,
                };
                self.tree = Some(Arc::new(tree));
                self.focus = self.tree.as_ref().map(|tree| tree.root);
                self.hovered = None;
                self.panel_hovered = None;
                self.selected = None;
                self.last_scan = Some(summary);
                self.error = None;
            }
            ScanResult::Completed(Err(message)) => {
                self.tree = None;
                self.last_scan = None;
                self.error = Some(message);
            }
            ScanResult::Cancelled => {
                self.error = None;
            }
        }
    }

    fn focus_root(&mut self) {
        if let Some(tree) = &self.tree {
            self.focus = Some(tree.root);
            self.hovered = None;
            self.panel_hovered = None;
            self.selected = None;
        }
    }

    fn focus_parent(&mut self) {
        let Some(tree) = &self.tree else {
            return;
        };
        let focus = self.focus.unwrap_or(tree.root);
        if let Some(parent) = tree.nodes.get(focus).and_then(|node| node.parent) {
            self.focus = Some(parent);
            self.hovered = None;
            self.panel_hovered = None;
            self.selected = None;
        }
    }

    fn can_focus_parent(&self) -> bool {
        let Some(tree) = &self.tree else {
            return false;
        };
        let focus = self.focus.unwrap_or(tree.root);
        tree.nodes.get(focus).and_then(|node| node.parent).is_some()
    }

    fn handle_flame_action(&mut self, action: FlameGraphAction) {
        match action {
            FlameGraphAction::Hovered(node) => {
                self.hovered = node;
            }
            FlameGraphAction::Selected(node) => {
                self.selected = node;
                if self.zoom_on_select {
                    self.focus = node.or(self.focus);
                    self.hovered = None;
                }
            }
        }
    }

    fn focus_node(&mut self, node_id: usize) {
        let Some(tree) = &self.tree else {
            return;
        };
        if node_id >= tree.nodes.len() {
            return;
        }
        self.focus = Some(node_id);
        self.hovered = None;
        self.panel_hovered = None;
        self.selected = None;
    }

    fn set_panel_hovered(&mut self, node: Option<usize>) {
        self.panel_hovered = node;
    }

    fn display_hovered(&self) -> Option<usize> {
        self.hovered
    }

    fn status_text(&self) -> String {
        if self.scanning {
            return format!("Scanning {}...", self.path_input.trim());
        }
        if let Some(error) = &self.error {
            return format!("Scan failed: {error}");
        }
        let Some(summary) = &self.last_scan else {
            return "Ready to scan".to_string();
        };
        let duration_ms = summary.duration.as_secs_f64() * 1000.0;
        format!(
            "Scanned {} in {:.0} ms | {} nodes | {} total | {} skipped | {} errors",
            summary.path.display(),
            duration_ms,
            summary.nodes,
            format_bytes(summary.total_bytes),
            summary.skipped,
            summary.errors
        )
    }

    fn detail_text(&self) -> String {
        let Some(tree) = &self.tree else {
            return "Hover or click a block to inspect size and path.".to_string();
        };
        let target = self.display_hovered().or(self.selected).or(self.focus)
            .unwrap_or(tree.root);
        let Some(node) = tree.nodes.get(target) else {
            return "Hover or click a block to inspect size and path.".to_string();
        };
        format!(
            "{} | {} | {}",
            node_label(node),
            format_bytes(node.size),
            node.path.display()
        )
    }
}

fn app_logic(data: &mut AppState) -> impl WidgetView<AppState> + use<> {
    let header = flex_row((
        label("FireSpace")
            .text_size(20.0)
            .weight(xilem::FontWeight::BOLD),
        FlexSpacer::Fixed(10.px()),
        text_input(data.path_input.clone(), |data: &mut AppState, value: String| {
            data.path_input = value;
        })
        .placeholder("Path to scan")
        .on_enter(|data: &mut AppState, _| {
            data.start_scan();
        })
        .flex(1.0),
        FlexSpacer::Fixed(8.px()),
        text_button(if data.scanning { "Stop" } else { "Scan" }, |data: &mut AppState| {
            if data.scanning {
                data.stop_scan();
            } else {
                data.start_scan();
            }
        })
        .disabled(data.scanning && data.cancel_flag.is_none()),
        FlexSpacer::Fixed(6.px()),
        text_button("Zoom Out", |data: &mut AppState| {
            data.focus_parent();
        })
        .disabled(!data.can_focus_parent()),
        FlexSpacer::Fixed(4.px()),
        text_button("Reset", |data: &mut AppState| {
            data.focus_root();
        })
        .disabled(data.tree.is_none()),
        FlexSpacer::Fixed(10.px()),
        checkbox("Radial view", data.view_mode == FlameGraphMode::Radial, |data: &mut AppState, value| {
            data.view_mode = if value {
                FlameGraphMode::Radial
            } else {
                FlameGraphMode::Flame
            };
        }),
        FlexSpacer::Fixed(8.px()),
        checkbox("Zoom on click", data.zoom_on_select, |data: &mut AppState, value| {
            data.zoom_on_select = value;
        }),
    ))
    .cross_axis_alignment(CrossAxisAlignment::Center)
    .main_axis_alignment(MainAxisAlignment::Start)
    .padding(10.0)
    .background_color(APP_BG_DARK);

    let graph = flame_graph(
        data.tree.clone(),
        data.focus,
        data.selected,
        data.view_mode,
        data.panel_hovered,
        |data: &mut AppState, action| {
            data.handle_flame_action(action);
        },
    )
    .flex(1.0);

    let title_bar = title_bar_view(data);

    let body = flex_row((graph, side_panel_view(data)))
        .cross_axis_alignment(CrossAxisAlignment::Fill)
        .main_axis_alignment(MainAxisAlignment::Start)
        .flex(1.0);

    let footer = flex_col((
        label(data.detail_text())
            .text_size(12.0)
            .color(xilem::Color::from_rgb8(216, 220, 230)),
        label(data.status_text())
            .text_size(11.0)
            .color(xilem::Color::from_rgb8(140, 150, 170)),
    ))
    .cross_axis_alignment(CrossAxisAlignment::Start)
    .padding(10.0)
    .background_color(APP_BG_DARKER);

    let ui = flex_col((title_bar, header, body, footer))
        .cross_axis_alignment(CrossAxisAlignment::Fill)
        .main_axis_alignment(MainAxisAlignment::Start)
        .must_fill_major_axis(true);

    fork(ui, scan_worker_view())
}

fn scan_worker_view() -> impl ViewSequence<AppState, (), xilem::ViewCtx, NoElement> {
    worker_raw(
        scan_worker_task,
        |data: &mut AppState, sender: UnboundedSender<ScanRequest>| {
            data.set_scan_sender(sender);
        },
        |data: &mut AppState, response: ScanResponse| {
            data.handle_scan_response(response);
        },
    )
}

#[derive(Clone)]
struct PanelEntry {
    name: String,
    size: u64,
    color: xilem::Color,
    node_id: Option<usize>,
}

struct PanelInfo {
    title: String,
    path: String,
    size: u64,
    entries: Vec<PanelEntry>,
    remainder: Option<u64>,
}

fn side_panel_view(data: &AppState) -> impl WidgetView<AppState> + use<> {
    let panel = if let Some(info) = panel_info(data) {
        panel_with_info(info).boxed()
    } else {
        panel_placeholder().boxed()
    };

    hover_clear(
        sized_box(panel)
            .width(320.px())
            .expand_height()
            .padding(12.0)
            .background_color(APP_BG),
        |data: &mut AppState| {
            data.set_panel_hovered(None);
        },
    )
}

fn panel_with_info(info: PanelInfo) -> impl WidgetView<AppState> + use<> {
    let mut items: Vec<AnyFlexChild<AppState>> = Vec::new();
    items.push(
        flex_row((
            label(info.title)
                .text_size(16.0)
                .weight(xilem::FontWeight::BOLD)
                .flex(1.0),
            label(format_bytes(info.size))
                .text_size(14.0)
                .color(xilem::Color::from_rgb8(200, 205, 214)),
        ))
        .cross_axis_alignment(CrossAxisAlignment::Center)
        .into_any_flex(),
    );
    items.push(
        label(info.path)
            .text_size(11.0)
            .color(xilem::Color::from_rgb8(120, 130, 148))
            .into_any_flex(),
    );
    items.push(FlexSpacer::Fixed(10.px()).into_any_flex());

    for entry in info.entries {
        items.push(panel_row(entry).into_any_flex());
    }

    if let Some(remainder) = info.remainder {
        items.push(FlexSpacer::Fixed(6.px()).into_any_flex());
        items.push(
            panel_row(PanelEntry {
                name: "smaller objects...".to_string(),
                size: remainder,
                color: xilem::Color::from_rgb8(90, 96, 110),
                node_id: None,
            })
            .into_any_flex(),
        );
    }

    flex_col(items)
        .cross_axis_alignment(CrossAxisAlignment::Start)
        .main_axis_alignment(MainAxisAlignment::Start)
        .must_fill_major_axis(true)
}

fn title_bar_view(data: &AppState) -> impl WidgetView<AppState> + use<> {
    let left_group = flex_row((
        button(
            label("<")
                .text_size(12.0)
                .color(xilem::Color::from_rgb8(210, 216, 228)),
            |data: &mut AppState| {
                data.focus_parent();
            },
        )
        .disabled(!data.can_focus_parent())
        .padding(Padding::from_vh(4.0, 6.0))
        .background_color(NAV_BG)
        .corner_radius(6.0)
        .border_width(0.0),
        FlexSpacer::Fixed(4.px()),
        button(
            label(">")
                .text_size(12.0)
                .color(xilem::Color::from_rgb8(210, 216, 228)),
            |_data: &mut AppState| {},
        )
        .disabled(true)
        .padding(Padding::from_vh(4.0, 6.0))
        .background_color(NAV_BG)
        .corner_radius(6.0)
        .border_width(0.0),
    ))
    .cross_axis_alignment(CrossAxisAlignment::Center)
    .main_axis_alignment(MainAxisAlignment::Start);

    let crumbs = breadcrumb_chips_view(data).flex(1.0);
    flex_row((left_group, FlexSpacer::Fixed(8.px()), crumbs))
        .cross_axis_alignment(CrossAxisAlignment::Center)
        .main_axis_alignment(MainAxisAlignment::Start)
        .padding(8.0)
        .background_color(TITLE_BAR_BG)
}

fn breadcrumb_chips_view(data: &AppState) -> impl WidgetView<AppState> + use<> {
    let mut items: Vec<AnyFlexChild<AppState>> = Vec::new();
    let chip_padding = Padding::from_vh(5.0, 10.0);
    if let Some(tree) = data.tree.as_ref() {
        let focus = data.focus.unwrap_or(tree.root);
        let nodes = breadcrumb_nodes(tree, focus);
        for (index, node_id) in nodes.iter().copied().enumerate() {
            let node = &tree.nodes[node_id];
            let is_last = index + 1 == nodes.len();
            if is_last {
                items.push(
                    sized_box(
                        label(node.name.clone())
                            .text_size(12.0)
                            .weight(xilem::FontWeight::BOLD)
                            .color(xilem::Color::from_rgb8(230, 236, 246)),
                    )
                    .padding(chip_padding)
                    .background_color(CHIP_BG)
                    .corner_radius(7.0)
                    .into_any_flex(),
                );
            } else {
                let name = node.name.clone();
                let target = node_id;
                let chip_color = if index == 0 {
                    CHIP_BG_ACTIVE
                } else {
                    CHIP_BG
                };
                items.push(
                    button(
                        label(name)
                            .text_size(12.0)
                            .color(xilem::Color::from_rgb8(230, 236, 246)),
                        move |data: &mut AppState| {
                            data.focus_node(target);
                        },
                    )
                    .disabled(data.scanning)
                    .padding(chip_padding)
                    .background_color(chip_color)
                    .corner_radius(7.0)
                    .border_width(0.0)
                    .into_any_flex(),
                );
            }
            if !is_last {
                items.push(FlexSpacer::Fixed(6.px()).into_any_flex());
            }
        }
    }

    if items.is_empty() {
        items.push(
            sized_box(
                label("No scan data")
                    .text_size(12.0)
                    .color(xilem::Color::from_rgb8(130, 140, 160)),
            )
            .padding(chip_padding)
            .background_color(CHIP_BG)
            .corner_radius(7.0)
                    .into_any_flex(),
        );
    }

    flex_row(items)
        .cross_axis_alignment(CrossAxisAlignment::Center)
        .main_axis_alignment(MainAxisAlignment::Start)
}

fn panel_row(entry: PanelEntry) -> impl WidgetView<AppState> + use<> {
    let row = flex_row((
        sized_box(label(""))
            .width(10.px())
            .height(10.px())
            .background_color(entry.color)
            .corner_radius(3.0),
        FlexSpacer::Fixed(8.px()),
        label(entry.name)
            .text_size(12.0)
            .color(xilem::Color::from_rgb8(222, 228, 236))
            .flex(1.0),
        label(format_bytes(entry.size))
            .text_size(12.0)
            .color(xilem::Color::from_rgb8(180, 186, 198)),
    ))
    .cross_axis_alignment(CrossAxisAlignment::Center)
    .main_axis_alignment(MainAxisAlignment::Start);

    if let Some(node_id) = entry.node_id {
        hover_wrap(row, node_id, |data: &mut AppState, action| match action {
            HoverWrapAction::Entered(node_id) => data.set_panel_hovered(Some(node_id)),
            HoverWrapAction::Exited(node_id) => {
                if data.panel_hovered == Some(node_id) {
                    data.set_panel_hovered(None);
                }
            }
            HoverWrapAction::Clicked(node_id) => {
                data.handle_flame_action(FlameGraphAction::Selected(Some(node_id)));
            }
        })
        .boxed()
    } else {
        row.boxed()
    }
}

fn panel_placeholder() -> impl WidgetView<AppState> + use<> {
    flex_col((
        label("No scan data")
            .text_size(14.0)
            .weight(xilem::FontWeight::BOLD),
        FlexSpacer::Fixed(6.px()),
        label("Run a scan and hover the flame graph to preview folders.")
            .text_size(11.0)
            .color(xilem::Color::from_rgb8(140, 150, 168)),
    ))
    .cross_axis_alignment(CrossAxisAlignment::Start)
    .main_axis_alignment(MainAxisAlignment::Start)
    .must_fill_major_axis(true)
}

fn panel_info(data: &AppState) -> Option<PanelInfo> {
    let tree = data.tree.as_ref()?;
    let focus = data.focus.unwrap_or(tree.root);
    let target = data
        .display_hovered()
        .or(data.selected)
        .or(Some(focus))
        .filter(|id| *id < tree.nodes.len())?;
    let node = tree.nodes.get(target)?;

    let mut children = node.children.clone();
    children.sort_by_key(|child| std::cmp::Reverse(tree.nodes[*child].size));

    let mut entries = Vec::new();
    let mut total: u64 = 0;
    for child_id in children.iter().take(10) {
        let child = &tree.nodes[*child_id];
        total = total.saturating_add(child.size);
        let depth = depth_from_focus(tree, *child_id, focus);
        let group_id = top_level_group(tree, *child_id, focus);
        let group_hash = hash_node(&tree.nodes[group_id]);
        let node_hash = hash_node(child);
        entries.push(PanelEntry {
            name: child.name.clone(),
            size: child.size,
            color: grouped_color(group_hash, node_hash, depth),
            node_id: Some(*child_id),
        });
    }

    let remainder = node.size.checked_sub(total).filter(|value| *value > 0);

    Some(PanelInfo {
        title: node.name.clone(),
        path: node.path.display().to_string(),
        size: node.size,
        entries,
        remainder,
    })
}

fn breadcrumb_nodes(tree: &FsTree, focus: usize) -> Vec<usize> {
    let mut nodes = Vec::new();
    let mut current = Some(focus);
    while let Some(node_id) = current {
        if node_id >= tree.nodes.len() {
            break;
        }
        nodes.push(node_id);
        current = tree.nodes[node_id].parent;
    }
    nodes.reverse();
    nodes
}

fn depth_from_focus(tree: &FsTree, node_id: usize, focus: usize) -> usize {
    if node_id == focus {
        return 0;
    }
    let mut depth = 0;
    let mut current = node_id;
    while current != focus {
        let Some(parent) = tree.nodes.get(current).and_then(|node| node.parent) else {
            break;
        };
        depth += 1;
        current = parent;
    }
    depth
}

fn top_level_group(tree: &FsTree, node_id: usize, focus: usize) -> usize {
    if node_id == focus {
        return focus;
    }
    let mut current = node_id;
    while let Some(parent) = tree.nodes.get(current).and_then(|node| node.parent) {
        if parent == focus {
            return current;
        }
        current = parent;
    }
    focus
}

fn hash_node(node: &FsNode) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    node.name.hash(&mut hasher);
    node.path.hash(&mut hasher);
    hasher.finish()
}

fn scan_worker_task(
    proxy: MessageProxy<ScanResponse>,
    mut rx: UnboundedReceiver<ScanRequest>,
) -> impl std::future::Future<Output = ()> {
    async move {
        while let Some(request) = rx.recv().await {
            let path = request.path;
            let cancel = request.cancel;
            let start = Instant::now();
            let path_for_scan = path.clone();
            let result = xilem::tokio::task::spawn_blocking(move || {
                scan::scan_path_with_cancel(&path_for_scan, &cancel)
            })
            .await;
            let duration = start.elapsed();
            let response = match result {
                Ok(Ok(scan::ScanOutcome::Completed(tree))) => ScanResponse {
                    path,
                    duration,
                    result: ScanResult::Completed(Ok(tree)),
                },
                Ok(Ok(scan::ScanOutcome::Cancelled)) => ScanResponse {
                    path,
                    duration,
                    result: ScanResult::Cancelled,
                },
                Ok(Err(err)) => ScanResponse {
                    path,
                    duration,
                    result: ScanResult::Completed(Err(err.to_string())),
                },
                Err(err) => ScanResponse {
                    path,
                    duration,
                    result: ScanResult::Completed(Err(err.to_string())),
                },
            };
            if proxy.message(response).is_err() {
                break;
            }
        }
    }
}

fn default_scan_path() -> PathBuf {
    env::var_os("HOME")
        .or_else(|| env::var_os("USERPROFILE"))
        .map(PathBuf::from)
        .unwrap_or_else(|| env::current_dir().unwrap_or_else(|_| PathBuf::from(".")))
}

fn normalize_input_path(input: &str) -> PathBuf {
    let trimmed = input.trim();
    if trimmed == "~" {
        return default_scan_path();
    }
    if trimmed.starts_with("~/") || trimmed.starts_with("~\\") {
        if let Some(home) = env::var_os("HOME").or_else(|| env::var_os("USERPROFILE")) {
            let mut path = PathBuf::from(home);
            let suffix = trimmed.trim_start_matches("~").trim_start_matches(['/', '\\']);
            path.push(suffix);
            return path;
        }
    }
    PathBuf::from(trimmed)
}

fn node_label(node: &FsNode) -> String {
    match node.kind {
        scan::NodeKind::Directory => format!("Dir: {}", node.name),
        scan::NodeKind::File => format!("File: {}", node.name),
    }
}

fn run(event_loop: EventLoopBuilder) -> Result<(), winit::error::EventLoopError> {
    let window_options = WindowOptions::new("FireSpace")
        .with_min_inner_size(LogicalSize::new(960.0, 640.0))
        .with_initial_inner_size(LogicalSize::new(1200.0, 820.0));
    let app = Xilem::new_simple(AppState::new(), app_logic, window_options);
    app.run_in(event_loop)?;
    Ok(())
}

fn main() -> Result<(), winit::error::EventLoopError> {
    run(EventLoop::with_user_event())
}
