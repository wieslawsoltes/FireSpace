use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::time::{Duration, Instant};

use vello::Scene;
use vello::kurbo::{Affine, BezPath, Circle, Point, Rect, RoundedRect, Size, Vec2};
use vello::peniko::{BlendMode, Brush, Color, Fill};
use xilem::core::{MessageContext, MessageResult, Mut, View, ViewMarker};
use xilem::masonry::accesskit::{Node, Role};
use xilem::masonry::core::{
    AccessCtx, BoxConstraints, BrushIndex, ChildrenIds, EventCtx, LayoutCtx, PaintCtx, PointerButton,
    PointerEvent, PropertiesMut, PropertiesRef, RegisterCtx, StyleProperty, UpdateCtx, Widget,
    WidgetMut, render_text,
};
use xilem::masonry::util::{fill_color, stroke};
use xilem::{Pod, ViewCtx};

use crate::flame::{
    FlameRect, LayoutSettings, RadialSegment, format_bytes, grouped_color, layout_flame_graph,
    layout_radial_graph, mix_color,
};
use crate::scan::FsTree;

#[derive(Clone, Debug)]
pub enum FlameGraphAction {
    Hovered(Option<usize>),
    Selected(Option<usize>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FlameGraphMode {
    Radial,
    Flame,
}

pub fn flame_graph<State, Action>(
    data: Option<Arc<FsTree>>,
    focus: Option<usize>,
    selected: Option<usize>,
    mode: FlameGraphMode,
    external_hovered: Option<usize>,
    on_action: impl Fn(&mut State, FlameGraphAction) -> Action + Send + Sync + 'static,
) -> FlameGraphView<impl Fn(&mut State, FlameGraphAction) -> Action + Send + Sync + 'static> {
    FlameGraphView {
        data,
        focus,
        selected,
        mode,
        external_hovered,
        on_action,
    }
}

pub struct FlameGraphView<F> {
    data: Option<Arc<FsTree>>,
    focus: Option<usize>,
    selected: Option<usize>,
    mode: FlameGraphMode,
    external_hovered: Option<usize>,
    on_action: F,
}

impl<F> ViewMarker for FlameGraphView<F> {}

impl<F, State, Action> View<State, Action, ViewCtx> for FlameGraphView<F>
where
    State: 'static,
    Action: 'static,
    F: Fn(&mut State, FlameGraphAction) -> Action + Send + Sync + 'static,
{
    type Element = Pod<FlameGraphWidget>;
    type ViewState = ();

    fn build(&self, ctx: &mut ViewCtx, _: &mut State) -> (Self::Element, Self::ViewState) {
        (
            ctx.with_action_widget(|ctx| {
                ctx.create_pod(FlameGraphWidget::new(
                    self.data.clone(),
                    self.focus,
                    self.selected,
                    self.mode,
                    self.external_hovered,
                ))
            }),
            (),
        )
    }

    fn rebuild(
        &self,
        prev: &Self,
        (): &mut Self::ViewState,
        _: &mut ViewCtx,
        mut element: Mut<'_, Self::Element>,
        _: &mut State,
    ) {
        if !same_data(&self.data, &prev.data) {
            FlameGraphWidget::set_data(&mut element, self.data.clone());
        }
        if self.focus != prev.focus {
            FlameGraphWidget::set_focus(&mut element, self.focus);
        }
        if self.selected != prev.selected {
            FlameGraphWidget::set_selected(&mut element, self.selected);
        }
        if self.mode != prev.mode {
            FlameGraphWidget::set_mode(&mut element, self.mode);
        }
        if self.external_hovered != prev.external_hovered {
            FlameGraphWidget::set_external_hovered(&mut element, self.external_hovered);
        }
    }

    fn teardown(
        &self,
        (): &mut Self::ViewState,
        ctx: &mut ViewCtx,
        element: Mut<'_, Self::Element>,
    ) {
        ctx.teardown_leaf(element);
    }

    fn message(
        &self,
        (): &mut Self::ViewState,
        message: &mut MessageContext,
        _: Mut<'_, Self::Element>,
        app_state: &mut State,
    ) -> MessageResult<Action> {
        if message.take_first().is_some() {
            return MessageResult::Stale;
        }
        match message.take_message::<FlameGraphAction>() {
            Some(action) => MessageResult::Action((self.on_action)(app_state, *action)),
            None => MessageResult::Stale,
        }
    }
}

fn same_data(left: &Option<Arc<FsTree>>, right: &Option<Arc<FsTree>>) -> bool {
    match (left, right) {
        (Some(a), Some(b)) => Arc::ptr_eq(a, b),
        (None, None) => true,
        _ => false,
    }
}

pub struct FlameGraphWidget {
    data: Option<Arc<FsTree>>,
    focus: Option<usize>,
    selected: Option<usize>,
    mode: FlameGraphMode,
    external_hovered: Option<usize>,
    hovered: Option<usize>,
    flame_layout: Vec<FlameRect>,
    radial_layout: Vec<RadialSegment>,
    radial_center: Point,
    layout_dirty: bool,
    settings: LayoutSettings,
    size: Size,
    transition: Option<GraphTransition>,
}

#[derive(Clone)]
struct GraphTransition {
    start: Option<Instant>,
    duration: Duration,
    prev_mode: FlameGraphMode,
    prev_focus: Option<usize>,
    prev_flame_layout: Vec<FlameRect>,
    prev_radial_layout: Vec<RadialSegment>,
    prev_radial_center: Point,
}

const NAV_ANIM_DURATION: Duration = Duration::from_millis(520);

impl FlameGraphWidget {
    pub fn new(
        data: Option<Arc<FsTree>>,
        focus: Option<usize>,
        selected: Option<usize>,
        mode: FlameGraphMode,
        external_hovered: Option<usize>,
    ) -> Self {
        Self {
            data,
            focus,
            selected,
            mode,
            external_hovered,
            hovered: None,
            flame_layout: Vec::new(),
            radial_layout: Vec::new(),
            radial_center: Point::ORIGIN,
            layout_dirty: true,
            settings: LayoutSettings::default(),
            size: Size::new(0.0, 0.0),
            transition: None,
        }
    }

    pub fn set_data(this: &mut WidgetMut<'_, Self>, data: Option<Arc<FsTree>>) {
        this.widget.data = data;
        this.widget.hovered = None;
        this.widget.selected = None;
        this.widget.flame_layout.clear();
        this.widget.radial_layout.clear();
        this.widget.transition = None;
        this.widget.layout_dirty = true;
        this.ctx.request_layout();
        this.ctx.request_paint_only();
    }

    pub fn set_focus(this: &mut WidgetMut<'_, Self>, focus: Option<usize>) {
        if this.widget.focus != focus {
            if this.widget.begin_transition() {
                this.ctx.request_anim_frame();
            }
            this.widget.focus = focus;
            this.widget.layout_dirty = true;
            this.ctx.request_layout();
            this.ctx.request_paint_only();
        }
    }

    pub fn set_mode(this: &mut WidgetMut<'_, Self>, mode: FlameGraphMode) {
        if this.widget.mode != mode {
            this.widget.transition = None;
            this.widget.mode = mode;
            this.widget.layout_dirty = true;
            this.ctx.request_layout();
            this.ctx.request_paint_only();
        }
    }

    pub fn set_external_hovered(this: &mut WidgetMut<'_, Self>, hovered: Option<usize>) {
        if this.widget.external_hovered != hovered {
            this.widget.external_hovered = hovered;
            this.ctx.request_paint_only();
        }
    }

    pub fn set_selected(this: &mut WidgetMut<'_, Self>, selected: Option<usize>) {
        if this.widget.selected != selected {
            this.widget.selected = selected;
            this.ctx.request_paint_only();
        }
    }

    fn begin_transition(&mut self) -> bool {
        let prev_mode = self.mode;
        let prev_focus = self.focus;
        let prev_flame = self.flame_layout.clone();
        let prev_radial = self.radial_layout.clone();
        let prev_center = self.radial_center;
        let has_prev = match prev_mode {
            FlameGraphMode::Flame => !prev_flame.is_empty(),
            FlameGraphMode::Radial => !prev_radial.is_empty(),
        };
        if !has_prev {
            return false;
        }
        self.transition = Some(GraphTransition {
            start: None,
            duration: NAV_ANIM_DURATION,
            prev_mode,
            prev_focus,
            prev_flame_layout: prev_flame,
            prev_radial_layout: prev_radial,
            prev_radial_center: prev_center,
        });
        true
    }

    fn rebuild_layout(&mut self) {
        self.flame_layout.clear();
        self.radial_layout.clear();
        let Some(tree) = self.data.as_ref() else {
            return;
        };
        let root = self
            .focus
            .filter(|id| *id < tree.nodes.len())
            .unwrap_or(tree.root);
        let bounds = Rect::from_origin_size(Point::ORIGIN, self.size);
        match self.mode {
            FlameGraphMode::Flame => {
                self.flame_layout = layout_flame_graph(tree, root, bounds, &self.settings);
            }
            FlameGraphMode::Radial => {
                let (segments, center) = layout_radial_graph(tree, root, bounds, &self.settings);
                self.radial_layout = segments;
                self.radial_center = center;
            }
        }
    }

    fn hit_test(&self, point: Point) -> Option<usize> {
        match self.mode {
            FlameGraphMode::Flame => self
                .flame_layout
                .iter()
                .rev()
                .find(|item| item.rect.contains(point))
                .map(|item| item.node),
            FlameGraphMode::Radial => hit_test_radial(&self.radial_layout, self.radial_center, point),
        }
    }

}

impl Widget for FlameGraphWidget {
    type Action = FlameGraphAction;

    fn register_children(&mut self, _ctx: &mut RegisterCtx<'_>) {}

    fn layout(
        &mut self,
        _ctx: &mut LayoutCtx<'_>,
        _props: &mut PropertiesMut<'_>,
        bc: &BoxConstraints,
    ) -> Size {
        let fallback = Size::new(900.0, 600.0);
        let size = bc.constrain(bc.bounded_or(fallback));
        if size != self.size || self.layout_dirty {
            self.size = size;
            self.rebuild_layout();
            self.layout_dirty = false;
        }
        size
    }

    fn paint(&mut self, ctx: &mut PaintCtx<'_>, _props: &PropertiesRef<'_>, scene: &mut Scene) {
        let size = ctx.size();
        let background = Color::from_rgb8(45, 48, 58);
        let border = Color::from_rgba8(255, 255, 255, 18);
        let hover_overlay = Color::from_rgb8(255, 240, 200);
        let select_overlay = Color::from_rgb8(120, 200, 255);
        let hover_node = self.hovered.or(self.external_hovered);
        if let Some(transition) = self.transition.as_mut() {
            if transition.start.is_none() {
                transition.start = Some(Instant::now());
            }
        }
        let transition_snapshot = self.transition.clone();
        let mut transition_t = 1.0;
        let mut draw_transition = false;
        let mut clear_transition = false;
        if let Some(transition) = transition_snapshot.as_ref() {
            if transition.prev_mode != self.mode {
                clear_transition = true;
            } else if let Some(start) = transition.start {
                let elapsed = start.elapsed();
                if elapsed >= transition.duration {
                    clear_transition = true;
                } else {
                    let raw_t = elapsed.as_secs_f64() / transition.duration.as_secs_f64();
                    transition_t = ease_out_cubic(raw_t);
                    draw_transition = true;
                }
            } else {
                draw_transition = true;
                transition_t = 0.0;
            }
        }
        if clear_transition {
            self.transition = None;
        }

        fill_color(
            scene,
            &Rect::from_origin_size(Point::ORIGIN, size),
            background,
        );
        let Some(tree) = self.data.as_ref() else {
            return;
        };

        match self.mode {
            FlameGraphMode::Flame => {
                if self.flame_layout.is_empty() {
                    return;
                }

                if draw_transition {
                    let Some(transition) = transition_snapshot.as_ref() else {
                        return;
                    };
                    let prev_focus = transition.prev_focus.unwrap_or(tree.root);
                    draw_flame_layout(
                        ctx,
                        scene,
                        &transition.prev_flame_layout,
                        tree,
                        prev_focus,
                        None,
                        None,
                        1.0 - transition_t,
                        border,
                        hover_overlay,
                        select_overlay,
                        false,
                    );
                }

                let focus_id = self.focus.unwrap_or(tree.root);
                draw_flame_layout(
                    ctx,
                    scene,
                    &self.flame_layout,
                    tree,
                    focus_id,
                    self.selected,
                    hover_node,
                    transition_t,
                    border,
                    hover_overlay,
                    select_overlay,
                    true,
                );

                if let Some(target) = hover_node {
                    if let Some(item) =
                        self.flame_layout.iter().find(|item| item.node == target)
                    {
                        draw_tooltip_for_node(ctx, scene, item.rect, tree, target, size, self.focus);
                    }
                }
            }
            FlameGraphMode::Radial => {
                if self.radial_layout.is_empty() {
                    return;
                }

                if draw_transition {
                    let Some(transition) = transition_snapshot.as_ref() else {
                        return;
                    };
                    let prev_focus = transition.prev_focus.unwrap_or(tree.root);
                    draw_radial_layout(
                        ctx,
                        scene,
                        &transition.prev_radial_layout,
                        tree,
                        prev_focus,
                        transition.prev_radial_center,
                        None,
                        None,
                        1.0 - transition_t,
                        border,
                        hover_overlay,
                        select_overlay,
                        false,
                    );
                }

                let focus_id = self
                    .focus
                    .filter(|id| *id < tree.nodes.len())
                    .unwrap_or(tree.root);
                draw_radial_layout(
                    ctx,
                    scene,
                    &self.radial_layout,
                    tree,
                    focus_id,
                    self.radial_center,
                    self.selected,
                    hover_node,
                    transition_t,
                    border,
                    hover_overlay,
                    select_overlay,
                    true,
                );

                let center_radius = (self.settings.radial_inner_radius - 4.0)
                    .max(CENTER_LABEL_MIN_RADIUS);
                draw_radial_center_label(
                    ctx,
                    scene,
                    self.radial_center,
                    center_radius,
                    tree.nodes[focus_id].size,
                );

                if let Some(target) = hover_node {
                    if let Some(segment) =
                        self.radial_layout.iter().find(|item| item.node == target)
                    {
                        let anchor = radial_anchor_rect(self.radial_center, segment);
                        draw_tooltip_for_node(ctx, scene, anchor, tree, target, size, self.focus);
                    }
                }
            }
        }
    }

    fn on_pointer_event(
        &mut self,
        ctx: &mut EventCtx<'_>,
        _props: &mut PropertiesMut<'_>,
        event: &PointerEvent,
    ) {
        match event {
            PointerEvent::Move(update) => {
                let local = ctx.local_position(update.current.position);
                let hit = self.hit_test(local);
                if hit != self.hovered {
                    self.hovered = hit;
                    ctx.submit_action::<FlameGraphAction>(FlameGraphAction::Hovered(hit));
                    ctx.request_paint_only();
                }
            }
            PointerEvent::Leave(_) => {
                if self.hovered.is_some() {
                    self.hovered = None;
                    ctx.submit_action::<FlameGraphAction>(FlameGraphAction::Hovered(None));
                    ctx.request_paint_only();
                }
            }
            PointerEvent::Down(event) => {
                if event.button == Some(PointerButton::Primary) {
                    let local = ctx.local_position(event.state.position);
                    if self.mode == FlameGraphMode::Radial {
                        if let Some(tree) = self.data.as_ref() {
                            let focus = self
                                .focus
                                .filter(|id| *id < tree.nodes.len())
                                .unwrap_or(tree.root);
                            if let Some(parent) =
                                tree.nodes.get(focus).and_then(|node| node.parent)
                            {
                                let center_radius = (self.settings.radial_inner_radius - 4.0)
                                    .max(CENTER_LABEL_MIN_RADIUS);
                                let dx = local.x - self.radial_center.x;
                                let dy = local.y - self.radial_center.y;
                                if dx * dx + dy * dy <= center_radius * center_radius {
                                    if Some(parent) != self.selected {
                                        self.selected = Some(parent);
                                        ctx.submit_action::<FlameGraphAction>(
                                            FlameGraphAction::Selected(Some(parent)),
                                        );
                                        ctx.request_paint_only();
                                    }
                                    return;
                                }
                            }
                        }
                    }
                    let hit = self.hit_test(local);
                    if hit != self.selected {
                        self.selected = hit;
                        ctx.submit_action::<FlameGraphAction>(FlameGraphAction::Selected(hit));
                        ctx.request_paint_only();
                    }
                }
            }
            _ => {}
        }
    }

    fn on_anim_frame(
        &mut self,
        ctx: &mut UpdateCtx<'_>,
        _props: &mut PropertiesMut<'_>,
        _interval: u64,
    ) {
        let mut clear_transition = false;
        if let Some(transition) = self.transition.as_mut() {
            let start = transition.start.get_or_insert_with(Instant::now);
            if start.elapsed() < transition.duration {
                ctx.request_anim_frame();
                ctx.request_paint_only();
            } else {
                clear_transition = true;
            }
        }
        if clear_transition {
            self.transition = None;
            ctx.request_paint_only();
        }
    }

    fn accessibility_role(&self) -> Role {
        Role::Image
    }

    fn accessibility(
        &mut self,
        _ctx: &mut AccessCtx<'_>,
        _props: &PropertiesRef<'_>,
        node: &mut Node,
    ) {
        node.set_label("Flame graph visualization");
    }

    fn children_ids(&self) -> ChildrenIds {
        ChildrenIds::new()
    }
}

const LABEL_FONT_SIZE: f32 = 11.0;
const LABEL_PADDING: f64 = 4.0;
const LABEL_MIN_WIDTH: f64 = 60.0;
const RADIAL_LABEL_PADDING: f64 = 2.0;
const RADIAL_LABEL_MIN_ARC: f64 = 48.0;
const CENTER_LABEL_MIN_RADIUS: f64 = 12.0;
const CENTER_LABEL_PADDING: f64 = 8.0;

fn apply_alpha(mut color: Color, alpha: f64) -> Color {
    color.components[3] = (color.components[3] * alpha as f32).clamp(0.0, 1.0);
    color
}

fn ease_out_cubic(t: f64) -> f64 {
    let t = t.clamp(0.0, 1.0);
    1.0 - (1.0 - t).powi(3)
}

fn draw_label_if_fits(
    ctx: &mut PaintCtx<'_>,
    scene: &mut Scene,
    item: &FlameRect,
    text: &str,
    bar_color: Color,
    label_alpha: f64,
) {
    let rect = item.rect;
    if rect.width() < LABEL_MIN_WIDTH || rect.height() < 14.0 {
        return;
    }
    let max_width = rect.width() - LABEL_PADDING * 2.0;
    if max_width <= 4.0 {
        return;
    }
    let layout = build_text_layout(ctx, text, max_width, LABEL_FONT_SIZE);
    let text_width = layout.width() as f64;
    let text_height = layout.height() as f64;
    if text_width > max_width || text_height > rect.height() - 2.0 {
        return;
    }

    let text_color = apply_alpha(readable_text_color(bar_color), label_alpha);
    let x = rect.x0 + LABEL_PADDING;
    let y = rect.y0 + (rect.height() - text_height) * 0.5;
    let clip_rect = rect.inset(-0.5);
    scene.push_layer(BlendMode::default(), 1.0, vello::kurbo::Affine::IDENTITY, &clip_rect);
    render_text(
        scene,
        vello::kurbo::Affine::translate((x, y)),
        &layout,
        &[Brush::Solid(text_color)],
        true,
    );
    scene.pop_layer();
}

fn draw_flame_layout(
    ctx: &mut PaintCtx<'_>,
    scene: &mut Scene,
    layout: &[FlameRect],
    tree: &FsTree,
    focus: usize,
    selected: Option<usize>,
    hover_node: Option<usize>,
    alpha: f64,
    border: Color,
    hover_overlay: Color,
    select_overlay: Color,
    draw_labels: bool,
) {
    if alpha <= 0.0 {
        return;
    }
    let border = apply_alpha(border, alpha);
    for item in layout {
        let node = &tree.nodes[item.node];
        let group_id = top_level_group(tree, item.node, focus);
        let group_hash = hash_node(&tree.nodes[group_id]);
        let node_hash = hash_node(node);
        let mut color = grouped_color(group_hash, node_hash, item.depth);
        if Some(item.node) == selected {
            color = mix_color(color, select_overlay, 0.3);
        }
        if Some(item.node) == hover_node {
            color = mix_color(color, hover_overlay, 0.4);
        }
        let color = apply_alpha(color, alpha);

        if item.rect.width() > 4.0 && item.rect.height() > 4.0 {
            let shape = RoundedRect::from_rect(item.rect, 2.0);
            fill_color(scene, &shape, color);
            stroke(scene, &shape, border, 0.7);
        } else {
            fill_color(scene, &item.rect, color);
        }

        if draw_labels {
            draw_label_if_fits(ctx, scene, item, node.name.as_ref(), color, alpha);
        }
    }
}

fn draw_radial_label_if_fits(
    ctx: &mut PaintCtx<'_>,
    scene: &mut Scene,
    segment: &RadialSegment,
    text: &str,
    bar_color: Color,
    center: Point,
    label_alpha: f64,
) {
    let band = segment.outer_radius - segment.inner_radius;
    if band < 12.0 {
        return;
    }
    let sweep = segment.sweep.abs();
    let mid_radius = (segment.inner_radius + segment.outer_radius) * 0.5;
    let arc_len = mid_radius * sweep;
    if arc_len < RADIAL_LABEL_MIN_ARC {
        return;
    }
    let max_width = (arc_len - RADIAL_LABEL_PADDING * 2.0).min(240.0);
    if max_width <= 8.0 {
        return;
    }
    let layout = build_text_layout(ctx, text, max_width, LABEL_FONT_SIZE);
    let mut lines = layout.lines();
    let Some(line) = lines.next() else {
        return;
    };
    if lines.next().is_some() {
        return;
    }
    let metrics = line.metrics();
    let text_width = (metrics.advance - metrics.trailing_whitespace) as f64;
    let text_height = metrics.line_height as f64;
    if text_width <= 0.0
        || text_width + RADIAL_LABEL_PADDING * 2.0 > arc_len
        || text_height + RADIAL_LABEL_PADDING * 2.0 > band
    {
        return;
    }

    let text_color = apply_alpha(readable_text_color(bar_color), label_alpha);
    let mid_angle = segment.start_angle + segment.sweep * 0.5;
    let text_angle = text_width / mid_radius;
    let mut direction = 1.0;
    let mut start_angle = mid_angle - text_angle * 0.5;
    if mid_angle > std::f64::consts::FRAC_PI_2
        && mid_angle < std::f64::consts::FRAC_PI_2 * 3.0
    {
        direction = -1.0;
        start_angle = mid_angle + text_angle * 0.5;
    }

    render_text_along_arc(
        scene,
        &layout,
        text_color,
        center,
        mid_radius,
        start_angle,
        direction,
    );
}

fn draw_radial_layout(
    ctx: &mut PaintCtx<'_>,
    scene: &mut Scene,
    layout: &[RadialSegment],
    tree: &FsTree,
    focus: usize,
    center: Point,
    selected: Option<usize>,
    hover_node: Option<usize>,
    alpha: f64,
    border: Color,
    hover_overlay: Color,
    select_overlay: Color,
    draw_labels: bool,
) {
    if alpha <= 0.0 {
        return;
    }
    let border = apply_alpha(border, alpha);
    for segment in layout {
        let node = &tree.nodes[segment.node];
        let group_id = top_level_group(tree, segment.node, focus);
        let group_hash = hash_node(&tree.nodes[group_id]);
        let node_hash = hash_node(node);
        let mut color = grouped_color(group_hash, node_hash, segment.depth);
        if Some(segment.node) == selected {
            color = mix_color(color, select_overlay, 0.3);
        }
        if Some(segment.node) == hover_node {
            color = mix_color(color, hover_overlay, 0.4);
        }
        let color = apply_alpha(color, alpha);

        let path = ring_segment_path(
            center,
            segment.inner_radius,
            segment.outer_radius,
            segment.start_angle,
            segment.sweep,
        );
        fill_color(scene, &path, color);
        stroke(scene, &path, border, 0.7);
        if draw_labels {
            draw_radial_label_if_fits(
                ctx,
                scene,
                segment,
                node.name.as_ref(),
                color,
                center,
                alpha,
            );
        }
    }
}

const TOOLTIP_FONT_SIZE: f32 = 12.0;
const TOOLTIP_SECONDARY_SIZE: f32 = 11.0;
const TOOLTIP_PADDING: f64 = 8.0;
const TOOLTIP_GAP: f64 = 4.0;
const TOOLTIP_MAX_WIDTH: f64 = 360.0;

fn draw_tooltip(
    ctx: &mut PaintCtx<'_>,
    scene: &mut Scene,
    anchor: Rect,
    title: &str,
    size_text: String,
    percent: f64,
    path: String,
    bounds: Size,
) {
    let line_one = title.to_string();
    let line_two = format!("{size_text} | {percent:.1}% of focus");
    let line_three = path;

    let layout_one = build_text_layout(ctx, &line_one, TOOLTIP_MAX_WIDTH, TOOLTIP_FONT_SIZE);
    let layout_two = build_text_layout(ctx, &line_two, TOOLTIP_MAX_WIDTH, TOOLTIP_SECONDARY_SIZE);
    let layout_three = build_text_layout(ctx, &line_three, TOOLTIP_MAX_WIDTH, TOOLTIP_SECONDARY_SIZE);

    let width = layout_one
        .width()
        .max(layout_two.width())
        .max(layout_three.width()) as f64;
    let height = layout_one.height() as f64
        + layout_two.height() as f64
        + layout_three.height() as f64
        + TOOLTIP_GAP * 2.0;

    let tooltip_width = (width + TOOLTIP_PADDING * 2.0).clamp(120.0, TOOLTIP_MAX_WIDTH + 20.0);
    let tooltip_height = height + TOOLTIP_PADDING * 2.0;

    let mut x = anchor.x0.min(bounds.width - tooltip_width - 6.0).max(6.0);
    let mut y = anchor.y0 - tooltip_height - 8.0;
    if y < 6.0 {
        y = anchor.y1 + 8.0;
        if y + tooltip_height > bounds.height {
            y = (bounds.height - tooltip_height - 6.0).max(6.0);
        }
    }
    if x + tooltip_width > bounds.width {
        x = (bounds.width - tooltip_width - 6.0).max(6.0);
    }

    let tooltip_rect = Rect::new(x, y, x + tooltip_width, y + tooltip_height);
    let tooltip_bg = Color::from_rgba8(34, 38, 48, 230);
    let tooltip_border = Color::from_rgba8(255, 255, 255, 40);
    let tooltip_text = Color::from_rgb8(230, 235, 242);
    let tooltip_secondary = Color::from_rgb8(165, 175, 190);

    let shape = RoundedRect::from_rect(tooltip_rect, 6.0);
    fill_color(scene, &shape, tooltip_bg);
    stroke(scene, &shape, tooltip_border, 1.0);

    let mut cursor_y = tooltip_rect.y0 + TOOLTIP_PADDING;
    render_text(
        scene,
        vello::kurbo::Affine::translate((tooltip_rect.x0 + TOOLTIP_PADDING, cursor_y)),
        &layout_one,
        &[Brush::Solid(tooltip_text)],
        true,
    );
    cursor_y += layout_one.height() as f64 + TOOLTIP_GAP;
    render_text(
        scene,
        vello::kurbo::Affine::translate((tooltip_rect.x0 + TOOLTIP_PADDING, cursor_y)),
        &layout_two,
        &[Brush::Solid(tooltip_secondary)],
        true,
    );
    cursor_y += layout_two.height() as f64 + TOOLTIP_GAP;
    render_text(
        scene,
        vello::kurbo::Affine::translate((tooltip_rect.x0 + TOOLTIP_PADDING, cursor_y)),
        &layout_three,
        &[Brush::Solid(tooltip_secondary)],
        true,
    );
}

fn draw_tooltip_for_node(
    ctx: &mut PaintCtx<'_>,
    scene: &mut Scene,
    anchor: Rect,
    tree: &FsTree,
    node_id: usize,
    size: Size,
    focus: Option<usize>,
) {
    let focus = focus.filter(|id| *id < tree.nodes.len()).unwrap_or(tree.root);
    let focus_size = tree.nodes[focus].size.max(1) as f64;
    let node = &tree.nodes[node_id];
    let percent = node.size as f64 / focus_size * 100.0;
    draw_tooltip(
        ctx,
        scene,
        anchor,
        &node.name,
        format_bytes(node.size),
        percent,
        node.path.display().to_string(),
        size,
    );
}

fn build_text_layout(
    ctx: &mut PaintCtx<'_>,
    text: &str,
    max_width: f64,
    font_size: f32,
) -> xilem::masonry::parley::Layout<BrushIndex> {
    let (font_ctx, layout_ctx) = ctx.text_contexts();
    let mut layout = xilem::masonry::parley::Layout::<BrushIndex>::new();
    let mut builder = layout_ctx.ranged_builder(font_ctx, text, 1.0, true);
    builder.push_default(StyleProperty::FontSize(font_size));
    builder.push_default(StyleProperty::Brush(BrushIndex(0)));
    builder.build_into(&mut layout, text);
    layout.break_all_lines(Some(max_width as f32));
    layout
}

fn draw_radial_center_label(
    ctx: &mut PaintCtx<'_>,
    scene: &mut Scene,
    center: Point,
    radius: f64,
    size: u64,
) {
    if radius <= 0.0 {
        return;
    }

    let circle = Circle::new(center, radius);
    let background = Color::from_rgb8(33, 36, 46);
    let border = Color::from_rgba8(255, 255, 255, 24);
    fill_color(scene, &circle, background);
    stroke(scene, &circle, border, 1.0);

    let max_width = (radius * 2.0 - CENTER_LABEL_PADDING * 2.0).max(0.0);
    let max_height = max_width;
    if max_width <= 6.0 || max_height <= 6.0 {
        return;
    }

    let text = format_bytes(size);
    let (value_text, unit_text) = text
        .rsplit_once(' ')
        .map(|(value, unit)| (value.to_string(), unit.to_string()))
        .unwrap_or((text, String::new()));

    let mut value_size = (radius * 0.55).clamp(12.0, 24.0) as f32;
    let mut unit_size = (value_size * 0.7).clamp(10.0, 18.0);
    let mut spacing = 2.0;

    let mut value_layout = build_text_layout(ctx, &value_text, max_width, value_size);
    let mut unit_layout = build_text_layout(ctx, &unit_text, max_width, unit_size);

    for _ in 0..3 {
        let value_width = value_layout.width() as f64;
        let unit_width = unit_layout.width() as f64;
        let total_height =
            value_layout.height() as f64 + unit_layout.height() as f64 + spacing;
        let max_line_width = value_width.max(unit_width);
        if max_line_width <= max_width && total_height <= max_height {
            break;
        }
        value_size = (value_size * 0.85).max(10.0);
        unit_size = (unit_size * 0.85).max(8.0);
        spacing = (spacing * 0.85).max(1.0);
        value_layout = build_text_layout(ctx, &value_text, max_width, value_size);
        unit_layout = build_text_layout(ctx, &unit_text, max_width, unit_size);
    }

    let text_color = Color::from_rgb8(232, 238, 248);
    let value_width = value_layout.width() as f64;
    let unit_width = unit_layout.width() as f64;
    let value_height = value_layout.height() as f64;
    let unit_height = unit_layout.height() as f64;
    let total_height = value_height + unit_height + spacing;
    let start_y = center.y - total_height * 0.5;

    let value_x = center.x - value_width * 0.5;
    let value_y = start_y;
    render_text(
        scene,
        vello::kurbo::Affine::translate((value_x, value_y)),
        &value_layout,
        &[Brush::Solid(text_color)],
        true,
    );

    if !unit_text.is_empty() {
        let unit_x = center.x - unit_width * 0.5;
        let unit_y = start_y + value_height + spacing;
        render_text(
            scene,
            vello::kurbo::Affine::translate((unit_x, unit_y)),
            &unit_layout,
            &[Brush::Solid(text_color)],
            true,
        );
    }
}

fn render_text_along_arc(
    scene: &mut Scene,
    layout: &xilem::masonry::parley::Layout<BrushIndex>,
    color: Color,
    center: Point,
    radius: f64,
    start_angle: f64,
    direction: f64,
) {
    let mut lines = layout.lines();
    let Some(line) = lines.next() else {
        return;
    };
    if lines.next().is_some() {
        return;
    }

    let metrics = line.metrics();
    // Center within the ring band: metrics are absolute, so shift relative to baseline.
    let baseline_shift =
        metrics.baseline as f64 - (metrics.min_coord + metrics.max_coord) as f64 * 0.5;
    let brush = Brush::Solid(color);

    for item in line.items() {
        let xilem::masonry::parley::PositionedLayoutItem::GlyphRun(glyph_run) = item else {
            continue;
        };
        let run = glyph_run.run();
        let font = run.font();
        let font_size = run.font_size();
        let synthesis = run.synthesis();
        let glyph_xform = synthesis
            .skew()
            .map(|angle| Affine::skew(angle.to_radians().tan() as f64, 0.0));
        let coords = run.normalized_coords();

        let mut x = glyph_run.offset() as f64 - metrics.offset as f64;
        for glyph in glyph_run.glyphs() {
            let gx = x + glyph.x as f64;
            let gy = (-glyph.y as f64 + baseline_shift) * -direction;
            x += glyph.advance as f64;

            let angle = start_angle + direction * (gx / radius);
            let radial = Vec2::new(angle.cos(), angle.sin());
            let origin = Point::new(
                center.x + radial.x * (radius + gy),
                center.y + radial.y * (radius + gy),
            );
            let transform = Affine::translate(origin.to_vec2())
                * Affine::rotate(angle + direction * std::f64::consts::FRAC_PI_2);

            scene
                .draw_glyphs(font)
                .brush(&brush)
                .hint(true)
                .transform(transform)
                .glyph_transform(glyph_xform)
                .font_size(font_size)
                .normalized_coords(coords)
                .draw(Fill::NonZero, std::iter::once(vello::Glyph {
                    id: glyph.id,
                    x: 0.0,
                    y: 0.0,
                }));
        }
    }
}

fn readable_text_color(color: Color) -> Color {
    let r = color.components[0];
    let g = color.components[1];
    let b = color.components[2];
    let luminance = 0.2126 * r + 0.7152 * g + 0.0722 * b;
    if luminance > 0.6 {
        Color::from_rgba8(12, 14, 18, 220)
    } else {
        Color::from_rgba8(244, 247, 252, 230)
    }
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

fn hash_node(node: &crate::scan::FsNode) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    node.name.hash(&mut hasher);
    node.path.hash(&mut hasher);
    hasher.finish()
}

fn hit_test_radial(segments: &[RadialSegment], center: Point, point: Point) -> Option<usize> {
    let dx = point.x - center.x;
    let dy = point.y - center.y;
    let radius = (dx * dx + dy * dy).sqrt();
    let mut angle = dy.atan2(dx);
    let tau = std::f64::consts::TAU;
    if angle < 0.0 {
        angle += tau;
    }

    let mut best: Option<(usize, usize)> = None;
    for segment in segments {
        if radius < segment.inner_radius || radius > segment.outer_radius {
            continue;
        }
        let contains = if segment.sweep >= tau - 1e-6 {
            true
        } else {
            let start = normalize_angle(segment.start_angle);
            let end = normalize_angle(segment.start_angle + segment.sweep);
            if end >= start {
                angle >= start && angle <= end
            } else {
                angle >= start || angle <= end
            }
        };
        if contains {
            if best.map(|(_, depth)| segment.depth > depth).unwrap_or(true) {
                best = Some((segment.node, segment.depth));
            }
        }
    }
    best.map(|(node, _)| node)
}

fn ring_segment_path(
    center: Point,
    inner_radius: f64,
    outer_radius: f64,
    start: f64,
    sweep: f64,
) -> BezPath {
    let end = start + sweep;
    let mut path = BezPath::new();
    let start_point = Point::new(
        center.x + outer_radius * start.cos(),
        center.y + outer_radius * start.sin(),
    );
    path.move_to(start_point);
    let outer_arc = vello::kurbo::Arc::new(
        center,
        Vec2::new(outer_radius, outer_radius),
        start,
        sweep,
        0.0,
    );
    for el in outer_arc.append_iter(0.5) {
        path.push(el);
    }
    let inner_point = Point::new(
        center.x + inner_radius * end.cos(),
        center.y + inner_radius * end.sin(),
    );
    path.line_to(inner_point);
    let inner_arc = vello::kurbo::Arc::new(
        center,
        Vec2::new(inner_radius, inner_radius),
        end,
        -sweep,
        0.0,
    );
    for el in inner_arc.append_iter(0.5) {
        path.push(el);
    }
    path.close_path();
    path
}

fn radial_anchor_rect(center: Point, segment: &RadialSegment) -> Rect {
    let mid_angle = segment.start_angle + segment.sweep * 0.5;
    let mid_radius = (segment.inner_radius + segment.outer_radius) * 0.5;
    let point = Point::new(
        center.x + mid_radius * mid_angle.cos(),
        center.y + mid_radius * mid_angle.sin(),
    );
    Rect::new(point.x - 4.0, point.y - 4.0, point.x + 4.0, point.y + 4.0)
}

fn normalize_angle(mut angle: f64) -> f64 {
    let tau = std::f64::consts::TAU;
    angle %= tau;
    if angle < 0.0 {
        angle += tau;
    }
    angle
}
