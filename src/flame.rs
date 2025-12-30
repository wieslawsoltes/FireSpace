use std::f64::consts::FRAC_PI_2;

use vello::kurbo::{Point, Rect};
use vello::peniko::Color;

use crate::scan::FsTree;

#[derive(Clone, Debug)]
pub struct LayoutSettings {
    pub bar_height: f64,
    pub depth_gap: f64,
    pub sibling_gap: f64,
    pub min_rect_width: f64,
    pub padding: f64,
    pub radial_inner_radius: f64,
    pub radial_band: f64,
    pub radial_gap: f64,
}

impl Default for LayoutSettings {
    fn default() -> Self {
        Self {
            bar_height: 22.0,
            depth_gap: 4.0,
            sibling_gap: 1.5,
            min_rect_width: 1.0,
            padding: 12.0,
            radial_inner_radius: 36.0,
            radial_band: 18.0,
            radial_gap: 2.0,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FlameRect {
    pub node: usize,
    pub rect: Rect,
    pub depth: usize,
}

#[derive(Clone, Debug)]
pub struct RadialSegment {
    pub node: usize,
    pub start_angle: f64,
    pub sweep: f64,
    pub inner_radius: f64,
    pub outer_radius: f64,
    pub depth: usize,
}

pub fn layout_flame_graph(
    tree: &FsTree,
    root: usize,
    bounds: Rect,
    settings: &LayoutSettings,
) -> Vec<FlameRect> {
    if tree.nodes.is_empty() || tree.nodes[root].size == 0 {
        return Vec::new();
    }

    let padded = Rect::new(
        bounds.x0 + settings.padding,
        bounds.y0 + settings.padding,
        bounds.x1 - settings.padding,
        bounds.y1 - settings.padding,
    );

    if padded.width() <= 0.0 || padded.height() <= 0.0 {
        return Vec::new();
    }

    let mut rects = Vec::new();
    let base_y = padded.y1 - settings.bar_height;
    layout_node(
        tree,
        root,
        padded.x0,
        base_y,
        padded.width(),
        0,
        padded.y0,
        settings,
        &mut rects,
    );
    rects
}

pub fn layout_radial_graph(
    tree: &FsTree,
    root: usize,
    bounds: Rect,
    settings: &LayoutSettings,
) -> (Vec<RadialSegment>, Point) {
    let mut segments = Vec::new();
    let padded = Rect::new(
        bounds.x0 + settings.padding,
        bounds.y0 + settings.padding,
        bounds.x1 - settings.padding,
        bounds.y1 - settings.padding,
    );
    let center = Point::new(
        (padded.x0 + padded.x1) * 0.5,
        (padded.y0 + padded.y1) * 0.5,
    );
    if tree.nodes.is_empty() || tree.nodes[root].size == 0 {
        return (segments, center);
    }
    let max_radius = 0.5 * padded.width().min(padded.height());
    if max_radius <= settings.radial_inner_radius {
        return (segments, center);
    }
    layout_radial_node(
        tree,
        root,
        -FRAC_PI_2,
        std::f64::consts::TAU,
        settings.radial_inner_radius,
        0,
        max_radius,
        settings,
        &mut segments,
    );
    (segments, center)
}

fn layout_node(
    tree: &FsTree,
    node_id: usize,
    x: f64,
    y: f64,
    width: f64,
    depth: usize,
    min_y: f64,
    settings: &LayoutSettings,
    rects: &mut Vec<FlameRect>,
) {
    if width < settings.min_rect_width || y < min_y {
        return;
    }

    rects.push(FlameRect {
        node: node_id,
        rect: Rect::new(x, y, x + width, y + settings.bar_height),
        depth,
    });

    let node = &tree.nodes[node_id];
    if node.children.is_empty() || node.size == 0 {
        return;
    }

    let mut children = node.children.clone();
    children.sort_by_key(|child_id| std::cmp::Reverse(tree.nodes[*child_id].size));

    let mut cursor = x;
    let next_y = y - settings.bar_height - settings.depth_gap;
    if next_y < min_y {
        return;
    }

    for child_id in children {
        let child = &tree.nodes[child_id];
        let ratio = child.size as f64 / node.size as f64;
        let raw_width = width * ratio;
        if raw_width < settings.min_rect_width {
            cursor += raw_width;
            continue;
        }
        let child_width = (raw_width - settings.sibling_gap).max(settings.min_rect_width);
        layout_node(
            tree,
            child_id,
            cursor,
            next_y,
            child_width,
            depth + 1,
            min_y,
            settings,
            rects,
        );
        cursor += raw_width;
    }
}

fn layout_radial_node(
    tree: &FsTree,
    node_id: usize,
    start_angle: f64,
    sweep: f64,
    inner_radius: f64,
    depth: usize,
    max_radius: f64,
    settings: &LayoutSettings,
    segments: &mut Vec<RadialSegment>,
) {
    let outer_radius = inner_radius + settings.radial_band;
    if outer_radius > max_radius || sweep <= 0.0 {
        return;
    }
    let min_angle = settings.min_rect_width / outer_radius.max(1.0);
    if sweep < min_angle {
        return;
    }
    segments.push(RadialSegment {
        node: node_id,
        start_angle,
        sweep,
        inner_radius,
        outer_radius,
        depth,
    });

    let node = &tree.nodes[node_id];
    if node.children.is_empty() || node.size == 0 {
        return;
    }
    let next_inner = outer_radius + settings.radial_gap;
    if next_inner + settings.radial_band > max_radius {
        return;
    }

    let mut children = node.children.clone();
    children.sort_by_key(|child_id| std::cmp::Reverse(tree.nodes[*child_id].size));

    let mut cursor = start_angle;
    for child_id in children {
        let child = &tree.nodes[child_id];
        let ratio = child.size as f64 / node.size as f64;
        let child_sweep = sweep * ratio;
        layout_radial_node(
            tree,
            child_id,
            cursor,
            child_sweep,
            next_inner,
            depth + 1,
            max_radius,
            settings,
            segments,
        );
        cursor += child_sweep;
    }
}

pub fn format_bytes(bytes: u64) -> String {
    const UNITS: [&str; 6] = ["B", "KB", "MB", "GB", "TB", "PB"];
    let mut value = bytes as f64;
    let mut unit = 0;
    while value >= 1024.0 && unit < UNITS.len() - 1 {
        value /= 1024.0;
        unit += 1;
    }
    if unit == 0 {
        format!("{bytes} {}", UNITS[unit])
    } else if value < 10.0 {
        format!("{value:.1} {}", UNITS[unit])
    } else {
        format!("{value:.0} {}", UNITS[unit])
    }
}

pub fn grouped_color(group_hash: u64, node_hash: u64, depth: usize) -> Color {
    const PALETTE: [f32; 12] = [210.0, 260.0, 300.0, 330.0, 20.0, 45.0, 90.0, 135.0, 170.0, 190.0, 230.0, 285.0];
    let base_hue = PALETTE[(group_hash as usize) % PALETTE.len()];
    let node_shift = ((node_hash & 0xff) as f32 / 255.0) * 6.0;
    let depth_shift = (depth as f32 * 8.0) % 64.0;
    let hue = (base_hue + node_shift + depth_shift) % 360.0;

    let sat = 0.6 + (((node_hash >> 8) & 0xff) as f32 / 255.0) * 0.18;
    let lum_base = 0.54 + (((node_hash >> 16) & 0xff) as f32 / 255.0) * 0.12;
    let depth_factor = (1.0 - depth as f32 * 0.028).clamp(0.4, 0.95);
    let lum = (lum_base * depth_factor).clamp(0.2, 0.86);

    let (r, g, b) = hsl_to_rgb(hue / 360.0, sat, lum);
    Color::from_rgb8(r, g, b)
}

pub fn mix_color(base: Color, overlay: Color, t: f64) -> Color {
    let t = t.clamp(0.0, 1.0) as f32;
    let inv = 1.0 - t;
    let r = (base.components[0] * inv + overlay.components[0] * t).clamp(0.0, 1.0);
    let g = (base.components[1] * inv + overlay.components[1] * t).clamp(0.0, 1.0);
    let b = (base.components[2] * inv + overlay.components[2] * t).clamp(0.0, 1.0);
    Color::new([r, g, b, 1.0])
}

fn hsl_to_rgb(h: f32, s: f32, l: f32) -> (u8, u8, u8) {
    let c = (1.0 - (2.0 * l - 1.0).abs()) * s;
    let h_prime = h * 6.0;
    let x = c * (1.0 - (h_prime % 2.0 - 1.0).abs());
    let (r1, g1, b1) = match h_prime as u32 {
        0 => (c, x, 0.0),
        1 => (x, c, 0.0),
        2 => (0.0, c, x),
        3 => (0.0, x, c),
        4 => (x, 0.0, c),
        _ => (c, 0.0, x),
    };
    let m = l - c * 0.5;
    (
        ((r1 + m) * 255.0).round() as u8,
        ((g1 + m) * 255.0).round() as u8,
        ((b1 + m) * 255.0).round() as u8,
    )
}
