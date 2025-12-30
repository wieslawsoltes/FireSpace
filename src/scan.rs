use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use rayon::prelude::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeKind {
    File,
    Directory,
}

#[derive(Clone, Debug)]
pub struct FsNode {
    pub name: String,
    pub path: PathBuf,
    pub size: u64,
    pub children: Vec<usize>,
    pub parent: Option<usize>,
    pub kind: NodeKind,
}

#[derive(Clone, Debug)]
pub struct FsTree {
    pub nodes: Vec<FsNode>,
    pub root: usize,
    pub total_bytes: u64,
    pub errors: usize,
    pub skipped: usize,
}

#[derive(Clone, Debug)]
pub enum ScanOutcome {
    Completed(FsTree),
    Cancelled,
}

struct ScanStats {
    errors: AtomicUsize,
    skipped: AtomicUsize,
}

impl ScanStats {
    fn new() -> Self {
        Self {
            errors: AtomicUsize::new(0),
            skipped: AtomicUsize::new(0),
        }
    }

    fn errors(&self) -> usize {
        self.errors.load(Ordering::Relaxed)
    }

    fn skipped(&self) -> usize {
        self.skipped.load(Ordering::Relaxed)
    }
}

#[derive(Clone, Debug)]
struct TreeNode {
    name: String,
    path: PathBuf,
    size: u64,
    kind: NodeKind,
    children: Vec<TreeNode>,
}

pub fn scan_path_with_cancel(path: &Path, cancel: &AtomicBool) -> anyhow::Result<ScanOutcome> {
    let mut nodes = Vec::new();
    let stats = ScanStats::new();
    let root_node = match scan_entry_tree(path, &stats, cancel) {
        Ok(Some(node)) => node,
        Ok(None) => {
            return Err(anyhow::anyhow!("nothing to scan at {}", path.display()));
        }
        Err(ScanCancelled) => {
            return Ok(ScanOutcome::Cancelled);
        }
    };
    let root = flatten_tree(root_node, None, &mut nodes);
    let total_bytes = nodes[root].size;
    Ok(ScanOutcome::Completed(FsTree {
        nodes,
        root,
        total_bytes,
        errors: stats.errors(),
        skipped: stats.skipped(),
    }))
}

#[derive(Clone, Copy, Debug)]
struct ScanCancelled;

fn scan_entry_tree(
    path: &Path,
    stats: &ScanStats,
    cancel: &AtomicBool,
) -> Result<Option<TreeNode>, ScanCancelled> {
    if cancel.load(Ordering::Relaxed) {
        return Err(ScanCancelled);
    }
    let metadata = match fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(err) => {
            record_error(&err, stats);
            return Ok(None);
        }
    };

    let file_type = metadata.file_type();
    if file_type.is_symlink() {
        stats.skipped.fetch_add(1, Ordering::Relaxed);
        return Ok(None);
    }

    if file_type.is_dir() {
        return scan_dir_tree(path, stats, cancel).map(Some);
    }

    if file_type.is_file() {
        return Ok(Some(TreeNode {
            name: display_name(path),
            path: path.to_path_buf(),
            size: metadata.len(),
            kind: NodeKind::File,
            children: Vec::new(),
        }));
    }

    stats.skipped.fetch_add(1, Ordering::Relaxed);
    Ok(None)
}

fn scan_dir_tree(
    path: &Path,
    stats: &ScanStats,
    cancel: &AtomicBool,
) -> Result<TreeNode, ScanCancelled> {
    if cancel.load(Ordering::Relaxed) {
        return Err(ScanCancelled);
    }
    let entries = match fs::read_dir(path) {
        Ok(entries) => entries,
        Err(err) => {
            record_error(&err, stats);
            return Ok(TreeNode {
                name: display_name(path),
                path: path.to_path_buf(),
                size: 0,
                kind: NodeKind::Directory,
                children: Vec::new(),
            });
        }
    };

    let mut children = Vec::new();
    let mut subdirs = Vec::new();
    let mut size: u64 = 0;
    for entry in entries {
        if cancel.load(Ordering::Relaxed) {
            return Err(ScanCancelled);
        }
        let entry = match entry {
            Ok(entry) => entry,
            Err(err) => {
                record_error(&err, stats);
                continue;
            }
        };
        let file_type = match entry.file_type() {
            Ok(file_type) => file_type,
            Err(err) => {
                record_error(&err, stats);
                continue;
            }
        };

        let child_path = entry.path();
        if file_type.is_symlink() {
            stats.skipped.fetch_add(1, Ordering::Relaxed);
            continue;
        }

        if file_type.is_dir() {
            subdirs.push(child_path);
            continue;
        }

        if file_type.is_file() {
            let metadata = match entry.metadata() {
                Ok(metadata) => metadata,
                Err(err) => {
                    record_error(&err, stats);
                    continue;
                }
            };
            size = size.saturating_add(metadata.len());
            children.push(TreeNode {
                name: display_name(&child_path),
                path: child_path,
                size: metadata.len(),
                kind: NodeKind::File,
                children: Vec::new(),
            });
            continue;
        }

        stats.skipped.fetch_add(1, Ordering::Relaxed);
    }

    let mut dir_children: Vec<TreeNode> = subdirs
        .par_iter()
        .map(|child_path| scan_dir_tree(child_path, stats, cancel))
        .collect::<Result<Vec<_>, _>>()?;
    for child in &dir_children {
        size = size.saturating_add(child.size);
    }

    children.append(&mut dir_children);
    Ok(TreeNode {
        name: display_name(path),
        path: path.to_path_buf(),
        size,
        kind: NodeKind::Directory,
        children,
    })
}

fn display_name(path: &Path) -> String {
    path.file_name()
        .map(|name| name.to_string_lossy().to_string())
        .unwrap_or_else(|| path.to_string_lossy().to_string())
}

fn record_error(err: &io::Error, stats: &ScanStats) {
    match err.kind() {
        io::ErrorKind::PermissionDenied
        | io::ErrorKind::NotFound
        | io::ErrorKind::InvalidData
        | io::ErrorKind::Interrupted => {
            stats.skipped.fetch_add(1, Ordering::Relaxed);
        }
        _ => {
            stats.errors.fetch_add(1, Ordering::Relaxed);
        }
    }
}

fn flatten_tree(node: TreeNode, parent: Option<usize>, nodes: &mut Vec<FsNode>) -> usize {
    let id = nodes.len();
    nodes.push(FsNode {
        name: node.name,
        path: node.path,
        size: node.size,
        children: Vec::new(),
        parent,
        kind: node.kind,
    });

    for child in node.children {
        let child_id = flatten_tree(child, Some(id), nodes);
        nodes[id].children.push(child_id);
    }

    id
}
