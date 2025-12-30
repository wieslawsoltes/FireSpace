# FireSpace

FireSpace is a cross-platform disk space analyzer written in Rust. It pairs a fast, parallel scanner with a flame-graph style visualization built on Vello for rendering and Xilem for UI.

## Highlights

- Radial and classic flame-graph views for directory size visualization.
- Interactive navigation: hover tooltips, click-to-zoom, breadcrumbs, and side-panel details.
- Fast scanning with parallel traversal and cancellation support.
- Works on macOS, Windows, and Linux.

## Requirements

- Rust (stable)
- A recent GPU driver for Vello rendering

## Build and Run

```bash
cargo run
```

To build a release binary:

```bash
cargo build --release
```

## Scanning Notes

- Symlinks are skipped to avoid cycles.
- Permission issues are recorded as skipped/errors; on macOS you may need Full Disk Access for complete results.
- Use the disk dropdown or type a path directly in the scan input.

## License

See `LICENSE`.
