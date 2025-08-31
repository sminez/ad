// Render times for the TUI ui implementation.
use ad_editor::{
    Config, Editor, EditorMode, LogBuffer, PlumbingRules,
    key::{Input, MouseButton, MouseEvent, MouseEventKind, MouseMod},
    system::DefaultSystem,
    ui::{GenericTui, Layout, UserInterface},
};
use criterion::{Criterion, criterion_group};
use std::{
    env::current_dir,
    hint::black_box,
    io::{self, Write},
    sync::{Arc, Mutex},
};

struct StdoutSink(Vec<u8>);

impl Write for StdoutSink {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.0.write_all(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        let res = self.0.flush();
        self.0.clear();

        res
    }
}

fn tui_and_layout(files: &[&str]) -> (GenericTui<StdoutSink>, Layout) {
    // This will need to point to the TS config for rust
    let config = Arc::new(Mutex::new(Config::try_load().unwrap()));
    let mut tui = GenericTui::new_with_stdout_handle(
        config.clone(),
        StdoutSink(Vec::with_capacity(512 * 1024)),
    );
    tui.set_size(80, 160);
    let mut layout = Layout::new_with_stub_lsp_handle(80, 160, config);

    let repo_root = current_dir().unwrap();

    for file in files {
        layout.open_or_focus(repo_root.join(file), false).unwrap();
    }

    (tui, layout)
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("TUI render");

    let (mut tui, layout) = tui_and_layout(&["src/util.rs"]);
    group.bench_function("single window", |b| {
        b.iter(|| {
            tui.refresh(
                black_box("NORMAL"),
                black_box(&layout),
                black_box(0),
                black_box(&[]),
                black_box(None),
                black_box(None),
            );
        })
    });

    let (mut tui, layout) = tui_and_layout(&["src/util.rs", "src/term.rs"]);
    group.bench_function("two windows", |b| {
        b.iter(|| {
            tui.refresh(
                black_box("NORMAL"),
                black_box(&layout),
                black_box(0),
                black_box(&[]),
                black_box(None),
                black_box(None),
            );
        })
    });

    let (mut tui, mut layout) = tui_and_layout(&["src/ui/tui.rs"]);
    let mut n = 0;
    let mut up = false;

    group.bench_function("single window scroll active", |b| {
        b.iter(|| {
            tui.refresh(
                black_box("NORMAL"),
                black_box(&layout),
                black_box(0),
                black_box(&[]),
                black_box(None),
                black_box(None),
            );

            n += 1;
            if n == 500 {
                n = 0;
                up = !up;
            }
            layout.scroll_active(up);
        })
    });

    let (mut tui, mut layout) = tui_and_layout(&["src/ui/tui.rs"]);
    let mut n = 0;
    let mut up = false;

    group.bench_function("single window scroll view", |b| {
        b.iter(|| {
            tui.refresh(
                black_box("NORMAL"),
                black_box(&layout),
                black_box(0),
                black_box(&[]),
                black_box(None),
                black_box(None),
            );

            n += 1;
            if n == 500 {
                n = 0;
                up = !up;
            }
            layout.scroll_view(20, 20, up);
        })
    });

    let config = Config::try_load().unwrap();
    let mut tui = GenericTui::new_with_stdout_handle(
        Arc::new(Mutex::new(config.clone())),
        StdoutSink(Vec::with_capacity(512 * 1024)),
    );
    tui.set_size(80, 160);

    let mut e = Editor::new_with_system_and_initial_files(
        Config::try_load(),
        PlumbingRules::try_load(),
        EditorMode::Boxed(Box::new(tui)),
        LogBuffer::default(),
        DefaultSystem::without_clipboard_provider(),
        &["src/ui/tui.rs"],
    );

    let mut n = 0;
    let mut btn = MouseButton::WheelDown;

    group.bench_function("single window editor scroll inputs", |b| {
        b.iter(|| {
            n += 1;
            if n == 500 {
                n = 0;
                btn = if btn == MouseButton::WheelUp {
                    MouseButton::WheelDown
                } else {
                    MouseButton::WheelUp
                };
            }

            e.handle_input(Input::Mouse(MouseEvent {
                k: MouseEventKind::Press,
                m: MouseMod::NoMod,
                b: btn,
                x: 20,
                y: 20,
            }));
            e.refresh_screen_w_minibuffer(None);
        })
    });

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
