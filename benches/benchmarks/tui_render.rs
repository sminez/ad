// Render times for the TUI ui implementation.
use ad_editor::{
    Config, Editor, EditorMode, LogBuffer, PlumbingRules,
    key::{Input, MouseButton, MouseEvent, MouseEventKind, MouseMod},
    system::DefaultSystem,
    ui::{GenericTui, Layout, UserInterface},
};
use criterion::{BenchmarkGroup, Criterion, criterion_group, measurement::WallTime};
use std::{
    env::current_dir,
    hint::black_box,
    io::{self, Write},
    sync::{Arc, RwLock},
};

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("TUI render");

    fixed_view(&mut group, "single window", &["src/util.rs"]);
    fixed_view(&mut group, "two windows", &["src/util.rs", "src/term.rs"]);

    single_window_editor_scroll_inputs(
        &mut group,
        "single window editor scroll inputs stdout",
        EditorMode::Terminal,
    );

    let config = Config::try_load().unwrap();
    let mut tui = GenericTui::new_with_stdout_handle(
        Arc::new(RwLock::new(config.clone())),
        StdoutSink(Vec::with_capacity(512 * 1024)),
    );
    tui.set_size(80, 160);

    single_window_editor_scroll_inputs(
        &mut group,
        "single window editor scroll inputs sink",
        EditorMode::Boxed(Box::new(tui)),
    );

    group.finish();
}

criterion_group!(benches, criterion_benchmark);

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
    let config = Arc::new(RwLock::new(Config::try_load().unwrap()));
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

fn fixed_view(group: &mut BenchmarkGroup<'_, WallTime>, title: &str, files: &[&str]) {
    let (mut tui, mut layout) = tui_and_layout(files);
    group.bench_function(title, |b| {
        b.iter(|| {
            tui.refresh(
                black_box("NORMAL"),
                black_box(&mut layout),
                black_box(0),
                black_box(&[]),
                black_box(None),
                black_box(None),
            );
        })
    });
}

fn single_window_editor_scroll_inputs(
    group: &mut BenchmarkGroup<'_, WallTime>,
    title: &str,
    editor_mode: EditorMode,
) {
    let mut e = Editor::new_with_system_and_initial_files(
        Config::try_load(),
        PlumbingRules::try_load(),
        editor_mode,
        LogBuffer::default(),
        DefaultSystem::without_clipboard_provider(),
        &["src/ui/tui.rs"],
    );

    let mut n = 0;
    let mut btn = MouseButton::WheelDown;

    group.bench_function(title, |b| {
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

            e.refresh_screen_w_minibuffer(None);
            e.handle_input(Input::Mouse(MouseEvent {
                k: MouseEventKind::Press,
                m: MouseMod::NoMod,
                b: btn,
                x: 20,
                y: 20,
            }));
        })
    });
}
