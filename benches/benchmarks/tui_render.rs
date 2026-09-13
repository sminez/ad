// Render times for the TUI ui implementation.
use ad_editor::{
    Config, Editor, EditorMode, LogBuffer, PlumbingRules,
    key::{Input, MouseButton, MouseEvent, MouseEventKind, MouseMod},
    system::DefaultSystem,
    ui::GenericTui,
};
use criterion::{BenchmarkGroup, Criterion, criterion_group, measurement::WallTime};
use parking_lot::RwLock;
use std::{
    io::{self, Write},
    sync::Arc,
};

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("TUI render");

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

            e.refresh_screen();
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
