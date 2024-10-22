//! Handling for acme-style mouse interactions with the editor.
use crate::{
    config_handle,
    dot::{Dot, Range},
    editor::{Action, Editor},
    key::{MouseButton, MouseEvent},
    system::System,
};
use ad_event::Source;
use std::time::Instant;

/// Transient state that we hold to track the last mouse click we saw while
/// we wait for it to be released or if the buffer changes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Click {
    /// The button being held down
    pub(crate) btn: MouseButton,
    /// The current state of the dot associated with this click. This is updated
    /// by Hold events and matches the buffer Dot for Left clicks. For Right and
    /// Middle clicks this is a separate selection that is used on release.
    pub(crate) selection: Range,
    cut_handled: bool,
    paste_handled: bool,
}

impl Click {
    fn new(btn: MouseButton, selection: Range) -> Self {
        Self {
            btn,
            selection,
            cut_handled: false,
            paste_handled: false,
        }
    }

    /// Only needed for Left clicks. Used to ensure that once the user has made
    /// a chorded action we still support the other remaining action but we
    /// stop setting the buffer Dot.
    fn chord_handled(&self) -> bool {
        self.btn == MouseButton::Left && (self.cut_handled || self.paste_handled)
    }
}

impl<S> Editor<S>
where
    S: System,
{
    /// The outcome of a mouse event depends on any prior mouse state that is being held:
    ///   - Left   (click+release):      set Cur Dot at the location of the click
    ///   - Left   (click+hold+release): set Range Dot from click->release with click active
    ///   - Right  (click+release):      Load Dot (expanding current dot from Cur if needed)
    ///   - Right  (click+hold+release): Load click->release
    ///   - Middle (click+release):      Execute Dot (expanding current dot from Cur if needed)
    ///   - Middle (click+hold+release): Execute click->release
    ///
    ///   -> For Execute actions, if the current Dot is a Range then it is used as an
    ///      argument to the command being executed
    ///
    /// The chording behaviour we implement follows that of acme: http://acme.cat-v.org/mouse
    ///   - Hold Left + click Right:    Paste into selection
    ///   - Hold Left + click Middle:   Delete selection (cut)
    ///   - Hold Right + click either:  cancel Load
    ///   - Hold Middle + click either: cancel Execute
    pub(super) fn handle_mouse_event(&mut self, evt: MouseEvent) {
        use MouseButton::*;

        let last_click_time = self.last_click_time;
        self.last_click_time = Instant::now();

        match evt {
            MouseEvent::Press { b: Left, x, y } => {
                // Left clicking while Right or Middle is held is always a cancel
                if self.held_click.is_some() {
                    self.held_click = None;
                    return;
                }

                let b = self.buffers.active_mut();
                self.windows.set_dot_from_screen_coords(b, x, y);

                if self.last_click_was_left {
                    let delta = (self.last_click_time - last_click_time).as_millis();
                    if delta < config_handle!().double_click_ms {
                        b.try_expand_delimited();
                        return;
                    }
                }

                self.held_click = Some(Click::new(Left, b.dot.as_range()));
                self.last_click_was_left = true;
            }

            MouseEvent::Press { b: Right, x, y } => self.handle_right_or_middle_click(true, x, y),
            MouseEvent::Press { b: Middle, x, y } => self.handle_right_or_middle_click(false, x, y),

            MouseEvent::Hold { x, y, .. } => {
                if let Some(click) = &mut self.held_click {
                    if click.chord_handled() {
                        return;
                    }

                    let cur = self
                        .windows
                        .cur_from_screen_coords(self.buffers.active_mut(), x, y);
                    click.selection.set_active_cursor(cur);

                    if click.btn == Left {
                        self.buffers.active_mut().dot = Dot::from(click.selection);
                    }
                }
            }

            MouseEvent::Press { b: WheelUp, .. } => {
                self.last_click_was_left = false;
                self.windows.scroll_up(self.buffers.active_mut());
            }

            MouseEvent::Press { b: WheelDown, .. } => {
                self.last_click_was_left = false;
                self.windows.scroll_down(self.buffers.active_mut());
            }

            MouseEvent::Release { b, x, y } => {
                if let Some(click) = self.held_click {
                    if click.btn == Left && (b == Right || b == Middle) {
                        return; // paste and cut are handled on click
                    }
                }

                let mut click = match self.held_click.take() {
                    Some(click) => click,
                    None => return,
                };

                if click.chord_handled() {
                    return;
                }

                let cur = self
                    .windows
                    .cur_from_screen_coords(self.buffers.active_mut(), x, y);
                click.selection.set_active_cursor(cur);

                match click.btn {
                    Left | WheelUp | WheelDown => (),
                    Right | Middle => {
                        self.handle_right_or_middle_release(click.btn == Right, click)
                    }
                }
            }
        }
    }

    #[inline]
    fn click_from_button(&mut self, btn: MouseButton, x: usize, y: usize) -> Click {
        let cur = self
            .windows
            .cur_from_screen_coords(self.buffers.active_mut(), x, y);

        Click::new(btn, Range::from_cursors(cur, cur, false))
    }

    #[inline]
    fn handle_right_or_middle_click(&mut self, is_right: bool, x: usize, y: usize) {
        use MouseButton::*;

        self.last_click_was_left = false;
        match self.held_click {
            Some(mut click) => {
                // Mouse chords execute on the click of the second button rather than the release
                if click.btn == Left {
                    if is_right && !click.paste_handled {
                        self.paste_from_clipboard(Source::Mouse);
                        click.paste_handled = true;
                        self.held_click = Some(click);
                    } else if !is_right && !click.cut_handled {
                        self.forward_action_to_active_buffer(Action::Delete, Source::Mouse);
                        click.selection = self.buffers.active().dot.as_range();
                        click.cut_handled = true;
                        self.held_click = Some(click);
                    }
                } else if (is_right && click.btn == Middle) || (!is_right && click.btn == Right) {
                    self.held_click = None;
                }
            }

            None => {
                let btn = if is_right { Right } else { Middle };
                self.held_click = Some(self.click_from_button(btn, x, y));
            }
        };
    }

    #[inline]
    fn handle_right_or_middle_release(&mut self, is_right: bool, click: Click) {
        if click.selection.start != click.selection.end {
            // In the case where the click selection is a range we Load/Execute it directly.
            // For Middle clicks, if there is also a range dot in the buffer then that is
            // used as an argument to the command being executed.
            if is_right {
                self.buffers.active_mut().dot = Dot::from(click.selection);
                self.default_load_dot(Source::Mouse);
            } else {
                let dot = self.buffers.active().dot;
                self.buffers.active_mut().dot = Dot::from(click.selection);

                if dot.is_range() {
                    // Execute as if the click selection was dot then reset dot
                    let arg = dot.content(self.buffers.active()).trim().to_string();
                    self.default_execute_dot(Some((dot.as_range(), arg)), Source::Mouse);
                    self.buffers.active_mut().dot = dot;
                } else {
                    self.default_execute_dot(None, Source::Mouse);
                }
            }
        } else {
            // In the case where the click selection was a Cur rather than a Range we
            // set the buffer dot to the click location if it is outside of the current buffer
            // dot (and allow smart expand to handle generating the selection) before we Load/Execute
            if !self.buffers.active().dot.contains(&click.selection.start) {
                self.buffers.active_mut().dot = Dot::from(click.selection.start);
            }

            if is_right {
                self.default_load_dot(Source::Mouse);
            } else {
                self.default_execute_dot(None, Source::Mouse);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        dot::{Cur, Range},
        editor::EditorMode,
        fsys::InputFilter,
        key::{MouseButton::*, MouseEvent::*},
        log::LogBuffer,
    };
    use ad_event::{FsysEvent, Kind, Source};
    use simple_test_case::test_case;
    use std::{io, sync::mpsc::channel};

    #[derive(Debug, Default)]
    struct TestSystem {
        clipboard: String,
    }

    impl System for TestSystem {
        fn set_clipboard(&mut self, s: &str) -> io::Result<()> {
            self.clipboard = s.to_string();

            Ok(())
        }

        fn read_clipboard(&self) -> io::Result<String> {
            Ok(self.clipboard.clone())
        }
    }

    fn r(start: usize, end: usize, start_active: bool) -> Range {
        Range {
            start: Cur { idx: start },
            end: Cur { idx: end },
            start_active,
        }
    }

    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Hold { b: Left, x: 7, y: 0 },
            Release { b: Left, x: 7, y: 0 },
        ],
        None,
        "some",
        "some text to test with\n",
        "X",
        &[];
        "left click drag selection complete"
    )]
    #[test_case(
        &[
            Press { b: Right, x: 3, y: 0 },
            Hold { b: Right, x: 7, y: 0 },
            Release { b: Right, x: 7, y: 0 },
        ],
        None,
        "some",
        "some text to test with\n",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::LoadBody, 0, 3, "some"),
        ];
        "right click drag selection complete"
    )]
    #[test_case(
        &[
            Press { b: Middle, x: 3, y: 0 },
            Hold { b: Middle, x: 7, y: 0 },
            Release { b: Middle, x: 7, y: 0 },
        ],
        None,
        "some",
        "some text to test with\n",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::ExecuteBody, 0, 3, "some"),
        ];
        "middle click drag selection complete"
    )]
    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Hold { b: Left, x: 7, y: 0 },
        ],
        Some(Click::new(Left, r(0, 3, false))),
        "some",
        "some text to test with\n",
        "X",
        &[];
        "left click drag selection without release"
    )]
    #[test_case(
        &[
            Press { b: Right, x: 3, y: 0 },
            Hold { b: Right, x: 7, y: 0 },
        ],
        Some(Click::new(Right, r(0, 3, false))),
        "t",  // default dot position
        "some text to test with\n",
        "X",
        &[];
        "right click drag selection without release"
    )]
    #[test_case(
        &[
            Press { b: Middle, x: 3, y: 0 },
            Hold { b: Middle, x: 7, y: 0 },
        ],
        Some(Click::new(Middle, r(0, 3, false))),
        "t",  // default dot position
        "some text to test with\n",
        "X",
        &[];
        "middle click drag selection without release"
    )]
    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Release { b: Left, x: 7, y: 0 },
            Press { b: Right, x: 4, y: 0 },
            Release { b: Right, x: 4, y: 0 },
        ],
        None,
        "some",
        "some text to test with\n",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::LoadBody, 0, 3, "some"),
        ];
        "right click expand in existing selection"
    )]
    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Release { b: Left, x: 7, y: 0 },
            Press { b: Middle, x: 4, y: 0 },
            Release { b: Middle, x: 4, y: 0 },
        ],
        None,
        "some",
        "some text to test with\n",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::ExecuteBody, 0, 3, "some"),
        ];
        "middle click expand in existing selection"
    )]
    #[test_case(
        &[
            Press { b: Left, x: 9, y: 0 },
            Hold { b: Left, x: 12, y: 0 },
            Release { b: Left, x: 12, y: 0 },
            Press { b: Middle, x: 3, y: 0 },
            Hold { b: Middle, x: 7, y: 0 },
            Release { b: Middle, x: 7, y: 0 },
        ],
        None,
        "text",
        "some text to test with\n",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::ChordedArgument, 5, 8, "text"),
            FsysEvent::new(Source::Mouse, Kind::ExecuteBody, 0, 3, "some"),
        ];
        "middle click with dot arg"
    )]
    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Hold { b: Left, x: 7, y: 0 },
            Press { b: Middle, x: 7, y: 0 },
            Release { b: Middle, x: 7, y: 0 },
            Release { b: Left, x: 7, y: 0 },
        ],
        None,
        " ",
        " text to test with\n",
        "some",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
        ];
        "chord cut"
    )]
    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Hold { b: Left, x: 7, y: 0 },
            Press { b: Right, x: 7, y: 0 },
            Release { b: Right, x: 7, y: 0 },
            Release { b: Left, x: 7, y: 0 },
        ],
        None,
        " ",
        "X text to test with\n",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
            FsysEvent::new(Source::Mouse, Kind::InsertBody, 0, 1, "X"),
        ];
        "chord paste"
    )]
    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Hold { b: Left, x: 7, y: 0 },
            Press { b: Middle, x: 7, y: 0 },
            Release { b: Middle, x: 7, y: 0 },
            Press { b: Right, x: 7, y: 0 },
            Release { b: Right, x: 7, y: 0 },
            Release { b: Left, x: 7, y: 0 },
        ],
        None,
        " ",
        "some text to test with\n",
        "some",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
            FsysEvent::new(Source::Mouse, Kind::InsertBody, 0, 4, "some"),
        ];
        "chord cut then paste"
    )]
    // not 100% sure this is correct at the moment in terms of what the
    // selection and clipboard end up as
    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Hold { b: Left, x: 7, y: 0 },
            Press { b: Right, x: 7, y: 0 },
            Release { b: Right, x: 7, y: 0 },
            Press { b: Middle, x: 7, y: 0 },
            Release { b: Middle, x: 7, y: 0 },
            Release { b: Left, x: 7, y: 0 },
        ],
        None,
        "t",
        "Xtext to test with\n",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
            FsysEvent::new(Source::Mouse, Kind::InsertBody, 0, 1, "X"),
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 1, 2, " "),
        ];
        "chord paste then cut"
    )]
    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Hold { b: Left, x: 7, y: 0 },
            Press { b: Middle, x: 7, y: 0 },
            Release { b: Middle, x: 7, y: 0 },
            Press { b: Middle, x: 7, y: 0 },
            Release { b: Middle, x: 7, y: 0 },
            Release { b: Left, x: 7, y: 0 },
        ],
        None,
        " ",
        " text to test with\n",
        "some",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
        ];
        "repeated chord cut"
    )]
    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Hold { b: Left, x: 7, y: 0 },
            Press { b: Right, x: 7, y: 0 },
            Release { b: Right, x: 7, y: 0 },
            Press { b: Right, x: 7, y: 0 },
            Release { b: Right, x: 7, y: 0 },
            Release { b: Left, x: 7, y: 0 },
        ],
        None,
        " ",
        "X text to test with\n",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
            FsysEvent::new(Source::Mouse, Kind::InsertBody, 0, 1, "X"),
        ];
        "repeated chord paste"
    )]
    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Hold { b: Left, x: 7, y: 0 },
            Press { b: Right, x: 7, y: 0 },
            Release { b: Right, x: 7, y: 0 },
            Hold { b: Left, x: 3, y: 0 },
            Release { b: Left, x: 3, y: 0 },
        ],
        None,
        " ",
        "X text to test with\n",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
            FsysEvent::new(Source::Mouse, Kind::InsertBody, 0, 1, "X"),
        ];
        "motion after chord paste is ignored"
    )]
    #[test_case(
        &[
            Press { b: Left, x: 3, y: 0 },
            Hold { b: Left, x: 7, y: 0 },
            Press { b: Middle, x: 7, y: 0 },
            Release { b: Middle, x: 7, y: 0 },
            Hold { b: Left, x: 2, y: 0 },
            Release { b: Left, x: 2, y: 0 },
        ],
        None,
        " ",
        " text to test with\n",
        "some",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
        ];
        "motion after chord cut is ignored"
    )]
    #[test_case(
        &[
            Press { b: Right, x: 3, y: 0 },
            Hold { b: Right, x: 7, y: 0 },
            Press { b: Left, x: 7, y: 0 },
            Release { b: Left, x: 7, y: 0 },
            Release { b: Right, x: 7, y: 0 },
        ],
        None,
        "t",
        "some text to test with\n",
        "X",
        &[];
        "right click cancel with left"
    )]
    #[test_case(
        &[
            Press { b: Right, x: 3, y: 0 },
            Hold { b: Right, x: 7, y: 0 },
            Press { b: Middle, x: 7, y: 0 },
            Release { b: Middle, x: 7, y: 0 },
            Release { b: Right, x: 7, y: 0 },
        ],
        None,
        "t",
        "some text to test with\n",
        "X",
        &[];
        "right click cancel with middle"
    )]
    #[test_case(
        &[
            Press { b: Middle, x: 3, y: 0 },
            Hold { b: Middle, x: 7, y: 0 },
            Press { b: Left, x: 7, y: 0 },
            Release { b: Left, x: 7, y: 0 },
            Release { b: Middle, x: 7, y: 0 },
        ],
        None,
        "t",
        "some text to test with\n",
        "X",
        &[];
        "middle click cancel with left"
    )]
    #[test_case(
        &[
            Press { b: Middle, x: 3, y: 0 },
            Hold { b: Middle, x: 7, y: 0 },
            Press { b: Right, x: 7, y: 0 },
            Release { b: Right, x: 7, y: 0 },
            Release { b: Middle, x: 7, y: 0 },
        ],
        None,
        "t",
        "some text to test with\n",
        "X",
        &[];
        "middle click cancel with right"
    )]
    #[test]
    fn mouse_interactions_work(
        evts: &[MouseEvent],
        click: Option<Click>,
        dot: &str,
        content: &str,
        clipboard: &str,
        fsys_events: &[FsysEvent],
    ) {
        let mut ed = Editor::new_with_system(
            Default::default(),
            Default::default(),
            EditorMode::Headless,
            LogBuffer::default(),
            TestSystem {
                clipboard: "X".to_string(),
            },
        );
        ed.open_virtual("test", "some text to test with");
        ed.buffers.active_mut().dot = Dot::Cur { c: Cur { idx: 5 } };

        // attach an input filter so we can intercept load and execute events
        let (tx, rx) = channel();
        let filter = InputFilter::new(tx);
        ed.try_set_input_filter(ed.active_buffer_id(), filter);

        for evt in evts.iter() {
            ed.handle_mouse_event(*evt);
        }

        let recvd_fsys_events: Vec<_> = rx.try_iter().collect();
        let b = ed.buffers.active();

        assert_eq!(ed.held_click, click, "click");
        assert_eq!(b.dot.content(b), dot, "dot content");
        assert_eq!(b.str_contents(), content, "buffer content");
        assert_eq!(ed.system.clipboard, clipboard, "clipboard content");
        assert_eq!(fsys_events, &recvd_fsys_events, "fsys events");
    }
}
