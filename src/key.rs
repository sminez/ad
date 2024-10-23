//! Keypresses and related user interactions.
use tracing::trace;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Arrow {
    Up,
    Down,
    Left,
    Right,
}

impl Arrow {
    pub fn flip(&self) -> Self {
        match self {
            Self::Up => Self::Down,
            Self::Down => Self::Up,
            Self::Left => Self::Right,
            Self::Right => Self::Left,
        }
    }
}

// using 'showkey -a' to view keycodes is useful for adding to this
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Input {
    Char(char),
    Ctrl(char),
    Alt(char),
    CtrlAlt(char),
    Tab,
    BackTab,
    Return,
    Backspace,
    Arrow(Arrow),
    Del,
    Home,
    End,
    PageUp,
    PageDown,
    Esc,
    Mouse(MouseEvent),
}

impl Input {
    pub fn from_char(c: char) -> Self {
        match c {
            '\x1b' => Input::Esc,
            '\n' | '\r' => Input::Return,
            '\x7f' => Input::Backspace,
            '\t' => Input::Tab,
            c @ '\x01'..='\x1A' => Input::Ctrl((c as u8 - 0x1 + b'a') as char),
            c @ '\x1C'..='\x1F' => Input::Ctrl((c as u8 - 0x1C + b'4') as char),
            _ => Input::Char(c),
        }
    }

    pub fn try_from_seq2(c1: char, c2: char) -> Option<Self> {
        match (c1, c2) {
            ('O', 'H') => Some(Input::Home),
            ('O', 'F') => Some(Input::End),

            ('[', 'A') => Some(Input::Arrow(Arrow::Up)),
            ('[', 'B') => Some(Input::Arrow(Arrow::Down)),
            ('[', 'C') => Some(Input::Arrow(Arrow::Right)),
            ('[', 'D') => Some(Input::Arrow(Arrow::Left)),
            ('[', 'H') => Some(Input::Home),
            ('[', 'Z') => Some(Input::BackTab),
            ('\x1b', c) if c.is_ascii() => match Self::from_char(c) {
                Input::Char(c) => Some(Input::Alt(c)),
                Input::Ctrl(c) => Some(Input::CtrlAlt(c)),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn try_from_bracket_tilde(c: char) -> Option<Self> {
        match c {
            '1' | '7' => Some(Input::Home),
            '4' | '8' => Some(Input::End),
            '3' => Some(Input::Del),
            '5' => Some(Input::PageUp),
            '6' => Some(Input::PageDown),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MouseButton {
    Left,
    Middle,
    Right,
    WheelUp,
    WheelDown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MouseMod {
    NoMod,
    Alt,
    Ctrl,
    AltCtrl,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MouseEventKind {
    Press,
    Hold,
    Release,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MouseEvent {
    pub k: MouseEventKind,
    pub m: MouseMod,
    pub b: MouseButton,
    pub x: usize,
    pub y: usize,
}

impl MouseEvent {
    pub(crate) fn try_from_raw(b: usize, x: usize, y: usize, c: char) -> Option<Self> {
        use MouseButton::*;
        use MouseEventKind::*;
        use MouseMod::*;

        let (k, m, b) = match (b, c) {
            // No modifiers
            (0, 'M') => (Press, NoMod, Left),
            (1, 'M') => (Press, NoMod, Middle),
            (2, 'M') => (Press, NoMod, Right),
            (64, 'M') => (Press, NoMod, WheelUp),
            (65, 'M') => (Press, NoMod, WheelDown),

            (0, 'm') | (3, _) => (Release, NoMod, Left),
            (1, 'm') => (Release, NoMod, Middle),
            (2, 'm') => (Release, NoMod, Right),
            (64, 'm') => (Release, NoMod, WheelUp),
            (65, 'm') => (Release, NoMod, WheelDown),

            (32, _) => (Hold, NoMod, Left),
            (33, _) => (Hold, NoMod, Middle),
            (34, _) => (Hold, NoMod, Right),

            // Alt modifier
            (8, 'M') => (Press, Alt, Left),
            (9, 'M') => (Press, Alt, Middle),
            (10, 'M') => (Press, Alt, Right),
            (72, 'M') => (Press, Alt, WheelUp),
            (73, 'M') => (Press, Alt, WheelDown),

            (8, 'm') => (Release, Alt, Left),
            (9, 'm') => (Release, Alt, Middle),
            (10, 'm') => (Release, Alt, Right),
            (72, 'm') => (Release, Alt, WheelUp),
            (73, 'm') => (Release, Alt, WheelDown),

            (40, _) => (Hold, Alt, Left),
            (41, _) => (Hold, Alt, Middle),
            (42, _) => (Hold, Alt, Right),

            // Ctrl modifier
            (16, 'M') => (Press, Ctrl, Left),
            (17, 'M') => (Press, Ctrl, Middle),
            (18, 'M') => (Press, Ctrl, Right),
            (80, 'M') => (Press, Ctrl, WheelUp),
            (81, 'M') => (Press, Ctrl, WheelDown),

            (16, 'm') => (Release, Ctrl, Left),
            (17, 'm') => (Release, Ctrl, Middle),
            (18, 'm') => (Release, Ctrl, Right),
            (80, 'm') => (Release, Ctrl, WheelUp),
            (81, 'm') => (Release, Ctrl, WheelDown),

            (48, _) => (Hold, Ctrl, Left),
            (49, _) => (Hold, Ctrl, Middle),
            (50, _) => (Hold, Ctrl, Right),

            // Alt+Ctrl modifiers
            (24, 'M') => (Press, AltCtrl, Left),
            (25, 'M') => (Press, AltCtrl, Middle),
            (26, 'M') => (Press, AltCtrl, Right),
            (88, 'M') => (Press, AltCtrl, WheelUp),
            (89, 'M') => (Press, AltCtrl, WheelDown),

            (24, 'm') => (Release, AltCtrl, Left),
            (25, 'm') => (Release, AltCtrl, Middle),
            (26, 'm') => (Release, AltCtrl, Right),
            (88, 'm') => (Release, AltCtrl, WheelUp),
            (89, 'm') => (Release, AltCtrl, WheelDown),

            (56, _) => (Hold, AltCtrl, Left),
            (57, _) => (Hold, AltCtrl, Middle),
            (58, _) => (Hold, AltCtrl, Right),

            _ => {
                trace!("unmapped mouse input: b=b m=m");
                return None;
            }
        };

        Some(MouseEvent { k, m, b, x, y })
    }
}
