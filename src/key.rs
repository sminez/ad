//! Keypresses and related user interactions.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Arrow {
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
pub(crate) enum Input {
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
pub(crate) enum MouseButton {
    Left,
    Middle,
    Right,
    WheelUp,
    WheelDown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum MouseEvent {
    Press { b: MouseButton, x: usize, y: usize },
    Hold { x: usize, y: usize },
    Release { x: usize, y: usize },
}

impl MouseEvent {
    pub(crate) fn try_from_raw(b: usize, x: usize, y: usize, m: char) -> Option<Self> {
        use MouseButton::*;

        match (b, m) {
            (0, 'M') => Some(Self::Press { b: Left, x, y }),
            (1, 'M') => Some(Self::Press { b: Middle, x, y }),
            (2, 'M') => Some(Self::Press { b: Right, x, y }),
            (64, 'M') => Some(Self::Press { b: WheelUp, x, y }),
            (65, 'M') => Some(Self::Press { b: WheelDown, x, y }),
            (0..=2 | 64..=65, 'm') | (3, _) => Some(Self::Release { x, y }),
            (32, _) => Some(Self::Hold { x, y }),
            _ => None,
        }
    }
}
