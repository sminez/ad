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
pub enum Key {
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
    // A little gross to call this a key but
    Mouse(MouseEvent),
}

impl Key {
    pub fn from_char(c: char) -> Self {
        match c {
            '\x1b' => Key::Esc,
            '\n' | '\r' => Key::Return,
            '\x7f' => Key::Backspace,
            '\t' => Key::Tab,
            c @ '\x01'..='\x1A' => Key::Ctrl((c as u8 - 0x1 + b'a') as char),
            c @ '\x1C'..='\x1F' => Key::Ctrl((c as u8 - 0x1C + b'4') as char),
            _ => Key::Char(c),
        }
    }

    pub fn try_from_seq2(c1: char, c2: char) -> Option<Self> {
        match (c1, c2) {
            ('O', 'H') => Some(Key::Home),
            ('O', 'F') => Some(Key::End),

            ('[', 'A') => Some(Key::Arrow(Arrow::Up)),
            ('[', 'B') => Some(Key::Arrow(Arrow::Down)),
            ('[', 'C') => Some(Key::Arrow(Arrow::Right)),
            ('[', 'D') => Some(Key::Arrow(Arrow::Left)),
            ('[', 'H') => Some(Key::Home),
            ('[', 'Z') => Some(Key::BackTab),
            ('\x1b', c) if c.is_ascii() => match Self::from_char(c) {
                Key::Char(c) => Some(Key::Alt(c)),
                Key::Ctrl(c) => Some(Key::CtrlAlt(c)),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn try_from_bracket_tilde(c: char) -> Option<Self> {
        match c {
            '1' | '7' => Some(Key::Home),
            '4' | '8' => Some(Key::End),
            '3' => Some(Key::Del),
            '5' => Some(Key::PageUp),
            '6' => Some(Key::PageDown),
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
pub enum MouseEvent {
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
