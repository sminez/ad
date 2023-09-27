#[derive(Debug, PartialEq, Eq)]
pub enum Key {
    Char(char),
    Ctrl(char),
    Tab,
    Return,
    Backspace,
    Arrow(Arrow),
    Del,
    Home,
    End,
    PageUp,
    PageDown,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Arrow {
    Up,
    Down,
    Left,
    Right,
}

impl Key {
    pub fn try_from_char(c: char) -> Option<Self> {
        match c {
            '\x1b' => None,
            '\n' | '\r' => Some(Key::Return),
            '\x7f' => Some(Key::Backspace),
            '\t' => Some(Key::Tab),
            c @ '\x01'..='\x1A' => Some(Key::Ctrl((c as u8 - 0x1 + b'a') as char)),
            c @ '\x1C'..='\x1F' => Some(Key::Ctrl((c as u8 - 0x1C + b'4') as char)),
            _ => Some(Key::Char(c)),
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
