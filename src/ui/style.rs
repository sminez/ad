//! Implementation agnostic UI styling
use serde::Deserialize;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Color {
    pub fn as_rgb_hex_string(&self) -> String {
        let rgb: u32 = ((self.r as u32) << 16) + ((self.g as u32) << 8) + self.b as u32;
        format!("#{:0>6X}", rgb)
    }
}

impl From<Color> for String {
    fn from(value: Color) -> Self {
        value.as_rgb_hex_string()
    }
}

impl TryFrom<&str> for Color {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, String> {
        let [_, r, g, b] = match u32::from_str_radix(s.strip_prefix('#').unwrap_or(s), 16) {
            Ok(hex) => hex.to_be_bytes(),
            Err(e) => return Err(format!("invalid color ('{s}'): {e}")),
        };

        Ok(Self { r, g, b })
    }
}

impl TryFrom<String> for Color {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::try_from(value.as_str())
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Styles {
    pub fg: Option<Color>,
    pub bg: Option<Color>,
    pub bold: bool,
    pub italic: bool,
    pub underline: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CurShape {
    Block,
    Bar,
    Underline,
    BlinkingBlock,
    BlinkingBar,
    BlinkingUnderline,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn color_roundtrip() {
        let s = "#FF9E3B";
        let c: Color = s.try_into().unwrap();

        assert_eq!(c.as_rgb_hex_string(), s);
    }
}
