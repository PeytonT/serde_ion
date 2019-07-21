// An operator is an unquoted sequence of one or more of: !#%&*+-./;<=>?@^`|~
pub enum OperatorCharacter {
    PlaceHolder,
    // !
    // #
    // %
    // &
    // *
    // +
    // -
    // .
    // /
    // ;
    // <
    // =
    // >
    // ?
    // @
    // ^
    // `
    // |
    // ~
}

impl OperatorCharacter {
    pub fn as_str(&self) -> String {
        match self {
            &OperatorCharacter::PlaceHolder => unimplemented!(),
            // ...
        }
    }
}

// In the text notation, integer values must be followed by one of the fifteen numeric stop-characters: {}[](),\"\'\ \t\n\r\v\f.
// In the text notation, real values must be followed by one of the fifteen numeric stop-characters: {}[](),\"\'\ \t\n\r\v\f.
pub enum NumericStopCharacter {
    LeftCurlyBracket,
    // {
    RightCurlyBracket,
    // }
    LeftSquareBracket,
    // [
    RightSquareBracket,
    // ]
    LeftParenthesis,
    // (
    RightParenthesis,
    // )
    Comma,
    // ,
    QuotationMark,
    // "
    Apostrophe,
    // '
    Space,
    // U+0020
    Tab,
    // \t
    LineFeed,
    // \n
    CarriageReturn,
    // \r
    VerticalTab,
    // \v
    FormFeed,           // \f
}

impl NumericStopCharacter {
    pub fn as_str(&self) -> String {
        match self {
            &NumericStopCharacter::LeftCurlyBracket => unimplemented!(),
            &NumericStopCharacter::RightCurlyBracket => unimplemented!(),
            &NumericStopCharacter::LeftSquareBracket => unimplemented!(),
            &NumericStopCharacter::RightSquareBracket => unimplemented!(),
            &NumericStopCharacter::LeftParenthesis => unimplemented!(),
            &NumericStopCharacter::RightParenthesis => unimplemented!(),
            &NumericStopCharacter::Comma => unimplemented!(),
            &NumericStopCharacter::QuotationMark => unimplemented!(),
            &NumericStopCharacter::Apostrophe => unimplemented!(),
            &NumericStopCharacter::Space => unimplemented!(),
            &NumericStopCharacter::Tab => unimplemented!(),
            &NumericStopCharacter::LineFeed => unimplemented!(),
            &NumericStopCharacter::CarriageReturn => unimplemented!(),
            &NumericStopCharacter::VerticalTab => unimplemented!(),
            &NumericStopCharacter::FormFeed => unimplemented!(),
        }
    }
}

// The Ion text format supports escape sequences only within quoted strings and symbols.
// Ion supports most of the escape sequences defined by C++, Java, and JSON.
pub enum TextFormatEscapeCharacter {
    PlaceHolder,
    //U+0000	\0	NUL
    //U+0007	\a	alert BEL
    //U+0008	\b	backspace BS
    //U+0009	\t	horizontal tab HT
    //U+000A	\n	linefeed LF
    //U+000C	\f	form feed FF
    //U+000D	\r	carriage return CR
    //U+000B	\v	vertical tab VT
    //U+0022	\"	double quote
    //U+0027	\'	single quote
    //U+003F	\?	question mark
    //U+005C	\\	backslash
    //U+002F	\/	forward slash
    //nothing	\NL	escaped NL expands to nothing
    //U+00HH	\xHH	2-digit hexadecimal Unicode code point
    //U+HHHH	\uHHHH	4-digit hexadecimal Unicode code point
    //U+HHHHHHHH	\UHHHHHHHH	8-digit hexadecimal Unicode code point
}

impl TextFormatEscapeCharacter {
    pub fn as_str(&self) -> String {
        match self {
            &TextFormatEscapeCharacter::PlaceHolder => unimplemented!(),
            // ...
        }
    }
}
