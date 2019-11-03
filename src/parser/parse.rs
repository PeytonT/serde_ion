use super::version_1_0::parse_value;
use crate::ion_types::{
    IonBlob, IonBoolean, IonClob, IonDecimal, IonFloat, IonInteger, IonList, IonNull, IonString,
    IonStructure, IonSymbol, IonSymbolicExpression, IonTimestamp, IonValue,
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    error::ErrorKind,
    multi::many0,
    sequence::{pair, preceded, tuple},
    Err, IResult,
};

// Binary Ion streams begin with a four-octet Binary Version Marker
// BVM_START MAJOR_VERSION MINOR_VERSION BVM_END
// For version 1.0, this is 0xE0 0x01 0x00 0xEA
pub const BVM_BYTES: usize = 4;
pub const BVM_START_BYTE: u8 = 0xE0;
pub const BVM_END_BYTE: u8 = 0xEA;
pub const BVM_START: [u8; 1] = [BVM_START_BYTE];
pub const BVM_END: [u8; 1] = [BVM_END_BYTE];
pub const VERSION_1_0: IonVersion = IonVersion {
    major: 0x01,
    minor: 0x00,
};
pub const BVM_1_0: [u8; 4] = ion_bvm(VERSION_1_0.major, VERSION_1_0.minor).as_bytes();

#[derive(Clone, Debug, PartialEq)]
pub struct IonVersion {
    major: u8,
    minor: u8,
}

#[derive(Clone, Debug, PartialEq)]
struct BinaryVersionMarker {
    start: u8,
    major: u8,
    minor: u8,
    end: u8,
}

const fn ion_bvm(major_version: u8, minor_version: u8) -> BinaryVersionMarker {
    BinaryVersionMarker {
        start: BVM_START_BYTE,
        major: major_version,
        minor: minor_version,
        end: BVM_END_BYTE,
    }
}

impl BinaryVersionMarker {
    const fn as_bytes(&self) -> [u8; 4] {
        [self.start, self.major, self.minor, self.end]
    }
}

pub fn take_ion_version(input: &[u8]) -> IResult<&[u8], IonVersion> {
    let (input, (start, major, minor, end)) =
        tuple((tag(BVM_START), take(1usize), take(1usize), tag(BVM_END)))(input)?;
    Ok((
        input,
        IonVersion {
            major: major[0],
            minor: minor[0],
        },
    ))
}

pub fn parse(input: &[u8]) -> IResult<&[u8], Vec<IonValue>> {
    alt((
        preceded(tag(BVM_1_0), many0(super::version_1_0::parse_value)),
        version_placeholder,
    ))(input)
}

/** Ion currently has only one version, so this exists to provide alternative in nom::branch::alt. Immediately errors if reached. */
fn version_placeholder(input: &[u8]) -> IResult<&[u8], Vec<IonValue>> {
    Err(Err::Error((input, ErrorKind::NoneOf)))
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn binary_version_marker_test() {
        let data = include_bytes!("../../tests/ion-tests/iontestdata/good/null.10n");
        assert_eq!(&BVM_1_0, &data[0..4]);
    }

    #[test]
    fn take_binary_version_marker_test() {
        let null_null = include_bytes!("../../tests/ion-tests/iontestdata/good/null.10n");
        assert_eq!(
            take_ion_version(null_null),
            Ok((&null_null[4..], VERSION_1_0))
        );
    }

    #[test]
    fn test_parse_value_null_bool() {
        // parse null.null
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/null.10n");
        let parse_result = parse(bytes).unwrap();
        let remaining_bytes = parse_result.0;
        let value = parse_result.1;
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonNull(IonNull::Null)]);
    }

    #[test]
    fn test_parse_value_nop_pad_one_byte() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nopPadOneByte.10n");
        let parse_result = parse(bytes).unwrap();
        let remaining_bytes = parse_result.0;
        let value = parse_result.1;
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonNull(IonNull::Pad)]);
    }

    #[test]
    fn test_parse_value_nop_pad_16_bytes() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nopPad16Bytes.10n");
        let parse_result = parse(bytes).unwrap();
        let remaining_bytes = parse_result.0;
        let value = parse_result.1;
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonNull(IonNull::Pad)]);
    }
}
