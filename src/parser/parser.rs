use nom;
use nom::alt;
use nom::do_parse;
use nom::many0;
use nom::named;
use nom::tag;
use nom::take;
use nom::IResult;

use crate::ion_types;

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

named!(pub take_ion_version( &[u8] ) -> IonVersion,
  do_parse!(
    start: tag!(BVM_START) >>
    major: take!(1) >>
    minor: take!(1) >>
    end: tag!(BVM_END) >>
    (IonVersion {major: major[0], minor: minor[0]})
  )
);

// Needs mechanism for parsing multiple values
//named!(parse, alt!(
//    tag!(&BVM_1_0)            => { |rest: &[u8]| ion_1_0::parse(rest) }
//    // Handling for future Ion versions
//));

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
