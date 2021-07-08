use num_bigint::BigUint;

// timestamp - Date/time/timezone moments of arbitrary precision
// Mostly ISO 8601
// Enum variant names represent the precision of the variant
//
// In the text format, timestamps follow the W3C note on date and time formats
// (https://www.w3.org/TR/NOTE-datetime), but they must end with the literal “T” if not at least
// whole-day precision. Fractional seconds are allowed, with at least one digit of precision and an
// unlimited maximum. Local-time offsets may be represented as either hour:minute offsets from UTC,
// or as the literal “Z” to denote a local time of UTC.
//
// Ion follows the “Unknown Local Offset Convention” of RFC3339:
// > If the time in UTC is known, but the offset to local time is unknown, this can be
// > represented with an offset of “-00:00”. This differs semantically from an offset of “Z” or
// > “+00:00”, which imply that UTC is the preferred reference point for the specified time.
//
// Local-time offsets are required on timestamps with time and are not allowed on date values.
// Values that are precise only to the year, month, or date are assumed to be UTC values with
// unknown local offset. As a result, Timestamp variants with this level of precision do not need
// to store an offset field.
//
// Zero and negative dates are not valid, so the earliest instant in time that can be
// represented as a timestamp is Jan 01, 0001. As per the W3C note, leap seconds cannot be
// represented.
//
// ---
//
// The formats are as follows (merging https://www.w3.org/TR/NOTE-datetime and Ion-specific requirements).
//
//    Year:
//       YYYYT (eg 1997T)
//    Year and month:
//       YYYY-MMT (eg 1997-07T)
//    Complete date:
//       YYYY-MM-DD(T) (eg 1997-07-16(T)) (the T is optional)
//    Complete date plus hours and minutes:
//       YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
//    Complete date plus hours, minutes and seconds:
//       YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
//    Complete date plus hours, minutes, seconds and a decimal fraction of a second
//       YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)
//
// where:
//
//      YYYY = four-digit year
//      MM   = two-digit month (01=January, etc.)
//      DD   = two-digit day of month (01 through 31)
//      hh   = two digits of hour (00 through 23) (am/pm NOT allowed)
//      mm   = two digits of minute (00 through 59)
//      ss   = two digits of second (00 through 59)
//      s    = one or more digits representing a decimal fraction of a second
//      TZD  = time zone designator (Z or +hh:mm or -hh:mm)
// TODO: Look into enforcing validity.
//  https://github.com/amzn/ion-docs/issues/91
//  https://github.com/amzn/ion-docs/issues/151
#[derive(Clone, Debug, PartialEq)]
pub enum Timestamp {
    Year {
        year: u16,
    },
    Month {
        year: u16,
        month: u8,
    },
    Day {
        year: u16,
        month: u8,
        day: u8,
    },
    Minute {
        // Minutes difference from UTC. Option::None indicates an unknown local offset.
        offset: Option<i16>,
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
    },
    Second {
        // Minutes difference from UTC. Option::None indicates an unknown local offset.
        offset: Option<i16>,
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        second: u8,
    },
    FractionalSecond {
        // Minutes difference from UTC. Option::None indicates an unknown local offset.
        offset: Option<i16>,
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        second: u8,
        fraction_coefficient: BigUint,
        // The restriction of fractional_exponent to i32 rather than BigInt should not pose an issue for any non-pathological use
        // TODO: Revisit this - absolute correctness to the spec is a compelling virtue.
        // TODO: It seems like this could be unsigned, since per the spec it should never need to be positive.
        fraction_exponent: i32,
    },
}

impl Timestamp {
    pub fn to_text(&self) -> String {
        todo!()
    }
}
