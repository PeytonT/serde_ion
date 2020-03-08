use crate::{error::TextFormatError, value::Timestamp};
use num_bigint::BigUint;
use std::{convert::TryFrom, fmt};
use time::{ComponentRangeError, UtcOffset};

#[derive(Clone, PartialEq)]
pub enum TextDate {
    Year { year: u16 },
    Month { year: u16, month: u8 },
    Day { date: time::Date },
}

impl TextDate {
    pub(crate) fn day(year: u16, month: u8, day: u8) -> Result<Self, ComponentRangeError> {
        let date = time::Date::try_from_ymd(year as i32, month, day)?;
        Ok(TextDate::Day { date })
    }
    pub(crate) fn month(year: u16, month: u8) -> Self {
        TextDate::Month { year, month }
    }
    pub(crate) fn year(year: u16) -> Self {
        TextDate::Year { year }
    }
}

impl fmt::Debug for TextDate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TextDate::Year { year } => format!("{:04}", year).fmt(f),
            TextDate::Month { year, month } => format!("{:04}-{:02}", year, month).fmt(f),
            TextDate::Day { date } => date.format("%Y-%m-%d").fmt(f),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum TextTime {
    Minute {
        hour: u8,
        minute: u8,
    },
    Second {
        hour: u8,
        minute: u8,
        second: u8,
    },
    FractionalSecond {
        hour: u8,
        minute: u8,
        second: u8,
        fraction_coefficient: BigUint,
        fraction_exponent: i32,
    },
}

impl fmt::Debug for TextTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TextTime::Minute { hour, minute } => format!("{:02}:{:02}", hour, minute).fmt(f),
            TextTime::Second {
                hour,
                minute,
                second,
            } => format!("{:02}:{:02}:{:02}", hour, minute, second,).fmt(f),
            TextTime::FractionalSecond {
                hour,
                minute,
                second,
                fraction_coefficient,
                ..
            } => format!(
                "{:02}:{:02}:{:02}.{}",
                hour,
                minute,
                second,
                // TODO: leading zeroes need to be added.
                fraction_coefficient.to_str_radix(10)
            )
            .fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TextTimestamp {
    date: TextDate,
    time: Option<TextTime>,
    offset: UtcOffset,
}

impl TextTimestamp {
    pub fn new(date: TextDate, time: Option<TextTime>, offset: time::UtcOffset) -> Self {
        Self { date, time, offset }
    }
}

impl TryFrom<TextTimestamp> for Timestamp {
    type Error = TextFormatError;

    fn try_from(timestamp: TextTimestamp) -> Result<Self, Self::Error> {
        let offset = timestamp.offset.as_seconds();
        Ok(match timestamp.time {
            None => match timestamp.date {
                TextDate::Year { year } => Timestamp::Year { year, offset },
                TextDate::Month { year, month } => Timestamp::Month {
                    year,
                    month,
                    offset,
                },
                TextDate::Day { date } => Timestamp::Day {
                    year: date.year() as u16,
                    month: date.month(),
                    day: date.day(),
                    offset,
                },
            },
            Some(time) => match timestamp.date {
                TextDate::Day { date } => match time {
                    TextTime::Minute { hour, minute } => Timestamp::Minute {
                        year: date.year() as u16,
                        month: date.month(),
                        day: date.day(),
                        hour,
                        minute,
                        offset,
                    },
                    TextTime::Second {
                        hour,
                        minute,
                        second,
                    } => Timestamp::Second {
                        year: date.year() as u16,
                        month: date.month(),
                        day: date.day(),
                        hour,
                        minute,
                        second,
                        offset,
                    },
                    TextTime::FractionalSecond {
                        hour,
                        minute,
                        second,
                        fraction_coefficient,
                        fraction_exponent,
                    } => Timestamp::FractionalSecond {
                        year: date.year() as u16,
                        month: date.month(),
                        day: date.day(),
                        hour,
                        minute,
                        second,
                        fraction_coefficient,
                        fraction_exponent,
                        offset,
                    },
                },
                _ => return Err(TextFormatError::ImpreciseDate),
            },
        })
    }
}
