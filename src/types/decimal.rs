use num_bigint::BigInt;

// decimal - Decimal-encoded real numbers of arbitrary precision
// Reference http://speleotrove.com/decimal/decarith.html
#[derive(Clone, Debug, PartialEq)]
pub struct Decimal {
    pub coefficient: BigInt,
    pub exponent: BigInt,
}

impl Decimal {
    pub fn to_text(&self) -> String {
        todo!()
    }
}
