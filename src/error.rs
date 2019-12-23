use thiserror::Error;

#[derive(Error, Debug)]
pub enum SymbolError {
    #[error("invalid symbol_id {symbol_id:?} is greater than local max_id {max_id:?})")]
    AboveMaxId { max_id: u32, symbol_id: u32 },
    #[error("invalid symbol_id {symbol_id:?} is less than local min_local_id {min_local_id:?})")]
    BelowMinId { min_local_id: u32, symbol_id: u32 },
    #[error("the text for SID `{0}` is unknown")]
    UnknownSymbolText(u32),
}
