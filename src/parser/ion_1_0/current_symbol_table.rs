use crate::error::SymbolError;
use crate::ion_types::{Data, List, String, Struct};
use crate::symbols::{SymbolToken, SYSTEM_SYMBOL_TABLE_V1};
use std::collections::hash_map::HashMap;

pub enum CurrentSymbolTable {
    Local { symbols: Vec<SymbolToken> },
    SystemV1,
}

impl CurrentSymbolTable {
    pub fn lookup_sid(&self, sid: usize) -> Result<SymbolToken, SymbolError> {
        if sid == 0 {
            return Ok(SymbolToken::Zero);
        }
        match self {
            CurrentSymbolTable::Local { symbols } => match symbols.get(sid) {
                Some(token) => Ok(token.clone()),
                None => Err(SymbolError::AboveMaxId {
                    symbol_id: sid,
                    max_id: symbols.len(),
                }),
            },
            CurrentSymbolTable::SystemV1 => match SYSTEM_SYMBOL_TABLE_V1.symbols.get(sid) {
                Some(token) => Ok(token.clone()),
                None => Err(SymbolError::AboveMaxId {
                    symbol_id: sid,
                    max_id: 9,
                }),
            },
        }
    }
}

/// # Local Symbol Tables
///
/// A local symbol table defines symbols through two mechanisms, both of which are optional.
///
/// First, it imports the symbols from one or more shared symbol tables,
/// offsetting symbol IDs appropriately so they do not overlap.
/// Instead of importing the symbols from shared symbol tables,
/// a local symbol table may import the current symbol table.
///
/// Second, it defines local symbols similarly to shared tables.
/// The latter aspect is generally not managed by users:
/// the system uses this form in the binary encoding to record local symbols encountered during parsing.
///
/// When immediately following an explicit system ID, a top-level struct whose first annotation is
/// $ion_symbol_table is interpreted as a local symbol table. If the struct is null (null.struct)
/// then it is treated as if it were an empty struct.
///
/// The imports field should be the symbol $ion_symbol_table or a list as specified in the
/// following section.
///
/// The symbols field should be a list of strings. If the field is missing or has any other type,
/// it is treated as if it were an empty list.
///
/// Null elements in the symbols list declare unknown symbol text (“gaps”) for its SID within the
/// sequence. Any element of the list that is not a string must be interpreted as if it were null.
/// Any SIDs that refer to null slots in a local symbol table are equivalent to symbol zero.
///
/// Any other field (including, for example, name or version) is ignored.

// Modify the current symbol table according to the encountered local symbol table.
pub(crate) fn update_current_symbol_table(current: &mut CurrentSymbolTable, encountered: &Struct) {
    let (imports, symbols): (TableImport, Vec<SymbolToken>) = match encountered {
        Struct::Null => (TableImport::None, vec![]),
        Struct::Struct { values } => {
            let keys: HashMap<&str, usize> = values
                .iter()
                .enumerate()
                .filter_map(|(i, val)| match &val.0 {
                    SymbolToken::Known { text } => Some((text.as_str(), i)),
                    SymbolToken::Unknown { .. } => None,
                    SymbolToken::Zero => None,
                })
                .collect();
            // The imports field should be the symbol $ion_symbol_table or a list as specified.
            let imports = match keys.get("imports") {
                None => TableImport::None,
                Some(index) => match &values.get(*index).unwrap().1.value {
                    Data::List(list) => match list {
                        List::Null => TableImport::None,
                        List::List { values } => TableImport::Imports(
                            values
                                .iter()
                                .filter_map(|value| match &value.value {
                                    Data::Struct(val) => Some(val.clone()),
                                    _ => None,
                                })
                                .collect(),
                        ),
                    },
                    _ => TableImport::None,
                },
            };
            // The symbols field should be a list of strings. If the field is missing or has any other type,
            // it is treated as if it were an empty list.
            // Null elements in the symbols list declare unknown symbol text (“gaps”) for its SID within the
            // sequence. Any element of the list that is not a string must be interpreted as if it were null.
            // Any SIDs that refer to null slots in a local symbol table are equivalent to symbol zero.
            let symbols = match keys.get("symbols") {
                None => vec![],
                Some(index) => match &values.get(*index).unwrap().1.value {
                    Data::List(list) => match list {
                        List::Null => vec![],
                        List::List { values } => values
                            .iter()
                            .map(|value| match &value.value {
                                Data::String(string) => match string {
                                    String::Null => SymbolToken::Zero,
                                    String::String { value } => SymbolToken::Known {
                                        text: value.clone(),
                                    },
                                },
                                _ => SymbolToken::Zero,
                            })
                            .collect(),
                    },
                    _ => vec![],
                },
            };
            (imports, symbols)
        }
    };
    match imports {
        TableImport::None => {
            if symbols.is_empty() {
                *current = CurrentSymbolTable::SystemV1;
                return;
            }
            append_symbols_to_system_table(current, symbols);
        }
        TableImport::IonSymbolTable => match current {
            CurrentSymbolTable::Local {
                symbols: existing_symbols,
            } => {
                existing_symbols.extend(symbols);
            }
            CurrentSymbolTable::SystemV1 => {
                append_symbols_to_system_table(current, symbols);
            }
        },
        TableImport::Imports(imports) => todo!(),
    }
}

fn append_symbols_to_system_table(table: &mut CurrentSymbolTable, symbols: Vec<SymbolToken>) {
    let mut symbol_vec: Vec<SymbolToken> = Vec::new();
    symbol_vec.extend(SYSTEM_SYMBOL_TABLE_V1.symbols.iter().cloned());
    symbol_vec.extend(symbols);
    *table = CurrentSymbolTable::Local {
        symbols: symbol_vec,
    };
}

/// Imports
///
/// A local symbol table implicitly imports the system symbol table that is active at the point
/// where the local table is encountered.
///
/// If the value of the imports field is the symbol $ion_symbol_table, then the all of the symbol
/// ID assignments in the current symbol table are imported into the new local table.
/// Thus, if the current symbol table was the system symbol table,
/// then processing is identical to having no imports field value.
///
/// If the value of the imports field is a list, each element of the list must be a struct;
/// each element that is null or is not a struct is ignored.
///
/// Each import (including the implicit system table import) allocates a contiguous,
/// non-overlapping sequence of symbol IDs. The system symbols start at 1,
/// each import starts one past the end of the previous import, and the local symbols start
/// immediately after the last import. The size of each import’s subsequence is defined by the
/// max_id on the import statement, regardless of the actual size of the referenced table.
enum TableImport {
    None,
    IonSymbolTable,
    Imports(Vec<Struct>),
}

/// Import structs in an import list are processed in order as follows:
///
/// If no name field is defined, or if it is not a non-empty string, the import clause is ignored.
/// If the name field is "$ion", the import clause is ignored.
/// If no version field is defined, or if it is null, not an int, or less than 1, act as if it is 1.
/// If a max_id field is defined but is null, not an int, or less than zero, act as if it is undefined.
/// Select a shared symbol table instance as follows:
/// Query the catalog to retrieve the specified table by name and version.
/// If an exact match is not found:
/// If max_id is undefined, implementations MUST raise an error and halt processing.
/// Otherwise query the catalog to retrieve the table with the given name and the greatest version available.
/// If no table has been selected, substitute a dummy table containing max_id undefined symbols.
/// If max_id is undefined, set it to the largest symbol ID of the selected table (which will necessarily be an exact match).
/// Allocate the next max_id symbol IDs to this imported symbol table.
/// After processing imports, a number of symbol IDs will have been allocated,
/// including at least those of a system symbol table.
/// This number is always well-defined, and any local symbols will be numbered immediately beyond
/// that point. We refer to the smallest local symbol ID as the local min_id.
///
/// Note: This specification allows a local table to declare multiple imports with the same name,
/// perhaps even the same version. Such a situation provides redundant data and allocates
/// unnecessary symbol IDs but is otherwise harmless.
///
/// Semantics
/// When mapping from symbol ID to string, there is no ambiguity. However, due to unavailable
/// imports, certain IDs may appear to be undefined when binary data is decoded. Any symbol ID
/// outside of the range of the local symbol table (or system symbol table if no local symbol
/// table is defined) for which it is encoded under MUST raise an error.
///
/// When mapping from string to symbol ID, there may be multiple assigned IDs; implementations
/// MUST select the lowest known ID. If an imported table is unavailable, this may cause selection
/// of a greater ID than would be the case otherwise. This restriction ensures that symbols defined
/// by system symbol tables can never be mapped to other IDs.
///
/// Put another way, string-to-SID mappings have the following precedence:
///
/// The system table is always consulted first.
/// Each imported table is consulted in the order of import.
/// Local symbols are last.
fn handle_imports() {
    todo!()
}
