use crate::error::SymbolError;

/// # symbol - Interned, Unicode symbolic atoms (aka identifiers)
///
/// symbol - Interned, Unicode symbolic atoms (aka identifiers)
/// A subset of symbols called identifiers can be denoted in text without single-quotes.
/// An identifier is a sequence of ASCII letters, digits, or the
/// characters $ (dollar sign) or _ (underscore), not starting with a digit.
///
/// Ion symbols may have text that is unknown. That is, there is no binding to a (potentially empty) sequence of text.
/// This can happen as a result of not having access to a shared symbol table being imported,
/// or having a symbol table (shared or local) that contains a null slot.
///
/// A processor encountering a symbol with unknown text and a valid SID other than $0 MAY produce an error
/// because this means that the context of the data is missing.
/// Note: serde_ion does not support symbols other than $0 with unknown text.

/// # Structures
///
/// Where Int may be any integer and String may be any string.

/// ## SymbolToken
///
/// <text:String, importLocation:ImportLocation>
///
/// ### SymbolToken equivalence
///
/// In order to fully support the equivalence semantics defined by the specification,
/// SymbolToken equivalence must be implemented as follows.
///
/// When text is
///
/// #### Defined
///
/// SymbolTokens with the same text are equivalent; importLocation is ignored.
///
/// #### Undefined
///
/// ```text
/// if importLocation is
///
///     * Defined: SymbolTokens are equivalent if and only if their importLocations’ importName and importSID are equivalent.
///
///     * Undefined: The SymbolToken represents the special symbol zero,
///       which is used to denote that a SymbolToken has unknown text in any symbol table.
///       SymbolTokens representing symbol zero are equivalent only to other SymbolTokens
///       representing symbol zero.
/// ```
///
/// ### Reading SymbolTokens
///
/// ```text
/// Ion readers must support being provided with an optional catalog to use for resolving shared
/// symbol table imports declared within local symbol tables encountered in the stream.
/// If a declared import is not found in the catalog,
/// all of the symbol IDs in its max_id range will have unknown text.
///
/// Generally, Ion readers provide two kinds of SymbolToken reading APIs, those that return:
///
///     * Raw text (for convenience), and
///     * Complete SymbolTokens (for full fidelity).
///
/// For a Binary reader, if the local symbol ID is
///
///     * Within the current local symbol table’s max_id range, if the local symbol ID maps to text which is
///
///         ** Known, for
///
///             *** Raw text APIs, return that text.
///
///             *** SymbolToken APIs, return a SymbolToken with that text and with an undefined importLocation.
///
///         ** Unknown, if the local symbol ID is
///
///             *** Less than the current local symbol table’s min_local_id (as defined by the specification), for
///
///                 **** Raw text APIs, the implementation should raise an error.
///
///                 **** SymbolToken APIs, return a SymbolToken with undefined text and with importLocation set.
///
///             *** At least min_local_id, then this symbol ID maps to a null (or non-string) slot in the local symbol table, and is treated as symbol zero. For
///
///                 **** Raw text APIs, return undefined text.
///
///             **** SymbolToken APIs, return a SymbolToken with undefined text and an undefined importLocation.
///
///     * Greater than the current local symbol table’s max_id, or less than zero, an error must be raised.
/// ```
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SymbolToken {
    // All SymbolTokens with identical known text are equivalent, import_location is ignored
    Known { text: String },
    // The SymbolToken's text is undefined if the import_location cannot not be resolved
    Unknown { import_location: ImportDescriptor },
    // Special symbol zero denotes unknown text in any symbol table
    Zero,
}

impl SymbolToken {
    pub fn to_text(&self) -> String {
        match self {
            SymbolToken::Known { text } => text.to_string(),
            SymbolToken::Unknown { .. } => todo!(), // should error, but how?
            SymbolToken::Zero => "$0".to_string(),
        }
    }
}

/// ## ImportDescriptor
///
/// <importName:String, version:Int, max_id:Int>
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImportDescriptor {
    import_name: String,
    version: u32,
    max_id: u32,
}

/// ## ImportLocation
///
/// <importName:String, importSID:Int>
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImportLocation {
    import_name: String,
    import_sid: u32,
}

/// ## SymbolTable
///
/// Stores a symbol mapping used to convert encountered Symbols back into text.
///
/// ### Semantics
/// When mapping from symbol ID to string, there is no ambiguity.
/// However, due to unavailable imports, certain IDs may appear to be undefined when binary data
/// is decoded. Any symbol ID outside of the range of the local symbol table (or system symbol
/// table if no local symbol table is defined) for which it is encoded under MUST raise an error.
///
/// When mapping from string to symbol ID, there may be multiple assigned IDs;
/// implementations MUST select the lowest known ID. If an imported table is unavailable,
/// this may cause selection of a greater ID than would be the case otherwise.
/// This restriction ensures that symbols defined by system symbol tables can
/// never be mapped to other IDs.
///
/// Put another way, string-to-SID mappings have the following precedence:
///
/// The system table is always consulted first.
/// Each imported table is consulted in the order of import.
/// Local symbols are last.
#[derive(Clone, Debug, PartialEq)]
pub enum SymbolTable {
    Local(LocalSymbolTable),
    Shared(SharedSymbolTable),
    SystemV1,
}

impl SymbolTable {
    pub fn lookup_text(&self, text: &str) -> Result<SymbolToken, SymbolError> {
        match self {
            SymbolTable::Local(table) => todo!(),
            SymbolTable::Shared(table) => todo!(),
            SymbolTable::SystemV1 => todo!(),
        }
    }

    pub fn lookup_sid(&self, sid: usize) -> Result<SymbolToken, SymbolError> {
        if sid == 0 {
            return Ok(SymbolToken::Zero);
        }
        match self {
            SymbolTable::Local(table) => todo!(),
            SymbolTable::Shared(table) => todo!(),
            SymbolTable::SystemV1 => match SYSTEM_SYMBOL_TABLE_V1.symbols.get(sid) {
                Some(token) => Ok(token.clone()),
                None => Err(SymbolError::AboveMaxId {
                    symbol_id: sid,
                    max_id: 9,
                }),
            },
        }
    }
}

/// ## LocalSymbolTable
///
/// Stores an in-stream symbol table definition.
/// A local symbol table imports either the symbols from a list of shared symbol tables,
/// or may import the current symbol table.
#[derive(Clone, Debug, PartialEq)]
pub struct LocalSymbolTable {
    imports: LocalImport,
    symbols: Vec<String>,
}

// Specifies the import of a SharedSymbolTable into a LocalSymbolTable
#[derive(Clone, Debug, PartialEq)]
pub struct LocalImport {
    name: String,
    version: u32,
    max_id: u32,
}

/// ## SharedSymbolTable
///
/// Stores an in-stream symbol table definition.
/// A local symbol table imports either the symbols from a list of shared symbol tables,
/// or may import the current symbol table.
#[derive(Clone, Debug, PartialEq)]
pub struct SharedSymbolTable {
    name: String,
    version: u32,
    // The imports field is for informational purposes only in shared tables.
    // They assert that this table contains a superset of the strings in each of these named tables.
    // It makes no assertion about any relationship between symbol IDs in this table and the imports,
    // only that the symbols’ text occurs here.
    imports: Vec<SharedImport>,
    symbols: Vec<String>,
}

// Specifies the import of a SharedSymbolTable into a SharedSymbolTable.
// This differs from LocalSymbolTableImport in that the import is informational only, so the max_id is unnecessary.
#[derive(Clone, Debug, PartialEq)]
pub struct SharedImport {
    name: String,
    version: u32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SystemSymbolTable {
    pub name: &'static str,
    pub version: u32,
    // Hopefully there will be const generics by the time the $ion symbol table has more than 1 version
    pub symbols: [SymbolToken; 10],
}

lazy_static! {
    pub static ref SYSTEM_SYMBOL_TABLE_V1: SystemSymbolTable = {
        SystemSymbolTable {
            name: "$ion",
            version: 1,
            symbols: [
                SymbolToken::Zero,
                SymbolToken::Known {
                    text: "$ion".to_string(),
                },
                SymbolToken::Known {
                    text: "$ion_1_0".to_string(),
                },
                SymbolToken::Known {
                    text: "$ion_symbol_table".to_string(),
                },
                SymbolToken::Known {
                    text: "name".to_string(),
                },
                SymbolToken::Known {
                    text: "version".to_string(),
                },
                SymbolToken::Known {
                    text: "imports".to_string(),
                },
                SymbolToken::Known {
                    text: "symbols".to_string(),
                },
                SymbolToken::Known {
                    text: "max_id".to_string(),
                },
                SymbolToken::Known {
                    text: "$ion_shared_symbol_table".to_string(),
                },
            ],
        }
    };
}
