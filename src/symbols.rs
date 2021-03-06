use crate::text::escape;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::fmt;

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
///
/// # Structures
///
/// Where Int may be any integer and String may be any string.
///
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
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SymbolToken {
    // All SymbolTokens with identical known text are equivalent, import_location is ignored
    Known { text: String },
    // The SymbolToken's text is undefined if the import_location cannot not be resolved
    Unknown { import_location: ImportLocation },
    // Special symbol zero denotes unknown text in any symbol table
    Zero,
}

impl SymbolToken {
    pub fn to_text(&self) -> String {
        match self {
            SymbolToken::Known { text } => {
                // Symbols in the 'identifiers' subset can be denoted in text without single-quotes.
                if IDENTIFIER_SYMBOL_REGEX.is_match(text) {
                    // 'identifiers' contain no code points that need to be escaped
                    return text.to_string();
                }
                // Otherwise, symbols are delimited by single-quotes and use the same
                // escape characters as strings.
                format!("\'{}\'", escape(text))
            }
            SymbolToken::Unknown { .. } => todo!(), // should error, but how?
            SymbolToken::Zero => "$0".to_string(),
        }
    }

    // Within S-expressions, the rules for unquoted symbols include additional tokens.
    pub fn to_sexp_text(&self) -> String {
        match self {
            SymbolToken::Known { text } => {
                // Symbols in the 'identifiers' subset can be denoted in text without single-quotes.
                if IDENTIFIER_SYMBOL_REGEX.is_match(text) {
                    // 'identifiers' contain no code points that need to be escaped
                    return text.to_string();
                }
                // Within S-expressions, the 'operators' symbols are also unquoted.
                if OPERATOR_SYMBOL_REGEX.is_match(text) {
                    // 'operators' contain no code points that need to be escaped
                    return text.to_string();
                }
                // Otherwise, symbols are delimited by single-quotes and use the same
                // escape characters as strings.
                format!("\'{}\'", escape(text))
            }
            SymbolToken::Unknown { .. } => todo!(), // should error, but how?
            SymbolToken::Zero => "$0".to_string(),
        }
    }
}

impl From<&str> for SymbolToken {
    fn from(symbol: &str) -> Self {
        let text = symbol.to_string();
        SymbolToken::Known { text }
    }
}

/// ## ImportLocation
///
/// <importName:String, importSID:Int>
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImportLocation {
    import_name: String,
    // Index into a shared symbol table’s list of symbols.
    // The first symbol in a shared symbol table always has an ImportSID of 1.
    import_sid: u32,
}

// Implement `Display` for `ImportLocation`.
impl fmt::Display for ImportLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "ImportLocation ({}, {})",
            self.import_name, self.import_sid
        )
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

impl ImportDescriptor {
    pub(crate) fn new(import_name: String, version: u32, max_id: u32) -> Self {
        Self {
            import_name,
            version,
            max_id,
        }
    }
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
pub(crate) struct SystemSymbolTable<const LENGTH: usize> {
    pub name: &'static str,
    pub version: u32,
    pub symbols: [SymbolToken; LENGTH],
}

pub(crate) const SYSTEM_SYMBOL_TABLE_V1_MAX_ID: usize = 9;

lazy_static! {
    pub(crate) static ref SYSTEM_SYMBOL_TABLE_V1: SystemSymbolTable<{ SYSTEM_SYMBOL_TABLE_V1_MAX_ID + 1 }> = {
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

    // A subset of symbols called identifiers can be denoted in text without single-quotes.
    // An identifier is a sequence of ASCII letters, digits, or the characters $ or _
    // that does not start with a digit.
    // TODO: Tests for this regex. So far this has only been sanity-checked with https://rustexp.lpil.uk/.
    pub(crate) static ref IDENTIFIER_SYMBOL_REGEX: Regex = Regex::new(r"^(?:[[:alpha:]]|_|\$)+(?:[[:alnum:]]|_|\$)*$").unwrap();

    // An operator is an unquoted sequence of one or more of the following nineteen ASCII characters: !#%&*+-./;<=>?@^`|~
    // TODO: Tests for this regex. So far this has only been sanity-checked with https://rustexp.lpil.uk/.
    pub(crate) static ref OPERATOR_SYMBOL_REGEX: Regex = Regex::new(r"^(?:!|#|%|&|\*|\+|-|\.|/|;|<|=|>|\?|@|\^|`|\||~)+$").unwrap();
}
