mod subfield;

use crate::parser::parse::BVM_1_0;
use crate::symbols::{SymbolToken, SYSTEM_SYMBOL_TABLE_V1_SIZE};
use crate::value::{Data, Value};
use crate::Version;
use itertools::Itertools;
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::mem::replace;

/// A binary writer takes a stream of Ion values and produces the corresponding binary bytestream.
///
/// For maximum compactness, write from the writer only once. This ensures that only a single local
/// symbol table is written, and that symbol IDs are efficiently distributed.
/// To minimize writes, use only one Ion version. Each time the version of written values changes,
/// the writer must write the new IVM and a new symbol table.
///
/// The writer retains the current symbol table between writes. For this reason, multiple writes
/// from the same writer will usually be more compact than joining the output of multiple writers.
pub struct Writer {
    // serialized bytes
    bytes: Vec<u8>,
    // local symbol table
    local_symbol_table: Option<LocalSymbolTable>,
    // symbols encountered in the current segment
    symbol_buffer: SymbolAccumulator,
    // values for the current segment
    value_buffer: Vec<Value>,
    // current segment Ion version
    current_version: Version,
}

impl Writer {
    pub fn new(current_version: Version) -> Writer {
        Writer {
            bytes: vec![],
            local_symbol_table: None,
            symbol_buffer: SymbolAccumulator::new(),
            value_buffer: vec![],
            current_version,
        }
    }

    /// Writes the current segment to bytes, consisting of the IVM for the current Ion version,
    /// the accumulated additions to the symbol table, and the buffered values.
    pub fn write(&mut self) {
        // There's only one version currently, but perhaps someday there will be more.
        match self.current_version {
            Version::Ion_1_0 => {
                self.bytes.extend_from_slice(&BVM_1_0);
                // Absorb the
                self.update_symbol_table_v1_0();
                // TODO: Serialize the symbol table
                // TODO: Serialize the buffered values using the symbol table
            }
        }
    }

    pub fn extend(&mut self, version: Version, values: Vec<Value>) {
        if self.current_version != version {
            self.write();
            self.current_version = version;
        }
        for value in &values {
            self.symbol_buffer.accumulate_symbols(value);
        }
        self.value_buffer.extend(values);
    }

    pub fn append(&mut self, version: Version, value: Value) {
        if self.current_version != version {
            self.write();
            self.current_version = version;
        }
        self.symbol_buffer.accumulate_symbols(&value);
        self.value_buffer.push(value);
    }

    /// Absorbs the accumulated symbols in the symbol buffer into the local symbol table.
    /// If there is a current symbol table it will be imported and extended, and otherwise a new
    /// local symbol table will be created.
    fn update_symbol_table_v1_0(&mut self) {
        // Steal the accumulator's counting map and reset the accumulator.
        let mut symbol_counts: HashMap<String, i32> = replace(
            self.symbol_buffer.symbol_counts.borrow_mut(),
            HashMap::new(),
        );

        match &mut self.local_symbol_table {
            // Initialize a local symbol table from scratch if there is no previous table to import.
            None => {
                // Remove Ion 1.0 system symbols. These symbols have fixed symbol IDs, so if they managed to
                // find their way into the count we don't want to unnecessarily include them in the local
                // symbol table's symbol list.
                symbol_counts.remove("$ion");
                symbol_counts.remove("$ion_1_0");
                symbol_counts.remove("$ion_symbol_table");
                symbol_counts.remove("name");
                symbol_counts.remove("version");
                symbol_counts.remove("imports");
                symbol_counts.remove("symbols");
                symbol_counts.remove("max_id");
                symbol_counts.remove("$ion_shared_symbol_table");

                // Sort the accumulated symbols in reverse order by count. Earlier symbols get represented
                // with shorter VarUInts, so it is space-efficient to put the most common symbols first.
                let ordered_symbols: Vec<String> = symbol_counts
                    .into_iter()
                    .sorted_by_key(|x| -x.1)
                    .map(|x| x.0)
                    .collect();

                // Holds the symbol->offset map for efficiently serializing symbols.
                let mut symbol_offsets: HashMap<String, usize> = HashMap::new();

                // Add the Ion 1.0 system symbols to the offset map.
                symbol_offsets.insert("$ion".to_owned(), 1);
                symbol_offsets.insert("$ion_1_0".to_owned(), 2);
                symbol_offsets.insert("$ion_symbol_table".to_owned(), 3);
                symbol_offsets.insert("name".to_owned(), 4);
                symbol_offsets.insert("version".to_owned(), 5);
                symbol_offsets.insert("imports".to_owned(), 6);
                symbol_offsets.insert("symbols".to_owned(), 7);
                symbol_offsets.insert("max_id".to_owned(), 8);
                symbol_offsets.insert("$ion_shared_symbol_table".to_owned(), 9);

                // Add the accumulated symbols to the offset map.
                for (index, value) in ordered_symbols.iter().enumerate() {
                    symbol_offsets.insert(value.to_owned(), index);
                }

                let max_id = ordered_symbols.len() + SYSTEM_SYMBOL_TABLE_V1_SIZE;

                self.local_symbol_table = Some(LocalSymbolTable {
                    symbol_offsets,
                    ordered_symbols,
                    max_id,
                    import_previous_table: false,
                });
            }
            // Update the current local symbol table if one exists.
            Some(table) => {
                // Sort the accumulated symbols in reverse order by count. There's never a downside
                // to doing this, though it's much less likely to save space when extending an existing table.
                // Filter out any symbols that already exist in the table.
                let ordered_symbols: Vec<String> = symbol_counts
                    .into_iter()
                    .filter(|x| table.symbol_offsets.contains_key(&x.0))
                    .sorted_by_key(|x| -x.1)
                    .map(|x| x.0)
                    .collect();

                // Add the new symbols to the offset map.
                for (index, value) in ordered_symbols.iter().enumerate() {
                    table
                        .symbol_offsets
                        .insert(value.to_owned(), index + table.max_id);
                }

                table.max_id += ordered_symbols.len();
                table.import_previous_table = true;
            }
        }
    }
}

#[derive(Debug)]
struct SymbolAccumulator {
    // Mapping from each literal symbol string to the number of times it has been encountered.
    symbol_counts: HashMap<String, i32>,
}

impl SymbolAccumulator {
    fn new() -> SymbolAccumulator {
        SymbolAccumulator {
            symbol_counts: HashMap::new(),
        }
    }

    fn increment(&mut self, literal: &str) {
        let counter = self.symbol_counts.entry(literal.to_owned()).or_insert(0);
        *counter += 1;
    }

    fn accumulate_symbols(&mut self, value: &Value) {
        for annotation in &value.annotations {
            if let Some(SymbolToken::Known { text }) = annotation {
                self.increment(text);
            }
        }
        match &value.value {
            Data::Symbol(Some(SymbolToken::Known { text })) => {
                self.increment(text);
            }
            Data::Struct(Some(r#struct)) => {
                for (token, value) in &r#struct.fields {
                    if let SymbolToken::Known { text } = token {
                        self.increment(text);
                    }
                    self.accumulate_symbols(value);
                }
            }
            Data::List(Some(list)) => {
                for value in &list.values {
                    self.accumulate_symbols(&value);
                }
            }
            Data::Sexp(Some(sexp)) => {
                for value in &sexp.values {
                    self.accumulate_symbols(&value);
                }
            }
            // No other Value variants contain Symbols.
            _ => {}
        }
    }
}

#[derive(Debug)]
struct LocalSymbolTable {
    // Mapping from each literal symbol string to its index in the local symbol table.
    // TODO: The mapping must be regenerated each time the version changes, which is currently never.
    symbol_offsets: HashMap<String, usize>,
    // The 'symbols' list of the serialized local symbol table.
    // Added in addition to any symbols imported from the system table or any imported tables, including the previous table.
    ordered_symbols: Vec<String>,
    // It's useful to keep track of the size of the logical symbol list.
    // This will be more useful if imports other than the current table are ever implemented.
    max_id: usize,
    // Informs table serialization logic of whether there is a preceding table that should be imported.
    import_previous_table: bool,
}