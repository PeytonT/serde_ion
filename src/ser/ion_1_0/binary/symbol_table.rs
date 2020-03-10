// Create a different data structure than the CurrentSymbolTable used for deserialization.
// Keep the ser/de usages separate, since they will have fairly different access patterns.
//
// Some notable differences are that the serialization structure will need
// - an efficient lookup/insert for SymbolTokens encountered during serialization by
// their text value, since non-system symbols won't yet have assigned indices
// - to be a counting map, since it's valuable for compactness to give the most
// frequently occurring symbols the lowest indices
// - a means to sort the counting map to derive the most efficient indices to assign, essentially,
// transitioning from being a map of SymbolToken->count to being a map of SymbolToken->index
