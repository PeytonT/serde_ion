use crate::parser::ion_1_0::text::tests::{find_ion_text, parse_file, test_path};
use crate::parser::parse::parse_ion_text_1_0;
use crate::value::{self as ion, Value};
use itertools::Itertools;
use std::path::Path;

#[test]
fn test_equivs() {
    comparison_test(&test_path("good/equivs"), equivalent);
}

#[test]
fn test_non_equivs() {
    comparison_test(&test_path("good/non-equivs"), non_equivalent);
}

fn equivalent(values: &[Vec<ion::Value>]) -> bool {
    for mut vec in values.iter().combinations(2) {
        let a = vec.pop().unwrap();
        let b = vec.pop().unwrap();
        if a != b {
            log::error!("Failed equivalency:\n{:?}\n{:?}", a, b);
            return false;
        }
    }

    true
}

fn non_equivalent(values: &[Vec<ion::Value>]) -> bool {
    for mut vec in values.iter().combinations(2) {
        let a = vec.pop().unwrap();
        let b = vec.pop().unwrap();
        if a == b {
            log::error!("Failed equivalency:\n{:?}\n{:?}", a, b);
            return false;
        }
    }

    true
}

fn comparison_test<A>(path: &Path, equivalence_predicate: A)
where
    A: Fn(&[Vec<Value>]) -> bool,
{
    pretty_env_logger::try_init().ok();

    let paths = find_ion_text(&path).unwrap_or_else(|_| {
        panic!(
            "Test path {:?} not found. git submodule update --init ?",
            path
        )
    });

    let parsed_test_data = paths
        .into_iter()
        .map(|path| {
            (
                path.clone(),
                parse_file(&path)
                    .unwrap_or_else(|_| panic!("Test at {:?} should be parseable.", &path)),
            )
        })
        .collect_vec();

    let mut failed = vec![];
    let mut succeeded = vec![];

    for (path, tlvs) in parsed_test_data {
        for (tlv_idx, tlv) in tlvs.into_iter().enumerate() {
            // This means that the equivalency has embedded strings which should be parsed prior
            // to comparison.
            let embedded = tlv.has_annotation("embedded_documents");

            match tlv {
                ion::Value {
                    value: ion::Data::Sexp(Some(ion::Sexp { values })),
                    ..
                }
                | ion::Value {
                    value: ion::Data::List(Some(ion::List { values })),
                    ..
                } => {
                    let values = if embedded {
                        values
                            .into_iter()
                            .enumerate()
                            .filter_map(|(idx, v)| {
                                if let ion::Value {
                                    value: ion::Data::String(Some(value)),
                                    ..
                                } = v
                                {
                                    let (_, values) = parse_ion_text_1_0(value.as_str())
                                        .unwrap_or_else(|e| {
                                            panic!(
                                            "{:?}: embedded document {}:{} should be parseable: {:?}, {:?}",
                                            path, tlv_idx, idx, value.as_str(), e
                                        )
                                        });
                                    Some(values)
                                } else {
                                    panic!(
                                        "{:?}: embedded document {}:{} contains non-string values",
                                        path, tlv_idx, idx
                                    )
                                }
                            })
                            .collect_vec()
                    } else {
                        vec![values]
                    };

                    if equivalence_predicate(&values) {
                        succeeded.push(path.clone())
                    } else {
                        failed.push(path.clone())
                    }
                }
                value => panic!("Top level value {:?} is not a list or sexp.", value),
            };
        }
    }

    if !failed.is_empty() {
        log::debug!(
            "Good news first. Correctly processed equivalencies for {} files.",
            succeeded.len()
        );
        log::debug!("Failed the following equivalencies:");
        for path in &failed {
            log::debug!(" - {:?}", path.file_name());
        }
    }

    assert!(
        failed.is_empty(),
        "Failed {} of {} equivalencies",
        failed.len(),
        succeeded.len() + failed.len()
    );
}
