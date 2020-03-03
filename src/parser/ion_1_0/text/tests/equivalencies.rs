use crate::{
    parser::ion_1_0::text::tests::{find_ion_text, parse_file, test_path},
    parser::parse::parse_ion_text_1_0,
    value::{self as ion, Value},
};
use itertools::Itertools;
use std::path::{Path, PathBuf};

#[test]
fn test_equivs() {
    fn equivalent(values: &[Vec<ion::Value>]) -> Result<(), String> {
        for mut vec in values.iter().combinations(2) {
            let a = vec.pop().unwrap();
            let b = vec.pop().unwrap();
            if a != b {
                return Err(format!(
                    "Failed equivalency:\n{:?} should equal \n{:?}",
                    a, b
                ));
            }
        }

        Ok(())
    }

    comparison_test(&test_path("good/fquivs"), equivalent);
}

#[test]
fn test_non_equivs() {
    fn non_equivalent(values: &[Vec<ion::Value>]) -> Result<(), String> {
        for mut vec in values.iter().combinations(2) {
            let a = vec.pop().unwrap();
            let b = vec.pop().unwrap();
            if a == b {
                return Err(format!(
                    "Failed equivalency:\n{:?} should not equal \n{:?}",
                    a, b
                ));
            }
        }

        Ok(())
    }

    comparison_test(&test_path("good/non-equivs"), non_equivalent);
}

fn comparison_test<P>(path: &Path, equivalence_predicate: P)
where
    P: Fn(&[Vec<Value>]) -> Result<(), String>,
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
        .map(|path| (path.clone(), parse_file(&path)))
        .collect_vec();

    let mut failed: Vec<(PathBuf, String)> = vec![];
    let mut succeeded: Vec<PathBuf> = vec![];

    for (path, result) in parsed_test_data {
        match result {
            Ok(tlvs) => {
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
                                            match parse_ion_text_1_0(value.as_str()) {
                                                Ok((_, values)) => Some(values),
                                                Err(e) => panic!(
                                                    "{:?}: embedded document {}:{} should be parseable: {:?}, {:?}",
                                                    path, tlv_idx, idx, value.as_str(), e
                                                )
                                            }
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

                            match equivalence_predicate(&values) {
                                Ok(_) => succeeded.push(path.clone()),
                                Err(e) => failed.push((path.clone(), e)),
                            }
                        }
                        value => panic!("Top level value {:?} is not a list or sexp.", value),
                    };
                }
            }
            Err(e) => failed.push((path.clone(), format!("failed to parse test file: {}", e))),
        }

        if !failed.is_empty() {
            log::debug!(
                "Good news first. Correctly processed equivalencies for {} files.",
                succeeded.len()
            );
            log::debug!("Failed the following equivalencies:");
            for (path, error) in &failed {
                log::debug!(" - {:?}: {}", path.file_name(), error);
            }
        }

        assert!(
            failed.is_empty(),
            "Failed {} of {} equivalencies",
            failed.len(),
            succeeded.len() + failed.len()
        );
    }
}
