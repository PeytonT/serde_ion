use crate::value::{self as ion};
use crate::parser::ion_1_0::text::parse_ion_1_0;
use crate::parser::ion_1_0::text::tests::{find_ion_text, parse_file, test_path};
use itertools::Itertools;

#[test]
fn test_non_equivs() {
    pretty_env_logger::try_init().ok();

    let equivs_files = find_ion_text(&test_path("good/non-equivs"))
        .expect("good/non-equivs directory not found. git submodule update --init ?");

    let results = equivs_files
        .into_iter()
        //        .filter(|path| {
        //            path.file_stem()
        //                .map(|s| s == "localSymbolTableAppend")
        //                .unwrap_or(false)
        //        })
        .map(|path| {
            (
                path.clone(),
                parse_file(&path).expect("equivalencies should be parseable"),
            )
        })
        .collect_vec();

    let mut failed = vec![];
    let mut succeeded = vec![];

    for (path, tlvs) in results {
        for tlv in tlvs {
            // This means that the equivalency has embedded strings which should be parsed prior
            // to comparison.
            let embedded = tlv.has_annotation("embedded_documents");

            if let ion::Value {
                value: ion::Data::List(Some(ion::List { values })),
                ..
            } = tlv
            {
                let values = if embedded {
                    values
                        .into_iter()
                        .enumerate()
                        .filter_map(|(idx, v)| {
                            if let ion::Value {
                                value: ion::Data::String( Some(value) ),
                                ..
                            } = v
                            {
                                let values = parse_ion_1_0(value.as_str()).unwrap_or_else(|e| {
                                    panic!(
                                        "{:?}: embedded document {} should be parseable: {:?}",
                                        path, idx, e
                                    )
                                });
                                Some(values)
                            } else {
                                panic!(
                                    "{:?}: failed to extract string from embedded document list",
                                    path
                                )
                            }
                        })
                        .collect_vec()
                } else {
                    vec![values]
                };

                if non_equivalent(values) {
                    succeeded.push(path.clone())
                } else {
                    failed.push(path.clone())
                }
            }
        }
    }

    if !failed.is_empty() {
        log::error!(
            "Good news first. Correctly processed equivalencies for {} files.",
            succeeded.len()
        );
        log::error!("Failed the following equivalencies:");
        for path in &failed {
            log::error!(" - {:?}", path.file_name());
        }
    }

    assert!(
        failed.is_empty(),
        "Failed {} of {} equivalencies",
        failed.len(),
        succeeded.len() + failed.len()
    );
}

fn non_equivalent(values: Vec<Vec<ion::Value>>) -> bool {
    for mut vec in values.iter().combinations(2) {
        let a = vec.pop().unwrap();
        let b = vec.pop().unwrap();
        if a == b {
            log::warn!("Failed equivalency:\n{:?}\n{:?}", a, b);
            return false;
        }
    }

    true
}
