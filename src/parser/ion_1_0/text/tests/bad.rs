use crate::parser::ion_1_0::text::tests::{find_ion_text, parse_file, test_path};
use itertools::Itertools;

#[test]
fn test_bad() {
    let bad_ion_files = find_ion_text(&test_path("bad"))
        .expect("bad tests directory not found. git submodule update --init ?");

    let results = bad_ion_files
        .into_iter()
        .map(|path| (path.clone(), parse_file(&path)))
        .collect_vec();
    let failed = results.iter().filter(|(_, r)| r.is_ok()).collect_vec();
    let succeeded = results.iter().filter(|(_, r)| r.is_err()).collect_vec();

    if !failed.is_empty() {
        log::debug!(
            "Good news first. Correctly failed to read {} files.",
            succeeded.len()
        );
        log::debug!("Read {} invalid .ion files:", failed.len());
        for (path, _) in &failed {
            log::debug!(" - {:?}", path.file_name());
        }
    }

    assert!(
        failed.is_empty(),
        "Accidentally parsed {} bad Ion files",
        failed.len()
    );
}
