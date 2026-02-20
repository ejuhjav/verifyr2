
# verifyr2 1.2.0 (unreleased)

## New features
- Support ignoring differences in empty spaces (optional) (TODO - add link before release)
- Support word level details comparison (optional) (TODO - add link before release)
- Improve options cancel, reset, and detecting changes requiring data reloading (TODO - add link before release) 
- Add configuration option descriptions (TODO - add link before release)

## Bug fixes
- Fix issue with comment exporting with multiple folder loads (TODO - add link before release)

# verifyr2 1.1.0

## Performance improvements
- Improve file content comparisons to be ~90% faster for large file contents and to not cap at 1000 lines ([#43](https://github.com/ejuhjav/verifyr2/pull/43)).

## Bug fixes
- Fix 'omit' input usage in 'compare specific files' tab in 'shiny' application ([#42](https://github.com/ejuhjav/verifyr2/pull/42)).
- Add missing 'purrr' library suggestion that is needed for running the 'shiny' application ([#41](https://github.com/ejuhjav/verifyr2/pull/41)).

# verifyr2 1.0.0
- Initial CRAN release.
