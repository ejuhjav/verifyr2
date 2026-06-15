
# verifyr2 1.3.0 (unreleased)

## Improvements
- Refactor image comparison logic implementation to single place (todo: add link)
- Use htmltools for HTML generation outside of shiny app (todo: add link)
- Force word level comparison in HTML export (todo: add link)
- Improve testing related to optional dependencies (todo: add link)

# verifyr2 1.2.0

## Improvements
- Add configuration option descriptions ([#48](https://github.com/ejuhjav/verifyr2/pull/48)).
- Support ignoring differences in empty spaces ([#49](https://github.com/ejuhjav/verifyr2/pull/49)).
- Support word level details comparison (optional) ([#52](https://github.com/ejuhjav/verifyr2/pull/52)).
- Improve options cancel, reset, and detecting changes requiring data reloading ([#54](https://github.com/ejuhjav/verifyr2/pull/54)).
- Improve RTF reading to handle some invalid RTF contents ([#62](https://github.com/ejuhjav/verifyr2/pull/62)).
- Implement HTML report with full comparison details ([#66](https://github.com/ejuhjav/verifyr2/pull/66)).

## Bug fixes
- Fix issue with comment exporting with multiple folder loads ([#51](https://github.com/ejuhjav/verifyr2/pull/51)).
- Fix global variable usage in shiny app that was causing random behavior when running on server ([#60](https://github.com/ejuhjav/verifyr2/pull/60)).

# verifyr2 1.1.0

## Improvements
- Improve file content comparisons to be ~90% faster for large file contents and to not cap at 1000 lines ([#43](https://github.com/ejuhjav/verifyr2/pull/43)).

## Bug fixes
- Add missing 'purrr' library suggestion that is needed for running the 'shiny' application ([#41](https://github.com/ejuhjav/verifyr2/pull/41)).
- Fix 'omit' input usage in 'compare specific files' tab in 'shiny' application ([#42](https://github.com/ejuhjav/verifyr2/pull/42)).

# verifyr2 1.0.0
- Initial CRAN release.
