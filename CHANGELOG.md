# Changelog
## Unreleased

## v0.0.5 (2025-12-09)
### Fixed
- Correctly bind the policy variable in the query to retrieve policy data.

### Added
- Allow to specify a filename to load policies from. ([#7](https://github.com/lblod/odrl-parser-service/pull/7))
- Added API function to load prefixes from a TTL file ([#7](https://github.com/lblod/odrl-parser-service/pull/7))

### Changed
- Improve error handling when trying to read from non-existing configuration files ([#7](https://github.com/lblod/odrl-parser-service/pull/7))

## v0.0.4 (2025-12-08)
### Added
- Load prefixes from a configuration file ([#6](https://github.com/lblod/odrl-parser-service/pull/6))

## v0.0.3 (2025-10-15)
### Changed
- Revised docker configuration to support live reloading ([#5](https://github.com/lblod/odrl-parser-service/pull/5)) [LBRON-488]
- Define prefixes in generated configurations ([#4](https://github.com/lblod/odrl-parser-service/pull/4))

## v0.0.2 (2025-10-07)
### Changed
- ODRL polices should now explicitly model Asset resources as SHACL node shapes ([#2](https://github.com/lblod/odrl-parser-service/pull/2)) [LBRON-560]

### Added
- Signal errors when input data is incomplete, as well as refactored data retrieval and parsing ([#3](https://github.com/lblod/odrl-parser-service/pull/3))

## v0.0.1 (2025-09-23)
Initial release.
