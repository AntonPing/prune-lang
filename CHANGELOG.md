# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.1] - 2025-11-22

### Added

- Create file `CHANGELOG.md`

### Changed

- Modify `avl_tree` examples for better performance.

### Fixed

- Fix a bug in parser error reporting.
- Fix a bug in SMT solver configuration.
- Fix a bug in example `avl_tree_bad`.
- Fix a vibe typo in `README.md`.

## [0.1.0] - 2025-11-15

### Added

- Update `README.md`.
- Support ad-hoc syntax for unit return value.
- Support both Z3 and CVC5 SMT solver backend.
- Support phoney SMT solver backend.
- Add special guard syntax for boolean values.
- Implement CLI tool.

### Changed

- Modify conflict-driven heuristic implementation.

### Removed

- Remove deprecated predicate syntax.
