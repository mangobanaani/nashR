# Contributing to nashR

## Code Style

### R Code
- Use R6 classes for game objects
- Use roxygen2 for documentation (with markdown enabled)
- Constructor functions should be lowercase with underscores (e.g., `normal_form`, `bayesian_game`)
- All exported functions must have roxygen2 documentation with `@export`

### C Code
- Include `nashR.h` in all source files
- Register all `.Call` entry points in `src/init.c`
- Use `R_alloc` for temporary memory (auto-freed by R)
- Use `PROTECT`/`UNPROTECT` for all `SEXP` allocations

## Testing

- Tests use testthat (edition 3) in `tests/testthat/`
- All new features must have corresponding tests
- Run tests with `make test` or `Rscript -e "testthat::test_dir('tests/testthat')"`

## Before Submitting

- Run `R CMD check` with no errors or warnings
- Run `make check` for the full build-and-check cycle
- Ensure all existing tests still pass
