# Repository instructions

This repository contains the cross-platform R package and CLI.

- Keep platform-specific build logic behind explicit platform checks.
- Use testthat edition 3 and avoid network access in tests.
- Do not commit build artifacts, runtime archives, transcripts, or scratch files.
- Run the test suite and `R CMD check --as-cran` before publishing changes.
