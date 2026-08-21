# Changelog

## rbm25 2.3.2

- NOTE: Switch from semver on the R-package level to taking the version
  of the underlying rust `bm25` crate.
- Upgrade to bm25 crate version 2.3.2
- fix “found non-API call to R” NOTE on CRAN
- expose the `avgdl` argument to users
- add store and load of weights
- pin `unicode-segmentation` to 1.12.0, since 1.13+ requires rustc 1.85
  and raises the effective MSRV above the package’s stated
  `rust-version`
- `store()`/[`load()`](https://rdrr.io/r/base/load.html) now save the
  corpus and BM25 parameters and rebuild the engine on load, rather than
  serializing the engine directly; `bm25` 2.3.2 does not implement
  `serde::Serialize`/`Deserialize` for `SearchEngine`

## rbm25 0.0.4

CRAN release: 2025-04-14

- update `extendr` and uses `rextendr::vendor_pkgs(overwrite=TRUE)` for
  vendoring rust dependencies.

## rbm25 0.0.2

- fix CRAN comments & resubmit

## rbm25 0.0.1

- initial functionality & tests
