# missForest NEWS

## 1.6 — 2025-10-13

### Added
- **Ranger backend (default):** `missForest()` can now fit forests via **ranger** with `backend = "ranger"` (default) or retain legacy behavior with `backend = "randomForest"`.
- **Backend selector:** New argument `backend = c("ranger", "randomForest")`.
- **Threading control:** New argument `num.threads` (used by ranger). In `parallelize = "variables"` mode, per-variable ranger calls use `num.threads = 1` to avoid oversubscription.

### Changed
- **Argument mapping for ranger:**
  - `ntree` → `num.trees`
  - `nodesize = c(num, fac)` → `min.bucket` (regression/classification respectively)
  - `sampsize` (counts) → `sample.fraction` (overall or per-class fractions)
  - `classwt` → `class.weights`
  - `cutoff` emulated via probability forests + post-thresholding
  - `maxnodes` has no exact equivalent and is ignored by ranger (consider `max.depth` at the ranger level if needed).
- **Defaults for node size:** `nodesize = c(5, 1)` interpreted consistently across backends (numeric first, factor second).
- **Parallelization behavior clarified:**
  - `parallelize = "variables"`: builds per-variable forests in parallel via **foreach**; ranger runs with `num.threads = 1` inside each task.
  - `parallelize = "forests"`: uses ranger’s internal threading (via `num.threads`) or combines sub-forests for randomForest.

### Fixed
- Prevented name conflicts and S3 confusion by narrowing imports (e.g., `stats::predict`) and explicitly importing only what’s used.
- Resolved NOTE about undefined `xntree` in `foreach` by localizing/binding the chunk variable in the loop.
- Corrected usage of iterator utilities by importing `itertools::isplitVector` and `iterators::idiv`.
- Removed unused macro imports and unicode arrows in Rd files that triggered warnings.

### Documentation
- **Help pages** refreshed with backend details, parameter mappings, and clear parallelization guidance.
- Cross-references now use package-qualified links:
  - `\link[randomForest]{randomForest}` and `\link[ranger]{ranger}`.
- Examples modernized (showing both backends and optional parallel usage).
- **CITATION** modernized to `bibentry()` with `person()`/`c()` and DOI.
- **README** rewritten to clearly explain capabilities, backends, and typical usage patterns.

### Internal / Packaging
- **DESCRIPTION**
  - `Version: 1.6` and current `Date`.
  - Added `Authors@R`.
  - Updated `Imports` to include only used packages (e.g., `ranger`, `randomForest`, `foreach`, `iterators`, `itertools`, `doRNG`, `stats`).
- **NAMESPACE**
  - Explicit, minimal imports to avoid clashes and CRAN checks.
  - Exported only user-facing functions: `missForest`, `mixError`, `nrmse`, `prodNA`, `varClass`.

### Compatibility Notes
- Results across backends should be similar but not identical due to implementation differences (sampling semantics, splits, probability handling).
- `maxnodes` is ignored with `backend = "ranger"`; consider `max.depth` at the ranger level if tree depth control is required.
