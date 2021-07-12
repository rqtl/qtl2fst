## qtl2fst 0.25-1 (2021-07-12)

### Minor changes

- If needed directory doesn't exist, create it rather than stopping
  with an error (`fst_genoprob()` and `bind_fst()` and functions that
  call them). Fixes [Issue #17](https://github.com/rqtl/qtl2fst/issues/17).

- Fix some URLs in Readme, News, and vignette.

- Have `fst_genoprob()` and `genoprob_to_alleleprob_fst()` check that
  input is of class `"calc_genoprob"`.


## qtl2fst 0.24 (2021-04-28)

### Minor changes

- Fix index entry in vignette

- Added tests for `qtl2::calc_het()` (which previously wasn't working
  with qtl2fst-based probabilities). Also added tests of
  `qtl2::calc_entropy()`, `qtl2::calc_geno_freq()`, and `qtl2::compare_genoprob()`.
  These tests fail without qtl2 >= 0.24, so added that in Imports in
  the description file.

- Removed `LazyData` field from DESCRIPTION file


## qtl2fst 0.22-7 (2020-07-24)

### Minor changes

- Small changes to prepare to post on [CRAN](https://cran.r-project.org).


## qtl2fst 0.22 (2020-05-26)

### Major changes

- Added `calc_genoprob_fst()` which combines `calc_genoprob()` and
  `fst_genoprob()` to save use of RAM when calculating genotype
  probabilties.

- Added `genoprob_to_alleleprob_fst()` which combines
  `genoprob_to_alleleprob()` and `fst_genoprob()` to save use of RAM
  when converting genotype probabilties to allele dosages.

- Added `summary()` and `print()` functions for `"fst_genoprob"`
  objects.

- Rewrote the vignette using the iron dataset, so that it's faster to
  build.

### Minor changes

- Fixed use of `class()`, avoiding constructions like
  `"blah" %in% class(object)` and instead using
  `inherits(object, "blah")`.


## qtl2fst 0.20 (2019-06-03)

### Minor changes

- Renumbering to match R/qtl2 version

- Use Markdown for function documentation, throughout


## qtl2fst 0.2-2 (2018-07-23)

### Interface changes

- `fst_genoprob()`: added `quiet` argument, to replace `verbose` which
  we'll ultimately remove. Also added an argument `overwrite`; if
  `FALSE` (the default), refuse to overwrite the `.fst` files.

- Renamed `fst2calc_genoprob()` as `fst_extract()`. The function with
  the original name will remain for a while, but will give a warning.

- Renamed `fst_genoprob_restore()` as `fst_restore(). The function with
  the original name will remain for a while, but will give a warning.

### New features

- When `fst_genoprob()` creates the set of `.fst` files with the
  genotype probabilities, it also creates a `.rds` file with the index
  object. Load this object with `readRDS()`. `cbind.fst_genoprob()` and
  `rbind.fst_genoprob()` will write the index `.fst` file for the new
  object.

- Added `overwrite` and `quiet` arguments to `cbind.fst_genoprob()`
  and `rbind.fst_genoprob()`.

- Added a function `fst_path()` for viewing the path to the files behind
  an `"fst_genoprob"` database.

- Added a function `fst_files()` for viewing the set of files behind an
  `"fst_genoprob"` database.

- Added a function `replace_path()` for replacing the file path within a
  `"fst_genoprob"` object.


## qtl2fst 0.1-3 (2018-03-13)

- New package for storing [R/qtl2](https://kbroman.org/qtl2/) genotype
  probabilities in files using the [fst
  package](https://www.fstpackage.org), for fast access with reduced
  memory usage.
