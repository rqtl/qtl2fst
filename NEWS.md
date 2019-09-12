## qtl2fst 0.21-4 (2019-07-15)

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

- New package for storing [R/qtl2](https://kbroman.org/qtl2) genotype
  probabilities in files using the [fst
  package](https://www.fstpackage.org), for fast access with reduced
  memory usage.
