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

- Added a function `fst_path()` for viewing the path to the files behind
  an `"fst_genoprob"` database.

- Added a function `fst_files()` for viewing the set of files behind an
  `"fst_genoprob"` database.

- Added a function `replace_path()` for replacing the file path within a
  `"fst_genoprob"` object.


## qtl2fst 0.1-3 (2018-03-13)

- New package for storing [R/qtl2](https://kbroman.org/qtl2) genotype
  probabilities in files using the [fst
  package](http://www.fstpackage.org), for fast access with reduced
  memory usage.
