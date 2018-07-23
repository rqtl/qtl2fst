## qtl2fst 0.2-1 (2018-07-23)

### Interface changes

- Rename `fst_genoprob()` as `probs2fsts()`.
- Rename `fst2calc_genoprob()` as `fst2probs()`.

### New features

- When `probs2fst()` creates the set of `.fst` files with the genotype
  probabilities, it also creates a `.rds` file with the index object.
  Load this object with `readRDS()`.

- Added a function `remove_fst()` for removing all of the files behind
  an `"fst_genoprob"` database.

- Added a function `fst_path()` for viewing the path to the files behind
  an `"fst_genoprob"` database.

- Added a function `fst_files()` for viewing the set of files behind an
  `"fst_genoprob"` database.

- Added a function `replace_path()` for replacing the file path within a
  `"fst_genoprob"` object.
