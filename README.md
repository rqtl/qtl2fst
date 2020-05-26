# qtl2fst

[![Build Status](https://travis-ci.org/rqtl/qtl2fst.svg?branch=master)](https://travis-ci.org/rqtl/qtl2fst)

[R/qtl2](https://kbroman.org/qtl2) (aka qtl2) is a reimplementation of
the QTL analysis software [R/qtl](https://rqtl.org), to better handle
high-dimensional data and complex cross designs.

The [qtl2fst](https://github.com/rqtl/qtl2fst) package uses
the [fst package](https://www.fstpackage.org/) to store genotype probabilities in
a set of files for rapid access but reduced memory usage. It is fully
integrated with [R/qtl2](https://kbroman.org/qtl2), and is modeled after the
[qtl2feather](https://github.com/byandell/qtl2feather) package which stores
files in the [feather](https://github.com/wesm/feather) format.

---

### Installation

Make sure you have the latest version of [R](https://cran.r-project.org).
Then install R/qtl2fst using the following:

    install.packages("qtl2fst", repos="https://rqtl.org/qtl2cran")

You can also install the package from GitHub using
[devtools](https://github.com/r-lib/devtools). You will first need to
install [R/qtl2](https://kbroman.org/qtl2) and
[fst](https://fstpackage.github.io). Then install qtl2fst as follows:

    devtools::install_github("rqtl/qtl2fst")

To also install vignettes, include `build_vignettes=TRUE`:

    devtools::install_github("rqtl/qtl2fst", build_vignettes = TRUE)

---

#### License

Licensed under [GPL-3](https://www.r-project.org/Licenses/GPL-3).
