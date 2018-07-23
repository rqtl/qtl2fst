# qtl2fst

[![Build Status](https://travis-ci.org/rqtl/qtl2fst.svg?branch=master)](https://travis-ci.org/rqtl/qtl2fst)

[R/qtl2](https://kbroman.org/qtl2) (aka qtl2) is a reimplementation of
the QTL analysis software [R/qtl](http://rqtl.org), to better handle
high-dimensional data and complex cross designs.

The [qtl2fst](https://github.com/rqtl/qtl2fst) package uses
[fst](http://www.fstpackage.org/) to store a genotype probabilities in
a set of files for rapid access but reduced memory usage. It is fully
integrated with [R/qtl2](https://kbroman.org/qtl2), and is based on the
[qtl2feather](https://github.com/byandell/qtl2feather) package which stores
files in the [feather](https://github.com/wesm/feather) format.

---

### Installation

Make sure you have the latest version of [R](https://cran.r-project.org).
Then install R/qtl2 using the following. (For more
detail, see the instructions at <https://kbroman.org/qtl2>.)

    install.packages("qtl2", repos="http://rqtl.org/qtl2cran")

Next install the [fst](https://fstpackage.github.io),
[dplyr](http://dplyr.tidyverse.org/), and
[devtools](https://github.com/r-lib/devtools) packages.

    install.packages(c("fst", "dplyr", "devtools"))

Once you have installed these, install qtl2fst from GitHub:

    devtools::install_github("kbroman/qtl2fst")

To also install vignettes, include `build_vignettes=TRUE`:

    devtools::install_github("kbroman/qtl2fst", build_vignettes = TRUE)

---

#### License

[Licensed](LICENSE) under [GPL-3](https://www.r-project.org/Licenses/GPL-3).
