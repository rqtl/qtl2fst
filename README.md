# qtl2fst

[![Build Status](https://travis-ci.org/rqtl/qtl2fst.svg?branch=master)](https://travis-ci.org/rqtl/qtl2fst)

[R/qtl2](http://kbroman.org/qtl2) package using the [fst](https://fstpackage.github.io) package.

R/qtl2fst is uses fst to store a genotype probabilitis in a set of
files for rapid access. It is fully integrated with
[R/qtl2](http://kbroman.org/qtl2) (aka qtl2). It is based on
[qtl2feather](https://github.com/byandell/qtl2feather).

---

### Installation

Make sure you have the latest version of [R (3.4.4)](https://cran.r-project.org).
Then install R/qtl2 using the following. (For more
detail, see the instructions at <http://kbroman.org/qtl2>.)

    install.packages("qtl2", repos="https://rqtl.org/qtl2cran")

Next install the [fst](https://fstpackage.github.io), and
[dplyr](http://dplyr.tidyverse.org/), and
[devtools](https://github.com/r-lib/devtools) packages.

    install.packages(c("fst", "dplyr", "devtools"))

Once you have installed these, install qtl2fst

    devtools::install_github("kbroman/qtl2fst")

To also install vignettes:

    devtools::install_github("kbroman/qtl2fst", build_vignettes = TRUE)

---

#### License

[Licensed](License.md) under [GPL-3](https://www.r-project.org/Licenses/GPL-3).
