# qtl2fst

[![Build Status](https://travis-ci.org/byandell/qtl2fst.svg?branch=master)](https://travis-ci.org/kbroman/qtl2fst)

[R/qtl2](http://kbroman.org/qtl2) package using the [fst](https://fstpackage.github.io) package.

R/qtl2fst is uses fst to store a genotype probabilitis in a set of
files for rapid access. It is fully integrated with
[R/qtl2](http://kbroman.org/qtl2) (aka qtl2). It is based on
[qtl2feather](https://github.com/byandell/qtl2feather).

---

### Installation

R/qtl2 is early in development and so is not yet available on
[CRAN](https://cran.r-project.org).

You can install R/qtl2 from [GitHub](https://github.com/rqtl).

You first need to install the [fst](https://fstpackage.github.io) and
[dplyr](http://dplyr.tidyverse.org/) packages.

    install.packages(c("fst", "dplyr"))

Once you have installed these, install qtl2fst as

    install_github("kbroman/qtl2fst")

To install vignettes:

    install_github("kbroman/qtl2fst", build_vignettes = TRUE)

---

#### License

[Licensed](License.md) under [GPL-3](https://www.r-project.org/Licenses/GPL-3).
