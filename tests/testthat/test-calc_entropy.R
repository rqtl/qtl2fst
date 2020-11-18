context("calc_entropy")

test_that("calc_entropy works", {

    library(qtl2)

    # load data and create pseudomarker map
    iron <- read_cross2(system.file("extdata", "iron.zip", package="qtl2"))
    gmap_w_pmar <- insert_pseudomarkers(iron$gmap, step=1)

    # create directory to hold results
    fst_dir <- file.path(tempdir(), "iron_genoprob")
    while(dir.exists(fst_dir)) {
        fst_dir <- paste0(fst_dir, sample(LETTERS, 1))
    }
    dir.create(fst_dir)

    probs_fst <- calc_genoprob_fst(iron, "iron", fst_dir, gmap_w_pmar, error_prob=0.002,
                                   overwrite=TRUE)

    probs <- fst_extract(probs_fst)

    expect_equal(calc_entropy(probs_fst), calc_entropy(probs))

    # clean up
    unlink(fst_dir, recursive=TRUE)
})
