context("calc_genoprob_fst")

test_that("calc_genoprob_fst works", {

    library(qtl2)

    # load data and create pseudomarker map
    grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
    gmap_w_pmar <- insert_pseudomarkers(grav2$gmap, step=1)

    # create directory to hold results
    fst_dir <- file.path(tempdir(), "grav2_genoprob")
    while(dir.exists(fst_dir)) {
        fst_dir <- paste0(fst_dir, sample(LETTERS, 1))
    }
    dir.create(fst_dir)

    probs_fst <- calc_genoprob_fst(grav2, "grav2", fst_dir, gmap_w_pmar, error_prob=0.002,
                                   overwrite=TRUE)

    probs <- calc_genoprob(grav2, gmap_w_pmar, error_prob=0.002)

    expect_equal(probs, fst_extract(probs_fst))

    # clean up
    unlink(fst_dir, recursive=TRUE)
})
