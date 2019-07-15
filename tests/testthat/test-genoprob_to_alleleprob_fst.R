context("genoprob_to_alleleprob_fst")

test_that("genoprob_to_alleleprob_fst works", {

    library(qtl2)

    # load data and create pseudomarker map
    iron <- read_cross2(system.file("extdata", "iron.zip", package="qtl2"))
    gmap_w_pmar <- insert_pseudomarkers(iron$gmap, step=1)

    probs <- calc_genoprob(iron, error_prob=0.002)

    # create directory to hold results
    fst_dir <- file.path(tempdir(), "iron_alleleprob")
    while(dir.exists(fst_dir)) {
        fst_dir <- paste0(fst_dir, sample(LETTERS, 1))
    }
    dir.create(fst_dir)

    apr_fst <- genoprob_to_alleleprob_fst(probs, "iron", fst_dir, overwrite=TRUE)

    # compare to direct calculation
    apr <- genoprob_to_alleleprob(probs)
    expect_equal(apr, fst_extract(apr_fst) )

    # clean up
    unlink(fst_dir, recursive=TRUE)
})
