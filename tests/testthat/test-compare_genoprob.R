context("compare_genoprob")

test_that("compare_genoprob works", {

    library(qtl2)

    # load data and create pseudomarker map
    iron <- read_cross2(system.file("extdata", "iron.zip", package="qtl2"))
    gmap_w_pmar <- insert_pseudomarkers(iron$gmap, step=1)

    # create directory to hold results
    fst_dir1 <- file.path(tempdir(), "iron_genoprob1")
    while(dir.exists(fst_dir1)) {
        fst_dir1 <- paste0(fst_dir1, sample(LETTERS, 1))
    }
    dir.create(fst_dir1)

    fst_dir2 <- file.path(tempdir(), "iron_genoprob2")
    while(dir.exists(fst_dir2)) {
        fst_dir2 <- paste0(fst_dir2, sample(LETTERS, 1))
    }
    dir.create(fst_dir2)

    probs_fst_1 <- calc_genoprob_fst(iron, "iron", fst_dir1, gmap_w_pmar, error_prob=0.002,
                                     overwrite=TRUE)

    probs_fst_2 <- calc_genoprob_fst(iron, "iron", fst_dir2, gmap_w_pmar, error_prob=0.2,
                                     overwrite=TRUE)

    probs_1 <- fst_extract(probs_fst_1)
    probs_2 <- fst_extract(probs_fst_2)
    result <- compare_genoprob(probs_1, probs_2, iron, ind=10, chr=2, minmarkers=1, minprob=0.5)

    expect_equal(compare_genoprob(probs_fst_1, probs_fst_2, iron, ind=10, chr=2, minmarkers=1, minprob=0.5), result)
    expect_equal(compare_genoprob(probs_1, probs_fst_2, iron, ind=10, chr=2, minmarkers=1, minprob=0.5), result)
    expect_equal(compare_genoprob(probs_fst_1, probs_2, iron, ind=10, chr=2, minmarkers=1, minprob=0.5), result)

    # clean up
    unlink(fst_dir1, recursive=TRUE)
    unlink(fst_dir2, recursive=TRUE)
})
