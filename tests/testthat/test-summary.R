context("summary.fst_genoprob")

test_that("summary.fst_genoprob works", {

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

    aprobs_fst <- genoprob_to_alleleprob_fst(probs_fst, "iron_aprob", fst_dir, overwrite=TRUE)

    path_probs <- file.path(fst_dir, "iron")
    path_aprobs <- file.path(fst_dir, "iron_aprob")

    expect_equal(summary(probs_fst),
                 structure(list(path = path_probs, crosstype = "f2",
                                chr = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                        "11", "12", "13", "14", "15", "16", "17", "18", "19", "X"),
                                n_ind = 284L,
                                n_gen = c(`1` = 3L, `2` = 3L, `3` = 3L, `4` = 3L, `5` = 3L,
                                          `6` = 3L, `7` = 3L, `8` = 3L, `9` = 3L, `10` = 3L,
                                          `11` = 3L, `12` = 3L, `13` = 3L, `14` = 3L, `15` = 3L,
                                          `16` = 3L, `17` = 3L, `18` = 3L, `19` = 3L, X = 6L),
                                n_mar = c(`1` = 86L, `2` = 39L, `3` = 31L, `4` = 44L, `5` = 46L,
                                          `6` = 27L, `7` = 58L, `8` = 83L, `9` = 58L, `10` = 35L,
                                          `11` = 63L, `12` = 40L, `13` = 24L, `14` = 34L, `15` = 34L,
                                          `16` = 48L, `17` = 37L, `18` = 28L, `19` = 36L, X = 30L),
                                alleleprobs = FALSE), class = c("summary.fst_genoprob", "list"))
                 )

    expect_equal(summary(aprobs_fst),
                 structure(list(path = path_aprobs, crosstype = "f2",
                                chr = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                        "11", "12", "13", "14", "15", "16", "17", "18", "19", "X"),
                                n_ind = 284L,
                                n_gen = c(`1` = 2L, `2` = 2L, `3` = 2L, `4` = 2L, `5` = 2L,
                                          `6` = 2L, `7` = 2L, `8` = 2L, `9` = 2L, `10` = 2L,
                                          `11` = 2L, `12` = 2L, `13` = 2L, `14` = 2L, `15` = 2L,
                                          `16` = 2L, `17` = 2L, `18` = 2L, `19` = 2L, X = 2L),
                                n_mar = c(`1` = 86L, `2` = 39L, `3` = 31L, `4` = 44L, `5` = 46L,
                                          `6` = 27L, `7` = 58L, `8` = 83L, `9` = 58L, `10` = 35L,
                                          `11` = 63L, `12` = 40L, `13` = 24L, `14` = 34L, `15` = 34L,
                                          `16` = 48L, `17` = 37L, `18` = 28L, `19` = 36L, X = 30L),
                                alleleprobs = TRUE), class = c("summary.fst_genoprob", "list"))
                 )


    # clean up
    unlink(fst_dir, recursive=TRUE)
})
