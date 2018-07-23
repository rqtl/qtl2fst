context("pull_genoprobpos")

test_that("qtl2::pull_genoprobpos works with qtl2fst", {

    library(qtl2)
    iron <- read_cross2(system.file("extdata", "iron.zip", package="qtl2"))
    iron <- iron[,c(18,19,"X")]

    map <- insert_pseudomarkers(iron$gmap, step=1)
    probs <- calc_genoprob(iron, map, error_prob=0.002)

    dir <- tempdir()
    fprobs <- fst_genoprob(probs, "iron_probs", dir)

    marker <- find_marker(map, 18, 30.4)
    pr_18_30 <- pull_genoprobpos(probs, marker)
    fpr_18_30 <- pull_genoprobpos(fprobs, marker)

    expect_equal(pr_18_30, fpr_18_30)

    # clean up: remove all of the files
    unlink( fst_files(fprobs) )

})
