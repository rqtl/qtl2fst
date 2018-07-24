context("cbind_fst_genoprob")

test_that("cbind_fst_genoprob works in RIL", {

    library(qtl2)
    grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
    map <- insert_pseudomarkers(grav2$gmap, step=1)
    probsA <- calc_genoprob(grav2[1:5,1:2], map, error_prob=0.002)
    probsB <- calc_genoprob(grav2[1:5,3:4], map, error_prob=0.002)
    dir <- tempdir()
    fprobsA <- fst_genoprob(probsA, "exampleAc", dir)
    fprobsB <- fst_genoprob(probsB, "exampleBc", dir)
    fprobs <- cbind(fprobsA, fprobsB, fbase = "exampleABc")

    expect_equal(cbind(probsA, probsB), fst_extract(fprobs))

    unlink( fst_files(fprobsA) )
    unlink( fst_files(fprobsB) )
    unlink( fst_files(fprobs)  )

})

test_that("cbind_fst_genoprob works in an intercross", {

    library(qtl2)
    iron <- read_cross2(system.file("extdata", "iron.zip", package="qtl2"))
    map <- insert_pseudomarkers(iron$gmap, step=1)
    probsA <- calc_genoprob(iron[7:12,c("3","X")], map, error_prob=0.002)
    probsB <- calc_genoprob(iron[7:12,c("5","7")], map, error_prob=0.002)
    dir <- tempdir()
    fprobsA <- fst_genoprob(probsA, "exampleAc", dir)
    fprobsB <- fst_genoprob(probsB, "exampleBc", dir)
    fprobs <- cbind(fprobsA, fprobsB, fbase = "exampleABc")

    expect_equal(cbind(probsA, probsB), fst_extract(fprobs))

    # check index file
    expect_equal(fprobs, readRDS(fst_files(fprobs)[1]))

    # go ahead and check the two pieces and their index files
    expect_equal(probsA, fst_extract(fprobsA))
    expect_equal(fprobsA, readRDS(fst_files(fprobsA)[1]))
    expect_equal(probsB, fst_extract(fprobsB))
    expect_equal(fprobsB, readRDS(fst_files(fprobsB)[1]))

    unlink( fst_files(fprobsA) )
    unlink( fst_files(fprobsB) )
    unlink( fst_files(fprobs)  )

})
