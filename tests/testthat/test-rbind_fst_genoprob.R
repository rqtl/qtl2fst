context("rbind_fst_genoprob")

test_that("rbind_fst_genoprob works", {

    library(qtl2)
    grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
    map <- insert_pseudomarkers(grav2$gmap, step=1)
    probsA <- calc_genoprob(grav2[1:5,], map, error_prob=0.002)
    probsB <- calc_genoprob(grav2[6:12,], map, error_prob=0.002)
    dir <- tempdir()
    fprobsA <- fst_genoprob(probsA, "exampleAr", dir)
    fprobsB <- fst_genoprob(probsB, "exampleBr", dir)
    fprobs <- rbind(fprobsA, fprobsB, fbase = "exampleABr")

    expect_equal(rbind(probsA, probsB), fst_extract(fprobs))

    unlink( fst_files(fprobsA) )
    unlink( fst_files(fprobsB) )
    unlink( fst_files(fprobs)  )

})
