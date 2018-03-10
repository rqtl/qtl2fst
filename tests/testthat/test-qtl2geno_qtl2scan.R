context("qtl2")

test_that("fst_genoprob works with qtl2 functions", {

    library(qtl2)
    iron <- read_cross2(system.file("extdata", "iron.zip", package="qtl2"))
    iron <- iron[,c(18,19,"X")]

    map <- insert_pseudomarkers(iron$gmap, step=1)
    probs <- calc_genoprob(iron, map, error_prob=0.002)

    dir <- tempdir()
    fprobs <- fst_genoprob(probs, "iron_probs", dir)

    # get the same thing when subsetting
    expect_equal(fprobs[["18"]], probs[["18"]])
    expect_equal(fprobs[[1]], probs[["18"]])
    expect_equal(fprobs[["X"]], probs[["X"]])
    expect_equal(fprobs[[3]], probs[["X"]])

    # calc_kinship
    expect_equal(calc_kinship(probs), calc_kinship(fprobs))
    expect_equal(calc_kinship(probs, "loco"), calc_kinship(fprobs, "loco"))
    expect_equal(calc_kinship(probs, "chr"), calc_kinship(fprobs, "chr"))

    # genoprob_to_alleleprob
    expect_equal(genoprob_to_alleleprob(probs), genoprob_to_alleleprob(fprobs))

    # probs_to_grid
    grid <- calc_grid(iron$gmap, step=1)
    expect_equal(probs_to_grid(probs, grid), probs_to_grid(fprobs, grid))

    # maxmarg (impute genotypes giving marginal probabilities)
    seed <- 47220527
    ### whole genome
    set.seed(seed)
    imp <- maxmarg(probs)
    set.seed(seed)
    fimp <- maxmarg(fprobs)
    expect_equal(imp, fimp)

    ### single autosomal position
    set.seed(seed)
    imp <- maxmarg(probs, map, chr="19", pos=10.3)
    set.seed(seed)
    fimp <- maxmarg(fprobs, map, chr="19", pos=10.3)
    expect_equal(imp, fimp)
    ### return character strings
    set.seed(seed)
    imp <- maxmarg(probs, map, chr="19", pos=10.3, return_char=TRUE)
    set.seed(seed)
    fimp <- maxmarg(fprobs, map, chr="19", pos=10.3, return_char=TRUE)
    expect_equal(imp, fimp)

    ### single X chr position
    set.seed(seed)
    imp <- maxmarg(probs, map, chr="X", pos=57.9)
    set.seed(seed)
    fimp <- maxmarg(fprobs, map, chr="X", pos=57.9)
    expect_equal(imp, fimp)
    ### return character strings
    set.seed(seed)
    imp <- maxmarg(probs, map, chr="X", pos=57.9, return_char=TRUE)
    set.seed(seed)
    fimp <- maxmarg(fprobs, map, chr="X", pos=57.9, return_char=TRUE)
    expect_equal(imp, fimp)

    ### single chromosome
    set.seed(seed)
    imp <- maxmarg(probs[19])
    set.seed(seed)
    fimp <- maxmarg(fprobs[19])
    expect_equal(imp, fimp)

    ### single X chr position
    set.seed(seed)
    imp <- maxmarg(probs[,"X"])
    set.seed(seed)
    fimp <- maxmarg(fprobs[,"X"])
    expect_equal(imp, fimp)


    # scan1
    Xcovar <- get_x_covar(iron)
    sex <- Xcovar[,"sex",drop=FALSE]
    expect_equal(scan1(probs, iron$pheno, Xcovar=Xcovar),
                 scan1(fprobs, iron$pheno, Xcovar=Xcovar))
    expect_equal(scan1(probs, iron$pheno, Xcovar=Xcovar, addcovar=sex),
                 scan1(fprobs, iron$pheno, Xcovar=Xcovar, addcovar=sex))
    expect_equal(scan1(probs, iron$pheno, Xcovar=Xcovar, addcovar=sex, intcovar=sex),
                 scan1(fprobs, iron$pheno, Xcovar=Xcovar, addcovar=sex, intcovar=sex))

    # scan1 with kinship
    k <- calc_kinship(probs)
    fk <- calc_kinship(fprobs)
    expect_equal(scan1(probs, iron$pheno, k, Xcovar=Xcovar),
                 scan1(fprobs, iron$pheno, fk, Xcovar=Xcovar))
    expect_equal(scan1(probs, iron$pheno, k, Xcovar=Xcovar, addcovar=sex),
                 scan1(fprobs, iron$pheno, fk, Xcovar=Xcovar, addcovar=sex))
    expect_equal(scan1(probs, iron$pheno, k, Xcovar=Xcovar, addcovar=sex, intcovar=sex),
                 scan1(fprobs, iron$pheno, fk, Xcovar=Xcovar, addcovar=sex, intcovar=sex))

    # scan1 with "loco" kinship
    k <- calc_kinship(probs, "loco")
    fk <- calc_kinship(fprobs, "loco")
    expect_equal(scan1(probs, iron$pheno, k, Xcovar=Xcovar),
                 scan1(fprobs, iron$pheno, fk, Xcovar=Xcovar))
    expect_equal(scan1(probs, iron$pheno, k, Xcovar=Xcovar, addcovar=sex),
                 scan1(fprobs, iron$pheno, fk, Xcovar=Xcovar, addcovar=sex))
    expect_equal(scan1(probs, iron$pheno, k, Xcovar=Xcovar, addcovar=sex, intcovar=sex),
                 scan1(fprobs, iron$pheno, fk, Xcovar=Xcovar, addcovar=sex, intcovar=sex))


    # scan1coef
    phe <- iron$pheno[,1,drop=FALSE]
    expect_equal(scan1coef(subset(probs, chr="18"), phe, addcovar=sex),
                 scan1coef(subset(fprobs, chr="18"), phe, addcovar=sex))
    expect_equal(scan1coef(subset(probs, chr="X"), phe, addcovar=Xcovar),
                 scan1coef(subset(fprobs, chr="X"), phe, addcovar=Xcovar))

    # scan1coef with kinship
    phe <- iron$pheno[,1,drop=FALSE]
    expect_equal(scan1coef(subset(probs, chr="18"), phe, k[["18"]], addcovar=sex),
                 scan1coef(subset(fprobs, chr="18"), phe, fk[["18"]], addcovar=sex))
    expect_equal(scan1coef(subset(probs, chr="X"), phe, k[["X"]], addcovar=Xcovar),
                 scan1coef(subset(fprobs, chr="X"), phe, fk[["X"]], addcovar=Xcovar))

    # scan1blup
    expect_equal(scan1blup(subset(probs, chr="18"), phe, addcovar=sex),
                 scan1blup(subset(fprobs, chr="18"), phe, addcovar=sex))
    expect_equal(scan1blup(subset(probs, chr="X"), phe, addcovar=Xcovar),
                 scan1blup(subset(fprobs, chr="X"), phe, addcovar=Xcovar))

    # scan1blup with kinship
    expect_equal(scan1blup(subset(probs, chr="18"), phe, k[["18"]], addcovar=sex),
                 scan1blup(subset(fprobs, chr="18"), phe, fk[["18"]], addcovar=sex))
    expect_equal(scan1blup(subset(probs, chr="X"), phe, k[["X"]], addcovar=Xcovar),
                 scan1blup(subset(fprobs, chr="X"), phe, fk[["X"]], addcovar=Xcovar))

    # scan1perm
    n_perm <- 3
    seed <- 65418959
    set.seed(seed)
    operm <- scan1perm(probs, phe, n_perm=n_perm)
    set.seed(seed)
    foperm <- scan1perm(fprobs, phe, n_perm=n_perm)
    expect_equal(operm, foperm)

    # scan1perm, 2 phenotypes, additive covariate
    set.seed(seed)
    operm <- scan1perm(probs, iron$pheno, n_perm=n_perm, addcovar=sex)
    set.seed(seed)
    foperm <- scan1perm(fprobs, iron$pheno, n_perm=n_perm, addcovar=sex)
    expect_equal(operm, foperm)

    # scan1perm, X-chr-specific
    set.seed(seed)
    operm <- scan1perm(probs, iron$pheno, n_perm=n_perm, addcovar=sex, Xcovar=Xcovar,
                       perm_Xsp=TRUE, chr_lengths=chr_lengths(map))
    set.seed(seed)
    foperm <- scan1perm(fprobs, iron$pheno, n_perm=n_perm, addcovar=sex, Xcovar=Xcovar,
                       perm_Xsp=TRUE, chr_lengths=chr_lengths(map))
    expect_equal(operm, foperm)

    # scan1perm, 2 phenotypes, additive covariate and kinship
    set.seed(seed)
    operm <- scan1perm(probs, iron$pheno, k, n_perm=n_perm, addcovar=sex)
    set.seed(seed)
    foperm <- scan1perm(fprobs, iron$pheno, fk, n_perm=n_perm, addcovar=sex)
    expect_equal(operm, foperm)


    # scan1perm, 2 phenotypes, additive covariate and kinship, and X-chr-sp
    set.seed(seed)
    operm <- scan1perm(probs, iron$pheno, k, n_perm=n_perm, addcovar=sex,
                       perm_Xsp=TRUE, chr_lengths=chr_lengths(map))
    set.seed(seed)
    foperm <- scan1perm(fprobs, iron$pheno, fk, n_perm=n_perm, addcovar=sex,
                       perm_Xsp=TRUE, chr_lengths=chr_lengths(map))
    expect_equal(operm, foperm)

    # clean up
    lf <- list.files(dir, pattern=".fst")
    unlink(file.path(dir, lf))

})
