#' Join genotype probabilities for different individuals
#'
#' Join multiple genotype probability objects, as produced by
#' [fst_genoprob()] for different individuals.
#'
#' @md
#'
#' @param ... Genotype probability objects as produced by
#' [fst_genoprob()]. Must have the same set of markers and
#' genotypes.
#' @param fbase Base of fileame for fst database.
#' Needed if objects have different fst databases.
#' @param fdir Directory for fst database.
#' @param overwrite If FALSE (the default), refuse to overwrite existing `.fst` files
#' @param quiet If TRUE, don't show any messages. Passed to [fst_genoprob()].
#'
#' @return A single genotype probability object.
#'
#' @examples
#' library(qtl2)
#' grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
#' map <- insert_pseudomarkers(grav2$gmap, step=1)
#' probsA <- calc_genoprob(grav2[1:5,], map, error_prob=0.002)
#' probsB <- calc_genoprob(grav2[6:12,], map, error_prob=0.002)
#' dir <- tempdir()
#' fprobsA <- fst_genoprob(probsA, "exampleAr", dir, overwrite=TRUE)
#' fprobsB <- fst_genoprob(probsB, "exampleBr", dir, overwrite=TRUE)
#' fprobs <- rbind(fprobsA, fprobsB, fbase = "exampleABr")
#'
#' @export
#' @export rbind.fst_genoprob
#' @method rbind fst_genoprob
#' @seealso [cbind.fst_genoprob()]

rbind.fst_genoprob <-
    function(..., fbase=NULL, fdir = NULL, overwrite=FALSE, quiet=FALSE)
{
    # to rbind: the data
    # to pass through (must match): crosstype, is_x_chr, alleles, alleleprobs

    args <- list(...)

    bind_fst(list(...),
             check_rbind,
             append_ind,
             rbind,
             fbase=fbase, fdir=fdir,
             overwrite=overwrite, quiet=quiet)
}

# FIX_ME: add explanation
check_rbind <- function(args) {
    result <- args[[1]]
    # check that things match
    other_stuff <- c("crosstype", "is_x_chr", "alleles", "alleleprobs")
    for(i in 2:length(args)) {
        if(!inherits(args[[i]], "fst_genoprob"))
            stop("argument ", i, "is not of class fst_genoprob")
        for(obj in other_stuff) {
            if(!identical(attr(result, obj), attr(args[[i]], obj)))
                stop("Input objects 1 and ", i, " differ in their ", obj)
        }
        if(!identical(names(result), names(args[[i]])))
            stop("Input objects 1 and ", i, " have different chromosome names")
    }
}

# FIX_ME: add explanation
append_ind <- function(result, i, argsi, attrs) {
    argsi <- unclass(argsi)

    new_ind <- is.na(match(argsi$ind, result$ind))
    if(!any(new_ind))
        stop("argument ", i, "has no new individuals")
    if(any(!new_ind))
        warning("duplicate ind ",
                paste(argsi$ind[!new_ind], collapse = ","),
                " in input object ", i, " ignored")

    # Append new individuals.
    new_ind <- argsi$ind[new_ind]
    result$ind <- c(result$ind, new_ind)

    list(result = result, attrs = attrs)
}
