#' Join genotype probabilities for different chromosomes
#'
#' Join multiple genotype probability objects, as produced by
#' \code{\link{fst_genoprob}} for different individuals.
#'
#' @param ... Genotype probability objects as produced by
#' \code{\link{fst_genoprob}}. Must have the same set of individuals.
#' @param fbase Base of fileame for fst database.
#' Needed if objects have different fst databases.
#' @param fdir Directory for fst database.
#'
#' @return A single genotype probability object.
#'
#' @examples
#' library(qtl2)
#' grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
#' map <- insert_pseudomarkers(grav2$gmap, step=1)
#' probsA <- calc_genoprob(grav2[1:5,1:2], map, error_prob=0.002)
#' probsB <- calc_genoprob(grav2[1:5,3:4], map, error_prob=0.002)
#' dir <- tempdir()
#' fprobsA <- fst_genoprob(probsA, "exampleAc", dir)
#' fprobsB <- fst_genoprob(probsB, "exampleBc", dir)
#' fprobs <- cbind(fprobsA, fprobsB, fbase = "exampleABc")
#'
#' @export
#' @export cbind.fst_genoprob
#' @method cbind fst_genoprob
#'
cbind.fst_genoprob <- function(..., fbase, fdir = NULL) {
    # to cbind: probs, is_x_chr
    # to pass through (must match): crosstype, alleles, alleleprobs

    bind_fst(list(...),
             check_cbind,
             append_chr,
             cbind,
             fbase, fdir)
}

# FIX_ME: add explanation
bind_fst <- function(args, check_fn, append_fn, bind_fn,
                     fbase, fdir = NULL) {

    result <- args[[1]]
    if(!inherits(result, "fst_genoprob"))
        stop("argument ", 1, "is not of class fst_genoprob")
    if(length(args) == 1)
        return(result)

    attrs <- attributes(result)
    result <- unclass(result)
    if(is.null(fdir))
        fdir <- dirname(result$fst)
    if(!dir.exists(fdir))
        stop("directory", fdir, "does not exist")

    check_fn(args)

    # paste stuff together
    diff_fst <- 0
    for(i in 2:length(args)) {
        # This requires some care, as need to combine fsts
        diff_fst <- (result$fst != unclass(args[[i]])$fst)
        if(diff_fst) {
            diff_fst <- i
            break
        }
        out <- append_fn(result, i, args[[i]], attrs)
        result <- out$result
        attrs <- out$attrs
    }

    if(diff_fst == 2) {
        # Result is just first argument.
        result <- args[[1]]
    } else {
        # Result has cbind of at least on other argument.
        # Set up attributes.
        ignore <- match(c("names","class"), names(attrs))
        for(a in names(attrs)[-ignore])
            attr(result, a) <- attrs[[a]]

        class(result) <- attrs$class
    }

    # Done, unless some args have different fst files.
    if(!diff_fst)
        return(result)

    # Different fsts. Need to convert to calc_genoprob and back again.
    if(missing(fbase))
        stop("need to supply fbase to bind distinct fst_genoprob objects")

    result <- fst2calc_genoprob(result)

    # Convert rest to calc_genoprob and append in usual way.
    for(i in seq(diff_fst, length(args))) {
        argsi <- fst2calc_genoprob(args[[i]])
        result <- bind_fn(result, argsi)
    }

    fst_genoprob(result, fbase, fdir)
}

# FIX_ME: add explanation
check_cbind <- function(args) {
    result <- unclass(args[[1]])

    for(i in 2:length(args)) {
        if(!inherits(args[[i]], "fst_genoprob"))
            stop("Input object ", i, "is not of class fst_genoprob")

        argsi <- unclass(args[[i]])
        if(length(result$ind) != length(argsi$ind) ||
           !all(result$ind == argsi$ind))
            stop("Input objects 1 and ", i, " have different individuals")
    }
}

# FIX_ME: add explanation
append_chr <- function(result, i, argsi, attrs) {
    is_x_chr <- attr(argsi, "is_x_chr")
    argsi <- unclass(argsi)

    # Identify what chromosomes are new (if any).
    new_chr <- is.na(match(argsi$chr, result$chr))
    if(!any(new_chr))
        stop("Input object ", i, "has no new chromosomes")
    if(any(!new_chr))
        warning("duplicate chr ",
                paste(argsi$chr[!new_chr], collapse = ","),
                " in input object ", i, " ignored")
    new_chr <- argsi$chr[new_chr]

    # Identify new markers.
    marnamesi <- unlist(lapply(argsi$dimnames[new_chr], function(x) x[[3]]))
    new_mar <- argsi$mar[argsi$mar %in% marnamesi]

    # Append new components.
    result$chr <- c(result$chr, new_chr)
    result$mar <- c(result$mar, new_mar)
    result$dim <- cbind(result$dim, argsi$dim[, new_chr, drop = FALSE])
    result$dimnames <- c(result$dimnames, argsi$dimnames[new_chr])
    attrs$is_x_chr <- c(attrs$is_x_chr, is_x_chr[new_chr])

    list(result = result, attrs = attrs)
}
