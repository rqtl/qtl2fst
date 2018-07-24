# fst_restore
#' Restore fst_genoprob object to original dimensions.
#'
#' Any `"fst_genoprob"` object has embedded its original data and dimensions.
#' This resets elements `ind`, `chr` and `mar` to the full set.
#'
#' @md
#'
#' @param object Object of class `"fst_genoprob"` as produced by [fst_genoprob()].
#'
#' @return Input `object` with dimensions restored.
#'
#' @details
#' Object is unclassed and elements `ind`, `chr` and `mar` are changed before
#' reseting attributes as `"fst_genoprob"` object.
#' See [fst_genoprob()] for details on the object.
#'
#' @export
#' @keywords utilities
#'
#' @seealso [fst_genoprob()], [fst_extract()]
#'
#' @examples
#' library(qtl2)
#' grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
#' map <- insert_pseudomarkers(grav2$gmap, step=1)
#' probs <- calc_genoprob(grav2, map, error_prob=0.002)
#' dir <- tempdir()
#' fprobs <- fst_genoprob(probs, "grav2", dir, overwrite=TRUE)
#' dim(fprobs)
#' fprobs2 <- subset(fprobs, chr=1:2)
#' dim(fprobs2)
#' fprobs5 <- fst_restore(fprobs2)
#' dim(fprobs5)
#' \dontshow{unlink( fst_files(fprobs) )}
#'
fst_restore <- function(object) {
    if(!inherits(object, "fst_genoprob"))
        stop("object must inherit class fst_genoprob")

    attrs <- attributes(object)
    result <- unclass(object)

    result$chr <- names(result$dimnames)
    result$ind <- result$dimnames[[1]][[1]]
    tmp <- unlist(lapply(result$dimnames, function(x) x[[3]]))
    names(tmp) <- NULL
    result$mar <- tmp

    # Set up attributes.
    ignore <- match(c("names","class"), names(attrs))
    for(a in names(attrs)[-ignore])
        attr(result, a) <- attrs[[a]]
    attr(result, "is_x_chr") <- result$is_x_chr

    class(result) <- attrs$class

    result
}

#' @export
#' @describeIn fst_restore Deprecated version (to be removed).
fst_genoprob_restore <-
    function(object)
{
    warning("fst_genoprob_restore() is deprecated and will be removed; use fst_restore() instead.")

    fst_restore(object)
}
