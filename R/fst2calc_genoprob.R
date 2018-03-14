# fst_genoprob
#' Store genotype probabilities in fst database
#'
#' Uses package fst to convert R object created in R/qtl2 for fast access.
#'
#' @md
#'
#' @param object Object of class `"fst_genoprob"`, linking to an fst database of genotype probabilities.
#'
#' @return An object of class `"calc_genoprob"` (a list of 3-dimensional arrays).
#'
#' @details
#' The genotype probabilities are extracted from fst databases. Each chromosome is extracted in turn.
#'
#' @export
#' @keywords utilities
#'
#' @examples
#' library(qtl2)
#' grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
#' map <- insert_pseudomarkers(grav2$gmap, step=1)
#' probs <- calc_genoprob(grav2, map, error_prob=0.002)
#' dir <- tempdir()
#' fprobs <- fst_genoprob(probs, "grav2", dir)
#' nprobs <- fst2calc_genoprob(fprobs)
#'
fst2calc_genoprob <- function(object) {
    if(!inherits(object, "fst_genoprob"))
        stop("object must inherit class fst_genoprob")

    attrs <- attributes(object)

    chr <- unclass(object)$chr
    result <- vector(mode = "list", length = length(chr))
    names(result) <- chr
    for(chri in chr)
        result[[chri]] <- object[[chri]]

    # Set up attributes.
    ignore <- match(c("names","class"), names(attrs))
    for(a in names(attrs)[-ignore])
        attr(result, a) <- attrs[[a]]

    class(result) <- attrs$class[-1]

    result
}
