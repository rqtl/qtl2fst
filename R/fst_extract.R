# fst_extract (previously fst2calc_genoprob)
#' Extract genotype probabilities from fst database
#'
#' Extract genotype probabilities from fst database as an ordinary calc_genoprob object.
#'
#' @param object Object of class `"fst_genoprob"`, linking to an fst database of genotype probabilities.
#'
#' @return An object of class `"calc_genoprob"` (a list of 3-dimensional arrays).
#'
#' @details
#' The genotype probabilities are extracted from the fst database. Each chromosome is extracted in turn.
#'
#' @export
#' @keywords utilities
#' @seealso [fst_genoprob()]
#'
#' @examples
#' library(qtl2)
#' grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
#' map <- insert_pseudomarkers(grav2$gmap, step=1)
#' probs <- calc_genoprob(grav2, map, error_prob=0.002)
#' dir <- tempdir()
#' fprobs <- fst_genoprob(probs, "grav2", dir, overwrite=TRUE)
#' nprobs <- fst_extract(fprobs)
#'
#' # clean up: remove all the files we created
#' unlink(fst_files(fprobs))
fst_extract <-
    function(object)
{
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

    # strip off fst_genoprob from class
    cl <- attrs$class
    class(result) <- cl[cl != "fst_genoprob"]

    result
}


#' @describeIn fst_extract Deprecated version (to be deleted)
#' @export
fst2calc_genoprob <-
    function(object)
{
    warning("fst2calc_genoprob() is deprecated and will be removed; use fst_extract() instead.")

    fst_extract(object)
}
