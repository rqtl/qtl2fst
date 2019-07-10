# calc_genoprob_fst
#' Calculate conditional genotype probabilities and write to fst database
#'
#' Uses a hidden Markov model to calculate the probabilities of the
#' true underlying genotypes given the observed multipoint marker
#' data, with possible allowance for genotyping errors.
#'
#' @param cross Object of class `"cross2"`. For details, see the
#' [R/qtl2 developer guide](https://kbroman.org/qtl2/assets/vignettes/developer_guide.html).
#' @param fbase Base of filename for fst database.
#' @param fdir Directory for fst database.
#' @param map Genetic map of markers. May include pseudomarker
#' locations (that is, locations that are not within the marker
#' genotype data). If NULL, the genetic map in `cross` is used.
#' @param error_prob Assumed genotyping error probability
#' @param map_function Character string indicating the map function to
#' use to convert genetic distances to recombination fractions.
#' @param lowmem If `FALSE`, split individuals into groups with
#' common sex and crossinfo and then precalculate the transition
#' matrices for a chromosome; potentially a lot faster but using more
#' memory.
#' @param quiet If `FALSE`, print progress messages.
#' @param cores Number of CPU cores to use, for parallel calculations.
#' (If `0`, use [parallel::detectCores()].)
#' Alternatively, this can be links to a set of cluster sockets, as
#' produced by [parallel::makeCluster()].
#' @param compress Amount of compression to use (value in the range 0-100; lower values mean larger file sizes)
#' @param overwrite If FALSE (the default), refuse to overwrite any files that already exist.
#'
#' @return A list containing the attributes of `genoprob`
#' and the address for the created fst database.
#' Components are:
#' * `dim` - List of all dimensions of 3-D arrays.
#' * `dimnames` - List of all dimension names of 3-D arrays.
#' * `is_x_chr` - Vector of all is_x_chr attributes.
#' * `chr` - Vector of (subset of) chromosome names for this object.
#' * `ind` - Vector of (subset of) individual names for this object.
#' * `mar` - Vector of (subset of) marker names for this object.
#' * `fst` - Path and base of file names for the fst database.
#'
#' @details This is like calling `qtl2::calc_genoprob()` and then
#'     `fst_genoprob()`, but in a way that hopefully saves memory by
#'     doing it one chromosome at a time.
#'
#' @export
#' @importFrom qtl2 insert_pseudomarkers calc_genoprob
#' @importFrom stats setNames
#' @keywords utilities
#' @seealso [qtl2::calc_genoprob()], [fst_genoprob()]
#'
#' @examples
#' library(qtl2)
#' grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
#' gmap_w_pmar <- insert_pseudomarkers(grav2$gmap, step=1)
#' fst_dir <- file.path(tempdir(), "grav2_genoprob")
#' dir.create(fst_dir)
#' probs_fst <- calc_genoprob_fst(grav2, "grav2", fst_dir, gmap_w_pmar, error_prob=0.002)

calc_genoprob_fst <-
function(cross, fbase, fdir=".", map=NULL, error_prob=1e-4,
         map_function=c("haldane", "kosambi", "c-f", "morgan"),
         lowmem=FALSE, quiet=TRUE, cores=1, compress=0, overwrite=FALSE)
{
    map_function <- match.arg(map_function)

    # pseudomarker map
    if(is.null(map)) {
        if("gmap" %in% names(cross)) {
            map <- qtl2::insert_pseudomarkers(cross$gmap)
        } else if("pmap" %in% names(cross)) {
            map <- qtl2::insert_pseudomarkers(cross$pmap)
        } else {
            stop("cross doesn't contain a genetic or physical map")
        }
    }
    # possibly subset the map
    if(length(map) != length(cross$geno) || !all(names(map) == names(cross$geno))) {
        chr <- names(cross$geno)
        if(!all(chr %in% names(map)))
            stop("map doesn't contain all of the necessary chromosomes")
        map <- map[chr]
    }

    if(is.null(fbase) || missing(fbase)) stop("fbase must be provided")

    pr_fst <- setNames(vector("list", length(cross$geno)), names(cross$geno))
    for(chr in names(cross$geno)) {
        if(!quiet) message("Chr ", chr)
        pr <- calc_genoprob(cross[,chr], map[chr], error_prob=error_prob,
                            map_function=map_function, lowmem=lowmem,
                            quiet=TRUE, cores=cores)

        pr_fst[[chr]] <- fst_genoprob(pr, fbase, fdir, compress=compress,
                                      overwrite=overwrite, quiet=quiet)
    }

    do.call("cbind", pr_fst)
}
