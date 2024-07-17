# genoprob_to_alleleprob_fst
#' Convert genotype probabilities to allele probabilities and write to fst database
#'
#' Reduce genotype probabilities (as calculated by
#' [qtl2::calc_genoprob()]) to allele probabilities, writing them to an fst database.
#'
#' @param probs Genotype probabilities, as calculated from
#' [qtl2::calc_genoprob()].
#' @param fbase Base of filename for fst database.
#' @param fdir Directory for fst database.
#' @param quiet IF `FALSE`, print progress messages.
#' @param cores Number of CPU cores to use, for parallel calculations.
#' (If `0`, use [parallel::detectCores()].)
#' Alternatively, this can be links to a set of cluster sockets, as
#' produced by [parallel::makeCluster()].
#' @param compress Amount of compression to use (value in the range 0-100; lower values mean larger file sizes)
#' @param overwrite If FALSE (the default), refuse to overwrite any files that already exist.
#'
#' @return Link to fst database for the `probs` input with probabilities
#' collapsed to alleles rather than genotypes.
#'
#' @details This is like calling `qtl2::genoprob_to_alleleprob()` and then
#'     `fst_genoprob()`, but in a way that hopefully saves memory by
#'     doing it one chromosome at a time.
#'
#' @export
#' @importFrom qtl2 genoprob_to_alleleprob
#' @keywords utilities
#' @seealso [qtl2::genoprob_to_alleleprob()], [fst_genoprob()]
#'
#' @examples
#' library(qtl2)
#' iron <- read_cross2(system.file("extdata", "iron.zip", package="qtl2"))
#' gmap_w_pmar <- insert_pseudomarkers(iron$gmap, step=1)
#'
#' # genotype probabilities
#' fst_dir <- file.path(tempdir(), "iron_genoprob")
#' dir.create(fst_dir)
#' probs_fst <- calc_genoprob_fst(iron, "iron", fst_dir, gmap_w_pmar, error_prob=0.002)
#'
#' # allele probabilities
#' fst_dir_apr <- file.path(tempdir(), "iron_alleleprob")
#' dir.create(fst_dir_apr)
#' aprobs_fst <- genoprob_to_alleleprob_fst(probs_fst, "iron", fst_dir_apr)
#'
#' # clean up: remove all the files we created
#' unlink(fst_files(probs_fst))
#' unlink(fst_files(aprobs_fst))

genoprob_to_alleleprob_fst <-
function(probs, fbase, fdir=".", quiet=TRUE, cores=1, compress=0, overwrite=FALSE)
{
    if(!inherits(probs, "calc_genoprob")) {
        stop('Input should have class "calc_genoprob".')
    }

    if(is.null(fbase) || missing(fbase)) stop("fbase must be provided")

    cores <- setup_cluster(cores)

    by_chr_function <- function(chr) {
        if(!quiet) message("Chr ", chr)
        apr <- genoprob_to_alleleprob(probs[,chr], quiet=TRUE, cores=1)

        fst_genoprob(apr, fbase, fdir, compress=compress,
                     overwrite=overwrite, quiet=quiet)
    }

    apr_fst <- cluster_lapply(cores, names(probs), by_chr_function)
    names(apr_fst) <- names(probs)

    do.call("cbind", apr_fst)
}
