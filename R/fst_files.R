#' List files used in fst_genoprob object
#'
#' List all of the files used in an fst_genoprob object.
#'
#' @md
#'
#' @param object An object of class `"fst_genoprob"` as created by [fst_genoprob()].
#'
#' @return Vector of character strings with the full paths for all of the files used for the input `object`.
#'
#' @export
#' @seealso [fst_path()]
#'
#' @examples
#' library(qtl2)
#' grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
#' probs <- calc_genoprob(grav2, error_prob=0.002)
#' dir <- tempdir()
#' fprobs <- fst_genoprob(probs, "grav2", dir, overwrite=TRUE)
#'
#' fst_path(fprobs)
#' fst_files(fprobs)
#' \dontshow{unlink(fst_files(fprobs))}

fst_files <-
    function(object)
{
    path <- fst_path(object)

    chr <- names(object)

    files <- c( paste0(path, "_index.rds"),
               paste0(path, "_", chr, ".fst") )

    exist <- file.exists(files)
    if(!all(exist)) warning(sum(!exist), " of the expected ", length(exist), " files don't exist")

    files
}
