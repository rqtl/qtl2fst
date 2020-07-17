#' Path used in fst_genoprob object
#'
#' Get the path used in an fst_genoprob object.
#'
#' @param object An object of class `"fst_genoprob"` as created by [fst_genoprob()].
#'
#' @return Character string with path (and initial file stem) for files used in the input `object`.
#'
#' @export
#' @seealso [fst_files()], [replace_path()]
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
#'
#' # clean up: remove all the files we created
#' unlink(fst_files(fprobs))
fst_path <-
    function(object)
{
    if(!inherits(object, "fst_genoprob"))
        stop('object is not of class "fst_genoprob"')

    unclass(object)$fst
}


#' Replace the path used in fst_genoprob object
#'
#' Replace the path used in an fst_genoprob object.
#'
#' @param object An object of class `"fst_genoprob"` as created by [fst_genoprob()].
#'
#' @param path New path (directory + file stem as a single character string) to be used in the object.
#'
#' @return The input `object` with the path replaced.
#' If any of the expected files don't exist with the new path, warnings are issued.
#'
#' @examples
#' library(qtl2)
#' grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
#' probs <- calc_genoprob(grav2, error_prob=0.002)
#' dir <- tempdir()
#' fprobs <- fst_genoprob(probs, "grav2", dir, overwrite=TRUE)
#'
#' # move the probabilities into a different directory
#' new_dir <- file.path(tempdir(), "subdir")
#' if(!dir.exists(new_dir)) dir.create(new_dir)
#' for(file in fst_files(fprobs)) {
#'    file.rename(file, file.path(new_dir, basename(file)))
#' }
#'
#' # revise the path in fprobs
#' new_path <- sub(dir, new_dir, fst_path(fprobs))
#' fprobs <- replace_path(fprobs, new_path)

#' @export
#' @seealso [fst_path()], [fst_files()]
replace_path <-
    function(object, path)
{
    if(!inherits(object, "fst_genoprob"))
        stop('object is not of class "fst_genoprob"')

    if(missing(path) || is.null(path) || !is.character(path) || length(path) != 1)
        stop("path should be a single character string")

    cl <- class(object)
    class(object) <- "list"
    object$fst <- path
    class(object) <- cl

    # warning if the files don't exist
    fst_files(object)

    object
}
