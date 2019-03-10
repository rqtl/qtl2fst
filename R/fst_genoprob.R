# fst_genoprob
#' Store genotype probabilities in fst database
#'
#' Save an R/qtl2 genotype probabilities object to a set of fst files for fast access with reduced memory usage.
#'
#' @param genoprob Object of class `"calc_genoprob"`. For details, see the
#' [R/qtl2 developer guide](https://kbroman.org/qtl2/assets/vignettes/developer_guide.html)
#' and [qtl2::calc_genoprob()].
#' @param fbase Base of filename for fst database.
#' @param fdir Directory for fst database.
#' @param compress Amount of compression to use (value in the range 0-100; lower values mean larger file sizes)
#' @param overwrite If FALSE (the default), refuse to overwrite any files that already exist.
#' @param quiet If FALSE (the default), show messages about fst database creation.
#' @param verbose Opposite of `quiet`; deprecated argument (to be removed).
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
#' @details
#' The genotype probabilities are stored in separate databases for each chromosome
#' as tables of (indivduals*genotypes) x (positions) in directory `fst`.
#' The `dim`, `dimnames` and `is_x_chr` elements of the object
#' have information about the entire fst database.
#' If a `fst_genoprob` object is a subset of another such object,
#' the `chr`, `ind`, and `mar` contain information about what is in the subset.
#' However, the `fst` databases are not altered in a subset, and can be restored by
#' [fst_restore()]. The actual elements of an `"fst_genoprob"`
#' object are only accessible to the user after a call to [unclass()]; instead
#' the usual access to elements of the object invoke [subset.fst_genoprob()].
#'
#' @importFrom fst write_fst
#' @export
#' @keywords utilities
#' @seealso [fst_path()], [fst_extract()], [fst_files()], [replace_path()], [fst_restore()]
#'
#' @examples
#' library(qtl2)
#' grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
#' map <- insert_pseudomarkers(grav2$gmap, step=1)
#' probs <- calc_genoprob(grav2, map, error_prob=0.002)
#' dir <- tempdir()
#' fprobs <- fst_genoprob(probs, "grav2", dir, overwrite=TRUE)
#' \dontshow{unlink(fst_files(fprobs))}

#' @describeIn probs2fst Deprecated version (to be deleted)
#' @export
fst_genoprob <-
    function(genoprob, fbase, fdir=".", compress=0, verbose=TRUE, overwrite=FALSE, quiet=!verbose)
{
    if(!missing(verbose)) {
        warning('The verbose argument is deprecated and will be removed; use "quiet" instead.')
    }

    if(!is.numeric(compress) || length(compress) != 1 || compress < 0 || compress > 100) {
        stop("compress should be a number between 0 and 100")
    }

    # Get attributes from genoprob object.
    attrs <- attributes(genoprob)

    # Get dimensions and dimnames for chromosome information
    chr_dim <- lapply(genoprob, function(x) attributes(x))
    result <- list(dim = sapply(chr_dim, function(x) x$dim),
                   dimnames = lapply(chr_dim, function(x) x$dimnames),
                   is_x_chr = attr(genoprob, "is_x_chr"))

    # Identify chromosome, individuals, markers (for later subset use).
    result$chr <- names(genoprob)
    result$ind <- result$dimnames[[1]][[1]]
    tmp <- unlist(lapply(result$dimnames, function(x) x[[3]]))
    names(tmp) <- NULL
    result$mar <- tmp

    # Add fst addresses
    if(missing(fbase) || is.null(fbase) || !is.character(fbase) || length(fbase) != 1) {
        stop('fbase should be a single character string, for the file "stem"')
    }
    if(is.null(fdir) || fdir == "") result$fst <- fbase
    else result$fst <- file.path(fdir, fbase)

    # Make sure directory exists
    if(!dir.exists(dirname(result$fst))) {
        stop("directory ", dirname(result$fst), " does not exist")
    }

    # Turn list of 3D arrays into table
    # Need to handle X chr separately!
    tbl_array <- function(x) {
        dims <- dim(x)
        dnames <- dimnames(x)
        dim(x) <- c(prod(dims[1:2]), dims[3])
        dimnames(x) <- list(NULL, dnames[[3]])
        as.data.frame(x)
    }

    files <- paste0(result$fst, "_", result$chr, ".fst")
    exists <- file.exists(files)
    if(!overwrite && any(exists)) {
        stop(sum(exists), " of the ", length(files), " already exist. ",
             "Use overwrite=TRUE to overwrite")
    }

    for(chr in result$chr) {
        probs <- tbl_array(genoprob[[chr]])
        fname <- paste0(result$fst, "_", chr, ".fst")
        if(file.exists(fname))
            warning("writing over existing ", fname)
        if(!quiet) message("writing ", fname)
        fst::write_fst(probs, fname, compress=compress)
    }

    # Set up attributes.
    ignore <- match(c("names","class"), names(attrs))
    for(a in names(attrs)[-ignore])
        attr(result, a) <- attrs[[a]]

    # RDS file to save index object
    index_file <- paste0(result$fst, "_fstindex.rds")

    class(result) <- c("fst_genoprob", attrs$class)

    # write index object to RDS file
    saveRDS(result, index_file)

    result
}

#' @export
# chromosome names in fst_genoprob database
names.fst_genoprob <- function(x) {
    unclass(x)$chr
}

#' @export
# number of chromosomes in fst_genoprob database
length.fst_genoprob <- function(x) {
    length(unclass(x)$chr)
}

#' @export
# dimension of fst_genoprob object
# matrix of size 3 x n_chr
# columns are chromosomes; rows are no. individuals, no. genotypes, and no. markers
dim.fst_genoprob <- function(x) {
    x <- unclass(x)
    out <- x$dim[, x$chr, drop = FALSE]
    rownames(out) <- c("ind","gen","mar")
    out[1,] <- length(x$ind)
    out[2,] <- sapply(index_chr(x$dimnames[x$chr], 2),
                      length)
    out[3,] <- sapply(index_chr(x$dimnames[x$chr], 3, x$mar),
                      length)
    out
}

#' @export
# dimnames of fst_genoprob object
#
# value = list with three components
#   first is vector of individual names
#   2nd is list of length n_chr, each being the genotype names for that chromosome
#   3rd is list of length n_chr, each being vector of marker names for that chromosome
dimnames.fst_genoprob <- function(x) {
    x <- unclass(x)
    dnames <- x$dimnames
    out <- list(ind = x$ind,
                gen = index_chr(dnames[x$chr], 2),
                mar = index_chr(dnames[x$chr], 3, x$mar))
    out
}


# dnames are dimension names for each chromosome
#    so a list of length n_chr, each having ind IDs x genotype names x marker names
# index is a numeric value in {1,2,3}; we're pulling out this component of the dimnames for each chromosome
# index_sub is a further index applied to the bit that's getting pulled out (actually a character vector)
#
# for each i in 1:length(dnames),
#    we take dnames[[i]][[index]]
# if index_sub is provided, we then take the part that is contained in index_sub
index_chr <- function(dnames, index, index_sub=NULL) {
    lapply(dnames, function(x, index, index_sub) {
        xnames <- x[[index]]
        if(!is.null(index_sub))
            xnames <- xnames[xnames %in% index_sub]
        xnames
    },
    index, index_sub)
}
