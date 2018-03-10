# subset fst_genoprob objects

#' Subsetting genotype probabilities
#'
#' Pull out a specified set of individuals and/or chromosomes from
#' the results of \code{\link{fst_genoprob}}.
#'
#' @param x Genotype probabilities as output from \code{\link{fst_genoprob}}.
#' @param ind A vector of individuals: numeric indices, logical
#' values, or character string IDs
#' @param chr A vector of chromosomes: logical values, or character
#' string IDs. Numbers are interpreted as character string IDs.
#' @param mar A vector of marker names as character string IDs.
#' @param ... Ignored.
#'
#' @return The input genotype probabilities, with the selected
#' individuals and/or chromsomes.
#'
#' @export
#'
#' @keywords utilities
#'
#' @examples
#' library(qtl2)
#' grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
#' \dontshow{grav2 <- grav2[1:8,c(1,2)]}
#' pr <- calc_genoprob(grav2)
#' dir <- tempdir()
#' fpr <- fst_genoprob(pr, "grav2", dir)
#' # keep just individuals 1:5, chromosome 2
#' prsub <- fpr[1:5,2]
#' # keep just chromosome 2
#' prsub2 <- fpr[,2]
subset_fst_genoprob <- function(x, ind=NULL, chr=NULL, mar=NULL, ...) {
    if(!inherits(x, "fst_genoprob"))
            stop("argument ", 1, "is not of class fst_genoprob")

    if(is.null(ind) && is.null(chr) && is.null(mar))
        stop("You must specify either ind or chr or mar.")

    attrs <- attributes(x)
    x <- unclass(x)

    if(!is.null(chr)) {
        x$chr <- get_dimension(chr, x$chr, type = "chromosome")
        attrs[["is_x_chr"]] <- attrs[["is_x_chr"]][chr]
        # Now adjust x$mar if needed
        tmp <- unlist(index_chr(x$dimnames[x$chr], 3, x$mar))
        names(tmp) <- NULL
        x$mar <- tmp
    }

    if(!is.null(ind))
        x$ind <- get_dimension(ind, x$ind, type = "individual")

    if(!is.null(mar)) {
        x$mar <- get_dimension(mar, x$mar, type = "marker")
        # Now adjust x$chr if needed
        mar_dim <- sapply(index_chr(x$dimnames[x$chr], 3, x$mar),
                          length)
        x$chr <- x$chr[mar_dim > 0]
    }

    # Set up attributes.
    ignore <- match(c("names","class"), names(attrs))
    for(a in names(attrs)[-ignore])
        attr(x, a) <- attrs[[a]]

    class(x) <- attrs$class

    x
}
get_dimension <- function(ind, indID, type = "individual") {
    n_ind <- length(indID)
    if(is.logical(ind)) {
        if(length(ind) != n_ind) {
            stop("length(ind) [", length(ind), "] != number of ", type, "s in x [",
                 n_ind, "]")
        }
        ind <- indID[ind] # convert to character strings
    }
    else if(is.numeric(ind)) {
        if(any(ind < 1 || ind > n_ind))
            stop("Numeric ind out of allowed range [1 - ", n_ind, "]")
        ind <- indID[ind] # convert to character strings
    }
    else {
        indindex <- match(ind, indID)
        if(any(is.na(indindex))) {
            stop("Some ", type, "s not found: ",
                 paste(ind[is.na(indindex)], collapse=", "))
        }
        ind <- indID[indindex]
    }
    if(length(ind) == 0)
        stop("Must retain at least one ", type, ".")

    ind
}

#' @export
#' @rdname subset_fst_genoprob
subset.fst_genoprob <- function(x, ind=NULL, chr=NULL, mar=NULL, ...) {
    subset_fst_genoprob(x, ind, chr, mar)
}

#' @importFrom fst read_fst
#' @export
`[.fst_genoprob` <- function(x, ind=NULL, chr=NULL, mar=NULL) {
    subset_fst_genoprob(x, ind, chr, mar)
}

# Return an array for chromosome, subset by individuals and markers.
element_fst_genoprob <- function(x, chr) {
    if(!inherits(x, "fst_genoprob"))
        stop("argument ", 1, "is not of class fst_genoprob")

    chr <- get_dimension(chr, names(x), type = "marker")

    if(length(chr) != 1)
        stop("need exactly one chr")

    is_x_chr <- attr(x, "is_x_chr")
    x <- unclass(x)

    # Make sure we have chr ID.
    if(is.numeric(chr))
        chr <- x$chr[chr]

    dnames <- x$dimnames[[chr]]
    dims <- x$dim[,chr]

    m <- which(dnames[[3]] %in% x$mar)
    if(!length(m))
        return(NULL)
    dnames[[3]] <- dnames[[3]][m]
    dims[3] <- length(m)

    path <- paste0(x$fst, "_", chr, ".fst")
    probs <- fst::read_fst(path, columns = dnames[[3]])

    probs <- as.array(as.matrix(probs))
    dim(probs) <- dims
    dimnames(probs) <- dnames

    # Subset on individuals and markers.
    probs[x$ind, ,, drop=FALSE]
}

#' @export
`[[.fst_genoprob` <- function(x, chr) {
    element_fst_genoprob(x, chr)
}

#' @export
`$.fst_genoprob` <- function(x, chr) {
    x[[chr]]
}
