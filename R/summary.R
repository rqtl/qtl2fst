# summary functions

# summary.fst_genoprob
#' Summary of an fst_genoprob object
#'
#' Summarize an fst_genoprob object
#'
#' @param object An object of class `"fst_genoprob"`, as output by [fst_genoprob()].
#' @param ... Ignored.
#'
#' @export
#' @keywords utilities
#'
#' @examples
#' library(qtl2)
#' grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
#' \dontshow{grav2 <- grav2[1:8,c(1,2)]}
#' pr <- calc_genoprob(grav2)
#' dir <- tempdir()
#' fpr <- fst_genoprob(pr, "grav2", dir)
#'
#' # summary of fst_genoprob object
#' summary(fpr)
#'
#' # clean up: remove all the files we created
#' unlink(fst_files(fpr))
summary.fst_genoprob <-
function(object, ...)
{
    if(!inherits(object, "fst_genoprob")) {
        stop('Input should have class "fst_genoprob".')
    }

    d <- dim(object)

    result <- list(path=fst_path(object),
                   crosstype=attr(object, "crosstype"),
                   chr=names(object),
                   n_ind=d[1,1],
                   n_gen=d[2,],
                   n_mar=d[3,],
                   alleleprobs=attr(object, "alleleprobs"))

    class(result) <- c("summary.fst_genoprob", "list")
    result
}

#' @export
print.summary.fst_genoprob <-
function(x, ...)
{
    cat('Object of class fst_genoprob\n\n')
    cat('path = "', x$path, '"\n', sep="")
    if(x$alleleprobs) { cat("Allele probabilities\n") }
    cat("\n")

    toprint <- c("No. individuals"=             x$n_ind,
                 "No. chromosomes"=             length(x$chr),
                 "Total positions"=             sum(x$n_mar))

    print_aligned(toprint)

    cat("\nNo. positions by chr:\n")
    print(x$n_mar)

    cat("\nNo. genotypes by chr:\n")
    print(x$n_gen)

    invisible(x)
}

print_aligned <-
function(x)
{
    newline_before <- grepl("\\n", names(x))
    names(x) <- sub("^\\n", "", names(x))

    ndig <- ceiling(log10(max(x, na.rm=TRUE)))
    n.char <- max(nchar(names(x)))

    format <- paste0("%-", n.char+1, "s  %", ndig, "d\n")

    for(i in seq(along=x)) {
        if(newline_before[i]) cat("\n")
        cat(sprintf(format, names(x)[i], x[i]))
    }
}

#' @export
print.fst_genoprob <- function(x, ...) print(summary(x, ...))
