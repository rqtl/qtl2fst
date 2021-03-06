% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fst_restore.R
\name{fst_restore}
\alias{fst_restore}
\alias{fst_genoprob_restore}
\title{Restore fst_genoprob object to original dimensions.}
\usage{
fst_restore(object)

fst_genoprob_restore(object)
}
\arguments{
\item{object}{Object of class \code{"fst_genoprob"} as produced by \code{\link[=fst_genoprob]{fst_genoprob()}}.}
}
\value{
Input \code{object} with dimensions restored.
}
\description{
Any \code{"fst_genoprob"} object has embedded its original data and dimensions.
This resets elements \code{ind}, \code{chr} and \code{mar} to the full set.
}
\details{
Object is unclassed and elements \code{ind}, \code{chr} and \code{mar} are changed before
reseting attributes as \code{"fst_genoprob"} object.
See \code{\link[=fst_genoprob]{fst_genoprob()}} for details on the object.
}
\section{Functions}{
\itemize{
\item \code{fst_genoprob_restore}: Deprecated version (to be removed).
}}

\examples{
library(qtl2)
grav2 <- read_cross2(system.file("extdata", "grav2.zip", package="qtl2"))
map <- insert_pseudomarkers(grav2$gmap, step=1)
probs <- calc_genoprob(grav2, map, error_prob=0.002)
dir <- tempdir()
fprobs <- fst_genoprob(probs, "grav2", dir, overwrite=TRUE)

# subset probabilities
fprobs2 <- subset(fprobs, chr=1:2)

# use object to get the full probabilities back
fprobs5 <- fst_restore(fprobs2)

# clean up: remove all the files we created
unlink(fst_files(fprobs))

}
\seealso{
\code{\link[=fst_genoprob]{fst_genoprob()}}, \code{\link[=fst_extract]{fst_extract()}}
}
\keyword{utilities}
