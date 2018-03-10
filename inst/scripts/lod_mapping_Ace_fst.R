library(dplyr)
library(qtl2)
library(qtl2convert)

library(fst)

load("C:/Temp/kidney.RData")
# which gene is Ace?
idx <- which(annot.mrna$symbol == "Ace")
addcovar <- covar[,-1]

db.file <- "C:/Temp/genoprobs_kidney.fst"

Nbatches = 1000
Nsnps <- nrow(snps)
# make SNPs split into Nbatches
batches <-  split(1:Nsnps, ceiling(seq_along(1:Nsnps)/Nbatches))

# lod object to be produced
result <- NULL

for (i in 1:length(batches)) {

  # fetch data from the database
  tmp <- read_fst(db.file, batches[[i]])
  print(i)

  # dimnames harcoded, ADD ASSERTIONS LATER
  tmp.snps <- names(tmp)
  tmp.probs <- as.array(as.matrix(tmp))
  dim(tmp.probs) <- c(nrow(tmp.probs) %/% 8, 8, ncol(tmp.probs))
  dimnames(tmp.probs) <- list(annot.samples$Mouse.ID, LETTERS[1:8], tmp.snps)

  # convert to qtl2 format
  probs <- probs_doqtl_to_qtl2(tmp.probs, snps[tmp.snps,], pos_column = "bp")
  chrs <- unique(snps[tmp.snps,"chr"])

  tmp.fit <- scan1(genoprobs=probs,
               kinship=Glist[chrs],
               pheno=expr.mrna[,idx],
               addcovar=addcovar,
               cores=1,
               reml=TRUE)

  # save LOD scores to results
  result <- rbind(result, tmp.fit$lod)
}
