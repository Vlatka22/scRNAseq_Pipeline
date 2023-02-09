library(matrixStats)
library(plyr)

#---------Quantile.loess FUNCTION-----------
# https://www.r-statistics.com/2010/04/quantile-loess-combining-a-moving-quantile-window-with-loess-r-function/
# Loading the Quantile.loess function:
source("https://www.r-statistics.com/wp-content/uploads/2010/04/Quantile.loess_.r.txt")


#---------DM (distance to the median) FUNCTION-----------
# Modifying the DM function from Jong Kyoung Kim (Kolodziejczyk et al. 2015)
# Without the length correction - no length bias in UMI-based seq.

DMqloess <- function(data, exprThreshold, output) {
  data <- data[rowMeans(data) > exprThreshold,]
  mean <- rowMeans(data)
  CV2 <- rowVars(as.matrix(data))/mean^2 
  CV2[is.nan(CV2)] <- 0
  
  qloess <- Quantile.loess(log10(CV2), log10(mean),
                           the.quant = .5, #default=.95
                           window.size = 20, #default=20
                           percent.of.overlap.between.two.windows = .5 #default=NULL
  )
  DM <- aaply(seq(1, length(mean)), 1, function(x) {
    # Determines the nearest (x-wise) qloess point
    mindex <- which.min(abs(qloess[[2]] - log10(mean[x])))
    # Calculates the difference between logCV(gene) to logCV of the qloess point
    as.vector(log10(CV2[x]) - qloess[[1]][mindex])
  })#, .expand=FALSE, .progress="text")
  names(DM) <- names(mean)
  
  if (output=="qloess"){return(qloess)}
  if (output=="DM"){return(DM)}
}


#---------VARIABLE GENES-----------
variableGenes <- function(data, exprThreshold, perc) {
  # logCV2 distance from the expected value
  DM <- DMqloess(data, exprThreshold, "DM")
  # How many top variable genes
  numVarGenes <- length(DM) * perc
  # Variable genes' names
  geneNames <- names(sort(DM, decreasing=TRUE)[1:numVarGenes])
  return(geneNames)
}
