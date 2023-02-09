
#---------Total UMIs per cell plot-----------
umiPlot <- function(totalumi, cell_ind){
  par(mgp = c(1.7, .6, 0), mar = c(3, 3, .5, .3))
  plot(totalumi, xlab = "Cell indices", ylab = "Total UMI",
       type = "l", col = "steelblue4", lwd = .5)
  
  # First replicate's median
  points(cell_ind[[1]], rep(median(totalumi[cell_ind[[1]]]), length(cell_ind[[1]])),
         cex = .1, col = 'gray77')
  
  # Other replicates median 
  for (i in 2:length(cell_ind)){
    # Replicate's start
    abline(v = length(cell_ind[[i-1]]), col = "black", lwd = 3, lty = 3)
    # Replicate's median
    points(cell_ind[[i]], rep(median(totalumi[cell_ind[[i]]]), length(cell_ind[[i]])),
           cex = .1, col = 'gray77')
  }
}


#---------Histogram-----------
hist_x <- function(x, title, x_label, cutoff = 0) {
  par(mgp = c(1.7, .6, 0), mar = c(3, 3, 1, .3))
  hist(x, breaks = 70, main = title, xlab = x_label, col = "steelblue3", xaxt = "n")
  # x-axis
  axis(1, at = seq(0, max(x), 10^floor(log10(max(x)))), pos = 0)
  # Cutoff value
  if (cutoff != 0) {abline(v = cutoff, col = "black", lwd = 3, lty = 1)}
}


#---------Histogram - Total UMIs per cell-----------
histumi <- function(x, title, cutoff = 0){
  x_label <- "Total UMI count per cell"
  hist_x(x, title, x_label, cutoff)
}


#---------Histogram - Num. of mapped genes per cell-----------
histm <- function(x, title, cutoff = 0){
  x_label <- "Number of mapped genes per cell"
  hist_x(x, title, x_label, cutoff)
}

