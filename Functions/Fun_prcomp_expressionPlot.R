source('Functions/Fun_prcomp_Plots.R')

# Global variables: pca (prcomp object), logCounts

#---------plotPCA_ind, for individual plots-----------
plotPCA_ind <- function(dim1, dim2, colIn, psize, bgcol = 'gray87'){
  # mgp: labels, numbers, ticks; mar: x1, y1, x2, y2
  par(mgp = c(2, .6, 0)) #, mar = c(4.5, 4, 1.5, 0.5)
  par(bg = bgcol, cex.axis = 1, cex.lab = 1)
  
  plot(pca$x[,dim1], pca$x[,dim2], pch = 20, cex = psize, bty = "L", col = colIn,
       # asp = 1, # margins are overwriting axes-limit settings when asp is set.
       xlim = c(min(pca$x[,dim1]), max(pca$x[,dim1])),
       xlab = paste0("PC", dim1, " (", cVar(pca, dim1), "%)"), 
       ylab = paste0("PC", dim2, " (", cVar(pca, dim2), "%)"))
}

#---------plotPCA, for multipanel plotting-----------
plotPCA_m <- function(dim1, dim2, colIn, psize, bgcol = 'gray87'){
  # mgp: labels, numbers, ticks; mar: x1, y1, x2, y2
  par(mgp = c(1.7, .6, 0), mar = c(.5, 2.7, 0, .3)) 
  
  par(bg = bgcol, cex.axis = 1, cex.lab = 1)
  plot(pca$x[,dim1], pca$x[,dim2], pch = 20, cex = psize, bty = "L", col = colIn,
       ylab = paste0("PC", dim2, " (", cVar(pca, dim2), "%)"),
       xlab = '', xaxt = "n")
  # Specific: PC4 and PC5 are plotted as a 3rd and 4th panel
  if (dim2 %in% 4:5) {axis(side = 1)}
}

#---------plotVector-----------
# Color-coded with any vector ('arg') of 'numCells' length
# (with black circumferences it can endure light colours as colour-code...)
plotVector <- function(dim1, dim2, arg, colPal, steps, psize1, psize2, if_legend, 
                       if_panels, bgcol = 'gray87', legend_pos = c(60,17,62,32)){
  # Plot PCA
  if (if_panels==T){
    plotPCA_m(dim1, dim2, 'black', psize1, bgcol) 
  }
  else plotPCA_ind(dim1, dim2, 'black', psize1, bgcol)
  
  # Sort the argument
  sortInd <- order(arg) # indices in 'arg' corresponding to ascending order
  sortArg <- arg[sortInd] # all(arg[sortInd] == sort(arg))
  
  # Colour-code data
  colData = colPal(steps)[as.numeric(cut(sortArg, breaks = steps))]
  # Plot overlay
  points(pca$x[sortInd, dim1], pca$x[sortInd, dim2], pch = 20, cex = psize2, col = colData)
  
  # Legend
  if (if_legend==T){
    gradientLegend(c(0, max(arg)), color = colPal(steps), nCol = steps, pos = legend_pos,
                 coords =T, pos.num = NULL, n.seg = 2,  dec = 1, border.col = "Black",
                 fit.margin = TRUE)
  }
}

#---------plotExpr-----------
# Gene expression plot (basically, 'plotVector' called with gene name)
plotExpr <- function(dim1, dim2, gene, colPal, steps, psize1, psize2, if_legend, 
                     if_panels, bgcol = 'gray87', legend_pos = c(60,17,62,32)){
  # Gene ID
  id = rownames(info)[info['Gene.Name']==gene]
  # Gene expression value
  arg = as.numeric(logCounts[id, ])
  # Plot
  plotVector(dim1, dim2, arg, colPal, steps, psize1, psize2, if_legend, if_panels, 
             bgcol, legend_pos)
  # Title
  if (if_panels==F) {title(main = gene, cex.main = 0.9)}
}

