# Plotting 'prcomp' objects

#---------cVar-----------
# Component's variance contribution
cVar <- function(pca, i){
  cvar <- format(pca$sdev[i]^2/sum(pca$sdev^2)*100, digits=3)
  return(cvar)
}

#---------plotPCA-----------
# Axes labeled with 'cVar' output
plotPCA <- function(pca, dim1, dim2, color = "black"){
  par(mar = c(2.7, 2.7, .7, 3.2), mgp = c(1.5, .5, 0), cex.axis = 1.2, cex.lab = 1.2)
  plotpca <- plot(pca$x[,dim1], pca$x[,dim2], pch = 20, cex = 0.8, col = color, 
                  xlab = paste0("PC", dim1, " (", cVar(pca, dim1), "%)"), 
                  ylab = paste0("PC", dim2, " (", cVar(pca, dim2), "%)"))
  return(plotpca)
}

#---------pointcolor-----------
# Overlay certain replicate's colored dots
pointRep <- function(pca, dim1, dim2, name, color){
  pcolour <- points(pca$x[grepl(name, rownames(pca$x)), dim1],
                   pca$x[grepl(name, rownames(pca$x)), dim2],
                   pch = 20, cex = 0.8, col = color)
  return(pcolour)
}

#---------plot_col-----------
# Map desired cell subsets on the PCA plot
plot_col <- function(dim1, dim2, set, psize, bgcol = 'gray', pos_legend = 'topright'){
  # Set colors for overlay
  cellcolor = rainbow(length(set))
  
  # Plot PCA coordinates
  plotPCA(pca, dim1, dim2)
  
  # Plot chosen variable 'set' (list of cell indices vectors)
  for (i in 1:length(set)){
    points(pca$x[unlist(set[i]), dim1], pca$x[unlist(set[i]), dim2], 
           pch = 20, cex = psize, col = cellcolor[i])
  }
  # Legend
  legend(pos_legend, legend = 1:length(set), xpd = T, 
         pch = 20, col = cellcolor, bty = "n", cex = 0.9)
}
