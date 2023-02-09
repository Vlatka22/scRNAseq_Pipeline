# Global variables: XX, logCounts, info

#---------plotEE-----------
plotEE <- function(colIn, psize, background = 'white'){
  # mgp: labels, numbers, ticks; mar: x1, y1, x2, y2
  par(mgp = c(1.6, .5, 0), mar = c(3, 3, 1.5, 0.5))
  par(bg = background, cex.axis = 1, cex.lab = 1) #gray87
  plot(XX[,1], XX[,2], pch = 20, cex = psize, bty = "L", col = colIn, 
       xlab = 'dimension 1', ylab = 'dimension 2')
}


#---------plotVector-----------
# Color-coded with any vector ('arg') of 'numCells' length
# (with black circumferences it can endure light colours as color-code...)
vplot <- function(arg, colPal, steps, psize1, psize2, if_legend, background = 'white', 
                  pos_legend = c(5, 1.5, 5.1, 3)){
  # Plot EE
  plotEE('black', psize1, background)
  
  # Sort the argument
  sortInd <- order(arg) # indices in 'arg' corresponding to ascending order
  sortArg <- arg[sortInd] # all(arg[sortInd] == sort(arg))
  
  # Colour-code data
  colData = colPal(steps)[as.numeric(cut(sortArg, breaks = steps))]
  # Plot overlay
  points(XX[sortInd, 1], XX[sortInd, 2], pch = 20, cex = psize2, col = colData)
  
  # Legend
  if (if_legend==T){
    gradientLegend(c(0, max(arg)), color = colPal(steps), nCol = steps, pos = pos_legend,
                 coords =T, pos.num = NULL, n.seg = 2,  dec = 1, border.col = "Black",
                 fit.margin = TRUE)
  }
}


#---------plot Expression-----------
# Global variables: XX, info, logCounts

#---------idplot-----------
## Gene expression plot (basically, 'plotVector' called with gene ID)
idplot <- function(id, colPal, steps, psize1, psize2, if_legend, if_title = T, 
                   background = 'white', pos_legend = c(5, 1.5, 5.1, 3)){
  # Gene expression value
  arg = as.numeric(logCounts[id, ])
  # Plot
  vplot(arg, colPal, steps, psize1, psize2, if_legend, background, pos_legend)
  # Gene Name
  gene = as.character(info[id, 'Gene.Name'])
  # Title
  if (if_title==T) {title(main = gene, cex.main = 0.9)}
}


#---------eplot-----------
## Gene expression plot ('idplot' called with gene name)
eplot <- function(gene, colPal, steps, psize1, psize2, if_legend, if_title = T, 
                  background = 'white', pos_legend = c(5, 1.5, 5.1, 3)){
  # Gene ID
  id = rownames(info)[which(info['Gene.Name']==gene)]
  # idplot
  idplot(id, colPal, steps, psize1, psize2, if_legend, if_title, 
                     background, pos_legend)
}


#---------Function 'vplotTop-----------
# Modified 'vplot' function for an option to cut-off the color scale 
# at a set value, i.e. everything of a higher value stays black...
vplotTop <- function(arg, out, colPal, steps, psize1, psize2, if_legend, l_dec, 
                     background = 'white', pos_legend = c(5, 1.5, 5.1, 3)){
  # Plot EE
  plotEE('black', psize1, background)
  
  # Sort the argument
  sortInd <- order(arg) # indices in 'arg' corresponding to ascending order
  sortArg <- arg[sortInd] # all(arg[sortInd] == sort(arg))
  
  # BELOW THE THRESHOLD VALUE
  argIn <- sortArg[sortArg <= out] # used here and in legend
  subInd <- sortInd[sortArg <= out] # ind in 'arg' (ascending & bellow threshold)
  #
  # Colour-code data
  colData <- colPal(steps)[cut(as.numeric(argIn), breaks = steps)]
  # Plot overlay
  points(XX[subInd, 1], XX[subInd, 2], pch = 20, cex = psize2, col = colData)
  
  # ABOVE THE THRESHOLD VALUE
  # Overlay with the max colour value
  subInd2 <- sortInd[sortArg > out] # ind in 'arg' (ascending & above threshold)
  #
  points(XX[subInd2, 1], XX[subInd2, 2], pch = 20, cex = psize2, col = colPal(2)[2])
         
  # Legend (argIn and l_dec make difference from 'plotVector's legend...)
  if (if_legend==T){
    gradientLegend(c(0, max(argIn)), color = colPal(steps), nCol = steps, pos = pos_legend,
                   coords = T, pos.num = NULL, n.seg = 2,  dec = l_dec, border.col = "Black",
                   fit.margin = TRUE)
  }
}