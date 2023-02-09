#---------Function 'd_plot'-----------
# Separate the top-level plot for readability
# Default parameters set for EE density plot
d_plot <- function(data = as.data.frame(XX), dim1 = 1, dim2 = 2, 
                   xlim = c(-5.5, 6.5), ylim = c(-6.5, 4.5), break_seq = seq(-6, 6, 2),
                   fix_aspect_ratio = T, fsize = 15){
  
  d <- ggplot(data, aes(x = data[,dim1], y = data[,dim2])) + 
    {if(fix_aspect_ratio == T) coord_fixed()} + 
    xlab('Dim 1') + ylab('Dim 2') +
    scale_x_continuous(limits = xlim, breaks = break_seq) +
    scale_y_continuous(limits = ylim, breaks = break_seq) +
    theme(panel.background = element_blank(), text = element_text(size = fsize),
          axis.line = element_line(colour = 'black', size = 1),
          axis.ticks = element_line(colour = "black", size = 1),
          axis.ticks.length = unit(.5, "cm"))
  return(d)
}

#---------Function 'calc_h'-----------
# Calculate bandwidth (of a Gaussian kernel density estimator)
calc_h <- function(dataset, dim1, dim2){
  h <- c(MASS::width.SJ(dataset[,dim1]), MASS::width.SJ(dataset[,dim2]))
  return(h)
}

#---------Function 'plotContour'-----------
# Global var: mypalette
plotContour <- function(plot0, contVar, hvalue = c(1,1), binNum = 12, 
                        alpha1 = .8, alpha2 = 0, psize = 1, if_legend = F){
  # contVar: 'density', 'ndensity' or 'count'
  # alpha1:(0.7-1); alpha2:(0,1)
  # alpha1 = .8; transparent enough to see individual dots
  # alpha2 = 0; completely transparent (non-visible) contour lines
  
  thePlot <- plot0 +  
    # Plot points, add title naming the variables
    geom_point(cex = psize) + 
    ggtitle(paste0(contVar, "; h = ", round(hvalue, 2)[[1]])) +
    
    
    # Overlay the contour plot
    stat_density2d_filled(contour_var = contVar, h = hvalue, bins = binNum,
                          #aes(fill = stat(nlevel)), 
                          alpha = alpha1, show.legend = if_legend) + # legend!
    
    # Re-colour the contours
    scale_fill_manual(values = mypalette) +
    
    # Overlay contour lines
    stat_density2d(contour_var = contVar, h = hvalue, bins = binNum,
                   size = .5, colour = "black", alpha = alpha2) +
    
    # Legend - export this once in a larger size (if_legend TRUE)
    theme(legend.text = element_text(size = 6), legend.key.size = unit(0.4, "cm"))
  
  return(thePlot)
}

#---------Function 'plotContour_pca'-----------
# Optimized for plotting PCA density plots
# (change in default parameters, which are set for EE density plot)
plotContour_pca <- function(contVar, h_value = c(10, 10), dim_2, y_lim, 
                            p_size = 1, f_size = 10){
  # Top level plot
  d_pca <- d_plot(data = as.data.frame(pca$x), dim1 = 1, dim2 = dim_2,
                  xlim = c(-50, 70), ylim = y_lim, break_seq = seq(-60, 60, 20),
                  fix_aspect_ratio = F, fsize = f_size)
  
  # With points and contours
  thePlot <- plotContour(d_pca, contVar, hvalue = h_value, binNum = 12,
                         alpha1 = .8, alpha2 = 0, psize = p_size)
  return(thePlot)
}
