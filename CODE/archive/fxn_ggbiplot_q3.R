#fxn_ggbiplot_q3.R


#based on ggbiplot()
#https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r

#Q3 biplot
ggbiplot_q3<- function(pcobj, df=NULL,...){
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  library(reshape2)
  
  choices=1:2
  scale=1
  pc.biplot=TRUE
  obs.scale=1 - scale
  var.scale=scale
  ellipse.prob=0.68
  circle=FALSE
  circle.prob=0.69
  varname.adjust=1.5
  
  
  # Recover the SVD
  nobs.factor <- sqrt(nrow(pcobj$x) - 1)
  d <- pcobj$sdev
  u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
  v <- pcobj$rotation
  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))
  names(df.u) <- c('xvar', 'yvar')
  df.u <- df.u * nobs.factor
  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])
  names(df.v) <- names(df.u)
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  
  # Change the labels for the axes
  u.axis.labs <- paste(paste('PC', choices, sep=''), 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  
  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  #other vars
  df.u$year <- factor(df$year)
  df.u$plotid <- df$plotid
  df.v$varname <- rownames(v)
  
  # Base plot
  shape.vec<-c(16,17) #circle=2012, triangle=2013, 
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal() +
    geom_point(aes(shape = year), color='darkgray') + mytheme +
    scale_color_manual(name="Year", values=shape.vec) 
  g
  
  # Add vectors
  pBasic <- g + geom_segment(data = df.v,
                             aes(x = 0, y = 0, xend = xvar, yend = yvar),
                             arrow = arrow(length = unit(1/2, 'picas')), 
                             color = 'black') + 
    geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), 
              color = 'black', size = 3) +
    xlim(c(-2, 2.5)) + ylim(c(-2.5, 2.5))
  pBasic
  
  result.list<-list(pBasic=pBasic)
  
  return(result.list)
  
}




