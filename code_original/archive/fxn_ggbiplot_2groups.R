#ggbiplot2
#https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r

# My modification of ggbiplot so that group can be continuous and modify the point size

#testing params
# data(wine)
# wine.pca <- prcomp(wine, scale. = TRUE)
# pcobj<-wine.pca
# continuousGrp<-seq(from=1,to=dim(wine)[1])
# categoricalGrp<-c(rep('green', dim(wine)[1]-50), rep('blue', 50))
#print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE))

ggbiplot_ptsize <- function(pcobj, groupColor =NULL, groupSize=NULL, 
                            ellipse=FALSE, ellipse.prob = 0.68,
                            varname.size=3, varname.adjust=1.5,...){
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  
  choices <- 1:2
  scale <- 1
  pc.biplot <- TRUE
  obs.scale <- 1 - scale
  var.scale <- scale
  ellipse.prob <- 0.68
  circle = FALSE
  circle.prob = 0.69
  
  # Recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } 
  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))
  names(df.u) <- c('xvar', 'yvar')
  df.u <- df.u * nobs.factor
  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])
  names(df.v) <- names(df.u)
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  
  # Change the labels for the axes
  u.axis.labs <- paste('PC', choices, sep='')
  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  
  df.u$groupColor <- groupColor
  df.u$groupSize <- groupSize
  df.v$varname <- rownames(v)
  
  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal() +
    geom_segment(data = df.v,
                 aes(x = 0, y = 0, xend = xvar, yend = yvar),
                 arrow = arrow(length = unit(1/2, 'picas')), 
                 color = muted('red')) +
    geom_point(aes(color = groupColor, size=groupSize))
  
  # Overlay a concentration ellipse
  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- cbind(cos(theta), sin(theta))
  ell <- ddply(df.u, 'groupColor', function(x) {
    sigma <- var(cbind(x$xvar, x$yvar))
    mu <- c(mean(x$xvar), mean(x$yvar))
    ed <- sqrt(qchisq(ellipse.prob, df = 2))
    data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), groupColor = x$groupColor[1])
  })
  names(ell)[c(1:2)] <- c('xvar', 'yvar')
  g <- g + geom_path(data = ell, aes(color = groupColor))
  
  # Label the variable axes
  g <- g + geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), color = 'darkred', size = varname.size)
  
  return(g)
}