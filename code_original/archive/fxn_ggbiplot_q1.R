#fxn_ggbiplot_q1.R

#based on ggbiplot()
#https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r

#Q1 biplot
ggbiplot_q1<- function(pcobj, df=NULL,...){
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
  df.u$year <- df$year
  df.u$inv <- df$inv
  df.u$invYear <- paste(df$inv, df$year, sep="_")
  df.u$plotid <- df$plotid
  df.v$varname <- rownames(v)
  
  df.u.melted<-melt(df.u, measure.vars = c('xvar','yvar'))
  df.u.castIN<-dcast(df.u.melted, plotid+year~variable+inv)
  df.u.castYr<-dcast(df.u.melted, plotid+inv~variable+year)
  
  # Base plot
  shape.vec<-c(16,17,1,2) #circle=2012, triangle=2013, reference=closed, invaded=open
  labels.vec<-c('Invaded, 2012','Invaded, 2013','Reference, 2012','Reference, 2013')
  shape.vec1<-c(1,16) #reference=closed, invaded=open
  labels.vec1<-c('Invaded','Reference')
  
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal() +
    geom_point(aes(shape = invYear), size=3, color='darkgray') + mytheme +
    scale_shape_manual(name="Invasion status & year", values=shape.vec, labels=labels.vec)
  #Base plot without points indicating year
  g1 <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal() +
    geom_point(aes(shape = inv), size=3, color='darkgray') + mytheme +
    scale_shape_manual(name="Invasion status", values=shape.vec1, labels=labels.vec1)
  
  # Add vectors
  pBasic <- g + geom_segment(data = df.v,
                             aes(x = 0, y = 0, xend = xvar, yend = yvar),
                             arrow = arrow(length = unit(1/2, 'picas')), 
                             color = 'black') + 
    geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), 
              color = 'black', size = 3)
  # Add vectors to g1
  pBasic1 <- g1 + geom_segment(data = df.v,
                             aes(x = 0, y = 0, xend = xvar, yend = yvar),
                             arrow = arrow(length = unit(1/2, 'picas')), 
                             color = 'black') + 
    geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), 
              color = 'black', size = 3)
  
  # Overlay a concentration ellipse for inv
  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- cbind(cos(theta), sin(theta))
  ell <- ddply(df.u, 'inv', function(x) {
    sigma <- var(cbind(x$xvar, x$yvar))
    mu <- c(mean(x$xvar), mean(x$yvar))
    ed <- sqrt(qchisq(ellipse.prob, df = 2))
    data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), inv = x$inv[1])
  })
  names(ell)[c(1:2)] <- c('xvar', 'yvar')
  pEllipseIN <- pBasic + geom_path(data = ell, aes(linetype = inv), color='blue') +
    scale_linetype_manual(name='Invasion status', values=c(3,1), labels=c('Invaded','Reference'))
  # Overlay a concentration ellipse for inv onto pBasic2
  pEllipseIN1 <- pBasic1 + geom_path(data = ell, aes(linetype = inv), color='blue') +
    scale_linetype_manual(name='Invasion status', values=c(3,1), labels=c('Invaded','Reference'))
  pEllipseIN1
  
  result.list<-list(pBasic=pBasic, pEllipseIN=pEllipseIN, pEllipseIN1=pEllipseIN1)
  
  return(result.list)
  
}


