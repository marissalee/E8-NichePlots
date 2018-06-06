#ggbiplot2
#https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r

# #testing params
# df=data.vars.PCA
# pcobj=PCA.res
# year=as.factor(data.vars.PCA$year)
# inv=data.vars.PCA$inv
# plotid=data.vars.PCA$plotid
# 
# ggbiplot_q1(pcobj=PCA.res, df=data.vars.PCA)

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
  df.u$plotid <- df$plotid
  df.v$varname <- rownames(v)
  
  df.u.melted<-melt(df.u, measure.vars = c('xvar','yvar'))
  df.u.castIN<-dcast(df.u.melted, plotid+year~variable+inv)
  df.u.castYr<-dcast(df.u.melted, plotid+inv~variable+year)
  
  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal() +
    geom_point(aes(color = year, shape = inv), size=3) + mytheme +
    scale_color_manual(name="Year", values=c('black','darkblue')) + 
    scale_shape_manual(name='Invasion', values=c(1,16))
  
  # Add vectors
  pBasic <- g + geom_segment(data = df.v,
                             aes(x = 0, y = 0, xend = xvar, yend = yvar),
                             arrow = arrow(length = unit(1/2, 'picas')), 
                             color = 'darkred') + 
    geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), color = 'darkred', size = 4)
  pBasic
  
  # Add arrows from paired N to I
  pPairedIN<-g + geom_segment(data=df.u.castIN, mapping=aes(x=xvar_N, y=yvar_N, xend=xvar_I, yend=yvar_I, color=year), arrow=arrow(), size=0.5) 
  pPairedIN 
  
  # Add arrows from paired 2012 to 2013
  pPairedYr<-g + geom_segment(data=df.u.castYr, mapping=aes(x=xvar_2012, y=yvar_2012, xend=xvar_2013, yend=yvar_2013, linetype=inv), arrow=arrow(), size=0.5) +
    scale_linetype_manual(name='Invasion', values=c(2,1))
  pPairedYr
  
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
  pEllipseIN <- pBasic + geom_path(data = ell, aes(linetype = inv)) +
    scale_linetype_manual(name='Invasion', values=c(2,1))
  pEllipseIN 
  
  # Overlay a concentration ellipse for year
  ell <- ddply(df.u, 'year', function(x) {
    sigma <- var(cbind(x$xvar, x$yvar))
    mu <- c(mean(x$xvar), mean(x$yvar))
    ed <- sqrt(qchisq(ellipse.prob, df = 2))
    data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), year = x$year[1])
  })
  names(ell)[c(1:2)] <- c('xvar', 'yvar')
  pEllipseYr <- pBasic + geom_path(data = ell, aes(color = year))
  pEllipseYr
  
  result.list<-list(pBasic=pBasic, pPairedIN=pPairedIN, pPairedYr=pPairedYr, pEllipseIN=pEllipseIN, pEllipseYr=pEllipseYr)
  
  return(result.list)
  
}


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
  df.u$mv_g.m2_logt <- df$mv_g.m2_logt
  df.u$plotid <- df$plotid
  df.v$varname <- rownames(v)
  
  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal() +
    geom_point(aes(color = year, size=mv_g.m2_logt)) + mytheme +
    scale_color_manual(name="Year", values=c('black','darkblue')) +
    scale_size_continuous(name="M.v. biomass")
  g
  
  # Add vectors
  pBasic <- g + geom_segment(data = df.v,
                             aes(x = 0, y = 0, xend = xvar, yend = yvar),
                             arrow = arrow(length = unit(1/2, 'picas')), 
                             color = 'darkred') + 
    geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), 
              color = 'darkred', size = 4) +
    xlim(c(-2, 2.5)) + ylim(c(-2.5, 2.5))
  pBasic
  
  result.list<-list(pBasic=pBasic)
  
  return(result.list)
  
}




