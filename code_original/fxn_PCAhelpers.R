 #fxn_PCAhelpers.R

#Q1 biplot based on ggbiplot(), https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r
ggbiplot_q1<- function(pcobj, df,...){
  require(ggplot2)
  require(plyr)
  require(scales)
  require(grid)
  require(reshape2)
  
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
    geom_point(aes(shape = inv), size=3, color='darkgray') +
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal() + mytheme +
    scale_shape_manual(name="Invasion status", values=shape.vec1, labels=labels.vec1)

  #Base plot with site vectors instead of plot points
  g2 <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    geom_segment(mapping=aes(x=xvar_N, y=yvar_N, xend=xvar_I, yend=yvar_I), 
                 data=df.u.castIN, arrow = arrow(length = unit(0.25, "cm")), color='darkgray') +
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal() + mytheme +
    scale_shape_manual(name="Invasion status", values=shape.vec1, labels=labels.vec1)
  
  # Add variable vectors
  pBasic <- g + 
    geom_segment(data = df.v,aes(x = 0, y = 0, xend = xvar, yend = yvar),arrow = arrow(length = unit(1/2, 'picas')), color = 'black') + 
    geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), color = 'black', size = 3)
  pBasic1 <- g1 + 
    geom_segment(data = df.v, aes(x = 0, y = 0, xend = xvar, yend = yvar), arrow = arrow(length = unit(1/2, 'picas')), color = 'black') + 
    geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), color = 'black', size = 3)
  pBasic2 <- g2 + 
    geom_segment(data = df.v, aes(x = 0, y = 0, xend = xvar, yend = yvar), arrow = arrow(length = unit(1/2, 'picas')), color = 'black') + 
    geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), color = 'black', size = 3)
  
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
  pEllipseIN1 <- pBasic1 + geom_path(data = ell, aes(linetype = inv), color='blue') +
    scale_linetype_manual(name='Invasion status', values=c(3,1), labels=c('Invaded','Reference'))
  pEllipseIN2 <- pBasic2 + geom_path(data = ell, aes(linetype = inv), color='blue') +
    scale_linetype_manual(name='Invasion status', values=c(3,1), labels=c('Invaded','Reference'))
  
  result.list<-list(pEllipseIN1=pEllipseIN1, pEllipseIN2=pEllipseIN2)
  
  return(result.list)
  
}

#Custom biplot based on ggbiplot(), https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r
ggbiplot_custom<- function(pcobj, byYear=FALSE, df=NULL,...){
  
  require(ggplot2)
  require(plyr)
  require(scales)
  require(grid)
  require(reshape2)
  
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
  df.v$varname <- rownames(v)
  
  if(byYear==TRUE){
    #annotate df.u with other identifying variables
    df.u$year <- factor(df$year)
    df.u$plotid <- df$plotid
    
    #make plot
    shape.vec<-c(16,17) #circle=2012, triangle=2013, 
    pBasic <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
      geom_point(aes(shape = year), color='darkgray') +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(1/2, 'picas')), 
                   color = 'black') + 
      geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), 
                color = 'black', size = 3) +
      xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal() +  mytheme +
      scale_color_manual(name="Year", values=shape.vec)
  }
  
  #make plot
  pBasic <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    geom_point(color='darkgray') + 
    geom_segment(data = df.v,
                 aes(x = 0, y = 0, xend = xvar, yend = yvar),
                 arrow = arrow(length = unit(1/2, 'picas')), 
                 color = 'black') + 
    geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust), 
              color = 'black', size = 3) +
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal() + mytheme
  
  return(pBasic)
  
}

#PCA.var.contrib: for q1, put together a dataframe with variables that contribute > 10% to PC1 or PC2 for a given diffVarShort
PCA.var.contrib_q1<-function(pca.obj){
  
  #extract loadings and sdev
  loadings <- pca.obj$rotation
  sdev <- pca.obj$sdev
  
  #calculate variable contributions
  var_cor_func <- function(var.loadings, comp.sdev){
    var.loadings*comp.sdev # calculate correlation between variables and principal components
  }
  contrib <- function(var.cos2, comp.cos2){
    var.cos2*100/comp.cos2 # calculate contribution of each variable to each PC (in %)
  }
  var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
  var.cos2 <- var.coord^2
  comp.cos2 <- apply(var.cos2, 2, sum)
  var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2)) #contribution of a variable to a given PC (in %)
  
  #put PC1 and PC2 info into a dataframe with loading values
  var.contrib1 <- round(var.contrib[,c('PC1','PC2')], digits=2)
  colnames(var.contrib1)<-c('PC1.contrib','PC2.contrib')
  var.loadings1 <- round(loadings[,c('PC1','PC2')], digits=2)
  colnames(var.loadings1)<-c('PC1.rot','PC2.rot')
  var.contrib.df <- data.frame(var=row.names(var.contrib), var.contrib1, var.loadings1) 
  
  #extract variables that contribute > 10% to PC1
  pc1.df<-var.contrib.df[var.contrib.df$PC1.contrib > 9.99,c('var','PC1.contrib','PC1.rot')]
  
  #extract variables that contribute > 10% to PC2
  pc2.df<-var.contrib.df[var.contrib.df$PC2.contrib > 9.99,c('var','PC2.contrib','PC2.rot')]
  
  #make a dataframe with the important vars for PC1 and PC2 only
  require(reshape2)
  PCvar<-rbind(melt(pc1.df, id.vars='var'), melt(pc2.df, id.vars='var'))
  detach(package:reshape2, TRUE)
  PCvar$PC<-NA
  PCvar[grepl("PC1", PCvar$variable),'PC']<-'PC1'
  PCvar[grepl("PC2", PCvar$variable),'PC']<-'PC2'
  PCvar$valueType<-NA
  PCvar[grepl("contrib", PCvar$variable),'valueType']<-'contrib'
  PCvar[grepl("rot", PCvar$variable),'valueType']<-'rot'
  
  return(PCvar)
  
}

#PCA.var.contrib: for q2, put together a dataframe with variables that contribute > 10% to PC1 or PC2 for a given diffVar.basic
PCA.var.contrib_q2 <- function(result.list, diffVar.basic){
  
  #extract PCA object
  pca.obj<-result[['PCA.ref']]
  
  #extract loadings and sdev
  loadings <- pca.obj$rotation
  sdev <- pca.obj$sdev
  
  #calculate variable contributions (uses helper fxns: var_cor_func and contrib)
  var_cor_func <- function(var.loadings, comp.sdev){
    var.loadings*comp.sdev # calculate correlation between variables and principal components
  }
  contrib <- function(var.cos2, comp.cos2){
    var.cos2*100/comp.cos2 # calculate contribution of each variable to each PC (in %)
  }
  var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
  var.cos2 <- var.coord^2
  comp.cos2 <- apply(var.cos2, 2, sum)
  var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2)) #contribution of a variable to a given PC (in %)
  
  #put PC1 and PC2 info into a dataframe with loading values
  var.contrib1 <- round(var.contrib[,c('PC1','PC2')], digits=2)
  colnames(var.contrib1)<-c('PC1.contrib','PC2.contrib')
  var.loadings1 <- round(loadings[,c('PC1','PC2')], digits=2)
  colnames(var.loadings1)<-c('PC1.rot','PC2.rot')
  var.contrib.df <- data.frame(var=row.names(var.contrib), var.contrib1, var.loadings1) 
  
  #extract variables that contribute > 10% to PC1
  pc1.df<-var.contrib.df[var.contrib.df$PC1.contrib > 9.99,c('var','PC1.contrib','PC1.rot')]
  
  #extract variables that contribute > 10% to PC2
  pc2.df<-var.contrib.df[var.contrib.df$PC2.contrib > 9.99,c('var','PC2.contrib','PC2.rot')]
  
  #make a dataframe with the important vars for PC1 and PC2 only
  require(reshape2)
  PCvar<-rbind(melt(pc1.df, id.vars='var'), melt(pc2.df, id.vars='var'))
  detach(package:reshape2, TRUE)
  PCvar$diffVar<-diffVar.basic
  PCvar$PC<-NA
  PCvar[grepl("PC1", PCvar$variable),'PC']<-'PC1'
  PCvar[grepl("PC2", PCvar$variable),'PC']<-'PC2'
  PCvar$valueType<-NA
  PCvar[grepl("contrib", PCvar$variable),'valueType']<-'contrib'
  PCvar[grepl("rot", PCvar$variable),'valueType']<-'rot'
  
  return(PCvar)
  
}

#RefPCA: do the reference condition ordination, removing one difference measurement type
RefPCA<-function(diffVar.basic, refVars, df, externalPercBA_AM){
  
  #identify impact measures
  select.diffVarTB<-paste('Diff',diffVar.basic, c('T','B'), sep='_')
  
  #identify variables to remove from refVars and the select.refVars and create dataframe
  excludeVars<-nVars.indx[nVars.indx$basic.nVars == diffVar.basic,'nVars']
  select.refVars<-refVars[!refVars %in% excludeVars]
  df.ref<-data.frame(df[,select.refVars], year=df$year)
  rownames(df.ref)<-df$plotYear
  df.ref.rm<-df.ref[!rowSums(is.na(df.ref)),]
  
  #ordinate select.refVars (without the year column)
  tmp<-df.ref.rm[,!(colnames(df.ref.rm) == 'year')]
  tmp.scaled<-scale(tmp)
  PCA.ref<-prcomp(tmp.scaled)
  pPCA.ref<-ggbiplot_custom(PCA.ref) + mytheme
  
  #identify model dataframe
  df.PCA.ref<-data.frame(plotYear=rownames(PCA.ref$x), PCA.ref$x)
  df.sub<-data.frame(plotYear=df$plotYear, 
                     year=df$year, 
                     select.diffVarT=df[,select.diffVarTB[1]], 
                     select.diffVarB=df[,select.diffVarTB[2]], 
                     mv_g.m2_logt=df$mv_g.m2_logt)
  if(externalPercBA_AM=='y'){
    df.sub<-data.frame(df.sub, MycBin=df$MycBin)
  }
  df.mod<-merge(df.sub, df.PCA.ref[,c(1:3)])
  
  #save everything in a list
  results<-list(select.diffVarTB=select.diffVarTB, select.refVars=select.refVars, 
                PCA.ref=PCA.ref, pPCA.ref=pPCA.ref, 
                df.mod=df.mod)
  
  return(results)
  
}

