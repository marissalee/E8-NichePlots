#ordination fxns

PC

BiPlot.VectorDF<-function(data.pts, PC){
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  datapc
  mult <- min(
    (max(data.pts[,y]) - min(data.pts[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data.pts[,x]) - min(data.pts[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  
  return(datapc)
  
}

