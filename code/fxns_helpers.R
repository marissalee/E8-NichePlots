
calc_varloads <- function(mod.obj){
  
  #extract loadings and sdev
  loadings <- mod.obj$rotation
  loadings.df <- data.frame(variable = row.names(loadings), loadings[,c("PC1","PC2")], type = rep("loadings", dim(loadings)[1]))
  sdev <- mod.obj$sdev
  
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
  contrib.df <- data.frame(variable = row.names(var.contrib), var.contrib[,c("PC1","PC2")], type = rep("contrib", dim(var.contrib)[1]))
  
  # create df of variable loadings and % contributions
  varloads <- rbind(loadings.df, contrib.df)
  varloads %>%
    gather(key = "PC", value = "value", -c(variable,type)) %>%
    spread(key = type, value = value) %>%
    arrange(PC, contrib) -> varloads
  
}

list_to_df <- function(mylist){
  
  # make a vector of row ids that correspond to the list names
  rowid.indx <- lapply(mylist, function(x) dim(x)[1])
  sourceVec.list <- list()
  for(i in 1:length(rowid.indx)){
    sourceName <- names(rowid.indx)[i]
    numRows <- rowid.indx[[i]]
    sourceVec.list[[i]] <- rep(sourceName, numRows)
  }
  rowVec <- unlist(sourceVec.list)
  
  # combine into df
  df <- data.frame(do.call(rbind, mylist))
  df$source <- rowVec
  
  return(df)
}
