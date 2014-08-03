#leeFunctions.R
#Updated 5/09/14
######################################################

######################################################
#RegressPlots2
######################################################
#What it does: 
#(a) For a set of subjects (plots), it makes a scatterplot with the subject names as the points... for 2 groups of subjects, e.g. plots in 2012 and 2013
#(b) Runs a linear fit
#What it needs: 
#(1) var.name = column name that identifies the y variable in the dataset d
#(2) var.lab = name for plotting
#(3) x.name = column name that identifies the y variable in the dataset d
#(4) x.lab = name for plotting
#(5) subj.name = column name that identifies the plot names in the dataset d
#(6) d0 = dataset
#(7) subset.name = name of the variable by which the data has already been subsetted (for labeling the plot)
#What it produces: Scatterplot with plotnames as the 'points', linear fit through them
RegressPlots2<-function(var.name, var.lab, x.name, x.lab, subj.name, d0, subsetcol, subset.name){
  #define parameters
  var.data<-d0[,var.name] 
  x.data<-d0[,x.name] 
  plotnames<-d0[,subj.name]
  subset<-d0[,subsetcol]
  d1<-data.frame(plotnames, x.data, var.data, subset)
  u.sub<-unique(d1$subset)
  colorlist<-c('blue','green')
  
  #plot
  plot(var.data~x.data,data=d1, xlab=x.lab, ylab=var.lab, type="n")
  i<-0
  for(i in 1:length(u.sub)){
    text(x=d1[d1$subset==u.sub[i],'x.data'], 
         y=d1[d1$subset==u.sub[i],'var.data'], 
         d1$plotnames, col=colorlist[i],cex=.7)
  }
  
  #linear fit
  fit<-lm(var.data~x.data, data=d1)
  abline(fit, lty=2)
  
  #labels
  title<-paste(subset.name,' \n',length(plotnames),'plots,')
  meastitle<-var.lab
  coeftab<-LmFitTab(fit)
  r2<-coeftab[3,1]
  m<-coeftab[2,1]
  pval<-coeftab[2,4]
  statstitle<-paste('\n','r2 =',r2,', m =',m,', p-value =',pval)
  mtext(paste(title,meastitle,statstitle), side=3)
  
  return(coeftab)
}

######################################################
#RegressPlots
######################################################
#What it does: 
#(a) For a set of subjects (plots), it makes a scatterplot with the subject names as the points
#(b) Runs a linear fit
#What it needs: 
#(1) var.name = column name that identifies the y variable in the dataset d
#(2) var.lab = name for plotting
#(3) x.name = column name that identifies the y variable in the dataset d
#(4) x.lab = name for plotting
#(5) subj.name = column name that identifies the plot names in the dataset d
#(6) d0 = dataset
#(7) subset.name = name of the variable by which the data has already been subsetted (for labeling the plot)
#What it produces: Scatterplot with plotnames as the 'points', linear fit through them

RegressPlots<-function(var.name, var.lab, x.name, x.lab, subj.name, d0, subset.name){
  #define parameters
  var.data<-d0[,var.name] 
  x.data<-d0[,x.name] 
  plotnames<-d0[,subj.name]
  d1<-data.frame(plotnames, x.data, var.data)
  
  #plot
  plot(var.data~x.data,data=d1, xlab=x.lab, ylab=var.lab, type="n")
  text(x=d1$x.data, y=d1$var.data, d1$plotnames, col="red",cex=.7)
  
  #linear fit
  fit<-lm(var.data~x.data, data=d1)
  abline(fit, lty=2)
  
  #labels
  title<-paste(subset.name,' \n',length(plotnames),'plots,')
  meastitle<-var.lab
  coeftab<-LmFitTab(fit)
  r2<-coeftab[3,1]
  m<-coeftab[2,1]
  pval<-coeftab[2,4]
  statstitle<-paste('\n','r2 =',r2,', m =',m,', p-value =',pval)
  mtext(paste(title,meastitle,statstitle), side=3)
  
  return(coeftab)
}

######################################################
#PlotPlots
######################################################
#What it does: 
#(a) For a set of measurements on a set of subjects (plots), it plots the measurement value against the ordered subjects (plotnames); each measurement has its own plot
#(b) Table for each measurement
#What it needs: 
#(1) d = data
#(2) measvec = vector of column numbers that identify the measurments of interest
#(3) subsetcol = column name of the column that you'd like to subset by (e.g. 'year') 
#(4) subsetvar = variable with in the column that you'd like to subset (e.g. 2011)
#(5) subjcol = column name of the column that holds subject names (e.g. 'plotname')
#What it produces: Plots, List of dataframes

PlotPlots<-function(d,measvec,subsetcol,subsetvar,subjcol, saveplot, width, height){
  #1 subset data by year
  dtmp<-d[d[,subsetcol]==subsetvar,] #subset by a variable (subsetvar) in a column (subsetcol)
  measvec.names<-colnames(d)[measvec]
  
  #loop through each measurement
  m<-0
  Tables<-list()
  par(mar=c(4,4,1,1)+0.1)
  for(m in 1:length(measvec)){ 
    
    ynum<-measvec[m] #pull out 1 measurement
    
    if(sum(!is.na(dtmp[,ynum]))>0){ # if there is data in that measurement column...
      
      if(saveplot==T){SavePlot(name=paste(subsetvar,measvec.names[m],sep='_'), width=width, height=height)}
      
      #order a table for that measurement
      tab<-data.frame(plotid=dtmp[,'plotid'],plotname=dtmp[,'plotname'],val=dtmp[,ynum], stringsAsFactors=F)
      tabo<-orderBy(~val,tab)
      
      #plot
      x<-tabo$plotname
      y<-tabo$val
      ATx<-1:12
      plot(y~ATx, xlab='Plot Name', ylab=paste(measvec.names[m], subsetvar,sep="  "), xaxt='n', pch=16)
      axis(1, at=ATx, labels=x, cex.axis=0.8)
      abline(h=0, lty=2)
      
      if(saveplot==T){dev.off()} #remember to finish the save plot by turning off the device
      
    } else { # if there is not data....
      tabo<-'No data'
    }
    
    #save table
    Tables[[m]]<-tabo
    
  }
  names(Tables)<-measvec.names
  
  return(Tables)
}

######################################################
#MinMax3byMeas
######################################################
#What it does: summarizes a dataset by creating a table with the max 3 and min 3 subjects and their values for each measurement column
#What it needs: 
#(1) d = data
#(2) measvec = vector of column numbers that identify the measurments of interest to summarize
#(3) subsetcol = column name of the column that you'd like to subset by (e.g. year) 
#(4) subsetvar = variable with in the column that you'd like to subset (e.g. 2011)
#What it produces: a table of mins and maxs

MinMax3byMeas<-function(d, measvec, subsetcol,subsetvar){
  
  dtmp<-d[d[,subsetcol]==subsetvar,] #subset by a variable (subsetvar) in a column (subsetcol)
  measvec.names<-colnames(d)[measvec]
  stat<-c('min1','min2','min3','max1','max2','max3')
  
  m<-0
  stored<-numeric(0)
  for(m in 1:length(measvec)){ #loop through each measurement
    
    #subset the current measurement
    ynum<-measvec[m]
    vec.measname<-rep(measvec.names[m],6) 
    
    #if there are values in this measurement column...
    if(sum(!is.na(dtmp[,ynum]))>0){ 
      
      #4a. Find 3 max values
      #max1
      ax1<-MinoMax(dtmp, ynum, 'plotname','max')
      row1<-unlist(ax1[1])
      #max2
      dtmp1<-dtmp[-row1,] # remove max1 row
      ax2<-MinoMax(dtmp1, ynum, 'plotname','max')
      row2<-unlist(ax2[1])
      #max3
      dtmp2<-dtmp1[-row2,] # remove max1 row
      ax3<-MinoMax(dtmp2, ynum, 'plotname','max')
      
      #4b. Find 3 min values
      #min1
      in1<-MinoMax(dtmp, ynum, 'plotname','min')
      row1<-unlist(in1[1])
      #min2
      dtmp1<-dtmp[-row1,] # remove max1 row
      in2<-MinoMax(dtmp1, ynum, 'plotname','min')
      row2<-unlist(in2[1])
      #min3
      dtmp2<-dtmp1[-row2,] # remove max1 row
      in3<-MinoMax(dtmp2, ynum, 'plotname','min')   
      
      #check to make sure that ax and in object have only 1 row
      tmplist<-list(in1,in2,in3,ax1,ax2,ax3)
      tmplist2<-list()
      i<-0
      for (i in 1:6){
        if(dim(tmplist[[i]])[1]>1){ #if there is more than one row...
          tmplist2[[i]]<-tmplist[[i]][1,] # get rid of the extra rows because each row should have the concatinated plot and val info
        } else { #if there is one row then just replace with that row
          tmplist2[[i]]<-tmplist[[i]]
        }
      }
      
      #4c. Store everything
      store1<-data.frame(matrix(unlist(tmplist2), nrow=length(tmplist2), byrow=T))
      names(store1)<-c('rows','plots','vals')
      vec.plots<-store1[,'plots']
      vec.vals<-store1[,'vals']
      
    } else {#5. if there are no values in the measurement column...
      #5a. Create empty plot and val vectors
      vec.plots<-as.character(rep('none',6))
      vec.vals<-as.numeric(rep(NA,6))
    } 
    
    #6. bind all the vectors for that year and measurement: vec.year, vec.measname, plots, vals
    store<-data.frame(vec.measname,stat,vec.plots,vec.vals)
    store
    stored<-rbind(stored,store)
    
  }
  
  return(stored) 
  
}

######################################################
#MinoMax
######################################################
#What it does: calculates min or max of a set of data
#What it needs: 
#(1) data
#(2) ycolnum
#(3) subjcolnum
#(4) minomax = 'min' or 'max
#What it produces: a dotplot calculated by omitting missing data, a table of important values

MinoMax<-function(data,ycolnum,subjcolnum,minomax){
  
  if(minomax=='min'){
    
    rowmin1<-which(data[,ycolnum]==min(data[,ycolnum], na.rm=T)) #calculate min
    
    if(length(rowmin1)==1){ # if there is one value
      min1<-round(data[rowmin1,ycolnum],4)
      plot.min1<-data[rowmin1,subjcolnum] 
    } else { # if there is a tie, then have them all concatinated into a string
      min1<-paste(round(data[rowmin1,ycolnum],4),collapse="_")
      plot.min1<-paste(data[rowmin1,subjcolnum], collapse="_")
    }
    
    result<-data.frame(rownum=rowmin1,plot=plot.min1,val=min1, stringsAsFactors=F)
    
  }
  
  if(minomax=='max'){
    
    rowmax1<-which(data[,ycolnum]==max(data[,ycolnum], na.rm=T)) #calculate max
    
    if(length(rowmax1)==1){ # if there is one value
      max1<-round(data[rowmax1,ycolnum],4)
      plot.max1<-data[rowmax1,subjcolnum] 
    } else { # if there is a tie, then have them all concatinated into a string
      max1<-paste(round(data[rowmax1,ycolnum],4),collapse="_")
      plot.max1<-paste(data[rowmax1,subjcolnum], collapse="_")
    }
    
    result<-data.frame(rownum=rowmax1,plot=plot.max1,val=max1, stringsAsFactors=F)
    
  }
  return(result)
}

######################################################
#RF.omit - be careful, NaNs in importance may be due to levels that do not get dropped
######################################################
#What it does: runs a randomForest (omits NAs in dataset)
#What it needs: 
#(1) y
#(2) xvars
#(3) year
#(4) copyplot
#(5) filename
#(6) width and heigth = plot dimensions, ~400
#What it produces: a dotplot calculated by omitting missing data, a table of important values

RF.omit<-function(y, xs, year, yname, copyplot, filename, width, height){
  d<-cbind(y,xs)
  do<-na.omit(d) #omit missing values
  
  #set up parameters
  nrows<-dim(do)[1]
  finalxcol<-dim(do)[2]
  yt<-do$y
  xst<-do[,2:finalxcol]
  omit.importance<-numeric(0)
  
  #run randomForest
  set.seed(10)
  t<-randomForest(x=xst, y=yt, importance=T)
  
  #plot
  cutoff<-abs(min(t$importance[,1]))
  dotchart((t$importance[,1]), xlab="%IncMSE", pch=16, cex=.7, main=paste("RandomForest for \n", yname, year,"\n NAs-omitted; n =",nrows))
  abline(v=cutoff, col="red",lty="longdash", lwd=2)
  abline(v=0, col="blue")
  
  #copy plot
  if(copyplot==T){CopyPlot(name=paste("rf",year,yname,filename,sep='_'), width=width, height=height)}
  
  #save list of importance values
  omit.importance<-t$importance
  importance<-omit.importance
  
  return(importance)
}

######################################################
#RFys.impute.omit - be careful, NaNs in importance may be due to levels that do not get dropped
######################################################
#What it does: runs a randomForest using multiple yvars, each with their own set of xvars
#this is meant to run from different year datasets and different response variables, hence the 'year' and 'yname' parameters
#What it needs: 
#(1) data
#(2) yvars
#(3) xvars
#(4) year
#(5) copyplot
#(6) filename
#(7) width and heigth = plot dimensions, ~400
#What it produces: two panels of dotplots for each yvar, one dotplot is calculated based on inferring missing data, the other dotplot is calculated by omitting missing data

RFys.impute.omit<-function(data, yvars, xvars, year, copyplot, filename, width, height){
  
  #for each yvar, make a subset of xvars that is appropriate (aka, doesn't include the predictor variable)
  xvarsets<-list()
  i<-0
  for (i in 1:length(yvars)){ #loop through all yvars
    yvar<-yvars[i]
    ystring<-unlist(strsplit(yvar,"_"))[2]
    watchout<-c("nhi","noi","toti","ammonifd","nitrifd","minzd")
    
    if (ystring == "nhi" | ystring == "noi"){
      excludetoti<-grep('toti',xvars)
      excludexs<-grep(ystring, xvars)
      new.xvars<-xvars[-c(excludetoti,excludexs)]
    }
    
    if(ystring == "ammonifd" | ystring == "nitrifd"){
      excludeminz<-grep('minz',xvars)
      excludexs<-grep(ystring, xvars)
      new.xvars<-xvars[-c(excludeminz,excludexs)]
    }
    
    if(ystring == "toti"){
      excludenh<-grep('nhi',xvars)
      excludeno<-grep('noi',xvars)
      excludexs<-grep(ystring, xvars)
      new.xvars<-xvars[-c(excludenh,excludeno,excludexs)]
    }
    
    if(ystring == "minzd"){
      excludeammon<-grep('ammonifd',xvars)
      excludenitrif<-grep('nitrifd',xvars)
      excludexs<-grep(ystring, xvars)
      new.xvars<-xvars[-c(excludeammon,excludenitrif,excludexs)]
    }
    
    if(!ystring %in% watchout){
      excludexs<-grep(ystring, xvars)
      new.xvars<-xvars[-excludexs]
    }
    
    xvarsets[[i]]<-new.xvars
  }
  names(xvarsets)<-paste("for_",yvars,sep="")
  
  # for each yvar, make a random forest using the corresponding xvarset and plot w/ and w/o interpolating missing xvar data
  ilist<-list()
  i<-0
  for (i in 1:length(yvars)){ #loop through each predictor variable (yvars)
    y<-data[,yvars[i]] # current y based on i counter through yvars
    xs<-data[,xvarsets[[i]]] #current xs based on i counter through xvarsets
    d<-cbind(y,xs)
    if(sum(is.na(d$y))>0){
      d<-d[!is.na(d$y),]
      y<-d$y
      xs<-d[,xvarsets[[i]]]
    }
    finalxcol<-dim(d)[2]
    yname<-yvars[i]
    xnames<-colnames(xs)
    nrows<-dim(d)[1]
    
    par(mfrow=c(1,2))
    
    #impute missing values
    imputed.importance<-numeric(0)
    set.seed(10)
    imputed.d<-rfImpute(x=xs, y=y, iter=5, ntree=300)
    set.seed(10)
    t<-randomForest(x=imputed.d[,2:finalxcol], y=imputed.d[,1], data=imputed.d, importance=T)
    dotchart((t$importance[,1]), xlab="%IncMSE", pch=16, cex=.7, main=paste("RandomForest for \n", yname, year, "\nNAs-imputed; n =",nrows))
    abline(v=abs(min(t$importance[,1])), col="red",lty="longdash", lwd=2)
    abline(v=0, col="blue")
    imputed.importance<-t$importance[,1]
    
    #omit missing values
    omit.importance<-numeric(0)
    omit.d<-na.omit(d)
    nrows<-dim(omit.d)[1]
    finalxcol<-dim(omit.d)[2]
    set.seed(10)
    t<-randomForest(x=omit.d[,2:finalxcol], y=omit.d[,1], data=omit.d, importance=T)
    dotchart((t$importance[,1]), xlab="%IncMSE", pch=16, cex=.7, main=paste("RandomForest for \n", yname, year,"\n NAs-omitted; n =",nrows))
    abline(v=abs(min(t$importance[,1])), col="red",lty="longdash", lwd=2)
    abline(v=0, col="blue")
    omit.importance<-t$importance[,1]
    
    #copy plot
    if(copyplot==T){CopyPlot(name=paste("rf",year,yname,filename,sep='_'),width=width,height=height)}
    
    #save lists
    importance<-list(imputed=imputed.importance,omited=omit.importance) #inner list of imputed and omited variable importances
    ilist[[i]]<-importance #outer list that combines each inner list from each iteration of i (yvar)

  }
  
  names(ilist)<-yvars
  return(ilist)
}

######################################################
#RF.impute.omit - be careful, NaNs in importance may be due to levels that do not get dropped
######################################################
#What it does: runs a randomForest using y and xs vars
#this is mean to run from different year datasets and different response variables, hence the 'year' and 'yname' parameters
#What it needs: 
#(1) y
#(2) xs
#(3) year
#(4) yname
#(5) copyplot
#(6) filename
#(7) width and height = copyplot dimensions, ~400
#What it produces: two panels of dotplots, one was calculated based on inferring missing data, other calculated by omitting missing data

RF.impute.omit<-function(y, xs, year, yname, copyplot, filename, width, height){
  d<-cbind(y,xs)
  nrows<-dim(d)[1]
  finalxcol<-dim(d)[2]
  
  par(mfrow=c(1,2))
  
  #impute missing values
  imputed.importance<-numeric(0)
  set.seed(10)
  imputed.d<-rfImpute(x=xs, y=y, iter=5, ntree=300)
  set.seed(10)
  t<-randomForest(x=imputed.d[,2:finalxcol], y=imputed.d[,1], importance=T)
  cutoff<-abs(min(t$importance[,1]))
  dotchart((t$importance[,1]), xlab="%IncMSE", pch=16, cex=.7, main=paste("RandomForest for \n", yname, year, "\nNAs-imputed; n =",nrows))
  abline(v=cutoff, col="red",lty="longdash", lwd=2)
  abline(v=0, col="blue")
  imputed.importance<-t$importance[,1]
  
  #omit missing values
  omit.importance<-numeric(0)
  omit.d<-na.omit(d)
  nrows<-dim(omit.d)[1]
  finalxcol<-dim(omit.d)[2]
  set.seed(10)
  xs.om<-omit.d[,2:finalxcol]
  y.om<-omit.d[,1]
  t<-randomForest(x=xs.om, y=y.om, importance=T)
  cutoff<-abs(min(t$importance[,1]))
  dotchart((t$importance[,1]), xlab="%IncMSE", pch=16, cex=.7, main=paste("RandomForest for \n", yname, year,"\n NAs-omitted; n =",nrows))
  abline(v=cutoff, col="red",lty="longdash", lwd=2)
  abline(v=0, col="blue")
  omit.importance<-t$importance[,1]
  
  #copy plot
  if(copyplot==T){CopyPlot(name=paste("rf",year,yname,filename,sep='_'), width=width, height=height)}
  
  #save list of important variables
  importance<-list(imputed=imputed.importance,omited=omit.importance)
  
  return(importance)
  
}

######################################################
#RegresTests
######################################################
#What it does: Spits out slopes and pvals for linear fits between one y and many xs (all numeric)
#What it needs: 
#(1) y
#(2) xs
#What it produces: pvaltab = table of pvals that correspond to each y by x comparison

RegresTests<-function(y,xs){
  
  #identify all of the parts
  num.x<-length(colnames(xs)) #number of y vars
  responsevars<-colnames(xs)
  
  #initialize loop items
  tabs<-list()
  slopes<-numeric(0)
  pvals<-numeric(0)
  r2s<-numeric(0)
  i<-0 
  
  #determine the role of factor X on each Y
  for (i in 1:num.x){ #loop through each y

    currentx<-xs[,i] #pull out the current x column
    df<-data.frame(currentx, y)

    if(sum(!is.na(currentx))>0){ #if there is any data in the column at all....
      
      #Linear fit
     mod<-lm(y~currentx, data=df)
     plot(resid(mod) ~ fitted(mod), main=responsevars[i]); abline(h = 0)
     tab<-LmFitTab(mod)
     slope<-tab[2,1]
     pval<-tab[2,4]
     r2<-tab[3,1]
      
    } else if(sum(!is.na(currenty))==0){ #if there is no data in the column at all...
      tab<-NA
      slope<-NA
      pval<-NA
      r2<-NA
    } 
    
    #save table and values at the end of each loop
    tabs[[i]]<-tab
    slopes<-c(slopes,slope)
    pvals<-c(pvals,pval)
    r2s<-c(r2s,r2)
  }
  
  #label p-values with responsevars after all the loops have finished
  valtab<-data.frame(responsevars,slopes,pvals,r2s)

  return(valtab)
  
}

######################################################
#PlotmanyXvsY
######################################################
#What it does: Plot and summarize many xs (numeric) across 1 y (numeric) - scatterplots; Uses CopyPlot and LmFitTab
#What it needs: 
#(1) y = numeric vector
#(2) xs = matrix of x cols; numeric; length of x col needs to be same as y col
#(3) main = title of each plot
#(4) ylab = character string naming the y variable
#(5) copyplot = should the plot be automatically copied as a .png to your working directory folder?
#What it produces: summaries, which is a list of y vars summarized across x levels; can save plots as .pngs; 1 plot per y var

PlotmanyXvsY<-function(y, xs, main, ylab, pointlabvec, slope, pval, copyplot){
  numcols<-dim(xs)[2] #number of x columns to loop through
  i<-0
  for (i in 1:numcols){ #loop through each x column and match with the 1 y column
    #pull out the current x column
    currentx<-xs[,i]
    
    #set up the plot
    plot(y~currentx, 
         main=main, 
         ylab=ylab,
         xlab=colnames(xs)[i], type='n')
    
    #add text points
    text(x=currentx, y=y, labels=pointlabvec, cex=.7)
    
    #add stats
    if(pval[i]<0.1){
      abline(lm(y~currentx))
      mtext(paste('slope = ',slope[i],sep=""), side=3, adj=1, line=1)
      mtext(paste('p-value = ',pval[i],sep=""), side=3, adj=1)
    }
    
    #copy plot
    if(copyplot==T){CopyPlot(name=paste(main,colnames(xs)[i],ylab,sep='_'))} 
  }
  return(print("done"))
}

######################################################
#PlotDatnCopy2.XFact
######################################################
#What it does: 
#(a) Plot and summarize many ys (numeric) across 1 x (factor); it produces a jittered scatter plot for each y variable across x levels
#(b) Uses CopyPlot instead of SavePlot, so that you can identify points
#(c) Plots points with plot labels, subplots are connected with gray lines
#(d) pvals can be entered manually
#What it needs: requires summarySE fxn
#(1) data1 = dataframe with x var and y vars
#(2) ycolrange = vector with a start column number and end column number to identify y cols within data1
#(3) xcol = column name of the x col (character)
#(4) xbuff = xlim buffer, often 0.5
#(5) pvals = vector of pvals that correspond to each y by x comparison
#(6) testdescript = character string used to describe pval in plot
#(7) main = plot title
#(8) copyplot = should the plot be automatically copied as a .png to your working directory folder?
#(9) What it produces: summaries, which is a list of y vars summarized across x levels; can save plots as .pngs; 1 plot per y var

PlotDatnCopy2.XFact<-function(data1, ycolrange, xcol, xbuff, pvals, testdescript, main, copyplot, pointlabvec){
  
  #1. define ys, ylims, and ycolnames (continuous)
  ys<-data1[,ycolrange[1]:ycolrange[2]]
  ymaxs<-apply(ys,2,max, na.rm=T)
  ymins<-apply(ys,2,min, na.rm=T)
  ycolnames<-colnames(ys)
  ylabs<-ycolnames
  num.y<-length(ycolnames)

  #2. define x (factor), xlims, and xlabels
  x<-as.numeric(data1[,xcol]) #make this numeric for plotting
  xlabels<-levels(data1[,xcol]) #character vector
  xAT<-unique(x) #numeric vector
  xlims<-c(min(xAT)-xbuff,max(xAT)+xbuff)
  xlab<-xcol
  num.sub<-length(xlabels) #number of x levels is the number of data subsets needed
  
  #3. summarize each y by x
  summaries<-list()
  i<-0
  for (i in 1:num.y){ #loop through each y
    summary<-summarySE(data=data1,measurevar=ycolnames[i], groupvars=xcol, na.rm=T) 
    summaries[[i]]<-summary
  }
  names(summaries)<-ycolnames #rename the df in the list to reflect the ycolnames
  
  #4. plot many ys (continuous) by 1 x (factor)
  i<-1 
  for (i in 1:num.y){ #loop through each y
    if(sum(!is.na(ys[,i]))>0){ 
      
      #set up plot
      plot(x=x,y=ys[,i],type='n',xaxt='n',xlab=xlab, ylab=ylabs[i],xlim=xlims, ylim=c(ymins[i],ymaxs[i]),main=main)
      axis(1, at=xAT, labels=xlabels)
      
      #add data
      palette(gray(seq(0,.9,len=25))) # gray scales; print old palette
      p<-0
      for (p in which(x==1)){
        set.seed(p)
        segments(x0=jitter(x[p], factor=5),y0=ys[p,i], x1=jitter(x[p+1], factor=5),y1=ys[(p+1),i], col=p) 
        set.seed(p)
        text(x=jitter(x[p], factor=5),y=ys[p,i], labels=pointlabvec[p], col=p)
        text(x=jitter(x[p+1], factor=5),y=ys[(p+1),i], labels=pointlabvec[p], col=p)
      }
      
      #add mean and error bars
      ysummary<-summaries[[i]]
      ymeans<-ysummary[,'mean']
      yseUPs<-ymeans+ysummary[,'se']
      yseDOWNs<-ymeans-ysummary[,'se']
      points(x=xAT,y=ymeans,type = "p", pch = 19, col='red') #add mean
      arrows(x0=xAT,y0=ymeans, y1=yseUPs,angle=90, code=3, length=0, col='red')
      arrows(x0=xAT,y0=ymeans, y1=yseDOWNs,angle=90, code=3, length=0, col='red')
      
      #add test stats if significant
      if (pvals[i]<0.1){mtext(paste(testdescript,'p-value',pvals[i]),side=3, adj=1)}
      
      #copy plot
      if(copyplot==T){CopyPlot(name=paste(main,ylabs[i],sep='_'))}
    }
  }
  return(summaries)
}

######################################################
#Normalize
######################################################
#What it does: Test whether columns of data are normally-distributed.  If the data is not normal, then it will test whether it is log-normal, and then sqrt-normal.  Data columns will be transformed to normal (if possible) and re-assembled into a dataframe with ammended column names that reflect their transformation (or not-normal status)  
#What it needs: 
#(1) d = dataframe with columns of data to be transformed
#What it produces: dataframe that holds the appropriately-transformed data columns; column names will reflect whether data was normal, log-transformed, sqrt-transformed, or not normal

Normalize<-function(d){

  #initialize new dataframe to hold transformed data
  d.t<-d
  
  #1. Evaluate which cols are normally distributed
  swresults<-numeric(0)
  for (i in 1:dim(d)[2]){ #loop through each column
    x<-d[,i]
    hist(x, main=colnames(d)[i])
    result<-shapiro.test(x)
    swresults<-c(swresults,result$p.value)
  }
  notnorm<-which(swresults<0.05) # save vars that are not normal
  
  #2. If there is at least 1 var that is not normally distributed, then keep going... OPEN
  if (is.empty(notnorm) == F){ 
    
    #3a. For cols that were not normal, Evaluate if they are log-normally distributed
    newdf<-d[,c(notnorm)] #isolate not normal data
    swresults2<-numeric(0) 
    #if there is more than 1 var to loop through, then do the loop
    if (is.vector(newdf) == F){ 
      for (i in 1:dim(newdf)[2]){ #loop through each column
        x<-log(newdf[,i]+1)
        hist(x, main=colnames(newdf)[i])
        result<-shapiro.test(x)
        swresults2<-c(swresults2,result$p.value)
      }  
    }
    #if there is only 1 var, then don't do the loop
    if (is.vector(newdf) == T){ 
      x<-log(newdf+1)
      hist(x)
      result<-shapiro.test(x)
      swresults2<-result$p.value
    }
    lognorm<-notnorm[which(swresults2>0.05)] #normal...these CAN be log-transformed 
    notnorm.o.lognorm<-notnorm[which(swresults2<0.05)] #not normal...these CANNOT be log-transformed
    
    #3b. If there is at least 1 var that can be log-transformed, then do the transformation
    if(is.empty(lognorm)==F){ 
      d.t[,lognorm]<-log(d[,lognorm]+1) #log-transform
      colnames(d.t)[lognorm]<-paste(colnames(d.t)[lognorm],"_logt", sep="") #rename column to indicate that it has been log-transformed
    }
    
    #4. If there is at least 1 var that is not normally distributed and cannot be log-transformed, then keep going...OPEN
    if(is.empty(notnorm.o.lognorm)==F){
      
      #5a. For cols that were not normal and not log-normal, Evaluate if they are sqrt-normally distributed
      newdf<-d[,notnorm.o.lognorm] #isolate not normal and not lognormal data
      swresults3<-numeric(0) 
      #if there is more than 1 var to loop through, then do the loop
      if (is.vector(newdf)==F){
        for (i in 1:dim(newdf)[2]){ #loop through each column
          x<-sqrt(newdf[,i]+1)
          hist(x, main=colnames(newdf)[i])
          result<-shapiro.test(x)
          swresults3<-c(swresults3,result$p.value)
        }
      }
      #if there is only 1 var, then don't do the loop
      if (is.vector(newdf)==T){
        x<-sqrt(newdf+1)
        hist(x)
        result<-shapiro.test(x)
        swresults3<-result$p.value
      }
      sqrtnorm<-notnorm[which(swresults3>0.05)] #normal...these CAN be sqrt-transformed 
      notnorm.o.lognorm.o.sqrtnorm<-notnorm[which(swresults2<0.05)] #not normal...these CANNOT be log-transformed
            
      #5b. If there is at least 1 var that can be sqrt-transformed, then do the transformation
      if(is.empty(sqrtnorm)==F){
        d.t[,sqrtnorm]<-sqrt(d[,sqrtnorm]+1) #sqrt-transform
        colnames(d.t)[sqrtnorm]<-paste(colnames(d.t)[sqrtnorm],"_sqrtt", sep="") #rename column to indicate that it has been sqrt-transformed
      }
      
      #6. If there is at least 1 var that is not normally distributed, cannot be log-transformed and cannot be sqrt-transformed, then keep going...OPEN
      if(is.empty(notnorm.o.lognorm.o.sqrtnorm) == F){
        #7. For data cols that cannot be transformed to be normally distributed, 
        colnames(d.t)[notnorm.o.lognorm.o.sqrtnorm]<-paste(colnames(d.t)[notnorm.o.lognorm.o.sqrtnorm],"_notnorm", sep="") #rename the column to reflect that the data could not be made normal
      } #CLOSED
    } #CLOSED
  } #CLOSED
  
  
  #Return the dataframe
  return(d.t)
}

######################################################
#is.empty
######################################################
#What it does: helps identify if a vector or element is empty
#What it needs: x
#What it produces: TRUE/FALSE

is.empty <- function(x, mode=NULL){
  if (is.null(mode)) mode <- class(x)
  identical(vector(mode,1),c(x,vector(class(x),1)))
}

#Some Examples....
a<-1
b <- numeric(0)

is.empty(a)
is.empty(a,"numeric")
is.empty(a,"character")

is.empty(b)
is.empty(b,"numeric")
is.empty(b,"character")

######################################################
#PlotDatnCopy.XFact
######################################################
#What it does: Plot and summarize many ys (numeric) across 1 x (factor); it produces a jittered scatter plot for each y variable across x levels -- uses CopyPlot instead of SavePlot, so that you can identify points
#What it needs: requires summarySE fxn
#(1) data1 = dataframe with x var and y vars
#(2) ycolrange = vector with a start column number and end column number to identify y cols within data1
#(3) xcol = column name of the x col (character)
#(4) xbuff = xlim buffer, often 0.5
#(5) pvals = vector of pvals that correspond to each y by x comparison
#(6) testdescript = character string used to describe pval in plot
#(7) main = plot title
#(8) identify = do you want to interact with the plots by labelling points?
#(9) copyplot = should the plot be automatically copied as a .png to your working directory folder?
#(10) width and height = copyplot dimensions, ~400 is good
#What it produces: summaries, which is a list of y vars summarized across x levels; can save plots as .pngs; 1 plot per y var

PlotDatnCopy.XFact<-function(data1, ycolrange, xcol, xbuff, 
                         pvals, testdescript, main, 
                         copyplot, width, height,
                         identify, pointlabvec){ #open
  
  #1. define ys, ylims, and ycolnames (continuous)
  ys<-data1[,ycolrange[1]:ycolrange[2]]
  ymaxs<-apply(ys,2,max, na.rm=T)
  ymins<-apply(ys,2,min, na.rm=T)
  ycolnames<-colnames(ys)
  ylabs<-ycolnames
  num.y<-length(ycolnames)
  
  #2. define x (factor), xlims, and xlabels
  x<-as.numeric(data1[,xcol]) #make this numeric for plotting
  xlabels<-levels(data1[,xcol]) #character vector
  xAT<-unique(x) #numeric vector
  xlims<-c(min(xAT)-xbuff,max(xAT)+xbuff)
  xlab<-xcol
  num.sub<-length(xlabels) #number of x levels is the number of data subsets needed
  
  #3. summarize each y by x
  summaries<-list()
  i<-0
  for (i in 1:num.y){ #loop through each y
    summary<-summarySE(data=data1,measurevar=ycolnames[i], groupvars=xcol, na.rm=T) 
    summaries[[i]]<-summary
  }
  names(summaries)<-ycolnames #rename the df in the list to reflect the ycolnames
  
  #4. plot many ys (continuous) by 1 x (factor)
  i<-0
  for (i in 1:num.y){ #loop through each y
    if(sum(!is.na(ys[,i]))>0){ 
      
      #set up plot
      plot(x=x,y=ys[,i],type='n',xaxt='n',
           xlab=xlab, ylab=ylabs[i],
           xlim=xlims, ylim=c(ymins[i],ymaxs[i]),
           main=main)
      axis(1, at=xAT, labels=xlabels)
      
      #add data
      set.seed(10)
      points(x=jitter(x, factor=.5),y=ys[,i],type = "p", pch = 16, cex=.7, col='gray') 
      
      #add mean and error bars
      ysummary<-summaries[[i]]
      ymeans<-ysummary[,'mean']
      yseUPs<-ymeans+ysummary[,'se']
      yseDOWNs<-ymeans-ysummary[,'se']
      points(x=xAT,y=ymeans,type = "p", pch = 19, col=2) #add mean
      arrows(x0=xAT,y0=ymeans, y1=yseUPs,angle=90, code=3, length=0, col=2)
      arrows(x0=xAT,y0=ymeans, y1=yseDOWNs,angle=90, code=3, length=0, col=2)
      
      #add test stats
      if (pvals[i]<0.1){
        mtext(paste(testdescript,'p-value',pvals[i]),side=3, adj=1)
      }
      
      #identify outliers
      if(identify==T){
        set.seed(10)
        identify(jitter(x, factor=.5), ys[,i], labels=pointlabvec, cex=.7)
      }
      
      #copy plot
      if(copyplot==T){CopyPlot(name=paste(main,ylabs[i],sep='_'), width, height)}
    }
  }
  return(summaries)
}

######################################################
#PlotDatnSave.XFact
######################################################
#What it does: Plot and summarize many ys (numeric) across 1 x (factor); it produces a jittered scatter plot for each y variable across x levels
#What it needs: 
#(1) requires summarySE fxn
#(2) data1 = dataframe with x var and y vars
#(3) ycolrange = vector with a start column number and end column number to identify y cols within data1
#(4) xcol = column name of the x col (character)
#(5) xbuff = xlim buffer, often 0.5
#(6) pvals = vector of pvals that correspond to each y by x comparison
#(7) testdescript = character string used to describe pval in plot
#(8) main = plot title
#(9) saveplot = should the plot be automatically saved?
#(10) width, height = dimensions for the saved plot
#What it produces: summaries, which is a list of y vars summarized across x levels; 1 plot per y var

PlotDatnSave.XFact<-function(data1,ycolrange,xcol,xbuff,pvals,testdescript, main, saveplot, width, height){
  
  #1. define ys, ylims, and ycolnames (continuous)
  ys<-data1[,ycolrange[1]:ycolrange[2]]
  ymaxs<-apply(ys,2,max, na.rm=T)
  ymins<-apply(ys,2,min, na.rm=T)
  ycolnames<-colnames(ys)
  ylabs<-ycolnames
  num.y<-length(ycolnames)
  
  #2. define x (factor), xlims, and xlabels
  x<-as.numeric(data1[,xcol]) #make this numeric for plotting
  xlabels<-levels(data1[,xcol]) #character vector
  xAT<-unique(x) #numeric vector
  xlims<-c(min(xAT)-xbuff,max(xAT)+xbuff)
  xlab<-xcol
  num.sub<-length(xlabels) #number of x levels is the number of data subsets needed
  
  #3. summarize each y by x
  summaries<-list()
  i<-0
  for (i in 1:num.y){ #loop through each y
    summary<-summarySE(data=data1,measurevar=ycolnames[i], groupvars=xcol, na.rm=T) 
    summaries[[i]]<-summary
  }
  names(summaries)<-ycolnames #rename the df in the list to reflect the ycolnames
  
  #4. plot many ys (continuous) by 1 x (factor)
  i<-0
  for (i in 1:num.y){ #loop through each y
    if(sum(!is.na(ys[,i]))>0){
      
      if(saveplot==T){SavePlot(name=paste(main,ylabs[i],sep='_'), width=width, height=height)}
      
      #set up plot
      plot(x=x,y=ys[,i],type='n',xaxt='n',
           xlab=xlab, ylab=ylabs[i],
           xlim=xlims, ylim=c(ymins[i],ymaxs[i]),
           main=main)
      axis(1, at=xAT, labels=xlabels)
      
      #add data
      points(x=jitter(x, factor=.5),y=ys[,i],type = "p", pch = 16, cex=.7, col='gray') 
      
      #add mean and error bars
      ysummary<-summaries[[i]]
      ymeans<-ysummary[,'mean']
      yseUPs<-ymeans+ysummary[,'se']
      yseDOWNs<-ymeans-ysummary[,'se']
      points(x=xAT,y=ymeans,type = "p", pch = 19, col=2) #add mean
      arrows(x0=xAT,y0=ymeans, y1=yseUPs,angle=90, code=3, length=0, col=2)
      arrows(x0=xAT,y0=ymeans, y1=yseDOWNs,angle=90, code=3, length=0, col=2)
      
      #add test stats
      if (pvals[i]<0.1){
        mtext(paste(testdescript,'p-value',pvals[i]),side=3, adj=1)
      }
      
      if(saveplot==T){dev.off()} #remember to finish the save plot by turning off the device
    }
  }
  return(summaries)
}

######################################################
#PairedTpvals2
######################################################
#What it does: Spits out pvals for paired parametric (t-test) and non-parametric t-tests (wilcoxon test) on many ys (numeric) across 1 x (factor)
#What it needs: 
#(1) data1 = dataset that holds columns for (1) x var, (2) y var, and (3) subjectname
#(2) subjectname = subject colname(character) i.e. plot or site identifier
#(3) xcol = x colname (character)
#(4) ycolrange = vector with a start column number and end column number to identify y cols within data1
#What it produces: pvaltab = table of pvals that correspond to each y by x comparison... t.pvals are from t-tests and w.pvals are from wilcoxon tests

PairedTpvals2<-function(data1,subjectname, xcol, ycolrange){
  
  #identify all of the parts
  subject<-data1[,subjectname] #pull out subject identifiers (character vector)
  xchar<-data1[,xcol] #pull out x var (character vector)
  xlevels<-unique(xchar) #levels within the X var factor
  ys<-data1[,ycolrange[1]:ycolrange[2]] #pull out matrix of y vars
  num.y<-length(colnames(ys)) #number of y vars
  responsevars<-colnames(ys)
  
  #initialize loop items
  t.pvals<-numeric(0)
  w.pvals<-numeric(0)
  i<-0 
  
  #determine the role of factor X on each Y
  for (i in 1:num.y){ #loop through each y
    currenty<-ys[,i] #pull out the current y column
    currentdf<-data.frame(subject,xchar,currenty) #assemble the current dataframe 
    
    if(sum(!is.na(currenty))>0){ #if there is any data in the column at all....
      
      if (sum(is.na(currentdf$currenty))>0){ # if there is one NA in the dataset...
        narows<-which(is.na(currentdf$currenty)) # figure out which rows have the NAs
        removesubjects<-currentdf[narows,]$subject # identify which plots have NAs and remove all rows for that plot, so there is a even test
        currentdf<-currentdf[!(currentdf$subject %in% removesubjects),] # remove rows with that plotname
      }
      
      if ((length(xlevels)-1)>1){print('warning: too more than 2 levels')}
      
      #Re-organize data by factor
      group1<-currentdf[currentdf$xchar==xlevels[1],]
      group2<-currentdf[currentdf$xchar==xlevels[2],]
      
      #1. Parametric t-test
      tsum<-t.test(group1$currenty,group2$currenty, paired=TRUE)
      t.pval<-round(tsum$p.value, digits=4)      
      
      #2. Non-parametric wilcoxon test
      wsum<-wilcox.test(group1$currenty,group2$currenty,paired=TRUE, exact=FALSE)
      w.pval<-round(wsum$p.value, digits=4)
    } else if(sum(!is.na(currenty))==0){ #if there is no data in the column at all...
      t.pval<-NA
      w.pval<-NA
    } 
    
    #save p-values at the end of each loop
    t.pvals<-c(t.pvals,t.pval)
    w.pvals<-c(w.pvals,w.pval)
  }
  
  #label p-values with responsevars after all the loops have finished
  pvaltab<-data.frame(responsevars,t.pvals,w.pvals)
  
  return(pvaltab)
  
}

######################################################
#PairedTpvals
######################################################
#What it does: Spits out pvals for paired t-tests on many ys (numeric) across 1 x (factor)
#What it needs: 
#(1) data1 = dataset that holds columns for (1) x var, (2) y var, and (3) subjectname
#(2) subjectname = subject colname(character) i.e. plot or site identifier
#(3) xcol = x colname (character)
#(4) ycolrange = vector with a start column number and end column number to identify y cols within data1
#What it produces: pvals = vector of pvals that correspond to each y by x comparison

PairedTpvals<-function(data1,subjectname, xcol, ycolrange){
  
  #identify all of the parts
  xchar<-data1[,xcol] #pull out x var (character vector)
  xlevels<-length(unique(xchar)) #number of levels within the X var factor
  ys<-data1[,ycolrange[1]:ycolrange[2]] #pull out matrix of y vars
  num.y<-length(colnames(ys)) #number of y vars
  
  #initialize loop items
  aovsums<-list()
  pvals<-numeric(0)
  i<-0 #need to change this back to 0
  
  #determine the role of factor X on each Y
  for (i in 1:num.y){ #loop through each y
    
    currenty<-ys[,i] #pull out the current y var (numeric vector)
    currentdf<-data.frame(subject,xchar,currenty) #assemble the current dataframe
    
    if(sum(!is.na(currenty))>0){ #if there is any data at all....
      
      if (sum(is.na(currentdf$currenty))>0){ # remove rows with NAs and remove the matching I/N subplot
        narows<-which(is.na(currentdf$currenty))
        removesubjects<-currentdf[narows,]$subject #identify which plots have NAs
        currentdf<-currentdf[!(currentdf$subject %in% removesubjects),] #remove rows with that plotname
      }
      
      if ((xlevels-1)>1){
        print('warning: too more than 2 levels')
      }
      
      aovsum<-summary(aov(currenty ~ xchar + Error(subject/xchar), currentdf)) # run paired t-test
      pval<-round(unlist(aovsum[[2]])['Pr(>F)1'], digits=4) #extract the pval for the fixed effect
      aovsums[[i]]<-aovsum #save aovsum
      
    } else if(sum(!is.na(currenty))==0){
      aovsums[[i]]<-NA
      pval<-NA
    } 
    pvals<-c(pvals,pval) #save pval
  }
  return(pvals)
  #return(aovsums)
}

######################################################
#NMDS.scree
######################################################
#What it does: it produces a scree plot for an NMDS
#What it needs: 
#(1) x = 'community' dataframe
#(2) distance = distance matrix, ie bray, euc
#What you get: scree plot

NMDS.scree<-function(x,distance) { #where x is the name of the data frame variable
  plot(rep(1,10),replicate(10,metaMDS(x,distance,noshare=FALSE, wascores=FALSE,autotransform=F,k=1)$stress/100),
       xlim=c(1,nrow(x)),ylim=NULL,
       xlab="# of Dimensions",ylab="Stress",main="NMDS stress plot")
  for (i in 1:(nrow(x)-2)) {
    points(rep(i+1,10),replicate(10,metaMDS(x,distance,noshare=FALSE, wascores=FALSE,autotransform=F,k=i+1)$stress/100)) 
  }
}

######################################################
#StandCenter
######################################################
#What it does: it centers and standardizes columns of data in a dataframe
#What it needs: 
#(1) data = rows are observations, columns are variables
#What you get: centered and standardized dataframe

StandCenter<-function(data){
  meanvec<-numeric(0)
  sdvec<-numeric(0)
  newdf<-mat.or.vec(dim(data)[1],dim(data)[2])
  for (i in 1:dim(data)[2]){ #loop through each column
    x<-data[,i]
    mean<-mean(x)
    sd<-sd(x)
    meanvec<-c(meanvec,mean)
    sdvec<-c(sdvec,sd)
    newdf[,i]<-(data[,i]-meanvec[i])/sdvec[i] # do the standardization calc
  }
  colnames(newdf)<-colnames(data) #transfer the columns names to the new dataset
  return(newdf)
}

######################################################
#DataToPCA
######################################################
#What it does: takes a dataset and does a prcomp() PCA ordination on it, creates a biplot
#What it needs: 
#(1) data = rows are observations, columns are variables
#(2) Labels = identifier for each row
#(3) plotname
#(4) center=T, scale=T
#(5) sfactor= scaling factor for plotting purposes only
#(6) spush= amount that pushes arrow label away from the arrow
#What you get: biplot

DataToPCA<-function(data, Labels, plotname, center, scale, sfactor, spush){
  mydata.pca <- prcomp(data, retx=TRUE, center=center, scale.=scale)
  sd <- mydata.pca$sdev #standard deviation
  loadings <- mydata.pca$rotation #loadings
  rownames(loadings) <- colnames(data) #loadings per variable
  scores <- mydata.pca$x #scores per observation
  #distance biplot
  xlim<-c(min(scores[,1])*1.2, max(scores[,1])*1.2)
  ylim<-c(min(scores[,2])*1.2, max(scores[,2])*1.2)
  plot(scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2", type="n", 
       xlim=xlim,
       ylim=ylim)
  arrows(0,0,loadings[,1]*sfactor,loadings[,2]*sfactor, length=0.1,angle=20, col="red") # scaling factor (sfactor=8) of may need to be changed depending on the data set
  text(loadings[,1]*sfactor*spush,loadings[,2]*sfactor*spush,rownames(loadings), col="red", cex=0.7) # additional scaling factor (spush=1.2) insures that labels are plotted just beyond the arrows
  text(scores[,1],scores[,2], Labels, col="blue", cex=0.7)
  mtext(plotname, side=3)
  dimtext<-paste(dim(data)[1],'plots,',dim(data)[2],'Variables')
  mtext(dimtext, side=4)
  return(mydata.pca)
}

######################################################
#boxplot.with.outlier.label, from http://www.r-statistics.com/tag/boxplot-outlier/
######################################################
#What it does: Labels outliers in a boxplot and creates an index of them for you
#What it needs: 
#(1) y
#(2) label_name = list of labels that correspond to the elements of y
#(3) push_text_right = enlarge push_text_right in order to push the text labels further from their point
#(4) label.col = label color, default is blue
#What you get: amended boxplot

require2 <- function (package, ask = TRUE, ...) {
  package <- as.character(substitute(package))
  if (!suppressWarnings(require(package = package, character.only = TRUE))) {
    install_package <- ask.user.yn.question(paste("Package ", package, " is not installed. Do you want to install it now?"))
    if (install_package) 
      install.packages(pkgs = package)
  }
  require(package = package, character.only = TRUE)
}

boxplot.with.outlier.label <- function(y, label_name, ..., spread_text = T, data, plot = T, range = 1.5, label.col = "blue", push_text_right = 1.5, # enlarge push_text_right in order to push the text labels further from their point
                                       segement_width_as_percent_of_label_dist = .45, # Change this if you want to have the line closer to the label (range should be between 0 to 1)
                                       jitter_if_duplicate = T, jitter_only_positive_duplicates = F){  
  # need to make sure package is open for is.formula and ddply functions within this function
  require2(plyr) 
  # a function to jitter data in case of ties in Y's
  jitter.duplicate <- function(x, only_positive = F){
    if(only_positive) {ss <- x > 0} else {ss <- T}	
    ss_dup <- duplicated(x[ss])
    # ss <- ss & ss_dup
    temp_length <- length(x[ss][ss_dup])	
    x[ss][ss_dup] <- x[ss][ss_dup] + seq(from = 0.00001, to = 0.00002, length.out = temp_length)
    x
  }
  # handle cases where (1)...
  if(jitter_if_duplicate) {
    if(!missing(data)) {	#IF THERE IS DATA
      y_name <- as.character(substitute(y))	
      if(length(y_name) > 1) {	#...AND THERE IS A FORMULA (for example: "~", "y", "x")
        model_frame_y <- model.frame(y, data = data)
        temp_y <- model_frame_y[,1]
        temp_y  <- jitter.duplicate(temp_y, jitter_only_positive_duplicates)	# notice that the default of the function is to work only with positive values...
        # the_txt <- paste(names(model_frame_y)[1], "temp_y", sep = "<<-") # wrong...
        the_txt <- paste("data['",names(model_frame_y)[1],"'] <- temp_y", sep = "")				
        eval(parse(text = the_txt))	# jutter out y var so to be able to handle identical values.
      } else {	#... AND THERE IS NO FORMULA
        data[,y_name] <- jitter.duplicate(data[,y_name], jitter_only_positive_duplicates)
        y <- data[,y_name]
      }		
    } 
    else {	#IF THERE IS NO DATA	 
      if(is.formula(y)) { #...AND THERE IS A FORMULA
        temp_y <- model.frame(y)[,1]
        temp_y  <- jitter.duplicate(temp_y, jitter_only_positive_duplicates)	# notice that the default of the function is to work only with positive values...
        temp_y_name <- names(model.frame(y))[1]	# we must extract the "names" before introducing a new enbironment (or there will be an error)
        environment(y) <- new.env()
        assign(temp_y_name, temp_y, environment(y))
        # Credit and thanks for doing this goes to Niels Richard Hansen (2 Jan 30, 2011)
        # http://r.789695.n4.nabble.com/environment-question-changing-variables-from-a-formula-through-model-frame-td3246608.html
        # warning("Your original variable (in the global environemnt) was just jittered.")	# maybe I should add a user input before doing this....
        # the_txt <- paste(names(model_frame_y)[1], "temp_y", sep = "<<-")
        # eval(parse(text = the_txt))	# jutter out y var so to be able to handle identical values.
      } else { #...AND THERE IS NO FORMULA
        y <- jitter.duplicate(y, jitter_only_positive_duplicates)
      }		
    }
  }
  #handle cases where (2)....
  if(missing(data)) {
    boxdata <- boxplot(y, plot = plot,range = range ,...)
  } 
  else {
    boxdata <- boxplot(y, plot = plot,data = data, range = range ,...)
  }
  if(length(boxdata$names) == 1 && boxdata$names =="") boxdata$names <- 1	# this is for cases of type: boxplot(y) (when there is no dependent group)
  if(length(boxdata$out) == 0 ) {
    warning("No outliers detected for this boxplot")
    return(invisible())
  }
  if(!missing(data)) attach(data)	# this might lead to problams I should check out for alternatives for using attach here...
  # create a data.frame with information from the boxplot output about the outliers (location and group)
  boxdata_group_name <- factor(boxdata$group)
  levels(boxdata_group_name) <- boxdata$names[as.numeric(levels(boxdata_group_name))]	# the subseting is for cases where we have some sub groups with no outliers
  if(!is.null(list(...)$at))	{	# if the user chose to use the "at" parameter, then we would like the function to still function (added on 19.04.2011)
    boxdata$group <- list(...)$at[boxdata$group]}
  boxdata_outlier_df <- data.frame(group = boxdata_group_name, y = boxdata$out, x = boxdata$group)
  # extract the x,y variables from the formula:
  if(is.formula(y)){
    model_frame_y <- model.frame(y)    
    y <- model_frame_y[,1]
    x <- model_frame_y[,-1]
    if(!is.null(dim(x))) {	# then x is a matrix/data.frame of the type x1*x2*..and so on - and we should merge all the variations...
      x <- apply(x,1, paste, collapse = ".")}
  } 
  else { # if(missing(x)) x <- rep(1, length(y))
    x <- rep(1, length(y))	# we do this in case y comes as a vector and without x
  }	
  # and put all the variables (x, y, and outlier label name) into one data.frame
  DATA <- data.frame(label_name, x ,y)
  #clean-up  
  if(!is.null(list(...)$names))	{	# if the user chose to use the names parameter, then we would like the function to still function (added on 19.04.2011)
    DATA$x <- factor(DATA$x, levels = unique(DATA$x))
    levels(DATA$x) = list(...)$names	# enable us to handle when the user adds the "names" parameter # fixed on 19.04.11	# notice that DATA$x must be of the "correct" order (that's why I used split above
    # warning("Careful, the use of the 'names' parameter is experimental.  If you notice any errors please e-mail me at: tal.galili@gmail.com")
  }
  if(!missing(data)) detach(data)	# we don't need to have "data" attached anymore.
  #function to spit out outlier data  
  boxplot.outlier.data <- function(xx, y_name = "y"){ # only keep the rows with our outliers 
    y <- xx[,y_name]
    boxplot_range <- range(boxplot.stats(y, coef = range )$stats)
    ss <- (y < boxplot_range[1]) | (y > boxplot_range[2])
    return(xx[ss,])	
  }
  outlier_df <-ddply(DATA, .(x), boxplot.outlier.data)
  # create propor x/y locations to handle over-laping dots...
  if(spread_text) {
    require2(TeachingDemos)		
    temp_x <- boxdata_outlier_df[,"x"]
    temp_y1 <- boxdata_outlier_df[,"y"]
    temp_y2 <- temp_y1
    for(i in unique(temp_x)){
      tmp <- temp_x == i
      temp_y2[ tmp ] <- spread.labs( temp_y2[ tmp ], 1.3*strheight('A'), maxiter=6000, stepsize = 0.05) #, min=0 )
    }
  }
  # plotting the outlier labels
  for(i in seq_len(dim(boxdata_outlier_df)[1])){
    ss <- (outlier_df[,"x"]  %in% boxdata_outlier_df[i,]$group) & (outlier_df[,"y"] %in% boxdata_outlier_df[i,]$y)
    current_label <- outlier_df[ss,"label_name"]
    temp_x <- boxdata_outlier_df[i,"x"]
    temp_y <- boxdata_outlier_df[i,"y"]		
    if(spread_text) {
      temp_y_new <- temp_y2[i] # not ss			
      move_text_right <- strwidth(current_label) * push_text_right
      text( temp_x+move_text_right, temp_y_new, current_label, col = label.col)			
      segments( temp_x+(move_text_right/6), temp_y, temp_x+(move_text_right*segement_width_as_percent_of_label_dist), temp_y_new )
    } 
    else {
      text(temp_x, temp_y, current_label, pos = 4, col = label.col)
    }		
  }
  # outputing some of the information we collected
  list(boxdata = boxdata, boxdata_outlier_df = boxdata_outlier_df, outlier_df=outlier_df)
}

######################################################
#PlotXmanyYnSave, created 2/19/14 for e8 analyses
######################################################
#What it does: Plots many ys against 1 x, saves plots as jpgs in R Plots folder
#What it needs:
#(1) plotnamevec
#(2) data
#(3) sdlist
#(4) tab
#(5) xcolnam
#(6) xlims
#(7) xAT
#(8) xnames
#(9) xlab
#(10) ys
#(11) yind
#What you get: many plots, all with the same x var saved as jpgs

PlotXmanyYnSave<-function(plotnamevec,
                    data,sdlist,tab,
                    xcolnam,xlims,xAT,xnames,xlab,
                    ys,yind){
  p<-0
  #loop through each y (row in ind)
  for (p in 1:dim(yind)[1]){ 
    par(mfrow = c(1, 1),cex = 0.8, mar = c(0, 4, 0, 0), oma = c(5, 5, 0.5, 0.5), tcl = -0.25, mgp = c(2, 0.6, 0),xpd = NA)
    #assign y
      currenty<-yind[p,'ycolnam'] #y name
      currentylab<-yind[p,'ylabs']  #y labels
      currentylims<-c(yind[p,'ymin'],yind[p,'ymax']) #y limits
    #start save plot
      SavePlot(name=plotnamevec[p])
    #set up plot
      plot(x=data[,as.character(xcolnam)],y=data[,as.character(currenty)], #set up plot
           type = "n", xaxt = "n",
           xlab = " ", ylab = currentylab,
           xlim = xlims, ylim = currentylims)
      axis(1, at=xAT, labels=xnames, cex.axis=.5)
      mtext(xlab, side=1, line=2)
    #add points: loop through categories of subsetted data (dataframe in sdlist, rows in tab)
      for (i in 1:length(sdlist)){ 
        sd<-sdlist[[i]]
        points(sd[,as.character(xcolnam)],sd[,as.character(currenty)], pch=tab[i,'pchs'], col=tab[i,'cols'])
      }
    #finish save plot
      dev.off()
  }
}

######################################################
#SavePlot, created 2/19/14 based on R news & tutorials
######################################################
#What it does: Saves your plot as a jpeg in the folder ~/Desktop/R Plots
#What it needs: 
#(1) name = a name for the plot; it will be saved as plot_'name'.jpg
#(2) width and height = typically 480 x 480
#What you get: plot jpeg
#Notes: In a loop, put this right BEFORE you've made the plot.  Remember to turn off the plot device within the loop

SavePlot<-function(name, width, height){
  plot.new()
  mypath <- file.path("/Users","/marissalee","/Desktop","/R Plots",paste(name, ".jpg", sep = ""))
  jpeg(file=mypath, width=width, height=height)
}
#make the plot
#dev.off()  #remember to finish the plot by turning off the device

######################################################
#CopyPlot, created 4/3/14 based on 'SavePlot'
######################################################
#What it does: Copies your plot as a .png and puts it into your working directory folder. This allows you to save plots that you have interacted with (e.g. used the 'identify' fxn)
#What it needs: 
#(1) name = a name for the plot; it will be saved as plot_'name'.jpg
#(2) width = ~400
#(3) height = ~400
#What you get: plot png in your working directory folder
#Notes: In a loop, put this right AFTER you've made the plot.  Remember to turn off the plot device within the loop...with dev.off() and graphics.off()

CopyPlot<-function(name, width, height){
  dev.copy(png, paste(name,".png", sep=""), width=width, height=height) #copy the device as a .png to your working directory folder
  dev.off() #finish saving the device
  graphics.off() #close all devices to start fresh for the next loop iteration
} 
# #EXAMPLE
# i<-0
# for (i in 4:5){
#   plot(mtcars$wt, mtcars$mpg)   #Make plot
#   identify(mtcars$wt, mtcars$mpg)   #interact with it
#   dev.copy(png, paste('myplot',i,'.png', sep=''))  #Copy plot
#   dev.off() #Finish saving the plot by closing that device
#   graphics.off()
# }

######################################################
#NoPlot, created 2/19/14
######################################################
#What it does: Makes a blank plot
#What it needs: nothing
#What you get: A blank plot, useful for plotting just a legend

NoPlot<-function(){
  dev.off()
  par(mfrow = c(1, 1),xpd = NA) # xpd makes it so that the legend can go outside the plot region
  x<-c(0,1) #arbitary x and y
  y<-c(0,1)
  plot(x=x,y=y, 
       type = "n", axes = FALSE, # axes = FALSE makes it so that the box is not shown
       xlab = " ", ylab = " ")
}

######################################################
#PlotXmanyY, created 2/18/14 for e8 analyses
######################################################
#What it does: Plots many ys against 1 x (made for e8 analyses)
#What it needs: 
#(1) data
#(2) sdlist
#(3) tab
#(4) xcolnam
#(5) xlims
#(6) xAT
#(7) xnames
#(8) xlab
#(9) ys
#(10) yind
#What you get: many plots, all with the same x var

PlotXmanyY<-function(data,sdlist,tab,
                   xcolnam,xlims,xAT,xnames,xlab,
                   ys,yind){
  p<-0
  #loop through each y (row in ind)
  for (p in 1:dim(yind)[1]){ 
    par(mfrow = c(1, 1),cex = 0.8, mar = c(0, 4, 0, 0), oma = c(5, 5, 0.5, 0.5), tcl = -0.25, mgp = c(2, 0.6, 0),xpd = NA)
    #assign y
    currenty<-yind[p,'ycolnam'] #y name
    currentylab<-yind[p,'ylabs']  #y labels
    currentylims<-c(yind[p,'ymin'],yind[p,'ymax']) #y limits
    #set up plot
    plot(x=data[,as.character(xcolnam)],y=data[,as.character(currenty)], #set up plot
         type = "n", xaxt = "n",
         xlab = " ", ylab = currentylab,
         xlim = xlims, ylim = currentylims)
    axis(1, at=xAT, labels=xnames, cex.axis=.5)
    mtext(xlab, side=1, line=2)
    #add points
    #loop through categories of subsetted data (dataframe in sdlist, rows in tab)
    for (i in 1:length(sdlist)){ 
      sd<-sdlist[[i]]
      points(sd[,as.character(xcolnam)],sd[,as.character(currenty)], pch=tab[i,'pchs'], col=tab[i,'cols'])
    }
  }
}

######################################################
#length2, from an R blog
######################################################
#What it does: A version of length which can handle NA's
#What it needs:
#(1) x
#What you get: a value

length2 <- function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}

######################################################
#summarySE, from an R blog
######################################################
#What it does: It summarizes data by group variables using plyr package
#What it needs: 
#(1) data
#(2) measurevar
#(3) groupvars
#What you get: dataframe with measure vars and groupvars as columns

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, .drop=TRUE) {
  require(plyr)  
  datac <- ddply(data, groupvars, .drop=.drop, # This is does the summary; it's not easy to understand...
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm))},measurevar,na.rm)
  #datac <- rename(datac, c("mean"=measurevar)) # Rename the "mean" column
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  return(datac)
}

######################################################
#PrepXY, created for analyzing metaanalysis trait x soilN data
######################################################
#What it does: It combines two dataframes, xmat and ymat.  Each xmat col gets paired with each ymat col for plotting purposes. E.g. if xmat had 2 cols: A,B and ymat had 2 cols: C,D, then colA,colC; colA,colD; colB,colC; colB,colD.
#What it needs:
#(1) xmat 
#(2) ymat (rows = observations, cols = measurement types)
#(3) xnames 
#(4) ynames (vector of measurements types that correspond with xmat/ymat cols)
#(5) xlims
#(6) ylims (row1 is min, row2 is max, cols correspond with xmat/ymat cols)
#What you get: A list (prepxy) of 3 lists (pairs, lims, ns)
#(a) pairs: list of dataframes, each has an x and y column, rows are observations
#(b) lims: list of dataframes, each has a xlim and ylim column, row1 is min, row2 is max
#(c) ns: list of vectors, each has a xn (x name) and yn (y name) element

PrepXY<-function(xmat, ymat, xnames, ynames, xlims, ylims){
#initialize lists, vectors, counter
    datavecs<-list()
    limvecs<-list()
    namevecs<-list()
    index<-numeric(0)
    counter<-0
#loop through each column of ymat
  for (j in 1:ncol(ymat)){ 
  #loop through each column of xmat
    for (i in 1:ncol(xmat)){
    #keep track of which y and x column you see... will use this to condense the lists in the next for-loop
      counter<-i+5*j
      index<-c(index,counter)
    #put the x and y cols into a dataframe, put that dataframe into the correct list slot
      x<-xmat[,i] #x and y data saved in datavecs list
      y<-ymat[,j]
      data<-data.frame(x,y)
      colnames(data)<-c(xnames[i],ynames[j])
      datavecs[[counter]]<-data
      xlim<-xlims[,i] #x and y lims saved in limvecs list
      ylim<-ylims[,j]
      lim<-data.frame(xlim,ylim)
      colnames(lim)<-c(xnames[i],ynames[j])
      limvecs[[counter]]<-lim
      xn<-xnames[i] #x and y names saved in namevecs list
      yn<-ynames[j]
      n<-data.frame(xn,yn)
      namevecs[[counter]]<-n
    }
  }
#reorganize the lists that were made above so that they are in consequtive slots
#initialize lists
  pairs<-list()
  lims<-list()
  ns<-list()
#loop through each element in index
  for (k in 1:length(index)){
  #use that index element to pull out a dataframe from the list, put that dataframe into the new list
    pair<-datavecs[index[k]] #x and y data saved in pair list
    pairs[k]<-pair
    lim1<-limvecs[index[k]] #x and y lims saved in lims list
    lims[k]<-lim1
    n<-namevecs[index[k]] #x and y names saved in ns list
    ns[k]<-n
  }
#re-name the dataframes in each list with a panel#  
  names(pairs)<-paste('panel',seq(1,length(pairs),1))
  names(lims)<-paste('panel',seq(1,length(lims),1))
  names(ns)<-paste('panel',seq(1,length(ns),1))
#compile lists into 1 list, prepxy
  prepxy<-list(pairs=pairs,lims=lims,ns=ns)
  return(prepxy)
}

######################################################
#PrepXY2, created for analyzing metaanalysis trait x soilN data
######################################################
#What it does: It combines two dataframes, xmat and ymat.  The 1st xmat col gets paired with the 1st ymat col, the 2nd xmat col gets paired with the 2nd ymat col, etc.
#What it needs: 
#(1) xmat 
#(2) ymat (rows = observations, cols = measurement types)
#(3) xnames 
#(4) ynames (vector of measurements types that correspond with xmat/ymat cols)
#(5) xlims
#(6) ylims (row1 is min, row2 is max, cols correspond with xmat/ymat cols)
#What you get: A list (prepxy) of 3 lists (pairs, lims, ns)

PrepXY2<-function(xmat, ymat, xnames, ynames, xlims, ylims){
  #initialize the lists, vectors, counter
    datavecs<-list()
    limvecs<-list()
    namevecs<-list()
    index<-numeric(0)
    counter<-0
  #loop through each col of ymat and xmat (at the same time)
    for (j in 1:ncol(ymat)){ 
      #keep track of which x and y cols you see
        counter<-j
        index<-c(index,counter)
      #put the x and y cols into a dataframe, put that dataframe into the correct list slot
        x<-xmat[,j]#x and y data saved in datavecs list
        y<-ymat[,j]
        data<-data.frame(x,y)
        colnames(data)<-c(xnames[j],ynames[j])
        datavecs[[j]]<-data
        xlim<-xlims[,j]#x and y lims saved in limvecs list
        ylim<-ylims[,j]
        lim<-data.frame(xlim,ylim)
        colnames(lim)<-c(xnames[j],ynames[j])
        limvecs[[j]]<-lim
        xn<-xnames[j]#x and y names saved in namevecs list
        yn<-ynames[j]
        n<-data.frame(xn,yn)
        namevecs[[j]]<-n
    }
  #reorganize the lists that were made above so that they are in consequtive slots
  #initialize lists  
    pairs<-list()
    lims<-list()
    ns<-list()
  #loop through each element in index  
    for (k in 1:length(index)){
      #use that index element to pull out a dataframe from the list, put that dataframe into the new list
      pair<-datavecs[index[k]]#x and y data saved in pair list 
      pairs[k]<-pair
      lim1<-limvecs[index[k]]#x and y data saved in lims list
      lims[k]<-lim1
      n<-namevecs[index[k]]#x and y data saved in ns list
      ns[k]<-n
    }
  #re-name the dataframes in each list with a panel#  
    names(pairs)<-paste('panel',seq(1,length(pairs),1))
    names(lims)<-paste('panel',seq(1,length(lims),1))
    names(ns)<-paste('panel',seq(1,length(ns),1))
  #compile lists into 1 list, prepxy2
    prepxy2<-list(pairs=pairs,lims=lims,ns=ns)
  return(prepxy2)
}

######################################################
#PrepXXY, created for analyzing metaanalysis trait x soilN data
######################################################
#What it does: It combines two dataframes, xmat and ymat.  It prepares paired xxy datasets where x1 and x2 columns are linked, e.g. X1a,X2a,Ya ; X1b,X2b,Ya ;.... (a,b for different x or y columns)
#What it needs: 
#(1) xmat1
#(2) xmat2
#(3) ymat
#(4) xnames1
#(5) xnames2
#(6) ynames
#What you get: A list (prepxxy) of 3 lists (pairs, lims, ns)

PrepXXY<-function(xmat1, xmat2, ymat, xnames1, xnames2, ynames){
  #initialize the lists, vectors, counter
    datavecs<-list()
    namevecs<-list()
    index<-numeric(0)
    counter<-0
  #loop through each col of ymat
    for (j in 1:ncol(ymat)){
    #loop through each col of xmat1 and xmat2 (at the same time)
      for (i in 1:ncol(xmat1)){ 
        #keep track of which x and y cols you see
        counter<-i+5*j
        index<-c(index,counter) #this keeps track of where the entries get slotted into the lists... will use 'index' in the next for-loop to condense the lists
        #put the x and y cols into a dataframe, put that dataframe into the correct list slot
        x1<-xmat1[,i]#xxy pair per panel
        x2<-xmat2[,i]
        y<-ymat[,j]
        data<-data.frame(x1,x2,y)
        colnames(data)<-c(xnames1[i],xnames2[i],ynames[j])
        datavecs[[counter]]<-data
        xn1<-xnames1[i]#xxy names per panel
        xn2<-xnames2[i]
        yn<-ynames[j]
        n<-data.frame(xn1,xn2,yn)
        namevecs[[counter]]<-n
      }
    }
  #reorganize the lists that were made above so that they are in consequtive slots
  #initialize lists  
    pairs<-list()
    ns<-list()
  #loop through each element in index  
    for (k in 1:length(index)){
      #use that index element to pull out a dataframe from the list, put that dataframe into the new list
      pair<-datavecs[index[k]]#xy pair per panel
      pairs[k]<-pair
      n<-namevecs[index[k]]#xy names per panel
      ns[k]<-n
    }
  #re-name the dataframes in each list with a panel#  
    names(pairs)<-paste('panel',seq(1,length(pairs),1))
    names(ns)<-paste('panel',seq(1,length(ns),1))
  #compile lists into 1 list, prepxxy
    prepxxy<-list(pairs=pairs,lims=lims,ns=ns)
  return(prepxxy)
}

######################################################
#PrepXXY2, created for analyzing metaanalysis trait x soilN data
######################################################
#What it does: It combines two dataframes, xmat and ymat.  It prepares paired xxy datasets where all X1, X2, and Y cols are unlinked; e.g. X1a,X2a,Ya ; X1b,X2a,Ya ; X1a,X2b,Ya ; X1a,X2a,Yb
#What it needs: 
#(1) xmat1
#(2) xmat2
#(3) ymat
#(4) xnames1
#(5) xnames2
#(6) ynames
#What you get: A list (prepxxy2) of 3 lists (pairs, lims, ns)

PrepXXY2<-function(xmat1, xmat2, ymat, xnames1, xnames2, ynames){
  #initialize the lists, vectors, counter
    datavecs<-list()
    namevecs<-list()
    index<-numeric(0)
    counter<-0
  #loop through each col of ymat
    for (j in 1:ncol(ymat)){ 
      #loop through each col of xmat1
      for (i in 1:ncol(xmat1)){ 
        #loop through each col of xmat2
        for(q in 1:ncol(xmat2)){ 
          #keep track of which x and y cols you see
            counter<-i+5*j+5*q
            index<-c(index,counter) #this keeps track of where the entries get slotted into the lists... will use 'index' in the next for-loop to condense the lists
          #put the x and y cols into a dataframe, put that dataframe into the correct list slot
            x1<-xmat1[,i]#xxy pair per panel
            x2<-xmat2[,q]
            y<-ymat[,j]
            data<-data.frame(x1,x2,y)
            colnames(data)<-c(xnames1[i],xnames2[q],ynames[j])
            datavecs[[i+5*j+5*q]]<-data
            xn1<-xnames1[i]#xxy names per panel
            xn2<-xnames2[q]
            yn<-ynames[j]
            n<-data.frame(xn1,xn2,yn)
            namevecs[[i+5*j+5*q]]<-n 
        }
      }
    } 
  #reorganize the lists that were made above so that they are in consequtive slots
  #initialize lists  
    pairs<-list()
    ns<-list()
  #loop through each element in index  
    for (k in 1:length(index)){
      #use that index element to pull out a dataframe from the list, put that dataframe into the new list
        pair<-datavecs[index[k]]#xy pair per panel
        pairs[k]<-pair
        n<-namevecs[index[k]]#xy names per panel
        ns[k]<-n
    }
  #re-name the dataframes in each list with a panel#  
    names(pairs)<-paste('panel',seq(1,length(pairs),1))
    names(ns)<-paste('panel',seq(1,length(ns),1))
  #compile lists into 1 list, prepxxy
    prepxxy2<-list(pairs=pairs,ns=ns)
  return(prepxxy2)
}

######################################################
#CleanXY, created for analyzing metaanalysis trait x soilN data
######################################################
#What it does: If an observation has an NA at X, then this fxn with replace the Y element for that observation with an NA.
#What it needs:
#(1) prepxy
#What you get: A list (cleanpairs) filled with xydata

CleanXY<-function(prepxy){
  pairs<-prepxy$pairs #extract the correct component of prepxy
  cleanpairs<-list()
  for (p in 1:length(pairs)){ #loop through each xy pair in the list 'pairs'
    xydata<-pairs[[p]]
    for (r in 1:dim(xydata)[1]){ #loop through each row of an xy pair
      if(is.na(xydata[r,1]) | is.na(xydata[r,2])){xydata[r,]<-c(NA,NA)} #if there is an NA is either cell of that row, then replace row with c(NA,NA)
    }
    cleanpairs[[p]]<-xydata #fill the new list 'cleanpairs' with each cleaned xy pair
  }
  names(cleanpairs)<-paste('panel',seq(1,length(pairs),1))
  return(cleanpairs)
}

######################################################
#LmFitTab, created for analyzing metaanalysis trait x soilN data
######################################################
#What it does: It consolidates regression info from a fit summary for/from plotting
#What it needs: 
#(1) fit<-lm(y~x)
#What you get: A table (coeftab) with rounded coefficients and r2 value 

LmFitTab<-function(fit){
  coefs<-summary(fit)$coefficients
  coefmat<-round(coefs, digits=2)
  r2<-c(round(summary(fit)$r.squared, digits=2),NA,NA,NA)
  coeftab<-rbind(coefmat,r2)
  return(coeftab)
}

#fit0<-lm(y~x)
#fit1<-lm(y~x+offset(x))
LmFitTab.slope01<-function(fit0,fit1){
  
  #table based on y~x (fit0)
  coefs<-summary(fit0)$coefficients
  coefmat<-round(coefs, digits=2)
  r2<-c(round(summary(fit0)$r.squared, digits=2),NA,NA,NA)
  
  #pull out p-val for testing if the slope differs from 1 (fit1)
  coefs1<-summary(fit1)$coefficients
  pval1<-c(round(coefs1[2,4], digits=2),NA,NA,NA)
  
  #put all of it together in a table
  coeftab<-rbind(coefmat,r2,pval1)
  return(coeftab)
}

######################################################
#DataSum, created for analyzing metaanalysis trait x soilN data
######################################################
#What it does: It evaluates the relationship between an xy pair.  Summary statistics for x and y are computed; linear fit y~x is tested, error bar values are computed for x and y.
#What it needs:
#(1) d (col1 is x data, col2 is y data, rows are observations)
#(2) l (col1 is xlims, col2 is ylims, row1 is min, row2 is max)
#(3) n (element1 is x name, element2 is yname)
#What you get: a list (dat) of many elements useful for plotting x and y

DataSum<-function(d,l,n){
  #establish elements of the dataset from d, l, n
    x<-d[,1]#d = x and y data
    y<-d[,2]
    data<-data.frame(x,y)
    xlims<-l[,1]#l = x and y lims
    ylims<-l[,2]
    xname<-n[1]#n = x and y names
    yname<-n[2]
  #summarize the dataset d: min, max, mean, se, n
    summat<-matrix(data= c(range(x, na.rm=T), mean(x, na.rm=T), sd(x,na.rm=T)/sqrt(sum(!is.na(x))), sum(!is.na(x)),
                           range(y, na.rm=T), mean(y, na.rm=T), sd(y,na.rm=T)/sqrt(sum(!is.na(y))), sum(!is.na(y))),
                   nrow=2, ncol=5, byrow=T)
    colnames(summat)<-c('min','max','mean','se','n')
    rownames(summat)<-c('x','y')
  #evalutate lm fit for dataset d
    fit<-lm(y~x)
    coeftab<-LmFitTab(fit)  
  #for error bars
    mx<-mean(x)
    sex<-sd(x)/sqrt(sum(!is.na(x)))
    my<-mean(y)
    sey<-sd(y)/sqrt(sum(!is.na(y)))
  #compile info into a big list (dat)
    dat<-list(x=x,y=y,data=data,
              xlims=xlims,ylims=ylims,
              xname=xname,yname=yname,
              summat=summat,
              coeftab=coeftab, fit=fit)
    return(dat)
}

#includes a test of slope=1...

DataSum.slope01<-function(d,l,n){
  #establish elements of the dataset from d, l, n
  x<-d[,1]#d = x and y data
  y<-d[,2]
  data<-data.frame(x,y)
  xlims<-l[,1]#l = x and y lims
  ylims<-l[,2]
  xname<-n[1]#n = x and y names
  yname<-n[2]
  #summarize the dataset d: min, max, mean, se, n
  summat<-matrix(data= c(range(x, na.rm=T), mean(x, na.rm=T), sd(x,na.rm=T)/sqrt(sum(!is.na(x))), sum(!is.na(x)),
                         range(y, na.rm=T), mean(y, na.rm=T), sd(y,na.rm=T)/sqrt(sum(!is.na(y))), sum(!is.na(y))),
                 nrow=2, ncol=5, byrow=T)
  colnames(summat)<-c('min','max','mean','se','n')
  rownames(summat)<-c('x','y')
  #evalutate lm fit for dataset d
  fit0<-lm(y~x)
  fit1<-lm(y~x+offset(x))
  coeftab<-LmFitTab.slope01(fit0=fit0, fit1=fit1)  
  #for error bars
  mx<-mean(x)
  sex<-sd(x)/sqrt(sum(!is.na(x)))
  my<-mean(y)
  sey<-sd(y)/sqrt(sum(!is.na(y)))
  #compile info into a big list (dat)
  dat<-list(x=x,y=y,data=data,
            xlims=xlims,ylims=ylims,
            xname=xname,yname=yname,
            summat=summat,
            coeftab=coeftab, fit=fit0)
  return(dat)
}

######################################################
#DataSum2, created for analyzing metaanalysis trait x soilN data
######################################################
#What it does: It evaluates the relationship between an xxy pair.  Summary statistics for x1, x2, and y are computed; linear fit y~x1*x2 is tested, error bar values are computed for x1,x2, and y.
#What it needs: 
#(1) d
#(2) n
#What you get: a list (dat) of many elements useful for plotting x1, x2, and y

DataSum2<-function(d,n){
  #establish elements of the dataset from d, n
    x1<-d[,1]#d = x1, x2, y data
    x2<-d[,2]
    y<-d[,3]
    data<-data.frame(x1,x2,y)
    xname1<-n[1]#n = x and y names
    xname2<-n[2]
    yname<-n[3]
  #summarize the dataset d: min, max, mean, se, n
    summat<-matrix(data= c(range(x1, na.rm=T), mean(x1, na.rm=T), sd(x1,na.rm=T)/sqrt(sum(!is.na(x1))), sum(!is.na(x1)),
                           range(x2, na.rm=T), mean(x2, na.rm=T), sd(x2,na.rm=T)/sqrt(sum(!is.na(x2))), sum(!is.na(x2)),
                           range(y, na.rm=T), mean(y, na.rm=T), sd(y,na.rm=T)/sqrt(sum(!is.na(y))), sum(!is.na(y))),
                   nrow=3, ncol=5, byrow=T)
    colnames(summat)<-c('min','max','mean','se','n')
    rownames(summat)<-c('x1','x2','y')
  #evalutate lm fit for dataset d
    fit<-lm(y~x1*x2)
    coeftab<-LmFitTab(fit)
  #compile info into a big list (dat)
  dat<-list(x1=x1,x2=x2,y=y,data=data,
            xname1=xname1,xname2=xname2,yname=yname,
            summat=summat,
            coeftab=coeftab, fit=fit)
  return(dat)
}

######################################################
#PlotDat, created for analyzing metaanalysis trait x soilN data
######################################################
#What it does: It plots xy pairs stored in dat
#What it needs: 
#(1) dat (product of DataSum fxn)
#(2) Labels (for labeling the points)
#(3) subsetindx (vector of items by which to subset the data via different colors, should be the same length as the number of rows)
#What you get: a plot that can be a part of a larger multipanel plot, can include regression lines, panel labels, etc
#(a) grouped panels of 3 rows x 4 columns
#(b) x and y error bars
#(c) dotted y=0 line
#(d) linear fit if p<0.1

PlotDat<-function(dat,Labels,subsetindx){
  #assign values
  x<-dat$x
  y<-dat$y
  xlims<-dat$xlims
  ylims<-dat$ylims
  xname<-dat$xname$xn
  yname<-dat$yname$yn
  coeftab<-dat$coeftab
  fit<-dat$fit
  
  #set up the plot
  plot(y~x, type='n', axes = F,xlim=xlims, ylim=ylims,xlab=xname,ylab=yname) 
  box()
  
  #add points
  colors<-seq(from=3,length.out=length(unique(subsetindx)))
  k<-0
  for (k in 1:length(unique(subsetindx))){
    text(y=y[subsetindx==(unique(subsetindx)[k])],
         x=x[subsetindx==(unique(subsetindx)[k])], 
         labels=Labels[subsetindx==(unique(subsetindx)[k])], cex=.8, col=colors[k])
  }
  #text(y=y,x=x,labels=Labels, cex=.8) #add 'points'
  #points(y=y,x=x, type='p',pch=16,cex=.6) #add 'points'
  #points(y=mean(y,na.rm=T), x=mean(x,na.rm=T),type='p',pch=1, cex=1, col=2) #add a mean point for each panel
  
  #add error bars
  mx<-mean(x,na.rm=T)
  sex<-sd(x,na.rm=T)/sqrt(sum(!is.na(x)))
  my<-mean(y,na.rm=T)
  sey<-sd(y,na.rm=T)/sqrt(sum(!is.na(y)))
  #y axis error bar
  segments(x0=mx,y0=my+sey,x1=mx,y1=my-sey,col=2,lty=1,lwd=2) 
  #x axis error bar
  segments(x0=mx+sex,y0=my,x1=mx-sex,y1=my,col=2,lty=1,lwd=2) 
  
  #add panel letters
  #mtext(letters[i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  
  #add horizontal line
  abline(a=0,b=1, lty=1)
  
  #add regression lines
  if(!is.na(coeftab[2,4]) & coeftab[2,4]<0.1){
    abline(fit, lty=2, col=2)
    mtext(paste('r^2 = ',coeftab[3,1]), #r2 value
          side = 3, line = -1.5, adj = 0.05, cex=.6, col=2)
    mtext(paste('slope = ',coeftab[2,1]), #p value
          side = 3, line = -2.5, adj = 0.05, cex=.6, col=2)
    mtext(paste('p0 = ',coeftab[2,4]), #p value
          side = 3, line = -3.5, adj = 0.05, cex=.6, col=2)
    mtext(paste('p1 = ',coeftab[4,1]), #p value
          side = 3, line = -4.5, adj = 0.05, cex=.6, col=2)
  }
  
  #add axis labels
  axis(1)
  axis(2)
  #FOR multi-panel plots...
  #if (i %in% c(9,10,11,12)){  axis(1) mtext(xname, side = 1, outer = F, cex = .8, adj= 0.55, line=2.2)}
  #LABELS y-axis
  #if (i %in% c(1,5,9)){ axis(2) mtext(yname, side = 2, outer = F, cex = .8, line=2.2) }
  
}

######################################################
#PlotDat2, created for analyzing metaanalysis trait x soilN data
######################################################
#What it does: It plots xy pairs stored in dat
#What it needs: 
#(1) dat
#What you get: a plot that can be a part of a larger multipanel plot, can include regression lines, panel labels, etc
#Set up for...
#(a) grouped panels of 3 rows x 1 columns
#(b) x and y error bars
#(c) dotted 1:1 line
#(d) linear fit if p<0.1

PlotDat2<-function(dat){
  #assign values  
    x<-dat$x
    y<-dat$y
    xlims<-dat$xlims
    ylims<-dat$ylims
    xname<-dat$xname$xn
    yname<-dat$yname$yn
    coeftab<-dat$coeftab
    fit<-dat$fit
  #set up the plot
    plot(y~x, type='n', axes = F,
         xlim=xlims, 
         ylim=ylims,
         xlab=xname,
         ylab=yname) 
    box()
  #add points
    text(y=y,x=x,labels=initials, cex=.8) #add 'points'
    #points(y=y,x=x, type='p',pch=16,cex=.6) #add 'points'
    #points(y=mean(y,na.rm=T), x=mean(x,na.rm=T),type='p',pch=1, cex=1, col=2) #add a mean point for each panel
  #add error bars
    mx<-mean(x,na.rm=T)
    sex<-sd(x,na.rm=T)/sqrt(sum(!is.na(x)))
    my<-mean(y,na.rm=T)
    sey<-sd(y,na.rm=T)/sqrt(sum(!is.na(y)))
  #y axis error bar
    #segments(x0=mx,y0=my+sey,x1=mx,y1=my-sey,col=2,lty=1,lwd=2) 
  #x axis error bar
    #segments(x0=mx+sex,y0=my,x1=mx-sex,y1=my,col=2,lty=1,lwd=2) 
  #add panel letters
    #mtext(letters[i], side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  #add 1:1 line
    abline(a=0,b=1, lty=2)
  #add regression lines
    if(!is.na(coeftab[2,4]) & coeftab[2,4]<0.1){
      abline(fit, lty=1)
      #mtext(paste('r^2 = ',coeftab[3,1]), #r2 value
      #     side = 3, line = -1.5, adj = 0.95, cex=.6)
      #mtext(paste('slope = ',coeftab[2,1]), #p value
      #     side = 3, line = -2.5, adj = 0.95, cex=.6)
      #mtext(paste('p = ',coeftab[2,4]), #p value
      #     side = 3, line = -3.5, adj = 0.95, cex=.6)
    }
  #FOR multi-panel plots...
  #LABELS x-axis
  if (i %in% c(1,2,3,4,5,6)){ 
    axis(1)
    #mtext(xname, side = 1, outer = F, cex = .8, adj= 0.55, line=2.2)
  }
  #LABELS y-axis
  if (i %in% c(1,2,3,4,5,6)){ 
    axis(2)
    #mtext(yname, side = 2, outer = F, cex = .8, line=2.2)
  }
}

######################################################
#ListToDF, created for analyzing metaanalysis trait x soilN data
######################################################
#What it does: Converts a list of dataframes to one dataframe by stacking dataframe rows together; rownames are renamed to reflect the list element names
#What it needs: 
#(1) list = list of dataframes with the same dimensions
#What you get: a long dataframe with new rownames (df)

ListToDF<-function(list){
  #scwish the list dataframes together with rbind
    df<-do.call("rbind", list)
  #rename the rows
    nr.vec<-numeric(0)
    for (i in 1:length(list)){ #loop through each item in the list
        ld<-list[[i]]
        nr<-dim(ld)[1] #count the number of rows
        nr.vec<-c(nr.vec,nr)
    }
    row.names(df)<-paste(rep(1:length(list), nr.vec),row.names(df))
  return(df)
}

######################################################
#PrunePlot
######################################################
#What it does: Pruning and plots an rpart regression tree
#What it needs: 
#(1) tree
#(2) ycol
#(3) ylabel
#What you get: plot and a pruned tree (pruned.tree)

PrunPlot<-function(tree, ycol, ylabel){
  #prune
    mincp<-which.min(tree$cptable[,"xerror"])
    cp.choice<-min(tree$cptable[mincp,"CP"])
    pruned.tree<-prune(tree, cp=cp.choice)
  #extract tree parameters
    rn<-rownames(pruned.tree$frame) #the rownames of the frame identify the horizontal coordinates of leaves
    lev<-rn[sort(unique(pruned.tree$where))] #match rownames with where elements
    where<-factor(rn[pruned.tree$where],levels=lev) #create a factor variable that identifies the leaves
    n<-tapply(ycol,where,length)#count number of observations at each leaf
  #plot
    par(mfrow=c(2,1),mar=c(1.1,4.1,1.1,1.1))
    plot(pruned.tree,margin=.2)
    text(pruned.tree,cex=.8,pretty=1, digits=3)
    par(mar=c(2.1,4.1,0.1,1.1))
    boxplot(ycol~where,varwidth=T,pars=list(axes=F))
    axis(2)
    box()
    mtext(side=3,line=.5,text=paste('n=',n),at=1:length(lev),cex=.9)
    mtext(side=2,line=2,text=ylabel,cex=.9)
  return(pruned.tree)
}


