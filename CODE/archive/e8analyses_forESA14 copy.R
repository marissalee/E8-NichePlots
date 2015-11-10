#E8 NichePlots
#ANALYSES (e8analyses_forESA14.R)
#need this txt file: 'e8all.txt'

#How to ordinate plots in nutrient economy space

#Last update: 2/25/14

#########################################################
#libraries
library(doBy) #summary stats
#library(car) #plotting
#library(vegan) #ordination
#library(labdsv) #ordination
#library(akima) #for interp fxn
#set directory and read-in the data
setwd("~/Desktop/E8_R_020914")
all<-read.table("e8all.txt",header=TRUE,sep="\t")
#functions
source('leeFunctions.R')
#########################################################
#check/fix data structure
#str(all) 
all$h<-as.factor(all$h)
all$plothalfid<-as.factor(all$plothalfid)
all$plotid<-as.factor(all$plotid)
####
#exclude esp high plant biomass sample in row 21
#which(all$mv==max(all$mv,na.rm=T))
#which(all$nat==max(all$nat,na.rm=T))
#all[21,c(12,13,14,15)]<-NA #replace mv, nat, litter, and sticks with NA for that observation - go back and check the data for this.
####
#ADD COLUMNS to 'all', mv+nat=total
all$total<-all$mv + all$nat
#ADD COLUMNS to 'all' to organize percam
sum <- summarySE(data=all,measurevar="percam", groupvars=c("plotid")) # summarySE provides the standarddeviation, standard error of the mean
sumBypercam<-orderBy(~percam,sum)
#add a column to 'all' that orders the plotids by percam
vec<-rep(0,dim(all)[1]);num<-0 #make a vec that is the right length and initialize 'num'
for (i in (1:length(vec))){ #loop through each element in the vector, aka each row in 'all'
  num<-which(sumBypercam$plotid==all[i,'plotid']) #based on the plotid of that row, assign a num that corresponds to the order of that plot in ascending percam
  vec[i]<-num #store the num in the vec
}
all$plotorder<-vec
#add a column to 'all' that bins plots by percam
vec<-rep(NA,dim(all)[1]);fac<-NA #make a vec that is the right length and initialize 'fac'
for (i in (1:length(vec))){ #loop through each element in the vector, aka each row in 'all'
  if(is.na(all[i,'percam'])) fac<-'None'
  if(!is.na(all[i,'percam']) & all[i,'percam']==0) fac<-'ECM'
  if(!is.na(all[i,'percam']) & all[i,'percam']==100) fac<-'AM'
  if(!is.na(all[i,'percam']) & all[i,'percam']>0 & all[i,'percam']<100) fac<-'Mix'
  vec[i]<-fac #store the fac in the vec
}
all$percamt<-vec
####
#ADD COLUMNS to 'all' to organize mv biomass in inv=I
#Isolate mv in inv=T 
invd<-subset(all,inv=='I')
invvec<-invd$mv
#add a column to 'all' that identifies the mv value for each row
vec<-rep(invvec,each=2)
length(vec); dim(all)[1]
all$mvinv<-vec
dim(all)
####
#calculate the weighted avg of 0-5cm and 5-15cm layers and insert this into the full dataset
wt<-5/15 #5cm depth sample / 15cm depth total
wb<-10/15 #10cm depth sample / 15cm depth total
vart<-all[,30:35]; dim(vart)
varb<-all[,24:29]; dim(varb)
varf<-mat.or.vec(dim(vart)[1],dim(vart)[2])
for (i in 1:dim(vart)[2]){
  varf[,i]<-(vart[,i]*wt)+(varb[,i]*wb)  
}
range(which(is.na(varf[,1]))) #figure out which rows have NAs, because these are all h==2011 rows
all[all$h==2012 | all$h==2013,18:23]<-varf[-(1:36),] #insert varf into appropriate cells of 'all'

View(all)

#########################################################
#Right now, use only 2012 data and data that is from native subplots
nd12<-all[all$h==2012 & all$inv=='N',]

#a. isolate potential Nutrient Economy variables and construct and index
nep<-c('percpar','litter',colnames(nd12)[18:35],'percam'); length(nep) #15 variables
names_nep<-c('PAR (%)','Litter (g/.1875m2)',
             'NH4 (ug/g) 0-15cm','NO3 (ug/g) 0-15cm','Nitrif.(ug/g*h) 0-15cm','Mineraliz.(ug/g*h) 0-15cm','Soil Moisture (%) 0-15cm','SOM (%) 0-15cm',
             'NH4 (ug/g) 5-15cm','NO3 (ug/g) 5-15cm','Nitrif.(ug/g*h) 5-15cm','Mineraliz.(ug/g*h) 5-15cm','Soil Moisture (%) 5-15cm','SOM (%) 5-15cm',
             'NH4 (ug/g) 0-5cm','NO3 (ug/g) 0-5cm','Nitrif.(ug/g*h) 0-5cm','Mineraliz.(ug/g*h) 0-5cm','Soil Moisture (%) 0-5cm','SOM (%) 0-5cm',
             'AM Trees (%)'); length(names_nep) #15 variables
layer<-c(rep(NA,2),rep('full',6),rep('bottom',6),rep('top',6),NA); length(layer)
ind.nep<-data.frame(nep,names_nep,layer)

#b. do the selected ordination (see e8analyses022514.R for more details)
#(14) Full soil layer, Exclude no-tree plots (include percam), Exclude percpar, Exclude litter
  plotname<-"full layer, tree plots, exclude percPAR, exclude litter"
  notreeplots<-which(is.na(nd12$percam)); nd.tmp<-nd12[-notreeplots,]; notreeplots 
  Labels.treeplots<-nd.tmp$plotname; length(Labels.treeplots) 
  exvarsTBCL<-c(which(ind.nep$layer=='top'), 
                which(ind.nep$layer=='bottom'),
                which(ind.nep$nep=='percpar'),
                which(ind.nep$nep=='litter'),
                which(ind.nep$nep=='soilmoif')); length(exvarsTBCL)
  newnep<-nep[-exvarsTBCL]; length(newnep)
  nd12.14<-nd12[-notreeplots,newnep]; dim(nd12.14)
  View(nd12.14)
  data<-nd12.14
  Labels<-Labels.treeplots 
  center<-T; scale<-T; sfactor<-1.8; spush<-1.1
#ordinate
  mydata.pca <- prcomp(data, retx=TRUE, center=center, scale.=scale)
  summary<-summary(mydata.pca)$importance
  xpropvar<-round(summary[2,1]*100, digits=2)
  ypropvar<-round(summary[2,2]*100, digits=2)
  sd <- mydata.pca$sdev #standard deviation
  loadings <- mydata.pca$rotation #loadings
  rownames(loadings) <- colnames(data) #loadings per variable
  scores <- mydata.pca$x #scores per observation
#distance biplot
  xlim<-c(min(scores[,1])*1.2, max(scores[,1])*1.2)
  ylim<-c(min(scores[,2])*1.2, max(scores[,2])*1.2)
  plot(scores[,1], scores[,2], 
       xlab=paste("PCA 1 ",xpropvar,"%"), 
       ylab=paste("PCA 2 ",ypropvar,"%"), 
       type="n", 
       xlim=xlim,ylim=ylim)
  arrows(0,0,loadings[,1]*sfactor,loadings[,2]*sfactor, length=0.1,angle=20, col="red") # scaling factor (sfactor=8) of may need to be changed depending on the data set
  text(loadings[,1]*sfactor*spush,loadings[,2]*sfactor*spush,rownames(loadings), col="red", cex=0.7) # additional scaling factor (spush=1.2) insures that labels are plotted just beyond the arrows
  text(scores[,1],scores[,2], Labels, col="blue", cex=0.7)
  #mtext(plotname, side=3)
  dimtext<-paste(dim(data)[1],'plots,',dim(data)[2],'Variables')
  mtext(dimtext, side=4)

#c. Take plot scores from pca-1 (49% of the variance) and plot mv biomass (y) against these values (x)
pcascores<-scores[,1]; length(pcascores)
nd12.trees<-nd12[-notreeplots,]; dim(nd12.trees)
x<-pcascores
y<-nd12.trees$mvinv
plot(y~x, ylim=c(0,30),
     xlab=paste("PCA 1 ",xpropvar,"%"),
     ylab='Mv biomass')
lm.fit<-lm(y~x)
abline(lm.fit, lty=2) #not significant
identify(x,y, labels=nd12.trees$plotname)

#d. Take plot scores from pca-1 (49% of the variance) and plot impacts (y) against these values (x)
#subset by inv/nat
nd12<-all[all$h==2012 & all$inv=='N',]
id12<-all[all$h==2012 & all$inv=='I',]
#View(nd12);View(id12)
#pick soil variables of interest and subset data
sparm<-colnames(id12)[c(13,14,18:36)]
inv<-nd12[,sparm]
nat<-id12[,sparm]
#View(inv);View(nat)
min(inv, na.rm=T);min(nat, na.rm=T)
#calculate response ratio log(inv)/log(nat)
inv1<-log(inv+1);nat1<-log(nat+1)
#View(inv1);View(nat1)
rr<-inv1-nat1
#View(rr)
#create dataframe with response ratios and percam so that they can be plotted against one another
length(pcascores)
nd12.trees<-nd12[-notreeplots,]; dim(nd12.trees)
identivars<-nd12.trees[,c(1:5,37:39)]; dim(identivars)
rr.trees<-rr[-notreeplots,]; dim(rr.trees)
df<-data.frame(identivars,pcascores,rr.trees); dim(df)
#plot parameters
ynames<-colnames(df)[c(10:17,24:30)]; length(ynames)
tmp<-c('Native biomass (g)','Litter (g)',
       'NH4 (ug/G) 0-15cm','NO3 (ug/G) 0-15cm','Nitrif. (ug/G*h) 0-15cm','Minz. (ug/G*h) 0-15cm','Soil moisture (%) 0-15cm','SOM (%) 0-15cm',
       'NH4 (ug/G) 0-5cm','NO3 (ug/G) 0-5cm','Nitrif. (ug/G*h) 0-5cm','Minz. (ug/G*h) 0-5cm','Soil moisture (%) 0-5cm','SOM (%) 0-5cm',
       'Aboveground biomass (g)'); length(tmp)
ylabs<-paste(tmp,'Log RR')
ys<-df[,ynames]; dim(ys)
xlims<-c(-2.5,5.5)
x<-df$pcascores
#plot
p<-0
m.summaries<-numeric(0)
y.summaries<-numeric(0)
for (p in 1:dim(ys)[2]){ #loop through each y (col in ys)
  #par(mfrow = c(1, 1),cex = 0.8, mar = c(0, 4, 0, 0), oma = c(5, 5, 0.5, 0.5), tcl = -0.25, mgp = c(2, 0.6, 0),xpd = NA)
  #start save plot
  #SavePlot(name=ynames[p],width=300,height=300)
  #assign y
    yname<-ynames[p] #y name
    currenty<-df[,as.character(yname)] #y data
    currentylab<-ylabs[p]  #y labels
    ymin<-min(currenty,na.rm=T)-min(currenty,na.rm=T)*.5
    ymax<-max(currenty,na.rm=T)+max(currenty,na.rm=T)*.5
    currentylims<-c(ymin,ymax) #y limits
  #plot
   plot(currenty ~ x,
        xlab = "PCA 1 score", ylab = currentylab,
        xlim=xlims, ylim=currentylims) 
    lines(x=c(-3,6),y=c(0,0), lty=2)
  #test if the y values differ from 0
    ytest<-t.test(currenty,mu=0) 
    y.pval<-ytest$p.value
    y.mean<-mean(currenty,na.rm=T); y.n<-length2(currenty); y.se<-sd(currenty,na.rm=T)/sqrt(y.n)
    y.summary<-data.frame(yname,y.mean,y.se,y.n,y.pval)
    y.summaries<-rbind(y.summaries,y.summary)
  #test if the slope differs from 0
    lmfit<-lm(currenty~x); mtest<-anova(lmfit)
    m.pval<-mtest[1,5]; m.coefs<-lmfit$coefficients; intercept<-m.coefs[1]; slope<-m.coefs[2]
    m.summary<-data.frame(xname='pca1 score',yname,intercept,slope,m.pval)
    m.summaries<-rbind(m.summaries,m.summary)
  #ammend the plot based on test results
    if(y.pval<0.1){
      mtext(paste('y.mean=',round(y.mean, digits=2), 
                '; y.se=',round(y.se, digits=2),
                '; y.pval=',round(y.pval, digits=4))
          ,side=3,line=-1.5, adj=0.95)
    }
    if(m.pval<0.1){
      abline(lm(currenty~x))
      mtext(paste('m=',round(slope,digits=2), '; m.pval=',round(m.pval,digits=4)),
            side=3,line=-2.5, adj=0.95)
    }
  identify(x,currenty, labels=nd12.trees$plotname)
  #end save plot
    #dev.off()
}
m.summaries
y.summaries
