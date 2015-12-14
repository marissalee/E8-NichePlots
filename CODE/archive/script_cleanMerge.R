#script_cleanMerge.R

#IMPORT RAW DATA
soilData<-read.table("DATA/e8_plothalfSoilData.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
vegData<-read.table("DATA/e8_plothalfVegData.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
plotLoc<-read.table("DATA/e8_plotLoc.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
plotTrees<-read.table("DATA/e8_plotTrees.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
soilData.dict<-read.table("DATA/e8_plothalfSoilData_dictionary.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
vegData.dict<-read.table("DATA/e8_plothalfVegData_dictionary.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
plotLoc.dict<-read.table("DATA/e8_plotLoc_dictionary.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
plotTrees.dict<-read.table("DATA/e8_plotTrees_dictionary.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
timeline<-read.table("DATA/e8_timeline.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
phData<-read.table("DATA/e8_ph.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)


##############
#A. Clean and merge
##############

# SOIL DATA
soilData$plothalfid1<-paste(soilData$inv,soilData$plotid, sep="_") #add new identifiers
removeCols<-c('plothalfid','site','rep') #prune columns
soilData.pruned<-soilData[,!colnames(soilData) %in% removeCols]
#reshape so that each depth x meas has its own column
soilData_melted <- melt(soilData.pruned,
                        id.vars=c("plotid","plothalfid1",
                                  "inv","depth","year"),
                        variable.name = "measCat")
soilData_melted$measCat2<-paste(soilData_melted$measCat, soilData_melted$depth, sep = '_')
soilData_cast <- dcast(soilData_melted, plotid + plothalfid1 + inv + year ~ measCat2, value.var="value")
soilData_c<-soilData_cast 

# PH DATA
phData$plothalfid1<-paste(phData$inv, phData$plotid, sep="_") #add new identifiers
#average the 2 pH values
colnames(phData)
sum(is.na(phData[,c('ph_1','ph_2')])) #good.  none missing
phData$ph<- phData$ph_1 + phData$ph_2 /2
#reshape so that each depth x meas has its own column
phData_pruned<-phData[,c('year','plothalfid1','depth','plotid','inv','ph')]
phData_c <- dcast(phData_pruned, plotid + plothalfid1 + inv + year ~ depth, value.var="ph")
colnames(phData_c)<-c('plotid','plothalfid1','inv','year','ph_B','ph_F','ph_T')
#merge with soilData_c
soilData_c<-merge(soilData_c, phData_c)

# VEG DATA
vegData$plothalfid1<-paste(vegData$inv,vegData$plotid, sep="_") #add new identifiers
vegData.pruned<-vegData[,!colnames(vegData) %in% removeCols] #prune columns
vegData.pruned$total<-vegData.pruned$mv+vegData.pruned$nat #make a total understory biomass variable
vegData.dict[c(2,4),c('v7','v8','v9')]
paste('Dry biomass values are currently in units of g/.1875m2')
ConvertBiom<-function(currVal){newVal <- currVal / 0.1875}
vegData.pruned$mv_g.m2<-ConvertBiom(vegData.pruned$mv)
vegData.pruned$nat_g.m2<-ConvertBiom(vegData.pruned$nat)
vegData.pruned$litter_g.m2<-ConvertBiom(vegData.pruned$litter)
vegData.pruned$total_g.m2<-ConvertBiom(vegData.pruned$total)
colsOldUnits<-c('mv','nat','litter','total')
vegData_c<-vegData.pruned[,!colnames(vegData.pruned) %in% colsOldUnits] #prune columns

#add vegData
soilVegData<-merge(soilData_c, vegData_c) #merge soilData_cast and vegData

# TREE DATA
plotTrees$basalArea.m2<-(plotTrees$dbh * plotTrees$dbh) * 0.00007854 #calculate basal area/m2 from each tree's dbh value
#summarize the total basal area/m2 per plot and that which is made up by either AM- or ECM-associated trees
plotTrees.summ<-ddply(plotTrees, ~plotid, summarize,
                      nTrees=length(plotid),
                      BA_total=sum(basalArea.m2, na.rm=T),
                      BA_AM=sum(basalArea.m2[myc=='A'], na.rm=T),
                      BA_ECM=sum(basalArea.m2[myc=='E'], na.rm=T),
                      PercBA_AM=(BA_AM/BA_total)*100,
                      PercBA_ECM=(BA_ECM/BA_total)*100)
#update the number of trees (there was a cell that was counted even for plots where there were no trees
plotTrees.summ[plotTrees.summ$plotid %in% c(12,15),'nTrees']<-0
plotTrees.summ[plotTrees.summ$plotid %in% c(12,15),c('PercBA_AM','PercBA_ECM')]<-NA
tmp<-plotTrees.summ[,c('plotid','nTrees','BA_total','PercBA_AM')]
trees_c<-tmp

#add tree data 
data<-merge(soilVegData, trees_c) 

#get rid of the plots without trees
data<-data[data$nTrees != 0,]
#str(data)

#deal with missing values in percpar.. make a column of percpar that is aggregated by year
data$yearInv<-paste(data$inv, data$year, sep="_")
pDistb_percpar<-ggplot(data, aes(y=percpar, x=plotid, color=yearInv, linetype=inv)) +  geom_point() + geom_line() +
  scale_color_manual(name='Inv & Year', values=c('red','darkorange','yellow','blue','darkblue','black'))
pDistb_percpar_logt<-ggplot(data, aes(y=log(percpar), x=plotid, color=yearInv, linetype=inv)) +  geom_point() + geom_line() +
  scale_color_manual(name='Inv & Year', values=c('red','darkorange','yellow','blue','darkblue','black'))
#save plots
newfilename<-'pDistb_percpar'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*4, height = fig.height*4, res=fig.res)
pDistb_percpar
dev.off()
#save plot
newfilename<-'pDistb_percpar_logt'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*4, height = fig.height*4, res=fig.res)
pDistb_percpar_logt
dev.off()
#decided to aggregate by year so that each plothalfid1 has 1 percpar value for last 2 years
data.sub<-subset(data, year %in% c(2012,2013))
summ.percpar<-ddply(data, ~plotid+inv, summarize,
      meanV=mean(percpar, na.rm=T),
      sdV=sd(percpar, na.rm=T),
      nV=length(year),
      seV=sdV/sqrt(nV),
      ymin=meanV-seV,
      ymax=meanV+seV)
colnames(summ.percpar)[colnames(summ.percpar)=='meanV']<-'percpar.AggY'
data<-merge(data, summ.percpar[,c('plotid','inv','percpar.AggY')])


##############
#B. Deal with soil N data in years where 0-5cm and 5-15cm layers were measured separately 
##############
# Plot histogram of values by depth

#subset by year
data.1213<-subset(data, year %in% c(2012, 2013))

#reshape
data.1213_melted <- melt(data.1213,id.vars=c("plotid","plothalfid1","inv","year"))
data.1213_melted$value<-as.numeric(data.1213_melted$value)

#subset by _B and _T
data.1213_melted_soilVars<-data.1213_melted[grepl("_B",data.1213_melted$variable) | grepl("_T",data.1213_melted$variable),]

#add back the separate measCat and depth columns
tmp<-ldply(strsplit(as.character(data.1213_melted_soilVars$variable),"_", fixed=T))
colnames(tmp)<-c('measCat','depth')
data.1213_melted_soilVars<-cbind(data.1213_melted_soilVars, tmp)

#correct the dataframe structure
data.1213_melted_soilVars$measCat<-factor(data.1213_melted_soilVars$measCat, levels=measCat_FULL_order)
data.1213_melted_soilVars$depth<-factor(data.1213_melted_soilVars$depth, levels=depth_order)
data.1213_melted_soilVars$inv<-factor(data.1213_melted_soilVars$inv)



#plot
pHist.depth<-ggplot(data.1213_melted_soilVars, aes(x=value, fill=depth)) + 
  geom_bar() + mytheme + facet_wrap(measCat~year, scales='free') +
  ggtitle("Histogram of soil measurement values by soil depth and year")
#save plot
newfilename<-'pHist_depth.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*4, height = fig.height*4, res=fig.res)
pHist.depth
dev.off()


##############
#C. DATASET A : Aggregate values, put aggregated values into full dataset and replace empty _F rows
##############

#aggregate by depth
summ.data.1213_melted_soilVars<-ddply(data.1213_melted_soilVars, ~plotid+plothalfid1+inv+year+measCat, summarize,
                                      new.value=mean(value, na.rm=T),
                                      n=length(plothalfid1),
                                      note='aggregated by depth')

#make a dataset without the dis-aggregated data
data_melted <- melt(data,id.vars=c("plotid","plothalfid1","inv","year")) #melt down the full dataset, including 2011 data
data_melted$value<-as.numeric(data_melted$value) #make values numeric again
uniqWo.B<-unique(data_melted$variable)[!grepl("_B",unique(data_melted$variable))] #isolate only the variables that do NOT included the non-aggregated data
uniqWo.BT<-uniqWo.B[!grepl("_T",uniqWo.B)] #ditto
data_m_woBT<-data_melted[data_melted$variable %in% uniqWo.BT,] #subset data by 'correct' variables
selectvars<-unique(data_m_woBT$variable)[grepl("_F",unique(data_m_woBT$variable))] #select measCat variables with "_F"
data_m_woBT_e<-data_m_woBT[!(data_m_woBT$year %in% c(2012,2013) & data_m_woBT$variable %in% selectvars), ] #remove rows without data because it has not yet been aggregated
data_m_woBT_e$note<-NA

#make a dataset with the correctly-aggregated data and comparable column names to data_m_woBT
summ.data.1213_melted_soilVars<-rename(summ.data.1213_melted_soilVars, replace=c("new.value" = "value")) #rename new.value -> value
summ.data.1213_melted_soilVars$variable<-paste(summ.data.1213_melted_soilVars$measCat, '_F',sep="")#measCat items should be ammended with '_F' to indicate that the values are representative of the full soil core depth... put this in a new column called 'variable'
summ.data.1213_melted_soilVars<-summ.data.1213_melted_soilVars[,c('plotid','plothalfid1','inv','year','variable','value','note')]#get rid of 'n' and 'measCat' columns

#merge
data_a<-rbind(data_m_woBT_e, summ.data.1213_melted_soilVars)
#ddply(data_a, ~year+variable, summarize, n=length(plotid)) #check the dataframe structure
#ddply(data_a, ~inv+variable, summarize, n=length(plotid)) #check the dataframe structure

#add a column that categorizes the variable types
data_a$varType<-NA
data_a[grepl("_F", data_a$variable),'varType']<-'measCat'
data_a[data_a$variable %in% c('mv_g.m2','nat_g.m2','litter_g.m2','total_g.m2'),'varType']<-'understoryBiom'
data_a[data_a$variable %in% c('percpar','percpar.AggY','soiltemp','soilBasin'),'varType']<-'environParam'
data_a[data_a$variable %in% c('nTrees','BA_total','PercBA_AM'),'varType']<-'overstoryParam'

#add a column that identifies whether the variable type is measured on the plot half or the plot level
plotlevel.vars<-c('soilBasin','nTrees','BA_total','PercBA_AM')
plothalflevel.vars<-unique(data_a$variable)[!unique(data_a$variable) %in% plotlevel.vars]
data_a$measLevel<-NA
data_a[data_a$variable %in% plotlevel.vars,'measLevel']<-'plot'
data_a[data_a$variable %in% plothalflevel.vars,'measLevel']<-'plothalf'

#Ammend variable names with "_N" or "_I" depending on the plothalf
data_a$variable1<-paste(data_a$variable, data_a$inv, sep="_")
data_a[data_a$measLevel=='plot','variable1']<-as.character(data_a[data_a$measLevel=='plot','variable'])

#save and export
newfilename<-'data_a.txt'
write.table(data_a, file=paste(synthdataPath,newfilename, sep='/'), sep='\t')


##############
#D. DATASET D : Keep 2012 and 2013 values dis-aggregated and remove 2011 _F values, remove biomass values from 2011 since time point was past peak biomass
##############
#start with data.1213_melted
data_d<-data.1213_melted[!grepl("_F", data.1213_melted$variable),] #remove variables that were only measured in 2011
data_d<-data_d[data_d$variable != 'soiltemp',] #remove variables that were only measured in 2011

#add a column that categorizes the depth types
data_d$depth<-NA
data_d[grepl("_B", data_d$variable),'depth']<-'B'
data_d[grepl("_T", data_d$variable),'depth']<-'T'

#add a column that categorizes the variable types
data_d$varType<-NA
data_d[grepl("_B", data_d$variable),'varType']<-'measCat'
data_d[grepl("_T", data_d$variable),'varType']<-'measCat'
data_d[data_d$variable %in% c('mv_g.m2','nat_g.m2','litter_g.m2','total_g.m2'),'varType']<-'understoryBiom'
data_d[data_d$variable %in% c('percpar','percpar.AggY'),'varType']<-'environParam'
data_d[data_d$variable %in% c('nTrees','BA_total','PercBA_AM'),'varType']<-'overstoryParam'

#add a column that identifies whether the variable type is measured on the plot half or the plot level
plotlevel.vars<-c('nTrees','BA_total','PercBA_AM')
plothalflevel.vars<-unique(data_d$variable)[!unique(data_d$variable) %in% plotlevel.vars]
data_d$measLevel<-NA
data_d[data_d$variable %in% plotlevel.vars,'measLevel']<-'plot'
data_d[data_d$variable %in% plothalflevel.vars,'measLevel']<-'plothalf'

#Ammend variable names with "_N" or "_I" depending on the plothalf
data_d$variable1<-paste(data_d$variable, data_d$inv, sep="_")
data_d[data_d$measLevel=='plot','variable1']<-as.character(data_d[data_d$measLevel=='plot','variable'])

#save and export
newfilename<-'data_d.txt'
write.table(data_d, file=paste(synthdataPath,newfilename, sep='/'), sep='\t')


