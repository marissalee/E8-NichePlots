
load_ph <- function(){
  
  phData<-read.table("DATA/e8_ph.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
  phData$plothalfid1<-paste(phData$inv, phData$plotid, sep="_") #add new identifiers
  #average the 2 pH values
  sum(is.na(phData[,c('ph_1','ph_2')])) #good.  none missing
  phData$ph<- (phData$ph_1 + phData$ph_2) /2

  phData %>%
    select(year, plothalfid1, depth, plotid, inv, ph) %>%
    spread(key = depth, value = ph) %>%
    select(-c(F)) %>%
    filter(year != 2011) %>%
    rename('ph_B'='B',
           'ph_T'='T') -> ph
  
  return(ph)
  
}

load_veg <- function(){
  
  vegData<-read.table("DATA/e8_plothalfVegData.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
  vegData.dict<-read.table("DATA/e8_plothalfVegData_dictionary.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
  
  removeCols<-c('plothalfid','site','rep') #prune columns
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
  
  #remove 2011 data
  vegData_c %>%
    filter(year != 2011) -> veg
  
  return(veg)
}

load_trees <- function(){
  
  plotTrees.dict<-read.table("DATA/e8_plotTrees_dictionary.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
  plotTrees<-read.table("DATA/e8_plotTrees.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
  
  plotTrees$basalArea.m2<-(plotTrees$dbh * plotTrees$dbh) * 0.00007854 #calculate basal area/m2 from each tree's dbh value
  #summarize the total basal area/m2 per plot and that which is made up by either AM- or ECM-associated trees
  plotTrees %>%
    group_by(plotid) %>%
    summarize(nTrees=length(plotid),
              BA_total=sum(basalArea.m2, na.rm=T),
              BA_AM=sum(basalArea.m2[myc=='A'], na.rm=T),
              BA_ECM=sum(basalArea.m2[myc=='E'], na.rm=T),
              PercBA_AM=(BA_AM/BA_total)*100,
              PercBA_ECM=(BA_ECM/BA_total)*100) -> plotTrees.summ
  
  #update the number of trees (there was a cell that was counted even for plots where there were no trees
  plotTrees.summ[plotTrees.summ$plotid %in% c(12,15),'nTrees']<-0
  plotTrees.summ[plotTrees.summ$plotid %in% c(12,15),c('PercBA_AM','PercBA_ECM')]<-NA
  tmp<-plotTrees.summ[,c('plotid','nTrees','BA_total','PercBA_AM')]
  trees_c<-tmp
  
  return(trees_c)
}

load_soilmoiOM <- function(){
  
  soilmoiom.dict<-read.table("DATA/e8_soilmoiOM_dictionary.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
  soilmoiom <- read.table("DATA/e8_soilmoiOM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
  
  soilmoiom %>%
    mutate(plothalfid1 = paste(inv, plotid, sep = "_")) %>%
    select(-c(plothalfid, site, rep)) -> soilmoiom
  
  return(soilmoiom)
  
}

load_soiln <- function(){
  
  minz.dict<-read.table("DATA/e8_minz_dictionary.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
  minz12 <- read.table("DATA/e8_summ12_minz.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
  minz13 <- read.table("DATA/e8_summ13_minz.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
  
  # replace below detection (BD) with 0.0001
  minz12[minz12 == "BD"] <- 0.0001
  minz13[minz13 == "BD"] <- 0.0001
  
  # get plotid index
  soilmoiom <- read.table("DATA/e8_soilmoiOM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
  plot.indx <- unique(soilmoiom[,c("plotid","plothalfid","inv")])
  
  # clean 12
  minz12 %>%
    rename('plothalfid'='plotid') %>% # this was mislabeled
    left_join(plot.indx) %>%
    mutate(year = 2012) %>%
    select(-c(site, rep)) %>%
    mutate(batch = "all12") -> minz12
  minz12[, c(4:9)] <- sapply(minz12[, c(4:9)], as.numeric)
  
  # reshape 13
  minz13 %>%
    mutate(year = 2013) %>%
    left_join(plot.indx) %>%
    select(-c(plothalfid)) -> minz13
  minz13[, c(3:8)] <- sapply(minz13[, c(3:8)], as.numeric)
  
  # pull years together into 1 df
  minz13 %>%
    full_join(minz12) -> minz
  
  # calculate mineralization rate on a per day basis
  minz %>%
    mutate(ammonifd = (nhf - nhi) / incubation_days) %>%
    mutate(nitrifd = (nof - noi) / incubation_days) %>%
    mutate(minzd = (totf - toti) / incubation_days) -> minz
  
  # remove 2013 batch 1 and use batch 1 redo instead -- many of the initial nitrate measurements were BD
  minz %>%
    filter(batch != "1") -> minz
  
  # subset for key cols
  minz %>%
    select(plotid, inv, year, depth, nhi, noi, ammonifd, nitrifd, minzd) %>%
    gather(key = "variable", value = "value", -c(plotid, inv, year, depth)) %>%
    mutate(variable.ann = paste(variable, depth, sep = "_")) %>%
    select(-c(variable, depth)) %>%
    spread(key = variable.ann, value = value) %>%
    mutate(plothalfid1 = paste(inv, plotid, sep = "_")) -> minz.select
  
  return(minz.select)
}

load_data <- function(){
  
  soilmoiom <- load_soilmoiOM()
  ph <- load_ph()
  soiln <- load_soiln()
  veg <- load_veg()
  trees <-load_trees()
  
  soilmoiom %>%
    left_join(ph) %>%
    left_join(soiln) %>%
    left_join(veg) %>%
    left_join(trees) %>%
    gather(key = "variable", value = "value", -c(plothalfid1, plotid, inv, year)) %>%
    filter(!variable %in% c("ph_F","soiltemp","PercBA_AM")) -> df.l
  
  # fix the depth extensions
  df.l %>%
    separate(variable, into = c("first","second"), sep = "_", remove = FALSE) %>%
    mutate(second.u = toupper(second)) %>%
    mutate(depth = ifelse(second.u %in% c("T", "B"), second.u, NA)) %>%
    mutate(new.variable = ifelse(!is.na(depth), paste(first, depth, sep ="_"), variable)) %>%
    select(-c(first, second, second.u, variable)) %>%
    rename('variable' = 'new.variable') -> df.l
  
  # categorize the variable types
  df.l$varType<-NA
  df.l[grepl("_B", df.l$variable),'varType']<-'measCat'
  df.l[grepl("_T", df.l$variable),'varType']<-'measCat'
  df.l[df.l$variable %in% c('mv_g.m2','nat_g.m2','litter_g.m2','total_g.m2'),'varType']<-'understoryBiom'
  df.l[df.l$variable %in% c('percpar'),'varType']<-'environParam'
  df.l[df.l$variable %in% c('nTrees','BA_total'),'varType']<-'overstoryParam'
  
  #add a columns that identify whether the variable is measured on the plot half or the plot level
  plotlevel.vars<-c('nTrees','BA_total')
  df.l %>%
    mutate(measLevel = ifelse(variable %in% plotlevel.vars, "plot", "plothalf")) %>%
    mutate(variable1 = ifelse(measLevel == "plot", variable, paste(variable, inv, sep = "_"))) -> df.l
  
  # use 2012 percpar only
  df.l %>%
    filter(variable == "percpar" & year == 2012) %>%
    mutate(variable = "percpar12") -> df.l.percpar12
  df.l.percpar12 %>%
    mutate(year = 2013) -> df.l.percpar13
  new.rows <- rbind(df.l.percpar12, df.l.percpar13)
  df.l <- rbind(df.l, new.rows)
  df.l %>%
    filter(variable != "percpar") -> df.l
   
  #get rid of the plots without trees
  df.l %>%
    filter(variable == "nTrees" & value == 0) #plots 12 and 15
  df.l %>%
    filter(!plotid %in% c(12,15)) -> df.l
  
  return(df.l)
  
}
