#extraScript_lookAtOtherModels.R

#reshape data.choice
data.T<-subset(data.choice, depth == 'T')
data.T1<-data.choice[data.choice$variable %in% c(allVars,'mv_g.m2'), 
                     c('plotid','plothalfid1','inv','year','variable','value')]
data.T1$variable<-factor(data.T1$variable, levels=c(allVars,'mv_g.m2'))
data.T1.wide<-dcast(data.T1, plotid + year ~ variable + inv, value.var='value')
df<-data.T1.wide

#nitrate
#reference
mod<-lmer(noi_T_N ~ nTrees_N + nitrifd_T_N + ph_T_N + soilmoi_T_N +
            (1|year), data=df)
summary(mod)
mod<-lmer(nitrifd_T_N ~ nTrees_N +
            (1|year), data=df)
summary(mod)
mod<-lmer(ph_T_N ~ nTrees_N +
            (1|year), data=df)
summary(mod)
mod<-lmer(soilmoi_T_N ~ nTrees_N + litter_g.m2_N +
            (1|year), data=df)
summary(mod)
mod<-lmer(litter_g.m2_N ~ nTrees_N +
            (1|year), data=df)
summary(mod)
#invaded
mod<-lmer(noi_T_I ~ nTrees_N + nitrifd_T_I + ph_T_I + soilmoi_T_I + mv_g.m2_I +
            (1|year), data=df)
summary(mod)
mod<-lmer(nitrifd_T_I ~ nTrees_N + mv_g.m2_I +
            (1|year), data=df)
summary(mod)
mod<-lmer(ph_T_I ~ nTrees_N + mv_g.m2_I +
            (1|year), data=df)
summary(mod)
mod<-lmer(soilmoi_T_I ~ nTrees_N + litter_g.m2_I + mv_g.m2_I +
            (1|year), data=df)
summary(mod)
mod<-lmer(litter_g.m2_I ~ nTrees_N + mv_g.m2_I +
            (1|year), data=df)
summary(mod)
## mv biomass
df$mv_g.m2_I_logt<-log(df$mv_g.m2_I+0.01)
mod<-lmer(mv_g.m2_I_logt ~ nTrees_N + noi_T_N + nitrifd_T_N + ph_T_N + soilmoi_T_N + litter_g.m2_N +
            (1|year), data=df)
summary(mod)

#ammonification
#reference
mod<-lmer(ammonifd_T_N ~ nTrees_N + nhi_T_N + som_T_N + ph_T_N + soilmoi_T_N +
            (1|year), data=df)
summary(mod)
mod<-lmer(nhi_T_N ~ nTrees_N +
            (1|year), data=df)
summary(mod)
mod<-lmer(som_T_N ~ nTrees_N +
            (1|year), data=df)
summary(mod)
mod<-lmer(ph_T_N ~ nTrees_N +
            (1|year), data=df)
summary(mod)
mod<-lmer(soilmoi_T_N ~ nTrees_N + litter_g.m2_N +
            (1|year), data=df)
summary(mod)
mod<-lmer(litter_g.m2_N ~ nTrees_N +
            (1|year), data=df)
summary(mod)
#invaded
mod<-lmer(ammonifd_T_I ~ nTrees_N + nhi_T_I + som_T_I + ph_T_I + soilmoi_T_I + mv_g.m2_I +
            (1|year), data=df)
summary(mod)
mod<-lmer(nhi_T_N ~ nTrees_N +mv_g.m2_I +
            (1|year), data=df)
summary(mod)
mod<-lmer(som_T_N ~ nTrees_N +mv_g.m2_I +
            (1|year), data=df)
summary(mod)
mod<-lmer(ph_T_N ~ nTrees_N +mv_g.m2_I +
            (1|year), data=df)
summary(mod)
mod<-lmer(soilmoi_T_N ~ nTrees_N + litter_g.m2_N + mv_g.m2_I +
            (1|year), data=df)
summary(mod)
mod<-lmer(litter_g.m2_N ~ nTrees_N + mv_g.m2_I +
            (1|year), data=df)
summary(mod)
#Mv biomass
mod<-lmer(mv_g.m2_I_logt ~ nTrees_N + ammonifd_T_N + nhi_T_N + som_T_N + 
            soilmoi_T_N + litter_g.m2_N +
            (1|year), data=df)
summary(mod)



#nitrate
modT<-lmer(noi_T ~ mv_g.m2_logt +  nTrees + litter_g.m2 +
             mv_g.m2_logt:nTrees + mv_g.m2_logt:litter_g.m2 + 
             (1|year), data=data.q2)
summary(modT)
meanVal<-mean(data.q2$litter_g.m2)
data.q2$litterBin<-'Low litter'
data.q2[data.q2$litter_g.m2>meanVal,'litterBin']<-'High litter'
data.q2$litterBin<-factor(data.q2$litterBin, levels=c('Low litter','High litter'))
ggplot(data.q2, aes(x=mv_g.m2_logt, y=noi_T)) + 
  geom_point() + facet_grid(~litterBin)

modB<-lmer(noi_B ~ mv_g.m2_logt +  nTrees + litter_g.m2 + 
             mv_g.m2_logt:nTrees + mv_g.m2_logt:litter_g.m2 + 
             (1|year), data=data.q2)
summary(modB)
meanVal<-mean(data.q2$litter_g.m2)
data.q2$litterBin<-'Low litter'
data.q2[data.q2$litter_g.m2>meanVal,'litterBin']<-'High litter'
data.q2$litterBin<-factor(data.q2$litterBin, levels=c('Low litter','High litter'))
ggplot(data.q2, aes(x=mv_g.m2_logt, y=noi_B)) + 
  geom_point() + facet_grid(~litterBin) 


#ammonification
modT<-lmer(ammonifd_T ~ mv_g.m2_logt + soilmoi_T + 
             mv_g.m2_logt:soilmoi_T +
             (1|year), data=data.q2)
summary(modT)
meanVal<-mean(data.q2$soilmoi_T)
data.q2$soilmoiBin<-'Low moisture'
data.q2[data.q2$soilmoi_T>meanVal,'soilmoiBin']<-'High moisture'
data.q2$soilmoiBin<-factor(data.q2$soilmoiBin, levels=c('Low moisture','High moisture'))
ggplot(data.q2, aes(x=mv_g.m2_logt, y=ammonifd_T, size=soilmoi_T)) + 
  geom_point() + facet_grid(~soilmoiBin) + geom_smooth(method='lm')

modT<-lmer(ammonifd_T ~ mv_g.m2_logt + soilmoi_T + PercBA_AM + 
             mv_g.m2_logt:soilmoi_T + PercBA_AM:soilmoi_T + mv_g.m2_logt:PercBA_AM +
             (1|year), data=data.q2)
summary(modT)