
# uses calc_varloads() in fxns_helpers.R
plot_pca_soiln <- function(data){
  
  #extract data subset
  soiln.levels <- soiln_levels()
  data %>%
    filter(variable %in% soiln.levels$nVars) %>%
    mutate(inv_plotid_year = paste(plothalfid1, year, sep = "_")) %>%
    select(inv_plotid_year, inv, plotid, year, plothalfid1, variable, value) %>%
    spread(key = variable, value = value) -> curr.data
  curr.data %>%
    select(-c(inv_plotid_year, inv, plotid, year, plothalfid1)) -> df.ord
  
  #PCA
  df.ord.scaled <- scale(df.ord)
  mod.obj <- prcomp(df.ord.scaled)
  scrs <- data.frame(inv_plotid_year = curr.data$inv_plotid_year, scores(mod.obj)[,1:2])
  scrs %>%
    left_join(curr.data) -> scrs
  scrs %>%
    mutate(plotid_year = paste(plotid, year, sep = "_")) -> scrs
  
  #plot
  shape.vec<-c(16,17)
  color.vec<-c("blue","black")
  scrs$inv <- recode(scrs$inv, `I` = "Invaded", `N` = "Reference")
  pc.varexpl <- round(100 * mod.obj$sdev[1:2]^2/sum(mod.obj$sdev^2), digits = 2)
  xlab <- paste0("PC 1 ", "(", pc.varexpl[1],"%)")
  ylab <- paste0("PC 2 ", "(", pc.varexpl[2],"%)")
  p <- ggplot(scrs, mapping = aes(x = PC1, y = PC2, color = inv)) +
    geom_line(aes(group = plotid_year), color = "darkgray") +
    geom_point(aes(shape = factor(year)), size = 3, alpha = .8) +
    xlab(xlab) + ylab(ylab) +
    stat_ellipse(aes(color = inv), type = "t") + #multivariate t-distribution
    coord_fixed() + ## need aspect ratio of 1!
    theme_bw() +
    scale_shape_manual(name = "Year", values = shape.vec) +
    scale_color_manual(name = "Plot", values = color.vec)
  
  #variable contributions to PC1 and PC2
  varloads <- calc_varloads(mod.obj =  mod.obj)
  
  # test the role of invasion status with permMANOVA
  perm <- adonis(df.ord.scaled ~ curr.data$inv, data = NULL, method='eu')
  aov.tab.perm <- data.frame(perm$aov.tab)
  
  pca.soiln <- list(curr.data = curr.data, 
                    p = p, 
                    pcvar = varloads, 
                    perm = aov.tab.perm)
  return(pca.soiln)
  
}

inveffect_models <- function(data){
  
  #extract data subset
  soiln.levels <- soiln_levels()
  data %>%
    filter(variable %in% soiln.levels$nVars) %>%
    mutate(inv_plotid_year = paste(plothalfid1, year, sep = "_")) %>%
    mutate(plotid_year = paste(plotid, year, sep = "_")) %>%
    select(inv_plotid_year, inv, plotid, year, plothalfid1, plotid_year, variable, value) %>%
    spread(key = variable, value = value) -> curr.data
  
  #run models
  Yvars <- soiln.levels$nVars
  modelFormulas <- paste(Yvars," ~ inv + (1|plotid_year)")
  mod.list <- lapply(modelFormulas, function(x){
    mod <- lmer(x, data = curr.data)
    return(mod)
  })
  names(mod.list) <- Yvars
  anova.list <- lapply(mod.list, anova) # this produces pvalues because the package LmerTest is loaded
  anova.df <- list_to_df(anova.list)
  
  #calculate means
  data %>%
    filter(variable %in% soiln.levels$nVars) %>%
    group_by(variable,inv,year) %>%
    summarize(n = length(value),
              mean = round(mean(value), digits = 2),
              se = round(sd(value)/sqrt(n), digits = 2)) -> means.df
  
  inveffect.list <- list(anova.df = anova.df, 
                         means.df = means.df)
  
  return(inveffect.list)
}

plot_inveffect <- function(means.df){
  
  #i) prep dataframe and params
  soiln.levels <- soiln_levels()
  means.df %>%
    rename('nVars'='variable') %>%
    left_join(soiln.levels$nVars.indx) %>%
    mutate(nVarNames2 = paste(basic.nVarNames, units)) %>%
    select(nVars, nVarNames2, inv, year, mean, se) %>%
    separate(nVars, into = c("var","depth"), sep = "_") -> means.df
  means.df$nVarNames2 <- factor(means.df$nVarNames2, levels = unique(means.df$nVarNames2)[c(3, 5, 1, 4, 2)])
  means.df$depth <- factor(means.df$depth, levels = c('T','B'))
  means.df$depth <- recode(means.df$depth, `T`='0-5cm', `B`='5-15cm')
  means.df$inv <- recode(means.df$inv, `I`="Invaded", `N`="Reference")
  means.df %>%
    mutate(inv_year = paste(inv, year, sep = " ")) -> means.df
  
  #ii) loop through each soil response variable
  ylabs <- levels(means.df$nVarNames2)
  figure.list <- list()
  for(i in 1:length(soiln.levels$basic.nVars)){
    means.df %>%
      filter(var == soiln.levels$basic.nVars[i]) %>%
      mutate(depth_year = paste0(depth, year))-> plot.df
    
    p <- ggplot(plot.df, aes(x = inv, y = mean, 
                             shape = factor(year),
                             color = depth,
                             group = depth_year)) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
      geom_line(aes(linetype = factor(year))) + 
      xlab(NULL) + ylab(ylabs[i]) + 
      scale_color_manual(name = "Depth", values = c("black","darkgray")) +
      scale_shape_discrete(name = "Year") +
      scale_linetype_manual(name = "Year", values = c(1, 2)) +
      mytheme + guides(color = FALSE, shape = FALSE, linetype = FALSE)
    figure.list[[i]] <- p
  }
  soiln.levels$basic.nVars
  names(figure.list)<-soiln.levels$basic.nVars
  
  #iii) extract the legend
  pleg <- ggplot(plot.df, aes(x = inv, y = mean, 
                           shape = factor(year),
                           linetype = factor(year),
                           color = depth,
                           group = depth_year)) + 
    geom_point(size = 2) + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) + 
    geom_line() + 
    xlab(NULL) + ylab(ylabs[i]) + 
    scale_color_manual(name = "Depth", values = c("black","darkgray")) +
    scale_shape_discrete(name = "Year") +
    scale_linetype_manual(name = "Year", values = c(1, 2)) +
    mytheme +
    theme(legend.key.height=unit(1, 'lines'), legend.title=element_text(size=8))
  leg1Grob<-g_legend(pleg)

  #iv) put panels together and save
  fig.width <- fig.height <- 2.5 #inches
  pdf(paste('output','pScat_q1.pdf', sep='/'), width = fig.width*2, height = fig.height*1.2)
  grid.arrange(
    figure.list[['nhi']] + ylim(c(0,4)),
    figure.list[['noi']] + ylim(c(0,4)),
    leg1Grob,
    figure.list[['ammonifd']] + ylim(c(-0.2,0.55)),
    figure.list[['nitrifd']] + ylim(c(-0.2,0.55)),
    figure.list[['minzd']] + ylim(c(-0.2,0.55)),
    nrow=2, ncol=3
  )
  dev.off()
}
