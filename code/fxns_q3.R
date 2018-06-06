
# uses calc_varloads() in fxns_helpers.R
plot_pca_ref <- function(data){
  
  #extract data subset
  allVars.levels <- allVars_levels()
  data %>%
    filter(variable %in% allVars.levels$allVars) %>%
    filter(inv == "N") %>%
    select(plotid, year, variable, value) %>%
    spread(key = variable, value = value) -> curr.data
  
  #add mv (will not be included in ordination)
  data %>%
    filter(inv == "I" & variable == "mv_g.m2") %>%
    select(year, plotid, value) %>%
    rename('mv' = 'value') %>%
    mutate(mv.logt = log(mv)) %>%
    left_join(curr.data) -> curr.data
  curr.data %>%
    select(-c(plotid, year, mv, mv.logt)) -> df.ord
  
  #PCA
  df.ord.scaled <- scale(df.ord)
  mod.obj <- prcomp(df.ord.scaled)
  scrs <- data.frame(plotid = curr.data$plotid, 
                     year = curr.data$year, 
                     scores(mod.obj)[,1:2],
                     mv = curr.data$mv)
  scrs %>%
    mutate(plotid_year = paste(plotid, year, sep = "_")) -> scrs
  
  #plot
  shape.vec<-c(16,17)
  pc.varexpl <- round(100 * mod.obj$sdev[1:2]^2/sum(mod.obj$sdev^2), digits = 2)
  xlab <- paste0("PC 1 ", "(", pc.varexpl[1],"%)")
  ylab <- paste0("PC 2 ", "(", pc.varexpl[2],"%)")
  p <- ggplot(scrs, mapping = aes(x = PC1, y = PC2)) +
    geom_point(aes(shape = factor(year), color = mv), size = 3 ,alpha = .8) +
    xlab(xlab) + ylab(ylab) +
    coord_fixed() + ## need aspect ratio of 1!
    theme_bw() +
    scale_shape_manual(name = "Year", values = shape.vec) +
    scale_color_continuous(name = "Invader \nbiomass (g/m2)")

  #variable contributions to PC1 and PC2
  varloads <- calc_varloads(mod.obj =  mod.obj)
  
  # test the role of Mv biomass with permMANOVA
  perm <- adonis(df.ord.scaled ~ mv.logt, data = curr.data, method='eu')
  aov.tab.perm <- data.frame(perm$aov.tab)
  
  pca.refmv <- list(curr.data = curr.data, 
                    p = p, 
                    pcvar = varloads, 
                    perm = aov.tab.perm)
  return(pca.refmv)
  
}
