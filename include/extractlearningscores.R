### DTS script to extract the PI learningscore values for the period specified in the project yaml file
if(PIs_present & !is.null(learningscore)){
  PIstat <- list()
  CatStat <- list()
  for(x in 1:NofGroups){
    PIstat[[x]] <- grouped.PIprofiles[[x]][[learningscore]]
    CatStat[[x]] <- grouped.Categories[[x]][[learningscore]]
  }
  PIstat <- as.data.frame(t(plyr::ldply(PIstat, rbind)))                            #convert PI list to data.frame
  colnames(PIstat) <- unlist(sapply(project.data[["resources"]], '[', 'name'))      #add group names as column names to PIstat
  CatStat <-  as.data.frame(t(plyr::ldply(CatStat, rbind)))                         #convert list of categories to data.frame
  colnames(CatStat) <- unlist(sapply(project.data[["resources"]], '[', 'name'))     #add group names as column names to CatStat
  
  #compute standard deviations
  SDs<-as.numeric(apply(PIstat, 2, function(x) sd(na.omit(x))))
  
  #combine PIstat and CatStat for plotting learningscores
  PIstatCombined <- melt(CatStat, measure.vars = names(CatStat), variable.name = "group", value.name = "category") #melt categories into dataframe with group as id-variable
  PIstatCombined["PIs"] = melt(PIstat)$value                                        #combine the categories with the PIs
  PIstat.rcp = PIstatCombined                                                       #save values with NAs for raincloudplots
  PIstatCombined = na.omit(PIstatCombined)                                          #delete NA rows
}

#Compute average pretest values and create dataframe for 2x2 repeated measures raincloudplot
if(PIs_present & twogroupstats){
  pretestPIs <- list()
  for(x in 1:NofGroups){
    pretestPIs[[x]] <- grouped.PIprofiles[[x]][pretestperiods]     #extract the PIs into a temproary list
    pretestPIs[[x]] <- rowMeans(pretestPIs[[x]])
  }
  pretestPIs <- as.data.frame(t(plyr::ldply(pretestPIs, rbind)))                    #convert PI list to a temproary data.frame
  colnames(pretestPIs) <- unlist(sapply(project.data[["resources"]], '[', 'name'))  #add group names as column names to PItemp
  pretestPIs <- melt(pretestPIs, measure.vars = names(pretestPIs), variable.name = "group", value.name = "category") #melt categories into dataframe with group as id-variable
}

#Create the raincloudplot dataframe
rcp.PIs <- data_2x2(
  array_1 = pretestPIs$category[pretestPIs$group==groupnames[1]],
  array_2 = pretestPIs$category[pretestPIs$group==groupnames[2]],
  array_3 = PIstat.rcp$PIs[PIstat.rcp$group==groupnames[1]],
  array_4 = PIstat.rcp$PIs[PIstat.rcp$group==groupnames[2]],
  labels = (c('congruent','incongruent')),
  jit_distance = .09,
  jit_seed = 321,
  spread_x_ticks = FALSE)


raincloud_2x2 <- raincloud_2x2_repmes(
  data = na.omit(rcp.PIs),
  colors = (c('dodgerblue', 'darkorange', 'dodgerblue', 'darkorange')),
  fills = (c('dodgerblue', 'darkorange', 'dodgerblue', 'darkorange')),
  size = 1,
  alpha = .6,
  spread_x_ticks = FALSE) +
  
  scale_x_continuous(breaks=c(1,2), labels=c("Pre", "Post"), limits=c(0, 3)) +
  xlab("Time") + 
  ylab("Score") +
  theme_classic()

raincloud_2x2