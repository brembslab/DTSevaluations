### DTS script to extract the PI learningscore values for the period specified in the project yaml file (and some other important PIs)
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

#Compute average pretest values and create dataframe
if(PIs_present){
pretestPIs <- list()
  for(x in 1:NofGroups){
    pretestPIs[[x]] <- grouped.PIprofiles[[x]][pretestperiods]     #extract the PIs into a temporary list
    pretestPIs[[x]] <- rowMeans(pretestPIs[[x]])
  }
  pretestPIs <- as.data.frame(t(plyr::ldply(pretestPIs, rbind)))                    #convert PI list to a temproary data.frame
  colnames(pretestPIs) <- unlist(sapply(project.data[["resources"]], '[', 'name'))  #add group names as column names to PItemp
  pretestPIs <- melt(pretestPIs, measure.vars = names(pretestPIs), variable.name = "group", value.name = "category") #melt categories into dataframe with group as id-variable
}

#Create raincloudplot dataframe for 2x2 raincloud plots
if(PIs_present & twogroupstats & NofGroups==2){
  rcp.PIs <- data_2x2(
  array_1 = pretestPIs$category[pretestPIs$group==groupnames[1]],
  array_2 = pretestPIs$category[pretestPIs$group==groupnames[2]],
  array_3 = PIstat.rcp$PIs[PIstat.rcp$group==groupnames[1]],
  array_4 = PIstat.rcp$PIs[PIstat.rcp$group==groupnames[2]],
  labels = (groupnames),
  jit_distance = .09,
  jit_seed = 321,
  spread_x_ticks = FALSE)
}