### DTS script to extract the PI learningscore values for the period specified in the dataset yaml file (and some other important PIs)
if(PIs_present & !is.null(learningscore)){
  PIstat <- list()
  CatStat <- list()
  for(x in 1:NofGroups){
    PIstat[[x]] <- grouped.PIprofiles[[x]][[learningscore]]
    CatStat[[x]] <- grouped.Categories[[x]][[learningscore]]
  }
  PIstat <- as.data.frame(t(plyr::ldply(PIstat, rbind)))                            #convert PI list to data.frame
  colnames(PIstat) <- unlist(sapply(dataset.data[["resources"]], '[', 'name'))      #add group names as column names to PIstat
  CatStat <-  as.data.frame(t(plyr::ldply(CatStat, rbind)))                         #convert list of categories to data.frame
  colnames(CatStat) <- unlist(sapply(dataset.data[["resources"]], '[', 'name'))     #add group names as column names to CatStat
  
  #compute standard deviations
  SDs<-as.numeric(apply(PIstat, 2, function(x) sd(na.omit(x))))
  
  #combine PIstat and CatStat for plotting learningscores
  PIstatCombined <- reshape2::melt(CatStat, measure.vars = names(CatStat), variable.name = "group", value.name = "category") #melt categories into dataframe with group as id-variable
  PIstatCombined["PIs"] = reshape2::melt(PIstat)$value                                        #combine the categories with the PIs
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
  colnames(pretestPIs) <- unlist(sapply(dataset.data[["resources"]], '[', 'name'))  #add group names as column names to PItemp
  pretestPIs <- reshape2::melt(pretestPIs, measure.vars = names(pretestPIs), variable.name = "group", value.name = "category", na.rm = TRUE) #melt categories into dataframe with group as id-variable
}

#Compute average post-training values and create dataframe
if(PIs_present){
  postPIs <- list()
  for(x in 1:NofGroups){
    postPIs[[x]] <- grouped.PIprofiles[[x]][postperiods]     #extract the PIs into a temporary list
    postPIs[[x]] <- rowMeans(postPIs[[x]])
  }
  postPIs <- as.data.frame(t(plyr::ldply(postPIs, rbind)))                    #convert PI list to a temproary data.frame
  colnames(postPIs) <- unlist(sapply(dataset.data[["resources"]], '[', 'name'))  #add group names as column names to PItemp
  postPIs <- reshape2::melt(postPIs, measure.vars = names(postPIs), variable.name = "group", value.name = "category", na.rm = TRUE) #melt categories into dataframe with group as id-variable
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