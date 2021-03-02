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
  PIstatCombined = na.omit(PIstatCombined)                                          #delete NA rows
}