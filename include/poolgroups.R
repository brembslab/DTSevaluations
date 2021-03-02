### DTS script to attempt to pool some PI data into two groupsif there are more than two groups
PoolGrps=FALSE

if(NofGroups>2 & length(unique(groupdescriptions))==2){
  PoolGrps=TRUE #we have several groups, but only one control and one experimental group
  
  #find out which group belongs to which pool
  pool1=unname(groupnames[which(sapply(project.data[["resources"]], function(x) x["description"])==unique(groupdescriptions)[1])])
  pool2=unname(groupnames[which(sapply(project.data[["resources"]], function(x) x["description"])==unique(groupdescriptions)[2])])
  
  #create two new dataframe (one melted one not) with the pooled groups
  #melted df
  PIstatPooled=PIstatCombined #create copy of many group dataframe
  PIstatPooled$group=gsub(x = PIstatPooled$group, pattern = paste(pool1, collapse = "|"), replacement = unique(groupdescriptions)[1]) #rename the ones from the first pool
  PIstatPooled$group=gsub(x = PIstatPooled$group, pattern = paste(pool2, collapse = "|"), replacement = unique(groupdescriptions)[2]) #rename the ones from the second pool
  #unmelted df
  PIprofilePooled=dcast(PIstatPooled[c("group", "PIs")], PIs~...)[,2:3] #create new dataframe for PIs in the pooled groups this doesn't work with unequal samplesizes: as.data.frame(lapply(dcast(PIstatPooled[c("group", "PIs")], PIs~...), na.omit))
  samplesizesPooled=colSums(!is.na(PIprofilePooled))  #find the new samplesizes for the different groups
  
  #create table with pooled groups for later plotting
  sq <- seq(max(length(pool1), length(pool2)))
  PooledGroups <- data.frame(pool1[sq], pool2[sq])
  colnames(PooledGroups)=unique(groupdescriptions)
}