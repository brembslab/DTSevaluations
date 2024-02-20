### DTS script to derive means and SDs for optomotor data in the group just evaluated and collect extracted OM parameters for dataset evaluation
library("wrMisc") #only needed for rowSEMs
if(any(grepl("optomotor", sequence$type)==TRUE)){    ###determine if there are optomotor periods
  if (any(!grepl("optomotor", sequence$type)==TRUE)){   ###if there are non-optomotor periods...
    if (grepl("optomotor", sequence$type[1]) & grepl("optomotor", tail(sequence$type, 1))){ ###...and the opto periods are in the beginning and the end
      ##then we have before/after optomotor data
      #before
      OMdataBefore$means=rowMeans(OMdataBefore[-1])
      OMdataBefore$sd=rowSds(OMdataBefore[-1])
      OMdataBefore$sem=rowSEMs(OMdataBefore[-1])
      OMdataBefore$group=dataset.data[["resources"]][[x]][["name"]]
      grouped.OMdataBefore[[x]] <- OMdataBefore #save optomotor data to groupwise list
      rm(OMdataBefore) #remove the optomotor data frame so it can be generated again for the next group
      OMparamsBefore$group=dataset.data[["resources"]][[x]][["name"]]
      OMparamsBefore$desc=dataset.data[["resources"]][[x]][["description"]]
      grouped.OMparamsBefore[[x]] <- OMparamsBefore #save extracted optomotor parameters to groupwise list
      rm(OMparamsBefore) #remove the optomotor parameters dataframe so it can be generated again for the next group
      #after
      OMdataAfter$means=rowMeans(OMdataAfter[-1])
      OMdataAfter$sd=rowSds(OMdataAfter[-1])
      OMdataAfter$sem=rowSEMs(OMdataAfter[-1])
      OMdataAfter$group=dataset.data[["resources"]][[x]][["name"]]
      OMparamsAfter$desc=dataset.data[["resources"]][[x]][["description"]]
      grouped.OMdataAfter[[x]] <- OMdataAfter #save optomotor data to groupwise list
      rm(OMdataAfter) #remove the optomotor data frame so it can be generated again for the next group
      OMparamsAfter$group=dataset.data[["resources"]][[x]][["name"]]
      grouped.OMparamsAfter[[x]] <- OMparamsAfter #save extracted optomotor parameters to groupwise list
      rm(OMparamsAfter) #remove the optomotor parameters dataframe so it can be generated again for the next group
      #before_swapped
      OMdataBefore_swapped$means=rowMeans(OMdataBefore_swapped[-1])
      OMdataBefore_swapped$sd=rowSds(OMdataBefore_swapped[-1])
      OMdataBefore_swapped$sem=rowSEMs(OMdataBefore_swapped[-1])
      OMdataBefore_swapped$group=dataset.data[["resources"]][[x]][["name"]]
      grouped.OMdataBefore_swapped[[x]] <- OMdataBefore_swapped #save optomotor data to groupwise list
      rm(OMdataBefore_swapped) #remove the optomotor data frame so it can be generated again for the next group
      #after_swapped
      OMdataAfter_swapped$means=rowMeans(OMdataAfter_swapped[-1])
      OMdataAfter_swapped$sd=rowSds(OMdataAfter_swapped[-1])
      OMdataAfter_swapped$sem=rowSEMs(OMdataAfter_swapped[-1])
      OMdataAfter_swapped$group=dataset.data[["resources"]][[x]][["name"]]
      grouped.OMdataAfter_swapped[[x]] <- OMdataAfter_swapped #save optomotor data to groupwise list
      rm(OMdataAfter_swapped) #remove the optomotor data frame so it can be generated again for the next group
    }
  } else {
    OMdata$means=rowMeans(OMdata[-1])
    OMdata$sd=rowSds(OMdata[-1])
    OMdata$sem=rowSEMs(OMdata[-1])
    OMdata$group=dataset.data[["resources"]][[x]][["name"]]
    grouped.OMdata[[x]] <- OMdata #save optomotor data to groupwise list
    rm(OMdata) #remove the optomotor data frame so it can be generated again for the next group
    OMparams$group=dataset.data[["resources"]][[x]][["name"]]
    OMparams$desc=dataset.data[["resources"]][[x]][["description"]]
    grouped.OMparams[[x]] <- OMparams #save extracted optomotor parameters to groupwise list
    rm(OMparams) #remove the optomotor parameters dataframe so it can be generated again for the next group
    #swapped for plotting
    OMdata_swapped$means=rowMeans(OMdata_swapped[-1])
    OMdata_swapped$sd=rowSds(OMdata_swapped[-1])
    OMdata_swapped$sem=rowSEMs(OMdata_swapped[-1])
    OMdata_swapped$group=dataset.data[["resources"]][[x]][["name"]]
    grouped.OMdata_swapped[[x]] <- OMdata_swapped #save optomotor data to groupwise list
    rm(OMdata_swapped) #remove the optomotor data frame so it can be generated again for the next group
  }
}
unloadNamespace("wrMisc") #dumping library as not needed anywhere else
