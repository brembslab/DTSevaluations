### DTS script to collect data from each group in their respective lists and empty the variables after all group evaluations are done
#### -- Period sequence design meta-data -- ####
#### -- Dwelling times -- ####
#### -- Performance Indices -- ####
#### -- Power spectra -- ####


#### -- Period sequence design meta-data -- ####
grouped.periods[[x]] = periods


#### -- Dwelling times -- ####
if (Dwell){
  dwellmeans$unpunished = dwellmeans$unpunished[nonOMperiods] #remove columns without data
  dwellmeans$punished = dwellmeans$punished[nonOMperiods]     #remove columns without data
  colnames(dwellmeans$punished) <- colnames(dwellmeans$unpunished) <- sprintf("PI%d", nonOMperiods)  #make colnames in dwellmeans
  grouped.dwell[[x]] = dwellmeans #Merge single fly dwell data to grouped
  if(!exists("dwellrange")){dwellrange=NA} #create vector to collect largest mean dwelling times per period for y-axis range
  dwellrange[x] = max(colMeans(dwellmeans$unpunished)) #store the largest value
}


#### -- Performance Indices -- ####
PIs_present <- !all(is.na(sequence$lambda)) ###determine if there are any PIs to be plotted

#PIprofiles for statistical analysis (PIs alone, periods as column names)
colnames(PIprofile) <- sprintf("PI%d", 1:NofPeriods)    #make colnames in PIprofile
grouped.PIprofiles[[x]] = PIprofile                     #add PIprofile to list of grouped PIs
PIprofile <- PIprofile[colSums(!is.na(PIprofile)) > 0]  #remove empty columns for combining with categories

#Categories for printing categorical colors (periods as column names)
colnames(Categories) <- sprintf("PI%d", 1:NofPeriods)     #make colnames in Categories
grouped.Categories[[x]] = Categories                      #add Categories to list of grouped Categories
Categories <- Categories[colSums(!is.na(Categories)) > 0] #remove empty columns

#PCombine categories with PIs for plotting (melted, periods as id-variable)
if (PIs_present)
{
  PIcombined <- melt(Categories, measure.vars = names(Categories), variable.name = "period", value.name = "category") #melt data frame to create a variable with periods as id values
  PIcombined["PIs"] = melt(PIprofile)$value                 #combine the categories with the PIs
  grouped.PIcombined[[x]] = PIcombined                      #add PIcombined to list of grouped PIs and categories (for plotting)
}

#Remove some items for reuse in the next group
rm(PIprofile, PIcombined, Categories)


#### -- Power spectra -- ####
spectemp <- do.call(cbind, speclist) #combine all power spectra
colnames(spectemp)[1]<-"freq" #label the first x-axis as frequency
spectemp$freq <- spectemp$freq*1000 #convert kHz to Hz
spectemp <- spectemp[, -grep("x", colnames(spectemp))] #drop all x-axes exept the one now labelled "freq"
spectemp[length(spectemp)+1] <- rowMeans(spectemp[, grep("y", colnames(spectemp))]) #calculate the mean power spectrum in the group
spectemp[length(spectemp)+1] <- rowSds(spectemp[, grep("y", colnames(spectemp))])/sqrt(length(project.data[["resources"]][[x]][["data"]])) #calculate the standard deviation in the group
spectemp[, grep("y", colnames(spectemp))] <- NULL #drop all raw data for summary data
spectemp$group <- as.factor(rep(paste(project.data[["resources"]][[x]][["name"]], ", N=", length(project.data[["resources"]][[x]][["data"]]), sep = ""), nrow(spectemp))) #add grouping variable for later plotting
colnames(spectemp)[2] <- "mean"
colnames(spectemp)[3] <- "sd"
grouped.spectra[[x]] = spectemp #save group mean/sd