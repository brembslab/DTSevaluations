### DTS script to Initialize empty lists where data are collected
grouped.PIprofiles <- list()   #PIProfile data frames in a list of length NofGroups
grouped.Categories <- list()   #For saving categories within groups
grouped.PIcombined <- list()   #For categorical color coding of PIs
grouped.periods <- list()      #Period designs in a list of length NofGroups
grouped.spectra <- list()      #Power spectra in a list of length NofGroups
grouphistdata <- list()        #list for histogram data
exp_groups <- list()           #Individual fly names in each group for display in dataset evaluation
grouped.OMdata <-list()        #Averaged optomotor data traces for each group
grouped.OMparams <-list()      #Extracted optomotor parameters for each group
grouped.OMdataBefore <-list()  #Averaged optomotor data traces for each group at start of experiment
grouped.OMparamsBefore <-list()#Extracted optomotor parameters for each group at start of experiment
grouped.OMdataAfter <-list()   #Averaged optomotor data traces for each group at end of experiment
grouped.OMparamsAfter <-list() #Extracted optomotor parameters for each group at end of experiment
grouped.OMdata_swapped <-list()        #For swapping left and right OM traces
grouped.OMdataBefore_swapped <-list()  #For swapping left and right OM traces
grouped.OMdataAfter_swapped <-list()   #For swapping left and right OM traces

flynames = matrix(ncol=NofGroups, nrow=max(samplesizes)) #create a place to collect all flynames
dwelldata = dwellplots = grouped.dwell = list()          #create dataframes for dwelling data
flies = 0                                                #initialize progress bar