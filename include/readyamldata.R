#DTS script for collecting essential data from the dataset file for statistics and plots

dataset.file <- file.choose()
dataset.path = dirname(dataset.file)
dataset.data <- yaml.load_file(dataset.file)

totalflies <- length(paste(dataset.path, unlist(do.call("rbind", lapply(dataset.data$resources, '[', 4))), sep = "/"))#get the number of flies total
NofGroups = unname(lengths(dataset.data["resources"]))                                               #get number of experimental groups
samplesizes = unname(lengths(sapply(dataset.data[["resources"]], function(x) x["data"])))            #get samplesizes
groupnames <- unlist(sapply(dataset.data[["resources"]], function(x) x["name"]))                     #get a vector with all group names
names(samplesizes) = groupnames                                                                      #name the samplesizes to assign them to groups
sortedSsizes = sort(samplesizes, decreasing = TRUE)                                                  #samplesizes sorted in ascending alphabetical groupname order
groupdescriptions <- unlist(sapply(dataset.data[["resources"]], function(x) x["description"]))       #get a vector with all group descriptions
signif = dataset.data[["statistics"]][["significance-levels"]]                                       #get significance levels
priorval = dataset.data[["statistics"]][["priors"]]                                                  #get priors for FPR calculation
twogroupstats <- identical(1,as.numeric(dataset.data[["statistics"]][["two.groups"]][["data"]]))     #determine if statistics for two groups are required
threegroupstats <- identical(1,as.numeric(dataset.data[["statistics"]][["three.groups"]][["data"]])) #determine if statistics for three groups are required
wil <- identical(1,as.numeric(dataset.data[["statistics"]][["single.groups"]][["data"]]))            #determine if we need to do single tests
learningscore = dataset.data[["statistics"]][["learning-score"]][["data"]]                           #get the PI that is going to be tested
colorrange = dataset.data[["statistics"]][["color-range"]]                                           #get the color range for plots
if(!is.null(unlist(sapply(dataset.data[["resources"]], function(x) x["id"])))){
  groupids <- hyperlinks.FBids(unlist(sapply(dataset.data[["resources"]], function(x) x["id"])))     #get a vector with all group FlyBase IDs and hyperlinks
} else groupids=NULL