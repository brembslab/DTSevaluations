#DTS script for collecting essential data from the project file for statistics and plots

project.file <- file.choose()
project.path = dirname(project.file)
project.data <- yaml.load_file(project.file)

totalflies <- length(paste(project.path, unlist(do.call("rbind", lapply(project.data$resources, '[', 4))), sep = "/"))#get the number of flies total
NofGroups = unname(lengths(project.data["resources"]))                                               #get number of experimental groups
samplesizes = unname(lengths(sapply(project.data[["resources"]], function(x) x["data"])))            #get samplesizes
groupnames <- unlist(sapply(project.data[["resources"]], function(x) x["name"]))                     #get a vector with all group names
names(samplesizes) = groupnames                                                                      #name the samplesizes to assign them to groups
sortedSsizes = sort(samplesizes, decreasing = TRUE)                                                  #samplesizes sorted in ascending alphabetical groupname order
groupdescriptions <- unlist(sapply(project.data[["resources"]], function(x) x["description"]))       #get a vector with all group descriptions
signif = project.data[["statistics"]][["significance-levels"]]                                       #get significance levels
priorval = project.data[["statistics"]][["priors"]]                                                  #get priors for FPR calculation
twogroupstats <- identical(1,as.numeric(project.data[["statistics"]][["two.groups"]][["data"]]))     #determine if statistics for two groups are required
threegroupstats <- identical(1,as.numeric(project.data[["statistics"]][["three.groups"]][["data"]])) #determine if statistics for three groups are required
wil <- identical(1,as.numeric(project.data[["statistics"]][["single.groups"]][["data"]]))            #determine if we need to do single tests
learningscore = project.data[["statistics"]][["learning-score"]][["data"]]                           #get the PI that is going to be tested
colorrange = project.data[["statistics"]][["color-range"]]                                           #get the color range for plots
if(!is.null(unlist(sapply(project.data[["resources"]], function(x) x["id"])))){
  groupids <- hyperlinks.FBids(unlist(sapply(project.data[["resources"]], function(x) x["id"])))     #get a vector with all group FlyBase IDs and hyperlinks
} else groupids=NULL