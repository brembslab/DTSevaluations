###DTS script to check that all prerequisites are met and what kind of experiments have been conducted

#make sure all flies have the identical experimental design and find out which don't
xml_list = paste(project.path, unlist(do.call("rbind", lapply(project.data$resources, '[', 4))), sep = "/") #create list of all file names
offending_metanames <- MultiFlyDataVerification(xml_list)
if(!is_null(offending_metanames)) stop("You have selected files with non-equal metadata. Please check the file(s) above for consistency!", cat("Error! File(s) with differing metadata: ", offending_metanames, sep = "\n"))

#check for duplicates in the raw data and report any occurrences
offending_behavnames <- MultiFlyDuplicateCheck(xml_list)
if(!is.null(offending_behavnames)) stop("There are duplicates in the raw data!", cat("Error! List of duplicate file(s): ", offending_behavnames, sep = "\n"))

#ensure all groups have unique names
if(anyDuplicated(groupnames)>0) stop("There is a duplicate group name!", cat("Error! Duplicate name(s): ", groupnames[anyDuplicated(groupnames)], sep = "\n"))

#what kind of experiment are we dealing with? Default is torquemeter
if (exists('type', where=project.data$experiment)){ExpType = project.data$experiment$type} else ExpType = "Torquemeter"
if (tolower(ExpType)=="torquemeter"){FlyBehavior="Torque"} else {FlyBehavior="Platform Position"}