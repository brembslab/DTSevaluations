#####################################################################################################################################
################## R-script to read YAML DTS dataset files, visualize and statistically evaluate data. Reports in HTML ##############
#####################################################################################################################################

setwd(dirname(parent.frame(2)$ofile)) #set working directory to source file location (allows loading of function files)

## source libraries and functions needed for analysis
source("include/includes.R")
source("include/importfunctions.R")
source("include/plotfunctions.R")
start.wd=getwd()
## read single YAML dataset file and collect essential data from the dataset file for statistics and plots
source("include/readyamldata.R")

#start busy animation and initialize progress bar
busy <- image_read(paste(start.wd,"/include/dataintegrity.gif", sep=""))
print(busy)
start_time <- Sys.time() #Records the system time at the start of the analysis
flycount = 1 #for progressbar

## check that all prerequisites are met and what kind of experiments we are dealing with
source("include/prerequisites.R")

## Initialize empty lists where data are collected and some other variables
source("include/initialize.R")

for(x in 1:NofGroups) #start main loop that colects data in each experimental group
{
  #gather necessary data and variables
  grp_title = dataset.data[["resources"]][[x]][["title"]] #collect title of the group
  grp_description = groupdescriptions[x] #collect description of the group
  xml_list = paste(dataset.path, dataset.data[["resources"]][[x]][["data"]], sep = "/") #create list of file names

  #create/empty lists for collecting all single fly data by period
  period.data <- list()     #data grouped by period
  grouped.data <- list()    #total data grouped
  speclist <- list()        #spectograms
  
  #start actually evaluating
  print(paste("Evaluating experiments in group: ",grp_title,sep = ""), quote=FALSE)
  pb <- winProgressBar(title = "progress bar", min = 0, max = length(xml_list), width = 300)

  for (l in 1:length(xml_list)) #start the loop that evaluates each individual fly, one at a time
    {
      #load current fly name
      xml_name=xml_list[[l]]
      
      # read the data with the corresponding function #######
      singleflydata <- flyDataImport(xml_name)
  
      #extract single fly data
      source("include/extractsingleflydata.R")
  
      #progress bar
      source("include/progressbar.R")    
      
      #### call RMarkdown for single fly evaluations ###############################################
      rmarkdown::render(paste(start.wd,"/rmarkdown/single_fly.Rmd", sep=""),                  ######
                        output_file = paste(flyname,"qc.html", sep="_"),                      ######
                        output_dir = dataset.path)                                            ######
      #### end RMarkdown for single fly evaluations ################################################
      
      ##once created, move PIs and categories to multi-experiment data.frames
      if(l>1){
        PIprofile <- rbind2(PIprofile, as.vector(t(sequence$lambda)))      #PIs in one dataframe
        Categories <- rbind2(Categories, as.vector(t(sequence$category)))  #and the categories in another dataframe
      }
  
      ##add period data to grouped data
      grouped.data[[l]] <- period.data
      xml_list[[l]] = paste('<a href="',flyname,'_qc.html">', flyname,'</a>', sep = '')  #create link to each single fly quality control HTML document to be used in dataset evaluation
      
      #open window with progress bar
      setWinProgressBar(pb, l, title=paste(round(l/length(xml_list)*100, 0), "% of",grp_title,"done"))
      
    } #for number of flies in xml_list - from here on group evaluations
  
  #close progress bar window
  close(pb)
  
  exp_groups[[x]] <- c(grp_title, grp_description, xml_list) #add name and description and file links to dataframe to be used in dataset evaluation document
  
  ## derive means and SDs for optomotor data in the group and collect extracted OM parameters
  source("include/optomotor.R")

  ## pool raw data traces in various ways for later (histogram) plotting
  source("include/pool.R")

  ## if we have two groups, collect data for superimposed histograms for either fly behavior or position
  source("include/superimposedhistograms.R")

  ## Wrap up: Collect data from each group in their respective lists and empty the variables
  source("include/wrapgroupup.R")
  
} #for nofGroups

## extract the PI learningscore values for the period specified in the dataset yaml file and create dataframes of PIs for plotting
source("include/extractlearningscores.R")

## generate important variables for later plotting and annotation
source("include/plotvariables.R")

## if there are three groups
source("include/three_groups.R")

## if there are more than two groups, attempt to pool some PI data into two groups
source("include/poolgroups.R")

#### ----- call RMarkdown for dataset evaluations ----- ################################################
rmarkdown::render(paste(start.wd,"/rmarkdown/dataset.Rmd", sep=""),                                #####
                  output_file = paste(dataset.data$experiment$name,"html", sep = "."),             #####
                  output_dir = dataset.path)                                                       #####
#### ----- end RMarkdown for dataset evaluations ----- #################################################

Progressbar = mtext(paste("Runtime was",(round(((Sys.time() - start_time)), 3)), " minutes in total"), side = 1, line = 1)

setwd(start.wd)