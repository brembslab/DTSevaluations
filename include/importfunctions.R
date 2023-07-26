################## Some functions to import XML DTS data

require("XML")

####################### Functions for importing data ##########################

#### Importing DTS data from an XML file####
flyDataImport <- function(xml_name) {
  
### Import the data from the .xml file.
  flyData <- xmlParse(xml_name)
  flyDataXMLtop = xmlRoot(flyData)
  
  ##parse the metadata
    URIs <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/URIs"))
    experimenter <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/experimenter"))
    fly <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/fly"))
    experiment <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/experiment"))
  ##parse sequence data
    NofPeriods = as.integer(xmlGetAttr(flyDataXMLtop[['sequence']], "periods"))
    ExpType = xmlGetAttr(flyDataXMLtop[['metadata']][['experiment']], "type")
    sequence <- xmlToDataFrame(nodes=getNodeSet(flyData,"//sequence/period"))    
  ##parse time series meta-data
    CSV_descriptor <- xmlToDataFrame(nodes=getNodeSet(flyData,"//timeseries/CSV_descriptor"))
    variables <- xmlToDataFrame(nodes=getNodeSet(flyData,"//timeseries/variables/variable"))
  ##parse the time series raw data
    rawdata <- read.table(text=xmlSApply(flyDataXMLtop[['timeseries']][['csv_data']], xmlValue), col.names=variables$type)
  ##reset periods to start from 1 of they start from 0
    if (rawdata$period[1]==0){rawdata$period=rawdata$period+1}
  ##reset position data to +/-180? [-1800..1796] for torquemeter experiments
    if (tolower(ExpType)=="torquemeter"){
    if (tolower(experiment$arena_type)=="lightguides"){rawdata$a_pos = rawdata$a_pos-1800}
    if (tolower(experiment$arena_type)=="motor"){rawdata$a_pos = round(rawdata$a_pos*0.87890625)}
    }
    
  ##adjusting data for shiming
    if (tolower(experiment$meter_type) == "shiming") {
      rawdata$time = round(rawdata$time*1000)
      rawdata$torque = rawdata$torque*100
    }
     
  ##change j_pos data from float to integer in Joystick experiments
    if(exists("j_pos", rawdata)){
      rawdata$j_pos = round(rawdata$j_pos*1000)
    }
  ##change a_pos data from float to integer in Joystick experiments
    if(experiment$meter_type=="Joystick"){
      rawdata$a_pos = round(rawdata$a_pos*1000)
    }    
  ##replace column name for fly behavior (torque, j_pos) with "fly"
    colnames(rawdata) = gsub("torque", "fly", colnames(rawdata))
    colnames(rawdata) = gsub("j_pos", "fly", colnames(rawdata))
    
  ##find range of fly behavior values  
    flyrange = range(rawdata$fly)
    
    
  ##calculate actual sampling rate and downsample if necessary
    real_sample_rate = nrow(rawdata)/(rawdata$time[nrow(rawdata)]/1000)
    traces <- rawdata
    NofDatapoints = as.numeric(as.character(experiment$duration))*20 #find the number of data points we should be having at 20Hz
    if (nrow(rawdata) != NofDatapoints) {
    rawdata <- downsampleapprox(rawdata, sequence, experiment, NofPeriods, NofDatapoints)
    down_sample_rate = nrow(rawdata)/(rawdata$time[nrow(rawdata)]/1000)
    } else {
      real_sample_rate = experiment$sample_rate
      down_sample_rate = experiment$sample_rate
    }
  
    options(digits.secs = 3)
    rawdata$date<-as.POSIXct(rawdata$time/1000, origin=date(experiment$dateTime), tz="UTC") #required for dygraphs
    traces$date<-as.POSIXct(traces$time/1000, origin=date(experiment$dateTime), tz="UTC") #required for dygraphs
    
    #remove any extra period numbers, if they exist
    if (length(table(rawdata$period)) > NofPeriods) {rawdata<-rawdata[!(rawdata$period==length(table(rawdata$period))),]}
    
    ##list all data
    singleflydata <- list(URIs, 
                          experimenter,
                          fly,
                          experiment,
                          NofPeriods,
                          sequence,
                          CSV_descriptor,
                          variables,
                          rawdata,
                          flyrange,
                          real_sample_rate,
                          down_sample_rate,
                          traces,
                          ExpType)
  
  return(singleflydata)
}



#### Importing only the meta-data from an XML file ####
flyMetaDataImport <- function(xml_name) {
  
  ### Import the data from the .xml file.
  flyData <- xmlParse(xml_name)
  flyDataXMLtop = xmlRoot(flyData)
  
  ##parse the metadata
  URIs <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/URIs"))
  
  experimenter <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/experimenter"))
  fly <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/fly"))
  experiment <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/experiment"))
  ##parse sequence data
  NofPeriods = as.integer(xmlGetAttr(flyDataXMLtop[['sequence']], "periods"))
  sequence <- xmlToDataFrame(nodes=getNodeSet(flyData,"//sequence/period"))    
  ##parse time series meta-data
  CSV_descriptor <- xmlToDataFrame(nodes=getNodeSet(flyData,"//timeseries/CSV_descriptor"))
  variables <- xmlToDataFrame(nodes=getNodeSet(flyData,"//timeseries/variables/variable"))

  ##list all data
  singleflymetadata <- list(URIs, 
                        experimenter,
                        fly,
                        experiment,
                        NofPeriods,
                        sequence,
                        CSV_descriptor,
                        variables)
  
  return(singleflymetadata)
}



#### make sure all flies in a group have the identical experimental design ####
MultiFlyDataVerification <- function(xml_list)
{
  writeLines("Verifying meta-data integrity:")
  pb <- txtProgressBar(min = 0, max = length(xml_list), style = 3, char = "=")
for (l in 1:length(xml_list)) 
  {
    xml_name=xml_list[l]
    setTxtProgressBar(pb, l)
    ## read the data
    singleflymetadata <- flyMetaDataImport(xml_name)
    
    ##extract meta-data
    experiment <- singleflymetadata[[4]]
    sequence <- singleflymetadata[[6]]
    flymetadata <- c(as.vector(as.numeric(as.character(experiment$duration))),
                    as.vector(as.numeric(as.character(experiment$sample_rate))),
                    as.vector(experiment$arena_type),
                    as.vector(experiment$meter_type),
                    as.vector(sequence$type),
                    as.vector(as.numeric(as.character(sequence$duration))),
                    as.vector(sequence$outcome),
                    as.vector(sequence$coup_coeff))
    if(l==1){
      metadata<-data.frame(flymetadata)} else {
      metadata[,l]<-data.frame(flymetadata)}
  }
  metadata <- data.frame(lapply(metadata, as.character), stringsAsFactors=FALSE) #convert factors to characters
  colnames(metadata)=xml_list #add filenames to metadata dataframe
  offending_metanames <- colnames(metadata[metadata %in% unique(as.list(metadata))[-1]]) #store offending filenames for each deviation
  if(!is_empty(offending_metanames)){return(offending_metanames)}else{return(NULL)} #return vector with offending filenames or NULL if empty
}



#### make sure there are no duplicated fly behavior traces in the list ####
MultiFlyDuplicateCheck <- function(xml_list)
{
  writeLines("Searching for data duplications:")
  pb <- txtProgressBar(min = 0, max = length(xml_list), style = 3, char = "=")
  for (l in 1:length(xml_list)) 
  {
    xml_name=xml_list[l]
    setTxtProgressBar(pb, l)
    ## read data and extract traces
    singledata <- flyDataImport(xml_name)
    temp.behav = singledata[[9]]$fly
    if(l==1){
      behavior<-data.frame(temp.behav)} else {
        behavior[,l]<-data.frame(temp.behav)}
  }
  colnames(behavior)=xml_list #add filenames to behavior dataframe
  offending_behavnames = colnames(behavior[behavior %in% behavior[which(duplicated(t(behavior)))]]) #find the pairs of files which are duplicated
  if(!is_empty(offending_behavnames)){return(offending_behavnames)}else{return(NULL)} #return vector with offending filenames or NULL if empty
}



##gather experimental metadata in a single vector for plotting in summary pages
collect.metadata <-function(singleflydata)
  {
  #retrieve meta-data
  experimenter <- singleflydata[[2]]
  experiment <- singleflydata[[4]]
  fly <- singleflydata[[3]]
  #create reporting strings
  exp.name = paste("Experimenter:", experimenter$firstname, experimenter$lastname, sep = " ")
  exp.orcid = paste("ORCID: ",experimenter$orcid)
  exp.date = paste("Date and time of the experiment: ",experiment$dateTime )
  exp.duration = paste("Experiment duration:", experiment$duration, "s", sep = " ")
  exp.description = paste("Experiment description: ", experiment$description)
  exp.setup = paste("Samplingrate: ", experiment$sample_rate, "Hz. Arena type:", experiment$arena_type, ". Torquemeter type: ", experiment$meter_type)
  fly = paste("Fly description: ", fly$description, ". FlybaseID: ", fly$flybase)
  mdata = c(exp.name, exp.orcid, exp.date, exp.duration, exp.description, exp.setup, fly)
  return(mdata)
}



### Downsample the rawdata using approx function (for data with period/time jitter)
downsampleapprox <- function(rawdata, sequence, experiment, NofPeriods, NofDatapoints) {

  # create vectors for fly behavior and arena position
  a_posDownsampled <- vector(mode = "numeric")
  flyDownsampled <- vector(mode = "numeric")
  
  # create new time and period values
  timeDownsampled = seq(0, (as.numeric(as.character(experiment$duration))*1000)-50, 50)
  periodDownsampled <- vector(mode = "numeric", length = NofDatapoints)
  p=1
  t=0
  for (index in 1:NofDatapoints){
    periodDownsampled[index]=p
    if (index == t+20*as.numeric(as.character(sequence$duration[p])))
    {
      t=t+20*as.numeric(as.character(sequence$duration[p]))
      p=p+1
    }
  }

  # downsample fly behavior and a_pos
  for (index in 1:NofPeriods){
    f=round(approx(subset(rawdata$fly, rawdata$period==index), n=table(periodDownsampled)[index])$y)
    flyDownsampled=c(flyDownsampled, f)
    p=round(approx(subset(rawdata$a_pos, rawdata$period==index), n=table(periodDownsampled)[index])$y)
    #create a position trace where the +/-180? point is shifted by 90?
    p_s=rawdata$a_pos+900 #make a copy of position trace and shift the values by 90?
    p_s[p_s>1796] = p_s[p_s>1796]-3600 #wrap the shifted 90? back around to -90?..-180?
    p_s=round(approx(subset(p_s, rawdata$period==index), n=table(periodDownsampled)[index])$y)
    p[p_s %in% -1000:-800] <- p_s[p_s %in% -1000:-800]-900 #replace values with shifted values
    p[p < -1800]=p[p < -1800] + 3600 #wrap the too small values around
    a_posDownsampled=c(a_posDownsampled, p)
  }

  # bind the downsampled vectors into one dataframe
  rawdataDown <- data.frame("time" = timeDownsampled, "a_pos" = a_posDownsampled, "fly" = flyDownsampled, "period" = periodDownsampled)
  
  # return the downsampled data
  return(rawdataDown)
}

### Downsampling to 20Hz by weighting according to the measured time within the 50ms bin (deprecated - could be re-used later)

weightedDownsample20Hz <- function(rawdata, sequence, experiment, NofPeriods) {
  
  rawdata$group_num <- 50*round(rawdata$time/50) # Create 50ms bins
  rawdata$weight <- 1/(1+abs(rawdata$time-rawdata$group_num)) # calculate distance from measurement point
  rawdata$norm <-ave(rawdata$weight,rawdata$group_num,FUN=function(x) x/sum(x)) #apply weights according to distance from bin center
  rawdata$fly2 <- rawdata$fly*rawdata$norm
  rawdata$a_pos2 <- rawdata$a_pos*rawdata$norm #needs more work because of values at +/-180?!!!
  rawdata$period2 <- rawdata$period*rawdata$norm
  
  # create the vectors in which to save the downsampled data
  timeDownsampled <- as.vector(unique(rawdata$group_num))
  a_posDownsampled <- as.vector(round(tapply(rawdata$a_pos2, rawdata$group_num, sum)))
  flyDownsampled <- as.vector(round(tapply(rawdata$fly2, rawdata$group_num, sum)))
  periodDownsampled <- as.vector(round(tapply(rawdata$period2, rawdata$group_num, sum)))
  
  # bind the downsampled vectors into one dataframe
  rawdataDown <- data.frame("time" = timeDownsampled, "a_pos" = a_posDownsampled, "fly" = flyDownsampled, "period" = periodDownsampled)
  
  ### check the dataframe for consistency
  if (length(table(rawdataDown$period)) > NofPeriods) {rawdataDown<-rawdataDown[!(rawdataDown$period==length(table(rawdataDown$period))),]} #remove any extra period numbers, if they exist
     # check if there are periods which deviate from projected duration
       difference = as.data.frame(table(rawdataDown$period)) #generate dataframe with actual numbers of data points
       difference$duration = as.numeric(as.character(sequence$duration))*20 # add column with expected values from sequence$duration @ 20Hz
       difference$deviation = difference$Freq-difference$duration
       
       if(any(abs(as.numeric(difference$deviation))>1)) stop("Number of data points does not match expectations. Check DTS Rawdata!") #check if there is more than one missing/additional data point
       
       diff_periods = rownames(difference)[difference$deviation!=0] #find periods with differing numbers of data points
       #mark the last data point of each offending period (assuming we're only one data point off!)
       if (length(diff_periods)!=0){
         rawdataDown$last = NA
         rawdataDown$last = with(rawdataDown, ave(last, match(rawdataDown$period, diff_periods), FUN = function(x) ifelse(seq_along(x) == length(x), 1, x))) # "1" marking the last data p?oint in an offending period
         #mark the last data points of periods with missing data points
         if (length(rownames(difference)[difference$deviation==-1])!=0){ #if there are periods with too few data points, duplicate the last
            negative_periods=rownames(difference)[difference$deviation==-1] #find the periods with missing values
            rawdataDown$last = with(rawdataDown, ave(last, match(rawdataDown$period, negative_periods), FUN = function(x) ifelse(seq_along(x) == length(x), 2, x))) # "2" marking the last data p?oint in an offending period
            copy = as.vector(rawdataDown[is.element(rawdataDown$last, 2),])
            copy$last=NA
            for (z in 1:length(negative_periods)) {
              temp.pos=as.numeric(rownames(copy[z,])) #find the right position to insert
              next.pos=temp.pos+1                     #for some reason, R also wants to have the next position as a variable
              rawdataDown <- rbind(rawdataDown[1:temp.pos,], copy[z,], rawdataDown[next.pos:nrow(rawdataDown),]) # duplicate the last data point in the offending periods
              } 
         }
         rawdataDown <- rawdataDown[!(grepl(1, rawdataDown$last)),] # delete last data point of the offending periods
         rawdataDown$last <- NULL #delete the unnecessary 'last' column
         rawdataDown$time = seq(0, (as.numeric(as.character(experiment$duration))*1000)-50, by=50) # fix the mangled time column
         row.names(rawdataDown) <- 1:nrow(rawdataDown) #fix rownames, too
       }
       
       
 
  # return the downsampled data
  return(rawdataDown)
}