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
    sequence <- xmlToDataFrame(nodes=getNodeSet(flyData,"//sequence/period"))    
  ##parse time series meta-data
    CSV_descriptor <- xmlToDataFrame(nodes=getNodeSet(flyData,"//timeseries/CSV_descriptor"))
    variables <- xmlToDataFrame(nodes=getNodeSet(flyData,"//timeseries/variables/variable"))
  ##parse the time series raw data
    rawdata <- read.table(text=xmlSApply(flyDataXMLtop[['timeseries']][['csv_data']], xmlValue), col.names=variables$type)
  ##reset periods to start from 1 of they start from 0
    if (rawdata$period[1]==0){rawdata$period=rawdata$period+1}
  ##reset position data to +/-180° [-1800..1796] for torquemeter experiments
    if (project.data$experiment$type=="Torquemeter"){
    if (experiment$arena_type=="lightguides"){rawdata$a_pos = rawdata$a_pos-1800}
    if (experiment$arena_type=="motor"){rawdata$a_pos = round(rawdata$a_pos*0.87890625)}
    }
    
  ##change j_pos data from float to integer and shift to make approx. zero symmetric (needs work!)
    if(exists("j_pos", rawdata)){
      rawdata$j_pos = round(rawdata$j_pos*1000)+1100
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
    if (round(real_sample_rate) > 20) {
      rawdata <- weightedDownsample20Hz(rawdata, sequence, experiment, NofPeriods)
      down_sample_rate = nrow(rawdata)/(rawdata$time[nrow(rawdata)]/1000)
    } else {
      real_sample_rate = experiment$sample_rate
      down_sample_rate = experiment$sample_rate
    }
  
    options(digits.secs = 3)
    rawdata$date<-as.POSIXct(rawdata$time/1000, origin=date(experiment$dateTime), tz="UTC") #required for some function...???
    
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
                          down_sample_rate)
  
  return(singleflydata)
}

#### Importing only the meta-data from an XML file####
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
for (l in 1:length(xml_list)) 
  {
    xml_name=xml_list[l]
    ##### read the data with the corresponding function (readXMLdatafile.R) #######
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
      metadata<-data.frame(flymetadata)}else{
      metadata[,l]<-data.frame(flymetadata)}
  }
  return(length(unique(as.list(metadata))) == 1)
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

##downsample the rawdata using a fixed bin width (deprecated)
downsamplebin <- function(rawdata, binsize) {
  
  # create the vectors in which to save the downsampled data
  timeDownsampled <- vector(mode = "numeric", length = ceiling(length(rawdata$time)/binsize))
  a_posDownsampled <- vector(mode = "numeric", length = ceiling(length(rawdata$a_pos)/binsize))
  flyDownsampled <- vector(mode = "numeric", length = ceiling(length(rawdata$fly)/binsize))
  periodDownsampled <- vector(mode = "numeric", length = ceiling(length(rawdata$period)/binsize))

  # downsampling time
  for (index in seq(1,length(rawdata$time),binsize)) {
    if(index < (length(rawdata$time)-binsize)) { # check whether we reached the end of the data; if not:
      timeDownsampled[((index-1)/binsize)+1] <- round(sum(rawdata$time[index:(index+binsize-1)])/binsize)  # average all data in the bin and save it in the right slot of the downsampled vector
    } else {  # in case we reached the end
      timeDownsampled[((index-1)/binsize)+1] <- round(sum(rawdata$time[index:length(rawdata$time)])/length(rawdata$time[index:length(rawdata$time)])) # average over the remaining values and save the result
      timeDownsampled <- timeDownsampled-timeDownsampled[1] #set time to start from 0
    }
  }
  # downsampling position (problem with values at +/-180°!!)
  for (index in seq(1,length(rawdata$a_pos),binsize)) {
    if(index < (length(rawdata$a_pos)-binsize)) { # check whether we reached the end of the data; if not:
      a_posDownsampled[((index-1)/binsize)+1] <- round(sum(rawdata$a_pos[index:(index+binsize-1)])/binsize)  # average all data in the bin and save it in the right slot of the downsampled vector
    } else {  # in case we reached the end
      a_posDownsampled[((index-1)/binsize)+1] <- round(sum(rawdata$a_pos[index:length(rawdata$a_pos)])/length(rawdata$a_pos[index:length(rawdata$a_pos)])) # average over the remaining values and save the result
    }
  }
  # downsampling fly behavior
  for (index in seq(1,length(rawdata$fly),binsize)) {
    if(index < (length(rawdata$fly)-binsize)) { # check whether we reached the end of the data; if not:
      flyDownsampled[((index-1)/binsize)+1] <- round(sum(rawdata$fly[index:(index+binsize-1)])/binsize)  # average all data in the bin and save it in the right slot of the downsampled vector
    } else {  # in case we reached the end
      flyDownsampled[((index-1)/binsize)+1] <- round(sum(rawdata$fly[index:length(rawdata$fly)])/length(rawdata$fly[index:length(rawdata$fly)])) # average over the remaining values and save the result
    }
  }
  # downsampling period
  for (index in seq(1,length(rawdata$period),binsize)) {
    if(index < (length(rawdata$period)-binsize)) { # check whether we reached the end of the data; if not:
        if (abs(max(rawdata$period[index:(index+binsize-1)]) - min(rawdata$period[index:(index+binsize-1)])) == 0){ #check if there is a period switch in the bin, if there is:
          periodDownsampled[((index-1)/binsize)+1] <- rawdata$period[index]
        } else if (table(rawdata$period[index:(index+binsize-1)])[1]>table(rawdata$period[index:(index+binsize-1)])[2]) #if the majority of values indicates old period
          {
            periodDownsampled[((index-1)/binsize)+1] <- rawdata$period[index] #make the period vale that of the old period
          } else {
            periodDownsampled[((index-1)/binsize)+1] <- rawdata$period[index+binsize-1] #if the majority of values indicates new period, set the value to the new period
          }
    } else { # in case we reached the end
      periodDownsampled[((index-1)/binsize)+1] <- rawdata$period[length(rawdata$period)]
    }
  }
  
  # bind the downsampled vectors into one dataframe
  rawdataDown <- data.frame("time" = timeDownsampled, "a_pos" = a_posDownsampled, "fly" = flyDownsampled, "period" = periodDownsampled)
  
  # return the downsampled data
  return(rawdataDown)
}

### Downsampling to 20Hz by weighting according to the measured time within the 50ms bin

weightedDownsample20Hz <- function(rawdata, sequence, experiment, NofPeriods) {
  
  rawdata$group_num <- 50*round(rawdata$time/50) # Create 50ms bins
  rawdata$weight <- 1/(1+abs(rawdata$time-rawdata$group_num)) # calculate distance from measurement point
  rawdata$norm <-ave(rawdata$weight,rawdata$group_num,FUN=function(x) x/sum(x)) #apply weights according to distance from bin center
  rawdata$fly2 <- rawdata$fly*rawdata$norm
  rawdata$a_pos2 <- rawdata$a_pos*rawdata$norm #needs more work because of values at +/-180°!!!
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
       #mark the last data pont of each offending period (assuming we're only one data point off!)
       if (length(diff_periods)!=0){
         rawdataDown$last = NA
         rawdataDown$last = with(rawdataDown, ave(last, match(rawdataDown$period, diff_periods), FUN = function(x) ifelse(seq_along(x) == length(x), 1, x))) # "1" marking the last data püoint in an offending period
         #mark the last data points of periods with missing data points
         if (length(rownames(difference)[difference$deviation==-1])!=0){ #if there are periods with too few data points, duplicate the last
            negative_periods=rownames(difference)[difference$deviation==-1] #find the periods with missing values
            rawdataDown$last = with(rawdataDown, ave(last, match(rawdataDown$period, negative_periods), FUN = function(x) ifelse(seq_along(x) == length(x), 2, x))) # "2" marking the last data püoint in an offending period
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