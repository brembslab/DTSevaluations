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
  ##reset position data to +/-180° [-1800..1796]
    if (experiment$arena_type=="lightguides"){rawdata$a_pos = rawdata$a_pos-1800}
    if (experiment$arena_type=="motor"){rawdata$a_pos = round(rawdata$a_pos*0.87890625)}
  ##reset periods to start from 1 of they start from 0
    if (rawdata$period[1]==0){rawdata$period=rawdata$period+1}
  ##calculate actual sampling rate and downsample if necessary
    real_sample_rate = nrow(rawdata)/(rawdata$time[nrow(rawdata)]/1000)
    if (round(real_sample_rate) > 20) {
      binsize = as.integer(real_sample_rate/20)
      rawdata <- downsamplebin(rawdata,binsize)
      down_sample_rate = nrow(rawdata)/(rawdata$time[nrow(rawdata)]/1000)
    } else {
      real_sample_rate = experiment$sample_rate
      down_sample_rate = experiment$sample_rate
    }
    
  ##find range of torque values  
    torquerange = range(rawdata$torque)
    
    
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
                          torquerange,
                          real_sample_rate,
                          down_sample_rate)
  
  return(singleflydata)
}

#### make sure all flies in a group have the identical experimental design ####
MultiFlyDataVerification <- function(xml_list)
{
for (l in 1:length(xml_list)) 
  {
    xml_name=xml_list[l]
    ##### read the data with the corresponding function (readXMLdatafile.R) #######
    singleflydata <- flyDataImport(xml_name)
    
    ##extract meta-data
    experiment <- singleflydata[[4]]
    sequence <- singleflydata[[6]]
    flymetadata <- c(as.vector(experiment$duration),
                    as.vector(experiment$sample_rate),
                    as.vector(experiment$arena_type),
                    as.vector(experiment$meter_type),
                    as.vector(sequence$type),
                    as.vector(sequence$duration),
                    as.vector(sequence$outcome),
                    as.vector(sequence$pattern),
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

##downsample the rawdata using a fixed bin width
downsamplebin <- function(rawdata, binsize) {
  
  # create the vectors in which to save the downsampled data
  timeDownsampled <- vector(mode = "numeric", length = ceiling(length(rawdata$time)/binsize))
  a_posDownsampled <- vector(mode = "numeric", length = ceiling(length(rawdata$a_pos)/binsize))
  torqueDownsampled <- vector(mode = "numeric", length = ceiling(length(rawdata$torque)/binsize))
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
  # downsampling position
  for (index in seq(1,length(rawdata$a_pos),binsize)) {
    if(index < (length(rawdata$a_pos)-binsize)) { # check whether we reached the end of the data; if not:
      a_posDownsampled[((index-1)/binsize)+1] <- round(sum(rawdata$a_pos[index:(index+binsize-1)])/binsize)  # average all data in the bin and save it in the right slot of the downsampled vector
    } else {  # in case we reached the end
      a_posDownsampled[((index-1)/binsize)+1] <- round(sum(rawdata$a_pos[index:length(rawdata$a_pos)])/length(rawdata$a_pos[index:length(rawdata$a_pos)])) # average over the remaining values and save the result
    }
  }
  # downsampling torque
  for (index in seq(1,length(rawdata$torque),binsize)) {
    if(index < (length(rawdata$torque)-binsize)) { # check whether we reached the end of the data; if not:
      torqueDownsampled[((index-1)/binsize)+1] <- round(sum(rawdata$torque[index:(index+binsize-1)])/binsize)  # average all data in the bin and save it in the right slot of the downsampled vector
    } else {  # in case we reached the end
      torqueDownsampled[((index-1)/binsize)+1] <- round(sum(rawdata$torque[index:length(rawdata$torque)])/length(rawdata$torque[index:length(rawdata$torque)])) # average over the remaining values and save the result
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
  rawdataDown <- data.frame("time" = timeDownsampled, "a_pos" = a_posDownsampled, "torque" = torqueDownsampled, "period" = periodDownsampled)
  
  # return the downsampled vector
  return(rawdataDown)
}

