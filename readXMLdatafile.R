################## Some functions to import XML DTS data

require("XML")

####################### Functions for importing data ##########################

## Importing DTS data from an XML file
flyDataImport <- function(xml_name) {
  
  ## Import the data from the .xml file.
  flyData <- xmlParse(xml_name)
  flyDataXMLtop = xmlRoot(flyData)
  
    #parse the metadata
    URIs <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/URIs"))
    
    experimenter <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/experimenter"))
    fly <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/fly"))
    experiment <- xmlToDataFrame(nodes=getNodeSet(flyData,"//metadata/experiment"))
    #parse sequence data
    NofPeriods = as.integer(xmlGetAttr(flyDataXMLtop[['sequence']], "periods"))
    sequence <- xmlToDataFrame(nodes=getNodeSet(flyData,"//sequence/period"))    
    #parse time series meta-data
    CSV_descriptor <- xmlToDataFrame(nodes=getNodeSet(flyData,"//timeseries/CSV_descriptor"))
    variables <- xmlToDataFrame(nodes=getNodeSet(flyData,"//timeseries/variables/variable"))
    #parse the time series raw data
    rawdata <- read.table(text=xmlSApply(flyDataXMLtop[['timeseries']][['csv_data']], xmlValue), col.names=variables$type)
    
    options(digits.secs = 3)
    rawdata$date<-as.POSIXct(rawdata$time/1000, origin="1970-01-01", tz="UTC")
    
    ##list all data
    singleflydata <- list(URIs, experimenter, fly, experiment, NofPeriods, sequence, CSV_descriptor, variables, rawdata)
  
  return(singleflydata)
}

## make sure all flies in a group have the identical experimental design
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
