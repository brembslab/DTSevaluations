################## Some functions to import XML DTS data

require("XML")

####################### Functions for importing data ##########################

##### Importing DTS data from an XML file
flyDataImport <- function() {
  
  ## Import the data from the .xml file.
  flyData <- xmlParse(file.choose())
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
    
    ##list all data
    singleflydata <- list(URIs, experimenter, fly, experiment, NofPeriods, sequence, CSV_descriptor, variables, rawdata)
  
  # reset the "Time" variable to make everything more readable
  #flyTracesFiltered$Time <- (flyTracesFiltered$Time - flyTracesFiltered$Time[1])
  
  return(singleflydata)
}




