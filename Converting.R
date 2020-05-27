library(dplyr)
library(XML)
library(yaml)
choose.dir()
#setwd("~/Convert")
options(max.print = 300000) #increse the max print output

file_list <- list.files(path=getwd(), pattern = "\\.txt$")
file_list2 <- list.files(path=getwd(), pattern =  "TXT")
yml_data<-read_yaml(file.choose()) #reading template yml file
yml_data$resources[[1]]$data<-file_list #filling in the list in to avoid manual writing
write_yaml(yml_data, ("New file.yml")) # saving the te template yml file

if((length(file_list)==length(file_list2))==TRUE){  
  for (i in 1:length(file_list)){
    temp_direct <- read.csv(file_list2[i], header = F)
    if (grepl("negative_right",temp_direct)==TRUE){
      temp_file = xmlParse("Blueprint_left.xml")
    } else { 
      temp_file = xmlParse("Blueprint_right.xml")
    }
    nodeSet = xpathApply(temp_file,"//metadata/fly/name")
    xmlValue(nodeSet[[1]]) = paste(substr(file_list[i], 1, nchar(file_list[i])-4))
    nodeSet = xpathApply(temp_file,"//experiment/dateTime")
    xmlValue(nodeSet[[1]]) = Sys.time()
    temp_data <- read.csv(file_list[i], skip = 21, header=T,sep = "\t", row.names = NULL) 
    temp_data <-temp_data[-5] %>% filter(temp_data[3] <= 9 & temp_data[3] >= 1)
    names(temp_data)=NULL
    temp_data<-toString.XMLNode(temp_data, row.names=NULL)
    nodeSet = xpathApply(temp_file,"//csv_data")
    xmlValue(nodeSet[[1]]) = temp_data
    saveXML(temp_file, file.path(paste("Output"), file=paste(substr(file_list[i], 1, nchar(file_list[i])-4) ,".xml", sep = "")))
    print(paste0(i, "of", length(file_list)))
  }}else{
    print("Lists have not the same lenght")
  }



           