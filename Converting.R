library(dplyr)
library(XML)
library(yaml)
#choose.dir()
setwd("~/Convert")
options(max.print = 300000)                                 #increse the max print output

file_list <- list.files(path=getwd(), pattern = "\\.txt$")  #listing the file with the raw data
file_list2 <- list.files(path=getwd(), pattern =  "TXT")    #listing the file with punished side
yml_data<-read_yaml(file.choose())                          #reading template yml file
yml_data$resources[[1]]$data<-file_list                     #filling in the list in to avoid manual writing
write_yaml(yml_data, ("New file.yml"))                      # saving the te template yml file

if((length(file_list)==length(file_list2))==TRUE){          #check if the list have the smae length and stop if not
  for (i in 1:length(file_list)){
    temp_direct <- read.csv(file_list2[i], header = F,sep = "\t")      #reading the file with punished side
    names(temp_direct)=NULL
    if (grepl("negative_right",temp_direct[4])==TRUE){         #split the data based on punished side
      temp_file = xmlParse("Blueprint_left.xml")
    } else { 
      temp_file = xmlParse("Blueprint_right.xml")
    }
    nodeSet = xpathApply(temp_file,"//metadata/fly/name")
    xmlValue(nodeSet[[1]]) = paste(substr(file_list[i], 1, nchar(file_list[i])-4))        #replacing the file name
    nodeSet = xpathApply(temp_file,"//experiment/dateTime")
    xmlValue(nodeSet[[1]]) = temp_direct[6:7]                                                  #replacing the time with the current one
    nodeSet = xpathSApply(temp_file,"//fly/description")
    xmlValue(nodeSet[[1]]) = temp_direct[1] 
    nodeSet = xpathApply(temp_file,"//fly")
    temp_data <- read.csv(file_list[i], skip = 21, header=T,sep = "\t", row.names = NULL) #reading in the raw data
    temp_data <-temp_data[-5] %>% filter(temp_data[3] <= 9 & temp_data[3] >= 1)           #removing unnecessary rows and colums
    names(temp_data)=NULL                                                                 #removing the colum names
    temp_data<-toString.XMLNode(temp_data, row.names=NULL)                                #converting raw data
    nodeSet = xpathApply(temp_file,"//csv_data")
    xmlValue(nodeSet[[1]]) = temp_data                                                    #replacing raw data
    saveXML(temp_file, file.path(paste("Output"), file=paste(substr(file_list[i], 1, nchar(file_list[i])-4) ,".xml", sep = "")))
    print(paste0(i, " of ", length(file_list)))
  }}else{
    print("Lists have not the same lenght")
  }
