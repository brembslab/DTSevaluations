library(dplyr)
library(XML)
library(yaml)
library(stringr)
#choose.dir()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(max.print = 300000)                                            #increse the max print output
file_list <- list.files(path=getwd(), pattern = "\\.txt$")             #listing the file with the raw data
file_list2 <- list.files(path=getwd(), pattern =  "TXT")               #listing the file with punished side
file_listDiff <- str_sub(file_list2, end = -5)                         #creating list for compairison
remove<-setdiff(file_list,file_listDiff)                               #find the files that are not in list 2 = incomplet files
file_list<-file_list[!file_list%in% remove]                            #remove incomplet files
comment<- matrix(ncol=2, nrow=length(file_list))                       #creating matrix for comments

yml_data<-read_yaml(file.choose())                                     #reading template yml file
file_list_name <- str_sub(file_list, end = -5) 
file_list_name <- paste(file_list_name, ".xml", sep="")
yml_data$resources[[1]]$data<-file_list_name                           #filling in the list in to avoid manual writing
write_yaml(yml_data, ("New file.yml"))                                 #saving the te template yml file

if((length(file_list)==length(file_list2))==TRUE){                     #check if the list have the same length and stop if not
  for (i in 1:length(file_list)){
    temp_direct <- read.csv(file_list2[i], header = F,sep = "\t")      #reading the file with punished side
    comment[i,1]<-paste(substr(file_list[i], 1, nchar(file_list[i])-4))#filling file name
    comment[i,2]<-temp_direct$V17                                      #filling comment
    names(temp_direct)=NULL
    if (grepl("negative_right",temp_direct[4])==TRUE){                 #split the data based on punished side
      temp_file = xmlParse("Blueprint_left.xml")
    } else { 
      temp_file = xmlParse("Blueprint_right.xml")
    }
    nodeSet = xpathApply(temp_file,"//metadata/fly/name")
    xmlValue(nodeSet[[1]]) = paste(substr(file_list[i], 1, nchar(file_list[i])-4))        #replacing the file name
    nodeSet = xpathApply(temp_file,"//experiment/dateTime")
    xmlValue(nodeSet[[1]]) = temp_direct[6:7]                                             #replacing the time with the current one
    nodeSet = xpathSApply(temp_file,"//fly/description")                                  
    xmlValue(nodeSet[[1]]) = temp_direct[1]                                               #replacing the description
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
    print("Lists have not the same lenght. Check list2, run: setdiff(file_listDiff, file_list)")
  }

comment<-na.omit(as.data.frame(comment))
names(comment)=c("name","comment")
write.csv(comment,"Comment.txt", row.names = FALSE)
cat("The following files are removed: \n",remove)
