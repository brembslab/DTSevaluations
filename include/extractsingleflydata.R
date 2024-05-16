###DTS script to extract single fly data from a DTS XML file and claculate some derivative variables needed later
#extract fly meta-data
fly <- singleflydata$fly
flyname = fly$name[1]
if(exists("flynames")){flynames[l,x] = paste(flyname)} #copy the name into the list of all flynames

#extract sequence meta-data
NofPeriods = singleflydata$NofPeriods
sequence <- singleflydata$sequence
samplerate = as.numeric(as.character(singleflydata$experiment$sample_rate))
real_sample_rate = as.numeric(as.character(singleflydata$real_sample_rate))
down_sample_rate = as.numeric(as.character(singleflydata$down_sample_rate))
nonOMperiods=which(!grepl("optomotor", sequence$type)==TRUE) #vector containing period numbers for non-optomotor periods
testperiods=which((sequence$type=="color" | sequence$type=="fs" | sequence$type=="yt" | sequence$type=="sw") & sequence$outcome=="0") #vector containing all testperiods
pretestperiods=as.vector(unlist(split(testperiods, cumsum(c(1, diff(testperiods) != 1)))[1]))
tempL=length(split(testperiods, cumsum(c(1, diff(testperiods) != 1))))
postperiods=as.vector(unlist(split(testperiods, cumsum(c(1, diff(testperiods) != 1)))[tempL]))

#extract experiment meta-data
experimenter <- singleflydata$experimenter
experiment <- singleflydata$experiment

#extract rawdata
rawdata <- singleflydata$rawdata
flyrange = singleflydata$flyrange
traces <- singleflydata$traces
#calculate max fly values for axes when plotting
maxfly = c(-round_any(max(abs(flyrange)), 100, f=ceiling)*0.9,round_any(max(abs(flyrange)), 100, f=ceiling)*0.9)
if(l==1){maxflymax=maxfly} #create maximum flyrange
if(maxfly[2]>maxflymax[2]){maxflymax=maxfly} #if current fly has bigger range than maximum range, expand maximum range

#create/empty the dataframe for dwellmeans
Dwell = any(c("yt","color","sw","fs") %in% sequence$type) ###determine if dwelling times should be calculated
if (Dwell & l==1){
  dwellmeans = list()
  dwellmeans$unpunished <- dwellmeans$punished <- data.frame(matrix(ncol = NofPeriods))
}