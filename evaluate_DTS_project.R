library################## An R-script to read YAML DTS project files, visualize and statistically evaluate data

library(ggplot2)
library(tidyr)
library(dygraphs)
library(grid)
library(reshape2)
library(dplyr)
library(gridExtra)
library(yaml)
library(ggsignif)
library(effsize)
library(pwr)
library(BayesFactor)
library(genefilter)
library(seewave)

## source the script with the functions needed for analysis
source("readXMLdatafile.R")
source("DTS_plotfunctions.R")

start.wd = getwd()

## read single YAML project file
project.file <- file.choose()
project.path = dirname(project.file)
project.data<-yaml.load_file(project.file)

#make sure the data are written in a subfolder of the data folder
evaluation.path = paste(project.path,"evaluations", sep = "/")
dir.create(evaluation.path, showWarnings = FALSE)
setwd(evaluation.path)

#how many experimental groups will need to be evaluated
NofGroups=lengths(project.data["resources"])

grouped.trqhistos <- list()   #Torque histograms for group in a list of length NofPeriods
grouped.poshistos <- list()   #Position histograms for group in a list of length NofPeriods
grouped.PIprofiles <- list()  #PIProfile data frames in a list of length NofGroups
grouped.periods <- list()     #Period designs in a list of length NofGroups
grouped.spectra <- list()      #Power spectra in a list of length NofGroups

for(x in 1:NofGroups)
{
xml_list = paste(project.path, project.data[["resources"]][[x]][["data"]], sep = "/")

#create/empty lists for collecting all single fly data by period
period.data <- list()     #data grouped by period
grouped.data <- list()    #total data grouped
speclist <- list()

#start evaluating
if(MultiFlyDataVerification(xml_list)==TRUE) # make sure all flies in a group have the identical experimental design
{
  for (l in 1:length(xml_list)) 
  {
    xml_name=xml_list[l]
    
    ##### read the data with the corresponding function #######
    singleflydata <- flyDataImport(xml_name)
    
    ##extract sequence meta-data
    NofPeriods = singleflydata[[5]]
    sequence <- singleflydata[[6]]
    samplerate = as.numeric(as.character(singleflydata[[4]]$sample_rate))
    
    ##extract fly meta-data
    fly <- singleflydata[[3]]
    flyname = fly$name[1]
    
    ##extract the rawdata
    rawdata <- singleflydata[[9]]
    
    #create/empty plot lists
    poshistos <- list()
    trqhistos <- list()
    
    #start writing to PDF
    filename = paste(flyname,"descr_anal.pdf", sep="_")
    pdf(file=filename, paper="a4r", pointsize=14, width = 0, height = 0)
    
    plot.new() #write text header to summary data
    #title
    mtext("Descriptive Data Evaluation Sheet", line = 2, font=2, cex=2)
    mtext(paste("for single fly experiment", flyname), line = 1, font=2, cex=1.3)
    #summary metadata text
    mdata <- collect.metadata(singleflydata)
    for(i in 1:length(mdata)){mtext(mdata[i], line = -i)}
    #plot table of period sequence
    periods=t(sequence)
    colnames(periods)<-sprintf("Period %s",1:NofPeriods)
    
    grid.table(periods)
    ##### analyze data from each period separately #######
    
    for(i in 1:NofPeriods){
      
      #save colors for later plotting
      if(sequence$outcome[i]==0){sequence$color[i]="lightyellow"} else {sequence$color[i]="orange"}
      if(sequence$outcome[i]==0){sequence$histocolor[i]="darkgreen"} else {sequence$histocolor[i]="orange"}
      
      #only look at period data
      temp  <- rawdata[rawdata$period == i, ]
      keeps = c("a_pos","torque")
      period.data[[i]] <- temp[keeps] #list only position and torque data by period
      
      if(sequence$type[i]=="fs"||sequence$type[i]=="color"||sequence$type[i]=="optomotor")
      {
        ## plot the torque and position time traces
        trq_pos_traces(temp)
      }
      
      #plot period histograms
      #torque
      trqhistos[[i]] <- ggplot(data=temp, aes_string(temp$torque)) +
        geom_histogram(binwidth = 3, fill = sequence$histocolor[i]) +
        labs(x="torque [arb units]", y="frequency") +
        xlim(-600,600) +
        ggtitle(paste(flyname, "Period", i))
      
      #position
      if(sequence$type[i]=="fs"||sequence$type[i]=="color"||sequence$type[i]=="optomotor")
      {
        poshistos[[i]] <- ggplot(data=temp, aes_string(temp$a_pos)) +
          geom_histogram(binwidth=10, fill = sequence$histocolor[i]) +
          labs(x="position [arb units]", y="frequency") +
          xlim(-2047,2048) +
          ggtitle(paste(flyname, "Period", i))
      }
      
      ##calculate PIs
      ##create data.frame for adding PIs if it doesn't eist, yet
      if(!exists("PIprofile")){PIprofile <- data.frame(matrix(ncol = NofPeriods))}
      
      #for position
      if(sequence$type[i]=="fs"||sequence$type[i]=="color")
      {
        t1 = sum(abs(temp$a_pos) >= 512 & abs(temp$a_pos) <= 1538)
        t2 = nrow(temp)-t1
        sequence$lambda[i] = (t1-t2)/(t1+t2)
        if (sequence$contingency[i] == '2_4_Q'){sequence$lambda[i]=-sequence$lambda[i]}
        if(l==1){PIprofile[1,i]=sequence$lambda[i]}
      }
      #for torque
      #if(sequence$type[i]=="yt"||"sw_blue"||"sw_green")
      #{}
    } #for Number of Periods
    
    ########## analyze data for entire experiment ##############
    
    
    ## plot position and torque histograms ##
    #torque
    trqhistos[[NofPeriods+1]] <- ggplot(data=rawdata, aes_string(rawdata$torque)) + 
      geom_histogram(binwidth=3) + 
      labs(x="torque [arb units]", y="frequency") + 
      xlim(-600,600) +
      ggtitle(paste(flyname, "total"))
    
    #position
    poshistos[[NofPeriods+1]] <- ggplot(data=rawdata, aes_string(rawdata$a_pos)) + 
      geom_histogram(binwidth=10) +
      labs(x="position [arb units]", y="frequency") + 
      xlim(-2047,2048) +
      ggtitle(paste(flyname, "total"))
    
    ##write histograms to file
    #torque
    multiplot(plotlist = trqhistos, cols=2)
    #position
    multiplot(plotlist = poshistos, cols=2)

    ##plot PI bargraph
    
    barplot(sequence$lambda, main = paste("Performance Indices", flyname),
            xlab="Time [periods]",
            ylab="PI [rel. units]",
            space=0,
            col = sequence$color)
    
    ## plot power spectrum and spectrogram ##
    meanspec(rawdata$torque, f = samplerate, wl = 600) #plot mean power spectrum for each fly
    title(paste("Torque-Powerspectrum", flyname))
    spectro(rawdata$torque, f = samplerate, wl=600) #plot spectrogram for each fly
    title(paste("Torque-Spectrogram", flyname))
    speclist[[l]] = as.data.frame(meanspec(rawdata$torque, f = samplerate, wl = 600, plot = FALSE)) #collect each spectrum for later avweraging
    
    dev.off()
    
    
    ##dyplot traces
    traces <- dytraces(rawdata)
    print(traces)
    
    ##move PIs to multi-experiment data.frame
    if(l>1){PIprofile <- rbind2(PIprofile, as.vector(t(sequence$lambda)))}
    
    ##add period data to grouped data
    grouped.data[[l]] <- period.data
    

  } #for number of flies

  ########### plot graphs for all experiments #####################
  
  ##pool all data except "optomotor" by period
  
  pooled.data<-list()
  
  for(i in 1:NofPeriods)
  {
    period.data<-data.frame()
    if(sequence$type[i]!="optomotor")
    {  
      for (l in 1:length(xml_list)) 
      {
        period.data <- rbind(period.data, grouped.data[[l]][[i]])
      }
    }
    pooled.data[[i]] <- period.data
  } #for number of periods
  
  ## plot pooled position and torque histograms by period ##
  
  for(i in 1:NofPeriods)
  {
    temp<-pooled.data[[i]]
    
    #torque
    trqhistos[[i]] <- ggplot(data=temp, aes_string(temp$torque)) +
      geom_histogram(binwidth = 3, fill = sequence$histocolor[i]) +
      labs(x="torque [arb units]", y="frequency") +
      xlim(-600,600) +
      ggtitle(paste("Period", i))
    
    #position
    if(sequence$type[i]=="fs"||sequence$type[i]=="color"||sequence$type[i]=="optomotor")
    {
      poshistos[[i]] <- ggplot(data=temp, aes_string(temp$a_pos)) +
        geom_histogram(binwidth=10, fill = sequence$histocolor[i]) +
        labs(x="position [arb units]", y="frequency") +
        xlim(-2047,2048) +
        ggtitle(paste("Period", i))
    }
  }
  
  ## pool all torque and position data into single data.frame
  
  all.data <- do.call(rbind, pooled.data)
  
  
  ## plot pooled histograms for all flies over all periods
  
  #torque
  trqhistos[[NofPeriods+1]] <- ggplot(data=all.data, aes_string(all.data$torque)) + 
    geom_histogram(binwidth=3) + 
    labs(x="torque [arb units]", y="frequency") + 
    xlim(-600,600) +
    ggtitle("Pooled Torque Histogram")
  
  #position
  poshistos[[NofPeriods+1]] <- ggplot(data=all.data, aes_string(all.data$a_pos)) + 
    geom_histogram(binwidth=10) +
    labs(x="position [arb units]", y="frequency") + 
    xlim(-2047,2048) +
    ggtitle("Pooled Position Histogram")

} else {print("You have selected files with differing metadata")}

###Collect the data from each group in their respective lists and empty the variables

#Period sequence design meta-data
grouped.periods[[x]] = periods

#Torque Histograms
grouped.trqhistos[[x]] = trqhistos #add torque histograms to list of grouped histograms
trqhistos <- list() #empty torque histograms

#Position Histograms
grouped.poshistos[[x]] = poshistos #add torque histograms to list of grouped position histograms
poshistos <- list() #empty list of position histograms

#PI data
colnames(PIprofile) <- sprintf("PI%d", 1:NofPeriods) #make colnames in PIprofile
grouped.PIprofiles[[x]] = PIprofile #add PIprofile to list of grouped PIs
PIprofile <- PIprofile[0,] #empty PIprofile

#Power spectra
spectemp <- do.call(cbind, speclist) #combine all power spectra
colnames(spectemp)[1]<-"freq" #label the first x-axis as frequency
spectemp$freq <- spectemp$freq*1000
spectemp <- spectemp[, -grep("x", colnames(spectemp))] #drop all x-axes exept the one now labelled "freq"
spectemp[length(spectemp)+1] <- rowMeans(spectemp[, grep("y", colnames(spectemp))]) #calculate the mean power spectrum in the group
spectemp[length(spectemp)+1] <- rowSds(spectemp[, grep("y", colnames(spectemp))]) #calculate the standard deviation in the group
spectemp[, grep("y", colnames(spectemp))] <- NULL #drop all raw data for summary data
spectemp$group <- as.factor(rep(paste(project.data[["resources"]][[x]][["title"]]), nrow(spectemp))) #add grouping variable for later plotting
colnames(spectemp)[2] <- "mean"
colnames(spectemp)[3] <- "sd"
grouped.spectra[[x]] = spectemp #save group mean/sd

} #for nofGroups
####################################################
######## plots and statistical evaluations ########
###################################################

###### Plots ######

pdf(file=paste(project.data$name,"pdf", sep = "."), paper="a4r", pointsize=1, width = 0, height = 0)
#title page with meta-data
plot.new() #write text header to summary data
#write titles
mtext("Project Evaluation Sheet", line = 1, font=2, cex=4)
mtext(project.data$title, line = -1, font=2, cex=1.8)
mtext(paste("Description:", project.data$description), line = -4, font=2, cex=1.3)
mtext(paste("Experimenter:", project.data[["author"]][[2]]), line = -5, font=2, cex=1.3)
mtext("Experimental design:", line = -8, font=2, cex=1.3)
#Write table of period design
periods <- periods[-6,]
periods[3,periods["outcome",]==0]="test"
periods[3,periods["outcome",]==1]="training"
grid.table(periods)

#plot torque histograms
for(x in 1:NofGroups)
  {
    trqhistos <- grouped.trqhistos[[x]]
    multiplot(plotlist = trqhistos, cols=2) #torquehistos
    mtext(project.data[["resources"]][[x]][["title"]], line = 3, font=2, cex=2)
}  

#plot position histograms
for(x in 1:NofGroups)
  {
    poshistos <- grouped.poshistos[[x]]
    multiplot(plotlist = poshistos, cols=2) #positionhistos
    mtext(project.data[["resources"]][[x]][["title"]], line = 3, font=2, cex=2)
}

#plot power spectra in single plot
spectemp <- do.call("rbind", grouped.spectra) #create single data.frame from list of groups
print(ggplot(spectemp, aes(x=freq, y=mean, group=group, colour=group, fill=group)) + 
        geom_line() + 
        geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd), alpha=0.2) +
        scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
        ggtitle("Powerspectra") +
        theme_light(base_size = 16) + theme(panel.grid.major.y = element_blank(),panel.grid.minor = element_blank(), panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
        theme(axis.text.y = element_text(size=16))+ ylab("mean rel. Power") + xlab("Frequency [Hz]"))

#plot PI bar plot with SEM
PIplots <- list()
for(x in 1:NofGroups)
  {
    PIprofile <- grouped.PIprofiles[[x]]
      # compute summary statistics
      error <- data.frame(period=integer(ncol(PIprofile)), mean=numeric(ncol(PIprofile)), sem=numeric(ncol(PIprofile)))
      for(e in 1:ncol(PIprofile))
      {
        error$period[e]=e
        error$mean[e]=mean(PIprofile[,e])
        error$sem[e]=sd(PIprofile[,e]/sqrt(nrow(PIprofile)))
      }
      # plot graph
     PIplots[[x]] <- ggplot(error, aes(x=period, y=mean)) + 
              geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
              geom_bar(fill = sequence$color, position=position_dodge(), stat="identity", colour="black") +
              geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),
                            width=0,
                            size=1.5,
                            position=position_dodge(.9)) +
              ggtitle(paste(project.data[["resources"]][[x]][["title"]], ", N=",nrow(PIprofile))) +
              scale_x_continuous(breaks = seq(1, NofPeriods, 1)) +
              scale_y_continuous(breaks = seq(-1, 1, .2)) +
              theme_light(base_size = 16) + theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(), panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
              theme(axis.text.y = element_text(size=16))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/NofPeriods)
  }
grid.arrange(grobs = PIplots, nrow=NofGroups)
      
# Plot box&dotplot with notches
for(x in 1:NofGroups)
  {
  PIprofile <- grouped.PIprofiles[[x]]
    PIplots[[x]] <- ggplot(melt(PIprofile), aes(variable, value)) +
              geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
              geom_boxplot(fill = sequence$color, notch = TRUE, outlier.color=NA, width=0.8, size=0.6) +
              geom_jitter(data = melt(PIprofile), aes(variable, value), position=position_jitter(0.3), cex=2, color="grey80") +
              ggtitle(paste(project.data[["resources"]][[x]][["title"]], ", N=",nrow(PIprofile))) +
              scale_y_continuous(breaks = seq(-1, 1, .4)) +
              theme_light(base_size = 16) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
              theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]")+ xlab("Experiment Sequence") + theme(aspect.ratio=4/NofPeriods)
  }  
grid.arrange(grobs = PIplots, nrow=NofGroups)

# Plot box&dotplot without notches
for(x in 1:NofGroups)
  {
  PIprofile <- grouped.PIprofiles[[x]]
    PIplots[[x]] <- ggplot(melt(PIprofile), aes(variable, value)) +
              geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
              geom_boxplot(fill = sequence$color, notch = FALSE, outlier.color=NA, width=0.8, size=0.6) +
              geom_jitter(data = melt(PIprofile), aes(variable, value), position=position_jitter(0.3), cex=2, color="grey80") +
              ggtitle(paste(project.data[["resources"]][[x]][["title"]], ", N=",nrow(PIprofile))) +
              scale_y_continuous(breaks = seq(-1, 1, .4)) +
              theme_light(base_size = 16) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
              theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]")+ xlab("Experiment Sequence") + theme(aspect.ratio=4/NofPeriods)
  }      
grid.arrange(grobs = PIplots, nrow=NofGroups)

# plot violin plot
for(x in 1:NofGroups)
  {
  PIprofile <- grouped.PIprofiles[[x]]
    PIplots[[x]] <- ggplot(melt(PIprofile), aes(variable, value)) +
              geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
              geom_violin(width = 1.1) +
              geom_boxplot(fill = sequence$color, width = 0.1, outlier.color="darkred") +
              ggtitle(paste(project.data[["resources"]][[x]][["title"]], ", N=",nrow(PIprofile))) +
              scale_y_continuous(breaks = seq(-1, 1, .4)) +
              theme_light(base_size = 16) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
              theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]")+ xlab("Experiment Sequence") + theme(aspect.ratio=4/NofPeriods)
  }
grid.arrange(grobs = PIplots, nrow=NofGroups)

###### if there are more than two groups, try to pool the data into 'experimental' and 'control'
if(NofGroups>2){}

###### statistical evaluations ######

if(!is.null(project.data[["statistics"]])) #check if there are statistics instructions
{  
signif = project.data[["statistics"]][["significance-levels"]] #get significance levels
learningscore=project.data[["statistics"]][["learning-score"]][["data"]] #get the PI that is going to be tested
groupnames <- unlist(sapply(project.data[["resources"]], function(x) x["name"])) #get a vector with all group names
#create new dataframe with only the chosen PI values
PIstat <- list()
for(x in 1:NofGroups)
{
  PIstat[[x]] <- grouped.PIprofiles[[x]][[learningscore]]
}
PIstat <- as.data.frame(t(plyr::ldply(PIstat, rbind))) #convert PI list to data.frame
colnames(PIstat) <- unlist(sapply(project.data[["resources"]], '[', 'name')) #add group names as column names to PIstat
###generate important variables for ater plotting and anotation
colorrange = project.data[["statistics"]][["color-range"]]
boxcolors = c(colorrange[1:NofGroups])
boxes<-c(1:NofGroups)
samplesizes<-as.numeric(apply(PIstat, 2, function(x) length(na.omit(x))))

##### Single group tests against zero #####
if(project.data[["statistics"]][["single.groups"]][["data"]]==1) #check if instructions contain Wlcoxon test against zero
{
  wilcoxon<-numeric()  
  for(x in 1:NofGroups){wilcoxon[x] = signif(wilcox.test(PIstat[[x]])$p.value, 3)} #test all groups against zero
  #compute Bayes Factor for each group
  results.bayes<-list()
  for(x in 1:NofGroups){results.bayes[[x]]=extractBF(ttestBF(na.omit(PIstat[[x]])))} #extract BayesFactors for all groups
  results.bayes<-do.call("rbind", results.bayes) #fuse all Bayes results into one dataframe
  results.bayes <- results.bayes[-c(3,4)]# drop the date and code columns
  results.bayes <- as.data.frame(sapply(results.bayes , signif, 3)) # reduce results to 3 significant digits
  #add group names as row names
  row.names(results.bayes) <- groupnames

# plot PI box plot test against zero
  plots.singles<-list(ggplot(melt(PIstat), aes(variable, value)) +
    geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
    geom_boxplot(fill = boxcolors, notch = FALSE, outlier.color=NA, width=0.8, size=0.6) +
    geom_jitter(data = melt(PIstat), aes(variable, value), position=position_jitter(0.3), cex=2, color="grey80") +
    ggtitle("Wilcoxon") +
    scale_y_continuous(breaks = seq(-1, 1, .2)) +
    theme_light(base_size = 16) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
    theme(axis.text.y = element_text(size=18))+ ylab(paste("PI", learningscore, " [rel. units]", sep = ""))+ xlab("Groups")+ theme(aspect.ratio=3/NofGroups)+
    samplesizes.annotate(boxes, samplesizes) +
    wilcox.annotate(boxes, wilcoxon))
  
#add table with results and plot
    plots.singles[[2]]<-tableGrob(results.bayes)
    grid.arrange(grobs = plots.singles, ncol=2)
  
}
##### Tests between two independent samples #####
if(project.data[["statistics"]][["two.groups"]][["data"]]==1 || NofGroups==2) #check if instructions contain U-test between two groups and if we have two grouos
{
    utest = signif(wilcox.test(PIstat[[1]],PIstat[[2]])$p.value, 3) #compare the two groups with a U-test and collect p-value
    w.statistic = signif(wilcox.test(PIstat[[1]],PIstat[[2]])$statistic, 3)
    #compute effect size Cohen's D
    cohend = signif(cohen.d(na.omit(PIstat[,1]), na.omit(PIstat[,2]))$estimate, 3)
    #calculate statistical power
    alt = project.data[["statistics"]][["two.groups"]][["power"]]
    power=signif(pwr.t2n.test(n1 = samplesizes[1], n2= samplesizes[2], d = cohend, alternative = alt, sig.level = signif[1])$power, 3)
    #calculate Bayes Factor
    bayesF=extractBF(ttestBF(na.omit(PIstat[[1]]), na.omit(PIstat[[2]])))
    #make tidy table of results
    results.utest<-data.frame(values=c(signif[1], w.statistic, cohend, power, signif(bayesF$bf, 3), signif(bayesF$error, 3)))
    rownames(results.utest)<-c("Significance level" ,"MW U-Test, W", "Cohen's D", "stat. Power", "Bayes Factor", "Bayes Factor error")
    
# plot two PIs with asterisks
  plots.2test<-list(ggplot(melt(PIstat), aes(variable, value)) +
      geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
      geom_boxplot(fill = boxcolors, notch = TRUE, outlier.color=NA, width=0.8, size=0.6) +
      geom_jitter(data = melt(PIstat), aes(variable, value), position=position_jitter(0.3), cex=2, color="grey80") +
      ggtitle(paste("U-Test, p=", utest)) +
      scale_y_continuous(breaks = seq(-1, 1, .2)) +
      theme_light(base_size = 16) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
      theme(axis.text.y = element_text(size=18))+ ylab(paste("PI", learningscore, " [rel. units]", sep = ""))+ xlab("Groups")+ theme(aspect.ratio=3/NofGroups)+
      geom_signif(comparisons = list(c(colnames(PIstat[1]), colnames(PIstat[2]))), map_signif_level= c("***"= signif[3],"**"= signif[2], "*"= signif[1])) +
      samplesizes.annotate(boxes, samplesizes))

  #add table with results and plot
  plots.2test[[2]]<-tableGrob(results.utest)
  grid.arrange(grobs = plots.2test, ncol=2)
}


} #for if exists statistics

dev.off()
setwd(start.wd)