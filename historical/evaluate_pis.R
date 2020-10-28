## R-Script to read ASCII performance index data and plot them

rm(list=ls()) #clean memory
gc()          #collect garbage

library(ggplot2)
library(reshape2)
library(gridExtra)
library(ggsignif)
library(effsize)
library(pwr)
library(BayesFactor)

source("DTS_plotfunctions.R")






################################# user settings ###########################
NofGroups = 2                             #set number of groups (max 2 for now)
groupnames = c("WTB master", "WTB yoke")  #set a vector with all group names
signif = c(0.005, 0.001, 0.0001)          #set significance levels
learningscore=14                          #set the period number for the learning score to be evaluated
priorval = c(0.5,0.1)                     #create the two priors for FPR calculations
############################### no need for user edits below this line ##################################






grouped.PIprofiles <-list()

for(x in 1:NofGroups)
{
  grouped.PIprofiles[[x]] <- read.csv(file=choose.files(), header=TRUE, sep=";")
  #drop all columns not performance indices
  grouped.PIprofiles[[x]]$FILE <- NULL
  grouped.PIprofiles[[x]]$DATE <- NULL
  grouped.PIprofiles[[x]]$HEAT <- NULL
}

#create a vector with colors for plotting PIs
colors=colnames(grouped.PIprofiles[[1]]) #extract period names
colors[grep("tr", colors)]="orange"      #set training periods to orange
colors[grep("te", colors)]="lightyellow" #set test periods to lightyellow

###### plot PI sequences #######

#bar plots with SEM

PIplots <- list()
for(x in 1:NofGroups)
{
  PIprofile <- grouped.PIprofiles[[x]]
  PIprofile <- PIprofile[colSums(!is.na(PIprofile)) > 0] #remove empty columns
  NofPeriods = ncol(PIprofile)
  # plot graph
  PIplots[[x]] <- ggplot(melt(PIprofile), aes(x=variable, y=value)) + 
    geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
    stat_summary(geom = "bar", fun.y = mean, position = "dodge", fill=colors, colour="black", width=1) +
    stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width=0, size=2) +
    ggtitle(paste(groupnames[x], ", N =",nrow(PIprofile))) +
    scale_y_continuous(breaks = seq(-1, 1, .2)) +
    theme_light(base_size = 16) + 
    theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(), panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
    theme(axis.text.y = element_text(size=18)) + 
    ylab("PI [rel. units]") +
    xlab("Experiment Sequence")
}
grid.arrange(grobs = PIplots, nrow=NofGroups)

###### statistical evaluations ######

#create new dataframe with only the chosen PI learningscores
PIstat <- list() 

for(x in 1:NofGroups)
{
  PIstat[[x]] <- grouped.PIprofiles[[x]][[learningscore]]
}
PIstat <- as.data.frame(t(plyr::ldply(PIstat, rbind))) #convert PI list to data.frame
colnames(PIstat) = groupnames #add group names as column names to PIstat
###generate important variables for ater plotting and anotation
colorrange = c("khaki", "olivedrab3")
boxcolors = c(colorrange[1:NofGroups])
boxes<-c(1:NofGroups)
samplesizes<-as.numeric(apply(PIstat, 2, function(x) length(na.omit(x))))

##### Single group tests against zero #####
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
  
  # plot PI box plot with post-hoc power and asterisks for Wilcoxon test against zero
  plots.singles<-list(ggplot(melt(PIstat), aes(variable, value)) +
                        geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
                        geom_boxplot(fill = boxcolors, notch = FALSE, outlier.color=NA, width=0.8, size=0.6) +
                        geom_jitter(data = melt(PIstat), aes(variable, value), position=position_jitter(0.3), shape=21, size=3, colour="black", fill="grey50", alpha=0.4) +
                        ggtitle("Wilcoxon") +
                        scale_y_continuous(breaks = seq(-1, 1, .2)) +
                        theme_light(base_size = 16) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
                        theme(axis.text.y = element_text(size=18))+ ylab(paste("PI", learningscore, " [rel. units]", sep = ""))+ xlab("Groups")+ theme(aspect.ratio=3/NofGroups)+
                        samplesizes.annotate(boxes, samplesizes) +
                        wilcox.annotate(boxes, wilcoxon))
  
  #add table with results and plot
  plots.singles[[2]]<-tableGrob(results.bayes)
  grid.arrange(grobs = plots.singles, ncol=2)
  

##### Tests between two independent samples #####
  utest = signif(wilcox.test(PIstat[[1]],PIstat[[2]])$p.value, 3) #compare the two groups with a U-test and collect p-value
  w.statistic = signif(wilcox.test(PIstat[[1]],PIstat[[2]])$statistic, 3)
  #compute effect size Cohen's D
  cohend = signif(cohen.d(na.omit(PIstat[,1]), na.omit(PIstat[,2]))$estimate, 3)
  #calculate statistical power
  alt = "two.sided" #set predicted mutant effect
  power=signif(pwr.t2n.test(n1 = samplesizes[1], n2= samplesizes[2], d = cohend, alternative = alt, sig.level = signif[1])$power, 3)
  #calculate Bayes Factor
  bayesF=extractBF(ttestBF(na.omit(PIstat[[1]]), na.omit(PIstat[[2]])))
  #calculate FPR for priors set in project file#
  #run first prior  
  prior=priorval[1]
  out=calc.FPR(samplesizes,utest,prior,abs(cohend))  #output=c(FPR,x0,y0,x1,y1)
  fpz1=out[1]
  #run second prior  
  prior=priorval[2]
  out=calc.FPR(samplesizes,utest,prior,abs(cohend))  #output=c(FPR,x0,y0,x1,y1)
  fpz2=out[1]
  #Power and likelihood ratio: NB for two sided test, need 2*y0
  LR=out[5]/(2*out[3])        #lik ratio (Hi1/H0) =y1/2*y0
  
  #make tidy table of results
  results.utest<-data.frame(values=c(signif[1],
                                     w.statistic,
                                     cohend,
                                     power,
                                     signif(bayesF$bf, 3),
                                     signif(bayesF$error, 3),
                                     signif(fpz1, 3),
                                     signif(fpz2, 3),
                                     signif(LR, 3)))
  rownames(results.utest)<-c("Significance level",
                             "MW U-Test, W",
                             "Cohen's D",
                             "stat. Power",
                             "Bayes Factor",
                             "Bayes Factor error",
                             paste("FP risk, prior ",priorval[1]),
                             paste("FP risk, prior ",priorval[2]),
                             "Likelihood Ratio")
  # plot two PIs with asterisks
  plots.2test<-list(ggplot(melt(PIstat), aes(variable, value)) +
                      geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
                      geom_boxplot(fill = boxcolors, notch = TRUE, outlier.color=NA, width=0.8, size=0.6) +
                      geom_jitter(data = melt(PIstat), aes(variable, value), position=position_jitter(0.3), shape=21, size=3, colour="black", fill="grey50", alpha=0.4) +
                      ggtitle(paste("U-Test, p=", utest)) +
                      scale_y_continuous(breaks = seq(-1, 1, .2)) +
                      theme_light(base_size = 16) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
                      theme(axis.text.y = element_text(size=18))+ ylab(paste("PI", learningscore, " [rel. units]", sep = ""))+ xlab("Groups")+ theme(aspect.ratio=3/NofGroups)+
                      geom_signif(comparisons = list(c(colnames(PIstat[1]), colnames(PIstat[2]))), map_signif_level= c("***"= signif[3],"**"= signif[2], "*"= signif[1])) +
                      samplesizes.annotate(boxes, samplesizes))
  
  #add table with results and plot
  plots.2test[[2]]<-tableGrob(results.utest)
  grid.arrange(grobs = plots.2test, ncol=2)

