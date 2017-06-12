## R-Script to read ASCII performance index data and plot them

library(ggplot2)
library(reshape2)
library(gridExtra)
library(ggsignif)
library(effsize)
library(pwr)
library(BayesFactor)

source("DTS_plotfunctions.R")

#read data files
NofGroups = 2 #set group number
grouped.PIprofiles <-list()

for(x in 1:NofGroups)
{
  grouped.PIprofiles[[x]] <- read.csv(file=choose.files(), header=TRUE, sep=";")
  #drop all columns not performance indices
  grouped.PIprofiles[[x]]$FILE <- NULL
  grouped.PIprofiles[[x]]$DATE <- NULL
  grouped.PIprofiles[[x]]$HEAT <- NULL
}


###### statistical evaluations ######

signif = c(0.005, 0.001, 0.0001) #set significance levels
learningscore=8 #set learning score to be evaluated
groupnames = c("radish", "Canton S")  #set a vector with all group names
#create new dataframe with only the chosen PI values
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
  
  # plot PI box plot with power analysis and asterisks for Wilcoxon test against zero
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

