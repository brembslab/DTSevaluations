## R-Script to read ASCII performance index data and plot them

library(ggplot2)
library(reshape2)

#read data file
PIprofile <- read.csv(file=choose.files(), header=TRUE, sep="\t")

#identify sorting columns, use them to define plot colors and then remove them
if("genotype" %in% colnames(PIprofile))
{
  #Define colors for LTM graphs
  barcolors = c("lightyellow", "orange", "lightyellow")
  PIprofile$genotype <- NULL
}

if("Nr" %in% colnames(PIprofile))
{
  #Define colors for foraging graphs
  barcolors = c("lightyellow", "orange", "orange", "lightyellow", "orange", "orange", "lightyellow")
  PIprofile$Nr <- NULL
}


## plot bar plot with sem
#compute summary statistics
error <- data.frame(period=integer(ncol(PIprofile)), mean=numeric(ncol(PIprofile)), sem=numeric(ncol(PIprofile)))
for(e in 1:ncol(PIprofile))
{
  error$period[e]=e
  error$mean[e]=mean(PIprofile[,e])
  error$sem[e]=sd(PIprofile[,e]/sqrt(nrow(PIprofile)))
}
# plot graph
ggplot(error, aes(x=period, y=mean)) + 
  geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
  geom_bar(fill = barcolors, position=position_dodge(), stat="identity", colour="black") +
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),
                width=0,
                size=1.5,
                position=position_dodge(.9)) +
  ggtitle(paste("PI Profile, N=",nrow(PIprofile))) +
  scale_x_continuous(breaks = seq(1, ncol(PIprofile), 1)) +
  scale_y_continuous(breaks = seq(-1, 1, .2)) +
  theme_light(base_size = 18) + theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(), panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
  theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/ncol(PIprofile))

## Plot box&dotplot with notches
ggplot(melt(PIprofile), aes(variable, value)) +
  geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
  geom_boxplot(fill = barcolors, notch = TRUE, outlier.color=NA, width=0.8, size=0.6) +
  geom_jitter(data = melt(PIprofile), aes(variable, value), position=position_jitter(0.3), shape=21, size=3, colour="black", fill="grey50", alpha=0.4) +
  ggtitle(paste("PI Profile, N=",nrow(PIprofile))) +
  scale_y_continuous(breaks = seq(-1, 1, .2)) +
  theme_light(base_size = 18) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
  theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/ncol(PIprofile))

## Plot box&dotplot without notches
ggplot(melt(PIprofile), aes(variable, value)) +
  geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
  geom_boxplot(fill = barcolors, notch = FALSE, outlier.color=NA, width=0.8, size=0.6) +
  geom_jitter(data = melt(PIprofile), aes(variable, value), position=position_jitter(0.3), shape=21, size=3, colour="black", fill="grey50", alpha=0.4) +
  ggtitle(paste("PI Profile, N=",nrow(PIprofile))) +
  scale_y_continuous(breaks = seq(-1, 1, .2)) +
  theme_light(base_size = 18) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
  theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/ncol(PIprofile))

## plot violin plot
ggplot(melt(PIprofile), aes(variable, value)) +
  geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
  geom_violin(width = 1.1) +
  geom_boxplot(fill = barcolors, width = 0.1, outlier.color="darkred") +
  ggtitle(paste("PI Profile, N=",nrow(PIprofile))) +
  scale_y_continuous(breaks = seq(-1, 1, .2)) +
  theme_light(base_size = 18) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
  theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/ncol(PIprofile))