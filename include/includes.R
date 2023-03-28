###DTS script cleaning memory loading libraries

rm(list=ls())                         #clean memory
gc()                                  #collect garbage
if(!is.null(dev.list())) dev.off()    #clear plots

#load libraries
library(ggplot2)
library(cowplot)
library(ggiraph)
library(tidyr)
library(dygraphs)
library(grid)
library(reshape2)
library(plyr)
library(dplyr)
library(gridExtra)
library(yaml)
library(ggsignif)
library(effsize)
library(pwr)
library(BayesFactor)
library(genefilter)
library(seewave)
library(lubridate)
library(rmarkdown)
library(markdown)
library(knitr)
library(dabestr)
library(zoo)
library(tidyverse)
library(questionr)
library(data.table)
library(DescTools)
library(magick)
library(reactable)
library(raincloudplots)
library(sicegar)
library(ggrain)