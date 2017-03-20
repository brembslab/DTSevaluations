################## An R-script to import DTS data and to plot it in several ways

library(ggplot2)
library(tidyr)

## source the script with the functions needed for analysis
source("readXMLdatafile.R")

##### read the data with the corresponding function
singleflydata <- flyDataImport()

##extract the rawdata
rawdata <- singleflydata[[9]]


## plot the traces and save them as .png files
png(file = "traces.png", width = 20000) # direct the following output to the image
plot(x = rownames(rawdata), rawdata$a_pos, type = "l", col="red3", ylab = "position[arb.units]")
par(new=TRUE)
plot(x = rownames(rawdata), rawdata$torque, type = "l", col="blue", main = "Fly Traces", axes=F, xlab=NA, ylab=NA)
axis(side = 4)
mtext(side = 4, line = 3, 'torque')
graphics.off()

png(file = "histograms.png", width = 1000) # direct the following output to the image
ggplot(gather(rawdata, cols, value), aes(x = value)) + 
  geom_histogram(binwidth = 10) + facet_grid(.~cols)
graphics.off()

png(file = "pos_histo.png", width = 1000) # direct the following output to the image
  ggplot(data=rawdata, aes(rawdata$a_pos)) + geom_histogram(binwidth=10)
graphics.off()

png(file = "torque_histo.png", width = 1000) # direct the following output to the image
    ggplot(data=rawdata, aes(rawdata$torque)) + geom_histogram(binwidth=3)
graphics.off()