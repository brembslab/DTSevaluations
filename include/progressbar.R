###DTS script to display a progressbar during the evaluation
if (exists("starttime")){iter_time = round((Sys.time()-starttime), 2)} else iter_time = 20  #calculates the iteration time
if (exists("Progressbar")){dev.off()} #deletes the previous plot. If not, this will generate an ever increasing number of plots in the end.
while (!is.null(dev.list()))  dev.off()
progress = round(flycount*(100/(totalflies))) #calculates the progress in percentage
esttime = (Sys.time() + (iter_time * (totalflies-flycount))) #estimated finish time, based on the last iteration and the number of flies left
rstudioapi::executeCommand("activatePlots") #switch focus from busy animation in viewer to progress bar in plots
Progressbar = barplot(progress,
                      col = "grey", ylab = "% progress",
                      ylim=c(0,100), axes = FALSE) #set axis to 100 and then removes it
axis(2, seq(0,100,25), las=2) #sets the axis ticks
mtext(paste("Iteration time: ", iter_time, "sec \n Current fly:", flyname, "\n Current group:", grp_title), side=3)
mtext(paste("Est. finish time",substring(esttime, 12)), side = 1)
text(Progressbar,16, paste(progress, "% completed \n Flies left:", (totalflies-flycount))) #adds the percentage as text and the number of flies left
starttime = Sys.time() #sets the start time until it reaches this point in the next iteration. 1st iteration is hardcoded to 20 seconds
flycount = flycount+1