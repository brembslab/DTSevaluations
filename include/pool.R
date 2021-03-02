###DTS script to pool raw data traves in various ways for each group

##pool all data by period

pooled.data<-list()

for(i in 1:NofPeriods)
{
  period.data<-data.frame()
  for (l in 1:length(xml_list))
  {
    period.data <- rbind(period.data, grouped.data[[l]][[i]])
  }
  pooled.data[[i]] <- period.data
}

##create a list with data for histograms
grouphistdata[[x]] = bind_rows(pooled.data, .id = "period")

## pool all fly and position data into single data.frame
all.data <- do.call(rbind, pooled.data)
