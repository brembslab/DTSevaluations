###DTS script to create superimposed histograms from the test periods of two groups

if (NofGroups==2){
  if('yt' %in% sequence$type || 'sw' %in% sequence$type) #for yt or sw experiments, collect fly data
  {
    if(x==1){
      histo1 <- data.frame(pooled.data[[learningscore]][["fly"]])
      histo1$v2 = groupnames[x]
      colnames(histo1)=c("fly","group")
    } else {
      histo2 <- data.frame(pooled.data[[learningscore]][["fly"]])
      histo2$v2 = groupnames[x]
      colnames(histo2)=c("fly","group")
      supHistos <- rbind(histo1,histo2) #make dataframe with fly data from both groups and group name as factor
    }
  } else if ('fs' %in% sequence$type || 'color' %in% sequence$type) #for fs or color experiments, collect arena position data and fold them to 0..90°
  {
    if(x==1){
      histo1 <- data.frame(pooled.data[[learningscore]][["a_pos"]])
      histo1$v2 = groupnames[x]
      colnames(histo1)=c("a_pos","group")
      histo1$a_pos = abs(histo1$a_pos)/10    #fold position data over to look at 180Â° equivalent fixation and bring into degree range
      histo1$a_pos[histo1$a_pos>90] = -histo1$a_pos[histo1$a_pos>90]+180 #fold anything larger than 90° to 0..90°
    } else {
      histo2 <- data.frame(pooled.data[[learningscore]][["a_pos"]])
      histo2$v2 = groupnames[x]
      colnames(histo2)=c("a_pos","group")
      histo2$a_pos = abs(histo2$a_pos)/10  #fold position data over to look at 180Â° equivalent fixation and bring into degree range
      histo2$a_pos[histo2$a_pos>90] = -histo2$a_pos[histo2$a_pos>90]+180 #fold anything larger than 90° to 0..90°
      supHistos <- rbind(histo1,histo2)  #make dataframe with position data from both groups and group name as factor
    }
  }
  
}