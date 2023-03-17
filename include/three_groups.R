# Organize the groups order and conbination baesd on the description
# e.g. 
# groupdescription[1] = control
# groupdescription[2] = control
# groupdescription[3] = experimental
# doubleton[1] = control, doubleton[2] = control
# singleton = experimental
# statistical analysis will be: singleton vs doubleton[1] and singleton vs doubleton[2]
doubleton <- list()
singleton <- list()

if(NofGroups==3 & length(unique(groupdescriptions))==2) {
  if(groupdescriptions[1] == groupdescriptions[2]){
      doubleton[[1]] = groupdescriptions[1]
      doubleton[[2]] = groupdescriptions[2]
      singleton = groupdescriptions[3]
  } 
  else if(groupdescriptions[2] == groupdescriptions[3]){
      doubleton[[1]] = groupdescriptions[2]
      doubleton[[2]] = groupdescriptions[3]
      singleton = groupdescriptions[1]
  }
  else {
      doubleton[[1]] = groupdescriptions[1]
      doubleton[[2]] = groupdescriptions[3]
      singleton = groupdescriptions[2]
  }
}


# statistical test function for three groups
statistical_test <- function(parameter1, parameter2) {
  utest = signif(wilcox.test(parameter1, parameter2)$p.value, 3)  #compare the two groups with a U-test and collect p-value
  wstatistic = signif(wilcox.test(parameter1, parameter2)$statistic, 3)
  #compute effect size Cohen's D
  cohend = signif(cohen.d(parameter1, parameter2)$estimate, 3)
  #calculate statistical power
  alt = dataset.data[["statistics"]][["three.groups"]][["power"]]
  power = signif(pwr.t2n.test(n1 = length(parameter1), n2= length(parameter2), d = cohend, alternative = alt, 
                              sig.level = signif[1])$power, 3)
  #calculate Bayes Factor
  bayesF = extractBF(ttestBF(parameter1, parameter2))
  #calculate FPR for priors set in dataset file#
  #run first prior  
  prior=priorval[1]
  out=calc.FPR(samplesizes,utest,prior,abs(cohend))  #output=c(FPR,x0,y0,x1,y1)
  fpz1=out[1]
  #run second prior  
  prior=priorval[2]
  out=calc.FPR(samplesizes,utest,prior,abs(cohend))  #output=c(FPR,x0,y0,x1,y1)
  fpz2=out[1]
  #Power and likelihood ratio: NB for two sided test, need 2*y0
  LR=out[5]/(2*out[3])    #lik ratio (Hi1/H0) =y1/2*y0
  #make table of results
  result = c(signif[1],
             wstatistic,
             utest,
             cohend,
             power,
             signif(bayesF$bf, 3),
             signif(bayesF$error, 3),
             signif(fpz1, 3),
             signif(fpz2, 3),
             signif(LR, 3))
  return(result)
}