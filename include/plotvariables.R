#####Various useful variables for plotting PIs and other data

boxes<-c(1:NofGroups)
boxcolors = c(colorrange[1:NofGroups])
if(Dwell){
  dwellrange=-round(1.5*max(dwellrange))
  dwellrange[2]=-dwellrange
}

##use categories to create variable defining the shapes of individual data points
#by default, create the numbers for filled shapes
HeatOn=as.integer(as.factor(PIstatCombined$category))+20
#special shapes for special cases
if(length(unique(PIstatCombined$category))==2){    #if there are only two categories
  HeatOn=case_when(
    PIstatCombined$category == "Right torque" ~ "►",
    PIstatCombined$category == "Left torque"~ "◄",
    TRUE ~ as.character(HeatOn)
  )
  HeatOn=case_when(
    PIstatCombined$category == "Right torque DL" ~ "►",
    PIstatCombined$category == "Left torque DL"~ "◄",
    TRUE ~ as.character(HeatOn)
  )
  HeatOn=case_when(
    PIstatCombined$category == "upright T"~"ꓔ",
    PIstatCombined$category == "inverted T" ~ "ꓕ",
    TRUE ~ as.character(HeatOn)
  )
  if(typeof(HeatOn)=="character" && (HeatOn[1]=="21" || HeatOn[1]=="22")){
    HeatOn=as.double(HeatOn) #case_when changes type of variable to 'character', even if no match is found, duh!
  }
}
