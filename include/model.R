library(data.table)
library(randomForest)

pause_annotations <- readRDS("pause_annotations.rds")

trainingdata <- list()
testingdata <- list()

wtb_vec <- seq(0, 43, by = 1) #Creates a vector, numbering the wtb flies, to be used as training data
rut_vec <- seq(0, 26, by = 1) #Creates a vector, numbering the rut flies, to be used as testing data

# Generate training data from the wtb flies
for(x in seq_along(wtb_vec)) {
  
  file_number <- wtb_vec[x]
  
  trainingdata[[x]] <- flyDataImport(
    paste0(
      "/media/fridik/Data 1/Project/DTSevaluations/example_data/Self-learning/wtb-",
      file_number,
      ".xml"
    )
  )
}

# Generate testing data from the rut flies
for(i in seq_along(rut_vec)) {
  
  file_number <- rut_vec[i]
  
  testingdata[[i]] <- flyDataImport(
    paste0(
      "/media/fridik/Data 1/Project/DTSevaluations/example_data/Self-learning/rut-",
      file_number,
      ".xml"
    )
  )
}

extract_pause_features <- function(rawdata, annotations,
                                   span = 21,
                                   pause_fraction = 0.8) {
  
  # Extract position and time
  position <- rawdata$fly
  time <- rawdata$time
  
  n <- length(position)
  
  # Calculate changes in position
  velocity <- c(NA, diff(position))
  acceleration <- c(NA, diff(velocity))
  
  # Create feature data frame
  features <- data.frame(
    time = time,
    variance = frollvar(position, n = span,
                        align = "center", fill = NA),
    sd = sqrt(frollvar(position, n = span,
                       align = "center", fill = NA)),
    mean_abs_velocity = frollmean(
      abs(velocity), n = span,
      align = "center", fill = NA
    ),
    max_abs_velocity = frollapply(
      abs(velocity), n = span,
      FUN = max, align = "center", fill = NA,
      na.rm = TRUE
    ),
    mean_abs_acceleration = frollmean(
      abs(acceleration), n = span,
      align = "center", fill = NA
    )
  )
  
  # Assign pause labels to individual samples
  is_pause <- rep(FALSE, n)
  
  if (nrow(annotations) > 0) {
    for (i in seq_len(nrow(annotations))) {
      is_pause <- is_pause |
        (time >= annotations$start[i] &
           time <= annotations$end[i])
    }
  }
  
  # Calculate the fraction of pause samples in each window
  pause_fraction_vec <- frollmean(
    as.numeric(is_pause),
    n = span,
    align = "center",
    fill = NA
  )
  
  # Label windows with sufficient overlap with a pause
  features$pause <- ifelse(
    pause_fraction_vec >= pause_fraction,
    "pause",
    "flight"
  )
  
  # Remove windows with incomplete feature values
  features <- features[complete.cases(features), ]
  
  # Make the target a factor for classification
  features$pause <- factor(
    features$pause,
    levels = c("flight", "pause")
  )
  
  return(features)
}

rawdata <- trainingdata[[2]]$rawdata
annotations <- pause_annotations[[2]]

features_2 <- extract_pause_features(
  rawdata,
  annotations
)

# Inspect the resulting data frame
head(features_2)

# Check the number of windows assigned to each class
table(features_2$pause)

# Inspect the feature distributions
summary(features_2)

# Compare feature values for pauses and flight
aggregate(
  cbind(
    variance,
    sd,
    mean_abs_velocity,
    max_abs_velocity,
    mean_abs_acceleration
  ) ~ pause,
  data = features_2,
  FUN = median
)