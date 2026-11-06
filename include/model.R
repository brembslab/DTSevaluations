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
    variancemid = frollvar(position, n = span,
                        align = "center", fill = NA),
    varianceright = frollvar(position, n = span,
                           align = "right", fill = NA),
    varianceleft = frollvar(position, n = span,
                           align = "left", fill = NA),
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

# Store features from each annotated recording
feature_list <- list()

for (i in seq_along(trainingdata)) {
  
  # Skip recordings without annotations
  if (i > length(pause_annotations) ||
      is.null(pause_annotations[[i]]) ||
      nrow(pause_annotations[[i]]) == 0) {
    next
  }
  
  # Extract raw data and annotations
  rawdata <- trainingdata[[i]]$rawdata
  annotations <- pause_annotations[[i]]
  
  # Extract features
  features <- extract_pause_features(
    rawdata,
    annotations
  )
  
  # Keep track of the source recording
  features$recording <- i
  
  # Store the result
  feature_list[[as.character(i)]] <- features
}

# Combine all recordings
training_features <- do.call(rbind, feature_list)

# Check the results
dim(training_features)
table(training_features$pause)
length(unique(training_features$recording))

set.seed(123)

# Identify the available recordings
recording_ids <- unique(training_features$recording)

# Randomly select 20% of recordings for validation
validation_ids <- sample(
  recording_ids,
  size = ceiling(0.2 * length(recording_ids))
)

# Split the data by recording
train_data <- training_features[
  !training_features$recording %in% validation_ids,
]

validation_data <- training_features[
  training_features$recording %in% validation_ids,
]

# Check the split
length(unique(train_data$recording))
length(unique(validation_data$recording))

table(train_data$pause)
table(validation_data$pause)

library(randomForest)

set.seed(123)

#Trains random forest model
rf_model <- randomForest(
  pause ~ variancemid +
    varianceright + 
    varianceleft +
    sd +
    mean_abs_velocity +
    max_abs_velocity +
    mean_abs_acceleration,
  data = train_data,
  ntree = 500,
  importance = TRUE,
  classwt = c("flight" = 1, "pause" = 5)
)

print(rf_model)

# Predict the held-out recordings
predictions <- predict(
  rf_model,
  newdata = validation_data
)

# Compare predictions with manual labels
confusion_matrix <- table(
  Actual = validation_data$pause,
  Predicted = predictions
)

confusion_matrix

# Extract confusion-matrix values
TP <- confusion_matrix["pause", "pause"]
FN <- confusion_matrix["pause", "flight"]
FP <- confusion_matrix["flight", "pause"]
TN <- confusion_matrix["flight", "flight"]

# Performance metrics
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
F1 <- 2 * precision * recall / (precision + recall)
accuracy <- (TP + TN) / sum(confusion_matrix)

data.frame(
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1 = F1
)