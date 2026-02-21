# ===============================
# Homework 4 
# ===============================

# Setting working directory
setwd("/Users/sabreenaaleemnabeela/Desktop/Main Folder/Data Stewardship/HW4")

# =====================================
# PART 1: pollutantmean
# =====================================

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  all_data <- numeric()
  
  for (i in id) {
    file_name <- paste0(sprintf("%03d", i), ".csv")
    file_path <- file.path(directory, file_name)
    
    data <- read.csv(file_path)
    
    all_data <- c(all_data, data[[pollutant]])
  }
  
  mean(all_data, na.rm = TRUE)
}


# =====================================
# PART 2: complete
# =====================================

complete <- function(directory, id = 1:332) {
  
  file_names <- character()
  nobs_vector <- numeric()
  
  for (i in id) {
    file_name <- paste0(sprintf("%03d", i), ".csv")
    file_path <- file.path(directory, file_name)
    
    data <- read.csv(file_path)
    
    file_names <- c(file_names, file_name)
    nobs_vector <- c(nobs_vector, sum(complete.cases(data)))
  }
  
  data.frame(file = file_names, nobs = nobs_vector)
}


# =====================================
# PART 3: corr
# =====================================

corr <- function(directory, threshold = 0) {
  
  correlations <- numeric()
  
  # Getting complete case counts
  cc <- complete(directory)
  
  # Selecting files meeting threshold
  valid_files <- cc$file[cc$nobs > threshold]
  
  for (file_name in valid_files) {
    
    file_path <- file.path(directory, file_name)
    data <- read.csv(file_path)
    
    clean_data <- data[complete.cases(data), ]
    
    if (nrow(clean_data) > 0) {
      correlations <- c(correlations,
                        cor(clean_data$sulfate,
                            clean_data$nitrate))
    }
  }
  
  correlations
}
