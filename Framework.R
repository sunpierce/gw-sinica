#### setup ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS) # LDA, QDA
library(class) # KNN
library(caret)
library(smotefamily)
library(SPlit)
library(vioplot)
library(knitr)
library(kableExtra)
library(randomForest)
library(gbm)
library(GpGp)
library(fields)
library(tree)
library(readr)
library(ggthemes)
library(parallel)
library(doSNOW)
library(xgboost)
library(e1071) # SVM
library(gamlss) # beta regression
library(statmod) # tweedie distribution
library(tweedie)
library(glmnet)
library(mgcv) # tweedie distribution
library(pscl)
library(betareg)
library(brms)
library(mltools) # mcc
library(yardstick) # balanced accuracy
library(rlang)

#### new_df: interleaved columns with center and neighbor for all stations and all station IDs ####
# Year: 2008 ~ 2023, Station ID: 1 ~ 68
# Note: Generating new_df using the above code is time-consuming. 
# It is recommended to run it once, save the result to a CSV file, and then load it directly using read.csv() in the future.
# Make sure to run the code to create new_df before executing the read.csv() command.
new_df = read.csv("new_df_diff8760.csv")

#### Code to generate new_df.csv ####
center = read.csv("triangle_water_level_data.csv")
neighbor = read.csv("neighbor_triangle_water_level_data.csv")
# Step 1: Get the first four columns from 'center'
new_df <- center[, 1:4]

# Step 2: Interleave columns from 'center' and 'neighbor' starting from the fifth column
interleaved_columns <- do.call(cbind,
                               lapply(5:ncol(center), function(i) {
                                 cbind(center[, i], neighbor[, i])
                               })
)

# Combine the first four columns with the interleaved columns
new_df <- cbind(new_df, interleaved_columns)

# Step 3: Rename the columns starting from the fifth column
colnames(new_df)[5:ncol(new_df)] <- unlist(
  lapply(1:68, function(i) c(paste0("t", i), paste0("n", i)))
)

# Display the new data frame
new_df

# Remove seasonality (daily: 24, weekly: 168, yearly: 8760)
new_df_diff = new_df  # copy the original
for (i in 5:ncol(new_df)) {
  new_df_diff[[i]] = c(rep(NA, 8760), diff(new_df[[i]], lag = 8760))
}

new_df = new_df_diff

# Define the range of interest (you can adjust this range as needed)
min_val <- floor(min(new_df[, 5:ncol(new_df)], na.rm = TRUE)) # Minimum value in the data
max_val <- ceiling(max(new_df[, 5:ncol(new_df)], na.rm = TRUE)) # Maximum value in the data

# Generate intervals of length 0.1 m
ranges <- lapply(seq(min_val, max_val - 0.1, by = 0.1),
                 function(x) c(round(x, 1), round(x + 0.1, 1)))

# Automatically generate labels from 1 to the number of intervals
labels <- seq_along(ranges)

# Function to assign levels based on the defined ranges, handling NA values
assign_level <- function(x) {
  if (is.na(x)) {
    return(NA)  # Return NA if the value is NA
  }
  for (i in seq_along(ranges)) {
    if (x >= ranges[[i]][1] && x < ranges[[i]][2]) {
      return(labels[i])
    }
    if (x == ranges[[290]][2]) return(labels[290])
  }
  return(NA)  # Return NA if no range is matched
}

# Apply the function to each value in the relevant columns (5th to last)
new_df[, 5:ncol(new_df)] <- apply(new_df[, 5:ncol(new_df)], 2, function(column) {
  sapply(column, assign_level)
})

# Display the updated data frame (with label)
# Year: 2008 ~ 2023, Station ID: 1 ~ 68
new_df

# Hour -= 1 for consistent with Hour format in earthquake data (0-23)
new_df$Hour = new_df$Hour - 1

# Save to csv file
write_csv(new_df, "new_df_diff8760.csv")


#### new_df_train ####

# IMPORTANT: Adjust the time range to match your training/testing window!
new_df_train = new_df %>% filter(Year < 2020 | (Year == 2020 & (Month < 3 | ( Month == 3 & Day <= 31))))
# new_df_train = new_df %>% filter(Year < 2020 | (Year == 2020 & (Month < 9 | ( Month == 9 & Day <= 30))))
# new_df_train = new_df %>% filter(Year < 2021 | (Year == 2021 & (Month < 3 | ( Month == 3 & Day <= 31))))
# new_df_train = new_df %>% filter(Year < 2021 | (Year == 2021 & (Month < 9 | ( Month == 9 & Day <= 30))))
# new_df_train = new_df %>% filter(Year < 2022 | (Year == 2022 & (Month < 3 | ( Month == 3 & Day <= 31))))
# new_df_train = new_df %>% filter(Year < 2022 | (Year == 2022 & (Month < 9 | ( Month == 9 & Day <= 30))))


#### earthquake_train ####
earthquake = read.csv("Earthquake_data.csv")

# IMPORTANT: Adjust the time range to match your training/testing window!
earthquake_train = earthquake %>% filter(Year < 2020 | (Year == 2020 & (Month < 3 | (Month == 3 & Day <= 31))))
# earthquake_train = earthquake %>% filter(Year < 2020 | (Year == 2020 & (Month < 9 | (Month == 9 & Day <= 30))))
# earthquake_train = earthquake %>% filter(Year < 2021 | (Year == 2021 & (Month < 3 | (Month == 3 & Day <= 31))))
# earthquake_train = earthquake %>% filter(Year < 2021 | (Year == 2021 & (Month < 9 | (Month == 9 & Day <= 30))))
# earthquake_train = earthquake %>% filter(Year < 2022 | (Year == 2022 & (Month < 3 | (Month == 3 & Day <= 31))))
# earthquake_train = earthquake %>% filter(Year < 2022 | (Year == 2022 & (Month < 9 | (Month == 9 & Day <= 30))))


#### sorted_combination_freq_whole: denominator for calculating occurrence ####
# Calculate the frequency
new_df = new_df_train
# Initialize an empty vector to store all individual combinations
all_combinations <- c()

# Loop through each row to extract individual combinations
for (i in 1:nrow(new_df)) {
  # Extract combinations for the current row
  pairs <- sapply(1:68, function(j) {
    t_val <- new_df[i, paste0("t", j)]
    n_val <- new_df[i, paste0("n", j)]
    if (is.na(t_val) || is.na(n_val)) {
      return("NA")
    } else {
      return(paste(t_val, n_val, sep = "_"))
    }
  })
  # Append the individual combinations to the vector
  all_combinations <- c(all_combinations, pairs)
}

# Calculate the frequency of each unique combination (can't use HourBlock to calculate)
combination_freq_whole <- table(all_combinations)

# Sorting
# Convert the names of combination_freq_whole into a data frame with X and Y as numeric columns
comb_df <- data.frame(
  combination = names(combination_freq_whole),
  freq = as.numeric(combination_freq_whole),
  stringsAsFactors = FALSE
)

# Split the combination into X and Y parts
comb_df$X <- as.numeric(sub("_.*", "", comb_df$combination))
comb_df$Y <- as.numeric(sub(".*_", "", comb_df$combination))

# Sort by X first and then by Y
sorted_comb_df <- comb_df[order(comb_df$X, comb_df$Y), ]

# Display the sorted combinations with their frequencies
sorted_combination_freq_whole <- sorted_comb_df[, c("combination", "freq")]
sorted_combination_freq_whole = data.frame(combination = sorted_combination_freq_whole$combination, frequency = sorted_combination_freq_whole$freq)
print(sorted_combination_freq_whole)

#### sorted_combination_freq_lag1 ~ sorted_combination_freq_lag9 ####
# Lag 1
# Read data
earthquake = earthquake_train

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake = earthquake[,c(2:5,8:12)]

# Initialize columns for the previous Day
earthquake$PrevDay1 <- earthquake$Day
earthquake$PrevMonth1 <- earthquake$Month
earthquake$PrevYear1 <- earthquake$Year

# Function to adjust for the previous day and handle month/year changes
adjust_date_lag1 <- function(year, month, day) {
  # Subtract 1 day
  day <- day - 1
  # Adjust day and month
  if (day <= 0) {
    month <- month - 1
    if (month <= 0) {
      month <- 12
      year <- year - 1
    }
    # Set day to the last day of the previous month
    if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
      day <- 31
    } else if (month %in% c(4, 6, 9, 11)) {
      day <- 30
    } else {
      if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
        day <- 29
      } else {
        day <- 28
      }
    }
  }
  return(c(year, month, day))
}

# Apply the adjustment for each row
adjustments <- t(sapply(1:nrow(earthquake), function(i) {
  adjust_date_lag1(earthquake$Year[i], earthquake$Month[i], earthquake$Day[i])
}))

# Assign adjusted values to the dataframe
earthquake$PrevYear1 <- adjustments[, 1]
earthquake$PrevMonth1 <- adjustments[, 2]
earthquake$PrevDay1 <- adjustments[, 3]

# Merge earthquake with new_df for lag 1
merged_df_lag1 <- merge(
  earthquake, 
  new_df, 
  by.x = c("PrevYear1", "PrevMonth1", "PrevDay1", "Hour"), 
  by.y = c("Year", "Month", "Day", "Hour"),
  all.x = TRUE
)

# Display the merged data frame
print(merged_df_lag1)

# Standardize the merged_df
merged_df = merged_df_lag1[,-(1:4)]

# Step 2: Extract the corresponding t and n columns based on tri_ID
merged_df$t <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("t", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})
merged_df$n <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("n", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})

# Step 3: Select only relevant columns for earthquake
earthquake <- merged_df[, c(1:8, ncol(merged_df)-1, ncol(merged_df))]

# Display the updated earthquake data frame
print(earthquake)

# Calculate the frequency
# Step 1: Create a vector of combinations (t, n)
combinations <- with(earthquake, ifelse(is.na(t) | is.na(n), "NA", paste(t, n, sep = "_")))

# Step 2: Calculate the frequency of each unique combination
combination_freq_lag1 <- table(combinations)

# Sorting
# Convert the names of combination_freq_whole into a data frame with X and Y as numeric columns
comb_df <- data.frame(
  combination = names(combination_freq_lag1),
  freq = as.numeric(combination_freq_lag1),
  stringsAsFactors = FALSE
)

# Split the combination into X and Y parts
comb_df$X <- as.numeric(sub("_.*", "", comb_df$combination))
comb_df$Y <- as.numeric(sub(".*_", "", comb_df$combination))

# Sort by X first and then by Y
sorted_comb_df <- comb_df[order(comb_df$X, comb_df$Y), ]

# Display the sorted combinations with their frequencies
sorted_combination_freq_lag1 <- sorted_comb_df[, c("combination", "freq")]
sorted_combination_freq_lag1 = data.frame(combination = sorted_combination_freq_lag1$combination, frequency = sorted_combination_freq_lag1$freq)
sorted_combination_freq_lag1 = sorted_combination_freq_lag1[-nrow(sorted_combination_freq_lag1),] # remove NA
print(sorted_combination_freq_lag1)

# Lag 2
# Read data
earthquake = earthquake_train

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake = earthquake[,c(2:5,8:12)]

# Initialize columns for the previous 2 days
earthquake$PrevDay2 <- earthquake$Day
earthquake$PrevMonth2 <- earthquake$Month
earthquake$PrevYear2 <- earthquake$Year

# Function to adjust for the previous 2 days and handle month/year changes
adjust_date_lag2 <- function(year, month, day) {
  # Subtract 2 days
  day <- day - 2
  
  # Adjust day and month
  if (day <= 0) {
    month <- month - 1
    if (month <= 0) {
      month <- 12
      year <- year - 1
    }
    
    # Set day to the last day of the previous month
    if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
      day <- 31 + day
    } else if (month %in% c(4, 6, 9, 11)) {
      day <- 30 + day
    } else {
      if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
        day <- 29 + day
      } else {
        day <- 28 + day
      }
    }
  }
  
  return(c(year, month, day))
}

# Apply the adjustment for each row
adjustments <- t(sapply(1:nrow(earthquake), function(i) {
  adjust_date_lag2(earthquake$Year[i], earthquake$Month[i], earthquake$Day[i])
}))

# Assign adjusted values to the dataframe
earthquake$PrevYear2 <- adjustments[, 1]
earthquake$PrevMonth2 <- adjustments[, 2]
earthquake$PrevDay2 <- adjustments[, 3]

# Merge earthquake with new_df for lag 2
merged_df_lag2 <- merge(
  earthquake, 
  new_df, 
  by.x = c("PrevYear2", "PrevMonth2", "PrevDay2", "Hour"), 
  by.y = c("Year", "Month", "Day", "Hour"),
  all.x = TRUE
)

# Display the merged data frame
print(merged_df_lag2)

# Standardize the merged_df
merged_df = merged_df_lag2[,-(1:4)]

# Step 2: Extract the corresponding t and n columns based on tri_ID
merged_df$t <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("t", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})
merged_df$n <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("n", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})

# Step 3: Select only relevant columns for earthquake
earthquake <- merged_df[, c(1:8, ncol(merged_df)-1, ncol(merged_df))]

# Display the updated earthquake data frame
print(earthquake)

# Calculate the frequency
# Step 1: Create a vector of combinations (t, n)
combinations <- with(earthquake, ifelse(is.na(t) | is.na(n), "NA", paste(t, n, sep = "_")))

# Step 2: Calculate the frequency of each unique combination
combination_freq_lag2 <- table(combinations)

# Sorting
# Convert the names of combination_freq_whole into a data frame with X and Y as numeric columns
comb_df <- data.frame(
  combination = names(combination_freq_lag2),
  freq = as.numeric(combination_freq_lag2),
  stringsAsFactors = FALSE
)

# Split the combination into X and Y parts
comb_df$X <- as.numeric(sub("_.*", "", comb_df$combination))
comb_df$Y <- as.numeric(sub(".*_", "", comb_df$combination))

# Sort by X first and then by Y
sorted_comb_df <- comb_df[order(comb_df$X, comb_df$Y), ]

# Display the sorted combinations with their frequencies
sorted_combination_freq_lag2 <- sorted_comb_df[, c("combination", "freq")]
sorted_combination_freq_lag2 = data.frame(combination = sorted_combination_freq_lag2$combination, frequency = sorted_combination_freq_lag2$freq)
sorted_combination_freq_lag2 = sorted_combination_freq_lag2[-nrow(sorted_combination_freq_lag2),] # remove NA
print(sorted_combination_freq_lag2)


# Lag 3
# Read data
earthquake = earthquake_train

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake = earthquake[,c(2:5,8:12)]

# Initialize columns for the previous 3 days
earthquake$PrevDay3 <- earthquake$Day
earthquake$PrevMonth3 <- earthquake$Month
earthquake$PrevYear3 <- earthquake$Year

# Function to adjust for the previous 3 days and handle month/year changes
adjust_date_lag3 <- function(year, month, day) {
  # Subtract 3 days
  day <- day - 3
  # Adjust day and month
  if (day <= 0) {
    month <- month - 1
    if (month <= 0) {
      month <- 12
      year <- year - 1
    }
    # Set day to the last day of the previous month
    if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
      day <- 31 + day
    } else if (month %in% c(4, 6, 9, 11)) {
      day <- 30 + day
    } else {
      if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
        day <- 29 + day
      } else {
        day <- 28 + day
      }
    }
  }
  return(c(year, month, day))
}

# Apply the adjustment for each row
adjustments <- t(sapply(1:nrow(earthquake), function(i) {
  adjust_date_lag3(earthquake$Year[i], earthquake$Month[i], earthquake$Day[i])
}))

# Assign adjusted values to the dataframe
earthquake$PrevYear3 <- adjustments[, 1]
earthquake$PrevMonth3 <- adjustments[, 2]
earthquake$PrevDay3 <- adjustments[, 3]

# Merge earthquake with new_df for lag 3
merged_df_lag3 <- merge(
  earthquake, 
  new_df, 
  by.x = c("PrevYear3", "PrevMonth3", "PrevDay3", "Hour"), 
  by.y = c("Year", "Month", "Day", "Hour"),
  all.x = TRUE
)

# Display the merged data frame
print(merged_df_lag3)

# Standardize the merged_df
merged_df = merged_df_lag3[,-(1:4)]

# Step 2: Extract the corresponding t and n columns based on tri_ID
merged_df$t <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("t", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})
merged_df$n <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("n", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})

# Step 3: Select only relevant columns for earthquake
earthquake <- merged_df[, c(1:8, ncol(merged_df)-1, ncol(merged_df))]

# Display the updated earthquake data frame
print(earthquake)

# Calculate the frequency
# Step 1: Create a vector of combinations (t, n)
combinations <- with(earthquake, ifelse(is.na(t) | is.na(n), "NA", paste(t, n, sep = "_")))

# Step 2: Calculate the frequency of each unique combination
combination_freq_lag3 <- table(combinations)

# Sorting
# Convert the names of combination_freq_whole into a data frame with X and Y as numeric columns
comb_df <- data.frame(
  combination = names(combination_freq_lag3),
  freq = as.numeric(combination_freq_lag3),
  stringsAsFactors = FALSE
)

# Split the combination into X and Y parts
comb_df$X <- as.numeric(sub("_.*", "", comb_df$combination))
comb_df$Y <- as.numeric(sub(".*_", "", comb_df$combination))

# Sort by X first and then by Y
sorted_comb_df <- comb_df[order(comb_df$X, comb_df$Y), ]

# Display the sorted combinations with their frequencies
sorted_combination_freq_lag3 <- sorted_comb_df[, c("combination", "freq")]
sorted_combination_freq_lag3 = data.frame(combination = sorted_combination_freq_lag3$combination, frequency = sorted_combination_freq_lag3$freq)
sorted_combination_freq_lag3 = sorted_combination_freq_lag3[-nrow(sorted_combination_freq_lag3),] # remove NA
print(sorted_combination_freq_lag3)


# Lag 4
# Read data
earthquake = earthquake_train

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake = earthquake[,c(2:5,8:12)]

# Initialize columns for the previous 4 days
earthquake$PrevDay4 <- earthquake$Day
earthquake$PrevMonth4 <- earthquake$Month
earthquake$PrevYear4 <- earthquake$Year

# Function to adjust for the previous 4 days and handle month/year changes
adjust_date_lag4 <- function(year, month, day) {
  # Subtract 4 days
  day <- day - 4
  # Adjust day and month
  if (day <= 0) {
    month <- month - 1
    if (month <= 0) {
      month <- 12
      year <- year - 1
    }
    # Set day to the last day of the previous month
    if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
      day <- 31 + day
    } else if (month %in% c(4, 6, 9, 11)) {
      day <- 30 + day
    } else {
      if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
        day <- 29 + day
      } else {
        day <- 28 + day
      }
    }
  }
  return(c(year, month, day))
}

# Apply the adjustment for each row
adjustments <- t(sapply(1:nrow(earthquake), function(i) {
  adjust_date_lag4(earthquake$Year[i], earthquake$Month[i], earthquake$Day[i])
}))

# Assign adjusted values to the dataframe
earthquake$PrevYear4 <- adjustments[, 1]
earthquake$PrevMonth4 <- adjustments[, 2]
earthquake$PrevDay4 <- adjustments[, 3]

# Merge earthquake with new_df for lag 4
merged_df_lag4 <- merge(
  earthquake, 
  new_df, 
  by.x = c("PrevYear4", "PrevMonth4", "PrevDay4", "Hour"), 
  by.y = c("Year", "Month", "Day", "Hour"),
  all.x = TRUE
)

# Display the merged data frame
print(merged_df_lag4)

# Standardize the merged_df
merged_df = merged_df_lag4[,-(1:4)]

# Step 2: Extract the corresponding t and n columns based on tri_ID
merged_df$t <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("t", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})
merged_df$n <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("n", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})

# Step 3: Select only relevant columns for earthquake
earthquake <- merged_df[, c(1:8, ncol(merged_df)-1, ncol(merged_df))]

# Display the updated earthquake data frame
print(earthquake)

# Calculate the frequency
# Step 1: Create a vector of combinations (t, n)
combinations <- with(earthquake, ifelse(is.na(t) | is.na(n), "NA", paste(t, n, sep = "_")))

# Step 2: Calculate the frequency of each unique combination
combination_freq_lag4 <- table(combinations)

# Sorting
# Convert the names of combination_freq_whole into a data frame with X and Y as numeric columns
comb_df <- data.frame(
  combination = names(combination_freq_lag4),
  freq = as.numeric(combination_freq_lag4),
  stringsAsFactors = FALSE
)

# Split the combination into X and Y parts
comb_df$X <- as.numeric(sub("_.*", "", comb_df$combination))
comb_df$Y <- as.numeric(sub(".*_", "", comb_df$combination))

# Sort by X first and then by Y
sorted_comb_df <- comb_df[order(comb_df$X, comb_df$Y), ]

# Display the sorted combinations with their frequencies
sorted_combination_freq_lag4 <- sorted_comb_df[, c("combination", "freq")]
sorted_combination_freq_lag4 = data.frame(combination = sorted_combination_freq_lag4$combination, frequency = sorted_combination_freq_lag4$freq)
sorted_combination_freq_lag4 = sorted_combination_freq_lag4[-nrow(sorted_combination_freq_lag4),] # remove NA
print(sorted_combination_freq_lag4)


# Lag 5
# Read data
earthquake = earthquake_train

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake = earthquake[,c(2:5,8:12)]

# Initialize columns for the previous 5 days
earthquake$PrevDay5 <- earthquake$Day
earthquake$PrevMonth5 <- earthquake$Month
earthquake$PrevYear5 <- earthquake$Year

# Function to adjust for the previous 5 days and handle month/year changes
adjust_date_lag5 <- function(year, month, day) {
  # Subtract 5 days
  day <- day - 5
  # Adjust day and month
  if (day <= 0) {
    month <- month - 1
    if (month <= 0) {
      month <- 12
      year <- year - 1
    }
    # Set day to the last day of the previous month
    if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
      day <- 31 + day
    } else if (month %in% c(4, 6, 9, 11)) {
      day <- 30 + day
    } else {
      if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
        day <- 29 + day
      } else {
        day <- 28 + day
      }
    }
  }
  return(c(year, month, day))
}

# Apply the adjustment for each row
adjustments <- t(sapply(1:nrow(earthquake), function(i) {
  adjust_date_lag5(earthquake$Year[i], earthquake$Month[i], earthquake$Day[i])
}))

# Assign adjusted values to the dataframe
earthquake$PrevYear5 <- adjustments[, 1]
earthquake$PrevMonth5 <- adjustments[, 2]
earthquake$PrevDay5 <- adjustments[, 3]

# Merge earthquake with new_df for lag 5
merged_df_lag5 <- merge(
  earthquake, 
  new_df, 
  by.x = c("PrevYear5", "PrevMonth5", "PrevDay5", "Hour"), 
  by.y = c("Year", "Month", "Day", "Hour"),
  all.x = TRUE
)

# Display the merged data frame
print(merged_df_lag5)

# Standardize the merged_df
merged_df = merged_df_lag5[,-(1:4)]

# Step 2: Extract the corresponding t and n columns based on tri_ID
merged_df$t <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("t", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})
merged_df$n <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("n", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})

# Step 3: Select only relevant columns for earthquake
earthquake <- merged_df[, c(1:8, ncol(merged_df)-1, ncol(merged_df))]

# Display the updated earthquake data frame
print(earthquake)

# Calculate the frequency
# Step 1: Create a vector of combinations (t, n)
combinations <- with(earthquake, ifelse(is.na(t) | is.na(n), "NA", paste(t, n, sep = "_")))

# Step 2: Calculate the frequency of each unique combination
combination_freq_lag5 <- table(combinations)

# Sorting
# Convert the names of combination_freq_whole into a data frame with X and Y as numeric columns
comb_df <- data.frame(
  combination = names(combination_freq_lag5),
  freq = as.numeric(combination_freq_lag5),
  stringsAsFactors = FALSE
)

# Split the combination into X and Y parts
comb_df$X <- as.numeric(sub("_.*", "", comb_df$combination))
comb_df$Y <- as.numeric(sub(".*_", "", comb_df$combination))

# Sort by X first and then by Y
sorted_comb_df <- comb_df[order(comb_df$X, comb_df$Y), ]

# Display the sorted combinations with their frequencies
sorted_combination_freq_lag5 <- sorted_comb_df[, c("combination", "freq")]
sorted_combination_freq_lag5 = data.frame(combination = sorted_combination_freq_lag5$combination, frequency = sorted_combination_freq_lag5$freq)
sorted_combination_freq_lag5 = sorted_combination_freq_lag5[-nrow(sorted_combination_freq_lag5),] # remove NA
print(sorted_combination_freq_lag5)

# Lag 6
# Read data
earthquake = earthquake_train

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake = earthquake[,c(2:5,8:12)]

# Initialize columns for the previous 6 days
earthquake$PrevDay6 <- earthquake$Day
earthquake$PrevMonth6 <- earthquake$Month
earthquake$PrevYear6 <- earthquake$Year

# Function to adjust for the previous 6 days and handle month/year changes
adjust_date_lag6 <- function(year, month, day) {
  day <- day - 6
  if (day <= 0) {
    month <- month - 1
    if (month <= 0) {
      month <- 12
      year <- year - 1
    }
    if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
      day <- 31 + day
    } else if (month %in% c(4, 6, 9, 11)) {
      day <- 30 + day
    } else {
      if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
        day <- 29 + day
      } else {
        day <- 28 + day
      }
    }
  }
  return(c(year, month, day))
}

# Apply the adjustment for each row
adjustments <- t(sapply(1:nrow(earthquake), function(i) {
  adjust_date_lag6(earthquake$Year[i], earthquake$Month[i], earthquake$Day[i])
}))

# Assign adjusted values to the dataframe
earthquake$PrevYear6 <- adjustments[, 1]
earthquake$PrevMonth6 <- adjustments[, 2]
earthquake$PrevDay6 <- adjustments[, 3]

# Merge earthquake with new_df for lag 6
merged_df_lag6 <- merge(
  earthquake, 
  new_df, 
  by.x = c("PrevYear6", "PrevMonth6", "PrevDay6", "Hour"), 
  by.y = c("Year", "Month", "Day", "Hour"),
  all.x = TRUE
)

# Display the merged data frame
print(merged_df_lag6)

# Standardize the merged_df
merged_df = merged_df_lag6[,-(1:4)]

# Step 2: Extract the corresponding t and n columns based on tri_ID
merged_df$t <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("t", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})
merged_df$n <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("n", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})

# Step 3: Select only relevant columns for earthquake
earthquake <- merged_df[, c(1:8, ncol(merged_df)-1, ncol(merged_df))]

# Display the updated earthquake data frame
print(earthquake)

# Calculate the frequency
# Step 1: Create a vector of combinations (t, n)
combinations <- with(earthquake, ifelse(is.na(t) | is.na(n), "NA", paste(t, n, sep = "_")))

# Step 2: Calculate the frequency of each unique combination
combination_freq_lag6 <- table(combinations)

# Sorting
# Convert the names of combination_freq_whole into a data frame with X and Y as numeric columns
comb_df <- data.frame(
  combination = names(combination_freq_lag6),
  freq = as.numeric(combination_freq_lag6),
  stringsAsFactors = FALSE
)

# Split the combination into X and Y parts
comb_df$X <- as.numeric(sub("_.*", "", comb_df$combination))
comb_df$Y <- as.numeric(sub(".*_", "", comb_df$combination))

# Sort by X first and then by Y
sorted_comb_df <- comb_df[order(comb_df$X, comb_df$Y), ]

# Display the sorted combinations with their frequencies
sorted_combination_freq_lag6 <- sorted_comb_df[, c("combination", "freq")]
sorted_combination_freq_lag6 = data.frame(combination = sorted_combination_freq_lag6$combination, frequency = sorted_combination_freq_lag6$freq)
sorted_combination_freq_lag6 = sorted_combination_freq_lag6[-nrow(sorted_combination_freq_lag6),] # remove NA
print(sorted_combination_freq_lag6)


# Lag 7
# Read data
earthquake = earthquake_train

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake = earthquake[,c(2:5,8:12)]

# Initialize columns for the previous 7 days
earthquake$PrevDay7 <- earthquake$Day
earthquake$PrevMonth7 <- earthquake$Month
earthquake$PrevYear7 <- earthquake$Year

# Function to adjust for the previous 7 days and handle month/year changes
adjust_date_lag7 <- function(year, month, day) {
  day <- day - 7
  if (day <= 0) {
    month <- month - 1
    if (month <= 0) {
      month <- 12
      year <- year - 1
    }
    if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
      day <- 31 + day
    } else if (month %in% c(4, 6, 9, 11)) {
      day <- 30 + day
    } else {
      if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
        day <- 29 + day
      } else {
        day <- 28 + day
      }
    }
  }
  return(c(year, month, day))
}

# Apply the adjustment for each row
adjustments <- t(sapply(1:nrow(earthquake), function(i) {
  adjust_date_lag7(earthquake$Year[i], earthquake$Month[i], earthquake$Day[i])
}))

# Assign adjusted values to the dataframe
earthquake$PrevYear7 <- adjustments[, 1]
earthquake$PrevMonth7 <- adjustments[, 2]
earthquake$PrevDay7 <- adjustments[, 3]

# Merge earthquake with new_df for lag 7
merged_df_lag7 <- merge(
  earthquake, 
  new_df, 
  by.x = c("PrevYear7", "PrevMonth7", "PrevDay7", "Hour"), 
  by.y = c("Year", "Month", "Day", "Hour"),
  all.x = TRUE
)

# Display the merged data frame
print(merged_df_lag7)

# Standardize the merged_df
merged_df = merged_df_lag7[,-(1:4)]

# Step 2: Extract the corresponding t and n columns based on tri_ID
merged_df$t <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("t", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})
merged_df$n <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("n", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})

# Step 3: Select only relevant columns for earthquake
earthquake <- merged_df[, c(1:8, ncol(merged_df)-1, ncol(merged_df))]

# Display the updated earthquake data frame
print(earthquake)

# Calculate the frequency
# Step 1: Create a vector of combinations (t, n)
combinations <- with(earthquake, ifelse(is.na(t) | is.na(n), "NA", paste(t, n, sep = "_")))

# Step 2: Calculate the frequency of each unique combination
combination_freq_lag7 <- table(combinations)

# Sorting
# Convert the names of combination_freq_whole into a data frame with X and Y as numeric columns
comb_df <- data.frame(
  combination = names(combination_freq_lag7),
  freq = as.numeric(combination_freq_lag7),
  stringsAsFactors = FALSE
)

# Split the combination into X and Y parts
comb_df$X <- as.numeric(sub("_.*", "", comb_df$combination))
comb_df$Y <- as.numeric(sub(".*_", "", comb_df$combination))

# Sort by X first and then by Y
sorted_comb_df <- comb_df[order(comb_df$X, comb_df$Y), ]

# Display the sorted combinations with their frequencies
sorted_combination_freq_lag7 <- sorted_comb_df[, c("combination", "freq")]
sorted_combination_freq_lag7 = data.frame(combination = sorted_combination_freq_lag7$combination, frequency = sorted_combination_freq_lag7$freq)
sorted_combination_freq_lag7 = sorted_combination_freq_lag7[-nrow(sorted_combination_freq_lag7),] # remove NA
print(sorted_combination_freq_lag7)


# Lag 8
# Read data
earthquake = earthquake_train

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake = earthquake[,c(2:5,8:12)]

# Initialize columns for the previous 8 days
earthquake$PrevDay8 <- earthquake$Day
earthquake$PrevMonth8 <- earthquake$Month
earthquake$PrevYear8 <- earthquake$Year

# Function to adjust for the previous 7 days and handle month/year changes
adjust_date_lag8 <- function(year, month, day) {
  day <- day - 8
  if (day <= 0) {
    month <- month - 1
    if (month <= 0) {
      month <- 12
      year <- year - 1
    }
    if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
      day <- 31 + day
    } else if (month %in% c(4, 6, 9, 11)) {
      day <- 30 + day
    } else {
      if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
        day <- 29 + day
      } else {
        day <- 28 + day
      }
    }
  }
  return(c(year, month, day))
}

# Apply the adjustment for each row
adjustments <- t(sapply(1:nrow(earthquake), function(i) {
  adjust_date_lag8(earthquake$Year[i], earthquake$Month[i], earthquake$Day[i])
}))

# Assign adjusted values to the dataframe
earthquake$PrevYear8 <- adjustments[, 1]
earthquake$PrevMonth8 <- adjustments[, 2]
earthquake$PrevDay8 <- adjustments[, 3]

# Merge earthquake with new_df for lag 8
merged_df_lag8 <- merge(
  earthquake, 
  new_df, 
  by.x = c("PrevYear8", "PrevMonth8", "PrevDay8", "Hour"), 
  by.y = c("Year", "Month", "Day", "Hour"),
  all.x = TRUE
)

# Display the merged data frame
print(merged_df_lag8)

# Standardize the merged_df
merged_df = merged_df_lag8[,-(1:4)]

# Step 2: Extract the corresponding t and n columns based on tri_ID
merged_df$t <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("t", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})
merged_df$n <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("n", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})

# Step 3: Select only relevant columns for earthquake
earthquake <- merged_df[, c(1:8, ncol(merged_df)-1, ncol(merged_df))]

# Display the updated earthquake data frame
print(earthquake)

# Calculate the frequency
# Step 1: Create a vector of combinations (t, n)
combinations <- with(earthquake, ifelse(is.na(t) | is.na(n), "NA", paste(t, n, sep = "_")))

# Step 2: Calculate the frequency of each unique combination
combination_freq_lag8 <- table(combinations)

# Sorting
# Convert the names of combination_freq_whole into a data frame with X and Y as numeric columns
comb_df <- data.frame(
  combination = names(combination_freq_lag8),
  freq = as.numeric(combination_freq_lag8),
  stringsAsFactors = FALSE
)

# Split the combination into X and Y parts
comb_df$X <- as.numeric(sub("_.*", "", comb_df$combination))
comb_df$Y <- as.numeric(sub(".*_", "", comb_df$combination))

# Sort by X first and then by Y
sorted_comb_df <- comb_df[order(comb_df$X, comb_df$Y), ]

# Display the sorted combinations with their frequencies
sorted_combination_freq_lag8 <- sorted_comb_df[, c("combination", "freq")]
sorted_combination_freq_lag8 = data.frame(combination = sorted_combination_freq_lag8$combination, frequency = sorted_combination_freq_lag8$freq)
sorted_combination_freq_lag8 = sorted_combination_freq_lag8[-nrow(sorted_combination_freq_lag8),] # remove NA
print(sorted_combination_freq_lag8)


# Lag 9
# Read data
earthquake = earthquake_train

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake = earthquake[,c(2:5,8:12)]

# Initialize columns for the previous 9 days
earthquake$PrevDay9 <- earthquake$Day
earthquake$PrevMonth9 <- earthquake$Month
earthquake$PrevYear9 <- earthquake$Year

# Function to adjust for the previous 9 days and handle month/year changes
adjust_date_lag9 <- function(year, month, day) {
  day <- day - 9
  if (day <= 0) {
    month <- month - 1
    if (month <= 0) {
      month <- 12
      year <- year - 1
    }
    if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
      day <- 31 + day
    } else if (month %in% c(4, 6, 9, 11)) {
      day <- 30 + day
    } else {
      if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
        day <- 29 + day
      } else {
        day <- 28 + day
      }
    }
  }
  return(c(year, month, day))
}

# Apply the adjustment for each row
adjustments <- t(sapply(1:nrow(earthquake), function(i) {
  adjust_date_lag9(earthquake$Year[i], earthquake$Month[i], earthquake$Day[i])
}))

# Assign adjusted values to the dataframe
earthquake$PrevYear9 <- adjustments[, 1]
earthquake$PrevMonth9 <- adjustments[, 2]
earthquake$PrevDay9 <- adjustments[, 3]

# Merge earthquake with new_df for lag 7
merged_df_lag9 <- merge(
  earthquake, 
  new_df, 
  by.x = c("PrevYear9", "PrevMonth9", "PrevDay9", "Hour"), 
  by.y = c("Year", "Month", "Day", "Hour"),
  all.x = TRUE
)

# Display the merged data frame
print(merged_df_lag9)

# Standardize the merged_df
merged_df = merged_df_lag9[,-(1:4)]

# Step 2: Extract the corresponding t and n columns based on tri_ID
merged_df$t <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("t", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})
merged_df$n <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("n", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})

# Step 3: Select only relevant columns for earthquake
earthquake <- merged_df[, c(1:8, ncol(merged_df)-1, ncol(merged_df))]

# Display the updated earthquake data frame
print(earthquake)

# Calculate the frequency
# Step 1: Create a vector of combinations (t, n)
combinations <- with(earthquake, ifelse(is.na(t) | is.na(n), "NA", paste(t, n, sep = "_")))

# Step 2: Calculate the frequency of each unique combination
combination_freq_lag9 <- table(combinations)

# Sorting
# Convert the names of combination_freq_whole into a data frame with X and Y as numeric columns
comb_df <- data.frame(
  combination = names(combination_freq_lag9),
  freq = as.numeric(combination_freq_lag9),
  stringsAsFactors = FALSE
)

# Split the combination into X and Y parts
comb_df$X <- as.numeric(sub("_.*", "", comb_df$combination))
comb_df$Y <- as.numeric(sub(".*_", "", comb_df$combination))

# Sort by X first and then by Y
sorted_comb_df <- comb_df[order(comb_df$X, comb_df$Y), ]

# Display the sorted combinations with their frequencies
sorted_combination_freq_lag9 <- sorted_comb_df[, c("combination", "freq")]
sorted_combination_freq_lag9 = data.frame(combination = sorted_combination_freq_lag9$combination, frequency = sorted_combination_freq_lag9$freq)
sorted_combination_freq_lag9 = sorted_combination_freq_lag9[-nrow(sorted_combination_freq_lag9),] # remove NA
print(sorted_combination_freq_lag9)

#### Dataset: calculate occurrence for modeling ####

# Function to calculate probabilities
calculate_probabilities <- function(union_df, whole_df) {
  prob_df <- whole_df %>%
    left_join(union_df, by = "combination") %>%
    mutate(probability = coalesce(frequency.y, 0) / frequency.x) %>%
    mutate(occur = ifelse(probability == 0, 0, 1)) %>%
    dplyr::select(combination, occur)
  return(prob_df)
}

# calculate occur_df for lag 1 to lag 9
# The paper only presents lag 1
for (i in 1:9) {
  lag_name <- paste0("lag", i)
  lag_df <- get(paste0("sorted_combination_freq_lag", i))  # Retrieve the corresponding lag data frame
  assign(lag_name, calculate_probabilities(lag_df, sorted_combination_freq_whole))
  df <- get(lag_name)
  df <- df[-nrow(df),]
  df <- df %>% separate(combination, into = c("center", "neighbor"), sep = "_")
  assign(lag_name, df)
}

combined_df <- bind_cols(
  lapply(1:9, function(i) {
    df <- get(paste0("lag", i))  # Retrieve lag1 to lag9
    colnames(df) <- paste0(colnames(df), i)  # Rename columns
    df
  })
)

write_csv(combined_df, "Dataset.csv")

# Visualization of Dataset
dat = read.csv("Dataset.csv")

# Only for lag 1
dat = dat[,1:3]

# Facet plot with larger size
ggplot(dat, aes(x = center1, y = neighbor1, color = factor(occur1))) +
  geom_point(data = filter(dat, occur1 == 0), alpha = 0.2) +  # Plot blue points first
  geom_point(data = filter(dat, occur1 == 1), alpha = 0.4) +  # Plot red points on top
  scale_color_manual(values = c("blue", "red"), labels = c("No", "Yes")) +  
  #scale_x_continuous(limits = c(50, 175)) +
  #scale_y_continuous(limits = c(50, 175)) +
  labs(x = "Central Groundwater Level",
       y = "Neighboring Groundwater Level",
       color = "Earthquake") +
  theme_minimal(base_size = 20)


#### Second Layer - 1: Hyperparameters tuning ####
# train for x = 1, 2, ..., 9
x = 1
dat = read.csv("Dataset.csv")
dat = dat[,(3*x-2):(3*x)]
center_col = paste0("center", x)
neighbor_col = paste0("neighbor", x)
occur_col = paste0("occur", x)
freq_table = table(dat[,occur_col])
prop_table = prop.table(freq_table)

loop = function() {
  # We do not tune for Random Forest
  FNR1 = MCC1 = BA1 = matrix(0, 1, 30) # Logistic
  FNR2 = MCC2 = BA2 = matrix(0, 1, 30) # LDA
  FNR3 = MCC3 = BA3 = matrix(0, 1, 30) # QDA
  FNR4 = MCC4 = BA4 = matrix(0, 1, 30) # KNN
  FNR5 = MCC5 = BA5 = matrix(0, 1, 30) # SVM
  FNR6 = MCC6 = BA6 = matrix(0, 1, 30) # XGBoost
  FPR1 = FPR2 = FPR3 = FPR4 = FPR5 = FPR6 = matrix(0, 1, 30) # False Positive Rate
  
  # SPlit the data
  first.idx = SPlit::SPlit(dat, splitRatio = 0.2)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit::SPlit(first, splitRatio = 0.25)
  train = first[-valid.idx,]
  validation = first[valid.idx,]
  # over-sampling using SMOTE on training data only
  genData = smotefamily::SMOTE(train[,c(center_col, neighbor_col)], train[,c(occur_col)])
  train.balanced = genData$data
  train.balanced[,occur_col] = as.factor(train.balanced$class)
  train = train.balanced[,c(center_col, neighbor_col, occur_col)]
  validation = validation[,c(center_col, neighbor_col, occur_col)]
  
  # Logistic regresssion (1)
  threshold = seq(0, 0.5, length.out = 30)
  for (j in 1:30) {
    fit.logit = glm(as.formula(paste(occur_col, "~", center_col, "+", neighbor_col)), data = train, family = binomial)
    prob.logit = predict(fit.logit, validation, type = "response")
    pred.logit = rep(0, length(prob.logit))
    pred.logit[prob.logit > threshold[j]] = 1
    # Evaluation metrics
    pred = pred.logit
    conf_matrix = table(pred, validation[,occur_col])
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else { 0 }
    FNR1[1,j] = false_negatives / sum(validation[,occur_col] == 1)
    # MCC
    MCC1[1,j] = mltools::mcc(preds = pred, actuals = validation[,occur_col])
    # Balanced accuracy
    d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
    d$estimate = factor(d$estimate, levels = c(0,1))
    balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
    BA1[1,j] = balanced_accuracy$.estimate
    # False positive rate
    false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
      conf_matrix["1", "0"]
    } else {0}
    FPR1[1,j] = false_positives / sum(validation[,occur_col] == 0)
  }
  
  # LDA (2)
  threshold = seq(0, 0.5, length.out = 30)
  for (j in 1:30) {
    fit.lda = MASS::lda(as.formula(paste(occur_col, "~", center_col, "+", neighbor_col)), data = train)
    out.lda = predict(fit.lda, validation)
    prob.lda = out.lda$posterior[,"1"]
    pred.lda = rep(0, length(prob.lda))
    pred.lda[prob.lda > threshold[j]] = 1
    # Evaluation metrics
    pred = pred.lda
    conf_matrix = table(pred, validation[,occur_col])
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else { 0 }
    FNR2[1,j] = false_negatives / sum(validation[,occur_col] == 1)
    # MCC
    MCC2[1,j] = mltools::mcc(preds = pred, actuals = validation[,occur_col])
    # Balanced accuracy
    d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
    d$estimate = factor(d$estimate, levels = c(0,1))
    balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
    BA2[1,j] = balanced_accuracy$.estimate
    # False positive rate
    false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
      conf_matrix["1", "0"]
    } else {0}
    FPR2[1,j] = false_positives / sum(validation[,occur_col] == 0)
  }
  
  # QDA (3)
  threshold = seq(0, 0.5, length.out = 30)
  for (j in 1:30) {
    fit.qda = MASS::qda(as.formula(paste(occur_col, "~", center_col, "+", neighbor_col)), data = train)
    out.qda = predict(fit.qda, validation)
    prob.qda = out.qda$posterior[,"1"]
    pred.qda = rep(0, length(prob.qda))
    pred.qda[prob.qda > threshold[j]] = 1
    # Evaluation metrics
    pred = pred.qda
    conf_matrix = table(pred, validation[,occur_col])
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else { 0 }
    FNR3[1,j] = false_negatives / sum(validation[,occur_col] == 1)
    # MCC
    MCC3[1,j] = mltools::mcc(preds = pred, actuals = validation[,occur_col])
    # Balanced accuracy
    d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
    d$estimate = factor(d$estimate, levels = c(0,1))
    balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
    BA3[1,j] = balanced_accuracy$.estimate
    # False positive rate
    false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
      conf_matrix["1", "0"]
    } else {0}
    FPR3[1,j] = false_positives / sum(validation[,occur_col] == 0)
  }
  
  # KNN (4)
  K = 30
  train.X = train[,c(center_col, neighbor_col)]
  train.Y = train[,occur_col]
  validation.X = validation[,c(center_col, neighbor_col)]
  validation.Y = validation[,occur_col]
  test.X = test[,c(center_col, neighbor_col)]
  test.Y = test[,occur_col]
  for (j in 1:K) {
    pred.knn = class::knn(train.X, validation.X, train.Y, k = j)
    # Evaluation metrics
    pred = pred.knn
    conf_matrix = table(pred, validation[,occur_col])
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else { 0 }
    FNR4[1,j] = false_negatives / sum(validation[,occur_col] == 1)
    # MCC
    MCC4[1,j] = mltools::mcc(preds = pred, actuals = as.factor(validation[,occur_col]))
    # Balanced accuracy
    d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
    d$estimate = factor(d$estimate, levels = c(0,1))
    balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
    BA4[1,j] = balanced_accuracy$.estimate
    # False positive rate
    false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
      conf_matrix["1", "0"]
    } else {0}
    FPR4[1,j] = false_positives / sum(validation[,occur_col] == 0)
  }
  
  # SVM (5)
  gamma_cand = c(0.5 ,1 ,2 ,3 ,4)
  cost_cand = c(0.1 ,1 ,10 ,100, 500 ,1000)
  k = 1
  for (i in 1:5) for (j in 1:6) {
    fit.svm = e1071::svm(as.formula(paste(occur_col, "~", center_col, "+", neighbor_col)),
                         data = train, kernel = "radial", gamma = gamma_cand[i], cost = cost_cand[j])
    pred.svm = predict(fit.svm, newdata = validation)
    # Evaluation metrics
    pred = pred.svm
    conf_matrix = table(pred, validation[,occur_col])
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else { 0 }
    FNR5[1,k] = false_negatives / sum(validation[,occur_col] == 1)
    # MCC
    MCC5[1,k] = mltools::mcc(preds = pred, actuals = as.factor(validation[,occur_col]))
    # Balanced accuracy
    d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
    d$estimate = factor(d$estimate, levels = c(0,1))
    balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
    BA5[1,k] = balanced_accuracy$.estimate
    # False positive rate
    false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
      conf_matrix["1", "0"]
    } else {0}
    FPR5[1,k] = false_positives / sum(validation[,occur_col] == 0)
    k = k + 1
  }
  
  # XGBoost (6)
  eta_cand = c(0.005, 0.01, 0.02)
  max_depth_cand = c(4, 7, 10)
  threshold_cand = c(0.2, 0.3, 0.4)
  X_train = data.matrix(train[,1:2])
  y_train = train[,occur_col]
  X_test = data.matrix(validation[,1:2])
  y_test = validation[,occur_col]
  # Convert factor to numeric, ensuring 0 and 1 values are preserved
  y_train = as.numeric(as.character(y_train))
  y_test = as.numeric(as.character(y_test))
  # convert the train and test data into xgboost matrix type.
  xgboost_train = xgboost::xgb.DMatrix(data=X_train, label=y_train)
  xgboost_test = xgboost::xgb.DMatrix(data=X_test, label=y_test)
  par <- list(
    objective = "binary:logistic", # Outputs probabilities for binary classification
    max_depth = 5,
    eta = 0.1
  )
  k = 1
  for (l in 1:3)
    for (i in 1:3)
      for (j in 1:3) {
        par$eta = eta_cand[l]
        par$max_depth = max_depth_cand[i]
        fit.xgb = xgboost::xgb.train(data = xgboost_train, 
                                     params = par,
                                     nrounds = 5000)
        pred = predict(fit.xgb, xgboost_test)
        pred.xgb = as.numeric(pred > threshold_cand[j])
        # Evaluation metrics
        pred = pred.xgb
        conf_matrix = table(pred, validation[,occur_col])
        # False negative rate
        false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
          conf_matrix["0","1"]
        } else { 0 }
        FNR6[1,k] = false_negatives / sum(validation[,occur_col] == 1)
        # MCC
        MCC6[1,k] = mltools::mcc(preds = pred, actuals = validation[,occur_col])
        # Balanced accuracy
        d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
        d$estimate = factor(d$estimate, levels = c(0,1))
        balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
        BA6[1,k] = balanced_accuracy$.estimate
        # False positive rate
        false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
          conf_matrix["1", "0"]
        } else {0}
        FPR6[1,k] = false_positives / sum(validation[,occur_col] == 0)
        k = k + 1
      }
  
  result = list(FNR1 = FNR1, FNR2 = FNR2, FNR3 = FNR3, FNR4 = FNR4, FNR5 = FNR5, FNR6 = FNR6,
                MCC1 = MCC1, MCC2 = MCC2, MCC3 = MCC3, MCC4 = MCC4, MCC5 = MCC5, MCC6 = MCC6,
                BA1 = BA1, BA2 = BA2, BA3 = BA3, BA4 = BA4, BA5 = BA5, BA6 = BA6,
                FPR1 = FPR1, FPR2 = FPR2, FPR3 = FPR3, FPR4 = FPR4, FPR5 = FPR5, FPR6 = FPR6)
  combined_df <- do.call(cbind, result)
  return(combined_df)
}

set.seed(5)
N = 50
num_core = detectCores() # 8 cores
cl = makeCluster(num_core)
registerDoSNOW(cl)
RES = foreach(j = 1:N,.combine = "rbind") %dopar% loop()
stopCluster(cl)

FNR_1 = colMeans(RES[,1:30]); MCC_1 = colMeans(RES[,181:210]); BA_1 = colMeans(RES[,361:390]); FPR_1 = colMeans(RES[,541:570])
FNR_2 = colMeans(RES[,31:60]); MCC_2 = colMeans(RES[,211:240]); BA_2 = colMeans(RES[,391:420]); FPR_2 = colMeans(RES[,571:600])
FNR_3 = colMeans(RES[,61:90]); MCC_3 = colMeans(RES[,241:270]); BA_3 = colMeans(RES[,421:450]); FPR_3 = colMeans(RES[,601:630])
FNR_4 = colMeans(RES[,91:120]); MCC_4 = colMeans(RES[,271:300]); BA_4 = colMeans(RES[,451:480]); FPR_4 = colMeans(RES[,631:660])
FNR_5 = colMeans(RES[,121:150]); MCC_5 = colMeans(RES[,301:330]); BA_5 = colMeans(RES[,481:510]); FPR_5 = colMeans(RES[,661:690])
FNR_6 = colMeans(RES[,151:177]); MCC_6 = colMeans(RES[,331:357]); BA_6 = colMeans(RES[,511:537]); FPR_6 = colMeans(RES[,691:717])

# Combine data
data <- bind_rows(
  data.frame(Method = "Logistic Regression", Threshold = 1:length(FNR_1), 
             Rate = c(FNR_1, MCC_1, BA_1, FPR_1), 
             Type = rep(c("False Negative Rate", "MCC", "Balanced Accuracy", "False Positive Rate"), each = length(FNR_1))),
  data.frame(Method = "LDA", Threshold = 1:length(FNR_2), 
             Rate = c(FNR_2, MCC_2, BA_2, FPR_2), 
             Type = rep(c("False Negative Rate", "MCC", "Balanced Accuracy", "False Positive Rate"), each = length(FNR_2))),
  data.frame(Method = "QDA", Threshold = 1:length(FNR_3), 
             Rate = c(FNR_3, MCC_3, BA_3, FPR_3), 
             Type = rep(c("False Negative Rate", "MCC", "Balanced Accuracy", "False Positive Rate"), each = length(FNR_3))),
  data.frame(Method = "KNN", Threshold = 1:length(FNR_4), 
             Rate = c(FNR_4, MCC_4, BA_4, FPR_4), 
             Type = rep(c("False Negative Rate", "MCC", "Balanced Accuracy", "False Positive Rate"), each = length(FNR_4))),
  data.frame(Method = "SVM", Threshold = 1:length(FNR_5), 
             Rate = c(FNR_5, MCC_5, BA_5, FPR_5), 
             Type = rep(c("False Negative Rate", "MCC", "Balanced Accuracy", "False Positive Rate"), each = length(FNR_5))),
  data.frame(Method = "XGBoost", Threshold = 1:length(FNR_6),
             Rate = c(FNR_6, MCC_6, BA_6, FPR_6), 
             Type = rep(c("False Negative Rate", "MCC", "Balanced Accuracy", "False Positive Rate"), each = length(FNR_6)))
)

vertical_lines <- data.frame(
  Method = c("Logistic Regression", "LDA", "QDA", "KNN", "SVM", "XGBoost"),
  Vertical_Line = c(27, 27, 30, 9, 11, 1)
)

ggplot(data, aes(x = Threshold, y = Rate, color = Type)) + 
  geom_line(size = 0.8) +   
  geom_point(aes(shape = Type), size = 2, stroke = 1) +  # Add points with different shapes based on Type
  # Add vertical lines   
  geom_vline(data = vertical_lines, aes(xintercept = Vertical_Line), 
             linetype = "longdash", color = "purple", linewidth = 1) +   
  # Facet by method   
  facet_wrap(~ Method, scales = "free_x", ncol = 6) +   
  # Adjust colors, linetypes, and shapes   
  scale_color_manual(values = c("MCC" = "deepskyblue", 
                                "False Negative Rate" = "coral1",
                                "Balanced Accuracy" = "green3",
                                "False Positive Rate" = "bisque4"), 
                     name = "") +
  scale_shape_manual(values = c("MCC" = 0,
                                "False Negative Rate" = 4,
                                "Balanced Accuracy" = 2,
                                "False Positive Rate" = 1)) +
  # scale_y_continuous(limits = c(0,1))+
  # Labels and themes   
  labs(x = "Threshold / Hyper-parameter", 
       y = "Rate (mean over 50 SPlit subsampling splits)", 
       color = "Metric", 
       shape = "Metric") +   
  theme_minimal(base_size = 20) +
  theme(text = element_text(family = "Helvetica"), 
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.5, "cm")) +   
  # Improved legend settings   
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(shape = c(2, 4, 1, 0))),
         shape = "none") +  # Merge shape into color legend
  theme(legend.position = "top", legend.box = "horizontal")



#### Second Layer - 2: Modeling on [train + validation] ####
x = 1
dat = read.csv("Dataset.csv")
dat = dat[,(3*x-2):(3*x)]
center_col = paste0("center", x)
neighbor_col = paste0("neighbor", x)
occur_col = paste0("occur", x)

loop = function() {
  FNR = MCC = BA = FPR = matrix(0, 1, 8) # 8 indicates the number of model (including baseline)
  
  # SPlit the data
  first.idx = SPlit::SPlit(dat, splitRatio = 0.2)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit::SPlit(first, splitRatio = 0.25)
  # over-sampling using SMOTE on training data only
  genData = smotefamily::SMOTE(first[,c(center_col, neighbor_col)], first[,c(occur_col)])
  first.balanced = genData$data
  first.balanced[,occur_col] = as.factor(first.balanced$class)
  train_valid = first.balanced[,c(center_col, neighbor_col, occur_col)]
  # Rename
  train = train_valid
  validation = test
  
  # Naive method - Always predict occur to 1 (1)
  pred.naive = rep(1, nrow(test))
  pred = pred.naive
  # False negative rate
  FNR[1,1] = 0
  # MCC
  MCC[1,1] = mltools::mcc(preds = pred, actuals = test[,occur_col])
  # Balanced accuracy
  d = data.frame(truth = as.factor(test[,occur_col]), estimate = as.factor(pred))
  d$estimate = factor(d$estimate, levels = c(0,1))
  balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
  BA[1,1] = balanced_accuracy$.estimate
  # False positive rate
  FPR[1,1] = 1
  
  # Logistic regresssion (2)
  threshold = seq(0, 0.5, length.out = 30)
  j = 27
  fit.logit = glm(as.formula(paste(occur_col, "~", center_col, "+", neighbor_col)), 
                  data = train, family = binomial)
  prob.logit = predict(fit.logit, validation, type = "response")
  pred.logit = rep(0, length(prob.logit))
  pred.logit[prob.logit > threshold[j]] = 1
  # Evaluation metrics
  pred = pred.logit
  conf_matrix = table(pred, validation[,occur_col])
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else { 0 }
  FNR[1,2] = false_negatives / sum(validation[,occur_col] == 1)
  # MCC
  MCC[1,2] = mltools::mcc(preds = pred, actuals = validation[,occur_col])
  # Balanced accuracy
  d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
  d$estimate = factor(d$estimate, levels = c(0,1))
  balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
  BA[1,2] = balanced_accuracy$.estimate
  # False positive rate
  false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1", "0"]
  } else {0}
  FPR[1,2] = false_positives / sum(validation[,occur_col] == 0)
  
  # LDA (3)
  threshold = seq(0, 0.5, length.out = 30)
  j = 27
  fit.lda = MASS::lda(as.formula(paste(occur_col, "~", center_col, "+", neighbor_col)), data = train)
  out.lda = predict(fit.lda, validation)
  prob.lda = out.lda$posterior[,"1"]
  pred.lda = rep(0, length(prob.lda))
  pred.lda[prob.lda > threshold[j]] = 1
  # Evaluation metrics
  pred = pred.lda
  conf_matrix = table(pred, validation[,occur_col])
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else { 0 }
  FNR[1,3] = false_negatives / sum(validation[,occur_col] == 1)
  # MCC
  MCC[1,3] = mltools::mcc(preds = pred, actuals = validation[,occur_col])
  # Balanced accuracy
  d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
  d$estimate = factor(d$estimate, levels = c(0,1))
  balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
  BA[1,3] = balanced_accuracy$.estimate
  # False positive rate
  false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1", "0"]
  } else {0}
  FPR[1,3] = false_positives / sum(validation[,occur_col] == 0)
  
  # QDA (4)
  threshold = seq(0, 0.5, length.out = 30)
  j = 30
  fit.qda = MASS::qda(as.formula(paste(occur_col, "~", center_col, "+", neighbor_col)), data = train)
  out.qda = predict(fit.qda, validation)
  prob.qda = out.qda$posterior[,"1"]
  pred.qda = rep(0, length(prob.qda))
  pred.qda[prob.qda > threshold[j]] = 1
  # Evaluation metrics
  pred = pred.qda
  conf_matrix = table(pred, validation[,occur_col])
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else { 0 }
  FNR[1,4] = false_negatives / sum(validation[,occur_col] == 1)
  # MCC
  MCC[1,4] = mltools::mcc(preds = pred, actuals = validation[,occur_col])
  # Balanced accuracy
  d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
  d$estimate = factor(d$estimate, levels = c(0,1))
  balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
  BA[1,4] = balanced_accuracy$.estimate
  # False positive rate
  false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1", "0"]
  } else {0}
  FPR[1,4] = false_positives / sum(validation[,occur_col] == 0)
  
  # KNN (5)
  j = 9
  train.X = train[,c(center_col, neighbor_col)]
  train.Y = train[,occur_col]
  validation.X = validation[,c(center_col, neighbor_col)]
  validation.Y = validation[,occur_col]
  test.X = test[,c(center_col, neighbor_col)]
  test.Y = test[,occur_col]
  pred.knn = class::knn(train.X, validation.X, train.Y, k = j)
  # Evaluation metrics
  pred = pred.knn
  conf_matrix = table(pred, validation[,occur_col])
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else { 0 }
  FNR[1,5] = false_negatives / sum(validation[,occur_col] == 1)
  # MCC
  MCC[1,5] = mltools::mcc(preds = pred, actuals = as.factor(validation[,occur_col]))
  # Balanced accuracy
  d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
  d$estimate = factor(d$estimate, levels = c(0,1))
  balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
  BA[1,5] = balanced_accuracy$.estimate
  # False positive rate
  false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1", "0"]
  } else {0}
  FPR[1,5] = false_positives / sum(validation[,occur_col] == 0)
  
  # SVM (6)
  gamma_cand = c(0.5 ,1 ,2 ,3 ,4)
  cost_cand = c(0.1 ,1 ,10 ,100, 500 ,1000)
  i = 2; j = 5
  fit.svm = e1071::svm(as.formula(paste(occur_col, "~", center_col, "+", neighbor_col)),
                       data = train, kernel = "radial", gamma = gamma_cand[i], cost = cost_cand[j])
  pred.svm = predict(fit.svm, newdata = validation)
  # Evaluation metrics
  pred = pred.svm
  conf_matrix = table(pred, validation[,occur_col])
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else { 0 }
  FNR[1,6] = false_negatives / sum(validation[,occur_col] == 1)
  # MCC
  MCC[1,6] = mltools::mcc(preds = pred, actuals = as.factor(validation[,occur_col]))
  # Balanced accuracy
  d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
  d$estimate = factor(d$estimate, levels = c(0,1))
  balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
  BA[1,6] = balanced_accuracy$.estimate
  # False positive rate
  false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1", "0"]
  } else {0}
  FPR[1,6] = false_positives / sum(validation[,occur_col] == 0)
  
  # XGBoost (7)
  eta_cand = c(0.005, 0.01, 0.02)
  max_depth_cand = c(4, 7, 10)
  threshold_cand = c(0.2, 0.3, 0.4)
  l = 1; i = 1; j = 1
  X_train = data.matrix(train[,1:2])
  y_train = train[,occur_col]
  X_test = data.matrix(validation[,1:2])
  y_test = validation[,occur_col]
  # Convert factor to numeric, ensuring 0 and 1 values are preserved
  y_train = as.numeric(as.character(y_train))
  y_test = as.numeric(as.character(y_test))
  # convert the train and test data into xgboost matrix type.
  xgboost_train = xgboost::xgb.DMatrix(data=X_train, label=y_train)
  xgboost_test = xgboost::xgb.DMatrix(data=X_test, label=y_test)
  par <- list(
    objective = "binary:logistic", # Outputs probabilities for binary classification
    max_depth = 5,
    eta = 0.1
  )
  par$eta = eta_cand[l]
  par$max_depth = max_depth_cand[i]
  fit.xgb = xgboost::xgb.train(data = xgboost_train, 
                               params = par,
                               nrounds = 5000)
  pred = predict(fit.xgb, xgboost_test)
  pred.xgb = as.numeric(pred > threshold_cand[j])
  # Evaluation metrics
  pred = pred.xgb
  conf_matrix = table(pred, validation[,occur_col])
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else { 0 }
  FNR[1,7] = false_negatives / sum(validation[,occur_col] == 1)
  # MCC
  MCC[1,7] = mltools::mcc(preds = pred, actuals = validation[,occur_col])
  # Balanced accuracy
  d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
  d$estimate = factor(d$estimate, levels = c(0,1))
  balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
  BA[1,7] = balanced_accuracy$.estimate
  # False positive rate
  false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1", "0"]
  } else {0}
  FPR[1,7] = false_positives / sum(validation[,occur_col] == 0)
  
  # Random Forest (8)
  fit.rf = randomForest::randomForest(as.formula(paste(occur_col, "~", center_col, "+", neighbor_col)),
                                      data = train, ntree = 5000)
  pred.rf = predict(fit.rf, validation)
  # Evaluation metrics
  pred = pred.rf
  conf_matrix = table(pred, validation[,occur_col])
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else { 0 }
  FNR[1,8] = false_negatives / sum(validation[,occur_col] == 1)
  # MCC
  MCC[1,8] = mltools::mcc(preds = pred, actuals = as.factor(validation[,occur_col]))
  # Balanced accuracy
  d = data.frame(truth = as.factor(validation[,occur_col]), estimate = as.factor(pred))
  d$estimate = factor(d$estimate, levels = c(0,1))
  balanced_accuracy = yardstick::bal_accuracy(d, truth, estimate)
  BA[1,8] = balanced_accuracy$.estimate
  # False positive rate
  false_positives = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1", "0"]
  } else {0}
  FPR[1,8] = false_positives / sum(validation[,occur_col] == 0)
  
  
  result = list(FNR = FNR, MCC = MCC, BA = BA, FPR = FPR)
  combined_df <- do.call(cbind, result)
  return(combined_df)
}

set.seed(5)
N = 50
num_core = detectCores() # 8 cores
cl = makeCluster(num_core)
registerDoSNOW(cl)
RES = foreach(j = 1:N,.combine = "rbind") %dopar% loop()
stopCluster(cl)

Naive_FNR = mean(RES[,1])
Naive_MCC = mean(RES[,9])
Naive_BA = mean(RES[,17])
Naive_FPR = mean(RES[,25])

# Combine the matrices into a data frame
df <- data.frame(
  Logistic = RES[, 2], LDA = RES[, 3], QDA = RES[, 4], KNN = RES[, 5], SVM = RES[, 6], XGB = RES[, 7], RF = RES[,8]
)
df$Metric <- "False negative rate"

df_pro <- data.frame(
  Logistic = RES[, 10], LDA = RES[, 11], QDA = RES[, 12], KNN = RES[, 13], SVM = RES[, 14], XGB = RES[, 15], RF = RES[,16]
)
df_pro$Metric <- "MCC"

df_ba <- data.frame(
  Logistic = RES[, 18], LDA = RES[, 19], QDA = RES[, 20], KNN = RES[, 21], SVM = RES[, 22], XGB = RES[, 23], RF = RES[, 24]
)
df_ba$Metric <- "Balanced accuracy"

df_fpr <- data.frame(
  Logistic = RES[, 26], LDA = RES[, 27], QDA = RES[, 28], KNN = RES[, 29], SVM = RES[, 30], XGB = RES[, 31], RF = RES[, 32]
)
df_fpr$Metric <- "False positive rate"

# Combine all data frames into one
df_combined <- bind_rows(df, df_pro, df_ba, df_fpr)

# Reshape data to long format
df_long <- df_combined %>%
  pivot_longer(cols = -Metric, names_to = "Model", values_to = "Value")

# Calculate mean for each Metric and Model
means <- df_long %>%
  group_by(Metric, Model) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")

naive_values <- data.frame(
  Metric = c("False negative rate", "MCC", "Balanced accuracy", "False positive rate"), 
  Value = c(Naive_FNR, Naive_MCC, Naive_BA, Naive_FPR)
)

# Create the violin plots
ggplot(df_long, aes(x = Model, y = Value)) +
  geom_boxplot(width = 0.4, 
               color = "green4",
               size = 0.8) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "azure4") +
  geom_point(data = means, aes(x = Model, y = Mean, color = "Mean"),
             size = 2, shape = 1, stroke = 1.5) +
  geom_text(
    data = means,
    aes(x = Model, y = Mean, label = round(Mean, 2)), # Round mean values to 2 decimal places
    color = "deeppink2", size = 5, vjust = -4, 
    hjust = 0.5, fontface = "bold"
  ) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 4) +
  labs(y = "Rate (mean over 50 SPlit subsampling splits)") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 20),
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    legend.key.size = unit(1.5, "cm")
  ) +
  # Add the horizontal lines for Naive model values with color and label for legend
  geom_hline(data = data.frame(Metric = c("False negative rate", "MCC", "Balanced accuracy", "False positive rate"),
                               Value = c(Naive_FNR, Naive_MCC, Naive_BA, Naive_FPR)),
             aes(yintercept = Value, color = "Baseline Performance"), linetype = "dashed", size = 1) +
  geom_text(
    data = naive_values,
    aes(x = 1.5, y = Value, label = round(Value, 2)),
    color = "darkorchid3", size = 4, fontface = "bold", hjust = 2, vjust = -1, na.rm = T
  ) +
  scale_color_manual(
    values = c("Mean" = "deeppink2", "Baseline Performance" = "darkorchid3"),
    name = ""
  ) +
  guides(fill = "none")


#### Error rate on future data for earthquake data points ####
dat = read.csv("Dataset.csv")
earthquake = read.csv("Earthquake_data.csv")

# IMPORTANT: Adjust the time range to match your training/testing window!
# 2020/4/1 ~ 2020/9/30
earthquake_test = earthquake[(earthquake$Year == 2020 & ((earthquake$Month > 4 & earthquake$Month < 9) | (earthquake$Month == 4 & earthquake$Day >= 1) | (earthquake$Month == 9 & earthquake$Day <= 30))),]
# 2020/10/1 ~ 2021/3/31
earthquake_test = earthquake[(earthquake$Year == 2020 & earthquake$Month >= 10) | (earthquake$Year == 2021 & earthquake$Month <= 3 & earthquake$Day <= 31),]
# 2021/4/1 ~ 2021/9/30
earthquake_test = earthquake[(earthquake$Year == 2021 & ((earthquake$Month > 4 & earthquake$Month < 9) | (earthquake$Month == 4 & earthquake$Day >= 1) | (earthquake$Month == 9 & earthquake$Day <= 30))),]
# 2021/10/1 ~ 2022/3/31
earthquake_test = earthquake[(earthquake$Year == 2021 & earthquake$Month >= 10) | (earthquake$Year == 2022 & earthquake$Month <= 3 & earthquake$Day <= 31),]
# 2022/4/1 ~ 2022/9/30
earthquake_test = earthquake[(earthquake$Year == 2022 & ((earthquake$Month > 4 & earthquake$Month < 9) | (earthquake$Month == 4 & earthquake$Day >= 1) | (earthquake$Month == 9 & earthquake$Day <= 30))),]
# 2022/10/1 ~ 2023/3/31
earthquake_test = earthquake[(earthquake$Year == 2022 & earthquake$Month >= 10) | (earthquake$Year == 2023 & earthquake$Month <= 3 & earthquake$Day <= 31),]


earthquake_test = earthquake_test[c("Year", "Month", "Day", "Hour", "tri_ID")]
earthquake_test$true = rep(1, nrow(earthquake_test))
earthquake = earthquake_test

# Initialize columns for the previous Day
earthquake$PrevDay1 <- earthquake$Day
earthquake$PrevMonth1 <- earthquake$Month
earthquake$PrevYear1 <- earthquake$Year

# Function to adjust for the previous day and handle month/year changes
adjust_date_lag1 <- function(year, month, day) {
# Subtract 1 day
day <- day - 1
# Adjust day and month
if (day <= 0) {
  month <- month - 1
  if (month <= 0) {
    month <- 12
    year <- year - 1
  }
  # Set day to the last day of the previous month
  if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
    day <- 31
  } else if (month %in% c(4, 6, 9, 11)) {
    day <- 30
  } else {
    if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
      day <- 29
    } else {
      day <- 28
    }
  }
}
return(c(year, month, day))
}

# Apply the adjustment for each row
adjustments <- t(sapply(1:nrow(earthquake), function(i) {
adjust_date_lag1(earthquake$Year[i], earthquake$Month[i], earthquake$Day[i])
}))

# Assign adjusted values to the dataframe
earthquake$PrevYear1 <- adjustments[, 1]
earthquake$PrevMonth1 <- adjustments[, 2]
earthquake$PrevDay1 <- adjustments[, 3]

new_df = read.csv("new_df_diff8760.csv")

# Merge earthquake with new_df for lag 1
merged_df_lag1 <- merge(
earthquake, 
new_df, 
by.x = c("PrevYear1", "PrevMonth1", "PrevDay1", "Hour"), 
by.y = c("Year", "Month", "Day", "Hour"),
all.x = TRUE
)

# Standardize the merged_df
merged_df = merged_df_lag1[,-(1:3)]

# Step 2: Extract the corresponding t and n columns based on tri_ID
merged_df$t <- sapply(1:nrow(merged_df), function(i) {
tri_col <- paste0("t", merged_df$tri_ID[i])
return(merged_df[i, tri_col])
})
merged_df$n <- sapply(1:nrow(merged_df), function(i) {
tri_col <- paste0("n", merged_df$tri_ID[i])
return(merged_df[i, tri_col])
})

# Step 3: Select only relevant columns for earthquake
earthquake_test = merged_df[,c("Year", "Month", "Day", "Hour", "true", "tri_ID", "t", "n")]
earthquake_test = earthquake_test[!is.na(earthquake_test$t) & !is.na(earthquake_test$n), ]

x = 1
dat = dat[,(3*x-2):(3*x)]
center_col = paste0("center", x)
neighbor_col = paste0("neighbor", x)
occur_col = paste0("occur", x)

# Build the model on the whole dataset
genData = smotefamily::SMOTE(dat[,c(center_col, neighbor_col)], dat[,c(occur_col)])
first.balanced = genData$data
first.balanced[,occur_col] = as.factor(first.balanced$class)
dat = first.balanced[,c(center_col, neighbor_col, occur_col)]

# Using QDA
threshold = seq(0, 0.5, length.out = 30)
j = 30
fit.qda = MASS::qda(as.formula(paste(occur_col, "~", center_col, "+", neighbor_col)), data = dat)
test = setNames(data.frame(earthquake_test$t, earthquake_test$n), c(center_col, neighbor_col))
out.qda = predict(fit.qda, test)
prob.qda = out.qda$posterior[,"1"]
pred.qda = rep(0, length(prob.qda))
pred.qda[prob.qda > threshold[j]] = 1
earthquake_test$predict = pred.qda

# Error rate
cat("Error Rate: ", mean(earthquake_test$true != earthquake_test$predict), "\n")
nrow(earthquake_test)

# Visualization
ggplot(dat, aes(x = center1, y = neighbor1, color = factor(occur1))) +
  geom_point(data = filter(dat, occur1 == 0), alpha = 0.2) +  # Plot blue points first
  geom_point(data = filter(dat, occur1 == 1), alpha = 0.4) +  # Plot red points on top
  scale_color_manual(values = c("blue", "red"), labels = c("No", "Yes")) +  
  scale_x_continuous(limits = c(0, 270)) +
  scale_y_continuous(limits = c(50, 230)) +
  labs(x = "Central Groundwater Level",
       y = "Neighboring Groundwater Level",
       color = "Earthquake") +
  theme_minimal(base_size = 20)
dat_2 = data.frame(center1 = earthquake_test$t, neighbor1 = earthquake_test$n, occur1 = earthquake_test$predict)
ggplot(dat_2, aes(x = center1, y = neighbor1, color = factor(occur1))) +
  geom_point(data = filter(dat_2, occur1 == 0), alpha = 0.2) +  # Plot blue points first
  geom_point(data = filter(dat_2, occur1 == 1), alpha = 0.4) +  # Plot red points on top
  scale_color_manual(values = c("red", "blue"), labels = c("Predict Yes", "Predict No")) +  
  scale_x_continuous(limits = c(0, 270)) +
  scale_y_continuous(limits = c(50, 230)) +
  labs(x = "Central Groundwater Level",
       y = "Neighboring Groundwater Level",
       color = "Earthquake (False Negative)") +
  theme_minimal(base_size = 20)




#### Error rate on future data for non-earthquake data points ####
dat = read.csv("Dataset.csv")
earthquake = read.csv("Earthquake_data.csv")

# IMPORTANT: Adjust the time range to match your training/testing window!
# 2020/4/1 ~ 2020/9/30
start_time <- as.POSIXct("2020-04-02", tz = "UTC")
end_time <- as.POSIXct("2020-09-30", tz = "UTC")
earthquake_test = earthquake[(earthquake$Year == 2020 & ((earthquake$Month > 4 & earthquake$Month < 9) | (earthquake$Month == 4 & earthquake$Day >= 1) | (earthquake$Month == 9 & earthquake$Day <= 30))),]
# 2020/10/1 ~ 2021/3/31
start_time <- as.POSIXct("2020-10-02", tz = "UTC")
end_time <- as.POSIXct("2021-03-31", tz = "UTC")
earthquake_test = earthquake[(earthquake$Year == 2020 & earthquake$Month >= 10) | (earthquake$Year == 2021 & earthquake$Month <= 3 & earthquake$Day <= 31),]
# 2021/4/1 ~ 2021/9/30
start_time <- as.POSIXct("2021-04-02", tz = "UTC")
end_time <- as.POSIXct("2021-09-30", tz = "UTC")
earthquake_test = earthquake[(earthquake$Year == 2021 & ((earthquake$Month > 4 & earthquake$Month < 9) | (earthquake$Month == 4 & earthquake$Day >= 1) | (earthquake$Month == 9 & earthquake$Day <= 30))),]
# 2021/10/1 ~ 2022/3/31
start_time <- as.POSIXct("2021-10-02", tz = "UTC")
end_time <- as.POSIXct("2022-03-31", tz = "UTC")
earthquake_test = earthquake[(earthquake$Year == 2021 & earthquake$Month >= 10) | (earthquake$Year == 2022 & earthquake$Month <= 3 & earthquake$Day <= 31),]
# 2022/4/1 ~ 2022/9/30
start_time <- as.POSIXct("2022-04-02", tz = "UTC")
end_time <- as.POSIXct("2022-09-30", tz = "UTC")
earthquake_test = earthquake[(earthquake$Year == 2022 & ((earthquake$Month > 4 & earthquake$Month < 9) | (earthquake$Month == 4 & earthquake$Day >= 1) | (earthquake$Month == 9 & earthquake$Day <= 30))),]
# 2022/10/1 ~ 2023/3/31
start_time <- as.POSIXct("2022-10-02", tz = "UTC")
end_time <- as.POSIXct("2023-03-31", tz = "UTC")
earthquake_test = earthquake[(earthquake$Year == 2022 & earthquake$Month >= 10) | (earthquake$Year == 2023 & earthquake$Month <= 3 & earthquake$Day <= 31),]

earthquake_test = earthquake_test[c("Year", "Month", "Day", "Hour", "tri_ID")]
earthquake_test$true = rep(1, nrow(earthquake_test))

# Create the date sequence
time_seq <- seq(from = start_time, to = end_time, by = "hour")

# Create a data frame for the full time sequence
full_time_df <- data.frame(
  Year = as.integer(format(time_seq, "%Y")),
  Month = as.integer(format(time_seq, "%m")),
  Day = as.integer(format(time_seq, "%d")),
  Hour = as.integer(format(time_seq, "%H")), 
  true = rep(0, length(time_seq))
)
tri_temp = data.frame(tri_ID = 1:68)
expanded_df = merge(full_time_df, tri_temp, by = NULL)

# Remove row that appear in earthquake_test
final_data = anti_join(expanded_df, earthquake_test, 
                        by = c("Year", "Month", "Day", "Hour", "tri_ID"))

earthquake = final_data

# Initialize columns for the previous Day
earthquake$PrevDay1 <- earthquake$Day
earthquake$PrevMonth1 <- earthquake$Month
earthquake$PrevYear1 <- earthquake$Year

# Function to adjust for the previous day and handle month/year changes
adjust_date_lag1 <- function(year, month, day) {
  # Subtract 1 day
  day <- day - 1
  # Adjust day and month
  if (day <= 0) {
    month <- month - 1
    if (month <= 0) {
      month <- 12
      year <- year - 1
    }
    # Set day to the last day of the previous month
    if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {
      day <- 31
    } else if (month %in% c(4, 6, 9, 11)) {
      day <- 30
    } else {
      if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
        day <- 29
      } else {
        day <- 28
      }
    }
  }
  return(c(year, month, day))
}

# Apply the adjustment for each row
adjustments <- t(sapply(1:nrow(earthquake), function(i) {
  adjust_date_lag1(earthquake$Year[i], earthquake$Month[i], earthquake$Day[i])
}))

# Assign adjusted values to the dataframe
earthquake$PrevYear1 <- adjustments[, 1]
earthquake$PrevMonth1 <- adjustments[, 2]
earthquake$PrevDay1 <- adjustments[, 3]

new_df = read.csv("new_df_diff8760.csv")

# Merge earthquake with new_df for lag 1
merged_df_lag1 <- merge(
  earthquake, 
  new_df, 
  by.x = c("PrevYear1", "PrevMonth1", "PrevDay1", "Hour"), 
  by.y = c("Year", "Month", "Day", "Hour"),
  all.x = TRUE
)

# Standardize the merged_df
merged_df = merged_df_lag1[,-(1:3)]

# Step 2: Extract the corresponding t and n columns based on tri_ID
merged_df$t <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("t", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})
merged_df$n <- sapply(1:nrow(merged_df), function(i) {
  tri_col <- paste0("n", merged_df$tri_ID[i])
  return(merged_df[i, tri_col])
})

# Step 3: Select only relevant columns for earthquake
earthquake_test = merged_df[,c("Year", "Month", "Day", "Hour", "true", "tri_ID", "t", "n")]
earthquake_test = earthquake_test[!is.na(earthquake_test$t) & !is.na(earthquake_test$n), ]

x = 1
dat = read.csv("Dataset.csv")
dat = dat[,(3*x-2):(3*x)]
center_col = paste0("center", x)
neighbor_col = paste0("neighbor", x)
occur_col = paste0("occur", x)

# Build the model on the whole dataset
genData = smotefamily::SMOTE(dat[,c(center_col, neighbor_col)], dat[,c(occur_col)])
first.balanced = genData$data
first.balanced[,occur_col] = as.factor(first.balanced$class)
dat = first.balanced[,c(center_col, neighbor_col, occur_col)]

# Using QDA
threshold = seq(0, 0.5, length.out = 30)
j = 30
fit.qda = MASS::qda(as.formula(paste(occur_col, "~", center_col, "+", neighbor_col)), data = dat)
test = setNames(data.frame(earthquake_test$t, earthquake_test$n), c(center_col, neighbor_col))
out.qda = predict(fit.qda, test)
prob.qda = out.qda$posterior[,"1"]
pred.qda = rep(0, length(prob.qda))
pred.qda[prob.qda > threshold[j]] = 1
earthquake_test$predict = pred.qda

# Error rate
mean(earthquake_test$true != earthquake_test$predict)
nrow(earthquake_test)

# Visualization
dat_2 = data.frame(center1 = earthquake_test$t, neighbor1 = earthquake_test$n, occur1 = earthquake_test$predict)
ggplot(dat_2, aes(x = center1, y = neighbor1, color = factor(occur1))) +
  geom_point(data = filter(dat_2, occur1 == 0), alpha = 0.2) +  # Plot blue points first
  geom_point(data = filter(dat_2, occur1 == 1), alpha = 0.4) +  # Plot red points on top
  scale_color_manual(values = c("blue", "red"), labels = c("Predict No", "Predict Yes")) +  
  scale_x_continuous(limits = c(0, 270)) +
  scale_y_continuous(limits = c(50, 230)) +
  labs(x = "Central Groundwater Level",
       y = "Neighboring Groundwater Level",
       color = "Earthquake (False Positive)") +
  theme_minimal(base_size = 20)



##### Map Figure ####
library(ggplot2)
library(maps)
library(mapdata)

df = read.csv("station_location_ver.2.csv")
earthquake = read.csv("Earthquake_data.csv")
# Get Taiwan map data
taiwan_map <- map_data("world", region = "Taiwan")

# Define zoom limits based on station locations
x_min <- min(df$X) - 0.01
x_max <- max(df$X) + 0.01
y_min <- min(df$Y) - 0.01
y_max <- max(df$Y) + 0.01

# Plot the zoomed-in map
ggplot() +
  geom_polygon(data = taiwan_map, aes(x = long, y = lat, group = group),
               fill = "darkseagreen1", color = "darkgreen") +
  geom_point(data = df, aes(x = X, y = Y), color = "blue", size = 2) +  # Plot stations
  geom_text(data = df, aes(x = X, y = Y, label = ID), vjust = -1, color = "blue", size = 2.5, fontface = "bold") +  # Label stations
  geom_point(data = earthquake, aes(x = Longitude, y = Latitude),
             shape = 16, size = 2, alpha = 0.25, color = "black") +
  scale_size_continuous(range = c(2, 6)) +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +  # Zoom into region
  # coord_fixed(ratio = 1) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")

