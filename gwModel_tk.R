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
library(e1071)
tri = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Occurrence_ver.2.1.csv")
dat = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag9_ver.2.1.csv")
#### First Layer: Split into two groups based on Triangle ID ####
# For group 1 (Tri_ID that doesn't lead to earthquake according to historical information), if water level combination happened before, then prob := 0.
# Otherwise (unusual combination of water level), we put it into second layer (exception handler).
# For group 2 (Tri_ID that lead to earthquake according to historical information), we put it into second layer.
all_ids <- 1:68
ids_lead_earthquake <- unique(tri$Tri_ID)
ids_no_earthquake <- setdiff(all_ids, ids_lead_earthquake)

# Remove Tri_ID that does not lead to earthquake and recalculate the earthquake occurrences
center = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/averaged_triangle_water_level_data.csv")
neighbor = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/neighbor_triangle_averaged_water_level_data.csv")
columns_to_remove_center <- c("Tri_ID52", "Tri_ID54", "Tri_ID56", "Tri_ID58", "Tri_ID59", "Tri_ID60", "Tri_ID64", "Tri_ID65")
columns_to_remove_neighbor <- c("Tri_ID_nb52", "Tri_ID_nb54", "Tri_ID_nb56", "Tri_ID_nb58", "Tri_ID_nb59", "Tri_ID_nb60", "Tri_ID_nb64", "Tri_ID_nb65")

# new center and new neighbor with Tri_ID that doesn't lead to earthquake removed
center <- center[, setdiff(names(center), columns_to_remove_center)]
neighbor <- neighbor[, setdiff(names(neighbor), columns_to_remove_neighbor)]

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
# result_vector contains IDs that lead to earthquake
result_vector <- setdiff(1:68, ids_no_earthquake)
colnames(new_df)[5:ncol(new_df)] <- unlist(
  lapply(result_vector, function(i) c(paste0("t", i), paste0("n", i)))
)

# Display the new data frame
new_df

# Define the range of interest (you can adjust this range as needed)
min_val <- floor(min(new_df[, 5:ncol(new_df)], na.rm = TRUE)) # Minimum value in the data
max_val <- ceiling(max(new_df[, 5:ncol(new_df)], na.rm = TRUE)) # Maximum value in the data

# Generate intervals of length 0.2 m
ranges <- lapply(seq(min_val, max_val - 0.2, by = 0.2), function(x) c(x, x + 0.2))
# Convert ranges to a data frame

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
  }
  return(NA)  # Return NA if no range is matched
}


# Apply the function to each value in the relevant columns (5th to last)
new_df[, 5:ncol(new_df)] <- apply(new_df[, 5:ncol(new_df)], 2, function(column) {
  sapply(column, assign_level)
})

# Display the updated data frame
new_df

# Calculate the frequency
# Initialize an empty vector to store all individual combinations
all_combinations <- c()

# Loop through each row to extract individual combinations
for (i in 1:nrow(new_df)) {
  # Extract combinations for the current row
  pairs <- sapply(result_vector, function(j) {
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

# Calculate the frequency of each unique combination
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

# Lag 1
# Read data
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/earthquake_in_triangle_ver.2.1_2.5_1e-3.csv")

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake$HourBlock <- cut(earthquake$Hour,
                            breaks = c(0, 6, 12, 18, 24),
                            labels = c(1, 2, 3, 4),
                            include.lowest = TRUE)
earthquake = earthquake[,c(2,3,4,13,8:12)]

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
  by.x = c("PrevYear1", "PrevMonth1", "PrevDay1", "HourBlock"), 
  by.y = c("Year", "Month", "Day", "HourBlock"),
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
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/earthquake_in_triangle_ver.2.1_2.5_1e-3.csv")

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake$HourBlock <- cut(earthquake$Hour,
                            breaks = c(0, 6, 12, 18, 24),
                            labels = c(1, 2, 3, 4),
                            include.lowest = TRUE)
earthquake = earthquake[,c(2,3,4,13,8:12)]

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
  by.x = c("PrevYear2", "PrevMonth2", "PrevDay2", "HourBlock"), 
  by.y = c("Year", "Month", "Day", "HourBlock"),
  all.x = TRUE
)

# Display the merged data frame
print(merged_df_lag2)

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
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/earthquake_in_triangle_ver.2.1_2.5_1e-3.csv")

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake$HourBlock <- cut(earthquake$Hour,
                            breaks = c(0, 6, 12, 18, 24),
                            labels = c(1, 2, 3, 4),
                            include.lowest = TRUE)
earthquake = earthquake[,c(2,3,4,13,8:12)]

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
  by.x = c("PrevYear3", "PrevMonth3", "PrevDay3", "HourBlock"), 
  by.y = c("Year", "Month", "Day", "HourBlock"),
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
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/earthquake_in_triangle_ver.2.1_2.5_1e-3.csv")

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake$HourBlock <- cut(earthquake$Hour,
                            breaks = c(0, 6, 12, 18, 24),
                            labels = c(1, 2, 3, 4),
                            include.lowest = TRUE)
earthquake = earthquake[,c(2,3,4,13,8:12)]

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
  by.x = c("PrevYear4", "PrevMonth4", "PrevDay4", "HourBlock"), 
  by.y = c("Year", "Month", "Day", "HourBlock"),
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
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/earthquake_in_triangle_ver.2.1_2.5_1e-3.csv")

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake$HourBlock <- cut(earthquake$Hour,
                            breaks = c(0, 6, 12, 18, 24),
                            labels = c(1, 2, 3, 4),
                            include.lowest = TRUE)
earthquake = earthquake[,c(2,3,4,13,8:12)]

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
  by.x = c("PrevYear5", "PrevMonth5", "PrevDay5", "HourBlock"), 
  by.y = c("Year", "Month", "Day", "HourBlock"),
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
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/earthquake_in_triangle_ver.2.1_2.5_1e-3.csv")

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake$HourBlock <- cut(earthquake$Hour,
                            breaks = c(0, 6, 12, 18, 24),
                            labels = c(1, 2, 3, 4),
                            include.lowest = TRUE)
earthquake = earthquake[,c(2,3,4,13,8:12)]

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
  by.x = c("PrevYear6", "PrevMonth6", "PrevDay6", "HourBlock"), 
  by.y = c("Year", "Month", "Day", "HourBlock"),
  all.x = TRUE
)

# Display the merged data frame
print(merged_df_lag6)

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
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/earthquake_in_triangle_ver.2.1_2.5_1e-3.csv")

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake$HourBlock <- cut(earthquake$Hour,
                            breaks = c(0, 6, 12, 18, 24),
                            labels = c(1, 2, 3, 4),
                            include.lowest = TRUE)
earthquake = earthquake[,c(2,3,4,13,8:12)]

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
  by.x = c("PrevYear7", "PrevMonth7", "PrevDay7", "HourBlock"), 
  by.y = c("Year", "Month", "Day", "HourBlock"),
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
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/earthquake_in_triangle_ver.2.1_2.5_1e-3.csv")

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake$HourBlock <- cut(earthquake$Hour,
                            breaks = c(0, 6, 12, 18, 24),
                            labels = c(1, 2, 3, 4),
                            include.lowest = TRUE)
earthquake = earthquake[,c(2,3,4,13,8:12)]

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
  by.x = c("PrevYear8", "PrevMonth8", "PrevDay8", "HourBlock"), 
  by.y = c("Year", "Month", "Day", "HourBlock"),
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
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/earthquake_in_triangle_ver.2.1_2.5_1e-3.csv")

# Add a new column 'HourBlock' based on the 'Hour' values
earthquake$HourBlock <- cut(earthquake$Hour,
                            breaks = c(0, 6, 12, 18, 24),
                            labels = c(1, 2, 3, 4),
                            include.lowest = TRUE)
earthquake = earthquake[,c(2,3,4,13,8:12)]

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
  by.x = c("PrevYear9", "PrevMonth9", "PrevDay9", "HourBlock"), 
  by.y = c("Year", "Month", "Day", "HourBlock"),
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

# Union Occurrences
# Step 1: Union lag1 and lag2
union_lag1_lag2 <- sorted_combination_freq_lag1 %>%
  full_join(sorted_combination_freq_lag2, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  dplyr::select(combination, frequency)

# Step 2: Union the result with lag3
union_lag1_lag3 <- union_lag1_lag2 %>%
  full_join(sorted_combination_freq_lag3, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  dplyr::select(combination, frequency)

# Step 3: Union the result with lag4
union_lag1_lag4 <- union_lag1_lag3 %>%
  full_join(sorted_combination_freq_lag4, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  dplyr::select(combination, frequency)

# Step 4: Union the result with lag5
union_lag1_lag5 <- union_lag1_lag4 %>%
  full_join(sorted_combination_freq_lag5, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  dplyr::select(combination, frequency)

# Step 5: Union the result with lag 6
union_lag1_lag6 = union_lag1_lag5 %>%
  full_join(sorted_combination_freq_lag6, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  dplyr::select(combination, frequency)

# Step 6: Union the result with lag 7
union_lag1_lag7 = union_lag1_lag6 %>%
  full_join(sorted_combination_freq_lag7, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  dplyr::select(combination, frequency)

# Step 7: Union the result with lag 8
union_lag1_lag8 = union_lag1_lag7 %>%
  full_join(sorted_combination_freq_lag8, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  dplyr::select(combination, frequency)

# Step 8: Union the result with lag 9
union_lag1_lag9 = union_lag1_lag8 %>%
  full_join(sorted_combination_freq_lag9, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  dplyr::select(combination, frequency)

# Split the combination into X and Y parts
union_lag1_lag3$X <- as.numeric(sub("_.*", "", union_lag1_lag3$combination))
union_lag1_lag3$Y <- as.numeric(sub(".*_", "", union_lag1_lag3$combination))
union_lag1_lag5$X <- as.numeric(sub("_.*", "", union_lag1_lag5$combination))
union_lag1_lag5$Y <- as.numeric(sub(".*_", "", union_lag1_lag5$combination))
union_lag1_lag7$X <- as.numeric(sub("_.*", "", union_lag1_lag7$combination))
union_lag1_lag7$Y <- as.numeric(sub(".*_", "", union_lag1_lag7$combination))
union_lag1_lag9$X <- as.numeric(sub("_.*", "", union_lag1_lag9$combination))
union_lag1_lag9$Y <- as.numeric(sub(".*_", "", union_lag1_lag9$combination))

# Sort by X first and then by Y
sorted_union_lag1_lag3 <- union_lag1_lag3[order(union_lag1_lag3$X, union_lag1_lag3$Y), ]
sorted_union_lag1_lag5 <- union_lag1_lag5[order(union_lag1_lag5$X, union_lag1_lag5$Y), ]
sorted_union_lag1_lag7 <- union_lag1_lag7[order(union_lag1_lag7$X, union_lag1_lag7$Y), ]
sorted_union_lag1_lag9 <- union_lag1_lag9[order(union_lag1_lag9$X, union_lag1_lag9$Y), ]

# Display the sorted combinations with their frequencies
union_lag1_lag3 <- sorted_union_lag1_lag3[, c("combination", "frequency")]
print(union_lag1_lag3)
union_lag1_lag5 <- sorted_union_lag1_lag5[, c("combination", "frequency")]
print(union_lag1_lag5)
union_lag1_lag7 <- sorted_union_lag1_lag7[, c("combination", "frequency")]
print(union_lag1_lag7)
union_lag1_lag9 <- sorted_union_lag1_lag9[, c("combination", "frequency")]
print(union_lag1_lag9)

# Function to calculate probabilities
calculate_probabilities <- function(union_df, whole_df) {
  prob_df <- whole_df %>%
    left_join(union_df, by = "combination") %>%
    mutate(probability = coalesce(frequency.y, 0) / frequency.x) %>%
    dplyr::select(combination, probability)
  return(prob_df)
}

# Calculate probabilities for each union data frame
prob_lag1_lag3 <- calculate_probabilities(union_lag1_lag3, sorted_combination_freq_whole)
prob_lag1_lag5 <- calculate_probabilities(union_lag1_lag5, sorted_combination_freq_whole)
prob_lag1_lag7 <- calculate_probabilities(union_lag1_lag7, sorted_combination_freq_whole)
prob_lag1_lag9 <- calculate_probabilities(union_lag1_lag9, sorted_combination_freq_whole)

# Result
head(prob_lag1_lag3[order(prob_lag1_lag3$probability, decreasing = T),])
head(prob_lag1_lag5[order(prob_lag1_lag5$probability, decreasing = T),])
head(prob_lag1_lag7[order(prob_lag1_lag7$probability, decreasing = T),])
head(prob_lag1_lag9[order(prob_lag1_lag9$probability, decreasing = T),])

# Write to csv (updated probability for group 2)
df <- prob_lag1_lag3[-nrow(prob_lag1_lag3),]
df <- df %>%
  separate(combination, into = c("center", "neighbor"), sep = "_")
write_csv(df, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag3_model.csv")
df <- prob_lag1_lag5[-nrow(prob_lag1_lag5),]
df <- df %>%
  separate(combination, into = c("center", "neighbor"), sep = "_")
write_csv(df, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag5_model.csv")
df <- prob_lag1_lag7[-nrow(prob_lag1_lag7),]
df <- df %>%
  separate(combination, into = c("center", "neighbor"), sep = "_")
write_csv(df, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag7_model.csv")
df <- prob_lag1_lag9[-nrow(prob_lag1_lag9),]
df <- df %>%
  separate(combination, into = c("center", "neighbor"), sep = "_")
write_csv(df, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag9_model.csv")

# Record water level combination for those Tri_ID that does not lead to earthquake
# Here the water level combination is called the usual combination
# new center and new neighbor with Tri_ID that doesn't lead to earthquake removed
center = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/averaged_triangle_water_level_data.csv")
neighbor = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.2.1/DataTemp/neighbor_triangle_averaged_water_level_data.csv")

center <- center[, c("Year", "Month", "Day", "HourBlock", columns_to_remove_center)]
neighbor <- neighbor[, c("Year", "Month", "Day", "HourBlock", columns_to_remove_neighbor)]

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
  lapply(ids_no_earthquake, function(i) c(paste0("t", i), paste0("n", i)))
)

# Display the new data frame
new_df

# Define the range of interest (you can adjust this range as needed)
# min_val <- floor(min(new_df[, 5:ncol(new_df)], na.rm = TRUE)) # Minimum value in the data
# max_val <- ceiling(max(new_df[, 5:ncol(new_df)], na.rm = TRUE)) # Maximum value in the data

# Generate intervals of length 1
# DO NOT generate the labels again (should reused)
# ranges <- lapply(seq(min_val, max_val - 1), function(x) c(x, x + 1))

# Automatically generate labels from 1 to the number of intervals
# labels <- seq_along(ranges)

# Function to assign levels based on the defined ranges, handling NA values
assign_level <- function(x) {
  if (is.na(x)) {
    return(NA)  # Return NA if the value is NA
  }
  for (i in seq_along(ranges)) {
    if (x >= ranges[[i]][1] && x < ranges[[i]][2]) {
      return(labels[i])
    }
  }
  return(NA)  # Return NA if no range is matched
}

# Apply the function to each value in the relevant columns (5th to last)
new_df[, 5:ncol(new_df)] <- apply(new_df[, 5:ncol(new_df)], 2, function(column) {
  sapply(column, assign_level)
})

# Display the updated data frame
new_df

# Calculate the frequency
# Initialize an empty vector to store all individual combinations
all_combinations <- c()

# Loop through each row to extract individual combinations
for (i in 1:nrow(new_df)) {
  # Extract combinations for the current row
  pairs <- sapply(ids_no_earthquake, function(j) {
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

# Calculate the frequency of each unique combination
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
sorted_combination_freq_whole_id_no_earthquake = data.frame(combination = sorted_combination_freq_whole$combination, frequency = sorted_combination_freq_whole$freq)
sorted_combination_freq_whole_id_no_earthquake = sorted_combination_freq_whole_id_no_earthquake[-nrow(sorted_combination_freq_whole_id_no_earthquake),]
print(sorted_combination_freq_whole_id_no_earthquake)
usual_combination_for_group_1 = sorted_combination_freq_whole_id_no_earthquake$combination
# Save the vector to a text file
writeLines(usual_combination_for_group_1, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/usual_combination_for_group_1.txt")


#### Second Layer: Classification for earthquake occurrences only on Group 2 ####
#### Second Layer - 1: Preprocessing ####
# Use 'Prob_cyc_lag1_lag9_model.csv'
dat = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag9_model.csv")

# Create a binary variable indicating non-zero probability and declare nominal categorical column as factor
dat <- dat %>%
  mutate(prob_nonzero = ifelse(probability > 0, 1, 0))
dat$prob_nonzero = as.factor(dat$prob_nonzero)

# Imbalanced (0.75:0.25)
freq_table = table(dat$prob_nonzero)
prop_table = prop.table(freq_table)
barplot(prop_table, ylim = c(0,1), main = "Distribution of prob_nonzero", xlab = "prob_nonzero",
        ylab = "proportion")

#### Second Layer - 2: Hyperparameters tuning ####
dat = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag9_model.csv")

# Create a binary variable indicating non-zero probability and declare nominal categorical column as factor
dat <- dat %>%
  mutate(prob_nonzero = ifelse(probability > 0, 1, 0))
dat$prob_nonzero = as.factor(dat$prob_nonzero)

loop = function() {
  FNR1 = ERR1 = PRO1 = matrix(0, 1, 30) # Logit
  FNR2 = ERR2 = PRO2 = matrix(0, 1, 30) # LDA
  FNR3 = ERR3 = PRO3 = matrix(0, 1, 30) # QDA
  FNR4 = ERR4 = PRO4 = matrix(0, 1, 30) # KNN
  FNR5 = ERR5 = PRO5 = matrix(0, 1, 100) # XGBoost
  # SPlit the data (n = 884)
  # train: 469 (SMOTE), test: 309, validation: 201
  first.idx = SPlit::SPlit(dat, splitRatio = 0.35)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit::SPlit(first, splitRatio = 0.35)
  train = first[-valid.idx,]
  validation = first[valid.idx,]
  # over-sampling using SMOTE on training data only
  genData = smotefamily::SMOTE(train[,c("center", "neighbor")], train[,c("prob_nonzero")])
  train.balanced = genData$data
  train.balanced$prob_nonzero = as.factor(train.balanced$class)
  train = train.balanced[,c("center", "neighbor", "prob_nonzero")]
  validation = validation[,c("center", "neighbor", "prob_nonzero")]
  # Naive method - Always predict prob_nonzero to 1 (worst)
  pred.naive = rep(1, nrow(test))
  Naive_Err = mean(pred.naive != test$prob_nonzero)
  Naive_FNR = 0
  conf_matrix = table(pred.naive, test$prob_nonzero)
  Naive_PRO = conf_matrix["1","1"] / (conf_matrix["1","0"] + conf_matrix["1","1"])
  # Logistic regresssion (1)
  threshold = seq(0, 0.5, length.out = 30)
  for (j in 1:30) {
    fit.logit = glm(prob_nonzero~center+neighbor, data = train, family = binomial)
    prob.logit = predict(fit.logit, validation, type = "response")
    pred.logit = rep(0, length(prob.logit))
    pred.logit[prob.logit > threshold[j]] = 1
    conf_matrix = table(pred.logit, validation$prob_nonzero)
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else {
      0  # If the indices are missing, default to 0
    }
    FNR1[1,j] = false_negatives / sum(validation$prob_nonzero == 1)
    # overall error rate
    ERR1[1,j] = mean(pred.logit != validation$prob_nonzero)
    conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["1","1"]
    } else {
      0
    }
    conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
      conf_matrix["1","0"]
    } else {
      0
    }
    PRO1[1,j] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
  }
  # LDA (2)
  threshold = seq(0, 0.5, length.out = 30)
  for (j in 1:30) {
    fit.lda = MASS::lda(prob_nonzero~center+neighbor, data = train)
    out.lda = predict(fit.lda, validation)
    prob.lda = out.lda$posterior[,"1"]
    pred.lda = rep(0, length(prob.lda))
    pred.lda[prob.lda > threshold[j]] = 1
    conf_matrix = table(pred.lda, validation$prob_nonzero)
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else {
      0  # If the indices are missing, default to 0
    }
    FNR2[1,j] = false_negatives / sum(validation$prob_nonzero == 1)
    # overall error rate
    ERR2[1,j] = mean(pred.lda != validation$prob_nonzero)
    conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["1","1"]
    } else {
      0
    }
    conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
      conf_matrix["1","0"]
    } else {
      0
    }
    PRO2[1,j] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
  }
  # QDA (3)
  threshold = seq(0, 0.5, length.out = 30)
  for (j in 1:30) {
    fit.qda = MASS::qda(prob_nonzero~center+neighbor, data = train)
    out.qda = predict(fit.qda, validation)
    prob.qda = out.qda$posterior[,"1"]
    pred.qda = rep(0, length(prob.qda))
    pred.qda[prob.qda > threshold[j]] = 1
    conf_matrix = table(pred.qda, validation$prob_nonzero)
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else {
      0  # If the indices are missing, default to 0
    }
    FNR3[1,j] = false_negatives / sum(validation$prob_nonzero == 1)
    # overall error rate
    ERR3[1,j] = mean(pred.qda != validation$prob_nonzero)
    conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["1","1"]
    } else {
      0
    }
    conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
      conf_matrix["1","0"]
    } else {
      0
    }
    PRO3[1,j] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
  }
  # KNN (4)
  K = 30
  train.X = train[,c("center", "neighbor")]
  train.Y = train$prob_nonzero
  validation.X = validation[,c("center", "neighbor")]
  validation.Y = validation$prob_nonzero
  test.X = test[,c("center", "neighbor")]
  test.Y = test$prob_nonzero
  for (j in 1:K) {
    pred.knn = class::knn(train.X, validation.X, train.Y, k = j)
    conf_matrix = table(pred.knn, validation.Y)
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else {
      0  # If the indices are missing, default to 0
    }
    FNR4[1,j] = false_negatives / sum(validation.Y == 1)
    # overall error rate
    ERR4[1,j] = mean(pred.knn != validation.Y)
    conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["1","1"]
    } else {
      0
    }
    conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
      conf_matrix["1","0"]
    } else {
      0
    }
    PRO4[1,j] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
  }
  # Random Forest (5)
  # ntree_cand = as.integer(seq(1000, 5000, length.out = 30))
  # for (j in 1:30) {
  #   fit.rf = randomForest::randomForest(prob_nonzero~neighbor+center, data = train, ntree = ntree_cand[j])
  #   pred.rf = predict(fit.rf, validation)
  #   conf_matrix = table(pred.rf, validation$prob_nonzero)
  #   # False negative rate
  #   false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
  #     conf_matrix["0","1"]
  #   } else {
  #     0  # If the indices are missing, default to 0
  #   }
  #   FNR5[1,j] = false_negatives / sum(validation$prob_nonzero == 1)
  #   # overall error rate
  #   ERR5[1,j] = mean(pred.rf != validation$prob_nonzero)
  #   conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
  #     conf_matrix["1","1"]
  #   } else {
  #     0
  #   }
  #   conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
  #     conf_matrix["1","0"]
  #   } else {
  #     0
  #   }
  #   PRO5[1,j] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
  # }
  # XGBoost
  eta_cand = c(0.005, 0.01, 0.02)
  max_depth_cand = c(4, 7, 10)
  threshold_cand = c(0.2, 0.3, 0.4)
  X_train = data.matrix(train[,1:2])
  y_train = train$prob_nonzero
  X_test = data.matrix(test[,1:2])
  y_test = test$prob_nonzero
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
                                     nrounds = 4000)
        pred = predict(fit.xgb, xgboost_test)
        pred.xgb = as.numeric(pred > threshold_cand[j])
        conf_matrix = table(pred.xgb, test$prob_nonzero)
        # False negative rate
        false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
          conf_matrix["0","1"]
        } else {
          0  # If the indices are missing, default to 0
        }
        FNR5[1,k] = false_negatives / sum(test$prob_nonzero == 1)
        # overall error rate
        ERR5[1,k] = mean(pred.xgb != test$prob_nonzero)
        conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
          conf_matrix["1","1"]
        } else {
          0
        }
        conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
          conf_matrix["1","0"]
        } else {
          0
        }
        PRO5[1,k] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
        k = k + 1
      }
  
  result = list(FNR1 = FNR1, FNR2 = FNR2, FNR3 = FNR3, FNR4 = FNR4, FNR5 = FNR5,
                ERR1 = ERR1, ERR2 = ERR2, ERR3 = ERR3, ERR4 = ERR4, ERR5 = ERR5,
                PRO1 = PRO1, PRO2 = PRO2, PRO3 = PRO3, PRO4 = PRO4, PRO5 = PRO5)
  combined_df <- do.call(cbind, result)
  return(combined_df)
}

set.seed(5)
N = 200
num_core = detectCores() # 8 cores
cl = makeCluster(num_core)
registerDoSNOW(cl)
RES = foreach(j = 1:N,.combine = "rbind") %dopar% loop()
stopCluster(cl)



FNR_1 = colMeans(RES[,1:30]); ERR_1 = colMeans(RES[,221:250]); PRO_1 = colMeans(RES[,441:470])
FNR_2 = colMeans(RES[,31:60]); ERR_2 = colMeans(RES[,251:280]); PRO_2 = colMeans(RES[,471:500])
FNR_3 = colMeans(RES[,61:90]); ERR_3 = colMeans(RES[,281:310]); PRO_3 = colMeans(RES[,501:530])
FNR_4 = colMeans(RES[,91:120]); ERR_4 = colMeans(RES[,311:340]); PRO_4 = colMeans(RES[,531:560])
FNR_5 = colMeans(RES[,121:220]); ERR_5 = colMeans(RES[,341:440]); PRO_5 = colMeans(RES[,561:660])
FNR_5 = FNR_5[1:27]; ERR_5 = ERR_5[1:27]; PRO_5 = PRO_5[1:27]

# Combine data
data <- bind_rows(
  data.frame(Method = "Logistic Regression", Threshold = 1:length(FNR_1), 
             Rate = c(FNR_1, ERR_1, PRO_1), 
             Type = rep(c("False Negative Rate", "Error Rate", "Proportion of Nonzero"), each = length(FNR_1))),
  data.frame(Method = "LDA", Threshold = 1:length(FNR_2), 
             Rate = c(FNR_2, ERR_2, PRO_2), 
             Type = rep(c("False Negative Rate", "Error Rate", "Proportion of Nonzero"), each = length(FNR_2))),
  data.frame(Method = "QDA", Threshold = 1:length(FNR_3), 
             Rate = c(FNR_3, ERR_3, PRO_3), 
             Type = rep(c("False Negative Rate", "Error Rate", "Proportion of Nonzero"), each = length(FNR_3))),
  data.frame(Method = "KNN", Threshold = 1:length(FNR_4), 
             Rate = c(FNR_4, ERR_4, PRO_4), 
             Type = rep(c("False Negative Rate", "Error Rate", "Proportion of Nonzero"), each = length(FNR_4))),
  data.frame(Method = "XGBoost", Threshold = 1:length(FNR_5), 
             Rate = c(FNR_5, ERR_5, PRO_5), 
             Type = rep(c("False Negative Rate", "Error Rate", "Proportion of Nonzero"), each = length(FNR_5)))
)

write_csv(data, "/Users/sunpierce/Desktop/Model.csv")
data = read.csv("/Users/sunpierce/Desktop/Model.csv")

vertical_lines <- data.frame(
  Method = c("Logistic Regression", "LDA", "QDA", "KNN", "XGBoost"),
  Vertical_Line = c(24, 24, 30, 5, 23) # Use NA for RF if no vertical line exists
)

ggplot(data, aes(x = Threshold, y = Rate, color = Type)) + 
  geom_line(size = 0.8) +   
  geom_point(aes(shape = Type), size = 2, stroke = 1) +  # Add points with different shapes based on Type
  # Add vertical lines   
  geom_vline(data = vertical_lines, aes(xintercept = Vertical_Line), 
             linetype = "longdash", color = "purple", linewidth = 1) +   
  # Facet by method   
  facet_wrap(~ Method, scales = "free_x", ncol = 5) +   
  # Adjust colors, linetypes, and shapes   
  scale_color_manual(values = c("Error Rate" = "deepskyblue", 
                                "False Negative Rate" = "coral1", 
                                "Proportion of Nonzero" = "chartreuse3"), 
                     name = "") +
  scale_shape_manual(values = c("Error Rate" = 0,
                                "False Negative Rate" = 4,
                                "Proportion of Nonzero" = 1)) +
  # Labels and themes   
  labs(x = "Threshold / Parameter", y = "Rate (mean over 200 SPlit subsampling splits)", color = "Metric", shape = "Metric") +   
  theme_minimal(base_size = 16) +
  theme(text = element_text(family = "Helvetica"), 
        legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm")) +   
  # Improved legend settings   
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(shape = c(0, 4, 1))),
         shape = "none") +  # Merge shape into color legend
  theme(legend.position = "top", legend.box = "horizontal")

#### Second Layer - 3: Modeling on [train + validation] ####
# Create a binary variable indicating non-zero probability and declare nominal categorical column as factor
dat = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag9_model.csv")
dat <- dat %>%
  mutate(prob_nonzero = ifelse(probability > 0, 1, 0))
dat$prob_nonzero = as.factor(dat$prob_nonzero)

threshold = seq(0, 0.5, length.out = 30)
loop = function() {
  FNR = ERR = PRO = matrix(0, 1, 7) # 6 indicates number of candidate model
  # SPlit the data
  first.idx = SPlit::SPlit(dat, splitRatio = 0.35)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit::SPlit(first, splitRatio = 0.35)
  train = first[-valid.idx,]
  validation = first[valid.idx,]
  # over-sampling using SMOTE on training data only
  genData = smotefamily::SMOTE(train[,c("center", "neighbor")], train[,c("prob_nonzero")])
  train.balanced = genData$data
  train.balanced$prob_nonzero = as.factor(train.balanced$class)
  train = train.balanced[,c("center", "neighbor", "prob_nonzero")]
  validation = validation[,c("center", "neighbor", "prob_nonzero")]
  # train: 540, test: 309, validation: 144
  # Naive method - Always predict prob_nonzero to 1 (worst)
  pred.naive = rep(1, nrow(test))
  ERR[1,7] = mean(pred.naive != test$prob_nonzero)
  FNR[1,7] = 0
  conf_matrix = table(pred.naive, test$prob_nonzero)
  PRO[1,7] = conf_matrix["1","1"] / (conf_matrix["1","0"] + conf_matrix["1","1"])
  # Logistic regression
  train_valid = rbind(train, validation)
  fit.logit = glm(prob_nonzero~center+neighbor, data = train_valid, family = binomial)
  prob.logit = predict(fit.logit, test, type = "response")
  pred.logit = rep(0, length(prob.logit))
  pred.logit[prob.logit > threshold[24]] = 1
  conf_matrix = table(pred.logit, test$prob_nonzero)
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else {
    0  # If the indices are missing, default to 0
  }
  FNR[1,1] = false_negatives / sum(test$prob_nonzero == 1)
  # overall error rate
  ERR[1,1] = mean(pred.logit != test$prob_nonzero)
  conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["1","1"]
  } else {
    0
  }
  conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1","0"]
  } else {
    0
  }
  PRO[1,1] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
  # LDA
  fit.lda = MASS::lda(prob_nonzero~center+neighbor, data = train_valid)
  out.lda = predict(fit.lda, test)
  prob.lda = out.lda$posterior[,"1"]
  pred.lda = rep(0, length(prob.lda))
  pred.lda[prob.lda > threshold[24]] = 1
  conf_matrix = table(pred.lda, test$prob_nonzero)
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else {
    0  # If the indices are missing, default to 0
  }
  FNR[1,2] = false_negatives / sum(test$prob_nonzero == 1)
  # overall error rate
  ERR[1,2] = mean(pred.lda != test$prob_nonzero)
  conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["1","1"]
  } else {
    0
  }
  conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1","0"]
  } else {
    0
  }
  PRO[1,2] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
  # QDA
  fit.qda = MASS::qda(prob_nonzero~center+neighbor, data = train_valid)
  out.qda = predict(fit.qda, test)
  prob.qda = out.qda$posterior[,"1"]
  pred.qda = rep(0, length(prob.qda))
  pred.qda[prob.qda > threshold[30]] = 1
  conf_matrix = table(pred.qda, test$prob_nonzero)
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else {
    0  # If the indices are missing, default to 0
  }
  FNR[1,3] = false_negatives / sum(test$prob_nonzero == 1)
  # overall error rate
  ERR[1,3] = mean(pred.qda != test$prob_nonzero)
  conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["1","1"]
  } else {
    0
  }
  conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1","0"]
  } else {
    0
  }
  PRO[1,3] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
  # KNN
  train_valid.X = train_valid[,c("center", "neighbor")]
  train_valid.Y = train_valid$prob_nonzero
  test.X = test[,c("center", "neighbor")]
  test.Y = test$prob_nonzero
  pred.knn = class::knn(train_valid.X, test.X, train_valid.Y, k = 5)
  conf_matrix = table(pred.knn, test.Y)
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else {
    0  # If the indices are missing, default to 0
  }
  FNR[1,4] = false_negatives / sum(test.Y == 1)
  # overall error rate
  ERR[1,4] = mean(pred.knn != test.Y)
  conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["1","1"]
  } else {
    0
  }
  conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1","0"]
  } else {
    0
  }
  PRO[1,4] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
  # Random forest
  fit.rf = randomForest::randomForest(prob_nonzero~., data = train_valid, ntree = 1000)
  pred.rf = predict(fit.rf, test)
  conf_matrix = table(pred.rf, test$prob_nonzero)
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else {
    0  # If the indices are missing, default to 0
  }
  FNR[1,5] = false_negatives / sum(test$prob_nonzero == 1)
  # overall error rate
  ERR[1,5] = mean(pred.rf != test$prob_nonzero)
  conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["1","1"]
  } else {
    0
  }
  conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1","0"]
  } else {
    0
  }
  PRO[1,5] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
  # XGboost
  eta_cand = c(0.005, 0.01, 0.05)
  max_depth_cand = c(5, 6, 7, 8)
  threshold_cand = c(0.2, 0.3, 0.4)
  X_train = data.matrix(train_valid[,1:2])
  y_train = train_valid$prob_nonzero
  X_test = data.matrix(test[,1:2])
  y_test = test$prob_nonzero
  # Convert factor to numeric, ensuring 0 and 1 values are preserved
  y_train = as.numeric(as.character(y_train))
  y_test = as.numeric(as.character(y_test))
  # convert the train and test data into xgboost matrix type.
  xgboost_train = xgboost::xgb.DMatrix(data=X_train, label=y_train)
  xgboost_test = xgboost::xgb.DMatrix(data=X_test, label=y_test)
  par <- list(
    objective = "binary:logistic", # Outputs probabilities for binary classification
    max_depth = max_depth_cand[2],
    eta = eta_cand[3]
  )
  fit.xgb = xgboost::xgb.train(data = xgboost_train, 
                   params = par,                           # max depth 
                   nrounds = 4000)                         # max number of boosting iterations
  pred = predict(fit.xgb, xgboost_test)
  pred.xgb = as.numeric(pred > threshold_cand[2])
  conf_matrix = table(pred.xgb, test$prob_nonzero)
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else {
    0  # If the indices are missing, default to 0
  }
  FNR[1,6] = false_negatives / sum(test$prob_nonzero == 1)
  # overall error rate
  ERR[1,6] = mean(pred.xgb != test$prob_nonzero)
  conf_matrix_11 = if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["1","1"]
  } else {
    0
  }
  conf_matrix_10 = if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
    conf_matrix["1","0"]
  } else {
    0
  }
  PRO[1,6] = conf_matrix_11 / (conf_matrix_10 + conf_matrix_11)
  
  result = list(FNR = FNR, ERR = ERR, PRO = PRO)
  combined_df <- do.call(cbind, result)
  return(combined_df)
}

# Parallel Computing
set.seed(5)
N = 500
num_core = detectCores() # 8 cores
cl = makeCluster(num_core)
registerDoSNOW(cl)
RES = foreach(j = 1:N,.combine = "rbind") %dopar% loop()
stopCluster(cl)

Naive_FNR = mean(RES[, 7])
Naive_Err = mean(RES[, 14])
Naive_PRO = mean(RES[, 21])

# Combine the matrices into a data frame
df <- data.frame(
  Logistic = RES[, 1], LDA = RES[, 2], QDA = RES[, 3], KNN = RES[, 4], RF = RES[, 5], XGB = RES[, 6]
)
df$Metric <- "False negative rate"

# Repeat for ERR and PRO
df_err <- data.frame(
  Logistic = RES[, 8], LDA = RES[, 9], QDA = RES[, 10], KNN = RES[, 11], RF = RES[, 12], XGB = RES[, 13]
)
df_err$Metric <- "Error rate"

df_pro <- data.frame(
  Logistic = RES[, 15], LDA = RES[, 16], QDA = RES[, 17], KNN = RES[, 18], RF = RES[, 19], XGB = RES[, 20]
)
df_pro$Metric <- "Proportion of earthquake occurrence"

# Combine all data frames into one
df_combined <- bind_rows(df, df_err, df_pro)

# Reshape data to long format
df_long <- df_combined %>%
  pivot_longer(cols = -Metric, names_to = "Model", values_to = "Value")

# write to csv
write_csv(df_long, "/Users/sunpierce/Desktop/Model_2.csv")

# read csv
df_long = read.csv("/Users/sunpierce/Desktop/Model_2.csv")

# Calculate mean for each Metric and Model
means <- df_long %>%
  group_by(Metric, Model) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")

naive_values <- data.frame(
  Metric = c("False negative rate", "Error rate", "Proportion of earthquake occurrence"), 
  Value = c(Naive_FNR, Naive_Err, Naive_PRO)
)

# Create the violin plots
ggplot(df_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_violin(trim = FALSE, show.legend = F, bounds = c(0,1)) +
  geom_point(data = means, aes(x = Model, y = Mean, color = "Mean"), size = 3, shape = 16) +
  geom_text(
    data = means,
    aes(x = Model, y = Mean, label = round(Mean, 3)), # Round mean values to 2 decimal places
    color = "deeppink2", size = 3.5, vjust = -0.1, 
    hjust = -0.3, fontface = "bold"
  ) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("aliceblue", "aliceblue", "aliceblue")) + 
  labs(y = "Rate (mean over 500 SPlit subsampling splits)") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.5, "cm")
  ) +
  # Add the horizontal lines for Naive model values with color and label for legend
  geom_hline(data = data.frame(Metric = c("False negative rate", "Error rate", "Proportion of earthquake occurrence"),
                               Value = c(Naive_FNR, Naive_Err, Naive_PRO)),
             aes(yintercept = Value, color = "Baseline Performance"), linetype = "dashed", size = 1) +
  geom_text(
    data = naive_values,
    aes(x = 1.5, y = Value, label = round(Value, 3)),
    color = "darkorchid3", size = 4, fontface = "bold", hjust = 2, vjust = -1, na.rm = T
  ) +
  scale_color_manual(
    values = c("Mean" = "deeppink2", "Baseline Performance" = "darkorchid3"),
    name = ""
  ) +
  guides(fill = "none")+
  #scale_y_continuous(limits = c(0, 1)) +
  geom_boxplot(aes(x = Model, y = Value), 
               width = 0.2, 
               color = "green4", 
               alpha = 0, 
               outlier.size = 0, size = 0.6)


#### Third Layer: Prediction of earthquake probability only for those classified as having earthquake ####
#### Third Layer - 1: Hyperparameters tuning ####
# Hyperparameter tuning for GB
set.seed(5)
N = 200
ntree = seq(100, 1000, by = 100)
interact = seq(1, 5, by = 1)
MSE = matrix(0, N, 50)
id = 1
for (i in 1:N) {
  first.idx = SPlit(dat, splitRatio = 0.4)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit(first, splitRatio = 0.2)
  train = first[-valid.idx,]
  validation = first[valid.idx,]
  # Mean baseline model
  train_valid = rbind(train, validation)
  # RMSE[i,1] = sqrt(mean((mean(train_valid$probability)-test$probability)^2))
  # over-sampling using SMOTE on training data only
  genData = SMOTE(train[,c("center", "neighbor")], train[,c("prob_nonzero")])
  train.balanced = genData$data
  train.balanced$prob_nonzero = as.factor(train.balanced$class)
  train = train.balanced[,c("center", "neighbor", "prob_nonzero")]
  validation = validation[,c("center", "neighbor", "prob_nonzero")]
  train_valid = rbind(train, validation)
  train_valid.X = train_valid[,c("center", "neighbor")]
  train_valid.Y = train_valid$prob_nonzero
  test.X = test[,c("center", "neighbor")]
  test.Y = test$prob_nonzero
  pred.knn = knn(train_valid.X, test.X, train_valid.Y, k = 11)
  idx = as.logical(as.integer(as.character(pred.knn)))
  dat.baseline = test
  dat.first = test[!idx,] # no adjustment can be made in the second layer
  dat.first$predict = rep(0, nrow(dat.first))
  # second part
  dat.second = test[idx,]
  # table(dat.second$prob_nonzero)
  # Second layer model
  # SPlit for train, test
  train.idx = SPlit(dat.second, splitRatio = 0.6)
  train = dat.second[-train.idx,]
  test = dat.second[train.idx,]
  for (j in ntree) {
    for (k in interact) {
      fit.gbm = gbm(probability~center+neighbor, data = train, distribution = "gaussian", 
                    n.trees = j, interaction.depth = k)
      pred.gbm = predict(fit.gbm, newdata = test, n.trees = j)
      pred.gbm = pmin(pmax(pred.gbm, 0), 1)  # Clip to [0, 1]
      MSE[i,id] = mean((pred.gbm - test$probability)^2)
      if (id + 1 == 51) {
        id = 1
      } else {
        id = id + 1
      }
    }
  }
}
mean_MSE = colMeans(MSE)
ts.plot(mean_MSE)
which.min(mean_MSE)
# Hyperparameter tuning for randomForest
set.seed(5)
N = 200
ntree = seq(1000, 5000, by = 1000)
nodesize = seq(1, 10, by = 1)
MSE = matrix(0, N, 50)
id = 1
for (i in 1:N) {
  first.idx = SPlit(dat, splitRatio = 0.4)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit(first, splitRatio = 0.2)
  train = first[-valid.idx,]
  validation = first[valid.idx,]
  # Mean baseline model
  train_valid = rbind(train, validation)
  # RMSE[i,1] = sqrt(mean((mean(train_valid$probability)-test$probability)^2))
  # over-sampling using SMOTE on training data only
  genData = SMOTE(train[,c("center", "neighbor")], train[,c("prob_nonzero")])
  train.balanced = genData$data
  train.balanced$prob_nonzero = as.factor(train.balanced$class)
  train = train.balanced[,c("center", "neighbor", "prob_nonzero")]
  validation = validation[,c("center", "neighbor", "prob_nonzero")]
  train_valid = rbind(train, validation)
  train_valid.X = train_valid[,c("center", "neighbor")]
  train_valid.Y = train_valid$prob_nonzero
  test.X = test[,c("center", "neighbor")]
  test.Y = test$prob_nonzero
  pred.knn = knn(train_valid.X, test.X, train_valid.Y, k = 11)
  idx = as.logical(as.integer(as.character(pred.knn)))
  dat.baseline = test
  dat.first = test[!idx,] # no adjustment can be made in the second layer
  dat.first$predict = rep(0, nrow(dat.first))
  # second part
  dat.second = test[idx,]
  # table(dat.second$prob_nonzero)
  # Second layer model
  # SPlit for train, test
  train.idx = SPlit(dat.second, splitRatio = 0.6)
  train = dat.second[-train.idx,]
  test = dat.second[train.idx,]
  for (j in ntree) {
    for (k in nodesize) {
      fit.rf = randomForest(probability~neighbor+center, data = train, ntree = j, nodesize = k)
      pred.rf = predict(fit.rf, test)
      pred.rf = pmin(pmax(pred.rf, 0), 1)  # Clip to [0, 1]
      MSE[i,id] = mean((pred.rf - test$probability)^2)
      if (id + 1 == 51) {
        id = 1
      } else {
        id = id + 1
      }
    }
  }
}
mean_MSE_rf = colMeans(MSE)
ts.plot(mean_MSE_rf)
which.min(mean_MSE_rf)

# Modeling
set.seed(5)
N = 300
RMSE = matrix(0, N, 6) # 6 indicates the number of candidate models
ACCU = matrix(0, N, 6) # accuracy
ERROR = matrix(0, N, 4) # false negative rate exclude baseline
for (i in 1:N) {
  first.idx = SPlit(dat, splitRatio = 0.4)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit(first, splitRatio = 0.2)
  train = first[-valid.idx,]
  validation = first[valid.idx,]
  # over-sampling using SMOTE on training data only
  genData = SMOTE(train[,c("center", "neighbor")], train[,c("prob_nonzero")])
  train.balanced = genData$data
  train.balanced$prob_nonzero = as.factor(train.balanced$class)
  train = train.balanced[,c("center", "neighbor", "prob_nonzero")]
  validation = validation[,c("center", "neighbor", "prob_nonzero")]
  train_valid = rbind(train, validation)
  train_valid.X = train_valid[,c("center", "neighbor")]
  train_valid.Y = train_valid$prob_nonzero
  test.X = test[,c("center", "neighbor")]
  test.Y = test$prob_nonzero
  pred.knn = knn(train_valid.X, test.X, train_valid.Y, k = 11)
  idx = as.logical(as.integer(as.character(pred.knn)))
  dat.baseline = test
  dat.first = test[!idx,] # no adjustment can be made in the second layer
  dat.first$predict = rep(0, nrow(dat.first))
  # second layer
  dat.second = test[idx,]
  # table(dat.second$prob_nonzero)
  # Second layer model
  # SPlit for train, test
  train.idx = SPlit(dat.second, splitRatio = 0.6)
  train = dat.second[-train.idx,]
  test = dat.second[train.idx,]
  train.idx = SPlit(dat.baseline, splitRatio = 0.6)
  train.baseline = dat.baseline[-train.idx,]
  test.baseline = dat.baseline[train.idx,]
  ## Baseline-1
  RMSE[i,1] = sqrt(mean((mean(dat[-first.idx,"probability"])-dat[first.idx,"probability"])^2))
  ACCU[i,1] = 0 # since the predicted value are all nonzero!!!
  ## Baseline-2
  RMSE[i,2] = sqrt((sum((mean(train.baseline$probability)-test.baseline$probability)^2) + sum((dat.first$probability - dat.first$predict)^2))/(nrow(test.baseline)+nrow(dat.first)))
  same_zero_count = 0
  test_zero_count = sum(test.baseline$probability == 0)
  same_zero_count_first = sum(dat.first$probability == 0 & dat.first$predict == 0)
  real_zero_count = sum(dat.first$probability == 0)
  ACCU[i,2] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  ## Gaussian process
  fit.gp = fit_model(train$probability, train[,c("center", "neighbor")], covfun_name="matern15_isotropic", silent = T)
  pred.gp = predictions(fit.gp, locs_pred = test[,c("center", "neighbor")], X_pred = rep(1,nrow(test)))
  pred.gp = pmin(pmax(pred.gp, 0), 1)  # Clip to [0, 1] for those less than 0 set it to 0
  RMSE[i,3] = sqrt((sum((dat.first$probability - dat.first$predict)^2) + sum((test$probability - pred.gp)^2)) / (nrow(dat.first) + nrow(test)))
  same_zero_count = sum(test$probability == 0 & pred.gp == 0)
  test_zero_count = sum(test$probability == 0)
  same_zero_count_first = sum(dat.first$probability == 0 & dat.first$predict == 0)
  real_zero_count = sum(dat.first$probability == 0)
  ACCU[i,3] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  same_zero_count = sum(test$probability != 0 & pred.gp == 0)
  test_zero_count = sum(pred.gp == 0)
  same_zero_count_first = sum(dat.first$probability != 0)
  real_zero_count = length(dat.first$predict)
  ERROR[i,1] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  ## Random forest
  fit.rf = randomForest(probability~neighbor+center, data = train, ntree = 2000, nodesize = 2)
  pred.rf = predict(fit.rf, test)
  pred.rf = pmin(pmax(pred.rf, 0), 1)
  RMSE[i,4] = sqrt((sum((dat.first$probability - dat.first$predict)^2) + sum((test$probability - pred.rf)^2)) / (nrow(dat.first) + nrow(test)))
  same_zero_count = sum(test$probability == 0 & pred.rf == 0)
  test_zero_count = sum(test$probability == 0)
  same_zero_count_first = sum(dat.first$probability == 0 & dat.first$predict == 0)
  real_zero_count = sum(dat.first$probability == 0)
  ACCU[i,4] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  same_zero_count = sum(test$probability != 0 & pred.rf == 0)
  test_zero_count = sum(pred.rf == 0)
  same_zero_count_first = sum(dat.first$probability != 0)
  real_zero_count = length(dat.first$predict)
  ERROR[i,2] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  ## Classification tree
  fit.tree = tree(probability~neighbor+center, data = train)
  # tree.cv = cv.tree(fit.tree)
  # optimal = tree.cv$size[which.min(tree.cv$dev)]
  # fit.pruned = prune.tree(fit.tree, best = optimal)
  # pred.tree = predict(fit.pruned, newdata = test)
  pred.tree = predict(fit.tree, newdata = test)
  pred.tree = pmin(pmax(pred.tree, 0), 1)  # Clip to [0, 1]
  RMSE[i,5] = sqrt((sum((dat.first$probability - dat.first$predict)^2) + sum((test$probability - pred.tree)^2)) / (nrow(dat.first) + nrow(test)))
  same_zero_count = sum(test$probability == 0 & pred.tree == 0)
  test_zero_count = sum(test$probability == 0)
  same_zero_count_first = sum(dat.first$probability == 0 & dat.first$predict == 0)
  real_zero_count = sum(dat.first$probability == 0)
  ACCU[i,5] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  same_zero_count = sum(test$probability != 0 & pred.tree == 0)
  test_zero_count = sum(pred.tree == 0)
  same_zero_count_first = sum(dat.first$probability != 0)
  real_zero_count = length(dat.first$predict)
  ERROR[i,3] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  ## Gradient boosting
  fit.gbm = gbm(probability~center+neighbor, data = train, distribution = "gaussian", 
                n.trees = 100, interaction.depth = 4)
  pred.gbm = predict(fit.gbm, newdata = test, n.trees = 100)
  pred.gbm = pmin(pmax(pred.gbm, 0), 1)  # Clip to [0, 1]
  RMSE[i,6] = sqrt((sum((dat.first$probability - dat.first$predict)^2) + sum((test$probability - pred.gbm)^2)) / (nrow(dat.first) + nrow(test)))
  same_zero_count = sum(test$probability == 0 & pred.gbm == 0)
  test_zero_count = sum(test$probability == 0)
  same_zero_count_first = sum(dat.first$probability == 0 & dat.first$predict == 0)
  real_zero_count = sum(dat.first$probability == 0)
  ACCU[i,6] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  same_zero_count = sum(test$probability != 0 & pred.gbm == 0)
  test_zero_count = sum(pred.gbm == 0)
  same_zero_count_first = sum(dat.first$probability != 0)
  real_zero_count = length(dat.first$predict)
  ERROR[i,4] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
}
par(mfrow = c(1,1), cex = 1.4, mar = c(2.7,2.6,2.6,0.5), mgp = c(1.6,.6,0))
vioplot(RMSE, main = "RMSE (test)", col = "azure3", names = c("Baseline model-1", "Baseline model-2", "Gaussian Process", "Random Forest", "Regression Tree", "Gradient Boosting"))
colMeans(RMSE)
par(mfrow = c(1,1), cex = 1.4, mar = c(2.7,2.6,2.6,0.5), mgp = c(1.6,.6,0))
vioplot(ACCU, main = "Accuracy (test)", col = "azure3", names = c("Baseline model-1", "Baseline model-2", "Gaussian Process", "Random Forest", "Regression Tree", "Gradient Boosting"))
colMeans(ACCU)
par(mfrow = c(1,1), cex = 1.4, mar = c(2.7,2.6,2.6,0.5), mgp = c(1.6,.6,0))
vioplot(ERROR, main = "Error rate (test)", col = "azure3", names = c("Gaussian Process", "Random Forest", "Regression Tree", "Gradient Boosting"))
colMeans(ERROR)
