library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)

#### Step 1: Combine all txt files into one csv file (combined_water_level_data) ####
# Set the folder path where your txt files are located
# all txt files in gw_data_ver.1 already replaced by NA if missing
folder_path <- "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Data/gw_data_ver.1" 

# List all txt files in the specified folder
txt_files <- list.files(path = folder_path, pattern = "*.txt", full.names = TRUE)

# Initialize an empty data frame to store the combined data
all_data <- data.frame()

# Loop through each txt file
for (file in txt_files) {
  # Read the txt file, assuming the first three columns are Year, Month, and Day, 
  # followed by Hour1 to Hour24
  data <- read.table(file, header = FALSE, sep = "", col.names = c("Year", "Month", "Day", paste0("Hour", 1:24)))
  
  # Extract the filename to use as the station name
  station_name <- tools::file_path_sans_ext(basename(file))
  
  # Use the pivot_longer function from tidyr to transform data into a single column for each station
  tidy_data <- data %>%
    pivot_longer(
      cols = starts_with("Hour"),  # Select columns that start with "Hour"
      names_to = "Hour",           # Create a new column named "Hour"
      names_prefix = "Hour",       # Remove the prefix "Hour" from the column names
      values_to = station_name     # Name the value column using the station name
    ) %>%
    mutate(
      Hour = as.integer(Hour),     # Convert Hour from character to integer
      !!station_name := na_if(!!sym(station_name), -999998)  # Replace -999998 with NA
    ) %>%
    select(Year, Month, Day, Hour, !!station_name)  # Select and rearrange columns
  
  # Ensure there are no duplicates for the same timestamp
  tidy_data <- tidy_data %>%
    distinct(Year, Month, Day, Hour, .keep_all = TRUE)
  
  # If all_data is empty, assign tidy_data to all_data
  if (nrow(all_data) == 0) {
    all_data <- tidy_data
  } else {
    # Merge tidy_data into all_data, joining by Year, Month, Day, Hour
    all_data <- full_join(all_data, tidy_data, by = c("Year", "Month", "Day", "Hour"), relationship = "many-to-many")
  }
}

# Combine successful
combined_water_level_data = all_data
str(combined_water_level_data)

# Demean
combined_water_level_data[, 5:61] <- apply(combined_water_level_data[, 5:61], 2, function(x) x - mean(x, na.rm = TRUE))
write_csv(combined_water_level_data, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/combined_water_level_data.csv")


#### Step 2: HourBlock conversion (averaged_water_level_data) ####
combined_water_level_data = read_csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/combined_water_level_data.csv")
# Function to convert Hour to HourBlock
hour_to_block <- function(hour) {
  if (hour >= 1 & hour <= 6) {
    return(1)
  } else if (hour >= 7 & hour <= 12) {
    return(2)
  } else if (hour >= 13 & hour <= 18) {
    return(3)
  } else {
    return(4)
  }
}

# Apply the hour_to_block function and add HourBlock column
water_level_data_2 <- combined_water_level_data %>%
  mutate(HourBlock = sapply(Hour, hour_to_block)) %>%
  select(-Hour) # Remove the original Hour column

# Reordering
water_level_data_3 <- water_level_data_2 %>%
  select(1:3, last_col(), 4:60)

# Calculate the average water levels for each station by Year, Month, Day, and HourBlock
averaged_data <- water_level_data_3 %>%
  group_by(across(1:4)) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop')

# Write to csv
write_csv(averaged_data, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/averaged_water_level_data.csv")


#### Step 3: Triangle processing for hourly data (triangle_water_level_data) ####
triangle_df <- read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Data/Triangle.csv") # mark the location for each triangle
water_level_df <- read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/combined_water_level_data.csv") # on an hourly basis

# Transform the format
df <- triangle_df %>%
  mutate(
    v1_ID = as.character(v1_ID),  # Convert v1_ID to character
    v1_ID = case_when(
      str_detect(v1_ID, "^7\\d{6}$") ~ str_replace(v1_ID, "^7", "X07"),
      str_detect(v1_ID, "^9\\d{6}$") ~ str_replace(v1_ID, "^9", "X09"),
      str_detect(v1_ID, "^10\\d{6}$") ~ str_replace(v1_ID, "^10", "X10"),
      TRUE ~ v1_ID
    )
  )
df <- df %>%
  mutate(
    v2_ID = as.character(v2_ID),
    v2_ID = case_when(
      str_detect(v2_ID, "^7\\d{6}$") ~ str_replace(v2_ID, "^7", "X07"),
      str_detect(v2_ID, "^9\\d{6}$") ~ str_replace(v2_ID, "^9", "X09"),
      str_detect(v2_ID, "^10\\d{6}$") ~ str_replace(v2_ID, "^10", "X10"),
      TRUE ~ v2_ID
    ),
    v3_ID = as.character(v3_ID),
    v3_ID = case_when(
      str_detect(v3_ID, "^7\\d{6}$") ~ str_replace(v3_ID, "^7", "X07"),
      str_detect(v3_ID, "^9\\d{6}$") ~ str_replace(v3_ID, "^9", "X09"),
      str_detect(v3_ID, "^10\\d{6}$") ~ str_replace(v3_ID, "^10", "X10"),
      TRUE ~ v3_ID
    )
  )

# Start merging
merge = water_level_df %>%
  select(Year, Month, Day, Hour)

for (i in 1:nrow(triangle_df)) { # process every row in triangle_df
  station1 = df$v1_ID[i]
  station2 = df$v2_ID[i]
  station3 = df$v3_ID[i]
  sum_temp = (water_level_df[,station1] + water_level_df[,station2] + water_level_df[,station3])/3
  tri_id_col <- paste0("Tri_ID", i)
  merge[[tri_id_col]] = sum_temp
}

# Write to csv
write_csv(merge, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/triangle_water_level_data.csv")


#### Step 4: Triangle processing for HourBlock data ####
triangle_df <- read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Data/Triangle.csv")
water_level_df <- read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/averaged_water_level_data.csv")

# Transform the format
df <- triangle_df %>%
  mutate(
    v1_ID = as.character(v1_ID),  # Convert v1_ID to character
    v1_ID = case_when(
      str_detect(v1_ID, "^7\\d{6}$") ~ str_replace(v1_ID, "^7", "X07"),
      str_detect(v1_ID, "^9\\d{6}$") ~ str_replace(v1_ID, "^9", "X09"),
      str_detect(v1_ID, "^10\\d{6}$") ~ str_replace(v1_ID, "^10", "X10"),
      TRUE ~ v1_ID
    )
  )
df <- df %>%
  mutate(
    v2_ID = as.character(v2_ID),
    v2_ID = case_when(
      str_detect(v2_ID, "^7\\d{6}$") ~ str_replace(v2_ID, "^7", "X07"),
      str_detect(v2_ID, "^9\\d{6}$") ~ str_replace(v2_ID, "^9", "X09"),
      str_detect(v2_ID, "^10\\d{6}$") ~ str_replace(v2_ID, "^10", "X10"),
      TRUE ~ v2_ID
    ),
    v3_ID = as.character(v3_ID),
    v3_ID = case_when(
      str_detect(v3_ID, "^7\\d{6}$") ~ str_replace(v3_ID, "^7", "X07"),
      str_detect(v3_ID, "^9\\d{6}$") ~ str_replace(v3_ID, "^9", "X09"),
      str_detect(v3_ID, "^10\\d{6}$") ~ str_replace(v3_ID, "^10", "X10"),
      TRUE ~ v3_ID
    )
  )

# Start merging
merge = water_level_df %>%
  select(Year, Month, Day, HourBlock)

for (i in 1:nrow(triangle_df)) { # process every row in triangle_df
  station1 = df$v1_ID[i]
  station2 = df$v2_ID[i]
  station3 = df$v3_ID[i]
  sum_temp = (water_level_df[,station1] + water_level_df[,station2] + water_level_df[,station3])/3
  tri_id_col <- paste0("Tri_ID", i)
  merge[[tri_id_col]] = sum_temp
}

# write csv
write_csv(merge, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/averaged_triangle_water_level_data.csv")


#### Step 5: Triangle processing for HourBlock data for neighbor ####
tri = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/averaged_neighbor_triangle.csv")
dat = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/averaged_triangle_water_level_data.csv")

# Rename column
for (i in 5:ncol(dat)) colnames(dat)[i] = i - 4

# process
merge = dat[,1:4]
for (i in 1:nrow(tri)) {
  if ( is.na(tri[i,"nb2_ID"]) ) {
    num = tri[i,"nb1_ID"]+4
    temp = dat[,num]
  } else if ( is.na(tri[i,"nb3_ID"]) ) {
    num = tri[i,"nb1_ID"]+4
    num2 = tri[i,"nb2_ID"]+4
    temp = (dat[,num] + dat[,num2]) / 2
  } else {
    num = tri[i,"nb1_ID"]+4
    num2 = tri[i,"nb2_ID"]+4
    num3 = tri[i,"nb3_ID"]+4
    temp = (dat[,num] + dat[,num2] + dat[,num3]) / 3
  }
  tri_id_col <- paste0("Tri_ID_nb", i)
  merge[[tri_id_col]] = temp
}

# write to csv file
write_csv(merge, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/neighbor_triangle_averaged_water_level_data.csv")


#### Step 6: Calculating occurrences for each combination (denominator) ####
# Read the data
center = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/averaged_triangle_water_level_data.csv")
neighbor = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/neighbor_triangle_averaged_water_level_data.csv")

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
  lapply(1:((ncol(new_df) - 4) / 2), function(i) c(paste0("t", i), paste0("n", i)))
)

# Display the new data frame
new_df

# Define the range of interest (you can adjust this range as needed)
min_val <- floor(min(new_df[, 5:ncol(new_df)], na.rm = TRUE)) # Minimum value in the data
max_val <- ceiling(max(new_df[, 5:ncol(new_df)], na.rm = TRUE)) # Maximum value in the data

# Generate intervals of length 1
ranges <- lapply(seq(min_val, max_val - 1), function(x) c(x, x + 1))

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
  pairs <- sapply(1:88, function(j) {
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


#### Step 7: Lag 1 ####
# Read data
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/earthquake_in_triangle_2.5_1e-3.csv")

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


#### Step 8: Lag 2 ####
# Read data
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/earthquake_in_triangle_2.5_1e-3.csv")

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


#### Step 9: Lag 3 ####
# Read data
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/earthquake_in_triangle_2.5_1e-3.csv")

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






#### Step 10: Lag 4 ####
# Read data
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/earthquake_in_triangle_2.5_1e-3.csv")

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


#### Step 11: Lag 5 ####
# Read data
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/earthquake_in_triangle_2.5_1e-3.csv")

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

#### Step 12: Lag 6 ####
# Read data
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/earthquake_in_triangle_2.5_1e-3.csv")

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


#### Step 13: Lag 7 ####
# Read data
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/earthquake_in_triangle_2.5_1e-3.csv")

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


#### Step 14: Lag 8 ####
# Read data
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/earthquake_in_triangle_2.5_1e-3.csv")

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


#### Step 15: Lag 9 ####
# Read data
earthquake = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Ver.1.1/DataTemp/earthquake_in_triangle_2.5_1e-3.csv")

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


#### Step 16: Union occurrences ####
# Step 1: Union lag1 and lag2
union_lag1_lag2 <- sorted_combination_freq_lag1 %>%
  full_join(sorted_combination_freq_lag2, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  select(combination, frequency)

# Step 2: Union the result with lag3
union_lag1_lag3 <- union_lag1_lag2 %>%
  full_join(sorted_combination_freq_lag3, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  select(combination, frequency)

# Step 3: Union the result with lag4
union_lag1_lag4 <- union_lag1_lag3 %>%
  full_join(sorted_combination_freq_lag4, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  select(combination, frequency)

# Step 4: Union the result with lag5
union_lag1_lag5 <- union_lag1_lag4 %>%
  full_join(sorted_combination_freq_lag5, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  select(combination, frequency)

# Step 5: Union the result with lag 6
union_lag1_lag6 = union_lag1_lag5 %>%
  full_join(sorted_combination_freq_lag6, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  select(combination, frequency)

# Step 6: Union the result with lag 7
union_lag1_lag7 = union_lag1_lag6 %>%
  full_join(sorted_combination_freq_lag7, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  select(combination, frequency)

# Step 7: Union the result with lag 8
union_lag1_lag8 = union_lag1_lag7 %>%
  full_join(sorted_combination_freq_lag8, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  select(combination, frequency)

# Step 8: Union the result with lag 9
union_lag1_lag9 = union_lag1_lag8 %>%
  full_join(sorted_combination_freq_lag9, by = "combination") %>%
  mutate(frequency = coalesce(frequency.x, 0) + coalesce(frequency.y, 0)) %>%
  select(combination, frequency)

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

#### Step 12: Probability ####
# Function to calculate probabilities
calculate_probabilities <- function(union_df, whole_df) {
  prob_df <- whole_df %>%
    left_join(union_df, by = "combination") %>%
    mutate(probability = coalesce(frequency.y, 0) / frequency.x) %>%
    select(combination, probability)
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

# Write to csv
df <- prob_lag1_lag3[-nrow(prob_lag1_lag3),]
df <- df %>%
  separate(combination, into = c("center", "neighbor"), sep = "_")
write_csv(df, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag3.csv")
df <- prob_lag1_lag5[-nrow(prob_lag1_lag5),]
df <- df %>%
  separate(combination, into = c("center", "neighbor"), sep = "_")
write_csv(df, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag5.csv")
df <- prob_lag1_lag7[-nrow(prob_lag1_lag7),]
df <- df %>%
  separate(combination, into = c("center", "neighbor"), sep = "_")
write_csv(df, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag7.csv")
df <- prob_lag1_lag9[-nrow(prob_lag1_lag9),]
df <- df %>%
  separate(combination, into = c("center", "neighbor"), sep = "_")
write_csv(df, "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag9.csv")


#### Demo ####
df1 = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag3.csv")
df2 = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag5.csv")
df3 = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag7.csv")
df4 = read.csv("/Users/sunpierce/Desktop/Academia Sinica/gw_project/Prob_cyc_lag1_lag9.csv")

head(df1[order(df1$probability, decreasing = T),])
head(df2[order(df2$probability, decreasing = T),])
head(df3[order(df3$probability, decreasing = T),])
head(df4[order(df4$probability, decreasing = T),])

# Labeling
df1 <- df1 %>%
  mutate(prob_nonzero = ifelse(probability > 0, 1, 0))
df1$prob_nonzero = as.factor(df1$prob_nonzero)
freq_table = table(df1$prob_nonzero)
prop_table = prop.table(freq_table)
barplot(prop_table, ylim = c(0,1), main = "Distribution of prob_nonzero", xlab = "prob_nonzero",
        ylab = "proportion")
df2 <- df2 %>%
  mutate(prob_nonzero = ifelse(probability > 0, 1, 0))
df2$prob_nonzero = as.factor(df2$prob_nonzero)
freq_table = table(df2$prob_nonzero)
prop_table = prop.table(freq_table)
barplot(prop_table, ylim = c(0,1), main = "Distribution of prob_nonzero", xlab = "prob_nonzero",
        ylab = "proportion")
df3 <- df3 %>%
  mutate(prob_nonzero = ifelse(probability > 0, 1, 0))
df3$prob_nonzero = as.factor(df3$prob_nonzero)
freq_table = table(df3$prob_nonzero)
prop_table = prop.table(freq_table)
barplot(prop_table, ylim = c(0,1), main = "Distribution of prob_nonzero", xlab = "prob_nonzero",
        ylab = "proportion")
df4 <- df4 %>%
  mutate(prob_nonzero = ifelse(probability > 0, 1, 0))
df4$prob_nonzero = as.factor(df4$prob_nonzero)
freq_table = table(df4$prob_nonzero)
prop_table = prop.table(freq_table)
barplot(prop_table, ylim = c(0,1), main = "Distribution of prob_nonzero", xlab = "prob_nonzero",
        ylab = "proportion")