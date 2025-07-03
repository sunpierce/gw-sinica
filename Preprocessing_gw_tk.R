library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)

#### Combine all txt files into one csv file (combined_water_level_data) ####
# Set the folder path where your txt files are located
# all txt files in gw_data_ver.1 already replaced by NA if missing
folder_path <- "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Data/gw_data_ver.2/gw_tk_processed" 

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
combined_water_level_data[, 5:51] <- apply(combined_water_level_data[, 5:51], 2, function(x) x - mean(x, na.rm = TRUE))
write_csv(combined_water_level_data, "combined_water_level_data.csv")

#### Triangle processing for hourly data (triangle_water_level_data) ####
triangle_df <- read.csv("Triangle_ver.2.csv") # mark the location for each triangle
water_level_df <- read.csv("combined_water_level_data.csv") # on an hourly basis

# Transform the format
df <- triangle_df %>%
  mutate(
    v1_ID = as.character(v1_ID),  # Convert v1_ID to character
    v1_ID = case_when(
      str_detect(v1_ID, "^11\\d{6}$") ~ str_replace(v1_ID, "^11", "X11"),
      str_detect(v1_ID, "^12\\d{6}$") ~ str_replace(v1_ID, "^12", "X12"),
      str_detect(v1_ID, "^21\\d{6}$") ~ str_replace(v1_ID, "^21", "X21"),
      str_detect(v1_ID, "^64\\d{6}$") ~ str_replace(v1_ID, "^64", "X64"),
      TRUE ~ v1_ID
    )
  )
df <- df %>%
  mutate(
    v2_ID = as.character(v2_ID),
    v2_ID = case_when(
      str_detect(v2_ID, "^11\\d{6}$") ~ str_replace(v2_ID, "^11", "X11"),
      str_detect(v2_ID, "^12\\d{6}$") ~ str_replace(v2_ID, "^12", "X12"),
      str_detect(v2_ID, "^21\\d{6}$") ~ str_replace(v2_ID, "^21", "X21"),
      str_detect(v2_ID, "^64\\d{6}$") ~ str_replace(v2_ID, "^64", "X64"),
      TRUE ~ v2_ID
    ),
    v3_ID = as.character(v3_ID),
    v3_ID = case_when(
      str_detect(v3_ID, "^11\\d{6}$") ~ str_replace(v3_ID, "^11", "X11"),
      str_detect(v3_ID, "^12\\d{6}$") ~ str_replace(v3_ID, "^12", "X12"),
      str_detect(v3_ID, "^21\\d{6}$") ~ str_replace(v3_ID, "^21", "X21"),
      str_detect(v3_ID, "^64\\d{6}$") ~ str_replace(v3_ID, "^64", "X64"),
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
write_csv(merge, "triangle_water_level_data.csv")


#### Triangle processing for hourly data for neighbor (neighbor_triangle_water_level_data) ####
tri = read.csv("averaged_neighbor_triangle_ver.2.csv")
dat = read.csv("triangle_water_level_data.csv")

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
write_csv(merge, "neighbor_triangle_water_level_data.csv")