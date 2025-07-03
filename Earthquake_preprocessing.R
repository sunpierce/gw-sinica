library(dplyr)

# Read the CSV file into a data frame
df <- read.csv("Earthquake_catalog.csv")

filtered_df <- df %>%
  filter(
    ((Year > 2008 & Year < 2023) | 
       (Year == 2008 & Month >= 4) | 
       (Year == 2023 & Month <= 3)) &
      Magnitude >= 2.5
  )

is_point_in_triangle <- function(px, py, x1, y1, x2, y2, x3, y3) {
  # Compute barycentric coordinates
  denominator <- (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
  lambda1 <- ((y2 - y3) * (px - x3) + (x3 - x2) * (py - y3)) / denominator
  lambda2 <- ((y3 - y1) * (px - x3) + (x1 - x3) * (py - y3)) / denominator
  lambda3 <- 1 - lambda1 - lambda2

  # Check if the point is inside the triangle
  return(lambda1 >= 0 & lambda1 <= 1 & lambda2 >= 0 & lambda2 <= 1 & lambda3 >= 0 & lambda3 <= 1)
}

# Read earthquake data
earthquake_data <- filtered_df

# Read triangle data (replace 'Triangle.csv' with your file path)
triangle_data <- read.csv("Triangle_ver.2.csv")

# Check if any earthquakes lie within any triangles
# Initialize a vector to store the triangle ID for each earthquake
tri_IDs <- rep(NA, nrow(earthquake_data))

# Loop over each earthquake
for (i in 1:nrow(earthquake_data)) {
  # Get the location of the earthquake (px,py)
  px <- earthquake_data$Longitude[i]
  py <- earthquake_data$Latitude[i]
  
  # Loop over each triangle
  for (j in 1:nrow(triangle_data)) {
    # Extract vertex indices for the current triangle
    v1 <- triangle_data$v1_ID[j]
    v2 <- triangle_data$v2_ID[j]
    v3 <- triangle_data$v3_ID[j]
    
    # Get the coordinates of the vertices
    x1 = triangle_data$v1_x[j]
    y1 = triangle_data$v1_y[j]
    x2 = triangle_data$v2_x[j]
    y2 = triangle_data$v2_y[j]
    x3 = triangle_data$v3_x[j]
    y3 = triangle_data$v3_y[j]
    
    # Check if the point is inside the current triangle
    if ( is_point_in_triangle(px, py, x1, y1, x2, y2, x3, y3) ) {
      tri_IDs[i] <- j  # Store the triangle ID
      break  # Exit the loop if the earthquake is inside the triangle
    }
  }
}

# Add tri_ID column to the earthquake data
earthquake_data$tri_ID <- tri_IDs

# Filter out earthquakes that are not in any triangle
filtered_earthquake_data <- earthquake_data %>% filter(!is.na(tri_ID))


# Write the filtered data to a new CSV file
write.csv(filtered_earthquake_data, "Earthquake_data.csv", row.names = FALSE)
cat("Finished\n")

# Display the filtered data
print(filtered_earthquake_data)
