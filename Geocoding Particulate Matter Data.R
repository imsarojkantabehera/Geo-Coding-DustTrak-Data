#### Code for Merging single gpx and csv files ####

# Load the libraries
{
  library(XML)
  library(sp)
  library(readr)
  library(dplyr)
  library(sf)
  library(lubridate)
}

# Read the GPX file (make sure to specify the correct layer)
GPS <- st_read("GPS.gpx", layer = "track_points")

# Extract and mutate to add latitude and longitude to gpx_data
GPS <- GPS %>%
  mutate(
    longitude = st_coordinates(geometry)[, 1],  # Extract longitude
    latitude = st_coordinates(geometry)[, 2],    # Extract latitude
    
    # Extract time in "HH:MM:SS" format
    hour = format(as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"),
    date = format(as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d"),
    
    # Convert time to seconds: num_time = hours*3600 + minutes*60 + seconds
    num_time = hour(as.POSIXct(hour, format="%H:%M:%S")) * 3600 +
      minute(as.POSIXct(hour, format="%H:%M:%S")) * 60 +
      second(as.POSIXct(hour, format="%H:%M:%S"))
  )


# Read the file, skipping the first 36 rows
DTT <- read.csv("DTT.csv", skip = 36) %>% 
  dplyr::rename(Elapsed_Time = 1, PM1 = 2, PM2.5 = 3, PM4 = 4, PM10 = 5, Total = 6)
DTT <- subset(DTT, select = -c(7, 8))

# Extract "Test Start Time" (assuming it's in HH:MM:SS format)

TST <- read.csv("DTT.csv")

# Get the Test Start Time from the 7th row and 2nd column
test_start_time <- TST[6, 2]

# Convert "Test Start Time" into total seconds (hours * 3600 + minutes * 60 + seconds)
time_parts <- unlist(strsplit(test_start_time, ":"))
num_time <- as.numeric(time_parts[1]) * 3600 + as.numeric(time_parts[2]) * 60 + as.numeric(time_parts[3])

# Add a new column "Num_Time" where the first value is "num_time"
# Then add (Elapsed Time - 1) to all rows
DTT <- DTT %>%
  mutate(num_time = num_time + Elapsed_Time - 1)

# Assuming DTT and GPS are your data frames and 'num_time' is the column to match
merged_data <- merge(DTT, GPS, by = "num_time", all = TRUE)

# Select only the specified columns
merged_data_subset <- merged_data[, c("num_time", "date", "hour", "PM1", "PM2.5", 
                                      "PM4", "PM10", "Total","geometry",
                                      "longitude","latitude")]
Final_Merged_Data <- na.omit(merged_data_subset)
Final_Merged_Data$geometry <- NULL  # Remove geometry column
Final_Merged_Data <- as.data.frame(Final_Merged_Data)
write.csv(Final_Merged_Data, "PM.csv", row.names = FALSE)