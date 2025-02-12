# Temperature vs activity

# Make extra weather days .Rdata
weather_extras <- fread(file.path("data","pika_activity","predict_score_weather_extras.csv"))
saveRDS(weather_extras,file.path("recognizer_outputs","predict_score_weather_extras.RData"))

# Merge all predictions into one mega-file


rds_folder <- "recognizer_outputs/predictions"

# Get the list of .RData files in the folder
rds_files <- list.files(rds_folder, pattern = "*.RData", full.names = TRUE)

all_data <- lapply(rds_files, function(rds_file) {
  readRDS(rds_file)  # Read the RDS file into R
})

# Combine all data.tables or data.frames into one (rbind all elements in the list)
merged_data <- do.call(rbind, all_data)

# Keep only data for PIKARU5, 10, 15, 25
filtered_pikaru <- merged_data[grepl("PIKARU5|PIKARU10|PIKARU15|PIKARU25", file)]


saveRDS(filtered_pikaru, file = "recognizer_outputs/predictions/all_sites_weather_predictions.RDS")

# Get the unique values in the 'file' column
unique_file_names <- unique(filtered_pikaru$file)

# Count the number of unique values
num_unique_file_names <- length(unique_file_names)

# Print the number of unique file names
print(num_unique_file_names)
