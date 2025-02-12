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

binary_pikaru <- filtered_pikaru[seq(1, .N, by = 2) & PIKA > 10]

processed_pikaru <- binary_pikaru[, .(
  site = ifelse(grepl("PIKARU5", file), "PC",
                ifelse(grepl("PIKARU10", file), "PP",
                       ifelse(grepl("PIKARU15", file), "MD",
                              ifelse(grepl("PIKARU25", file), "MB", NA)))),
  date = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\1", file))  # Extract date from file name
  , by = file]

site_date_count <- processed_pikaru[, .(count = .N), by = .(site, date)]

weather_data <- fread("data/pika_activity/weather_all_sites.csv")

# convert date to character
site_date_count$date <- as.character(site_date_count$date)
weather_data$date <- as.character(weather_data$date)

weather_vs_calls <- merge(site_date_count, weather_data, by = c("site","date"), all.x = TRUE)

