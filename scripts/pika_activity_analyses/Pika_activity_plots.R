# Graphing pika activity over the day and in relation to solar power

install.packages("data.table")
install.packages("purrr")
library(ggplot2)
library(tidyverse)
library(data.table)
library(purrr)
library(gridExtra)

# Set the path to your folder containing CSV files
csv_folder <- "C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/complete_predictions_csv"
output_folder <- "C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/complete_predictions"

# Get the list of all CSV files in the folder
csv_files <- list.files(csv_folder, pattern = "\\.csv$", full.names = TRUE)

# Process and save CSV files as RDS using lapply
lapply(csv_files, function(csv_file) {
  tryCatch({
    # Read the CSV file
    predictions_csv <- fread(csv_file)
    
    # Create a dynamic filename for the RDS file based on the CSV filename
    file_name <- tools::file_path_sans_ext(basename(csv_file))  # remove the extension
    output_file <- file.path(output_folder, paste0(file_name, ".RData"))
    
    # Save the data as an RDS file
    saveRDS(predictions_csv, output_file)
    
  }, error = function(e) {
    cat("Error with file:", csv_file, "\n")
    cat("Error message:", e$message, "\n")
  })
})

# Confirmation message
cat("All CSV files have been processed and saved as RData files.\n")

# Device color mappings
device_colors <- c("PIKARU1" = "deepskyblue", "PIKARU2" = "darksalmon", "PIKARU3" = "darkseagreen", 
                   "PIKARU4" = "darkmagenta", "PIKARU5" = "goldenrod3", "PIKARU6" = "deepskyblue", 
                   "PIKARU7" = "darksalmon", "PIKARU8" = "darkseagreen", "PIKARU9" = "darkmagenta", 
                   "PIKARU10" = "goldenrod3", "PIKARU11" = "deepskyblue", "PIKARU12" = "darksalmon", 
                   "PIKARU13" = "darkseagreen", "PIKARU14" = "darkmagenta", "PIKARU15" = "goldenrod3", 
                   "PIKARU21" = "deepskyblue", "PIKARU22" = "darksalmon", "PIKARU23" = "darkseagreen", 
                   "PIKARU24" = "darkmagenta", "PIKARU25" = "goldenrod3", "PIKARU20" = "goldenrod3")

rdata_folder <- "C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/complete_predictions"
rdata_files <- list.files(rdata_folder, pattern = "*.RData", full.names = TRUE)

# Function to process and plot each RDS file (now using readRDS)
process_and_plot <- function(rdata_file) {
  # Read the RDS file
  predictions_dt <- readRDS(rdata_file)
  
  # Ensure that 'predictions_dt' is a data.table
  if (!is.data.table(predictions_dt)) {
    setDT(predictions_dt)  # Convert it to a data.table if it's not
  }
  
  # Ensure that 'PIKA' exists in the dataset and is numeric
  if (!"PIKA" %in% names(predictions_dt)) {
    stop("PIKA column not found in the dataset")
  }
  
  # Convert 'PIKA' to numeric if it's not already
  predictions_dt[, PIKA := as.numeric(PIKA)]
  
  # Filter the data.table to remove unwanted overlapping clips and set score threshold to 10
  filtered_predictions <- predictions_dt[PIKA > 10]
  
  # Take every other row (seq(1, .N, by = 2) is simplified)
  filtered_predictions <- filtered_predictions[seq(1, .N, by = 2)]
  
  # Restructure the data table by device, date, and time
  filtered_predictions[, `:=`(
    device = sub(".*\\\\(PIKARU\\d+).*", "\\1", file),
    date = sub(".*_(\\d{8})_.*", "\\1", file),
    time = sub(".*_(\\d{6})\\.wav", "\\1", file)
  )]
  
  # Convert time to numeric
  filtered_predictions[, time := as.numeric(time)]
  
  # Extract the hour from the time, rounding to the nearest hour
  filtered_predictions[, hour := floor(time / 10000)]  # This divides by 10,000 to get the hour part
  
  # Count the number of occurrences per device, date, and hour
  summary_table_hourly <- filtered_predictions[, .N, by = .(device, date, hour)]
  
  # Rename the count column
  setnames(summary_table_hourly, "N", "count")
  
  # Create a plot of time vs #pika calls for each day at each site
  plot <- ggplot(summary_table_hourly, aes(x = hour, y = count, color = device)) +
    geom_point() +
    geom_line(linewidth=1) +
    labs(
      title = paste(sub("predict_score_(\\w+)_.*", "\\1", basename(rdata_file))),  
      x = "Hour of day",
      y = "Number of calls per hour"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black", linewidth = 0.8),
          axis.title.x = element_text(size = 14, margin = margin(t = 10)),  # Increase font size and spacing
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # Increase font size and spacing
          axis.text = element_text(size = 12),  # Increase axis text size
          legend.position = "none") +
    scale_color_manual(values = device_colors) +  # Apply the custom color mapping
    guides(color = guide_legend(title = NULL)) +  # Remove legend title
    ylim(0,150)
  
  # Return the plot
  return(plot)
}

# Use purrr::map() to process each RDS and generate a list of plots
plots <- map(rdata_files, process_and_plot)

# Arrange and display all the plots in a grid
grid.arrange(grobs = plots, ncol = 4)

# Optionally, view the first plot
print(plots[[13]])



###################################################OLD
# Turn csv files into Rdata files
# Set the path to your folder containing CSV files
csv_folder <- "C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/complete_predictions_csv"  
output_folder <- "C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/complete_predictions"   

# Get the list of all CSV files in the folder
csv_files <- list.files(csv_folder, pattern = "\\.csv$", full.names = TRUE)

# Process and save CSV files as RDS using lapply
lapply(csv_files, function(csv_file) {
  # Read the CSV file
  predictions_csv <- fread(csv_file)
  
  # Create a dynamic filename for the RDS file based on the CSV filename
  file_name <- tools::file_path_sans_ext(basename(csv_file))  # remove the extension
  output_file <- file.path(output_folder, paste0(file_name, ".RData"))
  
  # Save the data as an RDS file
  saveRDS(predictions_csv, output_file)
})

# Confirmation message
cat("All CSV files have been processed and saved as RData files.")




# This maps PIKARU6 to PIKARU1, PIKARU7 to PIKARU2, etc.
device_colors <- c("PIKARU1" = "deepskyblue", 
                   "PIKARU2" = "darksalmon", 
                   "PIKARU3" = "darkseagreen",
                   "PIKARU4" = "darkmagenta",
                   "PIKARU5" = "goldenrod3",
                   "PIKARU6" = "deepskyblue",  
                   "PIKARU7" = "darksalmon",  
                   "PIKARU8" = "darkseagreen", 
                   "PIKARU9" = "darkmagenta",  
                   "PIKARU10" = "goldenrod3",
                   "PIKARU11" = "deepskyblue", 
                   "PIKARU12" = "darksalmon", 
                   "PIKARU13" = "darkseagreen",
                   "PIKARU14" = "darkmagenta",
                   "PIKARU15" = "goldenrod3",
                   "PIKARU21" = "deepskyblue", 
                   "PIKARU22" = "darksalmon", 
                   "PIKARU23" = "darkseagreen",
                   "PIKARU24" = "darkmagenta",
                   "PIKARU25" = "goldenrod3",
                   "PIKARU20" = "goldenrod3")


rdata_folder <- "C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/complete_predictions"
rdata_files <- list.files(rdata_folder, pattern = "*.RData", full.names = TRUE)


# Function to process and plot each CSV file
process_and_plot <- function(rdata_file) {
  # Load the RData file (assuming it contains a data.table or data.frame)
  load(rdata_file)
  
  # Assuming the object inside the RData file is called 'predictions_dt'
  # If the name of the object inside the RData file is different, change accordingly
  predictions_dt <- get(ls()[1])  # Get the first object in the environment
  
  # Filter the data.table to remove unwanted overlapping clips and set score threshold to 6
  filtered_predictions <- predictions_dt[seq(1, .N, by = 2) & PIKA > 10]
  
  # Restructure the data table by device, date, and time
  filtered_predictions[, `:=`(
    device = sub(".*\\\\(PIKARU\\d+).*", "\\1", file),
    date = sub(".*_(\\d{8})_.*", "\\1", file),
    time = sub(".*_(\\d{6})\\.wav", "\\1", file)
  )]
  
  # Convert time to numeric
  filtered_predictions[, time := as.numeric(time)]
  
  # Extract the hour from the time, rounding to the nearest hour
  filtered_predictions[, hour := floor(time / 10000)]  # This divides by 10,000 to get the hour part
  
  # Count the number of occurrences per device, date, and hour
  summary_table_hourly <- filtered_predictions[, .N, by = .(device, date, hour)]
  
  # Rename the count column
  setnames(summary_table_hourly, "N", "count")
  
  # Create a plot of time vs #pika calls for each day at each site
  plot <- ggplot(summary_table_hourly, aes(x = hour, y = count, color = device)) +
    geom_point() +
    geom_line(linewidth=1) +
    labs(
      title = paste(rdata_file),  
      x = "Hour of day",
      y = "Number of calls per hour"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black", linewidth = 0.8),
          axis.title.x = element_text(size = 14, margin = margin(t = 10)),  # Increase font size and spacing
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # Increase font size and spacing
          axis.text = element_text(size = 12),  # Increase axis text size
          legend.position = "none") +
    scale_color_manual(values = device_colors) +  # Apply the custom color mapping
    guides(color = guide_legend(title = NULL)) +  # Remove legend title
    ylim(0,60)
  
  # Return the plot
  return(plot)
}

# Use purrr::map() to process each CSV and generate a list of plots
plots <- map(rdata_files, process_and_plot)

grid.arrange(grobs = plots, ncol = 2)

# Optionally, view the first plot
print(plots[[1]])






############################### Old code:

# Set working directory
setwd("C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/complete_predictions")

# Set file path to file with sunrise, sunset, solar noon
sun_times <- fread("C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/sunset_sunrise/sunrise_sunset_summer2024.csv")

# Get a list of all CSV files in the working directory
csv_files <- list.files(pattern = "*.csv")

head(csv_file_1)

# Function to process and plot each CSV file
process_and_plot <- function(csv_file) {
  # Read the CSV file into a data.table
  predictions_dt <- fread(csv_file)
  
  # Filter the data.table to remove unwanted overlapping clips and set score threshold to 6
  filtered_predictions <- predictions_dt[seq(1, .N, by = 2) & PIKA > 14]
  
  # Restructure the data table by device, date, and time
  filtered_predictions[, `:=`(
    device = sub(".*\\\\(PIKARU\\d+).*", "\\1", file),
    date = sub(".*_(\\d{8})_.*", "\\1", file),
    time = sub(".*_(\\d{6})\\.wav", "\\1", file)
  )]
  
  # Convert time to numeric
  filtered_predictions[, time := as.numeric(time)]
  
  # Extract the hour from the time, rounding to the nearest hour
  filtered_predictions[, hour := floor(time / 10000)]  # This divides by 10,000 to get the hour part
  
  # Count the number of occurrences per device, date, and hour
  summary_table_hourly <- filtered_predictions[, .N, by = .(device, date, hour)]
  
  # Rename the count column
  setnames(summary_table_hourly, "N", "count")
  
  # Get the corresponding sun times for this CSV file
  sun_time_for_csv <- sun_times[csv == basename(csv_file)]  # Match CSV name
  
  # Extract the sun times (sunrise, solar noon, sunset)
  sunrise_time <- sun_time_for_csv$sunrise
  solar_noon_time <- sun_time_for_csv$solar_noon
  sunset_time <- sun_time_for_csv$sunset
  
  # Convert times to numeric format (removing colon, e.g., "5:15" -> 515)
  sunrise_hour <- as.numeric(substr(sunrise_time, 1, 2)) # Extract the hour (HH)
  sunrise_minute <- as.numeric(substr(sunrise_time, 4, 5)) # Extract the minutes (MM)
  sunrise_time_num <- sunrise_hour + sunrise_minute / 60  # Convert to decimal hour
  
  solar_noon_hour <- as.numeric(substr(solar_noon_time, 1, 2))
  solar_noon_minute <- as.numeric(substr(solar_noon_time, 4, 5))
  solar_noon_time_num <- solar_noon_hour + solar_noon_minute / 60
  
  sunset_hour <- as.numeric(substr(sunset_time, 1, 2))
  sunset_minute <- as.numeric(substr(sunset_time, 4, 5))
  sunset_time_num <- sunset_hour + sunset_minute / 60
  
  # Create a plot of time vs #pika calls for each day at each site
  plot <- ggplot(summary_table_hourly, aes(x = hour, y = count, color = device)) +
    geom_point() +
    geom_line() +
    labs(
      title = paste(csv_file),  
      x = "Hour of day",
      y = "Number of calls per hour"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black", linewidth = 0.3)) +
    scale_color_manual(values = device_colors) +  # Apply the custom color mapping
    guides(color = guide_legend(title = NULL)) +  # Remove legend title
    ylim(0, 100) +
    # Add vertical dashed lines for sunrise, solar noon, and sunset
    geom_vline(xintercept = solar_noon_time_num, linetype = "dashed", color = "blue") +
    geom_vline(xintercept = sunset_time_num, linetype = "dashed", color = "blue") +
    geom_vline(xintercept = sunrise_time_num, linetype = "dashed", color = "blue")
  
  # Return the plot
  return(plot)
}

# Use purrr::map() to process each CSV and generate a list of plots
plots <- map(csv_files, process_and_plot)

grid.arrange(grobs = plots, ncol = 2)

########################################



