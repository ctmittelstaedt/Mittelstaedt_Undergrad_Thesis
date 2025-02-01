# Graphing pika activity over the day and in relation to solar power

install.packages("data.table")
install.packages("purrr")
library(ggplot2)
library(tidyverse)
library(data.table)
library(purrr)
library(gridExtra)

# Clean up prediction files and save to working directory
# Read the two CSV files into data tables
csv_file_1 <- fread("C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/MD/predict_score_MD_11-14_good_day.csv")
csv_file_2 <- fread("C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/MD/predict_score_MD_PIKARU15_28Jul2025.csv")

# Merge the data tables (combine them row-wise)
merged_data <- rbind(csv_file_1, csv_file_2)

# Save cleaned data to working directory
fwrite(merged_data, "C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/complete_predictions/predict_score_MD_28Jul2024.csv")

# Set working directory
setwd("C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/complete_predictions")

# Get a list of all CSV files in the working directory
csv_files <- list.files(pattern = "*.csv")

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
                   "PIKARU15" = "goldenrod3")


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
    ylim(0,100)
    
  # Return the plot
  return(plot)
}

# Use purrr::map() to process each CSV and generate a list of plots
plots <- map(csv_files, process_and_plot)

grid.arrange(grobs = plots, ncol = 2)

# Optionally, view the first plot
print(plots[[1]])







