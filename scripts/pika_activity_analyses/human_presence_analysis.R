# Human presence vs pika activity
library(data.table)


File path, site id, time (10 minutes), number of calls




csv_file <- "C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/Humans/all_human_dataset.csv"

# Read the CSV file into a data.table
human_predictions_dt <- fread(csv_file)

file_names_dt <- unique(human_predictions_dt[, .(file)])

filtered_h_predictions <- human_predictions_dt[seq(1, .N, by = 2) & PIKA > 14]

# Create the new data.table
new_table <- filtered_h_predictions[, .(
  site = sub(".*/([^/]+)$", "\\1", dirname(file)),  # Extract the site (folder name) from the file path
  date = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\1", file),  # Extract date from file name
  time = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\2", file),  # Extract time from file name
  count = .N  # Count the number of times the file name appears
), by = file]

# Make sure all filenames in file_names_dt appear, even those with count = 0
new_table_full <- merge(file_names_dt, new_table, by = "file", all.x = TRUE)

# Replace NAs in 'count' with 0 (for files that don't appear in filtered_h_predictions)
new_table_full[is.na(count), `:=`(
  count = 0,  # Set count to 0 for missing files
  site = sub(".*/([^/]+)$", "\\1", dirname(file)),  # Extract site again for missing files
  date = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\1", file),  # Extract date again for missing files
  time = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\2", file)   # Extract time again for missing files
)]

# View the final result
head(new_table_full)



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


# Bayesian model:
