# Graphing pika activity over the day and in relation to solar power

#testing changes
library(ggplot2)
library(tidyverse)
library(data.table)
library(purrr)
library(gridExtra)
library(grid)


##################################################################################
# Use the following code

rdata_folder <- "recognizer_outputs/predictions_diurnal_activity"
rdata_files <- list.files(rdata_folder, pattern = "*.RData", full.names = TRUE)

print(rdata_files)

# Device color mappings
device_colors <- c("PIKARU1" = "deepskyblue", "PIKARU2" = "darksalmon", "PIKARU3" = "darkseagreen", 
                   "PIKARU4" = "darkmagenta", "PIKARU5" = "goldenrod3", "PIKARU6" = "deepskyblue", 
                   "PIKARU7" = "darksalmon", "PIKARU8" = "darkseagreen", "PIKARU9" = "darkmagenta", 
                   "PIKARU10" = "goldenrod3", "PIKARU11" = "deepskyblue", "PIKARU12" = "darksalmon", 
                   "PIKARU13" = "darkseagreen", "PIKARU14" = "darkmagenta", "PIKARU15" = "goldenrod3", 
                   "PIKARU21" = "deepskyblue", "PIKARU22" = "darksalmon", "PIKARU23" = "darkseagreen", 
                   "PIKARU24" = "darkmagenta", "PIKARU25" = "goldenrod3", "PIKARU20" = "goldenrod3")

site_y_limits <- list(
  "PC" = c(0, 90),
  "PP" = c(0, 330),
  "MD" = c(0, 125),
  "MB" = c(0, 125)
)

# Function to process and plot each RDS file (now using readRDS)
process_and_plot <- function(rdata_file, site_y_limits) {
  # Load the RDS file
  predictions_dt <- readRDS(rdata_file)  # This loads the RDS object from the file
  
  # Ensure that 'predictions_dt' is a data.table
  if (!is.data.table(predictions_dt)) {
    setDT(predictions_dt)  # Convert to a data.table if it's not already
  }
  
  # Ensure that 'PIKA' exists in the dataset and is numeric
  if (!"PIKA" %in% names(predictions_dt)) {
    stop("PIKA column not found in the dataset")
  }
  
  # Convert 'PIKA' to numeric if it's not already
  predictions_dt[, PIKA := as.numeric(PIKA)]
  
  # Filter the data.table to remove unwanted overlapping clips and set score threshold to 10
  filtered_predictions <- predictions_dt[!grepl("\\.\\d5$", end_time), ]
  filtered_predictions <- filtered_predictions[PIKA > 10]
  filtered_predictions[, file := gsub(" - Copy", "", file)]
  
  # Restructure the data table by device, date, and time
  filtered_predictions[, `:=`(
    device = sub(".*\\\\(PIKARU\\d+).*", "\\1", file),
    date = sub(".*_(\\d{8})_.*", "\\1", file),
    time = sub(".*_(\\d{6})\\.wav", "\\1", file),
    site = sub("predict_score_(\\w+)_.*", "\\1", basename(rdata_file))
  )]
  
  # Convert time to numeric
  filtered_predictions[, time := as.numeric(time)]
  
  # Extract the hour from the time, rounding to the nearest hour
  filtered_predictions[, hour := floor(time / 10000)+0.5]  # This divides by 10,000 to get the hour part
  
  # Count the number of occurrences per device, date, and hour
  summary_table_hourly <- filtered_predictions[, .N, by = .(device, date, hour)]
  
  # Rename the count column
  setnames(summary_table_hourly, "N", "count")
  
  site <- unique(filtered_predictions$site)
 
  # Set y-axis limits based on the site
  y_limits <- site_y_limits[[site]]
  
  # Create a plot of time vs #pika calls for each day at each site
  plot <- ggplot(summary_table_hourly, aes(x = hour, y = count, color = device)) +
    geom_point(size=0.2) +
    geom_line(linewidth=1) +
    labs(
      title = paste(sub("predict_score_(\\w+)_.*", "\\1", basename(rdata_file))),  
      x = "Hour of day",
      y = "Number of calls per hour"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black", linewidth = 0.8),
          axis.text = element_text(size = 12),  # Increase axis text size
          legend.position = "none",
          axis.title.x = element_blank(),  # Remove x-axis title for individual plots
          axis.title.y = element_blank()) +
    scale_color_manual(values = device_colors) +  # Apply the custom color mapping
    guides(color = guide_legend(title = NULL)) +  # Remove legend title
    ylim(y_limits) +
    scale_x_continuous(
      breaks = seq(0, 24, by = 4),  # Set the breaks at each hour
      labels = function(x) sprintf("%02d:00", x)  # Format the x-axis labels as HH:00
    )
    
  # Return the plot
  return(plot)
}

# Use purrr::map() to process each RDS and generate a list of plots
plots <- map(rdata_files, ~process_and_plot(.x, site_y_limits))

plot_grid <- grid.arrange(
  grobs = plots,       # List of plots
  ncol = 3,            # Arrange in 3 columns
  left = textGrob("Number of calls per hour", rot = 90, gp = gpar(fontsize = 14)),  # y-axis label
  bottom = textGrob("Hour of day", gp = gpar(fontsize = 14))  # x-axis label
)

# Optionally, view the first plot
print(plots[[7]])


ggsave("figures/activity_plots.png", plot=plot_grid, width = 12, height =14 )


