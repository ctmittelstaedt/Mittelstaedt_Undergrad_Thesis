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

predictions <- read.csv("C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/predict_score_BC_7Aug2024_ARU16-19.csv")


saveRDS(predictions,file.path("recognizer_outputs/predictions_diurnal_activity/predict_score_BC_07Aug2024.Rdata"))

bc1 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_BC_07Aug2024.RData")
bc2 <- readRDS("recognizer_outputs/predictions/predict_score_BC_PIKARU20_07Aug2024.RData")
merged_bc <- rbind(bc1, bc2)
saveRDS(merged_bc, file.path("recognizer_outputs/predictions_diurnal_activity/predict_score_BC_07Aug2024.Rata"))

###################################
rdata_folder <- "recognizer_outputs/predictions_diurnal_activity"
rdata_files <- list.files(rdata_folder, pattern = "*.RData", full.names = TRUE)

print(rdata_files)

# Device color mappings
device_colors <- c("PIKARU1" = "#88CCEE", "PIKARU2" = "#CC6677", "PIKARU3" = "#44AA99", 
                   "PIKARU4" = "#882255", "PIKARU5" = "#999933", "PIKARU6" = "#88CCEE", 
                   "PIKARU7" = "#CC6677", "PIKARU8" = "#44AA99", "PIKARU9" = "#882255", 
                   "PIKARU10" = "#999933", "PIKARU11" = "#88CCEE", "PIKARU12" = "#CC6677", 
                   "PIKARU13" = "#44AA99", "PIKARU14" = "#882255", "PIKARU15" = "#999933", 
                   "PIKARU16" = "#88CCEE", "PIKARU17" = "#CC6677", "PIKARU18" = "#44AA99",
                   "PIKARU19" = "#882255", "PIKARU20" = "#999933", 
                   "PIKARU21" = "#88CCEE", "PIKARU22" = "#CC6677", "PIKARU23" = "#44AA99", 
                   "PIKARU24" = "#882255", "PIKARU25" = "#999933", "PIKARU20" = "#999933")

site_y_limits <- list(
  "PC" = c(0, 100),
  "PP" = c(0, 330),
  "MD" = c(0, 125),
  "MB" = c(0, 125),
  "BC" = c(0, 100)
)

sun_times <- read.csv("C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/data/pika_activity/sunrise_sunset_summer2024.csv")

# Function to process and plot each RDS file (now using readRDS)
process_and_plot <- function(rdata_file, site_y_limits, sun_times) {
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
  
  row_count_by_site <- filtered_predictions[, .N, by = site]
  print(row_count_by_site)
  
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
  
  unique_dates <- unique(filtered_predictions$date)
  
  ## Get sunrise/sunset times
  sunrise_time <- sapply(unique_dates, function(date) {
    sun_times[sun_times$site == site & sun_times$date == date, "sunrise"]
  })
  sunset_time <- sapply(unique_dates, function(date) {
    sun_times[sun_times$site == site & sun_times$date == date, "sunset"]
  })
  
  # Ensure that the sunrise and sunset times are correctly matched to the dates
  if (length(sunrise_time) != 1 || length(sunset_time) != 1) {
    stop("Unable to find sunrise and sunset times for the site and date combination.")
  }
  
  # Create a plot of time vs #pika calls for each day at each site
  plot <- ggplot(summary_table_hourly, aes(x = hour, y = count, color = device)) +
    geom_point(size=0.2) +
    geom_line(linewidth=1) +
    
    labs(
      #title = paste(sub("predict_score_(\\w+)_.*", "\\1", basename(rdata_file))),  
      x = "Hour of day",
      y = "Number of calls per hour"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black", linewidth = 0.8),
          axis.text = element_text(size = 15),  # Increase axis text size
          legend.position = "none",
          axis.title.x = element_blank(),  # Remove x-axis title for individual plots
          axis.title.y = element_blank()) +
    scale_color_manual(values = device_colors) +  # Apply the custom color mapping
    guides(color = guide_legend(title = NULL)) +  # Remove legend title
    ylim(y_limits) +
    scale_x_continuous(
      breaks = seq(0, 24, by = 6),  # Set the breaks at each hour
      labels = function(x) sprintf("%02d:00", x), # Format the x-axis labels as HH:00
      limits = c(0,24),
      expand = c(0, 0)
      )
  
 ####################
  if (sunset_time > 23) {
    # Shading the part between sunset and 24:00, and between 0:00 and sunrise
    plot <- plot + 
      geom_rect(data = data.frame(xmin = sunset_time, xmax = 24),
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey", alpha = 0.3, inherit.aes = FALSE) +
      geom_rect(data = data.frame(xmin = 0, xmax = sunrise_time),
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey", alpha = 0.3, inherit.aes = FALSE)
  } else {
    # Shading the part between sunset and sunrise (normal case)
    plot <- plot + 
      geom_rect(data = data.frame(xmin = sunset_time, xmax = sunrise_time),
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey", alpha = 0.3, inherit.aes = FALSE)
  }
    
  # Return the plot
  return(plot)
}


# Use purrr::map() to process each RDS and generate a list of plots
plots <- map(rdata_files, ~process_and_plot(.x, site_y_limits, sun_times))

plots_to_plot <- plots[8:10]

plots_to_plot_with_margin <- lapply(plots_to_plot, function(p) {
  p + theme(plot.margin = margin(t = 10, r = 22, b = 5, l = 7))  # t: top, r: right, b: bottom, l: left
})


adjusted_plots <- lapply(plots, function(p) {
  p + theme(
    plot.margin = unit(c(1, 0.8, 1.4, 0.5), "cm")  # Add 1 cm padding to all sides (top, right, bottom, left)
  )
})

modified_plots <- c(adjusted_plots[8],adjusted_plots[9],adjusted_plots[10],adjusted_plots[11],adjusted_plots[12],adjusted_plots[13],adjusted_plots[5],adjusted_plots[6],adjusted_plots[7],adjusted_plots[2],adjusted_plots[3],adjusted_plots[4],adjusted_plots[1])

plot_grid <- grid.arrange(
  grobs = modified_plots,       # List of plots
  ncol = 3,            # Arrange in 3 columns
  #left = textGrob("Number of calls\nper hour", rot = 90, gp = gpar(fontsize = 18)),  # y-axis label
  #bottom = textGrob("Hour of day", gp = gpar(fontsize = 18)),
  padding = unit(1, "cm")  # Equal heights for rows (adjust as needed)
)


# Optionally, view the first plot
print(plots[[1]])

ggsave("figures/activity_plots_all.png", plot=plot_grid, width = 12, height =15)








# Number of daylight vs nighttime calls
# Pika Camp
pc1 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_PC_18Jul2024.RData")
pc2 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_PC_20240721.RData")
pc3 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_PC_20240725.RData")

# Day 1
pc1 <- pc1[!grepl("\\.\\d5$", end_time)]
pc1 <- pc1[PIKA > 10]

pc1[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc1[, time := as.numeric(time)] 

# Night
count <- pc1[ (time >= 5000 & time <= 35000), .N]
count
count/3 #per hour

# Daylight
count <- pc1[ (time >= 35000 & time <= 235959) | (time >= 0 & time <= 5000), .N]
count
count/21 #per hour

# Day 2
pc2 <- pc2[!grepl("\\.\\d5$", end_time)]
pc2 <- pc2[PIKA > 10]

pc2[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc2[, time := as.numeric(time)] 

# Night
count <- pc2[ (time >= 4000 & time <= 40000), .N]
count
count/3.33 #per hour

# Daylight
count <- pc2[ (time >= 40000 & time <= 235959) | (time >= 0 & time <= 4000), .N]
count
count/20.66 #per hour

# Day 3
setDT(pc3)
pc3 <- pc3[!grepl("\\.\\d5$", end_time)]
pc3 <- pc3[PIKA > 10]

pc3[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc3[, time := as.numeric(time)] 

# Night
count <- pc3[ (time >= 2000 & time <= 42000), .N]
count
count/4 #per hour

# Daylight
count <- pc3[ (time >= 42000 & time <= 235959) | (time >= 0 & time <= 2000), .N]
count
count/20 #per hour


(99.71+103.38+113.45)/3
(45.33+63.66+35.75)/3

# Printer's Pass
pc1 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_PP_19Jul2025.RData")
pc2 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_PP_20240721.RData")
pc3 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_PP_20240724.RData")

setDT(pc1)
setDT(pc2)
setDT(pc3)

# Day 1
pc1 <- pc1[!grepl("\\.\\d5$", end_time)]
pc1 <- pc1[PIKA > 10]
pc1 <- pc1[, file := gsub(" - Copy", "", file)]

pc1[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc1[, time := as.numeric(time)]

# Night
count <- pc1[ (time >= 5000 & time <= 35000), .N]
count
count/3 #per hour

# Daylight
count <- pc1[ (time >= 35000 & time <= 235959) | (time >= 0 & time <= 5000), .N]
count
count/21 #per hour

# Day 2
pc2 <- pc2[!grepl("\\.\\d5$", end_time)]
pc2 <- pc2[PIKA > 10]
pc2 <- pc2[, file := gsub(" - Copy", "", file)]

pc2[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc2[, time := as.numeric(time)] 

# Night
count <- pc2[ (time >= 4000 & time <= 40000), .N]
count
count/3.33 #per hour

# Daylight
count <- pc2[ (time >= 40000 & time <= 235959) | (time >= 0 & time <= 4000), .N]
count
count/20.66 #per hour

# Day 3
setDT(pc3)
pc3 <- pc3[!grepl("\\.\\d5$", end_time)]
pc3 <- pc3[PIKA > 10]

pc3[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc3[, time := as.numeric(time)] 

# Night
count <- pc3[ (time >= 3000 & time <= 41000), .N]
count
count/3.66 #per hour

# Daylight
count <- pc3[ (time >= 41000 & time <= 235959) | (time >= 0 & time <= 3000), .N]
count
count/20.33 #per hour

(337+451.54+593.26)/3
(37+79.27+125.68)/3

#Mt. Decoeli
pc1 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_MD_28Jul2024.RData")
pc2 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_MD_31Jul2024.RData")
pc3 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_MD_20240727.RData")

setDT(pc1)
setDT(pc2)
setDT(pc3)

# Day 1
pc1 <- pc1[!grepl("\\.\\d5$", end_time)]
pc1 <- pc1[PIKA > 10]

pc1[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc1[, time := as.numeric(time)] 

# Night
count <- pc1[ (time >= 1000 & time <= 43000), .N]
count
count/4.33 #per hour

# Daylight
count <- pc1[ (time >= 43000 & time <= 235959) | (time >= 0 & time <= 1000), .N]
count
count/19.66 #per hour

# Day 2
pc2 <- pc2[!grepl("\\.\\d5$", end_time)]
pc2 <- pc2[PIKA > 10]

pc2[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc2[, time := as.numeric(time)]

# Night
count <- pc2[ (time >= 235000 & time <= 235959)|(time >= 0 & time <= 44000), .N]
count
count/4.83 #per hour

# Daylight
count <- pc2[ (time > 44000 & time <= 235000), .N]
count
count/19.17 #per hour

# Day 3
pc3 <- pc3[!grepl("\\.\\d5$", end_time)]
pc3 <- pc3[PIKA > 10]

pc3[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc3[, time := as.numeric(time)] 

# Night
count <- pc3[ (time >= 1000 & time <= 43000), .N]
count
count/4.33 #per hour

# Daylight
count <- pc3[ (time >= 43000 & time <= 235959) | (time >= 0 & time <= 1000), .N]
count
count/19.66 #per hour

(27.56+15.80+34.79)/3
(3+44.92+90.06)/3

#Mt. Boyle
pc1 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_MB_09Aug2024.RData")
pc2 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_MB_10Aug2024.RData")
pc3 <- readRDS("recognizer_outputs/predictions_diurnal_activity/predict_score_MB_12Aug2024.RData")

setDT(pc1)
setDT(pc2)
setDT(pc3)

# Day 1
pc1 <- pc1[!grepl("\\.\\d5$", end_time)]
pc1 <- pc1[PIKA > 10]

pc1[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc1[, time := as.numeric(time)] 

# Night
count <- pc1[ (time >= 1000 & time <= 43000), .N]
count
count/4.33 #per hour

# Daylight
count <- pc1[ (time >= 43000 & time <= 235959) | (time >= 0 & time <= 1000), .N]
count
count/19.66 #per hour

# Day 2
pc2 <- pc2[!grepl("\\.\\d5$", end_time)]
pc2 <- pc2[PIKA > 10]

pc2[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc2[, time := as.numeric(time)]

# Night
count <- pc2[ (time > 0 & time < 44000), .N]
count
count/4.66 #per hour

# Daylight
count <- pc2[ (time > 44000 & time <= 235999), .N]
count
count/19.33 #per hour

# Day 3
pc3 <- pc3[!grepl("\\.\\d5$", end_time)]
pc3 <- pc3[PIKA > 10]

pc3[, `:=`(
  date = sub(".*_(\\d{8})_.*", "\\1", file),
  time = sub(".*_(\\d{6})\\.wav", "\\1", file)
)]

pc3[, time := as.numeric(time)] 

# Night
count <- pc3[ (time >= 235000 & time <= 235959) | (time >= 0 & time < 44000), .N]
count
count/4.83 #per hour

# Daylight
count <- pc3[ (time >= 44000 & time < 235000), .N]
count
count/19.17 #per hour

(45.26+48.73+26.76)/3
(157.96+59.44+15.52)/3


(105+460+26+40)/4
(48+80+46+77)/4
