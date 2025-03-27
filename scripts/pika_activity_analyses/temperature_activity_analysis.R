# Temperature vs activity

library(tidyverse) # data manipulation (includes ggplot2)
library(tidybayes) # easy retrieval of BRM prediction
library(brms) # Bayesian model fitting
library(data.table)
library(sjPlot)
library(gghalves)
library(ggplot2)
library(gridExtra)
library(patchwork)

# ****IGNORE the  data processing!!!!!! Jump to line 40

# Convert extra weather days into .Rdata
weather_extras <- fread(file.path("data","pika_activity","predict_score_weather_extras.csv"))
saveRDS(weather_extras,file.path("recognizer_outputs","predict_score_weather_extras.RData"))

weather_extra_day <- "Recognizers/Predict/weather_extras"
weather_output_folder <- "recognizer_outputs/predictions"

# Get the list of all CSV files in the folder
weather_csv <- list.files(weather_extra_day, pattern = "\\.csv$", full.names = TRUE)

lapply(weather_csv, function(csv_file) {
  tryCatch({
    # Read the CSV file
    weather_csv_1 <- fread(csv_file)
    
    # Create a dynamic filename for the RDS file based on the CSV filename
    file_name <- tools::file_path_sans_ext(basename(csv_file))  # remove the extension from csv file
    output_file <- file.path(weather_output_folder, paste0(file_name, ".RData"))
    
    # Save the data as an RDS file
    saveRDS(weather_csv_1, output_file)
    
  }, error = function(e) {
    cat("Error with file:", csv_file, "\n")  # Use csv_file here
    cat("Error message:", e$message, "\n")
  })
})


# Merge all predictions into one mega-file
rds_folder <- "recognizer_outputs/predictions"

# Get the list of .RData files in the folder
rds_files <- list.files(rds_folder, pattern = "*.RData", full.names = TRUE)

all_data <- lapply(rds_files, function(rds_file) {
  readRDS(rds_file)  # Read the RDS file into R
})

# Combine all data.tables into one
merged_data <- do.call(rbind, all_data)

# Remove Blackcap data
filtered_pikaru <- merged_data[!grepl("PIKARU16|PIKARU17|PIKARU18|PIKARU19|PIKARU20", merged_data$file), ]

# Save filtered table
saveRDS(filtered_pikaru, file = "recognizer_outputs/predictions/all_sites_weather_predictions.RData")




###### START HERE: ######################################

# Call in .Rdata
filtered_pikaru <- readRDS("recognizer_outputs/predictions/all_sites_weather_predictions.RData")

# Extract features from file names
processed_pikaru <- filtered_pikaru[, 
                    ':=' (  date = sub(".*_(\\d{8})_\\d{6}.*\\.wav.*", "\\1", file),
                            site = ifelse(grepl("PIKARU11|PIKARU12|PIKARU13|PIKARU14|PIKARU15", file), "MD", 
                                                       ifelse(grepl("PIKARU21|PIKARU22|PIKARU23|PIKARU24|PIKARU25", file), "MB",
                                                              ifelse(grepl("PIKARU6|PIKARU7|PIKARU8|PIKARU9|PIKARU10", file), "PP", 
                                          ifelse(grepl("PIKARU1|PIKARU2|PIKARU3|PIKARU4|PIKARU5", file), "PC",
                                                  NA)))),  
                    ARU = sub("([^/]+)_[^_]+_.+\\.wav", "\\1", basename(file))
                                     )
]

print(unique(processed_pikaru$date))
print(table(processed_pikaru$ARU))

# Remove overlap and set score threshold to 10
binary_pikaru <- processed_pikaru[!grepl("\\.\\d5$", end_time), ]
threshold_pikaru <- binary_pikaru[PIKA > 10]

# Count the number of unique combinations of site x date
site_date_count <- threshold_pikaru[, .(count = .N), by = .(ARU, site, date)]

# Call in weather data
weather_data <- fread("data/pika_activity/weather_all_sites.csv")

# convert columns to characters
site_date_count$date <- as.character(site_date_count$date)
site_date_count$site <- as.character(site_date_count$site)
site_date_count$ARU <- as.character(site_date_count$ARU)
weather_data$date <- as.character(weather_data$date)
weather_data$site <- as.character(weather_data$site)

# Create final dataset
weather_vs_calls <- merge(site_date_count, weather_data, by = c("site","date"), all.x = TRUE)
weather_vs_calls$site <- factor(weather_vs_calls$site)


################################BAYESIAN MODEL############################################

# Scale data to help with intercept prior estimate
weather_vs_calls_scaled <- weather_vs_calls %>%
  mutate(
    max_temp = scale(max_temp, center = TRUE, scale = FALSE)[,1],         
    daily_rainfall = scale(daily_rainfall, center = TRUE, scale = FALSE)[,1],  
    avg_wind_speed = scale(avg_wind_speed, center = TRUE, scale = FALSE)[,1]   
  )

# Set priors - need to scale these down if using Poisson*make SD smaller from 5
prior1 <- c(set_prior(prior = 'normal(0.12,1)', class='b', coef='max_temp'), 	
            set_prior(prior = 'normal(-0.15,0.5)',class='b', coef='daily_rainfall'),
            set_prior(prior = 'normal(-0.15,0.5)', class='b', coef='avg_wind_speed'),
            set_prior(prior = 'normal(5.6,2)', class='Intercept', coef='')
            )

# Test collinearity
cor(weather_vs_calls[,c("max_temp","daily_rainfall","avg_wind_speed")])

## Model with site as a random effect
weather_model_1 <- brm(
  count ~ max_temp + 
          daily_rainfall + 
          avg_wind_speed + 
          (1 | site) +
          (1 | date) + 
          (1|ARU),
  weather_vs_calls_scaled,
  family = poisson(),
  prior = prior1,
  cores = 3,
  chains = 3,
  iter = 5000,
  warmup = 2000,
  control = list(adapt_delta = 0.999, max_treedepth = 20)
  )

saveRDS(weather_model_1, "weather_model_1.rds")

weather_model_1 <- readRDS("weather_model_1.rds")

## look at the distribution of the parameters, look at effective sample size ESS
summary(weather_model_1)
## summary of the fixed effects
fixef(weather_model_1)
ranef(weather_model_1)
## trace the plots to check convergence
plot(weather_model_1)
## plot a "goodness of fit" plot, compare your model distribution to the poster distriubtion of the data
pp_check(weather_model_1)
as_draws_df()

#Set colours
site_weather_colours <- c(
  "PC"="#e59e62",
  "PP"="#b8901e",
  "MD"="#336061",
  "MB"="#547349"
)

overall_colour <- "#0e1b5e"

# Set rainfall and wind speed as means to disentangle variables for plot 1 - temp
weather_vs_calls_pred <- weather_vs_calls_scaled
weather_vs_calls_pred$daily_rainfall = mean(weather_vs_calls_pred$daily_rainfall)
weather_vs_calls_pred$avg_wind_speed = mean(weather_vs_calls_pred$avg_wind_speed)

# Calculate meaninful effect sizes (slopes)
# Temp
exp(5.84 + 0.06 + (-0.002631234)*(mean(weather_vs_calls_pred$daily_rainfall))+(0.09)*(mean(weather_vs_calls_pred$avg_wind_speed))) - exp(5.84 + (-0.002631234)*(mean(weather_vs_calls_pred$daily_rainfall))+(0.09)*(mean(weather_vs_calls_pred$avg_wind_speed)))
# Wind
exp(5.84 + 0.09 + (-0.002631234)*(mean(weather_vs_calls_pred$daily_rainfall))+(0.06)*(mean(weather_vs_calls_pred$max_temp))) - exp(5.84 + (-0.002631234)*(mean(weather_vs_calls_pred$daily_rainfall)) + (0.06)*(mean(weather_vs_calls_pred$max_temp)))

## Create a data frame of predictions from our model
predictions <- add_epred_draws(weather_vs_calls_pred,
                                   weather_model_1,re_formula = ~ (1|site)) 

mean_max_temp <- mean(weather_vs_calls$max_temp)
predictions$uncentered_max_temp <- predictions$max_temp + mean_max_temp
weather_vs_calls_scaled$uncentered_max_temp <- weather_vs_calls_scaled$max_temp + mean_max_temp

plot1 <- ggplot(predictions,aes(uncentered_max_temp,count))+
  stat_lineribbon(data = predictions,aes(y = .epred,fill = site),.width = c(0.95), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .epred,color = site),.width = c(0), alpha = 1)+ ## mean
  geom_point(aes(x = uncentered_max_temp, y = count,color=site), data = weather_vs_calls_scaled)+
  scale_color_manual(values = site_colors) +  # Custom colors for points and lines
  scale_fill_manual(values = site_colors) + 
  scale_x_continuous(
      # Define the range of the x-axis
    #breaks = seq(6, 21, by = 3),  # Adjust the breaks as needed
    labels = scales::label_number() 
  )+
  theme_bw()+
  labs(x= "Maximum daily temperature (°C)",y="Number of calls per day")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),  # Add black axis lines for bottom and left
    axis.line.x = element_line(size = 0.5),    # Keep bottom axis line
    axis.line.y = element_line(size = 0.5),    # Keep left axis line
    axis.ticks = element_line(size = 1),       # Keep axis ticks
    axis.ticks.length = unit(0.2, "cm"),       # Set tick length
    axis.text.x = element_text(size = 15),     # X axis text size
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20, vjust = -2, margin=margin(b=10)),
    axis.title.y = element_text(size = 20, vjust = 4, margin=margin(l=10)),
    panel.border = element_blank(),            # Remove panel border
    legend.title = element_blank(),            # Remove legend title
    legend.key = element_blank(),
    legend.text = element_text(size=15)
        )+
  #guides(
    #fill = "none", 
    #color = guide_legend(override.aes = list(fill = "transparent", color = site_colors))  # Explicitly set color legend with no background
  guides(
    fill = "none",  # Remove the fill legend
    color = "none"  # Remove the color legend as well
  )
  

plot1


predictions2 <- add_epred_draws(weather_vs_calls_pred,
                               weather_model_1,re_formula = NA)

predictions2$uncentered_max_temp <- predictions2$max_temp + mean_max_temp
weather_vs_calls_scaled$uncentered_max_temp <- weather_vs_calls_scaled$max_temp + mean_max_temp


plot2 <- ggplot(predictions2,aes(uncentered_max_temp,count))+
  stat_lineribbon(data = predictions2,aes(y = .epred),.width = c(0.95), alpha = 0.25, colour = "#0e1b5e")+ ## 95% credible interval
  stat_lineribbon(data = predictions2,aes(y = .epred),.width = c(0), alpha = 1, colour = "#0e1b5e")+ ## mean
  geom_point(aes(x = uncentered_max_temp, y = count), colour = "#0e1b5e", data = weather_vs_calls_scaled)+
  scale_x_continuous(
    #breaks = seq(6, 21, by = 3),  # Adjust the breaks as needed
    labels = scales::label_number() 
  )+
  theme_bw()+
  labs(x= "Maximum daily temperature (°C)",y="Number of calls per day")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),  # Add black axis lines for bottom and left
    axis.line.x = element_line(size = 0.5),    # Keep bottom axis line
    axis.line.y = element_line(size = 0.5),    # Keep left axis line
    axis.ticks = element_line(size = 1),       # Keep axis ticks
    axis.ticks.length = unit(0.2, "cm"),       # Set tick length
    axis.text.x = element_text(size = 15),     # X axis text size
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20, vjust = -2, margin=margin(b=10)),
    axis.title.y = element_text(size = 20, vjust = 4, margin=margin(l=10)),
    panel.border = element_blank(),            # Remove panel border
    legend.title = element_blank(),            # Remove legend title
    legend.key = element_blank(),
    legend.text = element_text(size=15)
  )+
  guides(fill = "none", color = "legend")

plot2

# Plot 2 - rainfall
weather_vs_calls_pred_rain = weather_vs_calls_scaled
weather_vs_calls_pred_rain$max_temp = mean(weather_vs_calls_pred_rain$max_temp)
weather_vs_calls_pred_rain$avg_wind_speed = mean(weather_vs_calls_pred_rain$avg_wind_speed)

predictions3 <- add_epred_draws(weather_vs_calls_pred_rain,
                               weather_model_1,re_formula = ~(1|site)) 

mean_rain <- mean(weather_vs_calls$daily_rainfall)
predictions3$uncentered_rain <- predictions3$daily_rainfall + mean_rain
weather_vs_calls_scaled$uncentered_rain <- weather_vs_calls_scaled$daily_rainfall + mean_rain

plot3 <- ggplot(predictions3,aes(uncentered_rain,count))+
  stat_lineribbon(data = predictions3,aes(y = .epred,fill = site),.width = c(0.95), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions3,aes(y = .epred,color = site),.width = c(0), alpha = 1)+ ## mean
  geom_point(aes(x = uncentered_rain, y = count,color=site), data = weather_vs_calls_scaled)+
  scale_color_manual(values = site_colors) +  # Custom colors for points and lines
  scale_fill_manual(values = site_colors) + 
  scale_x_continuous(
    labels = scales::label_number() 
  )+
  theme_bw()+
  labs(x= "Daily rainfall (mm)",y="Number of calls per day")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),  # Add black axis lines for bottom and left
    axis.line.x = element_line(size = 0.5),    # Keep bottom axis line
    axis.line.y = element_line(size = 0.5),    # Keep left axis line
    axis.ticks = element_line(size = 1),       # Keep axis ticks
    axis.ticks.length = unit(0.2, "cm"),       # Set tick length
    axis.text.x = element_text(size = 15),     # X axis text size
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20, vjust = -2, margin=margin(b=10)),
    axis.title.y = element_text(size = 20, vjust = 4, margin=margin(l=10)),
    panel.border = element_blank(),            # Remove panel border
    legend.title = element_blank(),            # Remove legend title
    legend.key = element_blank(),
    legend.text = element_text(size=15),
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )+
  guides(
    fill = "none",  # Remove the fill legend
    color = "none"  # Remove the color legend as well
  )

plot3

#Fixed effect
predictions4 <- add_epred_draws(weather_vs_calls_pred_rain,
                                weather_model_1,re_formula = NA)

predictions4$uncentered_rain <- predictions4$daily_rainfall + mean_rain
weather_vs_calls_scaled$uncentered_rain <- weather_vs_calls_scaled$daily_rainfall + mean_rain


plot4 <- ggplot(predictions4,aes(uncentered_rain,count))+
  stat_lineribbon(data = predictions4,aes(y = .epred),.width = c(0.95), alpha = 0.25, colour = "#0e1b5e")+ ## 95% credible interval
  stat_lineribbon(data = predictions4,aes(y = .epred),.width = c(0), alpha = 1, colour = "#0e1b5e")+ ## mean
  geom_point(aes(x = uncentered_rain, y = count), data = weather_vs_calls_scaled, colour = "#0e1b5e")+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  scale_x_continuous(
    labels = scales::label_number() 
  ) +
  theme_bw()+
  labs(x= "Daily rainfall (mm)",y="Number of calls per day")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),  # Add black axis lines for bottom and left
    axis.line.x = element_line(size = 0.5),    # Keep bottom axis line
    axis.line.y = element_line(size = 0.5),    # Keep left axis line
    axis.ticks = element_line(size = 1),       # Keep axis ticks
    axis.ticks.length = unit(0.2, "cm"),       # Set tick length
    axis.text.x = element_text(size = 15),     # X axis text size
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20, vjust = -2, margin=margin(b=10)),
    axis.title.y = element_text(size = 20, vjust = 4, margin=margin(l=10)),
    panel.border = element_blank(),            # Remove panel border
    legend.title = element_blank(),            # Remove legend title
    legend.key = element_blank(),
    legend.text = element_text(size=15)
  ) + guides(fill = "none", color = "legend")

plot4

# Plot 3 - wind speed
weather_vs_calls_pred_wind = weather_vs_calls_scaled
weather_vs_calls_pred_wind$max_temp = mean(weather_vs_calls_pred_wind$max_temp)
weather_vs_calls_pred_wind$daily_rainfall = mean(weather_vs_calls_pred_wind$daily_rainfall)

predictions5 <- add_epred_draws(weather_vs_calls_pred_wind,
                               weather_model_1,re_formula = ~(1|site)) 

mean_wind <- mean(weather_vs_calls$avg_wind_speed)
predictions5$uncentered_wind <- predictions5$avg_wind_speed + mean_wind
weather_vs_calls_scaled$uncentered_wind <- weather_vs_calls_scaled$avg_wind_speed + mean_wind

plot5 <- ggplot(predictions5,aes(uncentered_wind,count))+
  stat_lineribbon(data = predictions5,aes(y = .epred,fill = site),.width = c(0.95), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions5,aes(y = .epred,color = site),.width = c(0), alpha = 1)+ ## mean
  geom_point(aes(x = uncentered_wind, y = count,color=site), data = weather_vs_calls_scaled)+
  scale_color_manual(values = site_colors) +  # Custom colors for points and lines
  scale_fill_manual(values = site_colors) + 
  scale_x_continuous(
    labels = scales::label_number() 
  )+
  theme_bw()+
  labs(x= "Average daily wind speed (km/h)",y="Number of calls per day")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),  # Add black axis lines for bottom and left
    axis.line.x = element_line(size = 0.5),    # Keep bottom axis line
    axis.line.y = element_line(size = 0.5),    # Keep left axis line
    axis.ticks = element_line(size = 1),       # Keep axis ticks
    axis.ticks.length = unit(0.2, "cm"),       # Set tick length
    axis.text.x = element_text(size = 15),     # X axis text size
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20, vjust = -2, margin=margin(b=10)),
    axis.title.y = element_text(size = 20, vjust = 4, margin=margin(l=10)),
    panel.border = element_blank(),            # Remove panel border
    legend.title = element_blank(),            # Remove legend title
    legend.key = element_blank(),
    legend.text = element_text(size=15)
  )+
  guides(
    fill = "none",  # Remove the fill legend
    color = "none"  # Remove the color legend as well
  )

plot5

#Fixed effect
predictions6 <- add_epred_draws(weather_vs_calls_pred_wind,
                                weather_model_1,re_formula = NA)

predictions6$uncentered_wind <- predictions6$avg_wind_speed + mean_wind
weather_vs_calls_scaled$uncentered_wind <- weather_vs_calls_scaled$avg_wind_speed + mean_wind


plot6 <- ggplot(predictions6,aes(uncentered_wind,count))+
  stat_lineribbon(data = predictions6,aes(y = .epred),.width = c(0.95), alpha = 0.25, colour = "#0e1b5e")+ ## 95% credible interval
  stat_lineribbon(data = predictions6,aes(y = .epred),.width = c(0), alpha = 1, colour = "#0e1b5e")+ ## mean
  geom_point(aes(x = uncentered_wind, y = count), data = weather_vs_calls_scaled, colour = "#0e1b5e")+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  scale_x_continuous(
    labels = scales::label_number() 
  ) +
  theme_bw()+
  labs(x= "Average daily wind speed (km/h)",y="Number of calls per day")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),  # Add black axis lines for bottom and left
    axis.line.x = element_line(size = 0.5),    # Keep bottom axis line
    axis.line.y = element_line(size = 0.5),    # Keep left axis line
    axis.ticks = element_line(size = 1),       # Keep axis ticks
    axis.ticks.length = unit(0.2, "cm"),       # Set tick length
    axis.text.x = element_text(size = 15),     # X axis text size
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20, vjust = -2, margin=margin(b=10)),
    axis.title.y = element_text(size = 20, vjust = 4, margin=margin(l=10)),
    panel.border = element_blank(),            # Remove panel border
    legend.title = element_blank(),            # Remove legend title
    legend.key = element_blank(),
    legend.text = element_text(size=15)
  ) + guides(fill = "none", color = "legend")

plot6

big_weather_plot <- (plot2) / (plot4) / (plot6)

big_weather_plot <- (plot1) / (plot3) / (plot5)

big_weather_plot + 
  plot_layout(guides = "collect") +  # Collect all legends into one
  theme(legend.position = "none")

ggsave("figures/final_weather_plot2.png", plot = big_weather_plot, width = 7.5, height = 18, dpi = 300)
