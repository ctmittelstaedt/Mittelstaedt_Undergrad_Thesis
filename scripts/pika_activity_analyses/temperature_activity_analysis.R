# Temperature vs activity

library(tidyverse) # data manipulation (includes ggplot2)
library(tidybayes) # easy retrieval of BRM prediction
library(brms) # Bayesian model fitting
library(data.table)
library(sjPlot)
library(gghalves)
library(ggplot2)

# ****IGNORE the  data processing!!!!!! Jump to line 40

# Convert extra weather days into .Rdata
weather_extras <- fread(file.path("data","pika_activity","predict_score_weather_extras.csv"))
saveRDS(weather_extras,file.path("recognizer_outputs","predict_score_weather_extras.RData"))

# Merge all predictions into one mega-file
rds_folder <- "recognizer_outputs/predictions"

# Get the list of .RData files in the folder
rds_files <- list.files(rds_folder, pattern = "*.RData", full.names = TRUE)

all_data <- lapply(rds_files, function(rds_file) {
  readRDS(rds_file)  # Read the RDS file into R
})

# Combine all data.tables into one
merged_data <- do.call(rbind, all_data)

# Keep only data for PIKARU5, 10, 15, 25
filtered_pikaru <- merged_data[grepl("PIKARU5|PIKARU10|PIKARU15|PIKARU25", file)]

# Save filtered table
saveRDS(filtered_pikaru, file = "recognizer_outputs/predictions/all_sites_weather_predictions.RData")




###### START HERE: ######################################

# Call in .Rdata
filtered_pikaru <- readRDS("recognizer_outputs/predictions/all_sites_weather_predictions.RData")

# Remove overlap and set score threshold to 10
binary_pikaru <- filtered_pikaru[seq(1, .N, by = 2) & PIKA > 10]

# Extract features from file names
processed_pikaru <- binary_pikaru[, .(
  site = ifelse(grepl("PIKARU5", file), "PC",
                ifelse(grepl("PIKARU10", file), "PP",
                       ifelse(grepl("PIKARU15", file), "MD",
                              ifelse(grepl("PIKARU25", file), "MB", NA)))),
  date = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\1", file))  # Extract date from file name
  , by = file]

# Count the number of unique combinations of site x date
site_date_count <- processed_pikaru[, .(count = .N), by = .(site, date)]

# Call in weather data
weather_data <- fread("data/pika_activity/weather_all_sites.csv")

# convert date columns to characters
site_date_count$date <- as.character(site_date_count$date)
weather_data$date <- as.character(weather_data$date)

# Create final dataset
weather_vs_calls <- merge(site_date_count, weather_data, by = c("site","date"), all.x = TRUE)
weather_vs_calls$site <- factor(weather_vs_calls$site)


################################BAYESIAN MODEL############################################

# Set priors - need to scale these down if using Poisson
prior1 <- c(set_prior(prior = 'normal(0.25,5)', class='b', coef='max_temp'), 	
            set_prior(prior = 'normal(-0.3,5)', class='b', coef='daily_rainfall'),
            set_prior(prior = 'normal(-0.3,5)', class='b', coef='avg_wind_speed')
            ) 

# Test collinearity
cor(weather_vs_calls[,c("max_temp","daily_rainfall","avg_wind_speed")])

## Model with site as a random effect
weather_model_1 <- brm(
  count ~ max_temp + 
          daily_rainfall + 
          avg_wind_speed + 
          (1 | site),
  weather_vs_calls,
  family = poisson(),
  prior = prior1,
  cores = 3,
  chains = 3,
  iter = 5000,
  warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
  )

## look at the distribution of the parameters, look at effective sample size ESS
summary(weather_model_1)
## summary of the fixed effects
fixef(weather_model_1)
## trace the plots to check convergence
plot(weather_model_1)
## plot a "goodness of fit" plot, compare your model distribution to the poster distriubtion of the data
pp_check(weather_model_1)

# Set rainfall and wind speed as means to disentangle variables for plot #1
weather_vs_calls_pred = weather_vs_calls
weather_vs_calls_pred$daily_rainfall = mean(weather_vs_calls_pred$daily_rainfall)
weather_vs_calls_pred$avg_wind_speed = mean(weather_vs_calls_pred$avg_wind_speed)

## Create a data frame of predictions from our model
predictions <- add_epred_draws(weather_vs_calls_pred,
                                   weather_model_1,re_formula = NA) 

# Simple temperature plot
plot_model(weather_model_1,terms = "max_temp",type="pred")

# Better plot - still working on this
ggplot(weather_vs_calls,aes(max_temp,count))+
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0.95), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0), alpha = 1)+ ## mean
  geom_point()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Daily maximum temperature (C)",y="Number of calls per day")

# Working on this
ggplot(weather_vs_calls,aes(daily_rainfall, count))+
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0.95), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0), alpha = 1)+ ## mean
  geom_point()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Daily rainfall (mm)",y="Number of calls per day")

# Working on this
ggplot(weather_vs_calls,aes(avg_wind_speed, count))+
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0.95), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0), alpha = 1)+ ## mean
  geom_point()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Average wind speed (km/h)",y="Number of calls per day")


