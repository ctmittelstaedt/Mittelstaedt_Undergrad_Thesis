# Temperature vs activity
library(tidyverse) # data manipulation (includes ggplot2)
library(tidybayes) # easy retrivale of BRM prediction
library(brms) # bayesian model fitting
library(data.table)

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


################################BAYESIAN MODEL############################################

# Test colinearity?

# Set prior
prior1 <-

## Set up model with site as a random effect
weather_model_1 <- brm(
  count ~ max_temp + 
          daily_rainfall + 
          avg_wind_speed + 
          (1 | site),
  weather_vs_calls,
  #family = ,
  #prior = prior1,
  cores = 3,
  chains = 3,
  iter = 4000,
  warmup = 1500
  #control = 
  )

## look at the distribution of the parameters, look at effective sample size ESS
summary(weather_model_1)
## summary of the fixed effects
fixef(weather_model_1)
## trace the plots to check convergence
plot(weather_model_1)
## plot a "goodness of fit" plot, compare your model distribution to the poster distriubtion of the data
pp_check(weather_model_1)

# investigate model fit
loo(weather_model_1)
pp_check(weather_model_1)


## create a data frame of predictions of our model
predictions <- add_predicted_draws(weather_vs_calls,
                                   weather_model_1) 

# Make plots
ggplot(weather_vs_calls,aes(max_temp,count))+
  stat_lineribbon(data = predictions,aes(y = .prediction),.width = c(0.2), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .prediction),.width = c(0), alpha = 1)+ ## mean
  geom_point()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Daily maximum temperature (C)",y="Number of calls per day")

ggplot(weather_vs_calls,aes(daily_rainfall, count))+
  stat_lineribbon(data = predictions,aes(y = .prediction),.width = c(0.2), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .prediction),.width = c(0), alpha = 1)+ ## mean
  geom_point()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Daily rainfall (mm)",y="Number of calls per day")

ggplot(weather_vs_calls,aes(avg_wind_speed, count))+
  stat_lineribbon(data = predictions,aes(y = .prediction),.width = c(0.2), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .prediction),.width = c(0), alpha = 1)+ ## mean
  geom_point()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Average wind speed (km/h)",y="Number of calls per day")

## results: this may or may not happen

class(weather_model_1)

