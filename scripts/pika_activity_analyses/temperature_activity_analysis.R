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
    max_temp = scale(max_temp, center = TRUE, scale = FALSE),         # Standardize max_temp
    daily_rainfall = scale(daily_rainfall, center = TRUE, scale = FALSE),  # Standardize daily_rainfall
    avg_wind_speed = scale(avg_wind_speed, center = TRUE, scale = FALSE)   # Standardize avg_wind_speed
  )

# Set priors - need to scale these down if using Poisson*make SD smaller from 5
prior1 <- c(set_prior(prior = 'normal(0.25,2)', class='b', coef='max_temp'), 	
            set_prior(prior = 'normal(-0.3,2)', class='b', coef='daily_rainfall'),
            set_prior(prior = 'normal(-0.3,2)', class='b', coef='avg_wind_speed'),
            set_prior(prior = 'gamma(5,2)', class='Intercept', coef='', lb=0)
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
          #(1|ARU),
  weather_vs_calls_scaled,
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
ranef(weather_model_1)
## trace the plots to check convergence
plot(weather_model_1)
## plot a "goodness of fit" plot, compare your model distribution to the poster distriubtion of the data
pp_check(weather_model_1)
as_draws_df()

# Set rainfall and wind speed as means to disentangle variables for plot 1 - temp
weather_vs_calls_pred = weather_vs_calls
weather_vs_calls_pred$daily_rainfall = mean(weather_vs_calls_pred$daily_rainfall)
weather_vs_calls_pred$avg_wind_speed = mean(weather_vs_calls_pred$avg_wind_speed)

## Create a data frame of predictions from our model
predictions <- add_epred_draws(weather_vs_calls_pred,
                                   weather_model_1,re_formula = NULL) 


ggplot(weather_vs_calls,aes(max_temp,count))+
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0.95), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0), alpha = 1)+ ## mean
  geom_point()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Daily maximum temperature (C)",y="Number of calls per day")

# Plot 2 - rainfall
weather_vs_calls_pred2 = weather_vs_calls
weather_vs_calls_pred2$max_temp = mean(weather_vs_calls_pred$max_temp)
weather_vs_calls_pred2$avg_wind_speed = mean(weather_vs_calls_pred$avg_wind_speed)

predictions <- add_epred_draws(weather_vs_calls_pred2,
                               weather_model_1,re_formula = NULL) 

ggplot(predictions, aes(x = daily_rainfall, y = .epred, color = site))+
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0.95), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0), alpha = 1)+ ## mean
  geom_point(data = weather_vs_calls, aes(x = max_temp, y = count), color = "black", alpha = 0.5) + 
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Daily rainfall (mm)",y="Number of calls per day")

# Plot 3 - wind speed
weather_vs_calls_pred3 = weather_vs_calls
weather_vs_calls_pred3$max_temp = mean(weather_vs_calls_pred$max_temp)
weather_vs_calls_pred3$daily_rainfall = mean(weather_vs_calls_pred$daily_rainfall)

predictions <- add_epred_draws(weather_vs_calls_pred3,
                               weather_model_1,re_formula = NULL) 

ggplot(weather_vs_calls,aes(avg_wind_speed, count))+
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0.95), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0), alpha = 1)+ ## mean
  geom_point()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Average wind speed (km/h)",y="Number of calls per day")


