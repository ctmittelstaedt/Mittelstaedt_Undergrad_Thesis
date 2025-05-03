##################################################################################
# Simple collared pika species distribution model
# Charlotte Mittelstaedt - The University of British Columbia
# Code adapted from Jordan Seider
# Created 10 April 2025
##################################################################################

library(sf)           # for spatial data
library(terra)        # for raster data
library(geodata)      # for geospatial data
library(rgbif)        # for biodiversity data
library(dplyr)        # for data manipulation
library(randomForest) # for machine learning SDM

# Define study area
study_area <- ext(-170.773407, -120.045898, 55, 73)

# Download climate data
climate_global <- worldclim_global(var = "bio", res = 0.5, path = "data/SDM")
climate_yukon <- crop(climate_global, study_area, mask = TRUE)

## BIO1  = Annual Mean Temperature
## BIO2  = Mean Diurnal Range (Mean of monthly (max temp - min temp))
## BIO3  = Isothermality (BIO2/BIO7) (×100)
## BIO4  = Temperature Seasonality (standard deviation ×100)
## BIO5  = Max Temperature of Warmest Month
## BIO6  = Min Temperature of Coldest Month
## BIO7  = Temperature Annual Range (BIO5-BIO6)
## BIO8  = Mean Temperature of Wettest Quarter
## BIO9  = Mean Temperature of Driest Quarter
## BIO10 = Mean Temperature of Warmest Quarter
## BIO11 = Mean Temperature of Coldest Quarter
## BIO12 = Annual Precipitation
## BIO13 = Precipitation of Wettest Month
## BIO14 = Precipitation of Driest Month
## BIO15 = Precipitation Seasonality (Coefficient of Variation)
## BIO16 = Precipitation of Wettest Quarter
## BIO17 = Precipitation of Driest Quarter
## BIO18 = Precipitation of Warmest Quarter
## BIO19 = Precipitation of Coldest Quarter

# Rename the bioclimatic variables
names(climate_yukon) <- sub("wc2.1_30s_", "", names(climate_yukon))

# Plot the bioclimatic variable
plot(climate_yukon)  # Annual Mean Temperature
plot(climate_yukon[[2]]) # Mean Temperature of Warmest Quarter

# Let's create our own BIO7 raster
maxTemp <- climate_yukon[[5]]
minTemp <- climate_yukon[[6]]
annual_range_YT <- maxTemp - minTemp

plot(climate_yukon[[7]])
plot(annual_range_YT)

# Load elevation data
elev_global <- elevation_global(res = 0.5, path = "data/SDM")
elevation <- crop(elev_global, study_area, mask = TRUE)

plot(st_geometry(study_area))  # Study area plot
plot(climate_yukon[[1]])
plot(elevation)
plot(st_geometry(study_area), add = TRUE, border = "red")
ext(climate)
ext(study_area)

# Create stack of predictor variables
predictors <- c(climate_yukon, elevation)

# Load the GBIF data
pika <- occ_search(scientificName = "Ochotona collaris", # Scientific name
                   hasCoordinate  = TRUE                 # Only records with coordinates
)$data %>% # Extract the data only
  
  # Convert to sf object
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = "EPSG:4326") # WGS84

# Extract the geometry to simplify data
pika_sf <- st_geometry(pika) 
pika_sf

# Number of occurrences
length(pika_sf)

# Clip occurrences to Yukon
study_area <- ext(study_area)

xmin <- study_area[1]
xmax <- study_area[2]
ymin <- study_area[3]
ymax <- study_area[4]

study_area <- st_sfc(st_polygon(list(matrix(c(xmin, ymin,
                                              xmax, ymin,
                                              xmax, ymax,
                                              xmin, ymax,
                                              xmin, ymin), 
                                            ncol = 2, byrow = TRUE))))

study_area <- st_as_sf(study_area)
st_crs(study_area) <- 4326

pika_yukon <- st_intersection(pika_sf, study_area)

# Number of occurrences in study area
length(pika_yukon)

# Plot the occurrences
plot(climate_yukon[[1]])
plot(pika_yukon, 
     add = TRUE, 
     pch = 20, 
     col = "black")

# Extract the predictor variables at the occurrence points
pika_data <- terra::extract(predictors, vect(pika_yukon)) %>% # extract predictor raster value at each point
  mutate(response = 1) %>%                                    # add response variable for presence/absence
  select(response, everything(), -ID)                         # remove ID column and rearrange (response first; not necessary though)

# Create background dataset
set.seed(1) # Set seed for reproducibility

background <- st_sample(x    = study_area,                  # Study area (bound from which to select random points)
                        size = length(pika_yukon) * 5) # Number of points; 5 times the number of occurrences
plot(background)

# Extract the predictors variables at the background points
background <- terra::extract(predictors, vect(background)) %>%
  mutate(response = 0) %>%
  select(response, everything(), -ID)

# Combine the occurrence and background data
data <- rbind(pika_data, background) %>%
  mutate(response = as.factor(response)) # convert response variable to factor (not numeric) for categorical machine learning
data <- na.omit(data)

# Run random forests model
set.seed(1)
model_rf <- randomForest(response ~ .,         # SDM formula (response as a function of all predictors)
                         data       = data,    # Data dataframe
                         ntree      = 1000,    # Number of trees to build
                         importance = TRUE,    # Calculate variable importance
                         classwt    = c(1, 5)) # Class weights (for unbalanced data)

# Check model output and confusion matrix
model_rf

# Variable importance plot
varImpPlot(model_rf, 
           main = "Variable Importance",
           type = 1) # Type 1 for mean decrease in accuracy


# Predict the distribution of the collared pika
predict_pika <- c(predict(predictors, model_rf, type = "response"),
                  predict(predictors, model_rf, type = "prob"))


# Rename raster layers for clarity
names(predict_pika) <- c("prediction", "prob_neg", "prob_pos")
predict_pika

par(mfrow = c(1, 2))
plot(predict_pika$prob_pos, main = "Probability of Presence")
plot(predict_pika$prediction, main = "Prediction")
par(mfrow = c(1, 1))

prediction_raster <- predict_pika[["prediction"]]

# Convert to integer explicitly
prediction_raster <- round(prediction_raster)
prediction_raster <- as.numeric(prediction_raster)  # ensure it's numeric
prediction_raster <- terra::classify(prediction_raster, rbind(c(-Inf, 0.5, 0), c(0.5, Inf, 1)))

# Set levels (categories)
levels <- data.frame(value = c(0, 1), 
                     category = c("Absence", "Presence"))
levels(prediction_raster) <- levels

# Save SDM
writeRaster(predict_pika$prediction,
            filename = "C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/data/SDM/sdm_final.tif",
            overwrite = TRUE,
            datatype = "INT1U")
