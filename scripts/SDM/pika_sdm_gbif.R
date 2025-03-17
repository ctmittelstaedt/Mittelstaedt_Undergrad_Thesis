#Pika SDM

library(terra)
library(geodata)
library(predicts)

# Download climate data
bioclim_data <- worldclim_global(var = "bio",
                                 res = 2.5,
                                 path = "data/SDM")

# Read in pika observations
obs_data <- read.csv(file = "data/SDM/GBIF/pika_occurrence_GBIF.csv")

# Check the data to make sure it loaded correctly
summary(obs_data)

# Notice NAs - drop them before proceeding
obs_data <- obs_data[!is.na(obs_data$decimalLongitude), ]

# Make sure those NA's went away
summary(obs_data)

# Determine geographic extent of our data
max_lat <- 70
min_lat <- 57.5
max_lon <- -122
min_lon <- -160


geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))

# Download data with geodata's world function to use for our base map
world_map <- world(resolution = 3,
                   path = "data/SDM")

# Crop the map to our area of interest
my_map <- crop(x = world_map, y = geographic_extent)

# Plot the base map
plot(my_map,
     axes = T, 
     col = "grey95")

# Add the points for individual observations
points(x = obs_data$decimalLongitude, 
       y = obs_data$decimalLatitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

# Make an extent that is 25% larger
sample_extent <- geographic_extent * 1.25

# Crop bioclim data to desired extent
bioclim_data <- crop(x = bioclim_data, y = sample_extent)

# Plot the first of the bioclim variables to check on cropping
plot(bioclim_data[[1]])

# Set the seed for the random-number generator to ensure results are similar
set.seed(20210707)

# Randomly sample points (same number as our observed points)
background <- spatSample(x = bioclim_data,
                         size = 882,    # generate 882 pseudo-absence points
                         values = FALSE, # don't need values
                         na.rm = TRUE,   # don't sample from ocean
                         xy = TRUE)      # just need coordinates

# Look at first few rows of background
head(background)

# Plot the base map
plot(my_map,
     axes = TRUE, 
     col = "grey95")

# Add the background points
points(background,
       col = "grey30",
       pch = 1,
       cex = 0.75)

# Add the points for individual observations
points(x = obs_data$decimalLongitude, 
       y = obs_data$decimalLatitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

# Pull out coordinate columns, x (longitude) first, then y (latitude) from 
# saguaro data
presence <- obs_data[, c("decimalLongitude", "decimalLatitude")]
# Add column indicating presence
presence$pa <- 1

# Convert background data to a data frame
absence <- as.data.frame(background)
# Update column names so they match presence points
colnames(absence) <- c("decimalLongitude", "decimalLatitude")
# Add column indicating absence
absence$pa <- 0

# Join data into single data frame
all_points <- rbind(presence, absence)

# Reality check on data
head(all_points)

bioclim_extract <- extract(x = bioclim_data,
                           y = all_points[, c("decimalLongitude", "decimalLatitude")],
                           ID = FALSE) # No need for an ID column

# Add the point and climate datasets together
points_climate <- cbind(all_points, bioclim_extract)

# Identify columns that are latitude & longitude
drop_cols <- which(colnames(points_climate) %in% c("decimalLongitude", "decimalLatitude"))
drop_cols # print the values as a reality check

# Remove the geographic coordinates from the data frame
points_climate <- points_climate[, -drop_cols]

# Create vector indicating fold
fold <- folds(x = points_climate,
              k = 5,
              by = points_climate$pa)

table(fold)

testing <- points_climate[fold == 1, ]
training <- points_climate[fold != 1, ]

# Build a model using training data
glm_model <- glm(pa ~ ., data = training, family = binomial())

# Get predicted values from the model
glm_predict <- predict(bioclim_data, glm_model, type = "response")

# Print predicted values
plot(glm_predict)

# Use testing data for model evaluation
glm_eval <- pa_evaluate(p = testing[testing$pa == 1, ],
                        a = testing[testing$pa == 0, ],
                        model = glm_model,
                        type = "response")

# Determine minimum threshold for "presence"
glm_threshold <- glm_eval@thresholds$max_spec_sens

# Plot base map
plot(my_map, 
     axes = TRUE, 
     col = "grey95")

# Only plot areas where probability of occurrence is greater than the threshold
plot(glm_predict > glm_threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = "olivedrab")

# And add those observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "black",
       pch = "+", 
       cex = 0.75)

# Redraw those country borders
plot(my_map, add = TRUE, axes = T, border = "grey5")

glm_predict > glm_threshold

# Plot base map
plot(my_map, 
     axes = T, 
     col = "white")

# Only plot areas where probability of occurrence is greater than the threshold
plot(glm_predict > glm_threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = c(NA, "olivedrab")) # <-- Update the values HERE

# And add those observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "black",
       pch = "+", 
       cex = 0.75)

library(sf)
# Canadian borders
canada_provinces <- st_read("data/SDM/lpr_000b16a_e.shp")
canada_provinces <- st_transform(canada_provinces, crs = st_crs(my_map))

# Redraw those country borders
plot(my_map, add = TRUE, border = "grey5",
     )

plot(st_geometry(canada_provinces), 
     add = TRUE, 
     border = "grey5")

str(canada_provinces)

ggplot(data = canada_provinces) +
  geom_sf(color = "blue", fill = NA, size = 1.5)
