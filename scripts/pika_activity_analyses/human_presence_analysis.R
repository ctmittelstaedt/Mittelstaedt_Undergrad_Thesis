# Human presence vs pika activity
library(data.table)
library(tidyverse)
library(dplyr)
library(brms)
library(ggplot2)

#########IGNORE data processing!!!! Jump to line 46! ###############
# Read the CSV file into a data.table
human_predictions_dt <- fread(file.path("data","pika_activity","all_human_dataset.csv"))

# Get unique file names
file_names_dt <- unique(human_predictions_dt[, .(file)])

# Remove clip overlap and set score threshold to 10
filtered_h_predictions <- human_predictions_dt[seq(1, .N, by = 2) & PIKA > 10]

# Extract features from file name
new_table <- filtered_h_predictions[, .(
  site = sub(".*/([^/]+)$", "\\1", dirname(file)),  # Extract the site (folder name) from the file path
  date = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\1", file),  # Extract date from file name
  time = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\2", file),  # Extract time from file name
  count = .N  # Count the number of times the file name appears
), by = file]

# Make sure all file names in file_names_dt appear, even those with count = 0
new_table_full <- merge(file_names_dt, new_table, by = "file", all.x = TRUE)

# Replace NAs in 'count' with 0
new_table_full[is.na(count), `:=`(
  count = 0,  # Set count to 0 for missing files
  site = sub(".*/([^/]+)$", "\\1", dirname(file)),  
  date = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\1", file),  
  time = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\2", file)   
)]

head(new_table_full)

# Add column for human presence
humans_present_table <- new_table_full %>%
  mutate(humans_present = ifelse(date %in% c(20240808, 20240726, 20240718), "Y", "N"))

saveRDS(humans_present_table, file = "data/pika_activity/full_human_dataset.RData")



###########START HERE: ##########################
humans_present_table <- readRDS("data/pika_activity/full_human_dataset.RData")

##########################BAYESIAN ANOVA#############################################

# Make categorical variables factors
humans_present_table$humans_present <- factor(humans_present_table$humans_present)
humans_present_table$site <- factor(humans_present_table$site)

# Set prior
human_prior <- c(set_prior(prior = 'normal(0,1)', class='b', coef='humans_presentY'))	
  
# Model
human_model2 <- brm(count ~ humans_present + (1|site),
                    data = humans_present_table,
                    family = poisson(),
                    prior = human_prior,
                    cores = 3,
                    chains = 3,
                    iter = 5000,
                    warmup = 2000,
                    control = list(adapt_delta = 0.99)
                    )

## look at the distribution of the parameters, look at effective sample size ESS
summary(human_model2)
## summary of the fixed effects
fixef(human_model2)
## trace the plots to check convergence
plot(human_model2)
## plot a "goodness of fit" plot, compare your model distribution to the poster distribution of the data
pp_check(human_model2)

# Make dots and whisker plot


