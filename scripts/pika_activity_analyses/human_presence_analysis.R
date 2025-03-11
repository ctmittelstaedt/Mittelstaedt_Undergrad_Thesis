# Human presence vs pika activity
library(data.table)
library(tidyverse)
library(dplyr)
library(brms)
library(ggplot2)

#########IGNORE data processing!!!! Jump to line 46! ###############
# Read the CSV file into a data.table
human_predictions_dt <- fread(file.path("data","pika_activity","predict_score_full_human_dataset.csv"))

# Get unique file names
file_names_dt <- unique(human_predictions_dt[, .(file)])

# Remove clip overlap and set score threshold to 10
filtered_h_predictions <- human_predictions_dt[seq(1, .N, by = 2) & PIKA > 10]

# Extract features from file name
new_table <- filtered_h_predictions[, .(
  site = sub(".*/([^/]+)$", "\\1", dirname(file)),  # Extract the site (folder name) from the file path
  date = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\1", file),  # Extract date from file name
  time = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\2", file), # Extract time from file name
  ARU = sub("([^/]+)_[^_]+_.+\\.wav", "\\1", basename(file)),  # Correct regex to extract ARU code
  count = .N  # Count the number of times the file name appears
), by = file]

# Make sure all file names in file_names_dt appear, even those with count = 0
new_table_full <- merge(file_names_dt, new_table, by = "file", all.x = TRUE)

# Replace NAs in 'count' with 0
new_table_full[is.na(count), `:=`(
  count = 0,  # Set count to 0 for missing files
  site = sub(".*/([^/]+)$", "\\1", dirname(file)),  
  date = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\1", file),  
  time = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\2", file),
  ARU = sub("([^/]+)_[^_]+_.+\\.wav", "\\1", basename(file))
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
humans_present_table$date <- factor(humans_present_table$date)
humans_present_table$time <- factor(humans_present_table$time)
humans_present_table$ARU <- factor(humans_present_table$ARU)

# Set prior
human_prior <- c(set_prior(prior = 'normal(0,3)', class='b', coef='humans_presentY'),
                 set_prior(prior = 'gamma(1,10)', class='Intercept', coef='', lb=0)
                 )	
  
# Model
human_model2 <- brm(count ~ humans_present + (1|site) +(1|date)+(1|time)+(1|ARU),
                    data = humans_present_table,
                    family = poisson(),
                    prior = human_prior,
                    cores = 3,
                    chains = 3,
                    iter = 5000,
                    warmup = 2000,
                    control = list(adapt_delta = 0.9999, max_treedepth = 15)
                    )

## look at the distribution of the parameters, look at effective sample size ESS
summary(human_model2)
## summary of the fixed effects
fixef(human_model2)
ranef(human_model2)
## trace the plots to check convergence
plot(human_model2)
## plot a "goodness of fit" plot, compare your model distribution to the poster distribution of the data
pp_check(human_model2)


# Make dot and whisker plot
# Generate posterior draws for the model's effects
predictions <- add_epred_draws(humans_present_table, human_model2, re_formula = NULL)

site_preds <- predictions %>%
  group_by(site, humans_present) %>%
  summarize(
    site_mean = mean(.epred),
    site_lower = quantile(.epred, probs = 0.025),
    site_upper = quantile(.epred, probs = 0.975)
  )

# Convert humans_present to numeric for positioning
site_preds$humans_present_numeric <- as.numeric(site_preds$humans_present)

# Prepare the fixed effect predictions
pred_summary <- predictions %>%
  group_by(humans_present) %>%
  summarize(
    mean_pred = mean(.epred), 
    lower = quantile(.epred, probs = 0.025),
    upper = quantile(.epred, probs = 0.975)
  )

# Set up the color palette for sites
site_colors <- brewer.pal(length(unique(site_preds$site)), "Set1")

# Plot
ggplot() +
  # Add the fixed effect (mean) as a central point with whiskers
  geom_point(data = pred_summary, aes(x = humans_present, y = mean_pred), colour = "black", size = 3) +
  geom_errorbar(data = pred_summary, aes(x = humans_present, ymin = lower, ymax = upper), width = 0.2, colour = "black") +
  
  # Add faint dots and whiskers for each site, spread symmetrically around the fixed effect
  geom_point(data = site_preds, aes(x = humans_present_numeric, y = site_mean, colour = site),
              alpha = 0.3, size = 2, width = 0.2) +  # Jitter the site-specific predictions
  
  geom_errorbar(data = site_preds, aes(x = humans_present_numeric, ymin = site_lower, ymax = site_upper, colour = site), 
                width = 0.2, alpha = 0.3) +
  
  # Customize the theme and labels
  scale_color_manual(values = site_colors) +  # Apply unique colors to each site
  scale_x_discrete(labels = c("N" = "No", "Y" = "Yes")) +
  theme_bw() + 
  labs(x = "Humans present", y = "Number of calls") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 14, vjust = -1),
    axis.title.y = element_text(size = 14, vjust = 3),
    axis.text.x = element_text(size = 12),  # Make sure x-axis text is readable
    panel.border = element_blank(),         # Remove the border
    axis.line.x = element_line(size = 0.5),   # Keep the bottom axis line
    axis.line.y = element_line(size = 0.5),   # Keep the left axis line
    axis.ticks = element_line(size = 1),
    legend.title = element_blank() 
  )



