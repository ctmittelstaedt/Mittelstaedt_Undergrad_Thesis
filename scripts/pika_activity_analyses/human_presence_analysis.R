##################################################################################
# Bayesian model of human presence vs pika activity
# Charlotte Mittelstaedt - The University of British Columbia
# Created 9 February 2025
##################################################################################

library(data.table)
library(tidyverse)
library(dplyr)
library(brms)
library(ggplot2)
library(viridis)
library(tidybayes)

## Data processing
# Read prediction score CSV file into a data.table
human_predictions_dt <- fread(file.path("data","pika_activity","predict_score_full_human_dataset.csv"))

# Get unique file names
file_names_dt <- unique(human_predictions_dt[, .(file)])

# Remove clip overlap and set score threshold to 10
unoverlap_h_predictions <- human_predictions_dt[!grepl("\\.\\d5$", end_time), ]
filtered_h_predictions <- unoverlap_h_predictions[PIKA > 10]

print(unique(filtered_h_predictions$file))
print(table(processed_pikaru$ARU))
print(unique(unoverlap_h_predictions$file))

# Extract features from file name
new_table <- filtered_h_predictions[, .(
  site = sub(".*/([^/]+)$", "\\1", dirname(file)),  # Extract the site (folder name) from the file path
  date = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\1", file),  
  time = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\2", file), 
  ARU = sub("([^/]+)_[^_]+_.+\\.wav", "\\1", basename(file)),  
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

# Save processed file
saveRDS(humans_present_table, file = "data/pika_activity/full_human_dataset.RData")

# Read in processed file
humans_present_table <- readRDS("data/pika_activity/full_human_dataset.RData")

# Make categorical variables factors
humans_present_table$humans_present <- factor(humans_present_table$humans_present)
humans_present_table$site <- factor(humans_present_table$site)
humans_present_table$date <- factor(humans_present_table$date)
humans_present_table$time <- factor(humans_present_table$time)
humans_present_table$ARU <- factor(humans_present_table$ARU)

# Set priors
human_prior <- c(set_prior(prior = 'normal(0,3)', class='b', coef='humans_presentY'),
                 set_prior(prior = 'normal(1,3)', class='Intercept', coef='')
                 )	
  
# Model
human_model2 <- brm(count ~ humans_present + (1|site) +(1|date)+(1|ARU),
                    data = humans_present_table,
                    family = poisson(),
                    prior = human_prior,
                    cores = 3,
                    chains = 3,
                    iter = 5000,
                    warmup = 2000,
                    control = list(adapt_delta = 0.9999, max_treedepth = 15)
                    )

# Save model
saveRDS(human_model2, "human_model2.rds")

# Read in model
human_model2 <- readRDS("human_model2.rds")

# Look at the distribution of the parameters, look at effective sample size ESS
summary(human_model2)
# Summary of the fixed effects
fixef(human_model2)
# Random effects
ranef(human_model2)
# Trace plots to check convergence
plot(human_model2)
# Goodness of fit plot, compare model distribution to the poster distribution of the data
pp_check(human_model2)

# Make dot and whisker plot

# Model predictions
predictions <- add_epred_draws(humans_present_table, human_model2, re_formula = ~(1|site))

print(predictions)

# Site effect predictions
site_preds <- predictions %>%
  group_by(site, humans_present) %>%
  summarize(
    site_mean = mean(.epred),
    site_lower = quantile(.epred, probs = 0.025),
    site_upper = quantile(.epred, probs = 0.975)
  )

view(site_preds)

# Convert humans_present to numeric for positioning
site_preds$humans_present_numeric <- as.numeric(site_preds$humans_present)

# Fixed effect predictions
pred_summary <- predictions %>%
  group_by(humans_present) %>%
  summarize(
    mean_pred = mean(.epred), 
    lower = quantile(.epred, probs = 0.025),
    upper = quantile(.epred, probs = 0.975)
  )

view(pred_summary)

# Set up the color palette for sites
site_colors <- c(
  "PC"="#e59e62",
  "PP"="#b8901e",
  "MD"="#336061",
  "MB"="#547349"
)

# Make dot and whisker plot
ggplot() +
  # Add the fixed effect as a central point with whiskers
  geom_point(data = pred_summary, aes(x = humans_present, y = mean_pred), colour = "#0e1b5e", size = 3) +
  geom_errorbar(data = pred_summary, aes(x = humans_present, ymin = lower, ymax = upper), width = 0.2, colour = "#0e1b5e") +
  
  # Add faint dots and whiskers for each site
  geom_point(data = site_preds, aes(x = humans_present_numeric, y = site_mean, colour = site),
              alpha = 0.55, size = 2, width = 0.2,position = position_dodge(width = 0.4)) +  # Jitter the site-specific predictions
  
  geom_errorbar(data = site_preds, aes(x = humans_present_numeric, ymin = site_lower, ymax = site_upper, colour = site), 
                width = 0.2, alpha = 0.55,position = position_dodge(width = 0.4)) +
  
  scale_color_manual(values = site_colors) +  
  scale_x_discrete(labels = c("N" = "No", "Y" = "Yes")) +
  theme_bw() + 
  labs(x = "Humans present", y = "Number of calls per recording") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20, vjust = -1, margin=margin(b=10)),
    axis.title.y = element_text(size = 20, vjust = 4.5, margin=margin(l=10)),
    axis.text.x = element_text(size = 16, margin = margin(t = 5)),
    axis.text.y = element_text(size = 16, margin = margin(r = 5)),
    panel.border = element_blank(),        
    axis.line.x = element_line(size = 0.5),   
    axis.line.y = element_line(size = 0.5),   
    axis.ticks = element_line(size = 1),
    legend.title = element_blank(),
    legend.text = element_text(size=16)
  )

# Save plot
ggsave("figures/humans_present.png", width = 8, height = 6, dpi = 300)


