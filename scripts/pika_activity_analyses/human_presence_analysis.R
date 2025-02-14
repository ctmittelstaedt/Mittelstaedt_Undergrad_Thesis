# Human presence vs pika activity
library(data.table)
library(tidyverse)
library(dplyr)


# Read the CSV file into a data.table
human_predictions_dt <- fread(file.path("data","pika_activity","all_human_dataset.csv"))

file_names_dt <- unique(human_predictions_dt[, .(file)])

filtered_h_predictions <- human_predictions_dt[seq(1, .N, by = 2) & PIKA > 10]

# Create the new data.table
new_table <- filtered_h_predictions[, .(
  site = sub(".*/([^/]+)$", "\\1", dirname(file)),  # Extract the site (folder name) from the file path
  date = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\1", file),  # Extract date from file name
  time = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\2", file),  # Extract time from file name
  count = .N  # Count the number of times the file name appears
), by = file]

# Make sure all filenames in file_names_dt appear, even those with count = 0
new_table_full <- merge(file_names_dt, new_table, by = "file", all.x = TRUE)

# Replace NAs in 'count' with 0 (for files that don't appear in filtered_h_predictions)
new_table_full[is.na(count), `:=`(
  count = 0,  # Set count to 0 for missing files
  site = sub(".*/([^/]+)$", "\\1", dirname(file)),  # Extract site again for missing files
  date = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\1", file),  # Extract date again for missing files
  time = sub(".*/[^/]+_(\\d{8})_(\\d{6})\\.wav", "\\2", file)   # Extract time again for missing files
)]

# View the final result
head(new_table_full)

humans_present_table <- new_table_full %>%
  mutate(humans_present = ifelse(date %in% c(20240808, 20240726, 20240718), "Y", "N"))

##########################BAYESIAN ANOVA#############################################
install.packages("BayesFactor")
library(BayesFactor)

humans_present_table$humans_present <- factor(humans_present_table$humans_present)
humans_present_table$site <- factor(humans_present_table$site)

human_model1 <- anovaBF( 
  count ~ humans_present + (1|site),
  data = humans_present_table
)

human_model1

# plot: what am I plotting? 
# Calculate means and 95% confidence intervals for each group
summary_data <- humans_present_table %>%
  group_by(humans_present) %>%
  summarise(
    mean_count = mean(count),  # Calculate mean count
    lower_ci = mean_count - qt(0.975, df = n() - 1) * sd(count) / sqrt(n()),  # Lower 95% CI
    upper_ci = mean_count + qt(0.975, df = n() - 1) * sd(count) / sqrt(n())   # Upper 95% CI
  )

ggplot(summary_data, aes(x = humans_present, y = mean_count)) +
  geom_point(size = 3, color = "blue") +  
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) + 
  labs(x = "Humans present", y = "Mean number of pika calls per recording") +
  theme_minimal()




