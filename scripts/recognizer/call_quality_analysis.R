##################################################################################
# Bayesian model of call quality vs recognizer score
# Charlotte Mittelstaedt - The University of British Columbia
# Created 12 February 2025
##################################################################################

library(tidyverse)
library(tidybayes)
library(brms) 
library(ggplot2)
library(gghalves)
library(ggeffects)
library(sjPlot)
library(RColorBrewer)

# Read csv
call_quality <- read_csv(file.path("data","recognizer","recognizer_call_quality_assessment.csv"))

# Check for collinearity
cor(call_quality$harmonics,call_quality$power_density) 

# Multicollinearity issue - remove power_density

# Set priors
prior1 <- c(set_prior(prior = 'normal(4,6)', class='b', coef='harmonics'),
            set_prior(prior = 'normal(0,6)', class='b', coef='logharmonics'),
                 set_prior(prior = 'normal(-13,6)', class='Intercept', coef=''))

# Simple model
quality_model_1  <- brm(predict_score ~ harmonics+log(harmonics),
                       call_quality,
                       cores = 3,
                       chains = 3,
                       prior = prior1,
                       iter = 5000,
                       warmup = 2000)

# Save model
saveRDS(quality_model_1, "quality_model_1.rds")

# Read model
quality_model_1 <- readRDS("quality_model_1.rds")

# Look at the distribution of the parameters, look at effective sample size ESS
summary(quality_model_1)
# Summary of the fixed effects
fixef(quality_model_1)
# Trace plots to check convergence
plot(quality_model_1)
# Goodness of fit plot, compare model distribution to the poster distribution of the data
pp_check(quality_model_1)

# Create dummy data table to smooth plot
dummydt <- data.frame(harmonics = seq(from=1,to=9,length.out=30))

# Predictions data frame
predictions <- add_epred_draws(dummydt,
                              quality_model_1,
                              re_formula = predict_score ~ harmonics + log(harmonics),
                              ndraws = 1000,
                              seed=1
                              )


# Plot number of harmonics vs score
ggplot(call_quality, aes(harmonics, predict_score)) +
  stat_lineribbon(data = predictions, aes(y = .epred), .width = c(0.95), alpha = 0.25, fill = "#1a3f60") +  # 95% credible interval
  stat_lineribbon(data = predictions, aes(y = .epred), .width = c(0), alpha = 1, color = "#1a3f60") +  # Mean prediction
  geom_point(color = "#1a3f60", size = 2.5, shape=16) +  # Adjust point size and shape
  theme_bw() +  
  labs(x = "Number of harmonics", y = "Prediction score") +
  scale_x_continuous(breaks = seq(0, 9, 1), labels = seq(0, 9, 1)) +  # Ensure x-axis has integer values
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(size = 17, vjust = -1.5, margin=margin(b=10)), 
    axis.title.y = element_text(size = 17, vjust = 1.5, margin=margin(l=10)),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    panel.border = element_blank(), 
    axis.line = element_line(color = "black", size = 0.5)
  )

# Save plot
ggsave("figures/call_quality.png", width = 8, height = 6, dpi = 300)



