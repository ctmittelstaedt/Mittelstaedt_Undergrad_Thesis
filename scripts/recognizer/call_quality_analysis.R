# Call quality analysis

library(tidyverse)
library(tidybayes)
library(brms) 
library(ggplot2)
library(gghalves)
library(ggeffects)
library(sjPlot)
library(RColorBrewer)


call_quality <- read_csv(file.path("data","recognizer","recognizer_call_quality_assessment.csv"))

# Check for collinearity
cor(call_quality$harmonics,call_quality$power_density)

# Set prior
prior1 <- c(set_prior(prior = 'normal(4,6)', class='b', coef='harmonics'),
            set_prior(prior = 'normal(0,6)', class='b', coef='logharmonics'),
                 set_prior(prior = 'normal(-13,6)', class='Intercept', coef=''))

# Simple model - no power_density because collinearity issue
quality_model_1  <- brm(predict_score ~ harmonics+log(harmonics),
                       call_quality,
                       cores = 3,
                       chains = 3,
                       prior = prior1,
                       iter = 5000,
                       warmup = 2000)

## look at the distribution of the parameters, look at effective sample size ESS
summary(quality_model_1)
## summary of the fixed effects
fixef(quality_model_1)
## trace the plots to check convergence
plot(quality_model_1)
## plot a "goodness of fit" plot, compare your model distribution to the poster distriubtion of the data
pp_check(quality_model_1)

dummydt <- data.frame(harmonics = seq(from=1,to=9,length.out=30))

## create a data frame of predictions of our model
predictions <- add_epred_draws(dummydt,
                              quality_model_1,
                              re_formula = predict_score ~ harmonics + log(harmonics),
                              ndraws = 1000,
                              seed=1
                              )


# Plot number of harmonics vs score
ggplot(call_quality, aes(harmonics, predict_score)) +
  stat_lineribbon(data = predictions, aes(y = .epred), .width = c(0.95), alpha = 0.25, fill = "#999933") +  # 95% credible interval
  stat_lineribbon(data = predictions, aes(y = .epred), .width = c(0), alpha = 1, color = "#999933") +  # Mean prediction
  geom_point(color = "#999933", size = 2.5, shape=16) +  # Adjust point size and shape
  theme_bw() +  # Use a clean theme
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

ggsave("figures/call_quality.png", width = 8, height = 6, dpi = 300)



