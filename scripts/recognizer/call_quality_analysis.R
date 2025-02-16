# Call quality analysis

library(tidyverse)
library(tidybayes)
library(brms) 

call_quality <- read_csv(file.path("data","recognizer","recognizer_call_quality_assessment.csv"))

# Create a scatterplot matrix to check collinearity
pairs(~ predict_score + harmonics + power_density, 
      data=call_quality, main="Scatterplot Matrix for call quality data")

# Double check
cor(call_quality$harmonics,call_quality$power_density)

# Simple model - no power_density because collinearity issue
quality_model_1  <- brm(predict_score ~ harmonics,
                       call_quality,
                       cores = 3,
                       chains = 3,
                       iter = 4000,
                       warmup = 1500)

## look at the distribution of the parameters, look at effective sample size ESS
summary(quality_model_1)
## summary of the fixed effects
fixef(quality_model_1)
## trace the plots to check convergence
plot(quality_model_1)
## plot a "goodness of fit" plot, compare your model distribution to the poster distriubtion of the data
pp_check(quality_model_1)


## create a data frame of predictions of our model
predictions <- add_predicted_draws(call_quality,
                                   quality_model_1,
                                   re_formula = predict_score ~ harmonics)


# Plot number of harmonics vs score
ggplot(call_quality,aes(harmonics,predict_score))+
  stat_lineribbon(data = predictions,aes(y = .prediction),.width = c(0.95), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .prediction),.width = c(0), alpha = 1)+ ## mean
  geom_point()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Number of harmonics",y="Prediction score")

# If we had included power_density
predictions <- add_epred_draws(call_quality,
                                   quality_model_1,
                                   re_formula = predict_score ~ power_density)


## results: this may or may not happen
class(quality_model_1)


