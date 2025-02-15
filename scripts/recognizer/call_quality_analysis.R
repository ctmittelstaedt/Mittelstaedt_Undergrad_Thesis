# Call quality analysis
library(tidyverse) # data manipulation (includes ggplot2)
library(tidybayes) # easy retrivale of BRM prediction
library(brms) # bayesian model fitting

call_quality <- read_csv(file.path("data","recognizer","recognizer_call_quality_assessment.csv"))

# Create a scatterplot matrix to test multicolinearity
pairs(~ predict_score + harmonics + power_density, 
      data=call_quality, main="Scatterplot Matrix for call quality data")

# Set prior* default prior but could change to poisson for harmonic integer
prior1 <- c(set_prior(prior = 'normal(0,6)', class='b', coef='harmonics'), 	
            # global slope belongs to a normal distribution centered around 0
            set_prior(prior = 'normal(0,6)', class='b', coef='power_density'),
            set_prior(prior = 'normal(0,6)', class='Intercept', coef=''))  
            # global intercept


# Write simple model
cor(call_quality$harmonics,call_quality$power_density)

quality_model_1  <- brm(predict_score ~ harmonics + I(harmonics^2) + power_density ,
                       call_quality,
                     # family = ,
                     # prior = prior1,
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
                                   re_formula = predict_score ~ harmonics + I(harmonics^2))


# Number of harmonics vs score
ggplot(call_quality,aes(harmonics,predict_score))+
  stat_lineribbon(data = predictions,aes(y = .prediction),.width = c(0.2), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .prediction),.width = c(0), alpha = 1)+ ## mean
  geom_point()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Number of harmonics",y="Prediction score")


predictions <- add_epred_draws(call_quality,
                                   quality_model_1,
                                   re_formula = predict_score ~ power_density)


# Number of harmonics vs score
ggplot(call_quality,aes(power_density,predict_score))+
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0.2), alpha = 0.25)+ ## 95% credible interval
  stat_lineribbon(data = predictions,aes(y = .epred),.width = c(0), alpha = 1)+ ## mean
  geom_point()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme_bw()+
  labs(x= "Average power density",y="Prediction score")

## results: this may or may not happen
class(quality_model_1)


