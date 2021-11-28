library(fitdistrplus)
library(gamlss)
library(tidyverse)

# load the data in, get rid of NAs, calculate proportions and make percentile a factor
### I would suggest calculating your proportions in Excel - convert your 'light count' to a proportion, then '1 - light proportion' is your dark proportion!
data <- read_delim("~/Desktop/copebuds_dph_chronic.csv", delim = ',') %>%
  na.omit() %>%
  mutate(prop = copepod_count/100,
         percentile = as.factor(percentile)) 

# check your distribution graph
### this graph doesn't have the binomial families so let us know what this looks like and we can chat!
descdist(data$dark_prop)

# visualize your distribution fits 
fit.gamma <- fitdist(data$dark_prop, "gamma")
fit.beta <- fitdist(data$dark_prop, "beta")
plot(fit.gamma)
plot(fit.beta)

# fit your data to the specific distributions 
mGA <- histDist(data$prop, "GA", density = T, main = "Gamma")
mBE <- histDist(data$prop, "BE", density = T, main = "Beta")

# model with the lowest AIC value is the best fit
GAIC(mGA, mBE)

# construct your full model with the correct model function and distribution
mod <- glm(prop ~ treatment + time + treatment:time, family = Gamma(link = "inverse"), data = data)

### next steps: backwards model selection! 

