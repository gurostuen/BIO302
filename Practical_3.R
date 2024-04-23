### Practical 3 ###

## GLS ##

# With the built-in `iris` data:

# Make a plot to show how `Petal.Length` varies between species.

library(ggplot2)
library(tidyverse)
data("iris")

iris_plot <- ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_boxplot(aes(fill = Species)) +
  theme_bw()

iris_plot

# Find the variance of `Petal.Length` for each species.

lm_iris <- lm(Petal.Length ~ Species, data = iris)
lm_iris

iris |> group_by(Species) |> 
  summarise(var(Petal.Length))

# Fit an anova using `lm` between `Petal.Length` and species and examine the diagnostic plots.

anova(lm_iris) 
plot(lm_iris) 

# Fit a `gls` for the same model. Have the coefficients changed?

library(nlme)
gls_iris <- nlme::gls(Petal.Length ~ Species, data = iris)
summary(gls_iris)
  
# Fit a `gls` for the same model but allow the variance to be different for each species. 

fit_gls_iris <- nlme::gls(Petal.Length ~ Species, data = iris, weights = varIdent(form = ~ +1|Species))
summary(fit_gls_iris)

# Use `AIC` to test if this is a better model.

anova(gls_iris, fit_gls_iris)



## NLS ##

# Import data amount.csv

calcium <- read.csv("amount.csv")

# Do a non-linear regression

calcium_plot <- ggplot(calcium, aes(x = calcium, y = amount)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

calcium_plot

nls_calcium <- nls(amount ~ b0 + b1 * exp(b2 * calcium),
                   data = calcium,
                   start = c(b0 = 0, b1 = 25, b2 = -1))

summary(nls_calcium)

# Interpret the results



# What is the expected value if calcium = 10?

new_data <- data.frame(calcium = 10)  
predicted_value <- predict(nls_calcium, newdata = new_data)

# Print the predicted value
print(predicted_value)