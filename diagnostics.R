library(performance)
library(DHARMa)

data <- iris
mod <- lm(data = data, Sepal.Length ~ Sepal.Width)

plot(mod)

performance::check_model(mod, detrend = FALSE)

vignette("DHARMa", package="DHARMa")

simulationOutput <- simulateResiduals(mod)

plot(simulationOutput)
plotQQunif(mod) 
plotResiduals(mod)

testDispersion(mod)

testCategorical(simulationOutput, catPred = iris$Species)
