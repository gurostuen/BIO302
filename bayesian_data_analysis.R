### EXERCISE 1: Bayesian A testing for Swedish Fish Incorporated ###



# Question I) Build a Bayesian model that answers the question: What would the rate of sign-up 
# be if method A was used on a larger number of people?

n_draws <- 20000 # Number of random draws from the prior

prior_rate <- runif(n_draws, 0, 1) # Defining and drawing from the prior distribution 
hist(prior_rate) # Uniform prior

# Defining the generative model
gen_model <- function(rate) {
  subscribers <- rbinom(1, size = 16, prob = rate) # Binomial distribution
  subscribers
}

# Simulating the data using the parameters from the prior and the generative model
subscribers <- rep(NA, n_draws)
for(i in 1:n_draws) {
  subscribers[i] <- gen_model(prior_rate[i])
}

post_rate <- prior_rate[subscribers == 6] # Filtering off all draws that do not match the data

length(post_rate) # See that we got enough draws left after the filtering, should aim for >1000 draws.

hist(post_rate, xlim = c(0, 1))

mean(post_rate)

quantile(post_rate, c(0.025, 0.975))



# Question II) What’s the probability that method A is better than telemarketing?

sum(post_rate > 0.2) / length(post_rate)



# Question III) If method A was used on 100 people what would be number of sign-ups?

# Since rbinom is vectorized we can simply write it like this:
signups <- rbinom(n = length(post_rate), size = 100, prob = post_rate)

hist(signups, xlim = c(0, 100))

quantile(signups, c(0.025, 0.975))






### EXERCISE 2: Bayesian A/B testing for Swedish Fish Incorporated with Stan ###



# Question I) Build a Bayesian model in Stan that answers the question: What is the probability that method B is better than method A?

library(rstan)

# The Stan model as a string.
model_string <- "
data {
  // Number of trials
  int nA;
  int nB;
  // Number of successes
  int sA;
  int sB;
}

parameters {
  real<lower=0, upper=1> rateA;
  real<lower=0, upper=1> rateB;
}

model {
  rateA ~ uniform(0, 1);
  rateB ~ uniform(0, 1);
  sA ~ binomial(nA, rateA);
  sB ~ binomial(nB, rateB); 
}

generated quantities {
  real rate_diff;
  rate_diff = rateB - rateA;
}
"

data_list <- list(nA = 16, nB = 16, sA = 6, sB = 10)

# Compiling and producing posterior samples from the model.
stan_samples <- stan(model_code = model_string, 
                     data = data_list)

# Plotting and summarizing the posterior distribution
stan_samples
traceplot(stan_samples) # See if the model converged
plot(stan_samples)

# Export the samples to a data.frame for easier handling.
posterior <- as.data.frame(stan_samples)

# Now we could, for example, calculate the probability that the rate is higher than, say, 20%
sum(posterior$rate > 0.2) / length(posterior$rate )

sum(posterior$rate_diff > 0) / length(posterior$rate_diff) # So with around 90% probability rate B is higher than rate A. 



# Question II) Change the model so that it uses a more informative prior. What is now the probability that method B is better than method A?

hist(rbeta(9999, shape1 = 3, shape2 = 25), xlim=c(0, 1), 30)
lines(c(0.05, 0.15), c(0,0), col="red", lwd = 3)



# Question III: So what should we do? Make a simple decision analysis.

posterior <- as.data.frame(stan_samples)

profitA <- -30 + posterior$rateA * 1000 
profitB <- -300 + posterior$rateB * 1000 

hist(profitA)
hist(profitB)

hist(profitA - profitB)
expected_profit_diff <- mean(profitA - profitB)
abline(v = expected_profit_diff, col = "red", lwd =2)



# Testing brms package

library(brms)
plant_data <- iris

model <- brm(Petal.Length ~ Sepal.Length, data = plant_data)
summary(model)
