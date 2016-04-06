# W241 Field Experiment: Survey Responses
# Carlos, Winston & Sean

library(data.table)
library(parallel)
library(pwr)

# Data Analysis Code

# Assumed variables:
# - Treatment/Control
# - Sex
# - Age
# - Title

#################################
#### Configuration variables ####
#################################
n <- 400  # Population size
pct_male = .5
age_min <- 22
age_max <- 70
titles <- c("Account Executive", "Business Analyst", "Customer Support", "Programmer")
pct_treat <- .5
baseline_outcome <- .25
ate <- .05

#################################
####### Utility functions #######
#################################
# Builds covariate data given parameters
# Parameters:
#   pct_male : Approx % male
#   age_min  : Minimum age 
#   age_max  : Maximum age
#   titles   : Vector of job titles (strings)
# Returns
#   A data table with:
#    binary column for male
#    age, taken from a uniform distribution on the given range
#    title, a factor taken by sampling the titles
build_cov_data <- function(pct_male, age_min, age_max, titles) {
  male <- rbinom(n, 1, pct_male)
  age <- floor(runif(n, age_min, age_max))
  title <- factor(sample(titles, n, replace=TRUE))
  data.table(male, age, title)
}

# Generates a random binary vector with with the specified 
# size and mean
get_random_assignment <- function(n, percentage) {
  n_ones <- n * percentage
  n_zeroes <- n - n_ones
  vals <- c(sample(1, n_ones, replace=TRUE),
            sample(0, n_zeroes, replace=TRUE))
  sample(vals, n)
}

assign_treatment <- function(data, pct_treat) {
  # Assign treatment
  data[, treat := get_random_assignment(nrow(data), pct_treat)]
  # Assign actual outcome
  data[, y := (y_1 * treat) + (y_0 * (1 - treat))]
}

# Assigns potential outcomes to the data.
# Parameters:
#   data     : The data to apply the outcomes to (data table)
#   mean_Y_0 : The % of respondents for control
#   ate      : Number of % point change in response from treatment
assign_potential_outcomes <- function (data, mean_Y_0, ate) {
  n <- nrow(data)
  data[, y_0 := get_random_assignment(n, mean_Y_0)]
  data[, y_1 := get_random_assignment(n, mean_Y_0 + ate)]
}

# Estimates ATE. 
# Assumes treat and y columns assigned as per functions above
# Can also manually assign treatment vector
estimate_ate <- function(data, treatment = NULL) {
  if(is.null(treatment)) {
    treatment <- data$treat
  }
  mean(data[treatment == 1]$y) - mean(data[treatment == 0]$y)
}
 
# Estimates P-value using randomization inference under sharp null
# Assumes y column is populated as per function above
estimate_p <- function(data, repetitions = 1000) {
  do_test_iteration <- function(data) {
    treat_hyp <- sample(data$treat, nrow(data))
    estimate_ate(data, treat_hyp)
  }
  est_ate <- estimate_ate(data)
  ates <- replicate(repetitions, do_test_iteration(data))

  is_greater <- abs(ates) >= abs(est_ate)
  mean(is_greater)
}

#################################
############ Testing ############
#################################

data <- build_cov_data(pct_male, age_min, age_max, titles)
assign_potential_outcomes(data, baseline_outcome, ate)
assign_treatment(data, pct_treat)

estimate_ate(data)
estimate_p(data)

#################################
## Statistical Power Estimates ##
#################################

data <- build_cov_data(pct_male, age_min, age_max, titles)

ates <- seq(-.25, .25, by=.01)
powers <- sapply(ates, function(ate) pwr.2p.test(ate, 800 / 2)$power)
plot(ates, powers)
