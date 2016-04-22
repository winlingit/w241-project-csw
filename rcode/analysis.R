# setwd("~/Documents/Berkeley/241/Final\ Project/w241-project-csw/rcode/")

library(data.table)
library(RCurl)
library(stargazer)
library(ri)

# Analysis of pilot
n.treat = 134
n.control = 133
n.treat.responded = 5
n.control.responded = 4

pilot.responded = c(rep(1, n.treat.responded),                # treat responses
                    rep(0, n.treat - n.treat.responded),      # treat non-responses
                    rep(1, n.control.responded),              # control responses
                    rep(0, n.control - n.control.responded))  # control non-responses
pilot.treat = c(rep(1, n.treat), rep(0, n.control))

pilot.perms <- genperms(pilot.treat) # all possible permutations
pilot.probs <- genprobexact(pilot.treat) # probability of treatment
pilot.ate <- estate(pilot.responded, pilot.treat, prob=pilot.probs) # estimate the ATE
pilot.ate

pilot.Ys <- genouts(pilot.responded, pilot.treat,ate=0) # generate potential outcomes under sharp null of no effect
pilot.distout <- gendist(pilot.Ys,pilot.perms, prob=pilot.probs) # generate sampling dist. under sharp null
dispdist(pilot.distout, pilot.ate)  # display characteristics of sampling dist. for inference

# Analysis of ATG feedback survey results

csv = getURL('https://raw.githubusercontent.com/winlingit/w241-project-csw/master/rcode/atg_results.csv')
dt = data.table(read.csv(textConnection(csv)))

dt[ , .(y = sum(Responses)/sum(N)), by = treat][ , y[1]-y[2]]

# estimated ATE = 0.073

# 2. recover observations
dtx = dt[rep(seq(.N), N), !'N', with = F]  # expand table to 352 rows
dtx[ , responded := c(rep(1, max(Responses)), rep(0, .N - max(Responses))), by = CollectorId]  # add var for responded
dt$Responses == dtx[ , sum(responded), by = CollectorId]$V1  # checksums for total responses for each collector

# 3. regression analysis
m1 = lm(responded ~ treat, data = dtx)  # treatment only
summary(m1)

m2 = lm(responded ~ treat + Org, data = dtx)  # treatment + org
summary(m2)

m3 = lm(responded ~ treat + Org + Female, data = dtx)  # treatment + org + sex
summary(m3)

coeff = summary(m3)$coefficients
ci = coeff[2,1] + c(-2,2)*coeff[2,2]  # 95% CI
ci

# 4. display results
stargazer(m1, m2, m3, type='text')


# stimated ATE = 0.073 (SE = 0.026) is significant (p < 0.01)
# Org and sex covariates were not predictive of response rate
# no significant interaction effects



###### ALTERNATIVE ANALYSIS USING RANDOMIZATION INFERENCE #######
## NOTE: This code is largely copy/pasted and hacked from the example code
# for the dt package
y = dtx$responded
Z = dtx$treat
block = dtx$blocknum

# Estimate ATE without blocking
ate.noblock = estate(y, Z); ate.est

# Estimate ATE with blocking
perms <- genperms(Z,blockvar=block) # all possible permutations
probs <- genprobexact(Z,blockvar=block) # probability of treatment
ate <- estate(y,Z,prob=probs) # estimate the ATE
ate


## Conduct Sharp Null Hypothesis Test of Zero Effect for Each Unit

Ys <- genouts(y,Z,ate=0) # generate potential outcomes under sharp null of no effect
distout <- gendist(Ys,perms, prob=probs) # generate sampling dist. under sharp null
dispdist(distout, ate)  # display characteristics of sampling dist. for inference

## Generate Sampling Distribution Around Estimated ATE

Ys <- genouts(y,Z,ate=ate) ## generate potential outcomes under tau = ATE
distout <- gendist(Ys,perms, prob=probs) # generate sampling dist. under tau = ATE
dispdist(distout, ate)  ## display characteristics of sampling dist. for inference
