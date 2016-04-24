

library(data.table)
library(RCurl)
library(stargazer)
library(ri)


###### ANALYSIS OF PILOT STUDY ######

n.treat = 134
n.control = 133
n.treat.responded = 6
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


###### ANALYSIS OF ATG FEEDBACK SURVEY ######

# read data
csv = getURL('https://raw.githubusercontent.com/winlingit/w241-project-csw/master/rcode/atg_results.csv')
dt = data.table(read.csv(textConnection(csv)))

ATE = dt[ , .(y = sum(Responses)/sum(N)), by = treat][ , y[1]-y[2]] # estimate ATE from frequency table
ATE # estimated ATE = 0.073


# recover observations
dtx = dt[rep(seq(.N), N), !'N', with = F]  # expand table to 352 rows
dtx[ , responded := c(rep(1, max(Responses)), rep(0, .N - max(Responses))), by = CollectorId]  # add var for responded
dt$Responses == dtx[ , sum(responded), by = CollectorId]$V1  # checksums for total responses for each collector


### REGRESSION ANALYSIS

# nest models
m1 = lm(responded ~ treat, data = dtx)  # treatment only
summary(m1)

m2 = lm(responded ~ treat + Org, data = dtx)  # treatment + org
summary(m2)

m3 = lm(responded ~ treat + Org + Female, data = dtx)  # treatment + org + sex
summary(m3)

coeff = summary(m3)$coefficients
ci = coeff[2,1] + c(-1.96, 1.96)*coeff[2,2]  # 95% CI
ci

# display results
stargazer(m1, m2, m3, type='text')

# estimated ATE = 0.073 (SE = 0.026) is significant (p < 0.01)
# Org and sex covariates were not predictive of response rate
# no significant interaction effects


###### ALTERNATIVE ANALYSIS USING RANDOMIZATION INFERENCE #######

### 1. Analysis with "ri" package

## NOTE: This code is largely copy/pasted and hacked from the example code
# for the dt package
y = dtx$responded
Z = dtx$treat
block = dtx$blocknum

# Estimate ATE without blocking
ate.noblock = estate(y, Z); ate.est
perms.noblock <- genperms(Z) # all possible permutations
probs.noblock <- genprobexact(Z) # probability of treatment
ate.noblock <- estate(y,Z,prob=probs.noblock) # estimate the ATE
ate.noblock

Ys <- genouts(y,Z,ate=0) # generate potential outcomes under sharp null of no effect
distout.noblock <- gendist(Ys,perms.noblock, prob=probs.noblock) # generate sampling dist. under sharp null
dispdist(distout.noblock, ate.noblock)  # display characteristics of sampling dist. for inference


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


### 2. Analysis with manual RI

# from randomization.R
assign.treatment <- function(n) {
        n.treat = round(n / 2)  # Uses banker's rounding
        n.control = n - n.treat
        data = sample(c(rep(1, n.treat), rep(0, n.control)))
        data
}

# generate sampling distribution for ATE
sim.ate = function(dt) {
        dt[ , treat.ri := assign.treatment(.N), by = block] # randomize assignments in each block
        ATE = dt[ , .(y = sum(responded)/.N), by = treat.ri][ , y[1]-y[2]] # estimate ATE for each assignment
        ATE
}

ate.dist = replicate(10000, sim.ate(dtx)) # generate sampling distribution for ATE
plot(density(ate.dist), main = "Distribution of ATE", col = "red", xlab = NA)

p.value = 2*mean(ate.dist >= ATE) # return 2-tailed p-value for estimated ATE
p.value # p-value < 0.01
