
library(data.table)
library(ggplot2)
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

dt.pilot = data.table(treat = pilot.treat, response = pilot.responded)
dt.pilot = dt.pilot[, .N, by=c('response', 'treat')]
dt.pilot[response == 1, Responded := 'yes']
dt.pilot[response == 0, Responded := 'no']
dt.pilot[, Responded := factor(Responded)]
dt.pilot[treat == 1, treated := 'treatment']
dt.pilot[treat == 0, treated := 'control']
dt.pilot[, treated := factor(treated)]

# Plot the chart of shame
ggplot(dt.pilot, aes(x=Responded, y=N, fill=Responded)) + 
  geom_bar(stat='identity') +
  geom_text(aes(x=Responded, y=N, label=N), vjust=-.1) +
  facet_wrap( ~ treated) +
  ggtitle("Pilot results") + 
  xlab("") + 
  ylab("Responses") + 
  theme( axis.line=element_blank(), 
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.x=element_blank(),
         panel.border=element_blank(),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())



###### ANALYSIS OF ATG FEEDBACK SURVEY RESULTS ######

# read data from remote git repo
csv = getURL('https://raw.githubusercontent.com/winlingit/w241-project-csw/master/rcode/atg_results.csv')
dt.atg = data.table(read.csv(textConnection(csv)))
# dt.atg = fread('~/Documents/Berkeley/241/Final\ Project/w241-project-csw/rcode/atg_results.csv')

# estimate overall ATE
dt.atg[ , .(y = sum(Responses)/sum(N)), by = treat][ , y[1]-y[2]]

# calculate response rates in each block
dt.atg[, Rate := Responses / N]
dt.atg[ treat == 1, Treat := 'treatment']
dt.atg[ treat == 0, Treat := 'control']

# Plot the chart of winning
ggplot(dt.atg, aes(x=Block, y=Responses, fill=Block)) + 
  geom_bar(stat='identity') +
  geom_text(aes(x=Block, y=Responses, label=Responses), vjust=-.1) +
  facet_wrap( ~ Treat) +
  ggtitle("ATG Responses by Block") + 
  xlab("") + 
  ylab("Responses") + 
  theme( axis.line=element_blank(), 
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.x=element_blank(),
         panel.border=element_blank(),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())

# Plot the chart of proportional winning
ggplot(dt.atg, aes(x=Block, y=Rate, fill=Block)) + 
  geom_bar(stat='identity') +
  facet_wrap( ~ Treat) +
  ggtitle("ATG Response Rates by Block") + 
  xlab("") + 
  ylab("Response Rate") + 
  theme( axis.line=element_blank(), 
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.x=element_blank(),
         panel.border=element_blank(),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())



###### ANALYSIS USING RANDOMIZATION INFERENCE #######

# recover observations, from: http://stackoverflow.com/questions/2894775/replicate-each-row-of-data-frame-and-specify-the-number-of-replications-for-each
dtx = dt.atg[rep(seq(.N), N)]  # expand table to 352 rows
dtx[ , responded := c(rep(1, max(Responses)), rep(0, .N - max(Responses))), by = CollectorId]  # add var for responded
dt.atg$Responses == dtx[ , sum(responded), by = CollectorId]$V1  # checksums for total responses for each collector

### 1. Analysis with "ri" package

## NOTE: This code is largely copy/pasted and hacked from the example code for the dt package
y = dtx$responded
Z = dtx$treat
block = dtx$blocknum

# Estimate ATE without blocking
perms.noblock <- genperms(Z, maxiter=100000) # Generate 100k additional assignments 
probs.noblock <- genprobexact(Z) # probability of treatment
ate.noblock <- estate(y, Z, prob=probs.noblock) # estimate the ATE
ate.noblock

Ys <- genouts(y, Z ,ate=0) # generate potential outcomes under sharp null of no effect
distout.noblock <- gendist(Ys, perms.noblock, prob=probs.noblock) # generate sampling dist. under sharp null
dispdist(distout.noblock, ate.noblock)  # display characteristics of sampling dist. for inference

Ys <- genouts(y,Z,ate=ate.noblock) ## generate potential outcomes under tau = ATE
distout.noblock <- gendist(Ys,perms.noblock, prob=probs.noblock) # generate sampling dist. under tau = ATE
dispdist(distout.noblock, ate.noblock)  ## display characteristics of sampling dist. for inference


# Estimate ATE with blocking
perms <- genperms(Z, blockvar=block, maxiter=100000) # Generate 100k additional assignments
probs <- genprobexact(Z, blockvar=block) # probability of treatment
ate <- estate(y, Z, prob=probs) # estimate the ATE
ate

# Conduct Sharp Null Hypothesis Test of Zero Effect for Each Unit

Ys <- genouts(y, Z, ate=0) # generate potential outcomes under sharp null of no effect
distout <- gendist(Ys, perms, prob=probs) # generate sampling dist. under sharp null
dispdist(distout, ate)  # display characteristics of sampling dist. for inference

# Generate Sampling Distribution Around Estimated ATE

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
plot(density(ate.dist), main = "Distribution of ATE", col = "black", xlab = NA)
abline(v = ATE, col = 'red')

p.value = 2*mean(ate.dist >= ATE) # return 2-tailed p-value for estimated ATE
p.value # p-value < 0.01



###### ANALYSIS USING REGRESSION ######

# regression models
m1 = lm(responded ~ treat, data = dtx) # treatment only
m2 = lm(responded ~ treat + Female, data = dtx) # treatment + female
m3 = lm(responded ~ treat + Female + Region, data = dtx) treatment + female + region
# m4 = lm(responded ~ treat + Female*Region, data = dtx)

stargazer(m1, m2, m3, type = 'text', title = 'Regression Analysis for ATG Feedback Survey Experiment')
