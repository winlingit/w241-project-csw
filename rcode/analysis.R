
library(data.table)
library(ggplot2)
library(RCurl)
library(stargazer)
library(ri)


###### ATG EXPERIMENT: EXPLORATORY ANALYSIS ######

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
  facet_wrap( ~ treat) +
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


###### ATG EXPERIMENT: RANDOMIZATION INFERENCE #######

# recover observations, from: http://stackoverflow.com/questions/2894775/replicate-each-row-of-data-frame-and-specify-the-number-of-replications-for-each
dt.atgx = dt.atg[rep(seq(.N), N)]  # expand table to 352 rows
dt.atgx[ , responded := c(rep(1, max(Responses)), rep(0, .N - max(Responses))), by = CollectorName]  # add var for responded
dt.atg$Responses == dt.atgx[ , sum(responded), by = CollectorName]$V1  # checksums for total responses for each collector

### 1. Analysis with "ri" package

## NOTE: This code is largely copy/pasted and hacked from the example code for the dt package
y = dt.atgx$responded
Z = dt.atgx$treat
block = dt.atgx$blocknum

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

ate.dist = replicate(10000, sim.ate(dt.atgx)) # generate sampling distribution for ATE
plot(density(ate.dist), main = "Distribution of ATE", col = "black", xlab = NA)
abline(v = ATE, col = 'red')

p.value = 2*mean(ate.dist >= ATE) # return 2-tailed p-value for estimated ATE
p.value # p-value < 0.01


###### ATG EXPERIMENT: REGRESSION ANALYSIS ######

# regression models
m1.atg = lm(responded ~ treat, data = dt.atgx) # treatment only
m2.atg = lm(responded ~ treat + Female, data = dt.atgx) # treatment + female
m3.atg = lm(responded ~ treat + Female + Org, data = dt.atgx) # treatment + female + org
m4.atg = lm(responded ~ treat + Female + Org + Region, data = dt.atgx) # treatment + female + org + region

stargazer(m1.atg, m2.atg, m3.atg, m4.atg, type = 'text', title = 'Regression Analysis for ATG Feedback Survey Experiment')




###### PTG EXPERIMENT: EXPLORATORY ANALYSIS ######

# read data from remote git repo
csv = getURL('https://raw.githubusercontent.com/winlingit/w241-project-csw/master/rcode/ptg_results.csv')
dt.ptg = data.table(read.csv(textConnection(csv)))

# recover observations
dt.ptgx = dt.ptg[rep(seq(.N), N)]  # expand table to 352 rows
dt.ptgx[ , responded := c(rep(1, max(Responses)), rep(0, .N - max(Responses))), by = CollectorName]  # add var for responded
dt.ptg$Responses == dt.ptgx[ , sum(responded), by = CollectorName]$V1  # checksums for total responses for each collector

# estimate overall ATE
dt.ptg[ , .(y = sum(Responses)/sum(N)), by = treat][ , y[1]-y[2]]

# calculate response rates in each block
dt.ptg[, Rate := Responses / N]
dt.ptg[ treat == 1, Treat := 'treatment']
dt.ptg[ treat == 0, Treat := 'control']

# Plot the chart of winning
ggplot(dt.ptg, aes(x=block, y=Responses, fill=block)) + 
        geom_bar(stat='identity') +
        geom_text(aes(x=block, y=Responses, label=Responses), vjust=-.1) +
        facet_wrap( ~ treat) +
        ggtitle("PTG Responses by Block") + 
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
ggplot(dt.ptg, aes(x=block, y=Rate, fill=block)) + 
        geom_bar(stat='identity') +
        facet_wrap( ~ Treat) +
        ggtitle("PTG Response Rates by Block") + 
        xlab("") + 
        ylab("Response Rate") + 
        theme( axis.line=element_blank(), 
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.x=element_blank(),
               panel.border=element_blank(),
               panel.grid.major.x=element_blank(),
               panel.grid.minor.x=element_blank())


###### PTG EXPERIMENT: REGRESSION ANALYSIS ######

# regression models
m1.ptg = lm(responded ~ treat, data = dt.ptgx) # treatment only
m2.ptg = lm(responded ~ treat + Female, data = dt.ptgx) # treatment + female
m3.ptg = lm(responded ~ treat + Female + Org, data = dt.ptgx) # treatment + female + org
m4.ptg = lm(responded ~ treat + Female + Org + Region, data = dt.ptgx) # treatment + female + org + region

stargazer(m1.ptg, m2.ptg, m3.ptg, m4.ptg, type = 'text', title = 'Regression Analysis for PTG Feedback Survey Experiment')
