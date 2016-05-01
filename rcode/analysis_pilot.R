
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

# Distribution under sharp null
pilot.Ys <- genouts(pilot.responded, pilot.treat,ate=0) # generate potential outcomes under sharp null of no effect
pilot.distout <- gendist(pilot.Ys,pilot.perms, prob=pilot.probs) # generate sampling dist. under sharp null
dispdist(pilot.distout, pilot.ate)  # display characteristics of sampling dist. for inference

# 95% CI
pilot.Ys <- genouts(pilot.responded, pilot.treat,ate=pilot.ate) ## generate potential outcomes under tau = ATE
pilot.distout <- gendist(pilot.Ys,pilot.perms, prob=pilot.probs) # generate sampling dist. under tau = ATE
dispdist(pilot.distout, pilot.ate)  ## display characteristics of sampling dist. for inference

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

