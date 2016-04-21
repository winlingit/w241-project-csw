# setwd("~/Documents/Berkeley/241/Final\ Project/w241-project-csw/rcode/")

library(data.table)
library(RCurl)
library(stargazer)

# Analysis of ATG feedback survey results

# 1. read data
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

# 4. display results
stargazer(m1, m2, m3, type='text') 

# stimated ATE = 0.073 (SE = 0.026) is significant (p < 0.01)
# Org and sex covariates were not predictive of response rate
# no significant interaction effects



## Randomization for real experiment

dt <- data.table(read.csv("participants.csv"))
collectors <- data.table(read.csv("collectors.csv"))

assign.treatment <- function(n) {
  n.treat = round(n / 2)  # Uses banker's rounding
  n.control = n - n.treat
  data = sample(c(rep(1, n.treat), rep(0, n.control)))
  data
}

# NAs go into their own block
dt[, has.na := is.na(FirstName) | is.na(Female)]
dt[has.na == TRUE, block := 'HasNas']

# Assign rest of blocks
dt[has.na == FALSE, block := paste(Female, Org, Region, sep='-')]

# There's only one female engineer in EMEA, so we won't block by sex
# for EMEA engineers
dt[Org == 'Engineering' & Region == 'EMEA',
   block := paste('_', Org, Region, sep='1')]

dt[, block := factor(block)]
dt[, blocknum := as.numeric(block)]

dt[has.na==TRUE, treat := 0]
dt[has.na == FALSE, treat := assign.treatment(.N), by=block]

# Making sure control FirstName is 'Team'
dt[treat == 0, FirstName := 'team']

# Assign collectors
collectors[, Id := .I]
dt[, CollectorId := blocknum * 2 - treat]
merged <- merge(dt, collectors, by.x='CollectorId', by.y='Id')

# Checking results
# Should have N be at least 2 for all blocks
# treat.pct should be as close to 50% as is possible given N.
# I think this is really all we need for checks.
stats <- unique(merged[, .(.N, Org, Region, Female, treat.pct=mean(treat) ), by=blocknum])
stats

write.csv(merged[treat==1], file="Randomized-with-collectors_TREATMENT.csv", quote=FALSE, row.names=FALSE)
write.csv(merged[treat==0], file="Randomized-with-collectors_CONTROL.csv", quote=FALSE, row.names=FALSE)

# Anonymizing by blanking out name and email
merged[, ':=' (
  email = '',
  FirstName = ''
)]

write.csv(merged, file="De-identified-with-collectors.csv", quote=FALSE, row.names=FALSE)
