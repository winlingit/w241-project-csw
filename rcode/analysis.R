# setwd("~/Documents/Berkeley/241/Final\ Project/w241-project-csw/rcode/")

library(data.table)
library(RCurl)
library(stargazer)

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

coef.test(m3)

# stimated ATE = 0.073 (SE = 0.026) is significant (p < 0.01)
# Org and sex covariates were not predictive of response rate
# no significant interaction effects
