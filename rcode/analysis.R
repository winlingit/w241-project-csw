# setwd("~/Documents/Berkeley/241/Final\ Project/w241-project-csw/rcode/")

library(data.table)

## Randomization for real experiment

dt <- data.table(read.csv("atg_results.csv"))
summary(dt)

dt[, response_rate := Responses / N]

# Simplistic estimate of ATE, without taking blocking into account:
treat_rate = mean(dt[treat == 1]$response_rate); treat_rate
control_rate = mean(dt[treat==0]$response_rate); control_rate
