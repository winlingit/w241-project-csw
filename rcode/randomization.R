# setwd("~/Documents/Berkeley/241/Final\ Project/w241-project-csw/rcode/")

library(data.table)

## Randomization for pilot we're doing with I School

dt <- data.table(read.csv("participants.csv"))

assign.treatment <- function(n) {
  n.treat = floor(n / 2)
  n.control = n - n.treat
  data = sample(c(rep(1, n.treat), rep(0, n.control)))
  data
}

dt[, block := factor(paste(Female, Org, Region, sep=','))]
dt[, blocknum := as.numeric(block)]
dt[, treat := assign.treatment(.N), by=block]

write.csv(dt, file="Randomized.csv", quote=FALSE, row.names=FALSE)

