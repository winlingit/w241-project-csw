# setwd("~/Documents/Berkeley/241/Final\ Project/w241-project-csw/rcode/")

library(data.table)

## Randomization for real experiment

dt <- data.table(read.csv("participants.csv"))
collectors <- data.table(read.csv("collectors.csv"))

# Making sure the correct number of collectors is supplied
if(nrow(collectors) != 24) {
  print('*** ERROR: Incorrect number of collectors supplied! (Expected 24)')
}

assign.treatment <- function(n) {
  n.treat = floor(n / 2)
  n.control = n - n.treat
  data = sample(c(rep(1, n.treat), rep(0, n.control)))
  data
}

dt[, block := factor(paste(Female, Org, Region, sep='-'))]
dt[, blocknum := as.numeric(block)]
dt[, treat := assign.treatment(.N), by=block]

if(max(dt$blocknum) != 12) {
  print('*** ERROR: Incorrect number of blocks found! (Expected 12)')
}

# Assign collectors
collectors[, Id := .I]
dt[, CollectorId := blocknum * 2 - treat]
merged <- merge(dt, collectors, by.x='CollectorId', by.y='Id')

write.csv(merged, file="Randomized-with-collectors.csv", quote=FALSE, row.names=FALSE)

