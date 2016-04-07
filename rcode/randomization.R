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
  n.treat = round(n / 2)  # Uses banker's rounding
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

# Checking results
# Should have N be at least 2 for all blocks
# treat.pct should be as close to 50% as is possible given N.
# I think this is really all we need for checks.
stats <- unique(merged[, .(.N, Org, Region, Female, treat.pct=mean(treat) ), by=blocknum])
stats

write.csv(merged, file="Randomized-with-collectors.csv", quote=FALSE, row.names=FALSE)

# Anonymizing by blanking out name and email
merged[, ':=' (
  email = '',
  FirstName = ''
)]

write.csv(merged, file="De-identified-with-collectors.csv", quote=FALSE, row.names=FALSE)
