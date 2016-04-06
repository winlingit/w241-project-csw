# setwd("~/Documents/Berkeley/241/Final\ Project/w241-project-csw/rcode/")

library(data.table)
original <- data.table(read.csv('source.csv'))

# Assign IDs to people. Random ordering of IDs to help preserve anonymity.
# Save to a file so we can keep track of what they were
with_ids <- original[,,keyby=sample(.I)]
write.csv(with_ids[, list(id, name, sex, age, department)], 
          file="NOT_anonymized.csv",
          row.names = FALSE,
          quote = FALSE)

# Write out a file for participants without names. 
# Sort by ID so we won't know the original order
without_ids <- with_ids[, list(id, sex, age, department)]
write.csv(without_ids[, list(id, sex, age, department), ], 
          file="anonymized.csv",
          row.names = FALSE,
          quote = FALSE)


