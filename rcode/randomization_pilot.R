# setwd("~/Documents/Berkeley/241/Final\ Project/w241-project-csw/rcode/")
#setwd("~/Documents/Berkeley/241/Final\ Project/")

library(data.table)

## Randomization for pilot we're doing with I School

dt <- data.table(read.csv("student_report.csv"))

head(dt)

n.student <- nrow(dt); n.student
n.treat <- floor(n.student / 2); n.treat
n.control <- n.student - n.treat; n.control

dt[, treat := sample(c(rep(1, n.treat), rep(0, n.control)))]
summary(dt)

dt.treat <- dt[treat==1, .(Name, Email)]
dt.control <- dt[treat==0, .(Name, Email)]

write.csv(dt.treat, file="Treatment.csv", quote=FALSE, row.names=FALSE)
write.csv(dt.control, file="Control.csv", quote=FALSE, row.names=FALSE)