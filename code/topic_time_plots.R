library(ggplot2)
library(tm)

K <- 25

load("../data/dates_hill.rda")
load(paste("../data/model_hill", K, ".rda", sep=""))

agg <- aggregate(model@gamma, list(dates), mean)

agg <- agg[agg$Group.1 >= as.Date("2009-03-18"),]
agg <- agg[agg$Group.1 < as.Date("2013-02-01"),]

for(i in seq(2, K+1, 1)){
    ggplot() + geom_line(aes(x=agg[,1], y=agg[,i])) + xlab("Date") + ylab("Topic Proportion")
    ggsave(paste("time_plot", i-1, ".pdf", sep=""))
}
