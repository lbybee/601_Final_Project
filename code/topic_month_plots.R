library(lubridate)
library(ggplot2)
library(tm)
library(extrafont)

K <- 30

load("../data/dates.rda")
load(paste("../data/model", K, ".rda", sep=""))

agg <- aggregate(model@gamma, list(dates), mean)

agg$Date <- floor_date(agg$Group.1, "month")
agg <- agg[agg$Date >= as.Date("2009-03-18"),]
agg <- agg[agg$Date < as.Date("2013-02-01"),]

temp <- agg[,2:(K+1)]
agg <- aggregate(temp, list(agg$Date), mean)

for(i in seq(2, K+1, 1)){
    gplot <- ggplot() + geom_line(aes(x=agg[,1], y=agg[,i])) + xlab("Date") + ylab("Topic Proportion")
    pdf(paste('../images/time_plot', i-1, '.pdf', sep=""), height=5.5, width=6.5, family='CM Roman')
    print(gplot)
    dev.off()
}

