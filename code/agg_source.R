library(tm)

K <- 30

load(paste("../data/model", K, ".rda", sep=""))
load("../data/senders.rda")

agg <- aggregate(model@gamma, list(senders$SenderPerson), mean)

people <- table(senders$SenderPerson)
people <- names(people[people > 100])
people <- people[people != ""]

agg <- agg[agg$Group.1 %in% people,]

nam <- c()
for(i in seq(2, K+1, 1)){
    temp <- order(agg[,i], decreasing=TRUE)[1:5]
    print(as.character(agg$Group.1[temp]))
    nam <- cbind(nam, as.character(agg$Group.1[temp]))
}

    
