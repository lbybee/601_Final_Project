library(tm)
library(glmnet)
library(nnet)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
require(extrafont)

K <- 30

load(paste("../data/model", K, ".rda", sep=""))
load("../data/senders.rda")

people <- table(senders$SenderPerson)
people <- names(people[people > 100])
people <- people[people != ""]

#temp <- model@gamma
#temp <- sweep(model@gamma, 2, apply(model@gamma, 2, mean))
temp <- scale(model@gamma)
data <- data.frame(temp)
data$Person <- senders$SenderPerson
data$Person <- as.factor(data$Person)
data <- data[data$Person %in% people,]
rows <- length(data[,1])

test_ind <- sample(1:rows, rows/10)
train_ind <- 1:rows[!(1:rows %in% test_ind)]

training <- data[train_ind,]
testing <- data[test_ind,]

fit <- multinom("Person ~ .", data=training)

test_pred <- predict(fit, testing)

test_names <- as.character(testing$Person)
test_pred_names <- as.character(test_pred)

u_names <- unique(test_names)
success <- c()
failure <- c()
count <- c()
for(i in u_names){
    sub_ind <- test_names %in% c(i)
    success <- c(success, sum(test_pred_names[sub_ind] == test_names[sub_ind]))
    count <- c(count, sum(sub_ind))
    failure <- c(failure, sum(test_pred_names[sub_ind] != test_names[sub_ind]))
}

coefficients <- data.frame(coef(fit))
coefficients <- coefficients[,2:(K+1)]
#coefficients[coefficients < -1] <- -1
#coefficients[coefficients < 0] <- 0
#coefficients <- scale(coefficients)
#for(i in 1:K){
#    coefficients[,i] <- (coefficients[,i] - min(coefficients[,i])) / (max(coefficients[,i]) - min(coefficients[,i]) + 0.0000001) 
#}
test <- c("Meetings", "Middle East", "Staff", "Politics", "Terrorism", "Foreign Policy", "Press", "Hillary", "Communication", "Common")
col <- c('#e31a1c','#1f78b4','#a6cee3','#33a02c','#b2df8a','#fb9a99','#ff7f00','#6a3d9a','#cab2d6','#fdbf6f')
sortorder <- c(5, 22, 6, 18, 19, 27, 29, 2, 7, 13, 21, 23, 26, 11, 17, 25, 16, 20, 4, 12, 9, 10, 24, 28, 30, 1, 3, 8, 14, 15)
colororder <- c(rep(col[5], 2), rep(col[2], 5), rep(col[6], 6), rep(col[4], 3), rep(col[3], 2), rep(col[7], 1), rep(col[8], 1), rep(col[1], 5), rep(col[9], 1), rep(col[10], 4))
coefficients <- coefficients[,sortorder]
coefficients[coefficients < -0.9] <- -0.9
coefficients[coefficients > 0.9] <- 0.9 
mlt <- melt(t(t(coefficients)))
colnames(mlt) <- c("Sender", "Topic", "Coefficient")
cols <- rev(brewer.pal(11, 'PiYG'))
#4D9221
#C51B7D
ggplot(data=mlt, aes(x=Topic, y=Sender)) + geom_tile(aes(fill=Coefficient), colour="white") + scale_fill_gradient2(midpoint=median(as.matrix(coefficients)), low="#00CDCD", high="#CC00CC") + theme(text=element_text(family="CM Roman"), axis.text.x=element_text(colour=colororder, angle = 90, hjust = 0))
ggsave("coefficients.pdf")
