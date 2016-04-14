library(tm)
library(glmnet)
library(nnet)
library(ggplot2)
library(reshape2)

K <- 30

load(paste("../data/model", K, ".rda", sep=""))
load("../data/senders.rda")

people <- table(senders$SenderPerson)
people <- names(people[people > 100])
people <- people[people != ""]

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
coefficients[coefficients < 0] <- 0
#coefficients <- scale(coefficients)
for(i in 1:K){
    coefficients[,i] <- (coefficients[,i] - min(coefficients[,i])) / (max(coefficients[,i]) - min(coefficients[,i]) + 0.0000001)
}
mlt <- melt(t(t(coefficients)))
colnames(mlt) <- c("Sender", "Topic", "value")
ggplot(data=mlt) + geom_tile(aes(x=Topic, y=Sender, fill=value)) + theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 0))
ggsave("coefficients.pdf")
