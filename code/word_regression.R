library(tm)
library(glmnet)
library(nnet)


load("../data/dtm.rda")
load("../data/senders.rda")

people <- table(senders$SenderPerson)
people <- names(people[people > 100])
people <- people[people != ""]

dtm <- weightTfIdf(dtm)
data <- as.matrix(dtm)
data <- data.frame(data)
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
