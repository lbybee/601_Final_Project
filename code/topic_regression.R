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

temp <- model@gamma
#temp <- sweep(model@gamma, 2, apply(model@gamma, 2, mean))
#temp <- scale(model@gamma)
data <- data.frame(temp)
data$Person <- senders$SenderPerson
data$Person <- as.factor(data$Person)
data <- data[data$Person %in% people,]
rows <- length(data[,1])

test_ind <- sample(1:rows, rows/10)
train_ind <- (1:rows)[!(1:rows %in% test_ind)]

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
coefficients <- scale(coefficients)
#for(i in 1:K){
#    coefficients[,i] <- (coefficients[,i] - min(coefficients[,i])) / (max(coefficients[,i]) - min(coefficients[,i]) + 0.0000001)
#}
#coefficients <- coefficients - mean(as.matrix(coefficients))
test <- c("Meetings", "Middle East", "Staff",   "Politics", "Terrorism", "Foreign Policy", "Press",   "Hillary", "Common")
col <-  c('#b2df8a',  '#fdbf6f',     '#a6cee3', '#33a02c',  '#e31a1c',   '#ff7f00',        '#fb9a99', '#6a3d9a', '#1f78b4')
sortorder <- c(5, 22, 6, 18, 19, 27, 29, 2, 7, 13, 21, 23, 26, 11, 17, 25, 16, 20, 4, 12, 1, 9, 10, 24, 28, 30, 3, 8, 14, 15)
colororder <- c(rep(col[5], 2), rep(col[2], 5), rep(col[6], 6), rep(col[4], 3), rep(col[3], 2), rep(col[7], 1), rep(col[8], 1), rep(col[1], 6), rep(col[9], 4))
coefficients <- coefficients[,sortorder]
coefficients[coefficients < -2] <- -2
coefficients[coefficients > 2] <- 2
mlt <- melt(t(t(coefficients)))
colnames(mlt) <- c("Sender", "Topic", "Coefficient")
cols <- rev(brewer.pal(11, 'PiYG'))

cats = c('Terrorism', 'Middle East', 'Foreign Policy', 'Politics', 'Staff',
         'Press', 'Hillary', 'Meetings', 'Common')
legend = c('Terrorism'='#e31a1c',
           'Middle East'='#fdbf6f',
           'Foreign Policy'='#ff7f00',
           'Politics'='#33a02c',
           'Staff'='#a6cee3',
           'Press'='#fb9a99',
           'Hillary'='#6a3d9a',
           'Meetings'='#b2df8a',
           'Common'='#1f78b4')
a = data.frame(x=rep(1, 9), y=rep(1, 9), z=cats)

gplot = ggplot(data=mlt, aes(x=Topic, y=Sender)) +
    geom_tile(aes(fill=Coefficient), color="white") +
    geom_line(data=a, aes(x=x, y=y, color=z), alpha=0.0, size=1.3) +
    scale_fill_gradient2(midpoint=0, low="#4d9221", high="#c51b7d") +
    scale_color_manual(name='Category', values=legend, breaks=cats) +
    guides(color=guide_legend(override.aes=list(alpha=1))) +
    theme(legend.key=element_blank(), panel.background=element_rect(fill="white", colour="white"),
          axis.text.x=element_text(color=colororder, angle=90, hjust=0, vjust=0.5))

pdf('../images/coefficients.pdf', height=5.5, width=6.5, family='CM Roman')
print(gplot)
dev.off()

