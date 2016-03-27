library(tm)
library(SnowballC)


tab <- read.csv("../data/emails.csv")

vect <- VectorSource(tab$AllText)
corpus <- Corpus(vect)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.999)

totals <- apply(dtm, 1, sum)
dtm <- dtm[totals > 0,]

save(dtm, file="../data/dtm.rda")
