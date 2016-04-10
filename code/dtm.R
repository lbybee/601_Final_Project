library(tm)
library(SnowballC)


tab = read.csv("../data/emails.csv")
ids = data.frame(Id=1:nrow(tab))
ids_kept = 1:nrow(tab)
date = as.Date(tab$DateSent)

tab = tab[!is.na(date),]
ids_kept = ids_kept[!is.na(date)]
date = date[!is.na(date)]

vect = VectorSource(tab$AllText)
corpus = Corpus(vect)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
# corpus = tm_map(corpus, removeWords, c(stopwords("english"), 'fyi'))
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)
meta(corpus, tag="date") = date
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)

totals = apply(dtm, 1, sum)
dtm = dtm[totals > 0,]
dates = date[totals > 0]
ids_kept = ids_kept[totals>0]

ids_kept = data.frame(Id=ids_kept, kept=1)
ids = merge(ids, ids_kept, by='Id', all.x=TRUE)
ids$kept[is.na(ids$kept)] = 0

save(ids, file="../data/ids_kept.rda")
save(dates, file="../data/dates.rda")
save(dtm, file="../data/dtm.rda")
