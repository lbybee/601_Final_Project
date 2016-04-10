library(tm)
library(SnowballC)

load("../data/dtm.rda")
datatf = weightTf(dtm)
mat = as.matrix(datatf)
write.table(mat, '../data/dtm_tf.csv', row.names=FALSE, col.names=FALSE)

datatfidf = weightTfIdf(dtm)
mat = as.matrix(datatfidf)
write.table(mat, '../data/dtm_tfidf.csv', row.names=FALSE, col.names=FALSE)


load("../data/senders.rda")
load("../data/ids_kept.rda")
write.csv(senders, '../data/senders.csv', row.names=FALSE)
write.csv(ids, '../data/ids_kept.csv', row.names=FALSE)
