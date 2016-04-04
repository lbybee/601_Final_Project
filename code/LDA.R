library(tm)
library(topicmodels)


genPost <- function(prob.mat, dtm){
    # generates the posterior distribution for some terms
    # by dividing them by the unconditional probability

    term.sums <- apply(dtm, 2, sum)
    term.sums <- term.sums / sum(term.sums)

    prob.res <- t(apply(prob.mat, 1, function(x) x / term.sums))
    prob.res[prob.res == Inf] = 0
    return(prob.res)
}


genTopTerms <- function(prob.mat, K, t.count, v){
    # generates the top terms from a probability matrix (beta)
    # this can take in a modified prob mat from genPost or genTfIdfPost
    # as well

    topic.terms <- matrix(NA, t.count, K)
    for(i in 1:K){
        topic.terms[,i] <- v[order(prob.mat[i,], decreasing=TRUE)[1:t.count]]
    }
    return(topic.terms)
}


load("../data/dtm.rda")

for(i in seq(5, 100, 5)){
    model <- LDA(dtm, i)
    print(i)
    save(model, file=paste("../data/model", i, ".rda", sep=""))
    prob.mat <- exp(model@beta)
    post.mat <- genPost(prob.mat, dtm)
    top.terms <- genTopTerms(post.mat, i, 10, Terms(dtm))
    write.csv(top.terms, paste("../data/top.terms.", i, ".csv", sep=""))
}
