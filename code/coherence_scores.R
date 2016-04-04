library(tm)
library(topicmodels)
library(ggplot2)


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


genUMassScores <- function(dtm, top_terms, K, term_count, t_ln){
    # generate the UMass scores so that they can be compared
    # to those we get from the null model

    topic_scores <- rep(NA, K)

    # make dtm truth mat
    dtm_mat <- as.matrix(dtm)
    dtm_t_mat <- dtm_mat > 0

    for(i in 1:K){
        t_terms <- top_terms[,i]
        t_comb <- combn(t_terms, 2)
        t_comb_ln <- dim(t_comb)[2]
        t_coh <- rep(NA, t_comb_ln)
        for(j in 1:t_comb_ln){
            s_comb <- t_comb[,j]
            i
            p_12 <- sum(dtm_t_mat[,s_comb[1]] & dtm_t_mat[,s_comb[2]])
            p_1 <- sum(dtm_t_mat[,s_comb[1]])
            p_2 <- sum(dtm_t_mat[,s_comb[2]])
            t_coh[j] <- log((p_12 + 1 / t_ln) * t_ln / (p_2 * p_1))
        }
        topic_scores[i] <- median(t_coh) 
    }

    return(topic_scores)
}


scores_mn <- c()
for(i in seq(5, 70, 5)){
    load(paste("../data/dtm.rda"))
    load(paste("../data/model", i, ".rda", sep=""))
    prob.mat <- exp(model@beta)
    post.mat <- genPost(prob.mat, dtm)
    top.terms <- genTopTerms(post.mat, i, 10, Terms(dtm))
    scores <- genUMassScores(dtm, top.terms, i, 10, dim(dtm)[1])
    scores_mn <- c(scores_mn, mean(scores))
    print(i)
}


ggplot() + geom_line(aes(x=seq(5, 70, 5), y=scores_mn)) + xlab("K") + ylab("Mean UMass Score")    
ggsave("umass_scores.pdf")
