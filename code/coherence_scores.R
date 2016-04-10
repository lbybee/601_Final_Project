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


genNullUMass <- function(dtm, samp_n, v_ln, t_ln){
    # what this does is randomly sample from pairs of
    # words in the dtm, it then finds the UMass score for
    # this random pair and returns a vector containing
    # all these UMass.  This gives a distribution of the
    # PMIs for the corpus and give us something to compare
    # the PMI score from UMass with.

    # make dtm truth mat
    dtm_mat <- as.matrix(dtm)
    dtm_t <- dtm_mat > 0

    scores <- rep(NA, samp_n)
    for(i in 1:samp_n){
        ind <- sample(1:v_ln, 2, replace=TRUE)
        p_12 <- sum(dtm_t[,ind[1]] & dtm_t[,ind[2]])
        p_1 <- sum(dtm_t[,ind[1]])
        p_2 <- sum(dtm_t[,ind[2]])
        scores[i] <- log((p_12 + 1 / t_ln) * t_ln / (p_2 * p_1))
    }
    scores[is.na(scores)] = 0
    return(scores)
}


genUMassScoreTest <- function(umass_scores, K, null_umass_dist){
    # gets the probability of getting a given score given the UMass score
    # distribution

    score_prob <- rep(NA, K)
    for(i in 1:K){
        score_prob[i] <- sum(umass_scores[i] <= null_umass_dist) / length(null_umass_dist)
    }
    return(score_prob)
}


K <- 30
samp <- 1000
t_count <- 10

load("../data/dtm.rda")
load(paste("../data/model", K, ".rda", sep=""))

null_scores <- genNullUMass(dtm, samp, dim(dtm)[2], dim(dtm)[1])
post_mat <- genPost(model@beta, dtm)
post_terms <- genTopTerms(post_mat, K, t_count, Terms(dtm))
umass_scores <- genUMassScores(dtm, post_terms, K, t_count, dim(dtm)[1]) 
umass_prob <- genUMassScoreTest(umass_scores, K, null_scores)

ggplot() + geom_density(aes(x=null_scores)) + geom_point(aes(x=umass_scores, y=rep(0, length(umass_scores))), color="blue") + xlab("UMass Score") + ylab("Density")
ggsave("null.density.jpg")
