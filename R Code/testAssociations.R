library(stm)
library(mvtnorm)
# Function used in hypothesis testing

# Main function: evaluates p(w2|w1) and word order, compared to baseLine
testAssociation <- function(w1, w2, beta, muTheta, baseLine){
  w1 <- tolower(w1)
  w2 <- tolower(w2)
  
  id1 <- which(names(baseLine)==w1)
  id2 <- which(names(baseLine)==w2)
  if (length(id1) == 0 | length(id2)==0){
    out <- data.frame(est=w1, res=w2, pW2W1=NA,
             pW2=NA, ratio=NA, mtRank=NA, mmRank=NA)
    return(out)
  }
  pZw1 <- (muTheta * beta[,id1]) / sum(muTheta * beta[,id1])
  #pZw1 <- beta[,id1]/sum(beta[,id1]) # Opção força bruta! Mas dá bons resultados.
  pw2w1 <- sum(beta[,id2] * pZw1)
  ord <- sum(apply(beta*pZw1, 2, sum) > pw2w1)
  out <- as.numeric(pw2w1/baseLine[id2])
  full <- data.frame(est=w1, res=w2, 
                     pW2W1=pw2w1, pW2=baseLine[id2],
                     ratio=out, mtRank=ord+1,
                     mmRank=which(names(sort(baseLine, decreasing = T))==w2))
  full
}

# Function to numerically integrate the LogitNormal distribution
muTheta <- function(session, sesIdx, M, n.sims=5000){
  mu <- apply(M$mu$mu[,sesIdx==session], 1, mean)
  sigma <- M$sigma
  sims <- array(NA, c(length(mu)+1, n.sims))
  for (i in 1:n.sims){
    temp <- c(rmvnorm(1, mu, sigma), 0)
    sims[, i] <- exp(temp)/sum(exp(temp))
  }
  apply(sims, 1, mean)
}

# Run the main function over all word pairs.
testAll <- function(df, baseLine, M, corpus){
  sessions <- unique(df$sessão)
  out <- data.frame()
  beta <- exp(M$beta$logbeta[[1]])
  
  for (s in 1:length(sessions)){
    mu <- muTheta(sessions[s], corpus$meta$session, M)
    #mu <- apply(M$theta, 2, mean) # If we don't want to take session number into account.
    tempData <- subset(df, df$sessão==sessions[s])
    outS <- data.frame()
    for (i in 1:dim(tempData)[1]){
      outS <- rbind(outS, testAssociation(tempData[i, 4], tempData[i, 5], beta, mu, baseLine))
    }
    out <- rbind(out, outS)
  }
  rownames(out) <- 1:dim(out)[1]
  out
}
# Build the baseline model based on marginal word distribution
baseLine <- apply(corpusDTM, 2, sum)/sum(corpusDTM)

# Load the associations
associations <- read.csv('../Data/associations.csv')

# Run all the tests
testeFinalSTM <- na.exclude(testAll(associations, baseLine, model, corpus))
