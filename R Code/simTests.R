library(stm)

D <- length(corpusFinal$documents)
Nd <- unlist(lapply(corpusFinal$documents, function(x) sum(x[2,])))
theta <- MFinal$theta
beta <- exp(MFinal$beta$logbeta[[1]])
vocab <- MFinal$vocab

simCorpus <- list()

for (d in 1:D){
  zDoc <- sample(1:50, Nd[d], replace=T, prob=theta[d,])
  wDoc <- sapply(zDoc, function(x) sample(vocab, 1, prob=beta[x,]))
  simCorpus[[d]] <- wDoc
}

simCorpus <- lapply(simCorpus, function(x) paste(x, sep= ' ', collapse=' '))

corpusSim <- textProcessor(simCorpus, metadata=corpusFinal$meta,
                           lowercase=TRUE, removenumbers=TRUE, 
                           removestopwords=FALSE, removepunctuation=TRUE, stem=FALSE,
                           wordLength=c(1, Inf), sparselevel=1, language='pt')

corpusSim <- prepDocuments(corpusSim$documents,
                           corpusSim$vocab, corpusSim$meta, lower.thresh = 1)

McorpusSim <- stm(documents = corpusSim$documents, vocab = corpusSim$vocab, LDAbeta = TRUE, interactions = F,
              K = 50, data = corpusSim$meta, init.type = 'Spectral',
              prevalence = ~ s(session) + speaker)

klDiv <- function(p, q){
  out <- p*(log(p)-log(q))
  out <- ifelse(is.infinite(out) | is.nan(out), 0, out)
  mean(out)
}

hellDist <- function(p, q){
  out <- (sqrt(p) - sqrt(q))^2
  sqrt(sum(out))/sqrt(2)
}

newBeta <- exp(MFinal$beta$logbeta[[1]][,MFinal$vocab %in% McorpusSim$vocab])
newBeta <- t(apply(newBeta, 1, function(x) x/sum(x)))
newTheta <- theta[as.numeric(names(corpusSim$documents)),]
betaSim <- exp(McorpusSim$beta$logbeta[[1]])
thetaSim <- McorpusSim$theta

allign <- array(NA, c(50,50))
for (p in 1:50){
  divs <- rep(NA, 50)
  for (q in 1:50){
    divs[q] <- klDiv(newBeta[p, ], betaSim[q, ])
  }
  allign[p,] <- divs
}

finalAllign <- rep(NA, 50)
temp <- allign
for (i in 1:50){
  ind <- which(temp==min(temp), arr.ind = T)
  finalAllign[ind[1]] <- ind[2]
  temp[ind[1],] <- 1
  temp[,ind[2]] <- 1
}
par(mfrow=c(4, 5))
for (i in 1:50){
  plot(cumsum(newBeta[i,]), type='l')
  lines(cumsum(betaSim[finalAllign[i], ]), col='red')
}

for (i in sample(1:(dim(newTheta)[1]), 20)){
  plot(cumsum(newTheta[i,]), type='l')
  lines(cumsum(thetaSim[i, finalAllign]), col='red')
}

thetaDist <- rep(NA, dim(newTheta)[1])
for (i in 1:(dim(newTheta)[1])){
  thetaDist[i] <- klDiv(newTheta[i, ], thetaSim[i, finalAllign])
}

betaDist <- rep(NA, 50)
for (i in 1:50){
  betaDist[i] <- klDiv(newBeta[i, ], betaSim[finalAllign[i],])
}
