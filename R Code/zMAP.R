# Functions used in chapter 4 for posterior predictive checking.

docs <- corpusFinal$documents
vocab <- corpusFinal$vocab


# Use model parameters to compute MAP for Z variable
zMAP <- function(model, document, docN) {
  # Build a matrix of topic probability for each word in a document
  # R automatically broadcasts log(theta) to correctly sum with log(Beta) matrix
  docProbs <- log(model$theta[docN,]) + model$beta$logbeta[[1]][,document[1,]]
  
  if (is.null(dim(docProbs))){
    which.max(docProbs)
  } else {
    apply(docProbs, 2, which.max)
  }
  
}

for (i in 1:length(docs)){
  docs[[i]] <- rbind(docs[[i]], zMAP(MFinal, docs[[i]], i))
}

buildZ <- function(doc){
  out <- NULL
  for (i in 1:dim(doc)[2]){
    out <- cbind(out, rbind(rep(doc[1, i], doc[2, i]), rep(doc[3, i], doc[2, i])))
  }
  return(out)
}

zDist <- lapply(docs, buildZ)

# Constrói as variáveis Z, W e D, necessária para o PPC.
lengths <- unlist(lapply(zDist, function(x) length(x[2,])))
Ds <- c()
for (i in 1:length(lengths)){
  Ds <- c(Ds, rep(names(lengths)[i], lengths[i]))
}
Ds <- as.numeric(Ds)
Zs <- as.numeric(unlist(lapply(zDist, function(x) x[2,])))
Ws <- as.numeric(unlist(lapply(zDist, function(x) x[1,])))

#Mutual Information entre W e D dado um tópico z
MI <- function(z, Ws){
  Nwdk <- table(Ws[Zs==z], Ds[Zs==z])
  Nwdk <- Nwdk/sum(Nwdk)
  #Nk <- sum(Zs==z)
  Nwk <- table(Ws[Zs==z])
  Nwk <- Nwk/sum(Nwk)
  Ndk <- table(Ds[Zs==z])
  Ndk <- Ndk/sum(Ndk)

  MI <- 0
  for (w in names(Nwk)){
    for (d in names(Ndk)){
      if (Nwdk[w, d]!=0){
        #MI <- MI + NwdkNk[w, d] * log((NwdkNk[w, d]*Nk)/(Ndk[d]*Nwk[w]))
        MI <- MI + Nwdk[w, d] * log((Nwdk[w, d])/(Ndk[d]*Nwk[w]))
      }
    }
  }
  MI
}


# Instantaneous Mutual Information entre w e D, dado z.
IMI <- function(z, w, Ws=Ws){
  HDk <- table(Ds[Zs==z])
  HDk <- HDk/sum(HDk)
  HDk <- -sum(HDk*log(HDk))
  HDwk <- table(Ds[Zs==z & Ws==w])
  HDwk <- HDwk/sum(HDwk)
  HDwk <- -sum(HDwk*log(HDwk))
  HDk - HDwk
}

# Simulate new data from the posterior distribution
newWs <- array(NA, c(100, length(Ws)))
for (i in 1:length(Ws)){
  newWs[, i] <- sample(1:3007, 100, replace=T, prob=beta[Zs[i], ])
}

# Compute discrepancy function for replicated data.

testStat <- array(NA, c(100, 50))
for (t in 1:50){
  for (r in 1:100){
    testStat[r, t] <- MI(t, newWs[r, ])
  }
}

realizedStat <- rep(NA, 50)
for (t in 1:50){
  realizedStat[t] <- MI(t, Ws)
}

deviances <- ((realizedStat - apply(testStat, 2, mean))/apply(testStat, 2, sd))

save(deviances, file='deviances.RData')


######################
# Get top topic words
getTopWords <- function(M, topic, n=10){
  beta <- exp(M$beta$logbeta[[1]])[topic, ]
  top <- (1:length(M$vocab))[order(beta, decreasing=T)][1:10]
  top
  
}

testWords <- array(NA, c(100, 10, 50))

for (t in 1:50){
  top10 <- getTopWords(MFinal, t)
  for (r in 1:100){
    for (w in 1:10){
      testWords[r, w, t] <- IMI(t, top10[w], newWs[r, ])
    }
  }
}

dataTestWords <- data.frame(IMI=as.vector(testWords[,,c(14, 29, 4)]),
                            words=c(MFinal$vocab[c(unlist(sapply(getTopWords(MFinal, 14), function(x) rep(x, 100))))],
                                    MFinal$vocab[c(unlist(sapply(getTopWords(MFinal, 29), function(x) rep(x, 100))))],
                                    MFinal$vocab[c(unlist(sapply(getTopWords(MFinal, 4), function(x) rep(x, 100))))]),
                            topic=factor(c(rep(14, 1000), rep(29, 1000), rep(4, 1000)), levels=c(14, 29, 4), ordered=F),
                            rank=c(rep(unlist(sapply(1:10, function(x) rep(x, 100))), 3)))
dataTestWords$words <- factor(dataTestWords$words, levels = unique(dataTestWords$words))
dataRealWord <- data.frame(IMI=c(unlist(sapply(getTopWords(MFinal, 14), function(x) IMI(14, x, Ws))),
                                 unlist(sapply(getTopWords(MFinal, 29), function(x) IMI(29, x, Ws))),
                                 unlist(sapply(getTopWords(MFinal, 4), function(x) IMI(4, x, Ws)))),
                           words=c(MFinal$vocab[getTopWords(MFinal, 14)], MFinal$vocab[getTopWords(MFinal, 29)],
                                   MFinal$vocab[getTopWords(MFinal, 4)]),
                           topic=factor(c(rep(14, 10), rep(29, 10), rep(4, 10)), levels=c(14, 29, 4), ordered=T),
                           rank=c(rep(1:10, 3)))
save(dataTestWords, dataRealWord, file='testWords.RData')
ggplot(dataTestWords, aes(x=IMI, y=rank, label=words)) + geom_point(alpha=0.2)  + 
  geom_point(data=dataRealWord, color='red')+ facet_wrap(~topic)  +
  scale_y_reverse(labels=1:10, breaks=1:10) + ylab('Ranque') +
  ggtitle('Checagem Posterior Preditiva')+
  geom_text(data=dataRealWord, aes(x=IMI, y=rank, label=words), nudge_x=2)
