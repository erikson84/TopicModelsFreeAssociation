# Exploratory function used in chapter 5

regData <- array(NA, c(3172, 3))
for (i in 3172:1){
  if (meta$speaker[i] == 'Analista' & meta$speaker[i-1]=='Analisando'){
    regData[i,] <- c(MFinal$theta[i, 21], MFinal$theta[i-1, 31], MFinal$theta[i-1, 35])
  }
}

regData <- na.exclude(data.frame(regData))
names(regData) <- c('t21', 't31', 't35')
summary(lm((t21)~I(t31*10)+I(t35*10), data=regData))


findThoughts(MFinal, unlist(raw)[as.numeric(names(corpusFinal$documents))], topics = 17, n=15)
corpusFinal$meta[order(decreasing = T, MFinal$theta[,17])[1:15],]
unlist(raw)[as.numeric(names(corpusFinal$documents))][1133]
plotTopic(topic=36, eff=eff, M=MFinal, frex=T)
order(beta[, which(MFinal$vocab=='terapeuta')], decreasing=T)
which(MFinal$vocab[order(beta[37, ], decreasing=T)]=='distração')
order(corrFinal$cor[17, ], decreasing=T)[1:7]
