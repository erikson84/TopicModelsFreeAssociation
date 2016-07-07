# Function to plot all topics and their expected proportion on the corpus
plotAll <- function(M, n=5){
  thetaMean <- colMeans(M$theta)
  labels <- rep(NA, length(thetaMean))
  lab <- labelTopics(M, n=n)$frex
  for (i in 1:length(thetaMean)){
    labTemp <- lab[i, ]
    labTemp <- paste(labTemp, collapse=', ')
    labels[i] <- paste('Tópico ', i, ': ', labTemp, sep='')
  }
  ggplot(data.frame(Lab=labels, Mean=thetaMean, Num=factor(1:50, levels=order(thetaMean), ordered = T)), aes(y=Mean, x=Num)) + 
    geom_bar(stat = 'identity', fill='#00BFC4', size=0.2)+ 
    theme( panel.grid.minor=element_blank(),
           panel.grid.major.y=element_blank(),
           panel.background=element_rect(fill='grey95')) + 
    xlab('Número do Complexo') + ylab('Probabilidade')+
    geom_text(aes(x=Num, y=Mean, label=Lab), nudge_y=0.03, size=3)+
    ylim(c(0, 0.1))+
    coord_flip()+
    ggtitle('Probabilidade esperada dos complexos')
}

#plotAll(MFinal)
