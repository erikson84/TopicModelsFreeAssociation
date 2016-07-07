library(ggplot2)
library(stm)
library(mvtnorm)
library(gridExtra)

# Plot function used in chapter 5 to show topic content and covariate effect

plotTopic <- function(M, eff, topic, frex=FALSE, n=30, type='all'){
  
  # Plotting terms probs in topic
  if (frex) {
    lab <- labelTopics(M, topics=topic, n=n)$frex[topic, ]
  } else {
    lab <- labelTopics(M, topics=topic, n=n)$prob[topic, ]
  }
  
  lab <- factor(lab, levels=rev(lab), labels=rev(lab), ordered=T)
  probs <- unlist(sapply(lab, function(x) exp(M$beta$logbeta[[1]][topic, which(M$vocab==x)])))
  labels <- ggplot(data.frame(Termos=lab, Probabilidade=probs), aes(x=Termos, y=Probabilidade)) + 
    geom_bar(stat = 'identity', fill='#00BFC4')+
    coord_flip() + 
    theme( panel.grid.minor=element_blank(),
           panel.grid.major.y=element_blank(),
           panel.background=element_rect(fill='grey95')) + 
    ggtitle(paste('Principais Termos do Complexo', topic, ifelse(frex, '(FREX)', '')))
  
  # Plotting cov
  estimate <- eff$parameters[[topic]]
  estimate <- do.call(rbind, lapply(estimate,
                                    function(x) rmvnorm(10, mean=x$est, sigma=x$vcov)))
  cmat <- stm:::produce_cmatrix(prep = eff, covariate = 'speaker', method = 'pointestimate')$cmatrix
  # 1=Analista, 2=Analisando
  sims <- data.frame(cmat %*% t(estimate))
  speakEff <- ggplot(data.frame(mu=apply(sims, 1, mean), se=apply(sims, 1, sd),
                                lower=apply(sims, 1, quantile, 0.025), upper=apply(sims, 1, quantile, 0.975),
                                vars=c('Analista', 'Analisando')),
                     aes(y=mu, x=vars, color=vars, fill=vars)) + geom_segment(aes(y=mu-se, yend=mu+se, x=vars, xend=vars), size=3, lineend='round') +
    geom_segment(aes(y=lower, yend=upper, x=vars, xend=vars), size=1, lineend='round')+
    geom_point(size=6, shape=21, stroke=2, color='black') + 
    ylab('Probabilidade') + xlab('Interlocutor') + theme(legend.position="none")+ ggtitle(paste('Expectância por Inter.'))
  
  # Plot temporal effects
  
  cAll <- stm:::produce_cmatrix(prep = eff, covariate = 'session', method = 'continuous')
  cmat <- cAll$cmatrix
  cdata <- cAll$cdata
  sims <- data.frame(cmat %*% t(estimate))
  sessionEff <- ggplot(data.frame(mu=apply(sims, 1, mean), se=apply(sims, 1, sd), vars=cdata$session),
                     aes(y=mu, x=vars)) + geom_ribbon(aes(ymin=mu-se, ymax=mu+se, x=vars), fill='#00BFC4', alpha=0.3) + geom_line(color='#00BFC4', size=1) +
    ylab('Probabilidade') + xlab('Sessão') + ggtitle(paste('Expectância do Complexo', topic,'em Função da Sessão'))
  whichPlot <- match.arg(type, c('label', 'speak', 'session', 'all'))
  switch(whichPlot,
         label=labels,
         speak=speakEff,
         session=sessionEff,
         all=grid.arrange(labels, speakEff, sessionEff, layout_matrix=matrix(c(1, 1, 2, 1, 1, 2, 3, 3, 3), nrow=3, ncol=3, byrow=T)))
}


#plotTopic(topic=11, eff=eff, M=MFinal, frex=F, type='speak')
