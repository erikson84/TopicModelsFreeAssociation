library(tm)
library(xtable)

#source('loadCorpus.R')

####################################
#
# Apresentação do corpus não tratado
#
####################################

tosPerSession <- unlist(lapply(raw, length))
expCorpus <- lapply(raw, function(x) gsub(pattern = '^(Alex|Ivan)\\:', replacement = '', x = x))
expCorpus <- lapply(expCorpus, function(x) gsub(pattern = '\\[?…\\]?', replacement=' Ret3.', x = x))
expCorpus <- lapply(expCorpus, tolower)
expCorpus1 <- Corpus(VectorSource(unlist(expCorpus)))
expCorpus1 <- tm_map(expCorpus1, removePunctuation)
dtmCorpus <- DocumentTermMatrix(expCorpus1, control=list(wordLengths=c(1, Inf)))
expWordCorpus <- textProcessor(unlist(expCorpus), lowercase=TRUE, removenumbers=FALSE, 
                               removestopwords=FALSE, removepunctuation=TRUE, stem=FALSE,
                               wordLength=c(1, Inf), sparselevel=1, language='pt')
expWordCorpus <- prepDocuments(expWordCorpus$documents,
                              expWordCorpus$vocab, expWordCorpus$meta, lower.thresh = 0)
wordPerTOS <- apply(dtmCorpus, 1, sum)

# Constrói tabela com informações sumárias do corpus

dadosCorpus <- data.frame(tos=c(as.numeric(table(speaker)), sum(as.numeric(table(speaker)))),
                          tosMu=c(round(as.numeric(apply(table(speaker, session), 1, mean))), sum(round(as.numeric(apply(table(speaker, session), 1, mean))))),
                          vocab=c(sum(apply(dtmCorpus[speaker=='Analisando',], 2, sum)>0), sum(apply(dtmCorpus[speaker=='Analista',], 2, sum)>0), sum(apply(dtmCorpus, 2, sum)>0)),
                          words=c(as.numeric(tapply(wordPerTOS, speaker, sum)), sum(wordPerTOS)),
                          wordsPerTOS=c(round(as.numeric(tapply(wordPerTOS, speaker, mean))), round(mean(wordPerTOS)))
)
rownames(dadosCorpus) <- c('Analisando', 'Analista', 'Total')
names(dadosCorpus) <- c('Turnos de Fala', 'Média de TdF por Sessão', 'Vocabulário', 'Palavras', 'Média de Palavras por TdF')
save(dadosCorpus, file = 'dadosCorpus.RData')

# Comandos para construção do gráfico Zipf

library(ggplot2)
dadosZipf <- data.frame(Frequência=sort(apply(dtmCorpus, 2, sum), decreasing=T), Ordem=1:(dim(dtmCorpus)[2]))
save(dadosZipf, file='dadosZipf.RData')


wordCount <- apply(dtmCorpus, 2, sum)

################################################
#
# Informações sobre as palavras mais frequentes
#
################################################

dados47 <- data.frame(Palavra=(names(sort(wordCount, decreasing = T))[1:47]),
                      Ocorrências=as.numeric(sort(wordCount, decreasing = T))[1:47],
                      Proporção=as.numeric(sort(wordCount, decreasing = T))[1:47]/sum(wordCount), stringsAsFactors = FALSE)
dados47 <- rbind(dados47, c('Total', sum(dados47$Ocorrências), sum(dados47$Proporção)))
dados47$Proporção <- round(as.numeric(dados47$Proporção), digits = 3)
dados47$Ocorrências <- as.numeric(dados47$Ocorrências)
save(dados47, file='dados47.RData')

Sessão=3*(as.numeric(tos==1)+as.numeric(tos==2))
Sessão=Sessão*as.numeric(session%%5==0)
Sessão=ifelse(Sessão>0, Sessão, NA)

dadosTOSWords <- data.frame(Palavras=wordPerTOS,
                            Interlocutor=speaker,
                            TOS=1:3219,
                            Sessão=Sessão,
                            sName=session)
save(dadosTOSWords, file='dadosTOSWords.RData')
ggplot(dadosTOSWords, aes(x=TOS, y=log10(Palavras))) +
  geom_line(aes(color=Interlocutor)) +
  geom_smooth(se=FALSE)+
  geom_text(aes(y=Sessão, x=TOS, label=sName)) + 
  facet_grid(.~Interlocutor)

########################################################
#
# Manipulação dos corpus com a demarcação de colocações
#
# 'corpus2' é construído em outro arquivo
#
########################################################

library(quanteda)

expCorpus2 <- Corpus(VectorSource(corpus2))
expCorpus2 <- tm_map(expCorpus2, removePunctuation)
dtmCorpus2 <- DocumentTermMatrix(expCorpus2, control=list(wordLengths=c(1, Inf)))
expWordCorpus2 <- textProcessor(corpus2, lowercase=TRUE, removenumbers=FALSE, 
                               removestopwords=FALSE, removepunctuation=TRUE, stem=FALSE,
                               wordLength=c(1, Inf), sparselevel=1, language='pt')
expWordCorpus2 <- prepDocuments(expWordCorpus2$documents,
                               expWordCorpus2$vocab, expWordCorpus2$meta, lower.thresh = 0)

sum(apply(dtmCorpus2, 2, sum)==1)

############################################
#
# Para identificação e remoção de stopwords
#
############################################

dtmCorpus2Norm <- t(apply(dtmCorpus2, 1, function(x) x/sum(x)))
cv <- sort(apply(dtmCorpus2Norm, 2, function(x) sd(x)/mean(x)))
idf <- log(length(expWordCorpus2$documents)/expWordCorpus2$wordcounts)
names(idf) <- expWordCorpus2$vocab
idf <- sort(idf)
stopwords <- data.frame(names(cv), names(idf))
selectWords <- as.character(unique(stopwords$names.cv.[1:300], stopwords$names.idf.[1:300]))
idxWords <- c(26, 29, 30, 35, 38, 40, 41, 42, 44, 51, 52, 54, 55, 56, 58, 61,
              62, 63, 64, 65, 66, 67, 68, 70, 71, 72, 74, 78, 79, 80, 81, 82,
              83, 85, 87, 88, 89, 90, 92, 98, 99, 100, 101, 103, 105)# Ret: 26, 29, 30
selectWords2 <- selectWords[-idxWords]
idxWords2 <- c(1:59, 68, 69, 86, 96, 98, 147, 150, 192, 200, 233, 234) # For original 50
#idxWords2 <- c(1:63, 71, 72, 89, 99, 101, 150, 153, 195, 203, 236, 237) # Em caso de remoção
swCorpus <- selectWords2[idxWords2]


#############################################
#
# Informações do corpus processado
#
#############################################

corpusFinalPre <- textProcessor(corpus2, metadata=meta,
                             lowercase=TRUE, removenumbers=TRUE, 
                             removestopwords=FALSE, removepunctuation=TRUE, stem=FALSE,
                             wordLength=c(1, Inf), sparselevel=1, language='pt',
                             customstopwords=swCorpus)
corpusFinal <- prepDocuments(corpusFinalPre$documents,
                             corpusFinalPre$vocab, corpusFinalPre$meta, lower.thresh = 1)

expCorpusFinal <- tm_map(expCorpus2, removeWords, swCorpus)
expCorpusFinal <- tm_map(expCorpusFinal, removeNumbers)

dtmCorpusFinal <- DocumentTermMatrix(expCorpusFinal,
                                     control=list(wordLengths=c(1, Inf), bounds=list(global=c(2, Inf))))
dtmCorpusFinal <- dtmCorpusFinal[(apply(dtmCorpusFinal, 1, sum)!=0),]

wordsPerTOSFinal <- apply(dtmCorpusFinal, 1, sum)
dadosCorpusFinal <- data.frame(tos=c(as.numeric(table(corpusFinal$meta$speaker)), sum(as.numeric(table(corpusFinal$meta$speaker)))),
                               tosMu=c(round(as.numeric(apply(table(corpusFinal$meta$speaker, corpusFinal$meta$session), 1, mean))), sum(round(as.numeric(apply(table(corpusFinal$meta$speaker, corpusFinal$meta$session), 1, mean))))),
                               vocab=c(sum(apply(dtmCorpusFinal[corpusFinal$meta$speaker=='Analisando',], 2, sum)>0), sum(apply(dtmCorpusFinal[corpusFinal$meta$speaker=='Analista',], 2, sum)>0), sum(apply(dtmCorpusFinal, 2, sum)>0)),
                               words=c(as.numeric(tapply(wordsPerTOSFinal, corpusFinal$meta$speaker, sum)), sum(wordsPerTOSFinal)),
                               wordsPerTOS=c(round(as.numeric(tapply(wordsPerTOSFinal, corpusFinal$meta$speaker, mean))), round(mean(wordsPerTOSFinal)))
)
rownames(dadosCorpusFinal) <- c('Analisando', 'Analista', 'Total')
names(dadosCorpusFinal) <- c('Turnos de Fala', 'Média de TdF por Sessão', 'Vocabulário', 'Palavras', 'Média de Palavras por TdF')
save(dadosCorpusFinal, file = 'dadosCorpusFinal.RData')


dadosZipfFinal <- data.frame(Frequência=sort(apply(dtmCorpusFinal, 2, sum), decreasing=T), Ordem=1:(dim(dtmCorpusFinal)[2]))
save(dadosZipfFinal, file='dadosZipfFinal.RData')

wordCountFinal <- apply(dtmCorpusFinal, 2, sum)
dados47Final <- data.frame(Palavra=(names(sort(wordCountFinal, decreasing = T))[1:47]),
                      Ocorrências=as.numeric(sort(wordCountFinal, decreasing = T))[1:47],
                      Proporção=as.numeric(sort(wordCountFinal, decreasing = T))[1:47]/sum(wordCountFinal), stringsAsFactors = FALSE)
dados47Final <- rbind(dados47Final, c('Total', sum(dados47Final$Ocorrências), sum(dados47Final$Proporção)))
dados47Final$Proporção <- round(as.numeric(dados47Final$Proporção), digits = 3)
dados47Final$Ocorrências <- as.numeric(dados47Final$Ocorrências)
save(dados47Final, file='dados47Final.RData')

SessãoFinal=2.6*(as.numeric(corpusFinal$meta$tos==1)+as.numeric(corpusFinal$meta$tos==2))
SessãoFinal=SessãoFinal*as.numeric(corpusFinal$meta$session%%5==0)
SessãoFinal=ifelse(SessãoFinal>0, SessãoFinal, NA)

dadosTOSWordsFinal <- data.frame(Palavras=wordsPerTOSFinal,
                                 Interlocutor=corpusFinal$meta$speaker,
                                 TOS=1:length(wordsPerTOSFinal),
                                 Sessão=SessãoFinal,
                                 sName=corpusFinal$meta$session)
save(dadosTOSWordsFinal, file='dadosTOSWordsFinal.RData')
ggplot(dadosTOSWordsFinal, aes(x=TOS, y=log10(Palavras))) +
  geom_line(aes(color=Interlocutor)) +
  geom_smooth(se=FALSE)+
  geom_text(aes(y=Sessão, x=TOS, label=sName)) + 
  facet_grid(.~Interlocutor)

#################################################
#
# Análise STM
#
#################################################

# Identificar o numero de tópicos, K

#bestK <- searchK(documents = corpusFinal$documents, vocab=corpusFinal$vocab, LDAbeta=TRUE,
#                 K = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), init.type = 'Spectral', data = corpusFinal$meta,
#                 prevalence = ~ s(session) + speaker, max.em.its=100, heldout.seed=1234)

#save(bestK, file='bestK.RData')

bK <- bestK$results
bK <- data.frame(Tópicos=rep(seq(10, 100, by = 10), 4),
                 Dados=c(bK$heldout, bK$residual, bK$exclus, bK$semcoh),
                 Critérios=c(rep('Ver. Dados Retidos', 10), rep('Resíduos', 10),
                             rep('Exclusividade', 10), rep('Coer. Semântica', 10)))
save(bK, file='bK.RData')
ggplot(bK, aes(x=Tópicos, y=Dados)) + geom_point() + geom_line() + facet_wrap(~Critérios, scales='free')

#mModels <- selectModel(documents = corpusFinal$documents, vocab = corpusFinal$vocab, LDAbeta = TRUE, interactions = F,
#                       K = 50, data = corpusFinal$meta, init.type = 'LDA',
#                       prevalence = ~ s(session) + speaker, emtol=1.2e-04, runs=25)

mBound <- sort(c(unlist(lapply(mModels$runout, function(x) tail(x$convergence$bound, 1))), tail(MFinal$convergence$bound, 1)))
boundDF <- data.frame(Modelo=1:6, Limite=mBound)
save(boundDF, file='mBound.RData')

MFinal <- stm(documents = corpusFinal$documents, vocab = corpusFinal$vocab, LDAbeta = TRUE, interactions = F,
         K = 50, data = corpusFinal$meta, init.type = 'Spectral', emtol = 1e-07,
         prevalence = ~ s(session) + speaker)#,
         #control=list(nits=300, burnin=100, alpha=6, eta=0.1))

toLDAvis(MFinal, corpusFinal$documents)
plot.STM(MFinal)