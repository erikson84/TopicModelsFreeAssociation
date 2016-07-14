###########################################
#
# Replication script
#
###########################################

library(stm) # STM package for fitting the topic model
library(ggplot2) # GGPlot for plotting functions
library(mvtnorm) # Multivariate normal RNG for numerical integration
library(tm) # General text processing functions

setwd(getwd()) # Set the current dir as the working dir.

###########################################
#
# (1) Load the data
#
###########################################

# We start by loading the corpus DTM and metadata.
# On the original research, the 'loadCorpus.R' code was
# executed on the raw data (psychoanalytic session transcripts)
# to generate both the metadata and the corpus.
# The original code used the 'lemmatize' function,
# which used TreeTagger as a backend to generate lemmas 
# from the raw data. Some corrections are needed, and they are
# all registered in 'loadCorpus.R'.

meta <- read.csv('../Data/metaData.csv')
corpus <- read.csv('../Data/corpusData.csv')
# Convert large sparse matrix to DTM and STM format.
corpusDTM <- as.DocumentTermMatrix(corpus, weighting = weightTf)
corpus <- readCorpus(corpusDTM, type='slam')
corpus <- prepDocuments(corpus$documents, corpus$vocab, meta = meta)
names(corpus$documents) <- 1:length(corpus$documents)
rm(meta)

###########################################
#
# (2) Fit the model
#
###########################################

# In the original research, the final model was fit
# after processing the raw corpus, which is described
# in the 'descriptiveCorpus.R'. Since we won't process
# the raw corpus here for confidentiality reasons, we jump straight
# to fitting the final topic model.
#
# The 'descriptiveCorpus.R' file also describes the model selection
# procedures, which are described in chapter 4.

# 'Spectral' initialization gives deterministics results, so
# we don't need to 'set.seed' to obtain exactly the same fit.

model <- stm(documents = corpus$documents, vocab = corpus$vocab, LDAbeta = TRUE, interactions = F,
             K = 50, data = corpus$meta, init.type = 'Spectral', emtol = 1e-07,
             prevalence = ~ s(session) + speaker)

# Generate LDAvis interactive visualization, the same presented
# in the repository
# It's commented out so this file can be executed completely
# toLDAvis(model, corpus$documents)

###########################################
#
# (3) Posterior Predictive Checking
#
###########################################

# We implemented Mimno's and Blei's posterior predictive checks
# for topic models, based on mutual information (for each topic)
# and instantaneous mutual information (for each word type).

# All procedures are described in the 'zMAP.R' file. 
# We simply run the source code, but it takes a while
# to finish running. It should output two graphs:
# (1) Histogram for topic MI deviance;
# (2) PPC plot for three topics and 10 top words in each.

source('./zMAP.R')

###########################################
#
# (4) Hypotheses testing
#
###########################################

# The external validation hypotheses described in 
# chapter 4 are implemented in the 'testAssociations.R' file.
# The source file implements a function to compute p(w2|w1)
# the expected value for each topic, E[z] based on numeric
# integration.

# The source saves all the results in a data frame,
# 'testeFinalSTM'.

source('./testAssociations.R')

# Plot histograms of probability ratios.

ggplot(data.frame(`Razão de Probabilidade`=c(hypTest$ratio, log(hypTest$ratio)), tipo=factor(c(rep('Original', 100), rep('Logarítmica', 100)), ordered=T, levels=c('Original', 'Logarítmica'))), aes(x=Razão.de.Probabilidade)) + 
  geom_histogram(fill='#00BFC4', bins=20)+facet_grid(~tipo, scales = 'free') +
  xlab('Razão de Probabilidade') + ylab('Frequência') +ggtitle('Distribuição da razão de probabilidade')

# Data frame for predicted word rank

testOrd = hypTest[order(hypTest$mtRank, hypTest$mmRank), ]
testOrd = na.exclude(testOrd)
testOrd = testOrd[testOrd$mtRank<=10,]
testOrd = testOrd[, c(1, 2, 6, 7)]
names(testOrd) = c('Palavra-estímulo', 'Palavra-resposta', 'Ordem MT', 'Ordem MM')
rownames(testOrd) = NULL
testOrd

###########################################
#
# (5) Graphics for model inferences
#
###########################################

# Finally, we implement the plots to present model inferences.
# The functions are based on 'stm' core functions, but implemented
# with ggplot. Both functions are commented in their source files.

source('./plotTopic.R')
source('./plotAll.R')

# Use stm's 'estimateEffect' function to compute the posterior distribution
# for regression coefficients
eff <- estimateEffect(~ speaker + s(session), model, metadata = corpus$meta,
                      documents = corpus$documents, nsims = 100)
# Plot topic 21
plotTopic(topic=21, eff=eff, M=model, frex=FALSE)

# Plot topic 35
plotTopic(topic=35, eff=eff, M=model, frex=TRUE)

# Plot topic 31
plotTopic(topic=31, eff=eff, M=model, frex=FALSE)

# Plot topic 46
plotTopic(topic=46, eff=eff, M=model, frex=FALSE)

# Plot topic 11
plotTopic(topic=11, eff=eff, M=model, frex=FALSE)

# Plot topic 43
plotTopic(topic=43, eff=eff, M=model, frex=FALSE)

# Plot both topic 18 and 12
grid.arrange(plotTopic(topic=18, eff=eff, M=model, frex=TRUE, type='label'),
             plotTopic(topic=12, eff=eff, M=model, frex=FALSE, type='label'), ncol=2)

# Plot topic 37
plotTopic(topic=37, eff=eff, M=model, frex=FALSE)

# Plot topic 26
plotTopic(topic=26, eff=eff, M=model, frex=TRUE)

# Plot topic 3
plotTopic(topic=3, eff=eff, M=model, frex=FALSE)

# Plot topic 30
plotTopic(topic=30, eff=eff, M=model, frex=TRUE)

# Plot topic 17
plotTopic(topic=17, eff=eff, M=model, frex=TRUE)

# Plot marginal topic proportions
plotAll(model)

# Plot correlation matrix as graph
# First, build sparse correlation based on stm's 'topicCorr' function
library(igraph) # For graph plotting functions and community discovery
corrFinal <- topicCorr(model, method='huge')
tempGraph = graph.adjacency(corrFinal$posadj, mode = "undirected", weighted = TRUE, diag = FALSE)
fc = fastgreedy.community(tempGraph)
plot(tempGraph, vertex.color = membership(fc),  vertex.label.cex=1)
