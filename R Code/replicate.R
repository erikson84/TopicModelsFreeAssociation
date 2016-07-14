###########################################
#
# Replication script
#
###########################################

library(stm) # STM package for fitting the topic model
library(ggplot2) # GGPlot for plotting functions
library(mvtnorm) # Multivariate normal RNG for numerical integration
library(tm) # General text processing functions

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

toLDAvis(model, corpus$documents)

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
# to finish running.

source('./zMAP.R')

###########################################
#
# (4) Hypotheses testing
#
###########################################

# The external validation hypotheses described in 
# chapter 4 are implemented in the 'testAssociations.R' file.
# The source file implements 

source('./testAssociations.R')
