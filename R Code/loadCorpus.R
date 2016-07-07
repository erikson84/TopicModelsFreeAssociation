library(stm)
library(tm)

# Load text files from 'Corpus' directory.
# Speakers are preceded by their names and each
# turn of speech is a single line.

path <- 'Corpus/'
files <- list.files(path)

raw <- list()

for (i in 1:length(files)){
  doc <- readLines(paste(path, files[i], sep=''))
  doc <- doc[doc != ''] # Remove blank lines.
  raw[[paste('Ent', i, sep='')]] <- doc
}

# Total number of turns of speech

N <- sum(unlist(lapply(raw, length)))

# Session order indicator

session <- NULL

for (i in 1:length(files)){
  session <- c(session, rep(i, length(raw[[i]])))
}

# Turn of speech order indicator (in each session)

tos <- NULL

for (i in 1:length(files)){
  tos <- c(tos, 1:length(raw[[i]]))
}

# Speaker indicator

speaker <- rep(0, N)
speaker[unlist(lapply(raw, function(x) grepl('^Alex:', x)))] <- 1
speaker <- factor(speaker, levels = c(0, 1), labels = c('Analisando', 'Analista'))

# Join everything in a single data frame

meta <- data.frame(session=session, tos=tos, speaker=speaker)

# Exclude speaker indicator and process text to analysis

corpus <- lapply(raw, function(x) gsub(pattern = '\\[?…\\]?', replacement=' Ret3.', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '(\\[|\\])', replacement='', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[\\.\\?,\\!]+', replacement=' .', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '-', replacement=' ', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '^(Alex|Ivan)\\:', replacement = '', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Ee]u não sei', replacement = 'eu_não_sei', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Ee]u acho que', replacement = 'eu_acho_que', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Dd]e alguma forma', replacement = 'de_alguma_forma', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Uu]m monte de', replacement = 'um_monte_de', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Ee]u não lembro', replacement = 'eu_não_lembro', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Mm]ais ou menos', replacement = 'mais_ou_menos', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Nn]ão me lembro', replacement = 'não_me_lembro', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Pp]or um lado', replacement = 'por_um_lado', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Cc]omo se fosse', replacement = 'como_se_fosse', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Qq]uando eu era', replacement = 'quando_eu_era', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Pp]or outro lado', replacement = 'por_outro_lado', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Qq]uando eu era', replacement = 'quando_eu_era', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Ff]im de semana', replacement = 'fim_de_semana', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Ee]u não consigo', replacement = 'eu_não_consigo', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Ee]u não t[ie]nh[ao]', replacement = 'eu_não_ter', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Ee]u estava lá', replacement = 'eu_estava_lá', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Nn]ão sei porque', replacement = 'não_sei_porque', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Nn]ão sei', replacement = 'não_sei', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Ee]u não', replacement = 'eu_não', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Àà]s vezes', replacement = 'as_vezes', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Ee]u acho', replacement = 'eu_acho', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Cc]omo se', replacement = 'como_se', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Ss]ei lá', replacement = 'sei_lá', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Pp]ode ser', replacement = 'pode_ser', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Dd]e repente', replacement = 'de_repente', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Pp]elo menos', replacement = 'pelo_menos', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Pp]ara que\\?', replacement = 'para_que?', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Aa]no passado', replacement = 'ano_passado', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Aa]lguma coisa', replacement = 'alguma_coisa', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Dd]e novo', replacement = 'de_novo', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Tt]empo todo', replacement = 'tempo_todo', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Pp]or exemplo', replacement = 'por_exemplo', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Oo]utras pessoas', replacement = 'outras_pessoas', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Nn]esse sentido', replacement = 'nesse_sentido', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Ee]stá bem', replacement = 'está_bem', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Nn]ão lembro', replacement = 'não_lembro', x = x))
corpus <- lapply(corpus, function(x) gsub(pattern = '[Oo]utras coisas', replacement = 'outras_coisas', x = x))

corpus <- lapply(corpus, tolower)

# Apply the 'lemmatize' function to transform each token into its lemma
corpus1 <- lapply(corpus, function(x) lemmatize(paste(x, collapse=' Nuline . ')))
corpus1 <- lapply(corpus1, function(x) unlist(strsplit(x, 'nuline')))

# Apply some corrections to TreeTagger results
corpus1 <- lapply(corpus1, function(x) gsub(pattern = 'moço', replacement = 'moça', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = 'fossar', replacement = 'ser', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = 'rodrigar', replacement = 'rodrigo', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = 'heleno', replacement = 'helena', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = 'catarino', replacement = 'catarina', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = 'nikolaia', replacement = 'nikolai', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = 'meninos', replacement = 'menino', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<vero\\>', replacement = 'vera', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = 'meninas', replacement = 'menina', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = 'irmãos', replacement = 'irmão', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = 'irmãs', replacement = 'irmã', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\+', replacement = ' ', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<os\\>', replacement = 'o', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<as\\>', replacement = 'o', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<a\\>', replacement = 'o', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<eles\\>', replacement = 'ele', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<elas\\>', replacement = 'ela', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<1\\>', replacement = 'um', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<10\\>', replacement = 'dez', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<11\\>', replacement = 'onze', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<12\\>', replacement = 'doze', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<13\\>', replacement = 'treze', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<14\\>', replacement = 'quatorze', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<15\\>', replacement = 'quinze', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<16\\>', replacement = 'dezessei', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<17\\>', replacement = 'dezessete', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<18\\>', replacement = 'dezoito', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<19\\>', replacement = 'dezenove', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2\\>', replacement = 'dois', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2000\\>', replacement = 'dois_mil', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2003\\>', replacement = 'doi_mil_tres', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2004\\>', replacement = 'dois_mil_quatro', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2005\\>', replacement = 'doi_mil_cinco', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2006\\>', replacement = 'doi_mil_seis', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2007\\>', replacement = 'doi_mil_sete', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2008\\>', replacement = 'doi_mil_oito', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2009\\>', replacement = 'doi_mil_nove', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2010\\>', replacement = 'doi_mil_dez', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2011\\>', replacement = 'doi_mil_onze', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2012\\>', replacement = 'doi_mil_doze', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2013\\>', replacement = 'doi_mil_treze', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2014\\>', replacement = 'doi_mil_quatorze', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2015\\>', replacement = 'doi_mil_quinze', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2016\\>', replacement = 'doi_mil_dezesseis', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<2017\\>', replacement = 'doi_mil_dezessete', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<21\\>', replacement = 'vinte_um', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<26\\>', replacement = 'vinte_seis', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<27\\>', replacement = 'vinte_sete', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<3\\>', replacement = 'três', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<4\\>', replacement = 'quatro', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<40\\>', replacement = 'quarenta', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<5\\>', replacement = 'cinco', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<50\\>', replacement = 'cinquenta', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<500\\>', replacement = 'quinhentos', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<6\\>', replacement = 'seis', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<7\\>', replacement = 'sete', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<8\\>', replacement = 'oito', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<9\\>', replacement = 'nove', x = x))
corpus1 <- lapply(corpus1, function(x) gsub(pattern = '\\<á\\>', replacement = 'o', x = x))

# Final output
corpus2 <- unlist(corpus1)

