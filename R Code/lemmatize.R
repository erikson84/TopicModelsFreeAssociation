# Function to apply the TreeTagger executable. Returns the lemmas of a character vector  
lemmatize <- function(charVec){
  treeTag <- system('~/Aplicativos/treeTagger/cmd/tree-tagger-portuguese', input=charVec,
                    intern = T, ignore.stderr = T)
  lemmas <- unlist(lapply(strsplit(treeTag, '\\t'),
                          function(x) ifelse(x[3] == '@card@' | 
                                               x[3] == '@ord@' |
                                               x[3] == '<unknown>' |
                                               x[3] == 'menino' |
                                               x[3] == 'irmão'|
                                               x[3] == 'ele'|
                                               x[3] == 'só',
                                             x[1], x[3])))
  paste(lemmas, collapse=' ')
}
