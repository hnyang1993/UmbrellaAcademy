## Text Mining Packages
## require(text2vec)
## require(data.table)
## require(magrittr)
## require(stopwords)
## require(SnowballC)
##

## Input a comment data set with comment column named as comment_text
Marginal_Freq <- function(commentData, label, f = NULL, id = 'id', colname = 'comment_text', min_count = 10, threshold = 0.02){
  
  ## Change column name to default 'id'
  col.id <- which(names(commentData)== id)
  names(commentData)[col.id] <- 'id'
  
  ## Change column name to default 'coment_text'
  col.comment <- which(names(commentData)== colname)
  names(commentData)[col.comment] <- 'comment_text'
  
  data.toxic <- commentData[label==1, ]
  data.nontoxic <- commentData[label==0, ]
  
  N.toxic <- dim(data.toxic)[1]
  N.nontoxic <- dim(data.nontoxic)[1]
  
  ## Text Mining Packages
  require(text2vec)
  require(data.table)
  require(magrittr)
  require(stopwords)
  require(SnowballC)
  ##
  
  ## Translate character to lower case
  prep_fun <- tolower
  
  ## stemming words
  tok_fun <- function(x) {
    word_tokenizer(x) %>% lapply( function(x) SnowballC::wordStem(x,language="en"))
  }
  
  ## Making frequency table
  ## Toxic count
  it_toxic = itoken(data.toxic$comment_text,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    ids = data.toxic$id,
                    progressbar = TRUE)
  
  vocab.toxic <- create_vocabulary(it_toxic, ngram=c(1L,3L), stopwords=stopwords("en",source="smart"))
  prune.vocab.toxic <- prune_vocabulary(vocab.toxic, term_count_min = min_count)
  
  ## Nontoxic count
  it_nontoxic = itoken(data.nontoxic$comment_text,
                       preprocessor = prep_fun,
                       tokenizer = tok_fun,
                       ids = data.nontoxic$id,
                       progressbar = TRUE)
  
  
  vocab.nontoxic <- create_vocabulary(it_nontoxic, ngram=c(1L,3L), stopwords=stopwords("en",source="smart"))
  prune.vocab.nontoxic <- prune_vocabulary(vocab.nontoxic, term_count_min = min_count)
  
  ## Merge two dataframe
  D.toxic <- data.frame("term" = prune.vocab.toxic$term, "termCount" = prune.vocab.toxic$term_count, "docCount" = prune.vocab.toxic$doc_count)
  D.nontoxic <- data.frame("term" = prune.vocab.nontoxic$term, "termCount" = prune.vocab.nontoxic$term_count, "docCount" = prune.vocab.nontoxic$doc_count)
  
  term.count <- merge(D.toxic, D.nontoxic, by = "term", all = TRUE, suffixes = c(".toxic",".nontoxic"))
  term.count$`toxic.never` <- is.na(term.count$termCount.toxic)
  term.count$`nontoxic.never` <- is.na(term.count$termCount.nontoxic)
  
  term.count[is.na(term.count)] <- 0
  
  ## Calculate word frequency in toxic and non-toxic
  term.count$termPercent.toxic <- term.count$termCount.toxic/N.toxic
  term.count$docPercent.toxic <- term.count$docCount.toxic/N.toxic
  
  term.count$termPercent.nontoxic <- term.count$termCount.nontoxic/N.nontoxic
  term.count$docPercent.nontoxic <- term.count$docCount.nontoxic/N.nontoxic
  
  ## Define default null
  if(is.null(f)){
    f <- function(f1, f2){ abs(f1-f2)/max(f1,f2) }
  }
  
  ## r = abs(f_1-f_2)/max(|f_1|,|f_2|)
  term.count$termPerDiff <- f(term.count$termPercent.nontoxic, term.count$termPercent.toxic)
  
  ## r > 0.02
  raw.list <- term.count$term[pmax(term.count$termCount.nontoxic, term.count$termCount.toxic)>min_count&term.count$termPerDiff>threshold]
  
  ## Split itoken items into single words
  clean.list <- c()
  
  for(word in raw.list){
    word.split <- unlist(strsplit(word ,split='[[:punct:]]'))
    word.split <- unique(word.split)
    word.split <- word.split[nchar(word.split)>=2]
    clean.list <- c(clean.list, word.split)
  }
  
  ## Key word list (K = 277)
  clean.list <- unique(clean.list)
  
  ## Return keyword list and frequency table
  return(list("keyword" = clean.list, "freq.table" = term.count))
  
}




