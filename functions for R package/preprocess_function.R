prep_fun = tolower
#stemming words
tok_fun = function(x) {
  word_tokenizer(x) %>% lapply( function(x) SnowballC::wordStem(x,language="en"))
}

preprocess <- function(data = train.sample, prep_fun = tolower, tok_fun = tok_fun){
  it_train = itoken(data$comment_text,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    ids = data$id,
                    progressbar = TRUE)
  
  
  vocab = create_vocabulary(it_train, ngram=c(1L,3L), stopwords=stopwords("en",source="smart"))
  
  prune.vocab <- prune_vocabulary(vocab, term_count_min = 10)
  
  vectorizer = vocab_vectorizer(prune.vocab)
  raw.dtm = create_dtm(it_train, vectorizer)
  
  return(raw.dtm)
}