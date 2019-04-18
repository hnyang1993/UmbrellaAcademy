#stemming words
tok_fun = function(x) {
  word_tokenizer(x) %>% lapply( function(x) SnowballC::wordStem(x,language="en"))
}

preprocess <- function(traindata, testdata, prep_fun = tolower, tok_fun = tok_fun){
  it_train = itoken(traindata$comment_text,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    ids = traindata$id,
                    progressbar = TRUE)
  
  
  vocab = create_vocabulary(it_train, ngram=c(1L,3L), stopwords=stopwords("en",source="smart"))
  
  prune.vocab <- prune_vocabulary(vocab, term_count_min = 10)
  
  vectorizer = vocab_vectorizer(prune.vocab)
  raw.dtm = create_dtm(it_train, vectorizer)
  
  it_test = itoken(testdata$comment_text,
                   preprocessor = prep_fun,
                   tokenizer = tok_fun,
                   ids = testdata$id,
                   progressbar = TRUE)
  
  raw.test.dtm = create_dtm(it_test, vectorizer)

  return(list("train.dtm" = "raw.dtm",
              "test.dtm"  = "raw.test.dtm")) 
}