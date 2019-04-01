library(text2vec)
library(data.table)
library(magrittr)
library(stopwords)
library(SnowballC)

sample       <- fread("sample_submission.csv")
test         <- fread("test.csv")
test_labels  <- fread("test_labels.csv")
train.data   <- fread("train.csv")

prep_fun = tolower
#stemming words
tok_fun = function(x) {
  word_tokenizer(x) %>% lapply( function(x) SnowballC::wordStem(x,language="en"))
}

it_train = itoken(train.data$comment_text,
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  ids = train.data$id,
                  progressbar = TRUE)


vocab = create_vocabulary(it_train, ngram=c(1L,3L), stopwords=stopwords("en",source="smart"))

prune.vocab <- prune_vocabulary(vocab, term_count_min = 10)

vectorizer = vocab_vectorizer(prune.vocab)
dtm_train = create_dtm(it_train, vectorizer)
dim(dtm_train)
colnames(dtm_train)[1:10]
dtm_train[1:10,1:10]
