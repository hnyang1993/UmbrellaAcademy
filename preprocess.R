library(text2vec)
library(data.table)
library(magrittr)
library(stopwords)
library(SnowballC)
library(caret)
library(glmnet)
setwd("~/Desktop/BIOS 735/final_project/jigsaw-toxic-comment-classification-challenge")
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
raw.dtm = create_dtm(it_train, vectorizer)
dim(raw.dtm)

bns = BNS$new()
train.dtm = bns$fit_transform(raw.dtm, train.data$toxic)
train.dtm = raw.dtm

feature.selection = cv.glmnet(train.dtm, train.data$toxic, family = "binomial")

fit = glmnet(train.dtm, train.data$toxic, family = "binomial",
             alpha=1,lambda=feature.selection$lambda.1se)

beta = fit$beta

s.train <- train.dtm[,which(beta!=0)]

dense.train.dtm <- as.matrix(s.train)
