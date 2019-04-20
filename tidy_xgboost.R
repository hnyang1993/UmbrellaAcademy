library(tidyverse)
library(magrittr)
library(text2vec)
library(tokenizers)
library(xgboost)
library(glmnet)
library(doParallel)
library(data.table)
library(tm)
library(stopwords)
library(SnowballC)
library(caret)
library(kernlab)
registerDoParallel(4)

subm      <- fread("sample_submission.csv")
test         <- fread("test.csv")
test_labels  <- fread("test_labels.csv")
train.data   <- fread("train.csv")

toxic.id = which(train.data$toxic==1) # # of toxic = 15294
nontoxic.id = which(train.data$toxic==0) # # of nontoxic = 144277
set.seed(123)
nontoxic.sampleId = sample(nontoxic.id,length(toxic.id))
train = train.data[c(toxic.id,nontoxic.sampleId),]
test_labels_use <- test_labels[test_labels$toxic!=-1,]
test_use <- test[test_labels$toxic!=-1,]

prep_fun = tolower

tok_fun = function(x) {
  word_tokenizer(x) %>% lapply( function(x) SnowballC::wordStem(x,language="en"))
}

it_train = itoken(train$comment_text,
                  preprocessor = prep_fun,
                  tokenizer = tokenize_word_stems,
                  ids = train$id,
                  progressbar = TRUE)

vocab = create_vocabulary(it_train, ngram=c(1L,3L), stopwords=stopwords("en",source="smart"))

prune.vocab <- prune_vocabulary(vocab, term_count_min = 10)

vectorizer = vocab_vectorizer(prune.vocab)

tri <- 1:nrow(train)
targets <- c("toxic", "severe_toxic", "obscene", "threat", "insult", "identity_hate")
target <- "toxic"

#---------------------------
cat("Basic preprocessing & stats...\n")
tr_te <- train %>% 
  select(-one_of(targets)) %>% 
  bind_rows(test_use) %>% 
  mutate(length = str_length(comment_text),
         ncap = str_count(comment_text, "[A-Z]"),
         ncap_len = ncap / length,
         nexcl = str_count(comment_text, fixed("!")),
         nquest = str_count(comment_text, fixed("?")),
         npunct = str_count(comment_text, "[[:punct:]]"),
         nword = str_count(comment_text, "\\w+"),
         nsymb = str_count(comment_text, "&|@|#|\\$|%|\\*|\\^"),
         nsmile = str_count(comment_text, "((?::|;|=)(?:-)?(?:\\)|D|P))")) %>% 
  select(-id) %T>% 
  glimpse()

#---------------------------
cat("Parsing comments...\n")
it <- tr_te %$%
  str_to_lower(comment_text) %>%
  str_replace_all("[^[:alpha:]]", " ") %>%
  str_replace_all("\\s+", " ") %>%
  itoken(tokenizer = tokenize_word_stems)

m_tfidf <- TfIdf$new(norm = "l2", sublinear_tf = T)
tfidf <- create_dtm(it, vectorizer) %>%
  fit_transform(m_tfidf)  

m_lsa <- LSA$new(n_topics = 25, method = "randomized")
lsa <- fit_transform(tfidf, m_lsa)

#---------------------------
cat("Preparing data for glmnet...\n")
X <- tr_te %>% 
  select(-comment_text) %>% 
  sparse.model.matrix(~ . - 1, .) %>% 
  cbind(tfidf, lsa)

X_test <- X[-tri, ]
X <- X[tri, ]
# ncol=13814

#---------------------------
cat("Training & predicting...\n")

p <- list(objective = "binary:logistic", 
          booster = "gbtree", 
          eval_metric = "auc", 
          nthread = 4, 
          eta = 0.01, 
          max_depth = 6,
          min_child_weight = 4,
          subsample = 0.7,
          colsample_bytree = 0.7)

#---------------------------
cat("\nFitting", target, "...\n")
y <- train[[target]]
## xgboost
m_xgb <- xgboost(X, y, params = p, print_every_n = 100, nrounds = 500)
## glmnet
m_glm <- cv.glmnet(X, factor(y), alpha = 1, family = "binomial", type.measure = "auc",
                   parallel = T, standardize = T, nfolds = 10)

pred = 0.2*predict(m_xgb, X_test) + 0.8*predict(m_glm, X_test, type="response", s = "lambda.min")
pred = ifelse(pred > 0.5, 1, 0)

confusionMatrix(as.factor(pred), as.factor(test_labels_use$toxic))
#  Confusion Matrix and Statistics
#             Reference
#  Prediction  0     1
#    0       49287   377
#    1       8601   5713
  
#  Overall Accuracy : 0.8597 
#  Toxic Accuracy   : 0.9381
#  Nontoxic Accuracy: 0.8514