library(text2vec)
library(data.table)
library(magrittr)
library(stopwords)
library(SnowballC)
library(caret)
library(glmnet)
library(kernlab)

setwd("jigsaw-toxic-comment-classification-challenge")

sample       <- fread("sample_submission.csv")
test         <- fread("test.csv")
test_labels  <- fread("test_labels.csv")
train.data   <- fread("train.csv")

# undersample nontoxic group
toxic.id = which(train.data$toxic==1) # # of toxic = 15294
nontoxic.id = which(train.data$toxic==0) # # of nontoxic = 144277
set.seed(123)
nontoxic.sampleId = sample(nontoxic.id,length(toxic.id))
train.sample = train.data[c(toxic.id,nontoxic.sampleId),]

test.labels <- test_labels[test_labels$toxic!=-1,]
test.data <- test[test_labels$toxic!=-1,]

prep_fun = tolower
#stemming words
tok_fun = function(x) {
  word_tokenizer(x) %>% lapply( function(x) SnowballC::wordStem(x,language="en"))
}

it_train = itoken(train.sample$comment_text,
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  ids = train.sample$id,
                  progressbar = TRUE)

it_test = itoken(test.data$comment_text,
                 preprocessor = prep_fun,
                 tokenizer = tok_fun,
                 ids = test.data$id,
                 progressbar = TRUE)

vocab = create_vocabulary(it_train, ngram=c(1L,3L), stopwords=stopwords("en",source="smart"))

prune.vocab <- prune_vocabulary(vocab, term_count_min = 10)

vectorizer = vocab_vectorizer(prune.vocab)
raw.train.dtm = create_dtm(it_train, vectorizer) 
raw.test.dtm = create_dtm(it_test, vectorizer)

## lasso
set.seed(111)
feature.selection = cv.glmnet(raw.train.dtm, train.sample$toxic, family = "binomial")
fit = glmnet(raw.train.dtm, train.sample$toxic, family = "binomial",
             alpha=1,lambda=feature.selection$lambda.1se)

pred <- predict(fit, raw.test.dtm, type = "class")
confusionMatrix(as.factor(as.numeric(pred)), as.factor(test.labels$toxic), positive = "1")


## selected vocab (# = 122)
s.vocab <- prune.vocab[which(fit$beta!=0),]
s.vectorizer <- vocab_vectorizer(s.vocab)
dense.train.dtm <- as.matrix(raw.train.dtm[, which(fit$beta!=0)])
test.dtm <- create_dtm(it_test, s.vectorizer)

## svmRadial--based on terms selected from lasso
set.seed(111)
trCtl = trainControl(method = "cv")
tg <- data.frame(C = c(1,seq(10,100,10)), sigma = c(0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2))
fit_svmRadial = train(x = dense.train.dtm, 
                      y = as.factor(train.sample$toxic), 
                      method = "svmRadial", 
                      tuneGrid = tg,
                      trControl = trCtl)
pred_svmRadial = predict(fit_svmRadial, as.matrix(test.dtm))
confusionMatrix(pred_svmRadial, as.factor(test.labels$toxic), positive = "1")


## svmLinear--based on terms selected from lasso
set.seed(111)
trCtl = trainControl(method = "cv")
tg <- data.frame(C=c(1,seq(10,100,10)))
fit_svmLinear = train(x = dense.train.dtm, 
                      y = as.factor(train.sample$toxic), 
                      method = "svmLinear",
                      tuneGrid = tg,
                      trControl = trCtl)
pred_svmLinear = predict(fit_svmLinear, as.matrix(test.dtm))
confusionMatrix(pred_svmLinear, as.factor(test.labels$toxic), positive = "1")


save(fit_svmRadial$results, file = "../svmRadial.RData")
save(fit_svmLinear$results, file = "../svmLinear.RData")
sink(file = "svm.txt")