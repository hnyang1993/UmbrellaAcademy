library(text2vec)
library(data.table)
library(magrittr)
library(stopwords)
library(SnowballC)
library(caret)
library(glmnet)

setwd("/pine/scr/b/j/bjia7/735project/")
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


vocab = create_vocabulary(it_train, ngram=c(1L,3L), stopwords=stopwords("en",source="smart"))

prune.vocab <- prune_vocabulary(vocab, term_count_min = 10)

vectorizer = vocab_vectorizer(prune.vocab)
raw.dtm = create_dtm(it_train, vectorizer)
dim(raw.dtm)

#bns = BNS$new()
#train.dtm = bns$fit_transform(raw.dtm, train.data$toxic)
train.dtm = raw.dtm
set.seed(111)
feature.selection = cv.glmnet(train.dtm, train.sample$toxic, family = "binomial",type.measure = "auc")
beta = coef(feature.selection,lambda = feature.selection$lambda.1se)

#colnames(train.dtm)[which(beta!=0)]

s.train <- train.dtm[,which(beta[-1]!=0)]

dense.train.dtm <- as.matrix(s.train)

library(tree)
library(randomForest)

# Create model with default paramters
control <- trainControl(method="cv", number=10, search="grid")
metric <- "Accuracy"
set.seed(1)
p <- dim(dense.train.dtm)[2]
tunegrid <- expand.grid(.mtry=c(2^(0:6),p))
rf_gridsearch <- train(dense.train.dtm, train.sample$toxic, 
                       method="rf", metric=metric, tuneGrid=tunegrid, 
                       trControl=control)

save(rf_gridsearch,file="/pine/scr/b/j/bjia7/735project/rf_gridsearch.Rdata")

