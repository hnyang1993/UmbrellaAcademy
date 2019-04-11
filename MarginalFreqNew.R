## Text mining packages
library(text2vec)
library(data.table)
library(magrittr)
library(stopwords)
library(SnowballC)

## Training packages
library(glmnet)
library(randomForest)
library(e1071)

## Data Directory
#setwd("/Users/liumochuan/Documents/UNC BIOS/735/GitPro/UmbrellaAcademy/jigsaw-toxic-comment-classification-challenge")
sample       <- fread("sample_submission.csv")
test         <- fread("test.csv")
test_labels  <- fread("test_labels.csv")
train.data   <- fread("train.csv")

test.data <- merge(test, test_labels, by = "id")
test.data <- test.data[test.data$toxic%in%c(0, 1), ]
test.data <- test.data[1:10000, ]

## 1:1
toxic.id <- which(train.data$toxic==1) # # of toxic = 15294
nontoxic.id <- which(train.data$toxic==0) # # of nontoxic = 144277
set.seed(123)
nontoxic.sampleId <- sample(nontoxic.id, length(toxic.id))
train.sample <- train.data[c(toxic.id, nontoxic.sampleId),]

train.data.toxic <- train.sample[train.sample$toxic==1, ]
train.data.nontoxic <- train.sample[train.sample$toxic==0, ]

N.toxic <- dim(train.data.toxic)[1]
N.nontoxic <- dim(train.data.nontoxic)[1]

prep_fun = tolower
#stemming words
tok_fun = function(x) {
  word_tokenizer(x) %>% lapply( function(x) SnowballC::wordStem(x,language="en"))
}

## Toxic count
it_train.toxic = itoken(train.data.toxic$comment_text,
                        preprocessor = prep_fun,
                        tokenizer = tok_fun,
                        ids = train.data.toxic$id,
                        progressbar = TRUE)


vocab.toxic = create_vocabulary(it_train.toxic, ngram=c(1L,3L), stopwords=stopwords("en",source="smart"))
prune.vocab.toxic <- prune_vocabulary(vocab.toxic, term_count_min = 10)

## Nontoxic count
it_train.nontoxic = itoken(train.data.nontoxic$comment_text,
                           preprocessor = prep_fun,
                           tokenizer = tok_fun,
                           ids = train.data.nontoxic$id,
                           progressbar = TRUE)


vocab.nontoxic = create_vocabulary(it_train.nontoxic, ngram=c(1L,3L), stopwords=stopwords("en",source="smart"))
prune.vocab.nontoxic <- prune_vocabulary(vocab.nontoxic, term_count_min = 10)


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

## r = abs(f_1-f_2)/max(|f_1|,|f_2|)
term.count$termPerDiff <- abs(term.count$termPercent.nontoxic - term.count$termPercent.toxic)/max(term.count$termPercent.nontoxic, term.count$termPercent.toxic)

## r > 0.02
raw.list <- term.count$term[pmax(term.count$termCount.nontoxic, term.count$termCount.toxic)>5&term.count$termPerDiff>0.02]

## Split itoken items into single words
clean.list <- c()

for(word in raw.list){
  word.split <- unlist(strsplit(word ,split='[[:punct:]]'))
  word.split <- unique(word.split)
  word.split <- word.split[nchar(word.split)>=2]
  clean.list <- c(clean.list, word.split)
}

## Key word list (K = 269)

clean.list <- unique(clean.list)
#sentimentr::sentiment(clean.list)


## Sub training / testing
CommentData <- train.sample[,1:2]
IndexData <- train.sample[,c(1,3:8)]

CommentTestData <- test.data[, 1:2]
IndexTestData <-  test.data[, c(1,3:8)]

## Generate counting sparse matrix
comment_matrix <- function(comment.data, key.word.list){
  
  require('Matrix')
  require('stringr')
  
  K <- length(key.word.list)
  N <- dim(comment.data)[1]
  
  count.matrix <- Matrix(0, nrow = N, ncol = K, sparse = TRUE)
  
  for(n in 1:N){
    current.comment <- comment.data$`comment_text`[n]
    for(k in 1:K){
      key.word <- key.word.list[k]
      current.count <- str_count(string = current.comment, pattern = key.word)
      
      if(current.count>0) count.matrix[n, k] <- current.count
      
    }
    print(n)
  }
  
  return(count.matrix)
}

comment.matrix <- comment_matrix(CommentData, clean.list)
test.matrix <- comment_matrix(CommentTestData, clean.list)

#library('Matrix')
#writeMM(test.matrix, file='/Users/liumochuan/Documents/UNC BIOS/735/test_mar.txt')
#writeMM(comment.matrix, file='/Users/liumochuan/Documents/UNC BIOS/735/train_mar.txt')

## Lasso (CV)
cv.model <- cv.glmnet(x = comment.matrix, y = IndexData$toxic, family = "binomial")
model.lasso <- glmnet(x = comment.matrix, y = IndexData$toxic, family = "binomial", alpha = 1, lambda = cv.model$lambda.min)

label.predict <- as.numeric(predict(model.lasso, newx = test.matrix, type = "class"))
label.true <- IndexTestData$`toxic`

sum(label.predict == label.true)/length(label.predict) # 0.846
sum(label.predict[label.true==1] == label.true[label.true==1])/sum(label.true==1) # 0.748
sum(label.predict[label.true==0] == label.true[label.true==0])/sum(label.true==0) # 0.857


## SVM (default radial basis)
model.svm <- svm(x = comment.matrix, y = factor(IndexData$toxic))

label.predict <- predict(model.svm, test.matrix)
label.true <- IndexTestData$`toxic`

sum(label.predict == label.true)/length(label.predict) # 0.750
sum(label.predict[label.true==1] == label.true[label.true==1])/sum(label.true==1) # 0.866
sum(label.predict[label.true==0] == label.true[label.true==0])/sum(label.true==0) # 0.737


## Random Forest
model.rf <- randomForest(x = as.matrix(comment.matrix), y = factor(IndexData$toxic))

label.predict <- predict(model.rf , test.matrix)
label.true <- IndexTestData$`toxic`

sum(label.predict == label.true)/length(label.predict) # 0.798
sum(label.predict[label.true==1] == label.true[label.true==1])/sum(label.true==1) # 0.844
sum(label.predict[label.true==0] == label.true[label.true==0])/sum(label.true==0) # 0.792

