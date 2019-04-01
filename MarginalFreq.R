library(text2vec)
library(data.table)
library(magrittr)
library(stopwords)
library(SnowballC)
library(caret)
library(glmnet)
setwd("/Users/liumochuan/Documents/UNC BIOS/735/GitPro/UmbrellaAcademy/jigsaw-toxic-comment-classification-challenge")
sample       <- fread("sample_submission.csv")
test         <- fread("test.csv")
test_labels  <- fread("test_labels.csv")
train.data   <- fread("train.csv")

train.data.toxic <- train.data[train.data$toxic==1, ]
train.data.nontoxic <- train.data[train.data$toxic==0, ]

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

term.count$termPercent.toxic <- term.count$termCount.toxic/N.toxic
term.count$docPercent.toxic <- term.count$docCount.toxic/N.toxic

term.count$termPercent.nontoxic <- term.count$termCount.nontoxic/N.nontoxic
term.count$docPercent.nontoxic <- term.count$docCount.nontoxic/N.nontoxic

term.count$termPerDiff <- abs(term.count$termPercent.nontoxic - term.count$termPercent.toxic)/max(term.count$termPercent.nontoxic, term.count$termPercent.toxic)

term.count$term[pmax(term.count$termCount.nontoxic, term.count$termCount.toxic)>5&term.count$termPerDiff>0.02]








