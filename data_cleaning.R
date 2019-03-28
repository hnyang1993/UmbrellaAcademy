library(data.table)
library(text2vec)
library(data.table)
library(magrittr)
library(stringr)
setwd("/Users/yugu/Desktop/735project/jigsaw-toxic-comment-classification-challenge")
train = fread("train.csv")
test = fread("test.csv")
toxic = train[which(train$toxic==1),]
severe_toxic = train[which(train$severe_toxic==1),]
obscene = train[which(train$obscene==1),]
threat = train[which(train$threat==1),]
insult = train[which(train$insult==1),]
identity_hate = train[which(train$identity_hate==1),]

tok_fun = word_tokenizer
getvocab = function(train){
  setDT(train)
  setkey(train, id)
  # create the vocabulary
  train_tokens = train$comment_text %>% 
    prep_fun %>% 
    tok_fun
  it_train = itoken(train_tokens, 
                    ids = train$id,
                    progressbar = FALSE)
  stop_words = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", 
                 "you", "your", "yours", "the", "in", "on", "a", "an", "but", "or", "was", "if", "with", "are", "have", "as", 
                 "be", "not", "this", "for", "it", "that", "is", "and", "of", "to" )
  vocab = create_vocabulary(it_train, stopwords = stop_words)
  # delete non-English terms   
  vocab = vocab[grep("[a-z]", vocab$term),]
  # delete terms containing numeric values
  vocab = vocab[grep("[0-9]", vocab$term, invert = TRUE),]
  # add a variable "long" to indicate whether a term is long (length > 20)
  vocab_length = str_length(vocab$term)
  long = rep(0, length(vocab_length))
  long[which(vocab_length > 20)] = 1
  # match long terms against short terms
  index_long = sapply(vocab$term[which(long==0)], grep, x = vocab$term[which(long==1)])
  count = sapply(index_long, length)
  # select successfully matched short terms
  index_short = match(names(which(count > 0)), vocab$term)
  # update term_count and doc_count for short terms 
  # may be problematic
  vocab[index_short, 2:3] = vocab[index_short, 2:3] + count[which(count > 0)]
  # use all the short terms as the vocabulary
  vocab = vocab[which(long==0),]
  return(vocab)
}
