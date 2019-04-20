#' Tokenization function for string splitting
#'
#' This function improves the simple `word_tokenizer` by applying stemming after tokenization.
#'
#' @param x character vector
#'
#' @return a list of character vectors, each element of the list contains vector of tokens.
#'
#' @examples
#' doc = c("I feel happy", "I get a feeling of happiness", "She feels happy")
#' tok_fun(doc)
#'
#' @import text2vec
#' @import magrittr
#' @import SnowballC
#'
#' @export
tok_fun = function(x) {
word_tokenizer(x) %>% lapply( function(x) SnowballC::wordStem(x,language="en"))
}

#' Data preprocessing
#'
#' This function converts raw comments data to a document-term matrix that can be used for classification.
#' By using up to 3-grams and then pruning vocabulary, it can significantly reduce the dimension.
#'
#' @param traindata a matrix, list or data frame for training, with one of its columns named "comment_text".
#' @param testdata a matrix, list or data frame for test, with one of its columns named "comment_text".
#' @param prep_fun a function which takes chunk of character vectors and does all pre-processing.
#' @param tok_fun a function which takes a character vector from preprocessor, split it into tokens and returns a list of character vectors.
#'
#' @return a list with the following elements:
#' \itemize{
#' \item{the document-term matrix for training data}
#' \item{the document-term matrix for test data}
#' }
#'
#' @examples
#' \dontrun{
#' data("train.data")
#' data("test.data")
#' N.train = 1000
#' N.test = 100
#' data.pre <- preprocess(traindata = train.data[1:N.train, ], testdata = test.data[1:N.test, ])
#' train.dtm <- data.pre$train.dtm
#' test.dtm  <- data.pre$test.dtm
#' }
#'
#' @import text2vec
#' @import magrittr
#' @import SnowballC
#' @import stopwords
#'
#' @export
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

  return(list(train.dtm = raw.dtm,
              test.dtm  = raw.test.dtm))
}
