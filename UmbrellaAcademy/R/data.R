#' Training data
#'
#' This is the training set containing comments with their binary labels.
#'
#' @format A data frame with 159571 rows and 8 variables:
#' \describe{
#' \item{id}{Comment ID}
#' \item{comment_text}{Raw Wikipedia comments}
#' \item{toxic}{binary label for type "toxic"}
#' \item{severe_toxic}{binary label for type "severe_toxic"}
#' \item{obscene}{binary label for type "obscene"}
#' \item{threat}{binary label for type "threat"}
#' \item{insult}{binary label for type "insult"}
#' \item{identity_hate}{binary label for type "identity_hate"}
#' }
#'
#' @source \url{https://www.kaggle.com/c/jigsaw-toxic-comment-classification-challenge/data}
"train.data"

#' Test data
#'
#' This is the test set containing comments from approximately 153k subjects.
#'
#' @format  A data frame with 153164 rows and 2 variables:
#' \describe{
#' \item{id}{Comment ID}
#' \item{comment_text}{Raw Wikipedia comments}
#' }
#'
#' @source \url{https://www.kaggle.com/c/jigsaw-toxic-comment-classification-challenge/data}
"test.data"

#' Test labels
#'
#' This data set contains labels for the test data. Value of -1 indicates it was not used for scoring.
#'
#' @format A data frame with 153164 rows and 7 variables:
#' \describe{
#' \item{id}{Comment ID}
#' \item{toxic}{label for type "toxic"}
#' \item{severe_toxic}{label for type "severe_toxic"}
#' \item{obscene}{label for type "obscene"}
#' \item{threat}{label for type "threat"}
#' \item{insult}{label for type "insult"}
#' \item{identity_hate}{label for type "identity_hate"}
#' }
#'
#' @source \url{https://www.kaggle.com/c/jigsaw-toxic-comment-classification-challenge/data}
"test_labels"
