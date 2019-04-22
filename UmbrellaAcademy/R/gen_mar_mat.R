#' Compute feature matrix for toxic/
#' for toxic and non-toxic comments classification problem
#'
#' This function will compute the feature (word count) matrix for a comment dataset given an input
#' keyword list
#'
#' @param commentData A dataframe of input comment data
#' @param key.word.list A vector of keyword list for which the the number of total count
#' in each comment will be computed
#' @param colname Column's name for comment text. By default is 'comment_text'
#' @param progress Display the progress bar if TRUE
#'
#' @return The function will return a sparse matrix whose column is the word count of
#' the corresponding keyword in each comment
#'
#' @examples
#' out <- Marginal_Freq(train.data, train.data$toxic)
#' comment_matrix(train.data, out$keyword)
#'
#' @import Matrix
#' @import stringr
#' @import svMisc
#'
#' @export
comment_matrix <- function(commentData, key.word.list, id = 'id', colname = 'comment_text', progress = TRUE){
  
  ## Testing functions
  if(!is.data.frame(commentData))  stop("Input data is not a dataframe")
  if(is.null(commentData[, id])) stop("Id is missing")
  if(is.null(commentData[, colname])) stop("Comment text is missing")
  
  ## Package
  #require('Matrix')
  #require('stringr')
  #require('svMisc')
  ##

  ## Change column name to default 'id'
  col.id <- which(names(commentData)== id)
  names(commentData)[col.id] <- 'id'
  
  ## Change column name to default 'coment_text'
  col.comment <- which(names(commentData)== colname)
  names(commentData)[col.comment] <- 'comment_text'

  K <- length(key.word.list)
  N <- dim(commentData)[1]
  
  check_point <- 0
  
  if(dim(commentData)[1]<=1000) progress <- FALSE

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = 1, initial = 0, style = 3)
    check_point <- ceiling(seq(0,1,0.05)*N)
    check_point[1] <- check_point + 1
    progress_point <- seq(0,1,0.05)
    setTxtProgressBar(pb, 0.0001)
  }

  count.matrix <- Matrix(0, nrow = N, ncol = K, sparse = TRUE)

  for(n in 1:N){

    current.comment <- commentData$`comment_text`[n]

    for(k in 1:K){
      key.word <- key.word.list[k]
      current.count <- str_count(string = current.comment, pattern = key.word)

      if(current.count>0) count.matrix[n, k] <- current.count

    }

    if(progress == TRUE&n%in%check_point){
      k <- which(check_point==n)
      setTxtProgressBar(pb, progress_point[k])
    }
  }

  if(progress == TRUE) close(pb)

  rownames(count.matrix) <- col.id
  colnames(count.matrix) <- key.word.list

  return(count.matrix)
}

#comment.matrix <- comment_matrix(commentData, key.word.list)
#test.matrix <- comment_matrix(CommentTestData, clean.list)




