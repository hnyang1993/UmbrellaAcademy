## Generate counting sparse matrix for marginal frequency
## Input keyword list and commentdata and keyword list
comment_matrix <- function(commentData, key.word.list, colname = 'comment_text', progress = TRUE){
  
  ## Package
  require('Matrix')
  require('stringr')
  require('svMisc')
  ##
  
  ## Change column name to default 'id'
  col.id <- which(names(commentData)== id)
  names(commentData)[col.id] <- 'id'
  
  K <- length(key.word.list)
  N <- dim(commentData)[1]
  
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
  
  if(progess == TRUE)   close(pb)
  
  return(count.matrix)
}

#comment.matrix <- comment_matrix(commentData, key.word.list)
#test.matrix <- comment_matrix(CommentTestData, clean.list)




