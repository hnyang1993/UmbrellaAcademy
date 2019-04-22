context("test-MarginalFrequency")

test_that("Marginal_Freq works properly", {
  
  d <-  data.frame(id=c(1:40),comment_text=c(rep("fuck",20),rep("good",20)))
  l <- c(rep(1,20),rep(0,20))
  d$comment_text <- as.character(d$comment_text)
  
  expect_error(Marginal_Freq(1,1))
  expect_error(Marginal_Freq(d[1:10, ],l))
  expect_error(Marginal_Freq(d[, 1],l))
  expect_equal(Marginal_Freq(d,l)$keyword, c("fuck", "good"))
  expect_equal(Marginal_Freq(d,l)$freq.table[, 2], c(20, 0))
  
})

test_that("comment_matrix works properly", {
  
  d <- data.frame(id = 1, comment_text="good fuck")
  key.word.list <- c("good", "fuck", "lol")
  
  l <- c(1,1,0)
  names(l) <- key.word.list
  
  expect_error(comment_matrix(1, key.word.lis))
  expect_equal(comment_matrix(d, key.word.list)[1,], l)
  
})

