context("test-mixlogistic")

data("train_mar")
data("train_mar_id")
data("train.data")
train_mar <- cbind(train_mar_id[,2], train_mar)
train_mar <- train_mar[order(train_mar[,1]),]
train_mar <- train_mar[,-1]
train_mar <- apply(train_mar,2,as.numeric)
train_mar[which(train_mar > 0)] <- 1 ##30588 by 277 matrix

train_sub <- train.data[which(unlist(train.data$id) %in% train_mar_id[,2]),]
train_sub <- train_sub[order(train_sub$id),]

label <- train_sub$toxic
l <- sapply(train_sub$comment_text, strsplit, " ")
gamma <- unlist(lapply(l,length)) ##30588 length vector, "length" of a comment text defined by number of words
gamma <- unname(gamma)
#gamma <- rep(0,nrow(train_mar))

y <- train_mar[,1]
n <- nrow(train_mar)

train_mar_new <- train_mar[,-which(colSums(train_mar) == 0)]

test_that("occurrence vector y does not only contain values 0 or 1", {
  expect_error(mixlogistic(y=rbinom(n,2,0.5), gamma=gamma, label=label))
  expect_error(mixlogistic(y=rep(0,n), gamma=gamma, label=label))
})

test_that("label for reference is missing",{
  expect_error(mixlogistic(y=y, gamma=gamma))
})

test_that("EM algorithm converges", {
  res <- mixlogistic(y=y, gamma=gamma, label=label)
  res2 <- mixlogistic(y=train_mar[,19], gamma=gamma, label=label)
  expect_equal(res$epsilon, 0)
  expect_gt(res2$epsilon, 0)
})