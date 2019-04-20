context("test-preprocess")

data("train.data")
data("test.data")
traindata = train.data[1:1000, ]
testdata = test.data[1:100, ]

test_that("errors for bad input", {
  expect_error(preprocess(traindata))
  expect_error(preprocess(traindata, testdata, prep_fun = mean))
  colnames(testdata) = c("id", "comment")
  expect_error(preprocess(traindata, testdata))
})
