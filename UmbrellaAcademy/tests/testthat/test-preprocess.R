context("test-preprocess")

data("train.data")
data("test.data")
traindata = train.data[1:1000, ]
testdata = test.data[1:100, ]

test_that("errors for bad input", {
  expect_error(preprocess(traindata))
  expect_error(preprocess(traindata, testdata, prep_fun = 1))
})

test_that("two elements of the output have equal number of columns", {
  pre = preprocess(traindata, testdata)
  expect_equal(ncol(pre$train.dtm), ncol(pre$test.dtm))
})
