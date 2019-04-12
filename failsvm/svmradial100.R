library(text2vec)
library(data.table)
library(magrittr)
library(stopwords)
library(SnowballC)
library(caret)
library(glmnet)
library(kernlab)

setwd("/nas/longleaf/home/yutongl/BIOS735/svm")

load("prelasso.RData")

## svmRadial--based on terms selected from lasso
set.seed(111)
trCtl = trainControl(method = "cv") 
tg <- expand.grid(C = 100, sigma = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75,1))
fit_svmRadial = train(x = dense.train.dtm, 
                      y = as.factor(train.sample$toxic), 
                      method = "svmRadial", 
                      tuneGrid = tg,
                      trControl = trCtl)
pred_svmRadial = predict(fit_svmRadial, as.matrix(test.dtm))
confusionMatrix(pred_svmRadial, as.factor(test.labels$toxic), positive = "1")



save(fit_svmRadial$results, file = "./svmRadial100.RData")
