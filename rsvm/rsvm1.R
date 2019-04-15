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
tg <- expand.grid(C = seq(10,100,10), sigma = c(0.5,1,1.5))
fit_svmRadial = train(x = dense.train.dtm, 
                      y = as.factor(train.sample$toxic), 
                      method = "svmRadial", 
                      tuneGrid = tg,
                      trControl = trCtl)
pred_svmRadial = predict(fit_svmRadial, as.matrix(test.dtm))
confusionMatrix(pred_svmRadial, as.factor(test.labels$toxic), positive = "1")



save(fit_svmRadial, file = "./rsvm1.RData")
