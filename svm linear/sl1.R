library(text2vec)
library(data.table)
library(magrittr)
library(stopwords)
library(SnowballC)
library(caret)
library(glmnet)
library(kernlab)

setwd("/nas/longleaf/home/yutongl/BIOS735/slinear")

load("prelasso.RData")

## svmLinear--based on terms selected from lasso
set.seed(111)
trCtl = trainControl(method = "cv")
tg <- data.frame(C=10)
fit_svmLinear_1 = train(x = dense.train.dtm, 
                      y = as.factor(train.sample$toxic), 
                      method = "svmLinear",
                      tuneGrid = tg,
                      trControl = trCtl)
pred_svmLinear_1 = predict(fit_svmLinear_1, as.matrix(test.dtm))
confusionMatrix(pred_svmLinear_1, as.factor(test.labels$toxic), positive = "1")

save(fit_svmLinear_1, file = "./sl1.RData")
sink(file = "svmlinear.txt")