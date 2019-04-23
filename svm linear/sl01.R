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
fit_svmLinear_01 = train(x = dense.train.dtm, 
                        y = as.factor(train.sample$toxic), 
                        method = "svmLinear",
                        tuneGrid = tg,
                        trControl = trCtl)
pred_svmLinear_01 = predict(fit_svmLinear_01, as.matrix(test.dtm))
confusionMatrix(pred_svmLinear_01, as.factor(test.labels$toxic), positive = "1")

save(fit_svmLinear_01, file = "./sl01.RData")
sink(file = "svmlinear.txt")