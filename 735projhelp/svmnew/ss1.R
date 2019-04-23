setwd("/nas/longleaf/home/yutongl/BIOS735/735projhelp/svmsep")
set.seed(111)
library(Matrix)
library(data.table)
train_mar <- readMM("/nas/longleaf/home/yutongl/BIOS735/735projhelp/svm/train_mar.txt")
train_mar <- as.matrix(train_mar)
keyword <- read.csv("/nas/longleaf/home/yutongl/BIOS735/735projhelp/svm/key_word_list.csv", stringsAsFactors = FALSE)
colnames(train_mar) <- keyword[,2]
train_mar_id <- read.csv("/nas/longleaf/home/yutongl/BIOS735/735projhelp/svm/train_mar_id.csv", stringsAsFactors=FALSE)
train_mar_y <- read.csv("/nas/longleaf/home/yutongl/BIOS735/735projhelp/svm/train_mar_label.csv", stringsAsFactors=FALSE)

train_mar_y <- train_mar_y$toxic

library(caret)
svmGrid <- expand.grid(C=0.1, sigma = sqrt(277))

trCtl <- trainControl(method="cv", number=10, savePredictions=FALSE)
fit <- train(train_mar, as.factor(train_mar_y), method="svmRadial", trControl=trCtl, tuneGrid = svmGrid)

result1 <- fit

## save output to .RData file
save(result1, file = "/nas/longleaf/home/yutongl/BIOS735/735projhelp/svmsep/s1.RData")
