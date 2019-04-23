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

train_mar_full <- cbind(train_mar_y, train_mar)
train_mar_full <- as.data.frame(train_mar_full)
library(e1071)

fit <- tune.svm(as.factor(train_mar_y)~., data = train_mar_full, gamma = 2^(-8:8), cost = 10)

result5 <- fit

## save output to .RData file
save(result5, file = "/nas/longleaf/home/yutongl/BIOS735/735projhelp/svmsep/s5.RData")
