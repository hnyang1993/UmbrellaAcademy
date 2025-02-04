## generate seed by directory name
path <- getwd()
seed <- as.numeric(tail(unlist(strsplit(path, '/')), 1))
set.seed(seed)
filename <- paste("svmCV",seed,".RData",sep="")


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
svmGrid <- expand.grid(C=seq(10,100,by=10))

trCtl <- trainControl(method="cv", number=10, savePredictions=FALSE)
fit <- train(train_mar, as.factor(train_mar_y), method="svmLinear", trControl=trCtl, tuneGrid = svmGrid)

result <- fit

## save output to .RData file
save(result, file = filename)
