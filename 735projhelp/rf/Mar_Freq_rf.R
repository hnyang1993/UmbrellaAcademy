## generate seed by directory name
path <- getwd()
seed <- as.numeric(tail(unlist(strsplit(path, '/')), 1))
set.seed(seed)
filename <- paste("rfCV",seed,".RData",sep="")


library(Matrix)
library(data.table)
train_mar <- readMM("/nas/longleaf/home/hnyang/735proj/rf/train_mar.txt")
train_mar <- as.matrix(train_mar)
keyword <- read.csv("/nas/longleaf/home/hnyang/735proj/rf/key_word_list.csv", stringsAsFactors = FALSE)
colnames(train_mar) <- keyword[,2]
train_mar_id <- read.csv("/nas/longleaf/home/hnyang/735proj/rf/train_mar_id.csv", stringsAsFactors=FALSE)
train_mar_y <- read.csv("/nas/longleaf/home/hnyang/735proj/rf/train_mar_label.csv", stringsAsFactors=FALSE)

train_mar_y <- train_mar_y$toxic

library(caret)
rfGrid <- expand.grid(mtry=c(2, 4, 8, 12, 16, 20, 24, 28, 32, 36))

trCtl <- trainControl(method="cv", number=10, savePredictions=FALSE)
fit <- train(train_mar, as.factor(train_mar_y), method="rf", trControl=trCtl, tuneGrid = rfGrid)

result <- fit

## save output to .RData file
save(result, file = filename)
