library(Matrix)
library(data.table)
train_mar <- readMM("/Users/tangxin/Desktop/Coursework/2019 Spring/BIOS 735/UmbrellaAcademy/jigsaw-toxic-comment-classification-challenge/train_mar.txt")
train_mar <- as.matrix(train_mar)
train_mar_id <- read.csv("/Users/tangxin/Desktop/Coursework/2019 Spring/BIOS 735/UmbrellaAcademy/jigsaw-toxic-comment-classification-challenge/train_mar_id.csv", stringsAsFactors=FALSE)
train_mar <- cbind(train_mar_id[,2], train_mar)
train_mar <- train_mar[order(train_mar[,1]),]
train_mar <- train_mar[,-1]
train_mar <- apply(train_mar,2,as.numeric)
train_mar[which(train_mar > 0)] <- 1 ##30588 by 277 matrix

train <- fread("/Users/tangxin/Desktop/Coursework/2019 Spring/BIOS 735/UmbrellaAcademy/jigsaw-toxic-comment-classification-challenge/train.csv")
train_sub <- train[which(unlist(train$id) %in% train_mar_id[,2]),]
train_sub <- train_sub[order(train_sub$id),]

label <- train_sub$toxic
l <- sapply(train_sub$comment_text, strsplit, " ")
gamma <- unlist(lapply(l,length)) ##30588 length vector, "length" of a comment text defined by number of words
gamma <- unname(gamma)
#gamma <- rep(0,nrow(train_mar))

y <- train_mar[,2]
n <- nrow(train_mar)

##Need input n, y (vector), gamma (vector), and some values for parameters (in computation)

mixlogistic <- function(y, gamma, tol = 10^-5, maxit = 50, prop_toxic = 0.5, label = NULL){

if(length(table(y)) == 1){stop("input data only contains 1 category")}

n = length(y)
iter = 0
eps = Inf
ll = -10000
fit = list()

## create posterior probability matrix
pp = matrix(0, n, 2)
colnames(pp) = c("non-toxic","toxic")

## use informative initialization, 
## should vary this and re-evaluate
prop_toxic = prop_toxic

## set everything greater than th
pp[y > quantile(y, prop_toxic),2] = 1
pp[,1] = 1 - pp[,2]

## now start the EM algorithm
start = Sys.time()
while(eps > tol & iter < maxit){
  
  ## save old ll
  ll0 = ll
  
  ## start M-step
  # pi, mean of component pp's
  pi = colMeans(pp)
  
  # thetak, weighted glm's based on pp
  for(i in 1:2) fit[[i]] = glm(y ~ 1, family = binomial(), offset = gamma, weights = pp[,i])
  
  ## start E-step
  # calculate numerator
  for(i in 1:2) pp[,i] = pi[i]*dbinom(y, size = 1, prob = fit[[i]]$fitted)
  
  # divide by denominator, the sum across components in each i
  pp = pp/rowSums(pp)
  
  ## calculate LL
  interior_sum = 0
  for(i in 1:2) interior_sum = interior_sum + pi[i]*dbinom(y, size = 1, prob = fit[[i]]$fitted) 
  ll = sum(log(interior_sum))
  
  ## calculate relative change in log likelihood  
  eps  = abs(ll-ll0)/abs(ll0)
  
  ## update iterator
  iter = iter + 1
  if(iter == maxit) warning("Iteration limit reached without convergence")
  
  ## print out info to keep track
  cat(sprintf("Iter: %d logL: %.2f pi1: %.3f  eps:%f\n",iter, ll,pi[1],eps))
}

marginalpred <- ifelse(pp[,2] > 0.5, 1, 0) #1 represent toxic and 0 represent non-toxic
marginalpredAcc <- length(which(marginalpred == label))/length(label)

return(list(Iteration = iter,
            logLikelihood = ll,
            epsilon = eps,
            PredAccuracy = marginalpredAcc))
}

train_mar_new <- train_mar[,-which(colSums(train_mar) == 0)]
l <- NULL
for(i in 1:ncol(train_mar_new)){
  l[i] <- length(table(train_mar_new[,i]))
}

r <- apply(train_mar_new[,1:30], 2, mixlogistic, gamma=gamma, tol=10^-5, maxit=50, prop_toxic=0.5, label=label)
accuracy <- unlist(lapply(r, '[[', 4))
##data has problem, train_mar[,13] is all zero##

##Model: given class label (toxicity), model probability of occurrence of word (adjusted by comment length)
##Prediction: given word occurrence indicator (adjusted by comment length), estimate probability of class (toxicity)

