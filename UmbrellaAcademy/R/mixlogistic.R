#' Marginal Keyword Check by Finite Mixture Logistic Regression
#' 
#' The function takes in a vector indicating the occurrence of a specific keyword and a vector storing the length of each comment,
#' fits a mixture logistic regression model with offset (via EM algorithm), then computes the posterior probabilities of toxicity,
#' and compare the classification result with a user-specified reference.
#' 
#' @param y A numeric vector containing 0's and 1's, indicating if a keyword occurs in the comments
#' @param gamma A numeric vector containing lengths of comments, usually use the word count of a comment as length
#' @param tol A numeric scalar controling the convergence criterion (change of log likelihood value) of EM algorithm,
#' the default value is set to 10^-5
#' @param maxit An integer indicating the maximum iteration for EM algorithm,
#' the default value is set to 50
#' @param prop_toxic A numeric scalar for initializing the parameter (proportion of toxic comment),
#' the default value is set to 0.5
#' @param label A numeric vector containing 0's and 1's, user-specified reference indicating toxicity of comments
#' @param Trace A boolean scalar, if Trace == T then prints the information of EM algorithm at each iteration
#' 
#' @return The function returns a list containing the number of iterations till the computation completes, the final
#' value of log likelihood, the final value of the change of log likelihood, overall prediction accuracy, sensitivity,
#' and specificity
#' 
#' @examples mixlogistic(train_mar[,1], gamma, label=label)
#' 
#' @import caret
#' 
#' @export
mixlogistic <- function(y, gamma, tol = 10^-5, maxit = 50, prop_toxic = 0.5, label = NULL, Trace=F){
  
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
    if(Trace == T){
      cat(sprintf("Iter: %d logL: %.2f pi1: %.3f  eps:%f\n",iter, ll,pi[1],eps))
    }
  }
  
  marginalpred <- ifelse(pp[,2] > 0.5, 1, 0) #1 represent toxic and 0 represent non-toxic
  marginalpredAcc <- length(which(marginalpred == label))/length(label)
  marginalpredSens <- confusionMatrix(as.factor(marginalpred), as.factor(label))$byClass[1]
  marginalpredSpec <- confusionMatrix(as.factor(marginalpred), as.factor(label))$byClass[2]
  return(list(Iteration = iter,
              logLikelihood = ll,
              epsilon = eps,
              PredAccuracy = marginalpredAcc,
              PredSensitivity = marginalpredSens,
              PredSpecificity = marginalpredSpec))
}