% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixlogistic.R
\name{mixlogistic}
\alias{mixlogistic}
\title{Marginal Keyword Check by Finite Mixture Logistic Regression}
\usage{
mixlogistic(y, gamma, tol = 10^-5, maxit = 50, prop_toxic = 0.5,
  label = NULL, Trace = F)
}
\arguments{
\item{y}{A numeric vector containing 0's and 1's, indicating if a keyword occurs in the comments}

\item{gamma}{A numeric vector containing lengths of comments, usually use the word count of a comment as length}

\item{tol}{A numeric scalar controling the convergence criterion (change of log likelihood value) of EM algorithm,
the default value is set to 10^-5}

\item{maxit}{An integer indicating the maximum iteration for EM algorithm,
the default value is set to 50}

\item{prop_toxic}{A numeric scalar for initializing the parameter (proportion of toxic comment),
the default value is set to 0.5}

\item{label}{A numeric vector containing 0's and 1's, user-specified reference indicating toxicity of comments}

\item{Trace}{A boolean scalar, if Trace == T then prints the information of EM algorithm at each iteration}
}
\value{
The function returns a list containing the number of iterations till the computation completes, the final
value of log likelihood, the final value of the change of log likelihood, overall prediction accuracy, sensitivity,
and specificity
}
\description{
The function takes in a vector indicating the occurrence of a specific keyword and a vector storing the length of each comment,
fits a mixture logistic regression model with offset (via EM algorithm), then computes the posterior probabilities of toxicity,
and compare the classification result with a user-specified reference.
}
\examples{
mixlogistic(train_mar[,1], gamma, label=label)

}
