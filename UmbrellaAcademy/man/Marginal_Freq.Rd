% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marginal_frequency.R
\name{Marginal_Freq}
\alias{Marginal_Freq}
\title{Find informative keywords based on marginal frequency difference
for toxic and non-toxic comments classification problem}
\usage{
Marginal_Freq(commentData, label, f = NULL, id = "id",
  colname = "comment_text", min_count = 10, threshold = 0.02)
}
\arguments{
\item{commentData}{A dataframe of input comment data}

\item{label}{A 0-1 vector of toxic label corresponding to the input comment data. The function requires
that toxic comment has lebel 1 and non-toxic comment has label 0}

\item{f}{A function to compute the marginal frequency. By default the marginal frequency difference is
defined as
\deqn{d(w)=\frac{|f_1(w)-f_2(w)|}{max\{|f_1(w),f_2(w)|\}}.}}

\item{id}{Column's name for comment id. By Default is 'id'}

\item{colname}{Column's name for comment text. By default is 'comment_text'}

\item{min_count}{Minimum count for a keyword/phrase to be considered. By default is 10}

\item{threshold}{Threshold of marginal frequency difference for keywords to be identified as informative.By default is 0.02}
}
\value{
The function will return a keyword list of selected informative keywords and the complete frequency table for all keywords
\describe{
  \item{\strong{keyword}}{A vector of selected keywords}
  \item{\strong{freq.table}}{Complete frequency table for all keywords}
}
}
\description{
This function computes the marginal frequency difference table (using function \strong{tokenizer} from \strong{\emph{text2vec}} package) for a given input comment
dataset and return a list of selected informative keywords, along with the complete frequency table. Keywords are identified as informative if
the marginal frequency difference in toxic and non-toxic comments exceeds the prefixed threshold.
}
\examples{
Marginal_Freq(train.data, train.data$toxic)

}
