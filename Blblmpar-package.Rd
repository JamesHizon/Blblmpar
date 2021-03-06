% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Blblmpar.R
\docType{package}
\name{Blblmpar-package}
\alias{Blblmpar}
\alias{Blblmpar-package}
\title{Blblmpar: Linear Regression Bag of Little Bootstraps with Parallelization}
\arguments{
\item{formula}{variabble formula for regression model}

\item{data}{dataframe eing used}

\item{m}{number of subsamples}

\item{B}{number of bootstrap samples}

\item{workers}{number of clusters being used}
}
\value{
fitted model using 4 clusters and blb
}
\description{
The following package will perform linear regression through bag of little bootstraps procedure. Bag of little bootstraps is a method that incorporates features of both the bootstrap and subsampling to obtain a robust, computationally efficient means of assessing the quality of estimators. This involves use sampling without replacement the sample s into sizes of b. Then, for each subsample, we resample each until sample size is n, r times. We compute the bootstrap statistic for each bootstrap sample. In our case, our bootstrap statistic is to obtain the linear regression coefficients. Next, we compute the confidence interval from the bootstrap statistics. Lastly, we obtain the average of the statistics while applying "mapreduce" method. Since we are applying parallelization if specified by user, we will use "future_map()" instead of the usual "map()" function. If necessary, we can alter the functions create to perform logistic regression to obtain the probability of binary outcomes. We can start by using glm() with family = "binomial" and altering the corresponding functions.  Another way of optimizing this R package is to convert code to C++ using Rcpp. For the sake of project completion, this will not be implemented.
}
\details{
Parallel Computation using Linear Regression with Little Bag of Bootstraps
}
\examples{
blblmpar(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, cl = 4)
}
