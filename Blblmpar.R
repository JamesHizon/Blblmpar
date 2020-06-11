### @import purrr
#' @import stats
#' @import furrr # Parallel Computing
#' @importFrom magrittr %>%
#' @import tidyverse
#' @details
#' Parallel Computation using Linear Regression with Little Bag of Bootstraps
#'
#' @param formula variabble formula for regression model
#' @param data dataframe eing used
#' @param m number of subsamples
#' @param B number of bootstrap samples
#' @param workers number of clusters being used
#'
#' @return fitted model using 4 clusters and blb
#' @export
#' @examples
#' blblmpar(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, cl = 4)
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

### NOTE:##########################################################
# - We will be creating multiple functions
# in our package to perform machine learning tasks
# such as regression for predictions.
# - Some functions will include other
# created functions, as does blblreg.
###################################################################

### Libraries:
#library(purrr)
library(stats)
library(furrr)
library(magrittr)
library(tidyverse)

#' @export # Export function
### Parallel Bag of Little Bootstraps Logistic Regression:
blblmpar <- function(formula, data, m = 10, B = 5000, cl) {

  # Split data into list of m files:
  data_list <- split_data(data, m)
  # The following will work occasionally.
  # May require continual restarting.
  suppressWarnings(plan(multiprocess, workers = cl))
  # Generate estimates from each file
  estimates <- future_map(
    data_list,
    ~ lmpar_each_subsample(formula = formula,
                          data = .,
                          n = nrow(data),
                          B = B))
  res <- list(estimates = estimates,
              formula = formula)
  class(res) <- "blblmpar"
  invisible(res)
}



#' split data into m parts of approximated equal sizes
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data),
                    replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
#' Will be used in blblm():
lmpar_each_subsample <- function(formula, data, n, B) {
  replicate(B, lmpar_each_boot(formula, data, n),
            simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#' Will be used in lm_each_subsample()
lmpar_each_boot <- function(formula, data, n) {
  # Generate multinomially distributed random number vectors
  # and compute multinomial probabilities:
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lmpar(formula, data, freqs)
}


#' obtain the linear regression estimates based on given the number of repetitions
lmpar <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(fit),
       sigma = blbsigma(fit))
}


#' compute the coefficients from fit
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' @export
#' @method print blblmpar
print.blblmpar <- function(x, ...) {
  cat("blblmpar model:", capture.output(x$formula))
  cat("\n")
}


#' @export
#' @method sigma blblmpar
sigma.blblmpar <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"),
                          c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

### Obtain coefficients of bag of little bootstraps:
#' @export
#' @method coef blblmpar
coef.blblmpar <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>%
             rowMeans())
}


#' @export
#' @method confint blblmpar
confint.blblmpar <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(fit$formula), "term.labels")
  }
  alpha <- 1 - 0.95 # Alpha for CI
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>%
               quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

### Generate predictions using blblmpar function:
#' @export
#' @method predict blblmpar
predict.blblmpar <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
               apply(1, mean_lwr_upr, level = level) %>%
               t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
               rowMeans())
  }
}

# 95% Bootstrap CI interval of mean:
mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>%
      set_names(c("lwr", "upr")))
}

# Obtain mean through map + reduce:
map_mean <- function(.x, .f, ...) {
  (future_map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

# Map reduce column bind:
map_cbind <- function(.x, .f, ...) {
  future_map(.x, .f, ...) %>% reduce(cbind)
}

# Map reduce row bind:
map_rbind <- function(.x, .f, ...) {
  future_map(.x, .f, ...) %>% reduce(rbind)
}
