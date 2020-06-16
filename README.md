# Blblmpar
An updated version of bag of little bootstraps procedure to calculate regression coefficients and confidence intervals.

To summarize, we use bag of little bootstraps procedure as a more efficient approach to obtaining confidence intervals when a simple "mapreduce" operation is not efficient for obtaining the sum of confidence intervals from our sampled datasets. We begin by splitting our dataset into multiple datasets by obtaining samples without replacement. Then, we obtain a statistic such as regression coefficients and then a bootstrap statistic (our confidence interval). Lastly, we apply our reduce operation to obtain the overall confidence interval lower and upper bounds by which our the mean regression coefficient lies.

In order to optimize our package, I have written the description, vignette, and allowed the R package to include parallelization through adding "furrr" package, changing "map" to "future_map" in our mapreduce operation, and planned 4 CPUs to process our data instead of just one.
