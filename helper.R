
##' Find nth highest column position for each row
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]  # function
df$first <- apply(df, 1, function(x)x[maxn(1)(x)])  # Maximum value
df$second <- apply(df, 1, function(x)x[maxn(1)(x)])  # Second highest value
                   
##' [rescale01] Function to rescale a variable from 0 to 1
rescale01 <- function(x) {
  (x - min(x)) / ((max(x)) - min(x))
}
