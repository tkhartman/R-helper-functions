
##' Find nth highest column position for each row
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]  # function
df$first <- apply(df, 1, function(x)x[maxn(1)(x)])  # Maximum value
df$second <- apply(df, 1, function(x)x[maxn(1)(x)])  # Second highest value
