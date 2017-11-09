
##' [maxn] Find nth highest column position for each row
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]  # function
df$first <- apply(df, 1, function(x)x[maxn(1)(x)])  # Maximum value
df$second <- apply(df, 1, function(x)x[maxn(1)(x)])  # Second highest value
                   
##' [rescale01] Function to rescale a variable from 0 to 1
rescale01 <- function(x) {
  (x - min(x)) / ((max(x)) - min(x))
}
                   
##' [clean.text] Function to clean Twitter data
clean.text <- function(x)
{
    x <- tolower(x)      # To lower case
    x <- gsub("rt", "", x)  # Remove 'rt'
    x <- gsub("@\\w+", "", x)  # Remove '@'
    x <- gsub("[[:punct:]]", "", x)  # Remove punctuation
    x <- gsub("[[:digit:]]", "", x)  # Remove numbers
    x <- gsub("http\\w+", "", x)  # Remove 'http' links
    x <- gsub("[ |\t]{2,}", "", x)  # Remove tabs
    x <- gsub("^ ", "", x)  # Remove leading blank spaces
    x <- gsub(" $", "", x)  # Remove ending blank spaces
    return(x)
}

##' [clear.history] Function to clear R history (removing sensitive information)
clear.history <- function() {  # Create a function to wipe your R history
    write("", file=".blank")
    loadhistory(".blank")
    unlink(".blank")
  }

clear.history()  # Remove sensitive data from your R History (CLEARS ALL OF YOUR HISTORY)
