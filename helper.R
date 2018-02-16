
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
                   
##' [match.order] Function to test whether elements in two vectors match and/or are in the correct order
match.order <- function(x,y) {
    if (isTRUE(all.equal(x,y))) 
        print('Perfect match in same order')
    if (!isTRUE(all.equal(x,y)) && isTRUE(all.equal(sort(x),sort(y))))
        print('Perfect match in wrong order')
    if (!isTRUE(all.equal(x,y)) && !isTRUE(all.equal(sort(x),sort(y))))
        print('No match')
}
                   
##' [multiplot] Function to combine multiple plots
## Source -- http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, 
                      labs=list(), labpos=list(c(0.5,0.01), c(0.02,0.5))) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
        
        if(!length(labs) == 0){
            grid.text(labs[1], x=labpos[[1]][1], y=labpos[[1]][2], gp=gpar(fontsize=12))
            grid.text(labs[2], x=labpos[[2]][1], y=labpos[[2]][2], rot=90, gp=gpar(fontsize=12))
        }
    }
}

