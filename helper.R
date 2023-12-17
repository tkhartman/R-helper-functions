## [sjPlot] View codebook (variable and value labels)
require(sjPlot)
sjPlot::view_df(df)  # Print codeobook, where 'df' is the dataframe name

## Import password protected SPSS files
## Use 'readspss' package to import password protected files
require(readspss)
df <- readspss::read.sav('COVID W1_W2_W3_W4_W5_W6 Cleaned 5364.sav', 
                              pass = 'ENTER PASSWORD HERE' )

## [maxn] Find nth highest column position for each row
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]  # function
df$first <- apply(df, 1, function(x)x[maxn(1)(x)])  # Maximum value
df$second <- apply(df, 1, function(x)x[maxn(2)(x)])  # Second highest value
                   
## [rescale01] Function to rescale a variable from 0 to 1
## Can pass na.rm = TRUE argument
rescale01 <- function(x, ...) {
  (x - min(x, ...)) / ((max(x, ...)) - min(x, ...))
}
                   
## [clean.text] Function to clean Twitter data
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

## [clear.history] Function to clear R history (removing sensitive information)
clear.history <- function() {  # Create a function to wipe your R history
    write("", file=".blank")
    loadhistory(".blank")
    unlink(".blank")
  }

clear.history()  # Remove sensitive data from your R History (CLEARS ALL OF YOUR HISTORY)
                   
## [match.order] Function to test whether elements in two vectors match and/or are in the correct order
match.order <- function(x,y) {
    if (isTRUE(all.equal(x,y))) 
        print('Perfect match in same order')
    if (!isTRUE(all.equal(x,y)) && isTRUE(all.equal(sort(x),sort(y))))
        print('Perfect match in wrong order')
    if (!isTRUE(all.equal(x,y)) && !isTRUE(all.equal(sort(x),sort(y))))
        print('No match')
}

                   
## [multiplot] Function to combine multiple plots
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

## Example - Making multiple plots
multiplot(h1, h2, h3, cols = 3, 
          labs = list("Distribution of 2017 Predicted Seat Marginality", ""))
   
                   
## [boot.se] Bootstrapped standard errors with 95% CI and 10,000 replications 
boot.se <- function(formula, data, indices) {
    d <- data[indices, ]  # Select sample 
    fit <- lm(formula, data = d)
    return(coef(fit)) 
} 

## Example - Bootstrapping with 10,000 replications 
require(boot)
boot.m1 <- boot(data = df, statistic = boot.se, R = 10000, 
                formula = Turnout17 ~ Turnout15 + vote.2015.margin + 
                    yg.margin + ac.margin)
boot.m1
boot.ci(boot.m1, type = "bca", index = 1)  # Intercept 95% CI
boot.ci(boot.m1, type = "bca", index = 2)  # B1 95% CI
boot.ci(boot.m1, type = "bca", index = 3)  # B2 95% CI
boot.ci(boot.m1, type = "bca", index = 4)  # B3 95% CI
boot.ci(boot.m1, type = "bca", index = 5)  # B4 95% CI                   
                   

## [odds.ratio] Calculate odds ratios following (ordered) logistic regression
odds.ratio <- function(model) {
    ctable <- coef(summary(model)) 
    pvalue <- pnorm(abs(ctable[ , "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = pvalue)
    ci <- confint(model)
    odds <- exp(cbind(OR = coef(model), ci))
    out <- list("Estimates" = ctable, "Odds Ratios" = odds)
    return(out)
}

## Example
m1 <- polr(formula = y ~ x, data = df, Hess = T)  # Ordered logit
summary(m1)
odds.ratio(m1)
                   

## [vcovCL] Calculate cluster-robust standard errors following 'polr'
vcovCL <- function(object, cluster = NULL, adjust = NULL) {
    stopifnot(require("sandwich"))
    ## Cluster specification
    if(is.null(cluster)) cluster <- attr(object, "cluster")
    if(is.null(cluster)) stop("no 'cluster' specification found")
    cluster <- factor(cluster)
    ## Estimating functions and dimensions
    ef <- estfun(object)
    n <- NROW(ef)
    k <- NCOL(ef)
    if(n != length(cluster))
        stop("length of 'cluster' does not match number of observations")
    m <- length(levels(cluster))
    ## Aggregate estimating functions by cluster and compute meat
    ef <- sapply(levels(cluster), function(i) colSums(ef[cluster == i, , drop = FALSE]))
    ef <- if(NCOL(ef) > 1L) t(ef) else matrix(ef, ncol = 1L)
    mt <- crossprod(ef)/n
    ## Bread
    br <- try(bread(object), silent = TRUE)
    if(inherits(br, "try-error")) br <- vcov(object) * n
    ## Put together sandwich
    vc <- 1/n * (br %*% mt %*% br)
    ## Adjustment
    if(is.null(adjust)) adjust <- class(object)[1L] == "lm"
    adj <- if(adjust) m/(m - 1L) * (n - 1L)/(n - k) else m/(m - 1L)
    ## Return
    return(adj * vc)
}
                 
## Use 'jtools' package to plot regression results (coefficient plots)
summ(regression.model)
plot_summs(regression.model, inner_ci_level = .9)

                 
## WEB SCRAPING EXAMPLE -- 'automagically' log into website to scrape data
## Load packages using 'pacman' package manager
pacman::p_load(haven, httr, RCurl, rvest)

## Log into the British Election Study website for downloading data
login.bes <- "https://www.britishelectionstudy.com/wp-login.php"
session <- html_session(login.bes)
form <- html_form(session)[[1]]
filled_form <- set_values(form,
                          log = 'XXXXXXXXXXXX',     # Replace with Username
                          pwd = 'XXXXXXXXXXXX')     # Replace with Password
## Save main page url
main_page <- submit_form(session, filled_form)

## URLS to BES data files; store in vector
url.w4 <- "https://www.britishelectionstudy.com/wp-content/uploads/2020/02/BES2015_W4_v4.0.sav"
url.w5 <- "https://www.britishelectionstudy.com/wp-content/uploads/2018/09/BES2015_W5_v3.9.sav"
url.w11 <- "https://www.britishelectionstudy.com/wp-content/uploads/2020/02/BES2015_W11_v1.6.sav"
url.w12 <- "https://www.britishelectionstudy.com/wp-content/uploads/2020/02/BES2015_W12_v1.6.sav"
url.w17 <- "https://www.britishelectionstudy.com/wp-content/uploads/2020/02/BES2019_W17_v0.1.sav"
url.w18 <- "https://www.britishelectionstudy.com/wp-content/uploads/2020/02/BES2019_W18_v0.1.sav"
url.results <- "https://www.britishelectionstudy.com/wp-content/uploads/2020/02/BES-2019-General-Election-results-file-v1.0.sav"

links <- c(url.w4, url.w5, url.w11, url.w12, url.w17, url.w18, url.results)

## Extract filenames and store in vector
files <- c(basename(url.w4), basename(url.w5),     # 2015 GE
           basename(url.w11), basename(url.w12),   # 2017 GE
           basename(url.w17), basename(url.w18),   # 2019 GE
           basename(url.results) )                 # Constituency results    

## Loop to download data file(s) if not in working directory
for (i in 1:length(files) ) {
if (!file.exists(files[i]) )
  { download <- jump_to(main_page, links[i])
  writeBin(download[['response']][['content']], files[i])
  }
}
