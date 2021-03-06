# tweetr

<!-- badges: start -->
<!-- badges: end -->

## Package Overview           
`pytweet` is a python package for text analysis and sentiment analysis on tweets. The package will allow you to extract tweets from Twitter, visualize user habit on tweet posting, and apply sentiment analysis to the data.        

## Features

- `get_tweets`:              
    - This function extracts tweets from a Twitter user given their handle (i.e. @elonmusk). 

- `plot_timeline`:             
    - This function creates an analysis of what time of day the tweets occurs and plots the counts of tweets and hours. 

- `plot_hashtags`:             
    - This function creates an analysis of the hashtags in tweets, and plots the hashtag analysis.

- `sentiment_analysis`:              
    - This function applies sentiment analysis to tweets. It associates tokens in tweets with positive or negative sentiments and calculates their corresponding frequencies.           

- `visualize_sentiment`:            
    -    This function takes in the output of sentiment_analysis function and creates a visualization of user's tweets with sentimental analysis.
    
## Related Packages

There are a few existing R packages that perform tweets text analysis and sentiment analysis available on CRAN, such as [twitteR](https://cran.r-project.org/web/packages/twitteR/), and [tidytext](https://cran.r-project.org/web/packages/tidytext/).

## Installation

You can install the released version of tweetr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tweetr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/tweetr")
```
## Example
