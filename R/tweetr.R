library(twitteR)
library(dplyr)
library(purrr)
library(tidyverse)

#' Get Tweets
#'
#' Create a data.frame of a user's tweets given the username/handle.
#' The output data.frame will contain two columns: sent time and tweet.
#'
#' REQUIRES: Twitter API credentials need to be stored as environment
#' variables as this function calls directly to the bash_profile.
#'
#' @param handle character, the username to query
#' @param n_tweets integer, the number of tweets to retrieve. Must be positive or -1 (by default, all)
#' @param include_replies logical, whether or not to include replies
#' @param verbose logical, whether or not to print progress during the fetch
#'
#' @return data.frame
#' @export
#'
#' @examples
#' get_tweets('@BrunoMars', n_tweets=100)
get_tweets <- function(handle, n_tweets = -1, include_replies = FALSE, verbose = TRUE) {

    # OAuth connection to Twitter API
    setup_twitter_oauth(Sys.getenv('TWITTER_CONS_KEY'),  # consumer key
                        Sys.getenv('TWITTER_CONS_SEC'),  # consumer secret
                        Sys.getenv('TWITTER_ACCS_KEY'),  # access key
                        Sys.getenv('TWITTER_ACCS_SEC'))  # access secret

    n_max <- 3200  # max batch size
    if (n_tweets < n_max & n_tweets > 0) {
        latest <- userTimeline(handle, n = n_tweets + 1, includeRts = TRUE, excludeReplies = !include_replies)
    } else {
        latest <- userTimeline(handle, n = n_max, includeRts = TRUE, excludeReplies = !include_replies)
    }

    result <- tibble(map_df(latest, as.data.frame))
    oldestID <- min(result$id)

    # recursively retrieve tweets
    while (length(latest) > 0 & nrow(result) < n_tweets) {
        if (n_tweets - nrow(result) < n_max) {
            n_max <- n_tweets - nrow(result)  # tweets to retrieve in final batch
        }

        latest <- userTimeline(handle, n = n_max, includeRts = TRUE, excludeReplies = !include_replies, maxID = oldestID)
        result <- rbind(result, tibble(map_df(latest, as.data.frame)))  # append results
        oldestID <- result$id[nrow(result)]  # oldest tweet

        if (verbose) print(paste(nrow(result), "tweets downloaded..."))
    }

    # format output
    output <- result[1:n_tweets,] %>%
        mutate(time = created, tweet = text) %>%
        select(time, tweet) %>%
        drop_na()
    return(output)
}

#' Plot timeline analysis
#'
#' Analysis what time of day the tweets occurs and plot the
#' counts of tweets versus hours.
#'
#' @param df data.frame
#' @param time_col A column name in data.frame
#'
#' @return A chart plotting the counts of tweets versus hours.
#'
#' @examples (tweet_data, time)
#'
plot_timeline <- function(df, time_col){

}

#' Plot hashtag word analysis
#'
#' A chart plotting analysis result of the most commonly used
#' words in tweets.
#'
#' @param df data.frame
#' @param text_col A column name in data.frame
#'
#' @return A chart plotting analysis the most commonly used words.
#'
#' @examples (tweet_data, time)
#'
plot_hashtags <- function(df, text_col){

}

#' sentiment_analysis
#'
#' This function takes a tweet dataframe as input. The input dataframe should contain a column named 'tweet' that contains tweet text information.
#' The function analyzes the sentiment of each tweet and categorize them into 'positive', 'negative' and 'netrual'. The sentiment information will be added as a new
#' column to the input dataframe and saved as a new dataframe (output).
#'
#' @param tweet data.frame
#'
#' @return tweet_senti data.frame
#'
#' @examples
#' sentiment_analysis(tweet_username_123)
sentiment_analysis <- function(tweet) {

}

#' Visualize Sentiment Analysis
#'
#' Takes in the output of sentiment_analysis and creates
#' a visualization of user's tweets with sentimental analysis.
#'
#' @param sentiment_df A DataFrame containing sentiment column from sentiment_analysis
#' @param plot_type An optional string for which chart to return. Options are ("Standard", "Stacked", "Separate")
#'
#' @return A bar chart containing most common words in tweets, colour coded by sentiment.
#' @export
#'
#' @examples visualize_sentiments(sentiment_df, plot_type = "Stacked")
visualize_sentiments <- function(sentiment_df, plot_type = "Standard") {

}
