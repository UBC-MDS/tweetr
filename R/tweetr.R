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
