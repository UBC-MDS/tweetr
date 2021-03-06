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