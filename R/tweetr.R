library(twitteR)
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

    # get first batch
    latest <- userTimeline(handle, n = 200, includeRts = TRUE, excludeReplies = !include_replies)
    result <- twListToDF(latest)
    # result <- tibble(purrr::map_df(latest, as.data.frame))
    oldestID <- min(result$id)  # oldest tweet retrieved so far

    # recursively retrieve tweets
    n_max <- 3200  # max batch size
    while (length(latest) > 0 & nrow(result) < n_tweets) {
        if (n_tweets - nrow(result) < n_max) {
            n_max <- n_tweets - nrow(result)  # tweets to retrieve in final batch
        }

        latest <- userTimeline(handle, n = n_max, includeRts = TRUE, excludeReplies = !include_replies, maxID = oldestID)
        result <- rbind(result, twListToDF(latest))  # append results
        # result <- rbind(result, tibble(map_df(latest, as.data.frame)))
        oldestID <- result$id[nrow(result)]  # oldest tweet

        if (verbose) print(paste(nrow(result), "tweets downloaded..."))
    }

    # format output
    if (n_tweets != -1) result <- result[1:n_tweets,]
    output <- tibble(result) %>%
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
#' @export
#' @examples
#' plot_timeline(tweet_data, time)
#'
plot_timeline <- function(df, time_col){

    #extract hour from time column
    tweet <- df %>%
        mutate(hours=lubridate::hour(strptime({{ time_col }}, '%m/%d/%Y %H:%M')))

    timeline_plot <- ggplot(data=tweet) +
        geom_line(aes(x=hours), stat = "count") +
        xlab("Hour of day") +
        ylab("Counts of Tweets") +
        ggtitle("Tweet Timeline Analysis") +
        theme(text = element_text(size=15)) +
        theme_bw()
}

#' Plot hashtag word analysis
#'
#' A chart plotting analysis result of the most commonly used
#' words in tweets.
#'
#' @param df data.frame
#'
#' @return A chart plotting analysis the most commonly used words.
#' @export
#' @examples
#' plot_hashtags(tweet_data)

plot_hashtags <- function(df){

    # extract hashtag words as a list
    hashtags = str_extract_all(df$tweet, "[#][a-zA-Z0-9]+")

    # initial a dataframe to store hashtags
    hashtag_data <- data.frame(hashtagwords = '')
    for(words in hashtags){
        for(word in words){
            hashtag_data <- rbind(hashtag_data, word)
        }
    }
    # count hashtag words and get the top 15 frequent word
    hashtag_data <- hashtag_data  %>% group_by(hashtagwords) %>% summarize(count=n())
    hashtag_data <- hashtag_data[order(-hashtag_data$count),][1:15,]
    hashtag_data

    # Plot hashtag words
    hashtag_plot <- ggplot(data=hashtag_data, aes(x = count, y = reorder(hashtagwords,count))) +
        geom_bar(stat="identity") +
        ggtitle("Top 15 Hashtag Words") +
        xlab("Hashtags") +
        ylab("Counts of Hashtags") +
        theme(text = element_text(size=15)) +
        theme_bw()

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
