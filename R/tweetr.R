library(twitteR)
library(tidyverse)
library(dplyr)
library(tidytext)
library(cowplot)


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
    if (!is.data.frame(df)) {
        stop("The argument 'df' should be a dataframe.")
    }

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
    if (!is.data.frame(df)) {
        stop("The argument 'df' should be a dataframe.")
    }

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
    if(nrow(hashtag_data) > 15){
        hashtag_data <- hashtag_data[order(-hashtag_data$count),][1:15,]
    }

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
#' The function cleans the text inn 'tweet' column by removing http component, punctuation, end words, reduce the letters to lowercase and word stemming. 
#' Then the function matches each word to the sentiment 'positive' or 'negative' using the tidytext 'Bing' lexicons. And output is a dataframe that contains
#' words used in the tweet texts and assign each word with either 'positive' or 'negative' sentiment plus sorting by the numbers of appearence of that word.
#'
#' @param tweet data.frame
#'
#' @return tweet_result data.frame
#'
#' @examples
#' sentiment_analysis(tweet_username_123)
sentiment_analysis <- function(tweet){
  tweet$clean_text <- gsub("http[[:alnum:][:punct:]]*", "", tweet$tweet) 
  sentiment_result <- tweet %>%
  select(clean_text) %>%
  unnest_tokens(word, clean_text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
  
  return(sentiment_result)
}

#' Visualize Sentiment Analysis
#'
#' Takes in the output of sentiment_analysis and creates
#' a visualization of user's tweets with sentimental analysis.
#'
#' @param sentiment_df A DataFrame containing sentiment column from sentiment_analysis
#' @param plot_type An optional string for which chart to return. Options are ("Standard", "Separate")
#'
#' @return A bar chart containing most common words in tweets, colour coded by sentiment.
#' @export
#'
#' @examples visualize_sentiments(sentiment_df, plot_type = "Separate")
visualize_sentiments <- function(sentiment_df, plot_type = "Standard") {
  
  #Checks
  
  if (!is.data.frame(sentiment_df)){
    stop("The input sentiment_df should be a dataframe, did you use output from sentiment_analysis?")
  }
  
  options <- c("Standard", "Separate")

  if (!(plot_type %in% options)){
   stop("Argument plot_type is invalid: it must be one of 'Standard' or 'Separate'")
  }
  
  if (!("sentiment" %in% colnames(sentiment_df))){
  stop("column name sentiment is not in sentiment_df, did you use output from sentiment_analysis?")
  }
  
  
  #Get top results
  top_positive <- sentiment_df %>% 
                  filter(sentiment == 'positive') %>% 
                  slice(1:10)

  top_negative <- sentiment_df %>% 
                  filter(sentiment == 'negative') %>% 
                  slice(1:10)
                  
                  
  #used for plotting
  colors <- c("positive" = "blue", "negative" = "red")

  if (plot_type == 'Standard'){
  
  #bind top results
  top_senti <- rbind(top_positive, top_negative)
  
  
  #plot resutlt
  plot <- ggplot(top_senti, aes(x = n, y = reorder(word,n), fill = sentiment))+
    geom_bar(stat = "identity")+
    xlab('Number of Occurences') +
    ylab("Word")+
    ggtitle("Most Common Words by Sentiment")  +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10, angle = 0),
          axis.text.y = element_text(size = 12, angle = 0),
          axis.title = element_text(size = 14))+
    labs(fill = "Sentiment")+
    scale_fill_manual(values = colors)
  
  }
  
  
  if (plot_type == 'Separate'){
  
  positive_plot <- ggplot(top_positive, aes(x = n, y = reorder(word,n), fill = sentiment))+
  geom_bar(stat = "identity")+
  xlab('Number of Occurences') +
  ylab("Word")+
  ggtitle("Most Common Positive Words")  +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 10, angle = 0),
        axis.text.y = element_text(size = 12, angle = 0),
        axis.title = element_text(size = 14),
        legend.position = 'none')+
  scale_fill_manual(values = colors)
  
  negative_plot <- ggplot(top_negative, aes(x = n, y = reorder(word,n), fill = sentiment))+
  geom_bar(stat = "identity")+
  xlab('Number of Occurences') +
  ylab("Word")+
  ggtitle("Most Common Negative Words")  +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 10, angle = 0),
        axis.text.y = element_text(size = 12, angle = 0),
        axis.title = element_text(size = 14),
        legend.position = 'none')+
  scale_fill_manual(values = colors) 
  
  plot <- plot_grid(positive_plot, negative_plot)
  }
  
return(plot)
}
