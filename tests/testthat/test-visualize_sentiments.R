
data <- get_tweets('@pytweetGod')
sentiment <- sentiment_analysis(data)

#' Tests for visualize_sentiments
#'
#' @param sentiment - output of sentiment_analysis 
#'
#' @return none
#' @export
#'
#' @examples test_visualize_sentiments(sentiment)
test_visualize_sentiments <- function(sentiment){

  test_that(
          "Error message should be expected if the sentiment_df argument is not a dataframe", {
              expect_error(visualize_sentiments("sentiment"),
                           regexp = "The input sentiment_df should be a dataframe, did you use output from sentiment_analysis?")
            })
  
  
  test_that(
          "Error message should be expected if plot_type argument misspelled or misused", {
              expect_error(visualize_sentiments(sentiment, plot_type = "a good plot"),
                           regexp = "Argument plot_type is invalid: it must be one of 'Standard' or 'Separate'")
            })
  
  test_that(
          "Error message should be expected if 'sentiment' is not a column of the dataframe", {
              expect_error(visualize_sentiments(data),
                           regexp = "column name sentiment is not in sentiment_df, did you use output from sentiment_analysis?")
            })
  
  
  test_that("The returned plot should be a ggplot object and match labels.", {
       plot <- visualize_sentiments(sentiment)
       expect_true(ggplot2::is.ggplot(plot))
       expect_true("Sentiment" == plot$labels$fill)
       expect_true("Word" == plot$labels$y)
       expect_true("Number of Occurences" == plot$labels$x)
  })
}

test_visualize_sentiments(sentiment)