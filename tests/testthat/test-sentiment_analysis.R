#' #' Tests for sentiment_analysis
#' #' @param None.
#' #'
#' #' @return None.
#' #'
#' #' @examples
#' #' test-sentiment_analysis()
#'
# load the toy data set
data <- tweetr::brunomars_tweet
# run sentiment analysis on the toy dataset
output <- sentiment_analysis(data)

test_sentiment_analysis <- function() {

    # Test output format and column name. 
    test_that('Output should be a data frame with a "word" column (chr), a "sentiment" column (chr) and a "n" column (int)', {
        expect_true(is.data.frame(output))
        expect_equal(names(output), c('word', 'sentiment', 'n'))
        expect_equal(class(output$word), 'character')
        expect_equal(class(output$sentiment), 'character')
        expect_equal(class(output$n), 'integer')
    })
}

test_sentiment_analysis()