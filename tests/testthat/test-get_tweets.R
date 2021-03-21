#' #' Tests for get_tweets
#' #' @param None.
#' #'
#' #' @return None.
#' #'
#' #' @examples
#' #' test_get_tweets()
#'
test_get_tweets <- function() {

    # Test output format
    output <- get_tweets('@pytweetGod')  # test Twitter bot
    test_that('Output should be a data frame with a "time" column (POSIXct) and a "tweet" column (char)', {
        expect_true(is.data.frame(output))
        expect_equal(names(output), c('time', 'tweet'))
        expect_equal(class(output$time)[1], 'POSIXct')
        expect_equal(class(output$tweet), 'character')
    })

    # Test getting all tweets
    test_that('Output should work corresponding to input params', {
        expect_equal(nrow(output), 15)
    })

    # test specify n_tweets
    n = 600
    output_10_tweets <- get_tweets('@BrunoMars', n_tweets = n)
    test_that('Output should work corresponding to input params', {
        expect_equal(nrow(output_10_tweets), n)
    })

    # test specify include_replies
    output_replies <- get_tweets('@pytweetGod', include_replies = TRUE)
    test_that('Output should work corresponding to input params', {
        expect_true(nrow(output_replies) > nrow(output))
    })
}

test_get_tweets()

#' #' Tests for get_tweets errors
#' #' @param None.
#' #'
#' #' @return None.
#' #'
#' #' @examples
#' #' test_get_tweets_error()
#'
test_get_tweets_error <- function() {

    # Test error messages
    test_that('Expect customized error messages for invalid input types', {
        expect_error(get_tweets(123), "The argument 'handle' should be a string.")
        expect_error(get_tweets('pytweetGod', n_tweets = 1.5),
                     "The argument n_tweets is invalid! Must be an integer greater than 0 or -1.")
        expect_error(get_tweets('pytweetGod', n_tweets = -5))
        expect_error(get_tweets('pytweetGod', include_replies = 'haha'),
                     "The argument 'include_replies' is invalid!")
        expect_error(get_tweets('pytweetGod', verbose = 'nice'),
                     "The argument 'verbose' is invalid!")
    })
}

test_get_tweets_error()

