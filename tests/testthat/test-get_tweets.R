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

    # # test specify n_tweets
    # n = 10
    # output_10_tweets <- get_tweets('@pytweetGod', n_tweets = n)
    # test_that('Output should work corresponding to input params', {
    #     expect_equal(nrow(output_10_tweets), n)
    # })
    #
    # # test specify include_replies
    # output_replies <- get_tweets('@pytweetGod', include_replies = TRUE)
    # test_that('Output should work corresponding to input params', {
    #     expect_true(nrow(output_replies) > nrow(output))
    # })
}

test_get_tweets()

