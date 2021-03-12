#' Tests the plot_hashtags function
#' @param df data.frame Test data.
#'
#' @return None.
#'
#' @examples
#' plot_hashtags()
test_plot_hashtags <- function(data) {

    # Test the input parameter
    test_that(
        "Corresponding error message should be expected if the df argument is not a dataframe", {
            expect_error(plot_hashtags("tweet_data"),
                         regexp = "The argument 'df' should be a dataframe.")
            expect_error(plot_hashtags(123),
                         regexp = "The argument 'df' should be a dataframe.")
            expect_error(plot_hashtags(c(1, 2, 3)),
                         regexp = "The argument 'df' should be a dataframe.")
    })

    # Tests if the output should be a ggplot object.
    test_that("The returned plot should be a ggplot object.", {
        expect_true(ggplot2::is.ggplot(plot_hashtags(tweet_data)))
    })

    # Tests that the plot is correct
    test_that('Plot should use geom_bar and map x to x-axis.', {
        hashtag_plot <- plot_hashtags(data)
        hashtag_plot
        expect_true("GeomBar" %in% class(hashtag_plot$layers[[1]]$geom))
        expect_true("count"  == rlang::get_expr(hashtag_plot$mapping$x))
    })

    # Test the plot when the input data has less than 15 rows
    test_that('Plot should work when there is less than 15 data', {
        small_data <- data.frame(time  = c("3/6/2021  4:39:00",
                                           "3/5/2021  8:41:00"),
                                 tweet = c("#SilkSonic song and video out #LeaveTheDoorOpen",
                                           "Just posted"))
        hashtag_plot <- plot_hashtags(small_data)
        expect_true("GeomBar" %in% class(hashtag_plot$layers[[1]]$geom))
        expect_true("count"  == rlang::get_expr(hashtag_plot$mapping$x))
    })

}

test_plot_hashtags(tweet_data)
