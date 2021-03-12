#' Tests the plot_hashtags function
#' @param df data.frame Test data.
#'
#' @return None.
#'
#' @examples
#' plot_hashtags()
test_plot_hashtags <- function(data) {

    # Test the input parameter is valid
    # TODO

    # Tests that the plot is correct
    test_that('Plot should use geom_bar and map x to x-axis.', {
        hashtag_plot <- plot_hashtags(data)
        hashtag_plot
        expect_true("GeomBar" %in% class(hashtag_plot$layers[[1]]$geom))
        expect_true("count"  == rlang::get_expr(hashtag_plot$mapping$x))
    })

}

test_plot_hashtags(tweet_data)
