#' Helper function to create helper data
#'
#' @return data.frame Return tweet data frame
#'
#' @export
#' @examples
#' test_plot_timeline()
helper_create_data <- function() {
    data <- read.csv("../../data/brunomars_data.csv")
}
tweet_data <- helper_create_data()

#' Tests the plot_timeline function
#' @param df data.frame Test data.
#'
#' @return None.
#'
#' @examples
#' test_plot_timeline()
test_plot_timeline <- function(data) {

    # Tests that the plot is correct
    test_that('Plot should use geom_line and map x to x-axis.', {
        timeline_plot <- plot_timeline(data, time)
        expect_true("GeomLine" %in% class(timeline_plot$layers[[1]]$geom))
        expect_true("hours"  == rlang::get_expr(timeline_plot$layers[[1]]$mapping$x))
    })
}
test_plot_timeline(tweet_data)