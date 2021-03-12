#' Helper function to create helper data
#'
#' @return data.frame Return tweet data frame
#'
#' @export
#' @examples
#' helper_create_data()
helper_create_data <- function() {
    # Using read.csv seems not working on package check(),
    # Might consider using return from get_tweets()
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
    # Test the input parameter
    test_that(
        "Corresponding error message should be expected if the `df` argument is not a dataframe", {
            expect_error(plot_timeline("tweet_data"),
                         regexp = "The argument 'df' should be a dataframe.")
            expect_error(plot_timeline(123),
                         regexp = "The argument 'df' should be a dataframe.")
            expect_error(plot_timeline(c(1, 2, 3)),
                         regexp = "The argument 'df' should be a dataframe.")
        })
    test_that(
        "Corresponding error message should be expected if the `time_col` argument is not a column name", {
            expect_error(plot_timeline(data, date_time),
                         regexp = "object 'date_time' not found")
        })


    # Tests that the plot is correct
    test_that('Plot should use geom_line and map x to x-axis.', {
        timeline_plot <- plot_timeline(data, time)
        expect_true("GeomLine" %in% class(timeline_plot$layers[[1]]$geom))
        expect_true("hours"  == rlang::get_expr(timeline_plot$layers[[1]]$mapping$x))
    })
}
test_plot_timeline(tweet_data)
