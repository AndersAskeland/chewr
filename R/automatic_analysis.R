#' Creates an automated plot comparing different participant groups from the
#' Multisite study. An alternative approch to using this automatic function
#' would be to manually create a ggplot with the "geom_scatter_column()" function
#' and "theme_chewr()".
#'
#' @param data tibble | Redcap, labka, or a combination of both.
#' @param filter string | What parameter do you want to filter on. I.e. baseline.
#' @param y_aes string | What y value to look at
#' @param order vector | The x-axis order
#' @param title string | Main title
#' @param subtitle string | Sub title
#' @param xlab string | Label on x-axis
#' @param ylab string | Label on y-axis
#'
#' @return plot
#' @export
#'
#' @examples
#' plot_compare_groups(data = redcap_data,
#'             filter = "baseline",
#'             y_aes = "pdff_liver_cirle_mean",
#'             order = c("control", "obese", "intervention"),
#'             plot_title = "Liver fat",
#'             plot_subtitle = "Amount of fat in liver",
#'             plot_xlab = "Groups",
#'             plot_ylab = "% of liver fat")
plot_compare_groups <- function(data, filter, y_aes, order=NULL, plot_title=NULL, plot_subtitle=NULL, plot_xlab=NULL, plot_ylab=NULL) {

    # TODO: Make X axis name capital first.

    # Make plot
    data %>% dplyr::filter(visit == filter) %>%
        dplyr::group_by(group) %>%
        ggplot2::ggplot(ggplot2::aes(x = factor(group, levels=order),
                                     y = eval(parse(text=y_aes)))) +
        geom_scatter_column() +
        ggplot2::labs(title = plot_title,
                      subtitle = plot_subtitle,
                      y = plot_ylab,
                      x = plot_xlab) +
        theme_chewr()

}
