#' Creates an automated plot comparing different participant groups from the
#' Multisite study. An alternative approch to using this automatic function
#' would be to manually create a ggplot with the "geom_scatter_column()" function
#' and "chewr_style()".
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
#' plot_groups(data = redcap_data,
#'             filter = "baseline",
#'             y_aes = "pdff_liver_cirle_mean",
#'             order = c("control", "obese", "intervention"),
#'             plot_title = "Liver fat",
#'             plot_subtitle = "Amount of fat in liver",
#'             plot_xlab = "Groups",
#'             plot_ylab = "% of liver fat")
plot_groups <- function(data, filter, y_aes, order=NULL, plot_title=NULL, plot_subtitle=NULL, plot_xlab=NULL, plot_ylab=NULL) {

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
        chewr_style()

}

#' Custom geom. Creates a scatter column plot. Can only be used as a geom
#' for a ggplot object.
#'
#' @return ggplot geom
#' @export
#'
#' @examples
#' ggplot(data) +
#'    geom_scatter_column()
geom_scatter_column <- function() {

    # Set jitter
    jitter <- ggplot2::geom_jitter(width = 0.2, colour = "#2b8cbe")

    # Set mean bar
    mean_bar <- ggplot2::stat_summary(mapping = ggplot2::aes(width = 0.1),
                             fun = "mean",
                             fun.min = "mean",
                             fun.max= "mean",
                             geom = "errorbar")

    # Set error bars
    error_bars <- ggplot2::stat_summary(mapping = ggplot2::aes(width = 0.5),
                                        geom = "errorbar",
                                        fun.data = ggplot2::mean_sdl,
                                        fun.args = list(mult = 1),
                                        position = "dodge")

    # Return
    return_vector <- c(jitter, mean_bar, error_bars)
    return(return_vector)
}

#' Changes plot theme to chewr (multisite) style. Used in combination
#' with ggplot.
#'
#' @param font string | Font type
#'
#' @return ggplot theme
#' @export
#'
#' @examples
#' ggplot(data) +
#'    geom_scatter_column() +
#'    chewr_style()

chewr_style <- function(font="Helvetica") {

    # Set theme
    theme <- ggplot2::theme(
        plot.title = ggplot2::element_text(family=font,
                                            size=20,
                                            face="bold",
                                            color="#222222"),
        plot.subtitle = ggplot2::element_text(family=font,
                                              size=12),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(family=font,
                                           size=15,
                                           color="#222222"),
        axis.text = ggplot2::element_text(family=font,
                                          size=10,
                                          color="#222222"),
        axis.text.x = ggplot2::element_text(margin=ggplot2::margin(t = 5)),
        axis.text.y = ggplot2::element_text(margin=ggplot2::margin(r = 10)),
        # Background
        panel.background = ggplot2::element_blank(),
        # New grid
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
        panel.grid.major.x = ggplot2::element_blank())

    # Return
    return(theme)
}
