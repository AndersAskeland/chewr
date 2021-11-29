#' Custom geom. Creates a scatter column plot. Can only be used as a geom
#' for a ggplot object.
#'
#' @return ggplot geom
#' @export
#'
#' @examples
#' ggplot(data) +
#'    geom_scatter_column()
geom_scatter_column <- function(scale = 1, color = "#2b8cbe") {

    print(scale)
    # Set jitter
    jitter <- ggplot2::geom_jitter(width = 0.15, colour = color, size = scale)

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

geom_paired <- function(paired_variable, scale = 1, color = "#2b8cbe") {

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
    # Set lines
    paired_lines <- ggplot2::geom_line(mapping = ggplot2::aes(group = eval(parse(text=paired_variable))),
                                       linetype = "dotted",
                                       color = "#333333")

    # Set points
    points <- ggplot2::geom_point(colour = color,
                                  size = scale)
    # Return
    return_vector <- c(mean_bar, error_bars, paired_lines, points)
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
#'    theme_chewr()
theme_chewr <- function(font="Helvetica", scale = 1) {

    # Set theme
    theme <- ggplot2::theme(
        # Set titles
        plot.title = ggplot2::element_text(family=font,
                                            size=24,
                                            face="bold",
                                            color="#222222"),
        plot.subtitle = ggplot2::element_text(family=font,
                                              size=15),
        # Set ticks
        axis.ticks = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_line(color="#222222"),
        axis.ticks.length.x = ggplot2::unit(0.2, "cm"),
        # Set axis stuff
        axis.title = ggplot2::element_text(family=font,
                                           size=15 * scale,
                                           color="#222222"),
        axis.title.y = ggplot2::element_text(margin=ggplot2::margin(r = 8)),
        axis.title.x = ggplot2::element_text(margin=ggplot2::margin(t = 5)),
        axis.text = ggplot2::element_text(family=font,
                                          size=13 * scale,
                                          color="#222222"),
        axis.text.x = ggplot2::element_text(margin=ggplot2::margin(t = 5),
                                            angle = 30,
                                            hjust = 1),
        axis.text.y = ggplot2::element_text(margin=ggplot2::margin(r = 5)),
        axis.line.y = ggplot2::element_line(color="#222222"),
        axis.line.x = ggplot2::element_line(color="#222222"),
        # Background
        panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        # New grid
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
        panel.grid.major.x = ggplot2::element_blank())

    # Return
    return(theme)
}

save_plot <- function(plot) {

}


stats_compare_means <- function(data, comparison) {

    #
    print(.data)
    ggpubr::compare_means(formula = {{comparison}} ~ group, data = .data)
}
