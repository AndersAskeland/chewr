# 1. Plotting devices ----------------------------------------------------------------

#' Custom ggplot2 geom that creates a scatter column plot. Good to use when
#' comparing groups.
#'
#' @return ggplot geom
#' @export
#'
#' @examples
#' ggplot(data) +
#'    geom_scatter_column()
geom_scatter_column <- function(mapping = NULL, jitter.params = list(), meanbar.params = list(), errorbar.params = list(), ...) {

    # Extract parameters
    params <- list(...)
    jitter.params <- modifyList(params, jitter.params)

    # Create jitter (Geom)
    jitter_function <- get("geom_jitter", asNamespace("ggplot2"))
    jitter <- do.call(jitter_function, modifyList(
        list(width = 0.15,
             mapping = mapping),
        jitter.params))

    # Create mean bar (Stat)
    mean_function <- get("stat_summary", asNamespace("ggplot2"))
    mean_bar <- do.call(mean_function, modifyList(
        list(mapping = ggplot2::aes(width = 0.1),
             fun = "mean",
             fun.min = "mean",
             fun.max= "mean",
             geom = "errorbar"),
        meanbar.params))

    # Create error bars (stat)
    error_function <- get("stat_summary", asNamespace("ggplot2"))
    error_bars <- do.call(error_function, modifyList(
        list(mapping = ggplot2::aes(width = 0.5),
            geom = "errorbar",
            fun.data = ggplot2::mean_sdl,
            fun.args = list(mult = 1),
            position = "dodge"),
        errorbar.params))

    # Return
    list(jitter, mean_bar, error_bars)
}


#' Custom ggplot2 geom that creates a Paried scatter column plot. Good to use
#' in paired analyses.
#'
#' @param paired_variable str | Column to group on
#' @param scale int | Size scale of coloumn
#' @param color str | Color code
#'
#' @return
#' @export
#'
#' @examples
#' paired_variable = "participant_id", color = theme$color, scale = theme$scale
geom_paired_column <- function(paired_variable, scale = 1, color = "#2b8cbe") {

    # Set mean bar
    mean_bar <- ggplot2::stat_summary(mapping = ggplot2::aes(width = 0.1),
                                      fun = "mean",
                                      fun.min = "mean",
                                      fun.max= "mean",
                                      fun.args = list(na.rm = T),
                                      geom = "errorbar")


    # Set error bars
    error_bars <- ggplot2::stat_summary(mapping = ggplot2::aes(width = 0.5),
                                        geom = "errorbar",
                                        fun.data = ggplot2::mean_sdl,
                                        fun.args = list(mult = 1, na.rm = T),
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

#' Custom theme for ggplot object.
#'
#' @param font str | Font type
#' @param scale int | How large text should be
#'
#' @return ggplot theme
#' @export
#'
#' @examples
#' ggplot(data) +
#'    geom_scatter_column() +
#'    theme_chewr()
theme_chewr <- function(...) {

    # Collect dynamic dots (...)
    dots <- rlang::list2(...)

    # set base size
    if("base_size" %in% names(dots)) {
        base_size <- dots$base_size
    } else {
        base_size <- 11
    }

    # Create new theme based on theme_minimal()
    theme_chewr <- ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
            # Text
            text = ggplot2::element_text(family = "Helvetica"),
            plot.title = ggplot2::element_text(face = "bold",
                                               color = "#222222"),
            # Axis
            axis.ticks = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_line(color = "#222222"),
            axis.ticks.length.x = ggplot2::unit(0.2, "cm"),
            axis.title = ggplot2::element_text(color = "#222222"),
            axis.text = ggplot2::element_text(color = "#222222"),
            axis.line.y = ggplot2::element_line(color="#222222"),
            axis.line.x = ggplot2::element_line(color="#222222"),
            # Background
            panel.background = ggplot2::element_rect(fill = "#F6F6F8", colour = NA),
            plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
            # Grid
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
            panel.grid.major.x = ggplot2::element_blank(),
        )

    # Return
    return(theme_chewr)
}

#' Sets axis text as angled or wrap.
#'
#' @param axis_text
#'
#' @return
#' @export
#'
#' @examples
#' label_text()
label_text <- function(axis_text = "angled") {
    # Set axis text format
    if(axis_text == "angled"){
        axis_text <- ggplot2::theme(axis.text.x =
                                        ggplot2::element_text(margin = ggplot2::margin(t = 5),
                                                              angle = 30,
                                                              hjust = 1))
    } else if(axis_text == "wrap") {
        axis_text <- ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))
    }

    # Return
    return(axis_text)

}

# 2. Automatic plotting ---------------------------------------------------------


#' Creates plot comparing different participant groups from the Multisite study.
#'
#' An alternative approach to using this automatic function is to manually
#' create a ggplot with the "geom_scatter_column()" function and "theme_chewr()".
#'
#' @param ... list | List of standard ggplot arguments
#' @param comparison str | Variable to be compared
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' autoplot_multisite_baseline(data = redcap_data,
#'             filter = "baseline",
#'             y_aes = "pdff_liver_cirle_mean",
#'             order = c("control", "obese", "intervention"),
#'             plot_title = "Liver fat",
#'             plot_subtitle = "Amount of fat in liver",
#'             plot_xlab = "Groups",
#'             plot_ylab = "% of liver fat")
autoplot_multisite_baseline <- function(comparison, ...) {

    # Extract labels from ... argument
    args <- list(...)
    labels <- extract_labs(args, ylab = comparison)

    # Extract theme
    theme <- extract_theme(args)

    # Read data and rename groups
    df <- redcap_read(columns = comparison, filter = "NAFLD")
    df <- rename_xlabs(df)

    # Plot data
    df %>% dplyr::filter(visit == "Baseline") %>%
        dplyr::group_by(group) %>%
        ggplot2::ggplot(ggplot2::aes(x = factor(group, levels=c("Lean control",
                                                                "Obese w/o NAFLD",
                                                                "Obese with NAFLD")),
                                     y = eval(parse(text=comparison)))) +
        geom_scatter_column(scale = theme$scale, color = theme$color) +
        ggplot2::labs(title = labels$title,
                      subtitle = labels$subtitle,
                      y = labels$ylab,
                      x = NULL) +
        theme_chewr(scale = theme$scale)
}


#' Creates plot comparing different participant groups from the Multisite study.
#' This function automatically compares weight loss
#'
#' @param comparison str | Variable to compare
#' @param ... Extra parameters
#' @param exclude bool | If one should exclude participants that does not have 3 measurments.
#'
#' @return
#' @export
#'
#' @examples
#' autoplot_multisite_weight_loss("bmi", df)
autoplot_multisite_weight_loss <- function(comparison, df, exclude = TRUE, ...) {

    # Extract labels from ... argument
    args <- list(...)
    labels <- extract_labs(args, ylab = comparison)

    # Extract theme
    theme <- extract_theme(args)

    # Read data if not included
        # TODO - Fix redcap_read function

    # Read data and rename groups
    df <- redcap_read(columns = comparison, filter = "NAFLD")
    df <- rename_xlabs(df)

    # Potentially exclude non-complete IDs
    if (exclude) {
        df <- stat_relative_change(df, eval(parse(text = comparison)))
    }

    # Plot
    plot <- df %>% dplyr::filter(group == "Obese with NAFLD") %>%
        dplyr::group_by(visit) %>%
        ggplot2::ggplot(ggplot2::aes(x = factor(visit, levels=c("Baseline",
                                                                "During weight loss",
                                                                "After weight loss")),
                                     y = eval(parse(text = comparison)))) +
        geom_paired_column(paired_variable = "participant_id", color = theme$color, scale = theme$scale) +
        ggplot2::labs(title = labels$title,
                      subtitle = labels$subtitle,
                      y = labels$ylab,
                      x = NULL) +
        theme_chewr(scale = theme$scale)

    # Return
    return(plot)
}


# 3. Animations -----------------------------------------------------------

#' Creates animation object. Can be saved by using ```gganimate::anim_save() ```.
#'
#' @param map str | World or DK
#' @param time int | Time in second the gif should run for
#'
#' @return gif item
#' @export
#'
#' @examples
#' gif <- anim_map_obesity(map = "world")
#'
#' # Save gif
#' gganimate::anim_save(filename = "world.gif", animation = gif, path = "~/")
anim_map_obesity <- function(map = "world", time = 20) {

    # Query WHO API
    df <- who_query(query = "obesity")

    # Create map
    if(map == "world") { # World map
        map_item <- ggplot2::map_data(map)

        plot <- ggplot2::ggplot(df) +
            # Map data
            ggplot2::geom_map(mapping = ggplot2::aes(map_id = region),
                              data = map_item,
                              map = map_item,
                              color="white",
                              size=0.1,
                              fill="grey") +
            # Obesity data
            ggplot2::geom_map(mapping = ggplot2::aes(fill = bmi_mean,
                                                     map_id = country),
                              map = map_item) +
            # Set display area
            ggplot2::expand_limits(x = map_item$long,
                                   y = map_item$lat) +
            # Set labels
            ggplot2::labs(subtitle = "Year: {frame_along}",
                          fill = "Percentage (%)") +
            # Theming
            ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                           panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
                           legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
                           axis.ticks = ggplot2::element_blank(),
                           axis.text = ggplot2::element_blank(),
                           axis.title = ggplot2::element_blank(),
                           panel.grid = ggplot2::element_blank(),
                           text = ggplot2::element_text(color = "#22211d"),
                           plot.subtitle = ggplot2::element_text(size=15, color = "#4e4d47", hjust = 0.5),
                           aspect.ratio = 1/2,
                           plot.margin = ggplot2::margin(0, -0.02, -0.04, -0.02, unit = "npc"),
                           panel.border = ggplot2::element_blank(),
                           legend.position = c(0.10, 0.4),
                           legend.title = ggplot2::element_blank()) +
            # Gradient
            ggplot2::scale_fill_gradient2(low = "#ffffcc",
                                          mid = "#ffeda0",
                                          high = "#800026",
                                          na.value = "grey",
                                          labels=scales::label_percent(scale = 1),
                                          limits=c(-5, 55),
                                          breaks = c(0, 10, 20, 30, 40, 50))
    } else { # Denmark map
        map_item <- ggplot2::map_data("world",
                                region = "Denmark")

        plot <- ggplot2::ggplot(df) +
            # Map data
            ggplot2::geom_map(mapping = ggplot2::aes(map_id = region),
                              data = map_item,
                              map = map_item,
                              color="white",
                              size=0.1,
                              fill="grey") +
            # Obesity data
            ggplot2::geom_map(mapping = ggplot2::aes(fill = bmi_mean,
                                                     map_id = country),
                              map = map_item) +
            # Set display area
            ggplot2::expand_limits(x = map_item$long,
                                   y = map_item$lat) +
            # Set labels
            ggplot2::labs(subtitle = "Year: {frame_along}",
                          fill = "Percentage (%)") +
            # Theming
            ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                           panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
                           legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
                           axis.ticks = ggplot2::element_blank(),
                           axis.text = ggplot2::element_blank(),
                           axis.title = ggplot2::element_blank(),
                           panel.grid = ggplot2::element_blank(),
                           text = ggplot2::element_text(color = "#22211d"),
                           plot.subtitle = ggplot2::element_text(hjust = 0.02),
                           aspect.ratio = 1/1,
                           plot.margin = ggplot2::margin(0, -0.02, -0.04, -0.02, unit = "npc"),
                           panel.border = ggplot2::element_blank(),
                           legend.position = c(0.80, 0.2),
                           legend.title = ggplot2::element_blank()) +
            # Gradient
            ggplot2::scale_fill_gradient2(low = "#ffffcc",
                                          mid = "#ffeda0",
                                          high = "#800026",
                                          na.value = "grey",
                                          labels=scales::label_percent(scale = 1),
                                          limits=c(-5, 55),
                                          breaks = c(0, 10, 20, 30, 40, 50))
    }


    # Animation
    animation <- plot +
        gganimate::transition_reveal(as.integer(year), keep_last = FALSE)

    # Gif settings
    frames <- length(unique(df$year)) - 1
    fps <- (length(unique(df$year)) / time)
    width <- dplyr::if_else(map == "world", 1600, 1000)
    height <- dplyr::if_else(map == "world", 900, 1000)

    # Animate gif
    gif <- gganimate::animate(animation,
                              bg = 'transparent',
                              nframes = frames,
                              fps = round(fps),
                              width = width,
                              height = height,
                              res = 200,
                              device = "png",
                              renderer = gganimate::magick_renderer(loop = FALSE))

    # Return
    gif
    }


#' Create plot
#'
#' @param country str | What country you want to plot or world
#' @param time int | How long
#' @param width int | Width resolution
#' @param height int | Height resolution
#'
#' @return
#' @export
#'
#' @examples
#' anim_plot_obesity()
anim_plot_obesity <- function(country = "world", time = 20, width = 1600, height = 1000) {

    # Query WHO API
    df <- who_query(query = "obesity")

        # Filter data
    if(country == "world") {
        df <- df %>%
            dplyr::group_by(year) %>%
            dplyr::summarise(bmi_mean = mean(bmi_mean, na.rm = T))

    } else if (country == "denmark" | country == "Denmark") {
        country_name <- Hmisc::capitalize(country)
        df <- df %>%
            dplyr::filter(country == country_name)
    } else {
        break
    }

    # Create graph
    plot <- ggplot2::ggplot(data = df, ggplot2::aes(x = year, y = bmi_mean)) +
        ggplot2::geom_line(color="red", size = 2) +
        ggplot2::labs(subtitle = "Year: {frame_along}",
                      y = "Percentage (%)") +
        theme_chewr(scale = 1.5) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
        ggplot2::scale_y_continuous(limits=c(0,45),
                                    breaks = c(5, 10, 20, 30, 40)) +
        ggplot2::scale_x_continuous(limits=c(1975,2017),
                                    breaks = c(1975, 1985, 1995, 2005, 2016))



    # Animation
    animation <- plot +
        gganimate::transition_reveal(as.integer(year))

    # Gif settings
    frames <- length(unique(df$year))
    fps <- (length(unique(df$year)) / time)

    # Animate gif
    gif <- gganimate::animate(animation,
                              bg = 'transparent',
                              nframes = frames,
                              fps = round(fps),
                              width = width,
                              height = height,
                              res = 200,
                              device = "png",
                              renderer = gganimate::magick_renderer(loop = FALSE))

    # Return
    gif
}

