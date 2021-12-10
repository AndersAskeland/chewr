
# 1. Plotting ----------------------------------------------------------------


#' Custom ggplot2 geom that creates a scatter column plot. Good to use when
#' comparing groups.
#'
#' @return ggplot geom
#' @export
#'
#' @examples
#' ggplot(data) +
#'    geom_scatter_column()
geom_scatter_column <- function(scale = 1, color = "#2b8cbe") {

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


#' Custom ggplot2 geom that creates a Paried scatter column plot. Good to use
#' in paired analyses.
#'
#' @param paired_variable
#' @param scale
#' @param color
#'
#' @return
#' @export
#'
#' @examples
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
#'    theme_chewr(scale = 2)
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
        panel.background = ggplot2::element_rect(fill = "#F6F6F8", colour = NA),
        plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        # New grid
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
        panel.grid.major.x = ggplot2::element_blank())

    # Return
    return(theme)
}

#' Saves plot
#'
#' @param plot
#'
#' @return
#' @export
#'
#' @examples
save_chewr <- function(plot) {

}


# Automatic plots ---------------------------------------------------------


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
#' plot_compare_baseline(data = redcap_data,
#'             filter = "baseline",
#'             y_aes = "pdff_liver_cirle_mean",
#'             order = c("control", "obese", "intervention"),
#'             plot_title = "Liver fat",
#'             plot_subtitle = "Amount of fat in liver",
#'             plot_xlab = "Groups",
#'             plot_ylab = "% of liver fat")
plot_multisite_baseline <- function(comparison, ...) {

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
#' @param comparison
#' @param ...
#' @param exclude bool | If one should exclude participants that does not have 3 measurments.
#'
#' @return
#' @export
#'
#' @examples
plot_multisite_weight_loss <- function(comparison, df, exclude = TRUE, ...) {

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
        df <- stat_difference(df, eval(parse(text = comparison)))
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
#'
#' @return
#' @export
#'
#' @examples
anim_plot_world_obesity <- function(map = "world", width = 1600, height = 800) {

    # Read data from WHO API
    request <- httr::GET("https://ghoapi.azureedge.net/api/NCD_BMI_30A")
    json_raw <- httr::content(x = request, as = "text")
    json_parsed <- jsonlite::fromJSON(json_raw)
    df <- dplyr::as_tibble(json_parsed[[2]])

    # Import country dim data (WHO data)
    request <- httr::GET("https://ghoapi.azureedge.net/api/DIMENSION/COUNTRY/DimensionValues")
    json_raw <- httr::content(x = request, as = "text")
    json_parsed <- jsonlite::fromJSON(json_raw)
    df_country_dims <- dplyr::as_tibble(json_parsed[[2]]) %>%
        dplyr::select(Code, Title) %>%
        dplyr::rename(country_code = Code, country = Title)

    # Clean and combine data
    df_clean <- df %>%
        dplyr::rename(id = Id,
                      year = TimeDim,
                      sex = Dim1,
                      country_code = SpatialDim,
                      bmi_mean = NumericValue,
                      bmi_low = Low,
                      bmi_high = High) %>%
        dplyr::select(id, year, sex, country_code, bmi_mean, bmi_low, bmi_high) %>%
        dplyr::mutate(sex = dplyr::case_when(
            sex == "FMLE" ~ "Female",
            sex == "MLE" ~ "Male",
            sex == "BTSX" ~ "Both sexes")) %>%
        dplyr::left_join(y = df_country_dims) %>%
        dplyr::mutate(country = dplyr::case_when(
            country == "United States of America" ~ "USA",
            country == "Russian Federation" ~ "Russia",
            country == "United Kingdom of Great Britain and Northern Ireland" ~ "UK",
            country == "United Republic of Tanzania" ~ "Tanzania",
            country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
            country == "Viet Nam" ~ "Vietnam",
            country == "Yemen Arab Republic (until 1990)" ~ "Yemen",
            country == "Syrian Arab Republic" ~ "Syria",
            country == "Sudan (until 2011)" ~ "Sudan",
            country == "Republic of Korea" ~ "North Korea",
            country == "Lao People's Democratic Republic" ~ "Laos",
            country == "Kiribati (until 1984)" ~ "Kiribati",
            country == "Democratic Republic of the Congo" ~ "Congo",
            country == "Democratic People's Republic of Korea" ~ "South Korea",
            country == "Bolivia (Plurinational State of)" ~ "Bolivia",
            country == "Iran (Islamic Republic of)" ~ "Iran",
            country == "Sudan (former)" ~ "Sudan",
            country == "Germany, Federal Republic (former)" ~ "Germany",
            country == "Congo" ~ "Democratic Republic of the Congo",
            country == "Côte d'Ivoire" ~ "Ivory Coast",
            country == "Czechia" ~ "Czech Republic",
            TRUE ~ country)) %>%
        dplyr::filter(sex == "Both sexes") %>%
        dplyr::group_by(country) %>%
        dplyr::filter(!is.na(bmi_mean) | !is.na(country) | !is.na(year)) %>%
        dplyr::arrange(dplyr::desc(year), .by_group = TRUE) %>%
        dplyr::ungroup()

    # Create map
    if(map == "world") {
        map <- ggplot2::map_data(map)
    } else {
        map <- ggplot2::map_data("world",
                                region = "Denmark")
    }

    plot <- ggplot2::ggplot(df_clean) +
        # Map data
        ggplot2::geom_map(mapping = ggplot2::aes(map_id = region),
                 data = map,
                 map = map,
                 color="white",
                 size=0.1,
                 fill="grey") +
        # Obesity data
        ggplot2::geom_map(mapping = ggplot2::aes(fill = bmi_mean,
                               map_id = country),
                 map = map) +
        # Set display area
        ggplot2::expand_limits(x = map$long,
                      y = map$lat) +
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
                       plot.subtitle = ggplot2::element_text(size=20, color = "#4e4d47", hjust = 0.5),
                       aspect.ratio = 1/2,
                       plot.margin = ggplot2::margin(0, -0.02, -0.04, -0.02, unit = "npc"),
                       panel.border = ggplot2::element_blank(),
                       legend.position = c(0.08, 0.3)) +
        # Gradient
        ggplot2::scale_fill_gradient2(low = "#ffffcc",
                                      mid = "#ffeda0",
                                      high = "#800026",
                                      na.value = "grey",
                                      limits=c(0, 55),
                                      breaks = c(0, 10, 20, 30, 40, 50))


    # Animation
    animation <- plot +
        gganimate::transition_reveal(as.integer(year))

    # Create gif
    gif <- gganimate::animate(animation,
                              bg = 'transparent',
                              width = 1800,
                              duration = 20,
                              device = "png",
                              renderer = gganimate::magick_renderer(loop = FALSE))

    # Save gif
    gganimate::anim_save(filename = "world.gif",
                         animation = gif,
                         path = "~/Downloads/")
     }


