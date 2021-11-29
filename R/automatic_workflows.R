#' Creates an automated plot comparing different participant groups from the
#' Multisite study. An alternative approch to using this automatic function
#' would be to manually create a ggplot with the "geom_scatter_column()" function
#' and "theme_chewr()".
#'
#' @param ...
#' @param comparison
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
auto_compare_baseline <- function(comparison, ...) {

    # Extract labels from ... argument
    args <- list(...)
    labels <- extract_labs(args, ylab = comparison)

    # Extract theme
    theme <- extract_theme(args)

    # Read data and rename groups
    df <- read_redcap(columns = comparison, filter = "NAFLD")
    df <- rename_xlabs(df)

    # Plot data
    df %>% dplyr::filter(visit == "Baseline") %>%
        dplyr::group_by(group) %>%
        ggplot2::ggplot(ggplot2::aes(x = factor(group, levels=c("Lean control",
                                                                "Obese without NAFLD",
                                                                "Obese with NAFLD")),
                                     y = eval(parse(text=comparison)))) +
        geom_scatter_column(scale = theme$scale, color = theme$color) +
        ggplot2::labs(title = labels$title,
                      subtitle = labels$subtitle,
                      y = labels$ylab,
                      x = NULL) +
        theme_chewr(scale = theme$scale)
}

#' Automatically compare weight loss
#'
#' @param comparison
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
auto_compare_weight_loss <- function(comparison, ...) {

    # Extract labels from ... argument
    args <- list(...)
    labels <- extract_labs(args, ylab = comparison)

    # Extract theme
    theme <- extract_theme(args)

    # Read data and rename groups
    df <- read_redcap(columns = comparison, filter = "NAFLD")
    df <- rename_xlabs(df)

    # Plot
    df %>% dplyr::filter(group == "Obese with NAFLD") %>%
        dplyr::group_by(visit) %>%
        ggplot2::ggplot(ggplot2::aes(x = factor(visit, levels=c("Baseline", "During weight loss", "After weight loss")),
                                     y = eval(parse(text=comparison)))) +
        geom_paired(paired_variable = "participant_id", color = theme$color, scale = theme$scale) +
        ggplot2::labs(title = labels$title,
                      subtitle = labels$subtitle,
                      y = labels$ylab,
                      x = NULL) +
        theme_chewr(scale = theme$scale)
}



#' Extract participant information and writes it to an easy to view table.
#'
#' @return Data table graphic
#' @export
#'
#' @examples
#' extract_participant_end_parameters(3030)
auto_end_table <- function(partcipant_id) {

    # Read data
    df <- read_redcap(columns = c("bmi", "weight", "whr",
                                  "waist", "hip", "systolic_bp_avg",
                                  "diastolic_bp_avg", "pdff_fat_prelim",
                                  "pdff_liver_cirle_mean", "pdff_pancreas_cirkle_mean",
                                  "total_body_fat"))

    # Table
    table <- df %>%
        dplyr::filter(participant_id == partcipant_id) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(start_date, group, participant_id)) %>%
        gt::gt(groupname_col = "visit") %>%
        gt::tab_header(
            title = gt::md(paste0("Information about participant **", partcipant_id, "**"))) %>%
        gt::cols_merge(
            columns = c(systolic_bp_avg, diastolic_bp_avg),
            pattern = "{1}/{2}") %>%
        gt::tab_spanner(
            label = "Anthropometrics",
            columns = c(weight, bmi, waist, hip, whr)) %>%
        gt::tab_spanner(
            label = "Liver fat",
            columns = c(pdff_fat_prelim, pdff_liver_cirle_mean)) %>%
        gt::tab_spanner(
            label = "Pancreatic fat",
            columns = c(pdff_pancreas_cirkle_mean)) %>%
        gt::fmt_number(
            columns = bmi,
            decimals = 1) %>%
        gt::fmt_number(
            columns = whr,
            decimals = 2) %>%
        gt::fmt_number(
            columns = weight,
            decimals = 1,
            pattern = "{x}kg") %>%
        gt::fmt_number(
            columns = c(waist, hip),
            decimals = 0,
            pattern = "{x}cm") %>%
        gt::fmt_number(
            columns = c(pdff_fat_prelim, pdff_liver_cirle_mean, pdff_pancreas_cirkle_mean, total_body_fat),
            decimals = 1,
            pattern = "{x}%") %>%
        gt::fmt_number(
            columns = c(systolic_bp_avg, diastolic_bp_avg),
            decimals = 0) %>%
        gt::cols_label(
            weight = "Weight",
            bmi = "BMI",
            waist = "Waist circ.",
            hip = "Hip circ.",
            whr = "WHR",
            systolic_bp_avg = "Blood pressure",
            pdff_fat_prelim = "Prelim.",
            pdff_liver_cirle_mean = "Circular ROI",
            pdff_pancreas_cirkle_mean = "Circular ROI",
            total_body_fat = "Total fat (DEXA)")

    # Return
    table
}
