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

#' Extract participant information and writes it to an easy to view table.
#'
#' @return Data table graphic
#' @export
#'
#' @examples
#' extract_participant_end_parameters(3030)
extract_participant_end_parameters <- function(partcipant_id) {

    # Read data
    df <- read_redcap(columns = c("bmi", "weight", "whr",
                                  "waist", "hip", "systolic_bp_avg",
                                  "diastolic_bp_avg", "pdff_fat_prelim",
                                  "pdff_liver_cirle_mean", "pdff_pancreas_cirkle_mean",
                                  "dexa_total_fat"))

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
            columns = c(waist, hip),
            decimals = 0,
            pattern = "{x}cm") %>%
        gt::fmt_number(
            columns = c(pdff_fat_prelim, pdff_liver_cirle_mean, pdff_pancreas_cirkle_mean),
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
            pdff_pancreas_cirkle_mean = "Circular ROI")

    # Return
    table
}
