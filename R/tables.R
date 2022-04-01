# 1. General tables ----------------------------------------------------------------
#' Extract participant information and writes it to an easy to view table.
#'
#' @param partcipant_id
#'
#' @return Data table graphic
#' @export
#'
#' @examples
#' table_information(3030)
table_information <- function(partcipant_id) {

    # Read data
    df <- redcap_read(fields = c("bmi", "weight", "whr",
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
    return(table)
}
