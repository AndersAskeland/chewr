# 1. Calculate relative changes ----------------------------------------------------------------
#' Calculate the relative change between different time points. For example
#' difference in BMI at baseline compared to month 1.
#'
#' @param df df | Dataframe object
#' @param column obj | Column name (Without quotes)
#'
#' @return Dataframe object
#' @export
#'
#' @examples
#' stat_relative_change(df, bmi)
stat_relative_change <- function(df, column) {

    # Extracted paired samples w. all variables
    df_paired <- df %>%
        dplyr::group_by(visit) %>%
        dplyr::group_by(participant_id) %>%
        dplyr::filter(!is.na({{ column }})) %>%
        dplyr::filter(dplyr::n() >= 3) %>%
        dplyr::ungroup()

    # Calculate percent change
    output <- df_paired %>%
        dplyr::group_by(participant_id) %>%
        dplyr::mutate(relative_change = dplyr::case_when(
            visit == "Baseline" | visit == "baseline" ~ NA_real_,
            visit == "During weight loss" | visit == "month_1" ~ max({{ column }}) - {{ column }},
            visit == "After weight loss" | visit == "month_5" ~ max({{ column }}) - {{ column }},
        )) %>%
        dplyr::mutate(relative_change_percent = dplyr::case_when(
            visit == "Baseline" | visit == "baseline"  ~ NA_real_,
            visit == "During weight loss" | visit == "month_1" ~ (relative_change / max({{ column }})) * 100,
            visit == "After weight loss" | visit == "month_5" ~ (relative_change / max({{ column }})) * 100
        )) %>%
        dplyr::ungroup()

    # Return
    output
}
