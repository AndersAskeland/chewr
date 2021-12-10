#' Calculate difference and add as columns
#'
#' @param df df | Dataframe object
#' @param column obj | Column name (Without quotes)
#'
#' @return
#' @export
#'
#' @examples
stat_difference <- function(df, column) {

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

