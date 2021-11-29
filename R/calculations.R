

calculate_change <- function(df, column) {

    # Names
    # column_name_change <- eval(parse(text=paste0(column, "_change")))
    # column_name_percent <- eval(parse(text=paste0(column, "_percent_change")))

    # Get only paired samples
    df_paired <- df %>%
        dplyr::group_by(visit) %>%
        dplyr::group_by(participant_id) %>%
        dplyr::filter(!is.na({{ column }})) %>%
        dplyr::filter(dplyr::n() >= 3) %>%
        dplyr::ungroup()

    # Calculate percent change
    output <- df_paired %>%
        dplyr::group_by(participant_id) %>%
        dplyr::mutate(absolute_change = max({{ column }}) - min({{ column }}),
                      percent_change = (absolute_change / max({{ column }})) * 100) %>%
        dplyr::ungroup()

    # Return
    output
}
