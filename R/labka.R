#' Read Labka data and converts to tibble.
#'
#' @param path string | File location of Labka data
#' @param file_type string | File type - Currently only supports xlsx
#' @param identifier bool | Whether or not to include CPR number in the return. You must have CPR number in your data when combining data with labka data.
#' @note If you wish to combine lakba with redcap data you must include CPR number (identifier = TRUE)
#' @return tibble
#' @export
#'
#' @examples
#' read_labka_data(path = "~/Documents/Data (Local)/lakba-export_2021-06-01.xlsx",
#'                 file_type = "xlsx",
#'                 indetifier = TRUE)
read_labka_data <- function(path, file_type="xlsx", identifier = FALSE) {

    # Read file (Currently only for XLXS - what I usually recieve from Simon (KBA).)
    if(file_type == "xlsx") {
        dat <- backports::suppressWarnings(readxl::read_xlsx(path))
    }

    # Clean data
    dat <- dat %>%
        dplyr::select(c(CPR, PT_DATO, ANALYSE_FORKORTELSE, SVAR_TAL)) %>%
        dplyr::rename(c(cpr_number = CPR, date = PT_DATO, analysis = ANALYSE_FORKORTELSE, value = SVAR_TAL)) %>%
        dplyr::mutate(cpr_number = map_chr(cpr_number, ~str_replace(., "-", ""))) %>%
        tidyr::pivot_wider(names_from = analysis, values_from = value) %>%
        dplyr::select(-c("Projekt-in", "Projekt"))

    # Check if return identifer (CPR number)
    if(identifier == FALSE) {
        dat <- dat %>%
            dplyr::select(-cpr_number)
    }

    # Return
    return(dat)
}

#' Combine data from labka export with redcap data.
#'
#' @param labka_data Tibble | Tibble of lakba data
#' @param redcap_data Tibble | Tibble of redcap data
#' @param identifier bool | Whether or not to include CPR number in the return. You must have CPR number in your data when combining data with labka data.
#'
#' @note You need to have CPR number included in both the labka and redcap data to use this function (left join is based on CPR number and date).
#' @return tibble
#' @export
#'
#' @examples
#' combine_redcap_labka(labka_data = labka_data,
#'                      redcap_data = redcap_data,
#'                      identifier = TRUE)
combine_redcap_labka <- function(labka_data, redcap_data, identifier = FALSE) {

    # Check if data includes cpr_number
    if(!"cpr_number" %in% colnames(labka_data)) {
        print("There is no column for CPR number in labka data.")
        stop()
    } else if(!"cpr_number" %in% colnames(redcap_data)) {
        print("There is no column for CPR number in redap data.")
        stop()
    }

    # Combine redcap with labka_data
    combined <- left_join(redcap_data, labka_data, by = c("cpr_number" = "cpr_number", "start_date" = "date"))

    # Check if return identifer (CPR number)
    if(identifier == FALSE) {
        dat <- combined %>%
            dplyr::select(-cpr_number)
    }

    # Return
    return(combined)
}
