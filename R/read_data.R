# 1. Read PIIINP ---------------------------------------------------------------

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_p3np <- function(path = "/Users/andersaskeland/Documents/0 - Multisite/Storage (Local)/10 - Data/PIIINP/results.xlsx") {

    # Open file
    df <- readxl::read_xlsx(path = path, .name_repair = snakecase::to_snake_case)

    # Clean data
    df <- df %>%
        dplyr::rename(p3np = `atellica_resultat_µg_l`) %>%
        dplyr::select(-c(kasse, verical, horizontal, glasnummer)) %>%
        dplyr::mutate(group = snakecase::to_snake_case(group)) %>%
        dplyr::mutate(visit = snakecase::to_snake_case(visit))


    # Return data
    df
}

# 2 - Read Labka ----------------------------------------------------------

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
#' labka_read(path = "~/Documents/Data (Local)/lakba-export_2021-06-01.xlsx",
#'                 file_type = "xlsx",
#'                 indetifier = TRUE)
labka_read <- function(path, file_type="xlsx", identifier = FALSE) {

    # Read file (Currently only for XLXS - what I usually recieve from Simon (KBA).)
    if(file_type == "xlsx") {
        dat <- suppressWarnings(readxl::read_xlsx(path))
    }

    # Clean data
    dat <- dat %>%
        dplyr::select(c(CPR, PT_DATO, ANALYSE_FORKORTELSE, SVAR)) %>%
        dplyr::mutate(SVAR = stringr::str_replace_all(SVAR, ",", ".")) %>%
        dplyr::mutate(SVAR = as.numeric(SVAR)) %>%
        dplyr::rename(c(cpr_number = CPR, date = PT_DATO, analysis = ANALYSE_FORKORTELSE, value = SVAR)) %>%
        dplyr::mutate(cpr_number = purrr::map_chr(cpr_number, ~ stringr::str_replace(., "-", ""))) %>%
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

