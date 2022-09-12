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



# 3. Read TIMP1 ---------------------------------------------------------------

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_timp1 <- function(path = "/Users/andersaskeland/Documents/0 - Multisite/Storage (Local)/10 - Data/TIMP1/timp1_results.xlsx") {

    # Open file
    df <- readxl::read_xlsx(path = path, .name_repair = snakecase::to_snake_case)

    # Clean data
    df <- df %>%
        dplyr::select(-c(plate_id, sample_id, date, optøing)) %>%
        dplyr::mutate(group = snakecase::to_snake_case(group)) %>%
        dplyr::mutate(visit = snakecase::to_snake_case(visit)) %>%
        dplyr::rename(timp1_concentration = concentration) %>%
        dplyr::mutate(visit = dplyr::case_when(
            visit == "intervention" ~ "baseline",
            TRUE ~ visit
        )) %>%
        dplyr::rename(c(timp1_absorbance_rep1 = timp_1_absorbance_rep_1, timp1_absorbance_rep2 = timp_1_absorbance_rep_2)) %>%
        dplyr::mutate(timp1_concentration = as.numeric(timp1_concentration))


    # Return data
    df
}

# 3. Read TIMP1 ---------------------------------------------------------------

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_insulin_cpeptide <- function(results = "/Users/andersaskeland/Documents/0 - Multisite/Storage (Local)/10 - Data/Insulin_c-peptide/insulin_c-peptide.csv",
                                  layout = "/Users/andersaskeland/Documents/0 - Multisite/Storage (Local)/10 - Data/Insulin_c-peptide/sample_layout.xlsx") {

    # Open results
    df <- readr::read_csv2(file = results,
                           name_repair = snakecase::to_snake_case) %>%
        dplyr::select(-c("", measuring_unit, validation_status, previous_result, result_date, instrument)) %>%
        tidyr::pivot_wider(names_from = "test", values_from = result) %>%
        dplyr::mutate(sample_id = stringr::str_sub(sample_id, 2)) %>%
        dplyr::mutate(identifier = as.numeric(sample_id))


    # Open layout
    df_layout <- readxl::read_xlsx(path = layout, .name_repair = snakecase::to_snake_case) %>%
        dplyr::select(-c(kasse, verical, horizontal))

    # Combine data
    combined <- dplyr::left_join(df,
                                 df_layout, by = c("identifier" = "identifier")) %>%
        dplyr::select(-c(sample_id, identifier)) %>%
        dplyr::mutate(group = tolower(group)) %>%
        dplyr::mutate(visit = snakecase::to_snake_case(visit)) %>%
        dplyr::rename(c_peptid = cpeptid) %>%
        dplyr::rename(insulin = Insulin)

    # Return data
    combined
}
