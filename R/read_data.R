# 1 - Read PIIINP ---------------------------------------------------------------

#' Read PIIINP data and returns as tibble.
#'
#' @param path str | Path to excel file with PIIINP results
#'
#' @return tibble
#' @export
#'
#' @examples
#' # Read PIIINP data
#' df <- read_p3np()
read_p3np <- function(path = "/Users/andersaskeland/Documents/0 - Multisite/Storage (Local)/10 - Data/PIIINP/results.xlsx") {

    # Check arguments
    checkmate::assert_file(path)

    # Message
    cli::cli_h1("Reading PIIINP")

    # Open file
    df <- readxl::read_xlsx(path = path,
                            .name_repair = snakecase::to_snake_case)

    # Clean data
    df <- df %>%
        dplyr::rename(p3np = `atellica_resultat_µg_l`, participant_id = patient_id) %>%
        dplyr::select(-c(kasse, verical, horizontal, glasnummer, identifier)) %>%
        dplyr::mutate(group = snakecase::to_snake_case(group)) %>%
        dplyr::mutate(visit = snakecase::to_snake_case(visit))

    # Return data
    cli::cli_h3("Status")
    cli::cli_alert_success("Finished reading PIIINP.")
    df
}


# 2 - Read DXA ------------------------------------------------------------
#' Reads DXA data and returns a tibble. TODO
#'
#' @param path str | Path to excel file with DXA results
#'
#' @return
#' @export
#'
#' @examples
#' # Read DXA data
#' df <- read_dxa()
read_dxa <- function(path = "/Users/andersaskeland/Documents/1 - Data/multisite_dxa/multisite_DXA.xlsx") {

    # Check arguments
    checkmate::assert_file(path)

    # Message
    cli::cli_h1("Reading DXA data")

    # Read data
    df <- readxl::read_xlsx(path)

    # Remove scans not containing data
    df <- df %>%
        dplyr::filter(!is.na(FAT_STD))

    # TODO

}

# 3 - Read Labka ----------------------------------------------------------

#' Read Labka data and converts to tibble.
#'
#' @param path string | File location of Labka data
#' @param identifier bool | Whether or not to include CPR number in the return.
#' One must have CPR number in your data when combining RedCap data with Labka data.
#' @note If you wish to combine Lakba with redcap data you must include CPR number (identifier = TRUE)
#' @return tibble
#' @export
#'
#' @examples
#' # Read labka data and get tibble with identifier
#' labka_read(path = "~/Documents/Data (Local)/lakba-export.xlsx",
#'                 indetifier = TRUE)
read_labka <- function(path, identifier = FALSE) {

    # Check arguments
    checkmate::assert_file(path)

    # Message
    cli::cli_h1("Reading LABKA data")

    # Read data
    df <- readxl::read_xlsx(path)

    # Clean data
    df <- df %>%
        dplyr::select(c(CPR, PT_DATO, ANALYSE_FORKORTELSE, SVAR)) %>%
        dplyr::mutate(SVAR = stringr::str_replace_all(SVAR, ",", ".")) %>%
        dplyr::mutate(SVAR = as.numeric(SVAR)) %>%
        dplyr::rename(c(cpr_number = CPR, start_date = PT_DATO, analysis = ANALYSE_FORKORTELSE, value = SVAR, )) %>%
        dplyr::mutate(cpr_number = purrr::map_chr(cpr_number, ~ stringr::str_replace(., "-", ""))) %>%
        tidyr::pivot_wider(names_from = analysis, values_from = value) %>%
        dplyr::select(-c("Projekt-in", "Projekt"))

    # Check if return identifer (CPR number)
    if(identifier == FALSE) {
        df <- df %>%
            dplyr::select(-cpr_number)
    }

    # Return
    cli::cli_h3("Status")
    cli::cli_alert_success("Finished reading Labka data")
    df
}

# 4 - Read CK18 -----------------------------------------------------------

#' Read CK18 data and returns as tibble.
#'
#' @param path str | Path to excel file with CK18 results
#'
#' @return tibble
#' @export
#'
#' @examples
#' # Read CK18 data
#' df <- read_ck18()
read_ck18 <- function(path = "/Users/andersaskeland/Documents/0 - Multisite/Storage (Local)/10 - Data/CK-18/CK18_results.xlsx") {

    # Check arguments
    checkmate::assert_file(path)

    # Message
    cli::cli_h1("Reading CK18")

    # Open file
    df <- readxl::read_xlsx(path = path, .name_repair = snakecase::to_snake_case)

    # Clean data
    df <- df %>%
        dplyr::select(-c(plate_id, sample_id, date, optøing)) %>%
        dplyr::mutate(group = snakecase::to_snake_case(group)) %>%
        dplyr::mutate(visit = snakecase::to_snake_case(visit)) %>%
        dplyr::rename(ck18_concentration = concentration, participant_id = "patient_id") %>%
        dplyr::mutate(visit = dplyr::case_when(
            visit == "intervention" ~ "baseline",
            TRUE ~ visit
        )) %>%
        dplyr::rename(c(ck18_absorbance_rep1 = ck_18_absorbance_rep_1, ck18_absorbance_rep2 = ck_18_absorbance_rep_2)) %>%
        dplyr::mutate(ck18_concentration = as.numeric(ck18_concentration))


    # Return data
    cli::cli_h3("Status")
    cli::cli_alert_success("Finished reading CK18 data")
    df
}

# 5 - Read TIMP1 ---------------------------------------------------------------

#' Reads TIMP1 data
#'
#' @param path str | Path to excel file with PIIINP results
#'
#' @return tibble
#' @export
#'
#' @examples
#' # Read TIMP1 data
#' df <- read_timp1()
read_timp1 <- function(path = "/Users/andersaskeland/Documents/0 - Multisite/Storage (Local)/10 - Data/TIMP1/timp1_results.xlsx") {

    # Check arguments
    checkmate::assert_file(path)

    # Message
    cli::cli_h1("Reading TIMP1")

    # Open file
    df <- readxl::read_xlsx(path = path, .name_repair = snakecase::to_snake_case)

    # Clean data
    df <- df %>%
        dplyr::select(-c(plate_id, sample_id, date, optøing)) %>%
        dplyr::mutate(group = snakecase::to_snake_case(group)) %>%
        dplyr::mutate(visit = snakecase::to_snake_case(visit)) %>%
        dplyr::rename(timp1_concentration = concentration, participant_id = "patient_id") %>%
        dplyr::mutate(visit = dplyr::case_when(
            visit == "intervention" ~ "baseline",
            TRUE ~ visit
        )) %>%
        dplyr::rename(c(timp1_absorbance_rep1 = timp_1_absorbance_rep_1, timp1_absorbance_rep2 = timp_1_absorbance_rep_2)) %>%
        dplyr::mutate(timp1_concentration = as.numeric(timp1_concentration))


    # Return data
    cli::cli_h3("Status")
    cli::cli_alert_success("Finished reading TIMP1 data")
    df
}

# 6 - Read Insulin + C-peptide ---------------------------------------------------------------

#' Reads insulin and C-peptide data
#'
#' @param results path | Path to file containing results & identifiers
#' @param layout path | Path to file containing group information
#'
#' @return tibble
#' @export
#'
#' @examples
#' # Read insulin and c-peptide data
#' df <- read_insulin()
read_insulin <- function(results = "/Users/andersaskeland/Documents/0 - Multisite/Storage (Local)/10 - Data/Insulin_c-peptide/insulin_c-peptide.csv",
                                  layout = "/Users/andersaskeland/Documents/0 - Multisite/Storage (Local)/10 - Data/Insulin_c-peptide/sample_layout.xlsx") {

    # Check arguments
    checkmate::assert_file(results)
    checkmate::assert_file(layout)

    # Message
    cli::cli_h1("Reading Isulin and C-peptide data")

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
        dplyr::rename(c_peptid = cpeptid, participant_id = patient_id) %>%
        dplyr::rename(insulin = Insulin)

    # Return data
    cli::cli_h3("Status")
    cli::cli_alert_success("Finished reading insulin and C-peptide data")
    combined
}

# 7 - Read HOMA -----------------------------------------------------------
#' Read HOMA-ir from CSV file. Homa IR is calculated using computer model from
#' HOMA2.
#'
#' @param path str | Path to excel file with PIIINP results
#'
#' @return
#' @export
#'
#' @examples
read_homa <- function(path = "~/Documents/1 - Data/multisite_homa/homa_ir.csv") {

    # Check arguments
    checkmate::assert_file(path)

    # Message
    cli::cli_h1("Reading HOMA IR data")

    # Read data
    df <- readr::read_csv(path, name_repair = snakecase::to_snake_case, col_types = "iccdddddd") %>%
        dplyr::select(-c(glc, insulin)) %>%
        dplyr::rename(homa2_b = homa_2_b, homa2_s = homa_2_s, homa2_ir = homa_2_ir)

    # Return
    cli::cli_h3("Status")
    cli::cli_alert_success("Finished reading HOMA2")
    df
}
