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
read_dxa <- function(path = "/Users/andersaskeland/Documents/1 - Data/multisite_dexa/data_old.xlsx") {

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
    df <- readr::read_csv(path, name_repair = snakecase::to_snake_case, col_types =
                              readr::cols(
                                  participant_id = readr::col_double(),
                                  visit = readr::col_character(),
                                  group = readr::col_character(),
                                  glc = readr::col_double(),
                                  insulin = readr::col_double(),
                                  homa_2_b = readr::col_double(),
                                  homa_2_s = readr::col_double(),
                                  homa_2_ir = readr::col_double()
                              )) %>%
        dplyr::select(-c(glc, insulin)) %>%
        dplyr::rename(homa2_b = homa_2_b, homa2_s = homa_2_s, homa2_ir = homa_2_ir)

    # Return
    cli::cli_h3("Status")
    cli::cli_alert_success("Finished reading HOMA2")
    df
}

# 8 - Read lihep -----------------------------------------------------------
#' Reads lithium heparin data manually entered.
#'
#' @param path str | Path to data location
#'
#' @return
#' @export
#'
#' @examples
read_lihep <- function(path = "~/Documents/1 - Data/multisite_lihep/data.xlsx") {

    # Check arguments
    checkmate::assert_file(path)

    # Message
    cli::cli_h1("Reading Lithium heperain data")

    # Read files
    df <- readxl::read_xlsx(path) %>%
        dplyr::mutate(visit = snakecase::to_snake_case(visit)) %>%
        dplyr::select(-c(identifier, l, h, i))

    # Return data
    cli::cli_h3("Status")
    cli::cli_alert_success("Finished reading Labka data")
    df
}

# 9 - Read DXA ------------------------------------------------------------


#' Reads lithium heparin data manually entered.
#'
#' @param path str | Path to data location
#'
#' @return
#' @export
#'
#' @examples
read_dxa <- function(path = "~/Documents/1 - Data/multisite_dxa/multisite_DXA.xlsx") {

    # Check arguments
    checkmate::assert_file(path)

    # Message
    cli::cli_h1("Reading DXA data")

    # Read files
    df <- readxl::read_xlsx(path) %>%
        dplyr::mutate("visit_date" = lubridate::as_date(SCAN_DATE)) %>%
        dplyr::relocate(participant_id, visit_date) %>%
        dplyr::mutate(participant_id = as.numeric(participant_id)) %>%
        dplyr::filter(!is.na(TOTAL_FAT))

     # Join on scan date
    visit_dates <- redcap_export() %>%
        dplyr::rename("visit_date" = "start_date") %>%
        dplyr::filter(visit != "year_1") %>%
        dplyr::filter(!is.na(visit_date))

    joined <- dplyr::full_join(visit_dates, df, keep = TRUE)

    missing_redcap <- joined %>%
        dplyr::filter(!is.na(group) & is.na(participant_id.y)) %>%
        dplyr::rename(participant_id = participant_id.x) %>%
        dplyr::select(1:4)

    missing_dxa <- joined %>%
        dplyr::filter(is.na(group) & is.na(visit)) %>%
        dplyr::rename(participant_id = participant_id.y) %>%
        dplyr::select(-c(1:4))

    missing_joined <- dplyr::left_join(missing_redcap, missing_dxa, by = "participant_id") %>%
        dplyr::filter(!is.na(visit_date.y))

    combined <- joined %>%
        dplyr::filter(!(is.na(participant_id.x) | is.na(participant_id.y))) %>%
        dplyr::rename(participant_id = participant_id.x) %>%
        dplyr::select(-participant_id.y) %>%
        dplyr::full_join(missing_joined) %>%
        dplyr::rename(c(visit_date = visit_date.x,
                        TISSUE_ANALYSIS_METHOD = TISSUE_ANALYSIS_METHOD.x,
                        ROI_WIDTH = ROI_WIDTH.x,
                        ROI_HEIGHT = ROI_HEIGHT.x
                        )) %>%
        dplyr::select(-c(visit_date.y,
                         SEX,
                         BIRTHDATE,
                         WEIGHT,
                         HEIGHT,
                         ETHNICITY,
                         MENOPAUSE_YEAR,
                         BMI,
                         ADJUSTED_AGE,
                         ADJUSTED_LABEL,
                         SCAN_DATE,
                         FAT_STD.x,
                         LEAN_STD.x,
                         BRAIN_FAT.x,
                         WATER_LBM.x,
                         FAT_STD.y,
                         LEAN_STD.y,
                         BRAIN_FAT.y,
                         WATER_LBM.y,
                         ROI_HEIGHT.y,
                         ROI_WIDTH.y,
                         TISSUE_ANALYSIS_METHOD.y,
                         ANDROID_NAME,
                         GYNOID_NAME,
                         VFAT_BODY_NAME,
                         VFAT_OUTERWALL_NAME,
                         VFAT_CAVITY_NAME,
                         visit_date,
                         group
                         ))
    # Return data
    cli::cli_h3("Status")
    cli::cli_alert_success("Finished reading Dxa data")
    combined
}
