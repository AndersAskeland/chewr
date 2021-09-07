#' Extracts data from redcap (using RCurl) and writes it to a easy to use tibble.
#' API token is colleced using R studio api when function is run.
#'
#' @param columns vector | Can contain up to 20 different variables. Use function "redcap_codebook()" to view avaliable variables.
#' @param column_types vector | Define column data types. Is in the order as displayed on redcap.
#' @param url string | Link to API website
#' @param identifier bool | Whether or not to include CPR number in the return. You must have CPR number in your data when combining data with labka data.
#' @param filter str | Filter out given data. Currently supported: 1. NAFLD - Removes people with 5% or more liver fat in the control and obese group
#'
#' @note If you wish to combine lakba with redcap data you must include CPR number (identifier = TRUE)
#' @return tibble
#' @export
#'
#' @examples
#' read_redcap(columns = c("bmi", "weight"),
#'             url = "https://redcap.rn.dk/api/")
read_redcap <- function(columns=NULL, column_types=NULL, url="https://redcap.rn.dk/api/", identifier = FALSE, filter = FALSE) {

    # Get token
    api_token <- rstudioapi::askForPassword(prompt = "Please enter your API key")

    # Check that columns are actually present inside redcap
    redcap_codes <- redcap_codebook(token = token)
    columns_tidy <- columns
    for(i in seq_along(columns)) {
        if(!columns[i] %in% dplyr::pull(redcap_codes)) {
            print(paste("The variable", columns[i], "does not exsist on redcap. Continuing without this variable."))
            columns_tidy <- columns_tidy[-i]
        }
    }

    # Extract data via curl
    export <- RCurl::postForm(
        uri=url,
        token=api_token,
        content='record',
        format='csv',
        type='flat',
        'fields[0]'='participant_id',
        'fields[1]'='cpr_nummer',
        'fields[2]'='visit_date_1',
        'fields[3]'= null_if_na(columns_tidy[1]),
        'fields[4]'= null_if_na(columns_tidy[2]),
        'fields[5]'= null_if_na(columns_tidy[3]),
        'fields[6]'= null_if_na(columns_tidy[4]),
        'fields[7]'= null_if_na(columns_tidy[5]),
        'fields[8]'= null_if_na(columns_tidy[6]),
        'fields[9]'= null_if_na(columns_tidy[7]),
        'fields[10]'= null_if_na(columns_tidy[8]),
        'fields[11]'= null_if_na(columns_tidy[9]),
        'fields[12]'= null_if_na(columns_tidy[10]),
        'fields[13]'= null_if_na(columns_tidy[11]),
        'fields[14]'= null_if_na(columns_tidy[12]),
        'fields[15]'= null_if_na(columns_tidy[13]),
        'fields[16]'= null_if_na(columns_tidy[14]),
        'fields[18]'= null_if_na(columns_tidy[15]),
        'fields[19]'= null_if_na(columns_tidy[16]),
        'fields[20]'= null_if_na(columns_tidy[17]),
        'fields[21]'= null_if_na(columns_tidy[18]),
        'fields[22]'= null_if_na(columns_tidy[19]),
        'fields[23]'= null_if_na(columns_tidy[20]),
        'events[0]'='enrolment_arm_1',
        'events[1]'='enrolment_arm_2',
        'events[2]'='enrolment_arm_3',
        'events[2]'='enrolment_arm_3',
        'events[3]'='examination__week_arm_1',
        'events[4]'='examination__week_arm_2',
        'events[5]'='examination__week_arm_3',
        'events[6]'='examination__week_arm_3b',
        'events[7]'='examination__week_arm_3c',
        rawOrLabel='raw',
        rawOrLabelHeaders='raw',
        exportCheckboxLabel='false',
        exportSurveyFields='false',
        exportDataAccessGroups='false',
        returnFormat='csv',
        .opts = list(ssl.verifypeer = TRUE)
    )

    # Convert to tibblem set CPR number, visit, and group.
    dat <- readr::read_csv(file=export, col_types=column_types, show_col_types = FALSE) %>%
        dplyr::group_by(participant_id) %>%
        tidyr::fill(cpr_nummer) %>%
        dplyr::filter(!redcap_event_name == "enrolment_arm_1" &
                          !redcap_event_name == "enrolment_arm_2" &
                          !redcap_event_name == "enrolment_arm_3") %>%
        dplyr::mutate(visit = dplyr::case_when(redcap_event_name == "examination__week_arm_1" ~ "baseline",
                                 redcap_event_name=="examination__week_arm_2" ~ "baseline",
                                 redcap_event_name=="examination__week_arm_3" ~ "baseline",
                                 redcap_event_name=="examination__week_arm_3b" ~ "month_1",
                                 redcap_event_name=="examination__week_arm_3c" ~ "month_5"),
                    group = dplyr::case_when(redcap_event_name == "examination__week_arm_1" ~ "control",
                                 redcap_event_name=="examination__week_arm_2" ~ "obese",
                                 redcap_event_name=="examination__week_arm_3" ~ "intervention",
                                 redcap_event_name=="examination__week_arm_3b" ~ "intervention",
                                 redcap_event_name=="examination__week_arm_3c" ~ "intervention")) %>%
        dplyr::select(-"redcap_event_name") %>%
        dplyr::rename(start_date = visit_date_1, cpr_number = cpr_nummer) %>%
        dplyr::relocate(participant_id, cpr_number, start_date, group, visit)

    # Filters data
    if(filter == "NAFLD") {
        if(!is.null(dat$pdff_liver_cirle_mean)) {
            print("Filtering on NAFLD based on circular ROI's")
            dat <- filter_nafld(dat = dat, token = api_token, arg = "pdff_liver_cirle_mean")
        } else if(!is.null(dat$pdff_liver_freehand)) {
            print("Filtering on NAFLD based on freehand ROI")
            dat <- filter_nafld(dat = dat, token = api_token, arg = "pdff_liver_freehand")
        } else {
            print("Filtering on NAFLD. PDFF measurment is not in data. Filtering on liver cirle mean.")
            dat <- suppressWarnings(filter_nafld(dat = dat, token = api_token))
        }
    }

    # Check if return identifer (CPR number)
    if(identifier == FALSE) {
        dat <- dat %>%
            dplyr::select(-cpr_number)
    }

    # Return
    return(dat)
}

#' Extract avaliable data variables from a redcap projet.
#' R studio API collects API key.
#'
#' @param url string | Link to API website
#'
#' @return tibble
#' @export
#'
#' @examples
#' redap_codebook(token = "dsadas8e218271732172313712",
#'                url = "https://redcap.rn.dk/api/")
redcap_codebook <- function(url="https://redcap.rn.dk/api/") {

    # Get token
    token <- rstudioapi::askForPassword(prompt = "Please enter your API key")

    # API call
    export <- RCurl::postForm(
        uri=url,
        token=token,
        content='exportFieldNames',
        format='csv',
        returnFormat='csv'
    )

    # Convert to tibble
    dat <- readr::read_csv(file=export, show_col_types = FALSE) %>%
        dplyr::select(export_field_name) %>%
        dplyr::rename(redcap_code = export_field_name)

    # Return
    return(dat)
}
