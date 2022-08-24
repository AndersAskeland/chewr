# 1. Export data from redcap ----------------------------------------------------------------

#' Extracts data from redcap (using httr) and writes it to a easy to use tibble.
#' API token is collected using R studio api when function is run.
#'
#' @param ... Extra arguments
#' @param fields str | Single or several fields to read from redcap
#' @param records num | Single or several records to read. Defaults to ALL
#' @param redcap_uri str | Url to redcap API
#' @param identifier bool | Whether or not to include CPR number in the return.
#' @note If you wish to combine lakba with redcap data you must include CPR number (identifier = TRUE)
#' @return tibble
#' @export
#'
#' @examples
#' redcap_read(fields = c("bmi", "weight"),
#'             records = c(1001, 1002))
redcap_read <- function(
        fields = NULL,
        records = NULL,
        redcap_uri = "https://redcap.rn.dk/api/",
        identifier = FALSE, ...) {

    # Check arguments
    checkmate::assert_atomic(fields, any.missing = FALSE)
    checkmate::assert_atomic(records, any.missing = FALSE)
    checkmate::assert_character(redcap_uri, any.missing = FALSE, len = 1, pattern = "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$")
    checkmate::assert_logical(identifier, any.missing = FALSE, len = 1)

    # Messages
    message(crayon::bold(crayon::blue("───"),
                         crayon::white("Reading data from RedCap"),
                         crayon::blue("─────────────────────────────────")))
    message(crayon::white("Fields:"))
    purrr::map(fields, ~message(paste0(crayon::blue(" ➤ "), crayon::blurred(crayon::white(.x)))))
    message("\n")

    # Collect dynamic dots (...)
    dots <- rlang::list2(...)

    # Get API token / match arguments
    if("api_token" %in% names(dots)) {
        api_token <- dots$api_token
    } else {
        api_token <- rstudioapi::askForPassword(prompt = "Please enter your RedCap API key")
    }

    # Validate fields
    field_names <- redcap_codebook(api_token = api_token)
    purrr::walk(fields, ~ifelse(any(stringr::str_starts(pattern = .x, string = field_names$redcap_code)), T, stop(paste(.x, "does not exist."))))

    # Generate field list
    field_names <- purrr::imap(fields, ~paste0("fields[", .y + 3, "]"))
    field_values <- purrr::map(fields, ~ .x)
    field_list <- purrr::set_names(field_values, field_names)

    # Validate records
    # TODO

    # Generate record list
    if(is.null(records)) {
        record_list <- NULL
    } else {
        record_names <- purrr::imap(fields, ~paste0("records[", .y - 1, "]"))
        record_list <- purrr::map(records, ~ .x) %>%
            purrr::set_names(record_names)
    }

    # Combine field and record list
    record_field_list <- purrr::prepend(field_list, record_list)

    # Generate complete request list
    form_data <- list(token = api_token,
                     content = 'record',
                     format = 'csv',
                     type = 'flat',
                     csvDelimiter = ',',
                     'fields[0]'='participant_id',
                     'fields[1]'='cpr_nummer',
                     'fields[2]'='visit_date_1',
                     'events[0]'='enrolment_arm_1',
                     'events[1]'='enrolment_arm_2',
                     'events[2]'='enrolment_arm_3',
                     'events[2]'='enrolment_arm_3',
                     'events[3]'='examination__week_arm_1',
                     'events[4]'='examination__week_arm_2',
                     'events[5]'='examination__week_arm_3',
                     'events[6]'='examination__week_arm_3b',
                     'events[7]'='examination__week_arm_3c',
                     rawOrLabel = 'raw',
                     rawOrLabelHeaders = 'raw',
                     exportCheckboxLabel = 'false',
                     exportSurveyFields = 'false',
                     exportDataAccessGroups = 'false',
                     returnFormat = 'csv') %>%
        purrr::prepend(record_field_list)

    # API request
    httr::set_config(httr::config(ssl_verifypeer = TRUE))
    request <- httr::POST(redcap_uri, body = form_data, encode = "form") %>%
        httr::content(show_col_types = FALSE)

    # Clean data and basic renaming
    redcap_df <- request %>%
        dplyr::group_by(participant_id) %>%
        tidyr::fill(dplyr::everything()) %>%
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
        dplyr::relocate(participant_id, cpr_number, start_date, group, visit) %>%
        dplyr::ungroup()

    # Manage identifier (CPR-number)
    if(identifier == FALSE) {
        redcap_df <- redcap_df %>%
            dplyr::select(-cpr_number)
    }

    # Return
    redcap_df
}

#' Extract available data variables from a redcap project.
#' R studio API collects API key.
#'
#' @param url string | Link to API website
#' @param ... Extra arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' redap_codebook(token = "dsadas8e218271732172313712",
#'                url = "https://redcap.rn.dk/api/")
redcap_codebook <- function(url="https://redcap.rn.dk/api/", ...) {

    # Extract args
    args <- list(...)

    # Set API token
    if(!exists("api_token", where = args, inherits = FALSE) || is.null(args$api_token)) {
        api_token <- rstudioapi::askForPassword(prompt = "Please enter your API key")
    } else {
        api_token <- args$api_token
    }

    # API call
    export <- RCurl::postForm(
        uri=url,
        token=api_token,
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

# 2. Import data to redcap ----------------------------------------------------------------

#' Imports results from p3np analysis into redcap
#'
#' @param df
#' @param redcap_uri
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
redcap_import_p3np <- function(p3np_df, redcap_uri = "https://redcap.rn.dk/api/", ...) {

    # Collect dynamic dots (...)
    dots <- rlang::list2()

    # Get API token / match arguments
    if("api_token" %in% names(dots)) {
        api_token <- dots$api_token
    } else {
        api_token <- rstudioapi::askForPassword(prompt = "Please enter your RedCap API key")
    }

    # Get redcap data
    redcap_df <- redcap_read(identifier = T)

    # Combine data
    combined_df <- combine_redcap_p3np(p3np_data = p3np_df,
                                     redcap_data = redcap_df,
                                     identifier = T)

    # Clean data for import
    clean_df <- combined_df %>%
        dplyr::rename(redcap_event_name = visit) %>%
        dplyr::mutate(redcap_event_name = dplyr::case_when(
            redcap_event_name == "baseline" & group == "control" ~ "examination__week_arm_1",
            redcap_event_name=="baseline" & group == "obese" ~ "examination__week_arm_2",
            redcap_event_name=="baseline" & group == "intervention" ~ "examination__week_arm_3",
            redcap_event_name=="month_1" ~ "examination__week_arm_3b",
            redcap_event_name=="month_5" ~ "examination__week_arm_3c")) %>%
        dplyr::select(-c(cpr_number, start_date, group, identifier)) %>%
        dplyr::rename_with(tolower) %>%
        dplyr::rename_with(~stringr::str_replace(.x, "-", "_"))

    # Import data
    import <- RCurl::postForm(
        uri = redcap_uri,
        token=api_token,
        content='record',
        type='flat',
        format="csv",
        overwriteBehavior='normal',
        data=readr::format_csv(clean_df, na = "")
    )

    # Return
    import
}


#' Imports lakba data into redcap.
#'
#' @param labka_df
#' @param redcap_uri
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
redcap_import_lakba <- function(labka_df, redcap_uri = "https://redcap.rn.dk/api/", ...) {

    # Collect dynamic dots (...)
    dots <- rlang::list2(...)

    # Get API token / match arguments
    if("api_token" %in% names(dots)) {
        api_token <- dots$api_token
    } else {
        api_token <- rstudioapi::askForPassword(prompt = "Please enter your RedCap API key")
    }

    # Get redcap data
    redcap_df <- redcap_read(identifier = T)

    # Combine redcap w. labka
    combined_df <- combine_redcap_labka(labka_data = labka_df,
                                        redcap_data = redcap_df,
                                        identifier = T)

    # Clean data for import
    clean_df <- combined_df %>%
        dplyr::rename(redcap_event_name = visit) %>%
        dplyr::mutate(redcap_event_name = dplyr::case_when(
            redcap_event_name == "baseline" & group == "control" ~ "examination__week_arm_1",
            redcap_event_name=="baseline" & group == "obese" ~ "examination__week_arm_2",
            redcap_event_name=="baseline" & group == "intervention" ~ "examination__week_arm_3",
            redcap_event_name=="month_1" ~ "examination__week_arm_3b",
            redcap_event_name=="month_5" ~ "examination__week_arm_3c")) %>%
        dplyr::select(-c(cpr_number, start_date, group)) %>%
        dplyr::rename_with(tolower) %>%
        dplyr::rename_with(~stringr::str_replace(.x, "-", "_"))

    # Import data
    import <- RCurl::postForm(
        uri = redcap_uri,
        token=api_token,
        content='record',
        type='flat',
        format="csv",
        overwriteBehavior='normal',
        data=readr::format_csv(clean_df, na = "")
    )

    # Return
    import
}

#' Import T1 fibrosis data into redcap
#'
#' @param t1_df
#' @param redcap_uri
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
redcap_import_t1 <- function(file, redcap_uri = "https://redcap.rn.dk/api/", ...) {

    # Collect dynamic dots (...)
    dots <- rlang::list2(...)

    # Get API token / match arguments
    if("api_token" %in% names(dots)) {
        api_token <- dots$api_token
    } else {
        api_token <- rstudioapi::askForPassword(prompt = "Please enter your RedCap API key")
    }

    # Read data
    t1_df <- readr::read_csv2(file)

    # Clean data
    clean_df <- t1_df %>%
        dplyr::mutate(redcap_event_name = dplyr::case_when(
            redcap_event_name == "Examination - Week 0 (Arm 1: Control)" ~ "examination__week_arm_1",
            redcap_event_name == "Examination - Week 0 (Arm 2: Obese)" ~ "examination__week_arm_2",
            redcap_event_name == "Examination - Week 0 (Arm 3: Intervention)" ~ "examination__week_arm_3",
            redcap_event_name == "Examination - Week 4 (Arm 3: Intervention)" ~ "examination__week_arm_3b",
            redcap_event_name == "Examination - Week 20 (Arm 3: Intervention)" ~ "examination__week_arm_3c")) %>%
        dplyr::rename_with(tolower) %>%
        dplyr::rename_with(~stringr::str_replace(.x, "-", "_"))

    # Import data
    import <- RCurl::postForm(
        uri = redcap_uri,
        token=api_token,
        content='record',
        type='flat',
        format="csv",
        overwriteBehavior='normal',
        data=readr::format_csv(clean_df, na = "")
    )

    # Return
    import
}

# 3. Randomize data ----------------------------------------------------------------

#' Randomizes continuous data.
#'
#' @param df
#' @param remove vector of variables to be completly removed.
#'
#' @return
#' @export
#'
#' @examples
redcap_anonymize_data <- function(df, remove = NULL) {

    # Remove potential variables
    if(!is.null(remove)) {
        df <- df %>%
            dplyr::select(-all_of(remove))
    }

    # Anonymise all columns (except participant ID, group and visit)
    random_df <- df %>%
        dplyr::group_by(visit, group) %>%
        dplyr::mutate(
            dplyr::across(where(is.numeric) & !c(participant_id), ~sample(x = min(.x, na.rm = T):max(.x, na.rm = T),
                                                                          size = length(.x),
                                                                          replace = T))) %>%
        dplyr::mutate(dplyr::across(participant_id, ~sample(.x))) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(participant_id)

    # Return
    random_df
}


# 4. Allocation functions ----------------------------------------------------------------

#' Takes a redcap export file as input and changes redcap arm on participant
#'
#' @param export_dir str | Export file path
#' @param file_location str | File location path
#'
#' @return None
#' @export
#'
#' @examples
#' redcap_manual_allocate(file_location = "/home/test.csv")
redcap_manual_allocate <- function(file_location, export_dir = "/Users/andersaskeland/Documents/Multisite (Local)/5 - Redcap/Import/") {

    # Read file
    df <- readr::read_csv(file = file_location, col_types = readr::cols())

    # Generate new participant ID (4*** to 3***)
    df$participant_id <- df$participant_id - 1000

    # Change enrollment arm
    if(df$group == 0) {
        message_text <- paste(">>> ", df$participant_id, "transfered to intervention!")
        df$redcap_event_name <- "enrolment_arm_3"
    } else if(df$group == 1) {
        message_text <- paste(">>> ", df$participant_id, "transfered to obese!")
        df$redcap_event_name <- "enrolment_arm_2"
    } else if(df$group == 2) {
        message_text <- paste(">>> ", df$participant_id, "transfered to excluded!")
        df$redcap_event_name <- "enrolment_arm_5"
    }

    # Write data
    file_name <- paste0(export_dir, "redcap_allocation_import", "_", (df$participant_id + 1000), "-", df$participant_id, ".csv")
    readr::write_csv(df, file = file_name, na = "")

    # Print message
    message(message_text)
}

#' Automatically transfer participant from allocation group to supplied group
#'
#' @param participant_id int | Participant you want to transfer
#' @param url str | Url for redcap server
#' @param ... Extra parameters
#'
#' @export
#'
#' @examples
#' redcap_allocate_automatic(3030)
redcap_allocate_automatic <- function(participant_id, url="https://redcap.rn.dk/api/", ...) {

    # Error out if ID not supported
    if(participant_id < 4000 || participant_id > 4999) {
        warning("Participant ID is not supported (Supported ID's: 4000 > 4999). Remember to use allocation ID.")
        return(0)
    }

    # Extract ... args
    args <- list(...)

    # Set API token
    if(!exists("api_token", where = args, inherits = FALSE) || is.null(args$api_token)) {
        api_token <- rstudioapi::askForPassword(prompt = "Please enter your API key")
    } else {
        api_token <- args$api_token
    }

    # Extract data via curl
    export <- RCurl::postForm(
        uri=url,
        token=api_token,
        content='record',
        format='csv',
        type='flat',
        csvDelimiter='',
        'records[0]'=participant_id,
        'events[0]'='enrolment_arm_4',
        rawOrLabel='raw',
        rawOrLabelHeaders='raw',
        exportCheckboxLabel='false',
        exportSurveyFields='false',
        exportDataAccessGroups='false',
        returnFormat='csv',
        .opts = list(ssl.verifypeer = TRUE)
        )

    # Turn into tibble
    df <- readr::read_csv(export, col_types = readr::cols())

    # Check if event is allocation group
    if(df$redcap_event_name != "enrolment_arm_4") {
        warning("Record is not in allocation arm.")
        return(0)
    }

    # Change participant ID from 4*** to 3*** and change enrollment arm
    df$participant_id <- df$participant_id - 1000
    if(df$group == 0) {
        message_text <- paste(">>> ", df$participant_id, "transfered to intervention!")
        df$redcap_event_name <- "enrolment_arm_3"
    } else if(df$group == 1) {
        message_text <- paste(">>> ", df$participant_id, "transfered to obese!")
        df$redcap_event_name <- "enrolment_arm_2"
    } else if(df$group == 2) {
        message_text <- paste(">>> ", df$participant_id, "transfered to excluded!")
        df$redcap_event_name <- "enrolment_arm_5"
    } else {
        warning("Participant has not been allocated to a group. Stopping function!")
        return(0)
    }

    # Check if already present
    if(check_record(participant_id=df$participant_id, enrolment_arm = df$redcap_event_name, api_token = api_token) == FALSE) {
        warning("Participant ", df$participant_id, " already exist in ", df$redcap_event_name, ". Function stopped.")
        return(0)
    }

    # Import into correct group
    result <- RCurl::postForm(
        token=api_token,
        uri=url,
        content='record',
        format='csv',
        type='flat',
        overwriteBehavior='normal',
        forceAutoNumber='false',
        data=readr::format_csv(df, na = "")
    )

    # Message
    message(message_text, " Return code: ", result)
}
