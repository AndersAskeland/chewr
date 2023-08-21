# 1. Export from redcap ----------------------------------------------------------------

#' Extracts/exports data from redcap (using httr) and converts it to a tibble.
#' API token is collected using R studio api when function is run (Avoids
#' collecting API token in Rhistory).
#'
#' @param fields str or vec | Single field or vector of fields to export.
#' @param records num | Single or several records to read. Defaults to ALL.
#' @param redcap_uri str | URL to redcap API. Defaults to RN server.
#' @param identifier bool | Whether or not to include CPR number in export.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Extra arguments.
#' @return tibble
#' @export
#'
#' @examples
#' # Export BMI and weight for participants 1001 and 1002. Is exported as tibble.
#' df <- redcap_read(fields = c("bmi", "weight"),
#'             records = c(1001, 1002))
redcap_export <- function(
        fields = NULL,
        records = NULL,
        redcap_uri = "https://redcap.rn.dk/api/",
        identifier = FALSE, ...) {

    # Check arguments
    checkmate::assert_atomic(fields, any.missing = FALSE)
    checkmate::assert_atomic(records, any.missing = FALSE)
    checkmate::assert_character(redcap_uri, any.missing = FALSE, len = 1, pattern = "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$")
    checkmate::assert_logical(identifier, any.missing = FALSE, len = 1)

    # Message
    cli::cli_h1("Exporting data from RedCap")
    cli::cli_h3("Fields")
    purrr::walk(fields, ~cli::cli_bullets(c(">" = .x)))

    # Collect dynamic dots (...)
    dots <- rlang::list2(...)

    # Get API token / match arguments
    if("api_token" %in% names(dots)) {
        api_token <- dots$api_token
    } else {
        api_token <- rstudioapi::askForPassword(prompt = "Please enter your RedCap API key")
    }

    # Check api token has been entered
    checkmate::assert_character(api_token, n.chars = 32)

    # Validate that all fields exist on RedCap
    field_names <- suppressMessages(redcap_export_field_list(api_token = api_token))
    purrr::walk(fields, ~ifelse(any(stringr::str_starts(pattern = .x, string = field_names$export_field_name)), T, stop(paste(.x, "does not exist."))))

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
        record_names <- purrr::imap(records, ~paste0("records[", .y - 1, "]"))
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
                     'events[8]'='follow_up__year_1_arm_3',
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

    # Clean data and basic renaming (something is going amiss here)
    redcap_df <- request %>%
        tidyr::fill(cpr_nummer) %>%
        dplyr::mutate(
            dplyr::across(dplyr::all_of(fields), ~custom_fill(.x,
                                               dplyr::cur_column(),
                                               request %>% tidyr::fill(cpr_nummer)))) %>%
        dplyr::group_by(participant_id) %>%
        dplyr::filter(!redcap_event_name == "enrolment_arm_1" &
                          !redcap_event_name == "enrolment_arm_2" &
                          !redcap_event_name == "enrolment_arm_3") %>%
        dplyr::mutate(visit = dplyr::case_when(redcap_event_name == "examination__week_arm_1" ~ "baseline",
                                               redcap_event_name=="examination__week_arm_2" ~ "baseline",
                                               redcap_event_name=="examination__week_arm_3" ~ "baseline",
                                               redcap_event_name=="examination__week_arm_3b" ~ "month_1",
                                               redcap_event_name=="examination__week_arm_3c" ~ "month_5",
                                               redcap_event_name=="follow_up__year_1_arm_3" ~ "year_1"),
                      group = dplyr::case_when(redcap_event_name == "examination__week_arm_1" ~ "control",
                                               redcap_event_name=="examination__week_arm_2" ~ "obese",
                                               redcap_event_name=="examination__week_arm_3" ~ "intervention",
                                               redcap_event_name=="examination__week_arm_3b" ~ "intervention",
                                               redcap_event_name=="examination__week_arm_3c" ~ "intervention",
                                               redcap_event_name=="follow_up__year_1_arm_3" ~ "intervention")) %>%
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
    cli::cli_h3("Status")
    cli::cli_alert_success("Sucessfully exported {fields}.")
    redcap_df
}

#' Extract field list from a redcap project.
#' API token is collected using R studio api when function is run (Avoids
#' collecting API token in Rhistory).
#'
#' @param redcap_uri str | URL to redcap API. Defaults to RN server.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Extra arguments.
#'
#' @return tibble
#' @export
#'
#' @examples
#' # Extracts all code's from redcap and store them in tibble
#' df <- redap_codebook()
redcap_export_field_list <- function(redcap_uri="https://redcap.rn.dk/api/",
                            ...) {

    # Check arguments
    checkmate::assert_character(redcap_uri, any.missing = FALSE, len = 1, pattern = "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$")

    # Message
    cli::cli_h1("Exporting RedCap field list")

    # Collect dynamic dots (...)
    dots <- rlang::list2(...)

    # Get API token / match arguments
    if("api_token" %in% names(dots)) {
        api_token <- dots$api_token
    } else {
        api_token <- rstudioapi::askForPassword(prompt = "Please enter your RedCap API key")
    }

    # Generate request list
    form_data <- list(token=api_token,
                     content='exportFieldNames',
                     format='csv',
                     returnFormat='csv'
    )

    print(form_data)

    # API call
    httr::set_config(httr::config(ssl_verifypeer = TRUE))
    request <- httr::POST(redcap_uri, body = form_data, encode = "form") %>%
        httr::content(show_col_types = FALSE) %>%
        dplyr::select(export_field_name)

    # Return
    cli::cli_h3("Status")
    cli::cli_alert_success("Generated field list")
    return(request)
}

#' Extract participant information and writes it to an easy to view table.
#'
#' @param partcipant_id int | Participant ID.
#'
#' @return tbl
#' @export
#'
#' @examples
#' # Generate table for participant 3030.
#' redcap_participant_info(3030)
redcap_export_participant_info <- function(partcipant_id) {

    # Check arguments
    checkmate::assert_int(partcipant_id)

    # Message
    cli::cli_h1("Generating participant information for participant {partcipant_id}")

    # Export data from RedCap
    df <- suppressMessages(redcap_export(fields = c("bmi", "weight", "whr",
                                 "waist", "hip", "systolic_bp_avg",
                                 "diastolic_bp_avg", "pdff_fat_prelim",
                                 "pdff_liver_cirle_mean",
                                 "pdff_pancreas_cirkle_mean",
                                 "total_body_fat"),
                        records = partcipant_id))

    # Convert variables
    df <- df %>%
        dplyr::mutate_at(dplyr::vars("bmi", "weight", "whr",
                              "waist", "hip", "systolic_bp_avg",
                              "diastolic_bp_avg", "pdff_fat_prelim",
                              "pdff_liver_cirle_mean",
                              "pdff_pancreas_cirkle_mean",
                              "total_body_fat"), .funs = as.numeric)


    # Table
    table <- df %>%
        dplyr::select(-c(start_date, group, participant_id)) %>%
        gt::gt(groupname_col = "visit") %>%
        gt::tab_header(
            title = gt::md(paste0("Information about participant **", partcipant_id, "**"))) %>%
        gt::cols_merge(
            columns = c(systolic_bp_avg, diastolic_bp_avg),
            pattern = "{1}/{2}") %>%
        gt::tab_spanner(
            label = "Anthropometrics",
            columns = c(weight, bmi, waist, hip, whr)) %>%
        gt::tab_spanner(
            label = "Liver fat",
            columns = c(pdff_fat_prelim, pdff_liver_cirle_mean)) %>%
        gt::tab_spanner(
            label = "Pancreatic fat",
            columns = c(pdff_pancreas_cirkle_mean)) %>%
        gt::fmt_number(
            columns = bmi,
            decimals = 1) %>%
        gt::fmt_number(
            columns = whr,
            decimals = 2) %>%
        gt::fmt_number(
            columns = weight,
            decimals = 1,
            pattern = "{x}kg") %>%
        gt::fmt_number(
            columns = c(waist, hip),
            decimals = 0,
            pattern = "{x}cm") %>%
        gt::fmt_number(
            columns = c(pdff_fat_prelim, pdff_liver_cirle_mean, pdff_pancreas_cirkle_mean, total_body_fat),
            decimals = 1,
            pattern = "{x}%") %>%
        gt::fmt_number(
            columns = c(systolic_bp_avg, diastolic_bp_avg),
            decimals = 0) %>%
        gt::cols_label(
            weight = "Weight",
            bmi = "BMI",
            waist = "Waist circ.",
            hip = "Hip circ.",
            whr = "WHR",
            systolic_bp_avg = "Blood pressure",
            pdff_fat_prelim = "Prelim.",
            pdff_liver_cirle_mean = "Circular ROI",
            pdff_pancreas_cirkle_mean = "Circular ROI",
            total_body_fat = "Total fat (DEXA)")

    # Return
    cli::cli_h3("Status")
    cli::cli_alert_success("Displaying participant info for {partcipant_id}")
    table
}


# 2. Import data to redcap ----------------------------------------------------------------

#' Imports data into RedCap.
#'
#' @param df tibble | Data frame to import into redcap. Supports data read using
#' * read_labka()
#' * read_ck18()
#' * read_dxa()
#' * read_p3np()
#' * read_homa()
#' * read_insulin()
#' * read_timp1()
#' * read_dxa()
#' @param redcap_uri str | URL to redcap API. Defaults to RN server.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Extra arguments.
#' @param import str | Selection of which data to import. Selections:
#' * labka
#' * ck18
#' * dxa
#' * p3np or piiinp
#' * homa
#' * insulin
#' * timp1
#' * lihep
#' @param overwrite str | Overwrite existing data on redcap or not. Selections:
#' * normal
#'
#' @export
#'
#' @examples
#' # Read PIIINP data
#' df_p3np <- read_p3np()
#' redcap_import(df_p3np, import = "p3np")
#'
#' # Read TIMP1 data
#' df_timp1 <- read_timp1()
#' redcap_import(df_timp1, import = "timp1")
#'
#' # Read LABKA data
#' df_labka <- read_labka(path = "~/Documents/1 - Data/Multisite-Labka/lakba-export_2022-09-13.xlsx",
#'                       identifier = T)
#' redcap_import(df_labka, import = "labka")
#'
#' # Read CK18
#' df_ck18 <- read_ck18()
#' redcap_import(df_ck18, import = "ck18")
#'
#' # Read insulin
#' df_insulin <- read_insulin()
#' redcap_import(df_insulin, import = "insulin")
redcap_import <- function(df,
                          import,
                          redcap_uri = "https://redcap.rn.dk/api/",
                          overwrite = 'normal', ...) {

    # Check arguments
    checkmate::assert_data_frame(df)
    checkmate::assert_choice(import, c("piiinp", "dxa", "dexa", "p3np", "labka", "timp1", "ck18", "insulin", "homa", "lihep"))
    checkmate::assert_character(redcap_uri, any.missing = FALSE, len = 1, pattern = "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$")
    checkmate::assert_choice(overwrite, choices = c("normal"))

    # Message
    cli::cli_h1("Importing {import} into RedCap.")

    # Collect dynamic dots (...)
    dots <- rlang::list2(...)

    # Import settings
    if(import == "p3np" | import == "piiinp") {
        combine_cols <- c("participant_id" = "participant_id",
                          "group" = "group",
                          "visit" = "visit")

        data_variables <- c("p3np")
    } else if(import == "ck18") {
        combine_cols <- c("participant_id" = "participant_id",
                          "visit" = "visit")

        data_variables <- c("ck18_absorbance_rep1", "ck18_absorbance_rep2", "ck18_concentration")
    } else if(import == "homa") {
        combine_cols <- c("participant_id" = "participant_id",
                          "visit" = "visit")

        data_variables <- c("homa2_b", "homa2_s", "homa2_ir")
    } else if(import == "insulin") {
        combine_cols <- c("participant_id" = "participant_id",
                          "visit" = "visit")

        data_variables <- c("c_peptid", "insulin")
    } else if(import == "timp1") {
        combine_cols <- c("participant_id" = "participant_id",
                          "visit" = "visit")

        data_variables <- c("timp1_absorbance_rep1", "timp1_absorbance_rep2", "timp1_concentration")
    } else if(import == "labka") {
        combine_cols <- c("cpr_number" = "cpr_number",
                          "start_date" = "start_date")

        data_variables <- c("Asat", "Dtot25", "Alat", "Alb", "AMYL-P", "Basp",
                            "Bili", "CRP", "Ftin", "Chol", "HDL-Chol", "LDL-Chol",
                            "Crea", "Ld", "Thyr-scr", "TSH", "Tt4", "Tgly-nf",
                            "eGFR2", "hegfr2", "Ggt", "Baso", "Eos", "Erc", "Evf",
                            "MCV", "eAG", "Hb", "HbA1ci", "MCHC", "MCH", "Lkc",
                            "Diff", "Lymf", "Im-gran", "Mono", "Neut", "Trc",
                            "AT", "D-dim", "Fib", "APTT", "KFinr", "Glc", "Ldlref-nf",
                            "Tt3", "LDL-Direkt")
    } else if(import == "lihep") {
        combine_cols <- c("participant_id" = "participant_id",
                          "visit" = "visit")

        data_variables <- c("asat", "alb", "amyl_p", "alat", "crp", "tsh")
    } else if(import %in% c("dxa", "dexa")) {
        combine_cols <- c("participant_id" = "participant_id",
                          "visit" = "visit")
        data_variables <- colnames(df %>% dplyr::select(-c(participant_id,
                                                           visit)))

    }

    # Get API token / match arguments
    if("api_token" %in% names(dots)) {
        api_token <- dots$api_token
    } else {
        api_token <- rstudioapi::askForPassword(prompt = "Please enter your RedCap API key")
    }

    # Export RedCap data
    redcap_df <- suppressMessages(redcap_export(identifier = TRUE,
                                                api_token = api_token))

    # Add group to data
    df <- redcap_df %>%
        dplyr::select(c(participant_id, group, visit)) %>%
        dplyr::right_join(df)

    # Combine RedCap and import data
    combined_df <- dplyr::left_join(redcap_df, df,
                                    by = c("group", combine_cols))

    # Remove not usable columns
    combined_df <- dplyr::semi_join(combined_df, df,
                                    by = c("participant_id", "visit"))

    # Clean data for import
    import_df <- combined_df %>%
        dplyr::rename(redcap_event_name = visit) %>%
        dplyr::mutate(redcap_event_name = dplyr::case_when(
            redcap_event_name == "baseline" & group == "control" ~ "examination__week_arm_1",
            redcap_event_name=="baseline" & group == "obese" ~ "examination__week_arm_2",
            redcap_event_name=="baseline" & group == "intervention" ~ "examination__week_arm_3",
            redcap_event_name=="month_1" ~ "examination__week_arm_3b",
            redcap_event_name=="month_5" ~ "examination__week_arm_3c",
            redcap_event_name=="year_1" ~ "follow_up__year_1_arm_3")) %>%
        dplyr::select(c(participant_id, redcap_event_name, data_variables)) %>%
        dplyr::rename_with(tolower) %>%
        dplyr::rename_with(~stringr::str_replace(.x, "-", "_"))

    # Generate complete request list
    form_data <- list(token = api_token,
                      content = 'record',
                      format = 'csv',
                      type = 'flat',
                      overwriteBehavior = overwrite,
                      data=readr::format_csv(import_df, na = "")
                  )

    # API request
    httr::set_config(httr::config(ssl_verifypeer = TRUE))
    request <- httr::POST(redcap_uri, body = form_data, encode = "form")

    # Return
    cli::cli_h3("Status")
    cli::cli_alert_success("Import of {import} data ended with {httr::http_status(request)$message}.")
}

# 3. Randomize data ----------------------------------------------------------------

#' Replaces data in RedCap df with random data.
#'
#' @param df tibble | Data frame that contains data exported from RedCap using
#' ```redcap_export()```.
#' @param remove vec | Vector of variables to be completely removed. Default: NULL
#'
#' @return
#' @export
#'
#' @examples
redcap_anonymize_data <- function(df, remove = NULL) {

    # Check arguments
    checkmate::assert_data_frame(df)

    # Message
    cli::cli_h1("Randomizing data points in {df}")

    # Remove potential variables
    if(!is.null(remove)) {
        df <- df %>%
            dplyr::select(-all_of(remove))
    }

    # Remove groups visits and groups with no data
    df %>%

    sapply(df$t1_liver_circle_mean, function(x)all(is.na(x)))

    # Anonymise all columns (except participant ID, group and visit)
    random_df <- df %>%
        dplyr::group_by(visit, group) %>%
        dplyr::mutate(
            dplyr::across(where(is.numeric) & !c(participant_id),
                          ~sample(x = min(.x, na.rm = T):max(.x, na.rm = T),
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
