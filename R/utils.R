#' Returns NULL if provided value is NA
#'
#' @param value str
#'
#' @return NULL or str
#'
#' @examples
#' vector <- c(1, 2, 3)
#'
#' null_if_na(vector[1])
#' null_if_na(vector[10])
null_if_na <- function(value) {
    if(is.na(value) || is.null(value)) {
        return(NULL)
    } else {
        return(value)
    }
}

#' Filters data on the basis of participants in the control and obese group should not have NAFLD.
#'
#' @param dat tibble | Data. I
#' @param token numeric | API token from redcap
#' @param arg tibble | Liver fat column to filter on. Is NULL if column is missing.
#'
#' @return tibble
#'
#' @examples
#' filter_nafld(dat = dat, token = "dasdsadas2239193219312", arg = "pdff_liver_cirle_mean")
filter_nafld <- function(dat, token, arg = NULL) {

    # Create copy of data so to not overwrite it if arg is NULL
    dat_copy <- dat

    # Check if liver fat is included in data. If not retrieve it
    if(is.null(arg)) {
        arg <- "pdff_liver_cirle_mean"
        dat_copy <- read_redcap(columns = arg, api_token = token)
    }

    # Find IDs to remove
    remove <- dat_copy %>%
        dplyr::filter(group == "control" | group == "obese") %>%
        dplyr::filter(eval(parse(text = arg)) >= 5)

    # Join
    dat <- dplyr::anti_join(dat, remove)

    # Return
    return(dat)
}

#' Checks if participant ID already exist inside given redcap arm.
#'
#' @param participant_id str
#' @param enrolment_arm str
#' @param url str
#' @param ...
#'
#' @return bool
#'
#' @examples
check_record <- function(participant_id, enrolment_arm, url="https://redcap.rn.dk/api/", ...) {

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
        'events[2]'=enrolment_arm,
        rawOrLabel='raw',
        rawOrLabelHeaders='raw',
        exportCheckboxLabel='false',
        exportSurveyFields='false',
        exportDataAccessGroups='false',
        returnFormat='csv',
        .opts = list(ssl.verifypeer = TRUE)
    )

    # Convert to tibble
    df <- readr::read_csv(export, show_col_types = FALSE)

    # Check if participant already exists in given arm
    if(nrow(df) > 0) { # Already contains data
        return(FALSE)
    } else { # No data
        return(TRUE)
    }
}

#' Extracts label names from list.
#'
#' @param args
#' @param ylab
#'
#' @return
#'
#' @examples
extract_labs <- function(args, ylab) {
    # Title
    if(!exists("title", where = args, inherits = FALSE)) {
        args$title <- ggplot2::waiver()
    }

    # Subtitle
    if(!exists("subtitle", where = args, inherits = FALSE)) {
        args$subtitle <- ggplot2::waiver()
    }

    # Y lab
    if(!exists("ylab", where = args, inherits = FALSE)) {
        args$ylab <- ylab
    }

    return(args)
}


extract_theme <- function(args) {
    # Scale
    if(!exists("scale", where = args, inherits = FALSE)) {
        args$scale <- 1
    } else if(args$scale == "poster") {
        args$scale <- 2
    }

    # Color
    if(!exists("color", where = args, inherits = FALSE)) {
        args$color <- "#2b8cbe"
    }

    return(args)
}



#' Renames groups
#'
#' @param df
#'
#' @return df
#'
#' @examples
rename_xlabs <- function(df) {
    # Rename groups
    df <- df %>%
        dplyr::mutate(
            group = dplyr::case_when(
                group == "control" ~ "Lean control",
                group == "obese" ~ "Obese without NAFLD",
                group == "intervention" ~ "Obese with NAFLD")) %>%
        dplyr::mutate(visit = dplyr::case_when(
                visit == "baseline" ~ "Baseline",
                visit == "month_1" ~ "During weight loss",
                visit == "month_5" ~ "After weight loss"
            ))

    return(df)
}
