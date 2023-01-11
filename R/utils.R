# 1 - General -------------------------------------------------------------

#' Format CPR number to have a "-".
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
cpr_number_formated <- function(df) {

    # Add - to cpr
    df <- df %>%
        dplyr::transmute(cpr_number = purrr::map_chr(cpr_number, ~ paste0(substr(.x, 1, 6),
                                                                          "-",
                                                                          substr(.x, 7, 10))))

    # Return
    df
}


# 2 - Save files ----------------------------------------------------------

#' Simple save svg.
#'
#' @param path str | File path
#' @param plot plot | Plot object to be saved
#'
#' @return
#' @export
#'
#' @examples
#' save_svg_wide("~/document/plot.png", plot)
save_svg_wide <- function(path, plot) {
    ggplot2::ggsave(filename = path, device = "svg", plot = plot, width = 26, height = 8.5, dpi = 300)
}



# Not exported ---------------------------------------------------------

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

#' Queries WHO API database and cleans data.
#'
#' @param query str | Data to query
#'
#' @return tibble
#'
#' @examples
#' who_query("obesity")
who_query <- function(query) {
    # Check query
    if(query == "obesity") {
        query_data <- "https://ghoapi.azureedge.net/api/NCD_BMI_30A"
        query_dim <- "https://ghoapi.azureedge.net/api/DIMENSION/COUNTRY/DimensionValues"
    }

    # Read data from WHO API
    request <- httr::GET(query_data)
    json_raw <- httr::content(x = request, as = "text")
    json_parsed <- jsonlite::fromJSON(json_raw)
    df <- dplyr::as_tibble(json_parsed[[2]])

    # Import country dim data (WHO data)
    request <- httr::GET(query_dim)
    json_raw <- httr::content(x = request, as = "text")
    json_parsed <- jsonlite::fromJSON(json_raw)
    df_country_dims <- dplyr::as_tibble(json_parsed[[2]]) %>%
        dplyr::select(Code, Title) %>%
        dplyr::rename(country_code = Code, country = Title)

    # Clean and combine data
    df_clean <- df %>%
        dplyr::rename(id = Id,
                      year = TimeDim,
                      sex = Dim1,
                      country_code = SpatialDim,
                      bmi_mean = NumericValue,
                      bmi_low = Low,
                      bmi_high = High) %>%
        dplyr::select(id, year, sex, country_code, bmi_mean, bmi_low, bmi_high) %>%
        dplyr::mutate(sex = dplyr::case_when(
            sex == "FMLE" ~ "Female",
            sex == "MLE" ~ "Male",
            sex == "BTSX" ~ "Both sexes")) %>%
        dplyr::left_join(y = df_country_dims) %>%
        dplyr::mutate(country = dplyr::case_when(
            country == "United States of America" ~ "USA",
            country == "Russian Federation" ~ "Russia",
            country == "United Kingdom of Great Britain and Northern Ireland" ~ "UK",
            country == "United Republic of Tanzania" ~ "Tanzania",
            country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
            country == "Viet Nam" ~ "Vietnam",
            country == "Yemen Arab Republic (until 1990)" ~ "Yemen",
            country == "Syrian Arab Republic" ~ "Syria",
            country == "Sudan (until 2011)" ~ "Sudan",
            country == "Republic of Korea" ~ "North Korea",
            country == "Lao People's Democratic Republic" ~ "Laos",
            country == "Kiribati (until 1984)" ~ "Kiribati",
            country == "Democratic Republic of the Congo" ~ "Congo",
            country == "Democratic People's Republic of Korea" ~ "South Korea",
            country == "Bolivia (Plurinational State of)" ~ "Bolivia",
            country == "Iran (Islamic Republic of)" ~ "Iran",
            country == "Sudan (former)" ~ "Sudan",
            country == "Germany, Federal Republic (former)" ~ "Germany",
            country == "Congo" ~ "Democratic Republic of the Congo",
            country == "Côte d'Ivoire" ~ "Ivory Coast",
            country == "Czechia" ~ "Czech Republic",
            TRUE ~ country)) %>%
        dplyr::filter(sex == "Both sexes") %>%
        dplyr::group_by(country) %>%
        dplyr::filter(!is.na(bmi_mean) | !is.na(country) | !is.na(year)) %>%
        dplyr::arrange(dplyr::desc(year), .by_group = TRUE) %>%
        dplyr::ungroup()

    # Return
    df_clean
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

    # Message
    print("Removing controls and obese participants that have NAFLD (>5% liver fat)")

    # Create copy of data so to not overwrite it if arg is NULL
    dat_copy <- dat

    # Check if liver fat is included in data. If not retrieve it
    if(is.null(arg)) {
        arg <- "pdff_liver_cirle_mean"
        dat_copy <- redcap_read(columns = arg, api_token = token)
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
#' check_record("3030", "enrolment_arm_1")
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
#' @param args ... |
#' @param ylab
#'
#' @return
#'
#' @examples
#' labels <- extract_labs(args, ylab = comparison)
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


#' Extracts theme arguments
#'
#' @param args ...
#'
#' @return
#' @export
#'
#' @examples
#' theme <- extract_theme(args)
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
#' rename_xlabs(df)
rename_xlabs <- function(df) {
    # Rename groups
    df <- df %>%
        dplyr::mutate(
            group = dplyr::case_when(
                group == "control" ~ "Lean control",
                group == "obese" ~ "Obese w/o NAFLD",
                group == "intervention" ~ "Obese with NAFLD")) %>%
        dplyr::mutate(visit = dplyr::case_when(
                visit == "baseline" ~ "Baseline",
                visit == "month_1" ~ "During weight loss",
                visit == "month_5" ~ "After weight loss"
            ))

    return(df)
}

#' Custom fill function. Will only fill if there only exists data in
#' enrollment arm 1.
#'
#' @param column
#' @param data
#'
#' @return
#'
#' @examples
custom_fill <- function(data_point, column, data){

    # Verify no NAs
    is_na <- request %>%
        dplyr::filter(redcap_event_name == "enrolment_arm_1") %>%
        dplyr::select(column) %>%
        purrr::map(~any(!is.na(.x))) %>%
        unlist()

    # Fill data
    filled_data <- data %>%
        tidyr::fill(column) %>%
        dplyr::select(column) %>%
        unlist()

    print(is_na)
    # Check
    if(is_na) {
        return(filled_data)
    } else {
        return(data_point)
    }

}



