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
        dat_copy <- read_redcap(token = token, columns = arg)
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

