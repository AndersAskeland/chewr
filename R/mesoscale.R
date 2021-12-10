#' Reads MSR table (CSV) data into a tibble.
#'
#' @note For raw data, use '''read_msd_raw'''
#' @param path str | Path to csv file
#'
#' @return tibble
#' @export
#'
#' @examples
#' read_msd_table("data.csv")
msd_read_table <- function(path) {

    # Open file
    df <- readr::read_csv(file = path, skip = 1, col_types = "cccnnnnnnnnnnnnn", show_col_types = FALSE)

    # Return
    return(df)
}

#' Read msd raw data
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
msd_read_raw <- function(path) {
    pass
}
