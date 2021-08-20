#' Make a sorted frequeny table for afactor
#'
#' @param factor
#'
#' @return A tibble
#' @export
#'
#' @examples
#' fcount(iris$Species)
fcount <- function(x) {
    forcats::fct_count(x, sort = TRUE)
}
