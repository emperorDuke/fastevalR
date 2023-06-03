#' Format descriptive stats summary
#'
#' It finds the mean and standard error of a numeric vector and formats then using  +/-.
#'
#' @param x numeric vector to be summarized
#' @return character vector
#' @export
get_summary <- function(x) {
  x <- x[!is.na(x)]
  sprintf("%.2f \u00B1 %.2f", mean(x), stats::sd(x) / sqrt(length(x)))
}

