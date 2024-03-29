#' Format descriptive stats summary
#'
#' It finds the mean and standard error of a numeric vector
#' and formats then using ±.
#'
#' @importFrom stats sd
#'
#' @param x numeric vector to be summarized
#' @param spread deviation type
#' @return character vector
#' @export
get_summary <- function(x, spread = "s.e") {
  if (!spread %in% c("s.e", "sd")) {
    stop("spread must either be `sd` or `s.e`")
  }

  x <- x[!is.na(x)]

  if (spread == "s.e") {
    return(sprintf("%.2f \u00B1 %.2f", mean(x), stats::sd(x) / sqrt(length(x))))
  }

  return(sprintf("%.2f \u00B1 %.2f", mean(x), stats::sd(x)))
}
