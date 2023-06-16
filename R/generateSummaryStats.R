#' Generate summary statistic with ANOVA for a data set
#'
#' It generate summary statistics and attaches ANOVA p values to the table and also ranks the data set
#'
#' @import dplyr
#' @importFrom tibble as_tibble
#'
#' @param data The data frame containing variables to be analyzed
#' @param x The independent or predictor variable in the data
#' @param deviation_type The type of degree of spread - `s.e` or `sd` default to `s.e`
#' @param console_view print as plain text if set to ``TRUE` or markdown if set to `FALSE`
#' @param factor_vars The factor variables in the data - Optional when `grouping var` argument is specified
#' @param grouping_vars The grouping variables in the data if there are any - Optional
#' @return a List of summary tables for all groups
#' @export
fastsummary.stats <- function(data,
                          x,
                          deviation_type = "s.e",
                          grouping_vars = NA,
                          factor_vars = NA,
                          console_view = TRUE) {
  data <- dplyr::mutate(data,
                        dplyr::across(dplyr::all_of(grouping_vars), as.character))

  seperator <- Separator$new(
    data = data,
    x = x,
    grouping_vars = grouping_vars,
    deviation_type = deviation_type,
    console_view = console_view,
    factor_vars = factor_vars
  )

  tbls <- seperator$display_table() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(x)))

  splitting.data.var <- data[, grouping_vars]
  splitting.tbls.var <- tbls[, grouping_vars]

  if (length(grouping_vars) > 1) {
    splitting.data.var <- as.list(data[, grouping_vars])
    splitting.tbls.var <- as.list(tbls[, grouping_vars])
  }

  splitted_anova <- data |>
    dplyr::mutate(dplyr::across(.cols = -dplyr::any_of(c(grouping_vars, x)), ~ ifelse(is.na(.x), 0, .x))) |>
    split(splitting.data.var) |>
    sapply(function(df) {
      dplyr::select(df,-dplyr::any_of(grouping_vars)) |>
        fastanova.test(x)
    }, simplify = FALSE)


  splitted_tbls <- split(tbls, splitting.tbls.var)


  splitted_tbls |>
    names() |>
    sapply(function(name) {
      splitted_tbls[[name]] |>
        dplyr::select(-dplyr::any_of(grouping_vars)) |>
        dplyr::bind_rows(
          tibble::as_tibble(sapply(colnames(splitted_anova[[name]]), function(c) "...", simplify = F)),
          dplyr::mutate(splitted_anova[[name]], dplyr::across(.cols = dplyr::all_of(x), ~ format.label("p-value", console_view)))
        )
    }, simplify = FALSE)
}
