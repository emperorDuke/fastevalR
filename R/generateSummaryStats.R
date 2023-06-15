#' Generate summary statistic with ANOVA for a data set
#'
#' It generate summary statistics and attaches ANOVA p values to the table and also ranks the data set
#'
#' @import dplyr
#' @importFrom tibble as_tibble
#'
#' @param data The data frame containing variables to be analyzed
#' @param indep_var The independent or predictor variable in the data
#' @param deviation_type The type of degree of spread - `s.e` or `sd` default to `s.e`
#' @param factor_vars The factor variables in the data - Optional when `grouping var` argument is specified
#' @param grouping_vars The grouping variables in the data if there are any - Optional
#' @return a List of summary tables for all groups
#' @export
generate_summary_stats <- function(data,
                                   indep_var,
                                   deviation_type = "s.e",
                                   grouping_vars = NA,
                                   factor_vars = NA) {
  data <- dplyr::mutate(data,
                        dplyr::across(dplyr::all_of(grouping_vars), as.character))

  seperator <- Separator$new(data = data,
                             grouping_vars = grouping_vars,
                             indep_var = indep_var,
                             deviation_type = deviation_type,
                             factor_vars = factor_vars)

  tbls <- seperator$display_table() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(indep_var)))

  splitting.data.var <- data[, grouping_vars]
  splitting.tbls.var <- tbls[, grouping_vars]

  if (length(grouping_vars) > 1) {
    splitting.data.var <- as.list(data[, grouping_vars])
    splitting.tbls.var <- as.list(tbls[, grouping_vars])
  }

  splitted_anova <- data |>
    dplyr::mutate(dplyr::across(.cols = -dplyr::any_of(c(grouping_vars, indep_var)), ~ ifelse(is.na(.x), 0, .x))) |>
    split(splitting.data.var) |>
    sapply(function(df) {
      dplyr::select(df, -dplyr::any_of(grouping_vars)) |>
        anova_test(indep_var)
    }, simplify = FALSE)


  splitted_tbls <- split(tbls, splitting.tbls.var)


  splitted_tbls |>
    names() |>
    sapply(function(name) {
      splitted_tbls[[name]] |>
        dplyr::select(-dplyr::any_of(grouping_vars)) |>
        dplyr::bind_rows(
          tibble::as_tibble(sapply(colnames(splitted_anova[[name]]), function(i) "...", simplify = F)),
          dplyr::mutate(splitted_anova[[name]], dplyr::across(.cols = dplyr::all_of(indep_var), ~ "**p-value**")
        ))
    }, simplify = FALSE)
}
