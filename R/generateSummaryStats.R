#' Generate summary statistic with ANOVA for a data set
#'
#' It generate summary statistics and attaches ANOVA p values to the table and also ranks the data set
#'
#' @import dplyr
#' @import tibble
#'
#' @param data The data frame containing variables to be analyzed
#' @param indep_var The independent or predictor variable in the data
#' @param factor_var The factor variables in the data - Optional when `grouping var` argument is specified
#' @param grouping_var The grouping variables in the data if there are any - Optional
#' @return a List of summary tables for all groups
#' @export
generate_summary_stats <- function(data, indep_var, grouping_vars = NA, factor_var = NA) {
  if (all(is.na(grouping_vars))) {
    seperator <- Separator$new(data = data,
                               grouping_vars = grouping_vars,
                               indep_var = indep_var,
                               factor_var = factor_var)

    tbls <- seperator$display_table() |>
      dplyr::rename_with(tolower) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(indep_var)))
  }

  data <- dplyr::mutate(data,
                        dplyr::across(dplyr::all_of(grouping_vars), as.character))

  seperator <- Separator$new(data = data,
                             grouping_vars = grouping_vars,
                             indep_var = indep_var,
                             factor_var = factor_var)

  tbls <- seperator$display_table() |>
    dplyr::rename_with(tolower) |>
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
        get_anova(indep_var)
    }, simplify = FALSE)


  splitted_tbls <- split(tbls, splitting.tbls.var)

  splitted_tbls |>
    names() |>
    sapply(function(name) {
      splitted_tbls[[name]] |>
        dplyr::select(-dplyr::any_of(grouping_vars)) |>
        dplyr::bind_rows(
          tibble::as_tibble(sapply(1:ncol(splitted_anova[[name]]), function(i) "...", simplify = F)),
          dplyr::mutate(splitted_anova[[name]], dplyr::across(.cols = dplyr::all_of(indep_var), ~ "**p-value**")
        ))
    }, simplify = FALSE)
}
