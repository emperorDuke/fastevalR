#' Generate summary statistic with ANOVA for a data set
#'
#' It generate summary statistics and attaches ANOVA p values to the table and also ranks the data set
#'
#' @import dplyr
#' @importFrom tibble as_tibble
#' @import lodaR
#'
#' @param data The data frame containing variables to be analyzed
#' @param x The independent or predictor variable in the data
#' @param deviation_type The type of degree of spread - `s.e` or `sd` default to `s.e`
#' @param console_view print as plain text if set to `TRUE` or markdown if set to `FALSE`
#' @param factor_vars a vector factor variables in the data - Optional
#' @param grouping_vars a vector of grouping variables in the data - Optional
#' @param add a vector containing ANOVA statistical result to add to the final data frame - `f-value` or/and `p-value` defaults to only `p-value`
#' @return a List of summary tables for all groups if there are grouping and a
#' dataframe if there are no grouping variables
#' @export
fastsummary.stats <- function(data,
                          x,
                          deviation_type = "s.e",
                          grouping_vars = NA,
                          factor_vars = NA,
                          add = "p-value",
                          console_view = TRUE) {

  ## get statistic label
  get_label <- function() {
    if (length(add) > 1) {
      return(paste0(paste(add, collapse = " ("), ")"))
    }

    return(add)
  }

  ## insert necessary parameters and call fastanova.test function
  fastanova.test_wrapper <- function(.data) {
    params <- list(data = .data, x = x, add = add)

    do.call(fastanova.test, params)
  }

  ## separator wrapper function
  ##
  get_separator <- function() {
    Separator$new(
      data = data,
      x = x,
      grouping_vars = grouping_vars,
      deviation_type = deviation_type,
      console_view = console_view,
      factor_vars = factor_vars
    )
  }


  ## there is no grouping vars
  ##
  if (all(is.na(grouping_vars))) {
    seperator <- get_separator()

    tbl <- seperator$display_table() |>
      dplyr::arrange(dplyr::across(dplyr::all_of(x)))

    if (all(is.na(factor_vars))) {
      anova_res <- data |>
        dplyr::mutate(dplyr::across(-dplyr::any_of(x), ~ ifelse(is.na(.x), 0, .x))) |>
        fastanova.test_wrapper()
    } else {
      anova_res <- data |>
        dplyr::mutate(dplyr::across(-dplyr::any_of(c(factor_vars, x)), ~ ifelse(is.na(.x), 0, .x))) |>
        dplyr::select(-dplyr::any_of(factor_vars)) |>
        fastanova.test_wrapper()
    }

    # print(anova_res)
    #   month  age
    # 1   ... 0.24

    tbl |>
      dplyr::bind_rows(
        tibble::as_tibble(sapply(colnames(anova_res), function(c) "...", simplify = F)),
        dplyr::mutate(anova_res, dplyr::across(.cols = dplyr::all_of(x), ~ format.label(get_label(), console_view)))
      )
  } else {
    seperator <- get_separator()

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
        if (all(is.na(factor_vars))) {
          dplyr::select(df,-dplyr::any_of(grouping_vars)) |>
            fastanova.test_wrapper()
        } else {
          dplyr::select(df,-dplyr::any_of(c(grouping_vars, factor_vars))) |>
            fastanova.test_wrapper()
        }
      }, simplify = FALSE)

    # print(splitted_anova)
    #
    # $F.abuja
    #   month  age
    # 1   ... 0.24
    #
    # $M.abuja
    #   month  age
    # 1   ... 0.54


    splitted_tbls <- split(tbls, splitting.tbls.var)


    splitted_tbls |>
      names() |>
      sapply(function(name) {
        splitted_tbls[[name]] |>
          dplyr::select(-dplyr::any_of(grouping_vars)) |>
          dplyr::bind_rows(
            tibble::as_tibble(sapply(colnames(splitted_anova[[name]]), function(c) "...", simplify = F)),
            dplyr::mutate(splitted_anova[[name]], dplyr::across(.cols = dplyr::all_of(x), ~ format.label(get_label(), console_view)))
          )
      }, simplify = FALSE)
  }
}
