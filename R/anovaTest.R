#' A pipe friendly wrapper for one way ANOVA
#'
#' It carries out one way ANOVA and is pipe friendly
#'
#' @import dplyr
#' @importFrom stats aov
#' @importFrom stats as.formula
#' @importFrom stats setNames
#' @importFrom stats anova
#'
#' @param data The data frame containing variables to be analyzed
#' @param indep_var The independent or predictor variable in the data
#' @return dataframe containing the ANOVA result
#' @export
fastanova.test <- function(data, indep_var) {
  groups <- dplyr::group_vars(data)

  get_p_value <- function(data, var) {
    formula <- stats::as.formula(sprintf("%s ~ %s", var, indep_var))
    aov_res <- stats::anova(stats::aov(formula, data = data))

    if (is.nan(aov_res$`Pr(>F)`[[1]])) {
      p_val <- ".."
    } else {
      p_val <- sprintf("%.2f", aov_res$`Pr(>F)`[[1]])
    }
  }

  if (length(groups) > 0) {
    splitted_data <- split(data, as.list(data[, groups]))

    return(
      data |>
        as.data.frame() |>
        dplyr::select(-dplyr::any_of(c(groups, indep_var))) |>
        colnames() |>
        lapply(function(var) {
          do.call(rbind, lapply(names(splitted_data), function(name) {
            p_val <- get_p_value(splitted_data[[name]], var)

            as.list(unlist(strsplit(name, ".", fixed = T))) |>
              append(list("...", p_val)) |>
              stats::setNames(c(groups, indep_var,  var)) |>
              as.data.frame()
          }))
        }) |>
        Reduce(function(.x, .y) merge(.x, .y, by = c(groups, indep_var)), x = _)
    )
  }

  return(
    data |>
      as.data.frame() |>
      dplyr::select(-dplyr::any_of(indep_var)) |>
      colnames() |>
      lapply(function(var) {
        p_val <- get_p_value(data[, c(var, indep_var)], var)

        as.data.frame(stats::setNames(list("...", p_val), c(indep_var, var)))
      })|>
      Reduce(function(.x, .y) merge(.x, .y, by = indep_var), x = _)
  )
}
