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
#' @param x The independent or predictor variable in the data
#' @param add a vector containing ANOVA statistical result to add to the final data frame - `f-value` or/and `p-value` defaults to only `p-value`
#' @return dataframe containing the ANOVA result
#' @export
fastanova.test <- function(data, x, add = "p-value") {
  groups <- dplyr::group_vars(data)

  get_stats <- function(data, var) {
    formula <- stats::as.formula(sprintf("%s ~ %s", var, x))
    aov_res <- stats::anova(stats::aov(formula, data = data))

    if (is.nan(aov_res$`Pr(>F)`[[1]])) {
      p_val <- ".."
    } else {
      res <- c(sprintf("%.2f", aov_res$`F value`[[1]]), sprintf("%.2f", aov_res$`Pr(>F)`[[1]]))
      names(res) <- c("f-value", "p-value")

      if (length(add) > 1) {
        p_val <- paste0(paste(res[add], collapse = " ("), ")")
      } else {
        p_val <- res[[add]]
      }
    }
  }

  if (length(groups) > 0) {
    splitted_data <- split(data, as.list(data[, groups]))

    return(
      data |>
        as.data.frame() |>
        dplyr::select(-dplyr::any_of(c(groups, x))) |>
        colnames() |>
        lapply(function(var) {
          do.call(rbind, lapply(names(splitted_data), function(name) {
            p_val <- get_stats(splitted_data[[name]], var)

            as.list(unlist(strsplit(name, ".", fixed = TRUE))) |>
              append(list("...", p_val)) |>
              stats::setNames(c(groups, x,  var)) |>
              as.data.frame()
          }))
        }) |>
        Reduce(function(.x, .y) merge(.x, .y, by = c(groups, x)), x = _)
    )
  }

  return(
    data |>
      as.data.frame() |>
      dplyr::select(-dplyr::any_of(x)) |>
      colnames() |>
      lapply(function(var) {
        p_val <- get_stats(data[, c(var, x)], var)

        as.data.frame(stats::setNames(list("...", p_val), c(x, var)))
      })|>
      Reduce(function(.x, .y) merge(.x, .y, by = x), x = _)
  )
}
