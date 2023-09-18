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
#' @param add a vector containing ANOVA statistical result to add to the final data frame - `f-value` or/and `p-value` defaults to only `p-value` # nolint
#' @return dataframe containing the ANOVA result
#' @export
fastanova_test <- function(data, x, add = "p-value") {
  groups <- dplyr::group_vars(data)

  get_stats <- function(data, var) {
    formula <- stats::as.formula(sprintf("%s ~ %s", var, x))
    aov_res <- stats::anova(stats::aov(formula, data = data))

    if (is.nan(aov_res$`Pr(>F)`[[1]])) {
      p_val <- ".."
    } else {
      res <- c(
        sprintf("%.2f", aov_res$`F value`[[1]]),
        sprintf("%.2f", aov_res$`Pr(>F)`[[1]])
      )
      names(res) <- c("f-value", "p-value")

      if (length(add) > 1) {
        p_val <- paste0(paste(res[add], collapse = " ("), ")")
      } else {
        p_val <- res[[add]]
      }
    }

    return(p_val)
  }

  if (length(groups) > 0) {
    splitted_data <- split(data, as.list(data[, groups]))

    data_vars <- colnames(data[, -which(colnames(data) %in% c(groups, x))])

    re_data <- lapply(data_vars, function(var) {
      do.call(rbind, lapply(names(splitted_data), function(name) {
        p_val <- get_stats(splitted_data[[name]], var)

        data_vars_vec <- unlist(strsplit(name, ".", fixed = TRUE))
        data_vars_vec <- c(data_vars_vec, "...", p_val)

        names(data_vars_vec) <- c(groups, x, var)

        return(as.data.frame(as.list(data_vars_vec)))
      }))
    })

    merged_data <- Reduce(function(.x, .y) {
      merge(.x, .y, by = c(groups, x))
    }, x = re_data)

    return(merged_data)
  }

  return(
    data |>
      as.data.frame() |>
      dplyr::select(-dplyr::any_of(x)) |>
      colnames() |>
      lapply(function(var) {
        p_val <- get_stats(data[, c(var, x)], var)

        as.data.frame(stats::setNames(list("...", p_val), c(x, var)))
      }) |>
      Reduce(function(.x, .y) merge(.x, .y, by = x), x = _)
  )
}