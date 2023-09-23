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
#' @param include a vector containing ANOVA statistical result to include to the final data frame - `f-value` or/and `p-value` defaults to only `p-value` # nolint
#' @param transform_func a callback that can be to transorm each dependent var in the dataset # nolint
#' @return dataframe containing the ANOVA result
#' @export
fastanova_test <- function(
  data,
  x,
  include = "p-value",
  transform_func = NULL
  ) {
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

      if (length(include) > 1) {
        p_val <- paste0(paste(res[include], collapse = " ("), ")")
      } else {
        p_val <- res[[include]]
      }
    }

    return(p_val)
  }

  if (length(groups) > 0) {
    data_vars <- colnames(data[, -which(colnames(data) %in% c(groups, x))])

    re_data <- lapply(data_vars, function(var) {

      if (!is.null(transform_func)) {
        data <- transform_func(data[, c(groups, x, var)])
      }

      splitted_data <- split(data, as.list(data[, groups]))

      do.call(rbind, lapply(names(splitted_data), function(name) {
        p_val <- get_stats(splitted_data[[name]], var)

        data_vars_vec <- unlist(strsplit(name, ".", fixed = TRUE))
        data_vars_vec <- c(data_vars_vec, "...", p_val)

        names(data_vars_vec) <- c(groups, x, var)

        return(as.data.frame(as.list(data_vars_vec)))
      }))
    })

    merged_data <- Reduce(
      function(a, b) merge(a, b, by = c(groups, x)),
      x = re_data
    )

    return(merged_data)
  }

  return(
    data |>
      as.data.frame() |>
      dplyr::select(-dplyr::any_of(x)) |>
      colnames() |>
      lapply(function(var) {
        if (!is.null(transform_func)) {
          data <- transform_func(data[, c(groups, x, var)])
        }

        p_val <- get_stats(data[, c(var, x)], var)

        as.data.frame(stats::setNames(list("...", p_val), c(x, var)))
      }) |>
      Reduce(function(.x, .y) merge(.x, .y, by = x), x = _)
  )
}