#'it is a pipe friendly version of the base R TukeyHSD function
#'
#'@importFrom stats aov
#'@importFrom stats setNames
#'@importFrom stats TukeyHSD
#'
#'@param data A data frame in which the variables specified in the formula will be found. If missing, the variables are searched for in the standard way.
#'@param formula A formula specifying the model
#'@returns a dataframe
#'@export
tukey.HSD <- function(data, formula) {
  grouping_vars <- dplyr::group_vars(data)

  get_splitted_data <- function(groups_vrs) {
    if (length(grouping_vars) > 1) {
      groups_vrs <- as.data.frame(apply(groups_vrs, 2, as.character))
      splitted_data <- data |> split(as.list(groups_vrs))
    } else {
      groups_vrs <- sapply(groups_vrs, as.character)
      splitted_data <- data |> split(groups_vrs)
    }
  }

  get_post_hoc_groups <- function(formula, data) {
    post_hoc <- stats::TukeyHSD(stats::aov(formula, data))

    indep_var <- names(post_hoc)
    post_hoc <- post_hoc[[indep_var]]

    groups <- do.call(rbind, lapply(row.names(post_hoc), function(name) {
      name |>
        strsplit("-", fixed = T) |>
        unlist() |>
        as.list() |>
        stats::setNames(c("group2", "group1")) |>
        as.data.frame()
    }))

    rownames(post_hoc) <- NULL

    return(cbind(groups, post_hoc))
  }

  if (length(grouping_vars) > 0) {
    groups_vrs <- data[, grouping_vars]
    splitted_data <- get_splitted_data(groups_vrs)

    results <- do.call(rbind, splitted_data |>
      names() |>
      lapply(function(name) {
        post_hoc_groups <- get_post_hoc_groups(formula, splitted_data[[name]])

        group_vars <- grouping_vars |>
          seq() |>
          lapply(function(i) {
            rep(unlist(strsplit(name, ".", fixed = T))[i], nrow(post_hoc_groups))
          }) |>
          stats::setNames(grouping_vars) |>
          as.data.frame()

        do.call(cbind, list(group_vars, post_hoc_groups))
      }))
  } else {
    results <- get_post_hoc_groups(formula, data)
  }

  colnames(results) <- lodaR::strip_white_spaces(colnames(results), replace_with = ".")

  return(results)
}
