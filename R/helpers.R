format.label <- function(label,
                         format = "plain",
                         type = "bold") {
  if (format == "plain") {
    return(paste0(" ", label))
  }

  html_list <- c(bold = "<strong>%s</strong>",
                 subscript = "<sup>%s</sup>")

  markdown <- c(bold = "**%s**",
                subscript = "^%s^")

  return(switch (
    format,
    html = sprintf(html_list[[type]], label),
    md = sprintf(markdown[[type]], label)
  ))
}

#' Helper function to help remove na's and null in a vector
#'
#' @param vec vector to be cleaned
#' @return vector
vec.na.rm <- function(vec) {
  vec[!is.na(vec) | !is.null(vec)]
}


merge_vars = function(data, grouping_vars, var, separator) {
  vars <- lapply(grouping_vars, function(.x) {
    as.character(data[[.x]])
  })

  ## variable vectors to be combined can be more that 1 sometimes
  ## so if they are more than 1 we need to the combination in a dataframe

  if (length(vars) > 1) {
    vars <- as.data.frame(vars) |>
      dplyr::rowwise() |>
      dplyr::transmute(code = paste(dplyr::c_across(cols = dplyr::everything()), collapse = separator))

    vars <- vars$code
  } else {
    ## but if the variable vector is less than 2
    ## we treat as normal
    vars <- unlist(vars)
  }

  ## merge functions coded vectors with `var` variable
  return(paste(vars, var, sep = separator))
}

#' A split function adapted for multiple split groups
#'
#' @param data data to be splited
#' @param x vector or list of data that will be used to split the data
#' @return list of splited data
custom_split <- function(data, x) {
  splitting_vars <- data[, x]

  if (length(x) > 1) {
    splitting_vars <- as.list(splitting_vars)
  }

  return(split(data, splitting_vars))
}
