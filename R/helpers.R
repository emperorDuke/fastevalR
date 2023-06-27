format.label <- function(label, console_view, type = "bold") {
  if (console_view) {
    return(label)
  }

  return(switch(
    type,
    bold = sprintf("**%s**", label),
    subscript = sprintf("^%s^", label)
  ))
}


vec.na.rm <- function(vec) {
  vec[!is.na(vec)]
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
