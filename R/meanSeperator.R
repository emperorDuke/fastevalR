#' Mean seperator using tukey HSD
#'
#' Is contains function that separate means and display them in a data frame
#'
#' @importFrom methods new
#' @importFrom methods setRefClass
#' @importFrom stats setNames
#' @import purrr
#' @import stringr
#' @import lodaR
#' @import dplyr
#' @importFrom multcompView multcompLetters
#'
#' @param data The data frame containing variables to be analyzed
#' @param indep_var The independent or predictor variable in the data
#' @param factor_var The factor variables in the data - Optional when `grouping var` argument is specified
#' @param grouping_var The grouping variables in the data if there are any - Optional
#' @param deviation_type The type of degree of spread - `s.e` or `sd` default to `s.e`
#' @param code_seperator The internal code separator defaults to `@` - Optional
#' @export
Separator <- methods::setRefClass(
  'Separator',
  fields = list(
    data = "data.frame",
    indep_var = "character",
    grouping_vars = "vector",
    deviation_type = "character",
    code_seperator = 'character',
    factor_vars = "vector",
    .letter.name = "character"
  ),
  methods = list(
    initialize = function(data,
                          indep_var,
                          factor_vars = NA,
                          grouping_vars = NA,
                          deviation_type = "s.e",
                          code_seperator = "@") {
      data <<- as.data.frame(data)
      indep_var <<- indep_var
      grouping_vars <<- grouping_vars
      code_seperator <<- code_seperator
      deviation_type <<- deviation_type
      .letter.name <<- "letters"

      if (all(is.na(factor_vars)) && !all(is.na(grouping_vars))) {
        factor_vars <<- c(grouping_vars, indep_var)
      } else if (!all(is.na(factor_vars)) && !all(is.na(grouping_vars))) {
        factor_vars <<- unique(c(factor_vars, grouping_vars, indep_var))
      } else {
        factor_vars <<- c(factor_vars, indep_var)
      }
    },
    .merge_vars = function(.self, data, var) {
      vars <- lapply(.self$grouping_vars, function(.x) {
        as.character(data[[.x]])
      })

      ## variable vectors to be combined can be more that 1 sometimes
      ## so if they are more than 1 we need to the combination in a dataframe

      if (length(vars) > 1) {
        vars <- as.data.frame(vars) |>
          dplyr::rowwise() |>
          dplyr::transmute(code = paste(dplyr::c_across(cols = everything()), collapse = .self$code_seperator))

        vars <- vars$code
      } else {
        ## but if the variable vector is less than 2
        ## we treat as normal
        vars <- unlist(vars)
      }

      ## merge functions coded vectors with `var` variable
      return(paste(vars, var, sep = .self$code_seperator))
    },
    .run_post_hoc = function(.self, var) {
      if (all(is.na(.self$grouping_vars))) {
        .self$data |>
          tukey.HSD(as.formula(sprintf("%s ~ %s", var, .self$indep_var))) |>
          dplyr::mutate(code = paste(group1, group2, sep = "-")) |>
          dplyr::select(p.adj, code)
      } else {
        .self$data |>
          dplyr::mutate(dplyr::across(.cols = -dplyr::any_of(c(.self$grouping_vars, .self$indep_var)), ~ ifelse(is.na(.x), 0, .x))) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(.self$grouping_vars))) |>
          tukey.HSD(as.formula(sprintf("%s ~ %s", var, .self$indep_var))) |>
          dplyr::arrange(dplyr::across(dplyr::all_of(.self$grouping_vars))) |>
          dplyr::mutate(combo1 = .self$.merge_vars(.data, group1)) |>
          dplyr::mutate(combo2 = .self$.merge_vars(.data, group2)) |>
          dplyr::mutate(code = paste(combo1, combo2, sep = "-")) |>
          dplyr::select(dplyr::all_of(.self$grouping_vars), p.adj, code)
      }
    },
    .compute_letters = function(.self, post_hoc_tbl) {
      get_letters = function(df) {
        if (all(is.na(df$p.adj)) || all(is.nan(df$p.adj))) {
          codes <- lapply(df$code, function(c) unlist(strsplit(c, "-", fixed = T)))
          codes <- unique(unlist(codes))

          return(rep(NA, length(codes)) |> stats::setNames(codes))
        } else {
          if (any(is.na(df$p.adj)) || any(is.nan(df$p.adj))) {
            df <- df[!df$p.adj %in% c(NA, NaN), ]
          }

          p_val <- stats::setNames(df$p.adj, df$code)
          compare <- multcompView::multcompLetters(p_val)

          return(compare$Letters)
        }
      }

      if (all(is.na(.self$grouping_vars))) {
        letters <- get_letters(post_hoc_tbl)

        return(do.call(rbind, lapply(names(letters), function(name) {
          list(name) |>
            append(letters[[name]]) |>
            stats::setNames(c(.self$indep_var, .self$.letter.name)) |>
            as.data.frame()
        })) |>
          dplyr::mutate(code = .data[[.self$indep_var]]))

      } else {
        splitting_vars <- post_hoc_tbl[, .self$grouping_vars]

        if (length(.self$grouping_vars) > 1) {
          splitting_vars <- as.list(splitting_vars)
        }

        letters <- post_hoc_tbl |>
          split(splitting_vars) |>
          lapply(get_letters) |>
          purrr::reduce(append)

        return(do.call(rbind, lapply(names(letters), function(name) {
          strsplit(name, .self$code_seperator, fixed = T) |>
            unlist() |>
            as.list() |>
            append(letters[[name]]) |>
            stats::setNames(c(.self$grouping_vars, .self$indep_var, .self$.letter.name)) |>
            as.data.frame()
        })) |>
          dplyr::mutate(code = .self$.merge_vars(.data, .data[[.self$indep_var]])))
      }
    },
    .attach_descriptive_stats = function(.self, letters_tbl, var) {
      get_code <- function(data) {
        if (all(is.na(.self$grouping_vars))) {
          return(as.character(data[[.self$indep_var]]))
        }

        return(.self$.merge_vars(data, data[[.self$indep_var]]))
      }

      summary_vars <- .self$factor_vars
      selection_vars <- c(.self$grouping_vars, .self$indep_var)

      if (all(is.na(.self$grouping_vars))) {
        selection_vars <- .self$indep_var
      }

      .self$data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(selection_vars))) |>
        dplyr::summarise(dplyr::across(.cols = -dplyr::any_of(summary_vars), .fns = ~ get_summary(.x, .self$deviation_type)), .groups = "drop") |>
        dplyr::select(dplyr::all_of(c(selection_vars, var))) |>
        dplyr::mutate(dplyr::across(.cols = -dplyr::any_of(summary_vars), .fns = ~ stringr::str_replace_all(.x, "NA|NaN", "0.00"))) |>
        dplyr::mutate(code = get_code(.data)) |>
        dplyr::inner_join(letters_tbl, by = 'code') |>
        dplyr::select(dplyr::all_of(c(var, .self$.letter.name)), dplyr::ends_with(".x")) |>
        dplyr::rename_with(~ ifelse(stringr::str_detect(.x, ".x"), lodaR::extract_chars(stringr::str_extract(.x, "^[a-z\\.]+(?=x)")), .x)) |>
        dplyr::mutate(
          mean = lodaR::extract_chars(.data[[var]], "^[\\d\\.\\-]+(?=\\s)"),
          s.e = lodaR::extract_chars(.data[[var]], "(?<=\\s)[\\d\\.\\-]+"),
          mean = purrr::map_dbl(mean, ~ ifelse(stringr::str_detect(.x, "^[\\d]+"), as.numeric(.x), NA)),
          s.e = purrr::map_dbl(s.e, ~ ifelse(stringr::str_detect(.x, "^[\\d]+"), as.numeric(.x), NA)),
          y.pt = mean + s.e
        ) |>
        dplyr::relocate(dplyr::any_of(selection_vars), .before = dplyr::all_of(var))
    },
    separate = function(.self) {
      .self$data |>
        dplyr::select(-dplyr::any_of(.self$factor_vars)) |>
        colnames() |>
        sapply(function(var) {
          .self$.run_post_hoc(var) |>
            .self$.compute_letters() |>
            .self$.attach_descriptive_stats(var)
        }, simplify = FALSE)
    },
    display_table = function(.self) {
      selection_vars <- c(.self$grouping_vars, .self$indep_var)

      if (all(is.na(.self$grouping_vars))) {
        selection_vars <- .self$indep_var
      }

      insert_stats <- function(data, var) {
        letters <- data[[.self$.letter.name]]
        var_data <- data[[var]]

        if (any(is.na(letters))) {
          return(sapply(seq(letters), function(i) {
            if (!is.na(letters[i])) {
              return(paste0(var_data[i], "^", letters[i], "^"))
            }

            return(var_data[i])
          }))
        }

        return(paste0(data[[var]], "^", data[[.self$.letter.name]], "^"))
      }

      seperated_means_list <- .self$separate()
      seperated_means_list |>
        names() |>
        lapply(function(var) {
          seperated_means_list[[var]] |>
            dplyr::mutate(dplyr::across(dplyr::all_of(var), ~ insert_stats(.data, var))) |>
            dplyr::select(dplyr::any_of(c(selection_vars, var)))
        }) |>
        purrr::reduce( ~ merge(.x, .y, by = selection_vars))
    }
  )
)

