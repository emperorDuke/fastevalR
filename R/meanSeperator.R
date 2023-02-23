#' Mean seperator using tukey HSD
#'
#' Is contains function that separate means and display them in a data frame
#'
#' @importFrom methods new
#' @importFrom methods setRefClass
#' @import purrr
#' @import dplyr
#' @import stringr
#' @import lodaR
#' @export
MeanSeperator <- methods::setRefClass(
  'MeanSeperator',
  fields = list(
    data = "data.frame",
    indep_var = "character",
    grouping_vars = "vector",
    code_seperator = 'character',
    factor_vars = "vector"
  ),
  methods = list(
    initialize = function(data,
                          indep_var,
                          factor_vars = NA,
                          grouping_vars = NA,
                          code_seperator = "@") {
      data <<- data
      indep_var <<- indep_var
      grouping_vars <<- grouping_vars
      code_seperator <<- code_seperator

      if (all(is.na(factor_vars)) && !all(is.na(grouping_vars))) {
        factor_vars <<- c(grouping_vars, indep_var)
      } else if (!all(is.na(factor_vars)) && !all(is.na(grouping_vars))) {
        factor_vars <<- c(factor_vars, grouping_vars, indep_var)
      } else {
        factor_vars <<- c(factor_vars, indep_var)
      }
    },
    .merge_vars = function(.self, data, mergee) {
      vars <- lapply(.self$grouping_vars, function(.x) {
        as.character(data[[.x]])
      })

      ## variable vectors to be combined can be more that 1 sometimes
      ## so if they are more than 1 we need to the combination in a dataframe

      if (length(vars) > 1) {
        vars <- as.data.frame(vars) |>
          dplyr::rowwise() |>
          dplyr::transmute(code = paste(dplyr::c_across(), collapse = .self$code_seperator))

        vars <- vars$code
      } else {
        ## but if the variable vector is less than 2
        ## we treat as normal
        vars <- unlist(vars)
      }

      ## merge functions coded vectors with `mergee` variable
      return(paste(vars, mergee, sep = .self$code_seperator))
    },
    .run_post_hoc = function(.self, var) {
      if (all(is.na(.self$grouping_vars))) {
        .self$data |>
          tukey.HSD(as.formula(sprintf("%s ~ %s", var, .self$indep_var))) |>
          dplyr::mutate(code = paste(group1, group2, sep = "-")) |>
          dplyr::select(p.adj, code)
      } else {
        tryCatch(
          .self$data |>
            dplyr::group_by(dplyr::across(dplyr::all_of(.self$grouping_vars))) |>
            tukey.HSD(as.formula(sprintf("%s ~ %s", var, .self$indep_var))) |>
            dplyr::arrange(dplyr::across(dplyr::all_of(.self$grouping_vars))) |>
            dplyr::mutate(combo1 = .self$.merge_vars(.data, group1)) |>
            dplyr::mutate(combo2 = .self$.merge_vars(.data, group2)) |>
            dplyr::mutate(code = paste(combo1, combo2, sep = "-")) |>
            dplyr::select(dplyr::all_of(.self$grouping_vars), p.adj, code),
          error = function(e) {
            .self$data |>
              dplyr::group_by(dplyr::across(dplyr::all_of(c(.self$grouping_vars, .self$indep_var)))) |>
              dplyr::mutate(p.adj = rep(NA, dplyr::n())) |>
              dplyr::mutate(combo1 = .self$.merge_vars(.data, .data[[.self$indep_var]])) |>
              dplyr::mutate(combo2 = .self$.merge_vars(.data, .data[[.self$indep_var]])) |>
              dplyr::mutate(code = paste(combo1, combo2, sep = "-")) |>
              dplyr::select(dplyr::all_of(c(.self$grouping_vars, .self$indep_var)), code, p.adj)
          }
        )
      }
    },
    .compute_letters = function(.self, post_hoc_tbl) {
      get_letters = function(df) {
        if (all(is.na(df$p.adj)) || all(is.nan(df$p.adj))) {
          p_val <- stats::setNames(rep(0, nrow(df)), df$code)
          .letters <- multcompView::multcompLetters(p_val)$Letters
          return(sapply(.letters, function(l) NA, simplify = F))
        } else {
          df <- with(df, df[!is.nan(p.adj) || !is.na(p.adj),])
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
            stats::setNames(c(.self$indep_var, "letters")) |>
            as.data.frame()
        })) |>
          dplyr::mutate(code = .data[[.self$indep_var]]))

      } else {
        splitting_vars <- post_hoc_tbl[, .self$grouping_vars]

        letters <- post_hoc_tbl |>
          split(as.list(splitting_vars)) |>
          lapply(get_letters) |>
          reduce(append)

        return(do.call(rbind, lapply(names(letters), function(name) {
          strsplit(name, .self$code_seperator, fixed = T) |>
            unlist() |>
            as.list() |>
            append(letters[[name]]) |>
            stats::setNames(c(.self$grouping_vars, .self$indep_var, "letters")) |>
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

      summary_vars <- c()
      selection_vars <- .self$factor_vars

      if (all(is.na(.self$grouping_vars))) {
        selection_vars <- .self$indep_var
        summary_vars <- .self$factor_vars
      }

      .self$data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(selection_vars))) |>
        dplyr::summarise(dplyr::across(.cols = -dplyr::any_of(summary_vars), .fns = get_summary), .groups = "drop") |>
        dplyr::select(dplyr::all_of(c(selection_vars, var))) |>
        na.omit() |>
        dplyr::mutate(code = get_code(.data)) |>
        dplyr::inner_join(letters_tbl, by = 'code') |>
        dplyr::select(dplyr::all_of(c(var, "letters")), dplyr::ends_with(".x")) |>
        dplyr::rename_with(~ ifelse(stringr::str_detect(.x, ".x"), lodaR::extract_chars(stringr::str_extract(.x, "^[a-z\\.]+(?=x)")), .x)) |>
        dplyr::mutate(
          mean = as.numeric(lodaR::extract_chars(.data[[var]], "^[\\d\\.]+(?=\\s)")),
          s.e = as.numeric(lodaR::extract_chars(.data[[var]], "(?<=\\s)[\\d\\.]+")),
          y.pt = mean + s.e
        ) |>
        dplyr::relocate(dplyr::any_of(selection_vars), .before = dplyr::all_of(var))
    },
    #' Groups results based on grouping vars
    #'
    #' groups the analyzed data and separation of mean analysis using the grouping var into a list
    #'
    #' @return a List
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
    #' Display summary statistics with mean of separation
    #'
    #' display human readable format of the separated mean
    #'
    #' @return  a Data frame
    display_table = function(.self) {
      seperated_means_list <- .self$separate()

      seperated_means_list |>
        names() |>
        lapply(function(var) {
          seperated_means_list[[var]] |>
            dplyr::mutate(dplyr::across(dplyr::all_of(var), ~ paste0(.data[[var]], .data[['letters']]))) |>
            dplyr::select(dplyr::all_of(c(.self$factor_vars, var)))
        }) |>
        purrr::reduce(~ merge(.x, .y, by = .self$factor_vars)) |>
        dplyr::rename_with(~ lodaR::capitalize(.x))
    }
  )
)
