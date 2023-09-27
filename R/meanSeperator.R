#' Mean seperator using tukey HSD
#'
#' Is contains function that separate means and display them in a data frame
#'
#' @importFrom methods new
#' @importFrom stats setNames
#' @importFrom stats as.formula
#' @import stringr
#' @import lodaR
#' @import dplyr
#' @import R6
#' @importFrom multcompView multcompLetters
#' @importFrom multcompView vec2mat
#'
#' @field data The data frame containing variables to be analyzed
#' @field x The independent or predictor variable in the data
#' @field factor_vars The factor variables in the data - Optional when `grouping var` argument is specified # nolint
#' @field grouping_vars The grouping variables in the data if there are any - Optional # nolint
#' @field deviation_type The type of degree of spread - `s.e` or `sd` default to `s.e` # nolint
#' @field code_seperator The internal code separator defaults to `@` - Optional
#' @field letter.name The column name for the separation of mean letters
#' @field decreasing The order at which the alphabet are ranked - defaults to `FALSE` # nolint
#' @field format The representation of the summary data in the dataframe - defaults to `plain` - options are `plain`, `md`, and `html` # nolint
#' @field include a vector containing ANOVA statistical result to add to the final data frame - `f-value` or/and `p-value` defaults to only `p-value` # nolint
#' @field transform_func a callback that can be to transorm each dependent var in the dataset # nolint
#' @export
# @field ANOVA_result cache the result of the `compute_ANOVA` function
# @field results The analysed results for all the groups
# @field table_display cache the result of the table_display function
Separator <- R6::R6Class(
  "Separator",
  private = list(
    table_display = NULL,
    ANOVA_result = NULL,
    results = NULL,
    functions_args = list(),
    # transforms the data for every var
    #
    # @param var varible pf interest
    transform_var = function(var) {
      if (!is.null(self$transform_func)) {
        transform_data <- self$transform_func(
          self$data[, c(self$factor_vars, self$x, var)]
        )

        return(transform_data)
      } else {
        return(self$data)
      }
    },
    # get codes for the data sets
    #
    # @param data dataframe
    get_code = function(data) {
      if (is.null(self$grouping_vars)) {
        return(as.character(data[[self$x]]))
      }

      return(merge_vars(
        data,
        self$grouping_vars,
        data[[self$x]],
        self$code_seperator
      ))
    },
    # Run post-hoc for a parameter in the data set
    #
    # @param param column in dataframe
    run_post_hoc = function(transformed_data, param) {
      if (is.null(self$grouping_vars)) {
        transformed_data |>
          tukey_hsd(stats::as.formula(sprintf("%s ~ %s", param, self$x))) |>
          dplyr::mutate(code = paste(group1, group2, sep = "-")) |>
          dplyr::select(p.adj, code)
      } else {
        transformed_data |>
          dplyr::mutate(dplyr::across(.cols = -dplyr::any_of(c(self$grouping_vars, self$x)), ~ ifelse(is.na(.x), 0, .x))) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(self$grouping_vars))) |>
          tukey_hsd(stats::as.formula(sprintf("%s ~ %s", param, self$x))) |>
          dplyr::arrange(dplyr::across(dplyr::all_of(self$grouping_vars))) |>
          dplyr::mutate(combo1 = merge_vars(.data, self$grouping_vars, group1, self$code_seperator)) |>
          dplyr::mutate(combo2 = merge_vars(.data, self$grouping_vars, group2, self$code_seperator)) |>
          dplyr::mutate(code = paste(combo1, combo2, sep = "-")) |>
          dplyr::select(dplyr::all_of(self$grouping_vars), p.adj, code)
      }
    },
    # Compute letters
    #
    # @param post_hoc_tbl post-hoc table
    # @param param column name of interest
    compute_letters = function(post_hoc_tbl, transformed_data, param) {
      get_letters <- function(df, raw_data) {
        if (all(is.na(df$p.adj)) || all(is.nan(df$p.adj))) {
          codes <- lapply(df$code, function(c) {
            unlist(strsplit(c, "-", fixed = TRUE))
          })
          codes <- unique(unlist(codes))

          return(rep(NA, length(codes)) |> stats::setNames(codes))
        } else {
          raw_data$code <- private$get_code(raw_data)

          mean_group <- tapply(
            raw_data[, param],
            raw_data$code,
            mean,
            na.rm = TRUE
          )
          ordered_mean <- order(mean_group, decreasing = self$decreasing)

          lvls <- names(mean_group)[ordered_mean]

          if (any(is.na(df$p.adj)) || any(is.nan(df$p.adj))) {
            df <- df[!df$p.adj %in% c(NA, NaN), ]
          }

          value <- multcompView::vec2mat(stats::setNames(df$p.adj, df$code))
          value <- value[lvls, lvls]

          compare <- multcompView::multcompLetters(value)
          return(compare$Letters)
        }
      }

      if (is.null(self$grouping_vars)) {
        letters <- get_letters(post_hoc_tbl, transformed_data)

        return(do.call(rbind, lapply(names(letters), function(name) {
          list(name) |>
            append(letters[[name]]) |>
            stats::setNames(c(self$x, self$letter.name)) |>
            as.data.frame()
        })) |>
          dplyr::mutate(code = .data[[self$x]]))
      } else {
        splited_tbls <- custom_split(post_hoc_tbl, self$grouping_vars)
        splited_raw_tbls <- custom_split(transformed_data, self$grouping_vars)

        ## verify if the names are the sames
        stopifnot(all(names(splited_raw_tbls) %in% names(splited_tbls)))

        letters <- lapply(names(splited_tbls), function(name) {
          get_letters(splited_tbls[[name]], splited_raw_tbls[[name]])
        })

        letters <- Reduce(append, x = letters)

        return(do.call(rbind, lapply(names(letters), function(name) {
          strsplit(name, self$code_seperator, fixed = TRUE) |>
            unlist() |>
            as.list() |>
            append(letters[[name]]) |>
            stats::setNames(c(self$grouping_vars, self$x, self$letter.name)) |>
            as.data.frame()
        })) |>
          dplyr::mutate(code = merge_vars(
            .data,
            self$grouping_vars,
            .data[[self$x]],
            self$code_seperator
          )))
      }
    },
    # attaches descriptive stats
    # @param letters_tbl dataframe containing letters
    # @param var  column in the data set
    attach_descriptive_stats = function(letters_tbl, transformed_data, var) {
      selection_vars <- vec_na_rm(c(self$grouping_vars, self$x))

      transformed_data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(selection_vars))) |>
        dplyr::summarise(dplyr::across(.cols = -dplyr::any_of(c(self$factor_vars, self$x)), ~ get_summary(.x, self$deviation_type)), .groups = "drop") |>
        dplyr::select(dplyr::all_of(c(selection_vars, var))) |>
        dplyr::mutate(dplyr::across(.cols = -dplyr::any_of(c(self$factor_vars, self$x)), ~ stringr::str_replace_all(.x, "NA|NaN", "0.00"))) |>
        dplyr::mutate(code = private$get_code(.data)) |>
        dplyr::inner_join(letters_tbl, by = "code") |>
        dplyr::select(dplyr::all_of(c(var, self$letter.name)), dplyr::ends_with(".x")) |>
        dplyr::rename_with(~ ifelse(stringr::str_detect(.x, ".x"), lodaR::extract_chars(stringr::str_extract(.x, "^[a-z\\.]+(?=x)")), .x)) |>
        dplyr::mutate(
          mean = lodaR::extract_chars(.data[[var]], "^[\\d\\.\\-]+(?=\\s)"),
          s.e = lodaR::extract_chars(.data[[var]], "(?<=\\s)[\\d\\.\\-]+"),
          mean = sapply(mean, function(.x) ifelse(stringr::str_detect(.x, "^[\\d]+"), as.numeric(.x), NA)),
          s.e = sapply(s.e, function(.x) ifelse(stringr::str_detect(.x, "^[\\d]+"), as.numeric(.x), NA)),
          y.pt = mean + s.e
        ) |>
        dplyr::relocate(dplyr::any_of(selection_vars), .before = dplyr::all_of(var)) |>
        dplyr::rename_with(~ ifelse(.x == "s.e", self$deviation_type, .x))
    }
  ),
  public = list(
    data = NULL,
    x = NULL,
    grouping_vars = NULL,
    deviation_type = NULL,
    code_seperator = "@",
    factor_vars = NULL,
    format = NULL,
    letter.name = "letters",
    include = NULL,
    decreasing = NULL,
    transform_func = NULL,
    #' @param data The data frame containing variables to be analyzed
    #' @param x The independent or predictor variable in the data
    #' @param factor_vars The factor variables in the data - Optional when `grouping var` argument is specified # nolint
    #' @param grouping_vars The grouping variables in the data if there are any - Optional # nolint
    #' @param deviation_type The type of degree of spread - `s.e` or `sd` default to `s.e` # nolint
    #' @param console_view print as plain text if set to `TRUE` or markdown if set to `FALSE` # nolint
    #' @param code_seperator The internal code separator defaults to `@` - Optional # nolint
    #' @param decreasing The order at which the alphabet are ranked - defaults to `FALSE` # nolint
    #' @param format The representation of the summary data in the dataframe - defaults to `plain` - options are `plain`, # nolint
    #' @param include a vector containing ANOVA statistical result to add to the final data frame - `f-value` or/and `p-value` defaults to only `p-value` # nolint
    #' @param transform_func a callback that can be to transorm each dependent var in the dataset # nolint
    initialize = function(data,
                          x,
                          factor_vars = NULL,
                          grouping_vars = NULL,
                          deviation_type = "s.e",
                          format = "plain",
                          include = "p-value",
                          decreasing = FALSE,
                          transform_func = NULL) {
      self$data <- data
      self$x <- x
      self$grouping_vars <- grouping_vars
      self$deviation_type <- deviation_type
      self$include <- include
      self$format <- format
      self$decreasing <- decreasing
      self$transform_func <- transform_func
      self$factor_vars <- vec_na_rm(unique(c(
        factor_vars,
        grouping_vars
      )))

      ## converting group columns from factors to character makes
      ## it easier to split the data sets
      ## as compared to when the columns are factors
      if (!is.null(grouping_vars)) {
        for (var in grouping_vars) {
          self$data[[var]] <- as.character(self$data[[var]])
        }
      }
    },
    #' separates the dataframe
    #'
    #' It separates and return a list of the results in there respective groups
    #'
    #' @return list containing the groups of data set result
    separate = function() {
      if (is.null(private$results)) {
        vars <- setdiff(colnames(self$data), c(self$factor_vars, self$x))

        result <- self$data |>
          subset(select = vars) |>
          colnames() |>
          sapply(function(var) {
            transformed_data <- private$transform_var(var)

            res_data <- transformed_data |>
              private$run_post_hoc(var) |>
              private$compute_letters(transformed_data, var) |>
              private$attach_descriptive_stats(transformed_data, var)

            res_data[[self$x]] <- factor(
              res_data[[self$x]],
              levels = unique(transformed_data[[self$x]])
            )

            res_data <- res_data[order(res_data[[self$x]]), ]

            return(res_data)
          }, simplify = FALSE)

        private$results <- result
        self$compute_ANOVA()

        return(result)
      } else {
        return(private$results)
      }
    },
    #' Compute the ANOVA for the data
    #'
    #' it compute the anova and returns a dataframe containing
    #' groups and p-value's
    #' @return dataframe
    compute_ANOVA = function() {
      remove_factor_vars <- function(data) {
        if (is.null(self$factor_vars)) {
          return(data)
        }

        return(data[, -match(self$factor_vars, colnames(data))])
      }

      if (is.null(private$ANOVA_result)) {
        if (is.null(self$grouping_vars)) {
          result <- self$data |>
            remove_factor_vars() |>
            ## change all na's to 0 because some section of the
            ## data may have na's all through
            ## which is bad for the `aov` function
            dplyr::mutate(dplyr::across(
              -dplyr::any_of(self$x),
              ~ ifelse(is.na(.x), 0, .x))
            ) |>
            fastanova_test(
              x = self$x,
              include = self$include,
              transform_func = self$transform_func
            )
        } else {
          user_factors <- setdiff(self$factor_vars, self$grouping_vars)

          result <- self$data |>
            dplyr::select(-dplyr::any_of(user_factors)) |>
            ## change all na's to 0 because some section of the
            ## data may have na's all through
            ## which is bad for the `aov` function
            dplyr::mutate(dplyr::across(
              -dplyr::any_of(c(self$grouping_vars, self$x)),
              ~ ifelse(is.na(.x), 0, .x)
              )) |>
            dplyr::group_by(dplyr::across(dplyr::all_of(self$grouping_vars))) |>
            fastanova_test(
              x = self$x,
              include = self$include,
              transform_func = self$transform_func
            )
        }

        private$ANOVA_result <<- result
        return(result)
      } else {
        return(private$ANOVA_result)
      }
    },
    #' displays a human readable table
    #'
    #' It display human readable dataframe
    #' 
    #' @param order_by  Option to either order the results using independent variable (x) or the grouping vars # nolint
    #' @param rep_rm logical argument indicating to remove repitions from grouping variables # nolint
    #'
    #' @return dataframe
    display_table = function(order_by = "x", rep_rm = FALSE) {

      insert_stats <- function(data, var) {
        letters <- data[[self$letter.name]]
        var_data <- data[[var]]

        if (any(is.na(letters))) {
          return(sapply(seq_along(letters), function(i) {
            if (!is.na(letters[i])) {
              return(paste0(
                var_data[i],
                format_label(letters[i],
                  self$format,
                  type = "subscript"
                )
              ))
            }

            return(var_data[i])
          }))
        }

        return(paste0(
          data[[var]],
          format_label(data[[self$letter.name]],
            self$format,
            type = "subscript"
          )
        ))
      }

      if (is.null(private$table_display)) {
        if (order_by == "x") {
          order_var <- self$x
        } else if (order_by == "grouping_vars") {
          order_var <- self$grouping_vars
        }

        selection_vars <- vec_na_rm(c(self$grouping_vars, self$x))

        seperated_means_list <- self$separate()

        results <- lapply(names(seperated_means_list), function(var) {
            sub_data <- seperated_means_list[[var]]
            sub_data[[var]] <- insert_stats(sub_data, var)

            return(sub_data[, c(selection_vars, var)])
          }) |>
          Reduce(function(a, b) merge(a, b, by = selection_vars), x = _) |>
          dplyr::arrange(dplyr::across(dplyr::all_of(order_var)))

          if (rep_rm & !is.null(self$grouping_vars)) {
            results <- remove_repititions(results, self$grouping_vars)
          }

        private$table_display <<- results

        return(results)
      } else {
        return(private$table_display)
      }
    },
    #' Create a table summary for all the groups
    #'
    #' It create a summary of all the possible groups if there are any
    #'
    #' @return list | dataframe
    table_summary = function() {
      get_label <- function() {
        if (length(self$include) > 1) {
          return(paste0(paste(self$include, collapse = " ("), ")"))
        }

        return(self$include)
      }

      aov_tbl <- self$compute_ANOVA()

      ## insert labels like p-value in the data
      aov_tbl[[self$x]] <- sapply(aov_tbl[[self$x]], function(x) {
        format_label(get_label(), self$format)
      })

      tbl <- rbind(self$display_table(), aov_tbl)

      if (is.null(self$grouping_vars)) {
        spacer <- sapply(colnames(tbl), function(c) "...", simplify = FALSE)

        summary <- do.call(rbind, list(
          tbl[seq_len(nrow(tbl) - 1), ],
          as.data.frame(spacer),
          tbl[nrow(tbl), ]
        ))

        rownames(summary) <- NULL
      } else {
        splitted_tbls <- custom_split(tbl, self$grouping_vars)

        summary <- lapply(splitted_tbls, function(df) {
          spacer <- sapply(colnames(df), function(c) "...", simplify = FALSE)

          sections <- list(
            df[seq_len(nrow(df) - 1), ],
            as.data.frame(spacer),
            df[nrow(df), ]
          )

          all_sections <- do.call(rbind, sections)
          idx <- which(colnames(all_sections) %in% self$grouping_vars)
          all_sections <- all_sections[, -idx]

          rownames(all_sections) <- NULL

          return(all_sections)
        })
      }

      return(summary)
    }
  )
)
