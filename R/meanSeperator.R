#' Mean seperator using tukey HSD
#'
#' Is contains function that separate means and display them in a data frame
#'
#' @importFrom methods new
#' @importFrom stats setNames
#' @import stringr
#' @import lodaR
#' @import dplyr
#' @import R6
#' @importFrom multcompView multcompLetters
#' @importFrom multcompView vec2mat
#'
#' @field data The data frame containing variables to be analyzed
#' @field x The independent or predictor variable in the data
#' @field factor_vars The factor variables in the data - Optional when `grouping var` argument is specified
#' @field grouping_vars The grouping variables in the data if there are any - Optional
#' @field deviation_type The type of degree of spread - `s.e` or `sd` default to `s.e`
#' @field code_seperator The internal code separator defaults to `@` - Optional
#' @field letter.name The column name for the separation of mean letters
#' @field decreasing The order at which the alphabet are ranked - defaults to `FALSE`
#' @field format The representation of the summary data in the dataframe - defaults to `plain` - options are `plain`, `md`, and `html`
#' @field include a vector containing ANOVA statistical result to add to the final data frame - `f-value` or/and `p-value` defaults to only `p-value`
#' @export
# @field ANOVA_result cache the result of the `compute_ANOVA` function
# @field results The analysed results for all the groups
# @field table_display cache the result of the table_display function
Separator <- R6::R6Class(
  class = FALSE,
  cloneable = FALSE,
  'Separator',
  private = list(
    table_display = NULL,
    ANOVA_result = NULL,
    results = NULL,
    # get codes for the data sets
    #
    # @param data dataframe
    get_code = function(data) {
      if (is.null(self$grouping_vars)) {
        return(as.character(data[[self$x]]))
      }

      return(merge_vars(data,
                        self$grouping_vars,
                        data[[self$x]],
                        self$code_seperator))
    },
    # Run post-hoc for a parameter in the data set
    #
    # @param param column in dataframe
    run_post_hoc = function(param) {
      if (is.null(self$grouping_vars)) {
        self$data |>
          tukey.HSD(as.formula(sprintf("%s ~ %s", param, self$x))) |>
          dplyr::mutate(code = paste(group1, group2, sep = "-")) |>
          dplyr::select(p.adj, code)
      } else {
        self$data |>
          dplyr::mutate(dplyr::across(.cols = -dplyr::any_of(c(self$grouping_vars, self$x)), ~ ifelse(is.na(.x), 0, .x))) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(self$grouping_vars))) |>
          tukey.HSD(as.formula(sprintf("%s ~ %s", param, self$x))) |>
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
    compute_letters = function(post_hoc_tbl, param) {
      get_letters = function(df, raw_data) {
        if (all(is.na(df$p.adj)) || all(is.nan(df$p.adj))) {
          codes <- lapply(df$code, function(c) unlist(strsplit(c, "-", fixed = TRUE)))
          codes <- unique(unlist(codes))

          return(rep(NA, length(codes)) |> stats::setNames(codes))
        } else {
          raw_data$code <- private$get_code(raw_data)

          mean_group <- tapply(raw_data[, param], raw_data[, self$x], mean)
          ordered_mean <- order(mean_group, decreasing = self$decreasing)

          lvls <- unique(raw_data[, c("code")])[ordered_mean]

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
        letters <- get_letters(post_hoc_tbl, self$data)

        return(do.call(rbind, lapply(names(letters), function(name) {
          list(name) |>
            append(letters[[name]]) |>
            stats::setNames(c(self$x, self$letter.name)) |>
            as.data.frame()
        })) |>
          dplyr::mutate(code = .data[[self$x]]))

      } else {
        splited_tbls <- custom_split(post_hoc_tbl, self$grouping_vars)
        splited_raw_tbls <- custom_split(self$data, self$grouping_vars)

        ## verify if the names are the sames
        stopifnot(names(splited_raw_tbls) %in% names(splited_tbls))

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
          dplyr::mutate(code = merge_vars(.data,
                                          self$grouping_vars,
                                          .data[[self$x]],
                                          self$code_seperator)))
      }
    },
    # attaches descriptive stats
    # @param letters_tbl dataframe containing letters
    # @param var  column in the data set
    attach_descriptive_stats = function(letters_tbl, var) {
      selection_vars <- vec.na.rm(c(self$grouping_vars, self$x))

      self$data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(selection_vars))) |>
        dplyr::summarise(dplyr::across(.cols = -dplyr::any_of(c(self$factor_vars, self$x)), ~ get_summary(.x, self$deviation_type)), .groups = "drop") |>
        dplyr::select(dplyr::all_of(c(selection_vars, var))) |>
        dplyr::mutate(dplyr::across(.cols = -dplyr::any_of(c(self$factor_vars, self$x)), ~ stringr::str_replace_all(.x, "NA|NaN", "0.00"))) |>
        dplyr::mutate(code = private$get_code(.data)) |>
        dplyr::inner_join(letters_tbl, by = 'code') |>
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
    #' @param data The data frame containing variables to be analyzed
    #' @param x The independent or predictor variable in the data
    #' @param factor_vars The factor variables in the data - Optional when `grouping var` argument is specified
    #' @param grouping_vars The grouping variables in the data if there are any - Optional
    #' @param deviation_type The type of degree of spread - `s.e` or `sd` default to `s.e`
    #' @param console_view print as plain text if set to `TRUE` or markdown if set to `FALSE`
    #' @param code_seperator The internal code separator defaults to `@` - Optional
    #' @param decreasing The order at which the alphabet are ranked - defaults to `FALSE`
    #' @param format The representation of the summary data in the dataframe - defaults to `plain` - options are `plain`,
    #' @param include a vector containing ANOVA statistical result to add to the final data frame - `f-value` or/and `p-value` defaults to only `p-value`
    initialize = function(data,
                          x,
                          factor_vars = NULL,
                          grouping_vars = NULL,
                          deviation_type = "s.e",
                          format = "plain",
                          include = "p-value",
                          decreasing = FALSE) {

      if (!is.null(grouping_vars)) {
        self$data <- data

        for (var in grouping_vars) {
          self$data[[var]] <- as.character(self$data[[var]])
        }
      } else {
        self$data <- data
      }

      self$x <- x
      self$grouping_vars <- grouping_vars
      self$deviation_type <- deviation_type
      self$include <- include
      self$format <- format
      self$decreasing <- decreasing
      self$factor_vars <- vec.na.rm(unique(c(factor_vars,
                                             grouping_vars)))
    },
    #' separates the dataframe
    #'
    #' It separates and return a list of the results in there respective groups
    #'
    #' @return list containing the groups of data set result
    separate = function() {
      if (is.null(private$results)) {
        result <- self$data |>
          dplyr::select(-dplyr::any_of(c(self$factor_vars, self$x))) |>
          colnames() |>
          sapply(function(var) {
            private$run_post_hoc(var) |>
              private$compute_letters(var) |>
              private$attach_descriptive_stats(var) |>
              dplyr::mutate(dplyr::across(dplyr::all_of(self$x), ~ factor(.x, levels = unique(self$data[[self$x]])))) |>
              dplyr::arrange(dplyr::across(dplyr::all_of(self$x)))
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
    #' it compute the anova and returns a dataframe containing groups and p-value's
    #' @return dataframe
    compute_ANOVA = function() {
      remove_factor_vars <- function(data) {
        if (is.null(self$factor_vars)) {
          return(data)
        }

        return(dplyr::select(data, -dplyr::any_of(self$factor_vars)))
      }

      if (is.null(private$ANOVA_result)) {
        if (is.null(self$grouping_vars)) {
          result <- self$data |>
            remove_factor_vars() |>
            fastanova.test(x = self$x, add = self$include)

        } else {
          user_factors <- self$factor_vars[!self$factor_vars %in% self$grouping_vars]

          result <- self$data |>
            dplyr::select(-dplyr::any_of(user_factors)) |>
            dplyr::group_by(dplyr::across(dplyr::all_of(self$grouping_vars))) |>
            fastanova.test(x = self$x, add = self$include)
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
    #' @return dataframe
    display_table = function() {

      get_label <- function() {
        if (length(self$include) > 1) {
          return(paste0(paste(self$include, collapse = " ("), ")"))
        }

        return(self$include)
      }


      insert_stats <- function(data, var) {
        letters <- data[[self$letter.name]]
        var_data <- data[[var]]

        if (any(is.na(letters))) {
          return(sapply(seq(letters), function(i) {
            if (!is.na(letters[i])) {
              return(paste0(var_data[i],
                            format.label(letters[i],
                                         self$format, type = "subscript")))
            }

            return(var_data[i])
          }))
        }

        return(paste0(data[[var]],
                      format.label(data[[self$letter.name]],
                                   self$format, type = "subscript")))
      }

      if (is.null(private$table_display)) {

        selection_vars <- vec.na.rm(c(self$grouping_vars, self$x))

        seperated_means_list <- self$separate()

        aov_tbl <- private$ANOVA_result
        aov_tbl[[self$x]] <- sapply(aov_tbl[[self$x]], function(x) format.label(get_label(), self$format))


        results <- seperated_means_list |>
          names() |>
          lapply(function(var) {
            seperated_means_list[[var]] |>
              dplyr::mutate(dplyr::across(dplyr::all_of(var), ~ insert_stats(.data, var))) |>
              dplyr::select(dplyr::any_of(c(selection_vars, var)))

          }) |>
          Reduce(function(.x, .y) merge(.x, .y, by = selection_vars), x = _) |>
          dplyr::mutate(dplyr::across(dplyr::all_of(self$x), ~ factor(.x, levels = unique(self$data[[self$x]])))) |>
          dplyr::arrange(dplyr::across(dplyr::all_of(self$x))) |>
          dplyr::bind_rows(aov_tbl)

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
      tbl <- self$display_table()

      if (is.null(self$grouping_vars)) {
        summary <- do.call(rbind, list(tbl[1:nrow(tbl) - 1, ],
                            as.data.frame(sapply(colnames(tbl), function(c) "...", simplify = FALSE)),
                            tbl[nrow(tbl), ]))
      } else {
        splitted.tbls <- custom_split(tbl, self$grouping_vars)

        summary <- lapply(splitted.tbls, function(df) {
          sections <- list(
            df[1:nrow(df) - 1, ],
            as.data.frame(sapply(colnames(df), function(c) "...", simplify = FALSE)),
            df[nrow(df), ]
          )

          all_sections <- do.call(rbind, sections)
          all_sections <- all_sections[, -which(colnames(all_sections) %in% self$grouping_vars)]

          return(all_sections)
        })
      }

      return(summary)
    }
))
