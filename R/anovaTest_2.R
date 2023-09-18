#' A more complex anova function, thats carries out one-way
#' , two-way and mixed anova
#'
#' @importFrom stats aov
#' @importFrom stats as.formula
#' @importFrom broom tidy
#'
#' @param data dataframe containing data to analyzed
#' @param wid the subject identifiers in the datasets
#' @param within variables used for within effect of the analysis
#' @param between variables used for between effect of the analysis
#' @param term the type of term to extract which is one of `all` - gets all effect except `residuals`, # nolint
#' `marginal`- gets only marginal effect and `interaction` - gets only interaction effects # nolint
#' @param btw_terms_sep between terms formula seprator with could be `+`, `*` etc # nolint
#' @param within_terms_sep =within terms formula seprator with could be `+`, `*` etc # nolint
#' @param transform_func a callback that can be to transorm each dependent var in the dataset # nolint
#' @return a matrix containing p values
#' @export
fastanova_test_2 <- function(
    data,
    wid = NULL,
    within = NULL,
    between = NULL,
    term = "all",
    btw_terms_sep = "+",
    within_terms_sep = "*",
    transform_func = NULL
) {

    join_str <- function(sep = "*", vars) {
        Reduce(function(a, b) paste(a, b, sep = sep), vars)
    }

    get_terms <- function(data, btw_vars = between) {
        res_btw_var <- join_str(":", btw_vars)

        if (term == "all") {
            res <- data[data$term != "Residuals", ]
        } else if (term == "marginal") {
            res <- data[data$term %in% btw_vars, ]
        } else if (term == "interaction") {
            res <- data[data$term == res_btw_var, ]
        }

        return(res)
    }

    char_vars <- sapply(colnames(data), function(c) {
        vec <- data[[c]][!is.na(data[[c]])]

        if (!is.numeric(vec)) {
            return(c)
        }

        return(NA)
    })

    factor_vars <- unique(c(wid, char_vars[!is.na(char_vars)]))
    vars <- colnames(data[, -which(colnames(data) %in% factor_vars)])
    btw_vars  <- join_str(btw_terms_sep, between)
    stratum <- NULL

    if (!is.null(within)) {
        withn_vars <- join_str(within_terms_sep, within)
        stratum <- sprintf("Error(%s)", withn_vars)

        if (!is.null(wid)) {
            stratum <- sprintf("Error(%s / %s)", wid, withn_vars)
        }
    }

    if (!is.null(wid) && is.null(within)) {
        stratum <- sprintf("Error(%s)", wid)
    }

    stats_res <- lapply(vars, function(var) {
        formula_str <- sprintf("%s ~ %s", var, btw_vars)

        if (!is.null(transform_func)) {
            data <- transform_func(data[, c(factor_vars, var)])
        }

        if (!is.null(stratum)) {
            formula_str <- paste(formula_str, stratum, sep = "+")
        }

        res <- broom::tidy(
            stats::aov(stats::as.formula(formula_str), data = data)
        )

        if ("stratum" %in% colnames(res)) {
            res <- get_terms(res[res$stratum == "Within", ])
        } else {
            res <- get_terms(res)
        }

        mat_data_arg <- c(
            unlist(lapply(between, function(b) rep(b, length(res$p.value)))),
            sapply(res$p.value, function(p) sprintf("%.2f", p))
        )

        mat <- matrix(
            data = mat_data_arg,
            ncol = length(between) + 1,
            nrow = length(res$p.value)
        )

        dimnames(mat) <- list(res$term, c(between, var))

        mat[, seq_along(between)] <- "..."
        mat[, 1] <- paste0(res$term, " **(p value)**")

        return(mat)
    })

    mat_res <- Reduce(function(a, b) merge(a, b, by = between), x = stats_res)

    return(mat_res)
}
