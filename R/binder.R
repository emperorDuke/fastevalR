#' Bind the spliited final results into a dataframe
#'
#' It combines the list of results splitted by grouping vars into
#' a single dataframe
#'
#' @param ls_summary splitted final results from table_summary function in
#' meanSeparator class
#' @param grouping_vars grouping vars specified in meanSeparator class
#' @return dataframe
#' @export
binder_1 <- function(ls_summary, grouping_vars) {
    ls_names <- names(ls_summary)
    ls_names <- ls_names[order(ls_names)]

    ls_df <- lapply(ls_names, function(name) {
        df <- ls_summary[[name]]
        has_groups <- grepl("^[\\w]+((\\.){1,1}[\\w]+)+$", name, perl = TRUE)
        val_space <- nrow(df) - 2

        if(has_groups) {
            splitted_str <- strsplit(name, ".", fixed = TRUE)[[1]]

            filler <- as.data.frame(
                lapply(splitted_str, function(str) {
                    c(rep(str, val_space), "...", "...")
                })
            )

            filler[nrow(filler), 1] <- as.character(df[nrow(df), 1])
            df[nrow(df), 1] <- "..."

            colnames(filler) <- grouping_vars

            return(cbind(filler, df))
        } else {
            filler <- data.frame(x = c(rep(name, val_space), "...", "..."))

            filler[nrow(filler), 1] <- as.character(df[nrow(df), 1])
            df[nrow(df), 1] <- "..."

            colnames(filler) <- grouping_vars

            return(cbind(filler, df))
        }
    })

    return(do.call(rbind, ls_df))
}


#' Bind the result of post-hoc and aov into a dataframe
#'
#' It combines the list of results splitted by grouping vars into
#' a single dataframe
#'
#' @param post_hoc_tbl final results from meanSeparator
#' @param aov_tbl aov results
#' @return dataframe
#' @export
binder_2 <- function(post_hoc_tbl, aov_tbl) {
    char_vars <- sapply(colnames(post_hoc_tbl), function(c) {
        var <- vec_na_rm(post_hoc_tbl[[c]])

        if (!any(grepl("\u00B1", var, perl = TRUE))) {
            return(c)
        }
    })

    factor_vars <- vec_na_rm(unlist(char_vars))
    post_hoc_tbl_vars <- colnames(post_hoc_tbl)

    non_factor_vars <- setdiff(post_hoc_tbl_vars, factor_vars)

    terms <- aov_tbl[[1]]

    aov_tbl[[1]] <- "..."
    aov_tbl <- cbind(aov_tbl[, c(factor_vars)], aov_tbl[, non_factor_vars])
    aov_tbl[[1]] <- terms

    spacer <- sapply(post_hoc_tbl_vars, function(r) "...", simplify = FALSE)

    final_res <- do.call(
        rbind,
        list(post_hoc_tbl, as.data.frame(spacer), aov_tbl)
    )

    rownames(final_res) <- NULL

    return(final_res)
}
