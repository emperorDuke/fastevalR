n_month <- 12
sample_size <- 360
population <- rnorm(200, mean = 80.4, sd = 4)
height_population <- rnorm(200, mean = 55.4, sd = 10)

data <- data.frame(
  month = rep(month.abb[seq_len(n_month)], sample_size / n_month),
  gender = rep(c("M", "F"), each = sample_size / 2),
  location = sample(c("abuja", "lagos", "calabar"), size = sample_size, replace = TRUE), # nolint
  age = sample(population, size = sample_size, replace = TRUE),
  height = sample(height_population, size = sample_size, replace = TRUE)
)

data <- data |>
    split(data$month) |>
    lapply(function(df) within(df, { subject <- seq_len(nrow(df)) })) |>
    do.call(rbind, args = _) |>
    within({ month <- factor(month) })

# res <- fastanova_test_2(
#     data = data,
#     between = c("month", "location"),
#     within = "month",
#     wid = "subject",
#     btw_terms_sep = "*"
#   )

# obj <- Separator$new(
#     data = data,
#     x = "location",
#     grouping_vars = "month",
#     factor_vars = c("subject", "gender")
#   )

# result <- obj$display_table(
#   include_aov = FALSE,
#   order_by = "group",
#   rep_rm = FALSE
# )

# binder_2(result, res)

# test_that("separator works without grouping vars", {
#   result <- fastanova_test_2(
#     data = data,
#     between = c("gender", "location"),
#     within = "month",
#     wid = "subject",
#     btw_terms_sep = "*"
#   )

#   expect_equal(ncol(result), ncol(data) - 2)
#   expect_equal(colnames(result), c("month", "age", "height"))
# })