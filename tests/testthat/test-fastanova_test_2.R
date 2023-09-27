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

test_that("fastanova_test_2 works", {
  aov_res <- fastanova_test_2(
    data = data,
    between = c("gender", "location"),
    within = "month",
    wid = "subject",
    btw_terms_sep = "*"
  )

  expect_equal(ncol(aov_res), ncol(data) - 1)
  expect_equal(colnames(aov_res), colnames(subset(data, select = c(-subject))))
})