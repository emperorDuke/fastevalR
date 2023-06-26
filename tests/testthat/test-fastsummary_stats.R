n_month <- 12
sample_size <- 360
population <- rnorm(200, mean = 80.4, sd = 4)

data <- data.frame(
  month = rep(month.abb[seq(n_month)], sample_size / n_month),
  gender = rep(c('M', 'F'), each = sample_size / 2),
  location = sample(c("abuja", "lagos", "calabar"), size = sample_size, replace = TRUE),
  age = sample(population, size = sample_size, replace = TRUE)
)


test_that("fast summary function works with grouping variable and only p-value as the statistic result", {
  result <- fastsummary.stats(
    data = data,
    x = "month",
    grouping_vars = c("gender", "location"),
    add = "p-value",
    deviation_type = "sd",
    console_view = FALSE
  )

  expect_equal(length(result), length(unique(data$gender)) * length(unique(data$location)))
  expect_equal(colnames(result[[1]]), c("month", "age"))
  expect_equal(nrow(result[[1]]), n_month + 2)
  expect_equal(result[[1]][[1]][nrow(result[[1]])], "**p-value**")
})

test_that("fast summary function works with grouping variable and has p-value and f-value as the statistic result", {
  result <- fastsummary.stats(
    data = data,
    x = "month",
    grouping_vars = c("gender", "location"),
    add = c("f-value", "p-value")
  )

  expect_equal(length(result), length(unique(data$gender)) * length(unique(data$location)))
  expect_equal(colnames(result[[1]]), c("month", "age"))
  expect_equal(nrow(result[[1]]), n_month + 2)
  expect_equal(result[[1]][[1]][nrow(result[[1]])], "f-value (p-value)")
  expect_true(stringr::str_detect(result[[1]][[1]][nrow(result[[2]])], "[\\d\\s]+(?=\\()"))
})

test_that("fast summary function works with one grouping variable and one factor variable", {
  result <- fastsummary.stats(
    data = data,
    x = "month",
    grouping_vars = "gender",
    factor_vars = "location",
    add = c("f-value", "p-value"),
    deviation_type = "sd",
    console_view = FALSE
  )

  expect_equal(length(result), length(unique(data$gender)))
  expect_equal(colnames(result[[1]]), c("month", "age"))
  expect_equal(nrow(result[[1]]), n_month + 2)
  expect_equal(result[[1]][[1]][nrow(result[[1]])], "**f-value (p-value)**")
  expect_true(stringr::str_detect(result[[1]][[1]][nrow(result[[2]])], "[\\d\\s]+(?=\\()"))
})


test_that("fast summary function works with no grouping variable and more than one factor variable", {
  result <- fastsummary.stats(
    data = data,
    x = "month",
    factor_vars = c("location", "gender"),
    add = c("f-value", "p-value"),
    deviation_type = "sd",
    console_view = FALSE
  )

  expect_equal(colnames(result), c("month", "age"))
  expect_equal(nrow(result), n_month + 2)
  expect_equal(result[[1]][nrow(result)], "**f-value (p-value)**")
  expect_true(stringr::str_detect(result[[2]][nrow(result)], "[\\d\\s]+(?=\\()"))
})


test_that("fast summary function works with no grouping variable and no factor variable", {
  result <- data |>
    dplyr::select(-gender,-location) |>
    fastsummary.stats(
      x = "month",
      deviation_type = "sd",
      console_view = FALSE
    )

  expect_equal(colnames(result), c("month", "age"))
  expect_equal(nrow(result), n_month + 2)
  expect_equal(result[[1]][nrow(result)], "**p-value**")
})
