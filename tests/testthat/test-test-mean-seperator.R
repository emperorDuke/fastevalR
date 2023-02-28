test_that("separator works without grouping vars", {
  data <- data.frame(
    month = rep(month.abb[1:4], 4),
    gender = rep(c('M', 'F'), each = 8),
    age = c(rnorm(8, mean = 66.4), rnorm(8, mean = 60.4))
  )

  obj <- new(
    'Separator',
    data = data,
    indep_var = "month",
    factor_vars = "gender"
  )
  result <- obj$display_table()

  expect_equal(length(colnames(result)), 2)
  expect_equal(colnames(result), c("Month", "Age"))
  expect_equal(nrow(result), 4)
})

test_that("separator works with grouping vars", {
  data <- data.frame(
    month = rep(month.abb[1:4], 4),
    gender = rep(c('M', 'F'), each = 8),
    age = c(rnorm(8, mean = 66.4), rnorm(8, mean = 60.4))
  )

  obj <- new(
    'Separator',
    data = data,
    indep_var = "month",
    grouping_vars = "gender"
  )
  result <- obj$display_table()

  expect_equal(length(colnames(result)), 3)
  expect_equal(colnames(result), c("Gender", "Month", "Age"))
  expect_equal(nrow(result), 8)
})

test_that("separator works with grouping vars and factor vars", {
  data <- data.frame(
    month = rep(month.abb[1:4], 4),
    gender = rep(c('M', 'F'), each = 8),
    letter = rep(letters[1:4], 4),
    age = c(rnorm(8, mean = 66.4), rnorm(8, mean = 60.4))
  )

  obj <- new(
    'Separator',
    data = data,
    indep_var = "month",
    grouping_vars = "gender",
    factor_vars = "letter"
  )
  result <- obj$display_table()

  expect_equal(length(colnames(result)), 3)
  expect_equal(colnames(result), c("Gender", "Month", "Age"))
  expect_equal(nrow(result), 8)
})

test_that("separator works with repetition in factor vars", {
  data <- data.frame(
    month = rep(month.abb[1:4], 4),
    gender = rep(c('M', 'F'), each = 8),
    letter = rep(letters[1:4], 4),
    age = c(rnorm(8, mean = 66.4), rnorm(8, mean = 60.4))
  )

  obj <- new(
    'Separator',
    data = data,
    indep_var = "month",
    grouping_vars = "gender",
    factor_vars = c("letter", "gender")
  )
  result <- obj$display_table()

  expect_equal(length(colnames(result)), 3)
  expect_equal(colnames(result), c("Gender", "Month", "Age"))
  expect_equal(nrow(result), 8)
})
