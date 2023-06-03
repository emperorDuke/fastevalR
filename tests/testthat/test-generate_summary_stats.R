test_that("multiplication works", {
  data <- data.frame(
    month = rep(month.abb[1:4], 4),
    gender = rep(c('M', 'F'), each = 8),
    age = c(rnorm(8, mean = 66.4), rnorm(8, mean = 60.4))
  )

  obj <- generate_summary_stats(
    data = data,
    indep_var = "month",
    factor_vars = "gender"
  )
  result <- obj$display_table()

  expect_equal(length(colnames(result)), 2)
  expect_equal(colnames(result), c("Month", "Age"))
  expect_equal(nrow(result), 4)
})
