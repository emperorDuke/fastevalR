test_that("generate summary works with grouping variable", {
  data <- data.frame(
    month = rep(month.abb[1:4], 4),
    gender = rep(c('M', 'F'), each = 8),
    age = c(rnorm(8, mean = 70.4), rnorm(8, mean = 80.4))
  )

  result <- generate_summary_stats(
    data = data,
    x = "month",
    grouping_vars = "gender",
    deviation_type = "sd",
    console_view = FALSE
  )

  expect_equal(length(result), length(unique(data$gender)))
  expect_equal(colnames(result[[1]]), c("month", "age"))
  expect_equal(nrow(result[[1]]), 4 + 2)
  expect_equal(result[[1]][[1]][nrow(result[[1]])], "**p-value**")
})
