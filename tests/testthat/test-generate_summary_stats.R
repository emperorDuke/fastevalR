n_month <- 8
sample_size <- 32
population <- rnorm(30, mean = 80.4, sd = 4)

data <- data.frame(
  month = rep(month.abb[seq(n_month)], sample_size / n_month),
  gender = rep(c('M', 'F'), each = sample_size / 2),
  age = sample(population, size = sample_size, replace = TRUE)
)


test_that("generate summary works with grouping variable", {
  result <- fastsummary.stats(
    data = data,
    x = "month",
    grouping_vars = "gender",
    deviation_type = "sd",
    console_view = FALSE
  )

  expect_equal(length(result), length(unique(data$gender)))
  expect_equal(colnames(result[[1]]), c("month", "age"))
  expect_equal(nrow(result[[1]]), n_month + 2)
  expect_equal(result[[1]][[1]][nrow(result[[1]])], "**p-value**")
})
