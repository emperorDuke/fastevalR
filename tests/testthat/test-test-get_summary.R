test_that("get summary works", {
  data <- data.frame(
    month = rep(month.abb[1:4], 4),
    gender = rep(c("M", "F"), each = 8),
    age = c(rnorm(8, mean = 66.4), rnorm(8, mean = 60.4))
  )

  summary <- get_summary(data$age, spread = "sd")

  expect_true(stringr::str_detect(summary, "±"))
  expect_true(stringr::str_detect(summary, "^[\\d\\.]+(?=\\s)"))
  expect_true(stringr::str_detect(summary, "(?<=\\s)[\\d\\.]+"))
})
