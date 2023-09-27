n_month <- 12
sample_size <- 360
population <- rnorm(200, mean = 80.4, sd = 4)
height_population <- rnorm(200, mean = 55.4, sd = 10)

data <- data.frame(
  month = rep(month.abb, sample_size / n_month),
  gender = rep(c("M", "F"), each = sample_size / 2),
  location = sample(c("abuja", "lagos", "calabar"), size = sample_size, replace = TRUE), # nolint
  age = sample(population, size = sample_size, replace = TRUE),
  height = sample(height_population, size = sample_size, replace = TRUE)
)


test_that("fastanova_test_ works", {
  res <- data |>
    dplyr::group_by(gender, location) |>
    fastanova_test(x = "month")

    expect_equal(
      nrow(res),
      length(unique(data$location)) * length(unique(data$gender))
    )
})
