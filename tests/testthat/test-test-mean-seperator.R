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

test_that("separator works without grouping vars", {
  obj <- Separator$new(
    data = data,
    x = "month",
    factor_vars = c("gender", "location")
  )

  result <- obj$display_table()

  expect_equal(ncol(result), ncol(data) - 2)
  expect_equal(colnames(result), c("month", "age", "height"))
})

test_that("separator works with grouping vars", {
  obj <- Separator$new(
    data = data,
    x = "month",
    grouping_vars = c("gender", "location"),
    deviation_type = "s.e"
  )

  result <- obj$display_table()

  expect_equal(ncol(result), ncol(data))
  expect_equal(
    colnames(result),
    c("gender", "location", "month", "age", "height")
  )
})

test_that("separator works with grouping vars and factor vars", {

  obj <- Separator$new(
    data = data,
    x = "month",
    grouping_vars = "gender",
    factor_vars = "location"
  )

  result <- obj$display_table()

  expect_equal(ncol(result), ncol(data) - 1)
  expect_equal(colnames(result), c("gender", "month", "age", "height"))
})

test_that("separator works with repetition in factor vars", {
  data$month <- factor(data$month)

  obj <- Separator$new(
    data = data,
    x = "month",
    grouping_vars = "gender",
    factor_vars = c("location", "gender")
  )

  result <- obj$display_table()

  expect_equal(ncol(result), 4)
  expect_equal(colnames(result), c("gender", "month", "age", "height"))
})


test_that(
  "fast summary function works with grouping variable and 
  only p-value as the statistic result", {

  obj <- Separator$new(
    data = data,
    x = "month",
    grouping_vars = c("gender", "location"),
    include = "p-value",
    deviation_type = "sd",
    format = "plain"
  )

  result <- obj$table_summary()

  label <- as.character(result[[1]][[1]])[nrow(result[[1]])]

  expect_equal(
    length(result),
    length(unique(data$gender)) * length(unique(data$location))
  )
  expect_equal(colnames(result[[1]]), c("month", "age", "height"))
  expect_equal(nrow(result[[1]]), n_month + 2)
  expect_equal(label, " p-value")
})

test_that(
  "fast summary function works with grouping variable and has p-value
  and f-value as the statistic result", {

  obj <- Separator$new(
    data = data,
    x = "month",
    grouping_vars = c("gender", "location"),
    include = c("f-value", "p-value")
  )

  result <- obj$table_summary()

  print(obj$table_summary(TRUE))

  label <- as.character(result[[1]][[1]])[nrow(result[[1]])]

  expect_equal(
    length(result),
    length(unique(data$gender)) * length(unique(data$location))
  )
  expect_equal(colnames(result[[1]]), c("month", "age", "height"))
  expect_equal(nrow(result[[1]]), n_month + 2)
  expect_equal(label, " f-value (p-value)")
  expect_true(
    grepl(
      "[\\d\\s]+(?=\\()",
      result[[1]][[2]][nrow(result[[2]])],
      perl = TRUE
    )
  )
})


test_that("table summary function works with one grouping 
variable and one factor variable", {

  obj <- Separator$new(
    data = data,
    x = "month",
    grouping_vars = "gender",
    factor_vars = "location",
    include = c("f-value", "p-value"),
    deviation_type = "sd",
    decreasing = TRUE,
    format = "html"
  )

  result <- obj$table_summary()

  label <- as.character(result[[1]][[1]])[nrow(result[[1]])]

  expect_equal(length(result), length(unique(data$gender)))
  expect_equal(colnames(result[[1]]), c("month", "age", "height"))
  expect_equal(nrow(result[[1]]), n_month + 2)
  expect_equal(label, "<strong>f-value (p-value)</strong>")
  expect_true(
    grepl(
      "[\\d\\s]+(?=\\()",
      result[[1]][[2]][nrow(result[[2]])],
      perl = TRUE
    )
  )
})

test_that("table summary function works with no grouping variable 
and more than one factor variable", {

  obj <- Separator$new(
    data = data,
    x = "month",
    factor_vars = c("location", "gender"),
    include = c("f-value", "p-value"),
    deviation_type = "sd",
    decreasing = TRUE,
    format = "md"
  )

  result <- obj$table_summary()

  expect_equal(colnames(result), c("month", "age", "height"))
  expect_equal(nrow(result), n_month + 2)
  expect_equal(as.character(result[[1]])[nrow(result)], "**f-value (p-value)**")
  expect_true(grepl("[\\d\\s]+(?=\\()", result[[2]][nrow(result)], perl = TRUE))
})

test_that("table summary function works with no grouping variable and 
no factor variable", {

  obj <- Separator$new(
    data =  subset(data, select = c(-gender, -location)),
    x = "month",
    deviation_type = "sd",
    decreasing = TRUE,
    format = "md"
  )

  result <- obj$table_summary()

  expect_equal(colnames(result), c("month", "age", "height"))
  expect_equal(nrow(result), n_month + 2)
  expect_equal(as.character(result[[1]])[nrow(result)], "**p-value**")
})


test_that("table summary function works when data is transformed", {

  obj <- Separator$new(
    data = data,
    x = "month",
    deviation_type = "sd",
    decreasing = TRUE,
    grouping_vars = c("gender", "location"),
    transform_func = function(df) {
      df$month <- factor(
        df$month,
        levels = month.abb,
        labels = month.name
      )

      return(df)
    }
  )

  result <- obj$table_summary()

  label <- as.character(result[[1]][[1]])[nrow(result[[1]])]

  expect_equal(
    length(result),
    length(unique(data$gender)) * length(unique(data$location))
  )
  expect_equal(colnames(result[[1]]), c("month", "age", "height"))
  expect_equal(nrow(result[[1]]), n_month + 2)
  expect_equal(as.character(result[[1]]$month[seq_len(n_month)]), month.name)
  expect_equal(label, " p-value")
  expect_true(
    grepl(
      "[\\d\\.]+", result[[1]][[2]][nrow(result[[2]])], perl = TRUE
    )
  )
})