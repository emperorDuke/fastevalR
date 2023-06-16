## The fastevalR package

This package convert a raw data into descriptive statistics and separation of means using Tukey HSD for now

### Installation

```{r}
devtools::install_github("https://github.com/emperorDuke/meanSeparator.git")
```

### Requirements

-   R 4.1.0 \>

-   tidyverse

-   lodaR :: installation link is below

```{r}
devtools::install_github("https://github.com/emperorDuke/lodaR.git")
```

```{r}

library(fastevalR)

data <- data.frame(
  month = rep(month.abb[1:4], 4),
  gender = rep(c('M', 'F'), each = 8),
  letter = rep(letters[1:4], 4),
  age = c(rnorm(8, mean = 66.4), rnorm(8, mean = 60.4))
)
  
obj <- new(
  'Separator',
  data = data,
  x = "month",
  grouping_vars = "gender",
  factor_vars = "letter"
)

## for displaying a well packaged table

result <- obj$display_table()

## for display the a list containing results from the differenct groups without ANOVA p-value

splitted_results <- obj$separate()

## For generation a summary table with ANOVA p-values 
## `result` is a list of `grouping_vars` and thier respective dataframe without ANOVA p-value

result <- fastsummary.stats(
  data = data,
  x = "month",
  deviation_type = "s.e",
  grouping_vars = "gender",
  factor_vars = "letter",
  console_view = TRUE
)

## A list

result
```

@copyright *emperorDuke*
