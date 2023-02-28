## The mean separator package

This package convert a raw data into descriptive statistics and separation of means using Tukey HSD for now

### Installation

```{r}
devtools::install_github("https://github.com/emperorDuke/meanSeparator.git")
```

### Requirements

-   R 4.1.0 \>

-   tidyverse

-   lodaR :: installation link is below

``` r
devtools::install_github("https://github.com/emperorDuke/lodaR.git")
```

```{r}

library(meanSeperator)

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

## for displaying a well packaged table

result <- obj$display_table()

## for display the a list containing results from the differenct groups

splitted_results <- obj$separate()
```

## Parameters the function uses

-   data - The raw data in form of a dataframe (Required)

-   indep_var: Independent variable that is used in the anova (Required)

-   grouping_vars: The variables that will be used to group the data (Optional)

-   factor_vars: A vector containing names of the factor variables in the dataset (Optional)

## @copyright*:* *emperorDuke*
