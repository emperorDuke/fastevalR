## The fastevalR package

This package convert a raw data into descriptive statistics and separation of means using Tukey HSD for now

### Installation

```{r}
devtools::install_github("https://github.com/emperorDuke/fastevalR.git")
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
  location = rep(letters[1:4], 4),
  age = c(rnorm(8, mean = 66.4), rnorm(8, mean = 60.4)),
  height = c(rnorm(8, mean = 5.4), rnorm(8, mean = 6.4))
)
```

```{r}
# print(data)
# 
#    month gender location      age   height
# 1    Jan      M      a    67.96101 7.347027
# 2    Feb      M      b    66.41855 4.457138
# 3    Mar      M      c    65.14273 5.749648
# 4    Apr      M      d    66.09282 6.555883
# 5    Jan      M      a    66.84869 4.698016
# 6    Feb      M      b    67.30455 6.060497
# 7    Mar      M      c    65.64137 4.235999
# 8    Apr      M      d    66.20104 3.847365
# 9    Jan      F      a    59.28777 6.721247
# 10   Feb      F      b    61.68628 6.961674
# 11   Mar      F      c    63.08159 6.541809
# 12   Apr      F      d    59.61394 5.800709
# 13   Jan      F      a    62.22129 6.591437
# 14   Feb      F      b    60.90156 4.613485
# 15   Mar      F      c    62.19581 5.459384
# 16   Apr      F      d    61.25008 5.806265

``` 
## For displaying a well packaged table

```{r}
obj <- new(
  'Separator',
  data = data,
  x = "month",
  grouping_vars = "gender",
  factor_vars = "location"
)

result <- obj$display_table()

# print(result)
# 
#  gender month            age        height
# 1      F   Apr 60.65 ± 0.13a  6.04 ± 1.23a
# 2      F   Feb 60.60 ± 0.13a  4.33 ± 0.28a
# 3      F   Jan 61.28 ± 1.11a  5.98 ± 0.98a
# 4      F   Mar 61.08 ± 0.05a  6.24 ± 1.13a
# 5      M   Apr 66.67 ± 0.54a  5.19 ± 0.78a
# 6      M   Feb 66.59 ± 0.85a  5.01 ± 0.59a
# 7      M   Jan 67.80 ± 0.05a  5.14 ± 1.06a
# 8      M   Mar 66.69 ± 0.13a  6.29 ± 0.73a
```
## For display the a list containing results from the differenct groups without ANOVA p-value

```{r}
splitted_results <- obj$separate()

# print(splitted_results)
# 
# $age
# # A tibble: 8 × 7
#   gender month age          letters  mean   s.e  y.pt
#   <chr>  <chr> <chr>        <chr>   <dbl> <dbl> <dbl>
# 1 F      Apr   60.65 ± 0.13 a        60.6  0.13  60.8
# 2 F      Feb   60.60 ± 0.13 a        60.6  0.13  60.7
# 3 F      Jan   61.28 ± 1.11 a        61.3  1.11  62.4
# 4 F      Mar   61.08 ± 0.05 a        61.1  0.05  61.1
# 5 M      Apr   66.67 ± 0.54 a        66.7  0.54  67.2
# 6 M      Feb   66.59 ± 0.85 a        66.6  0.85  67.4
# 7 M      Jan   67.80 ± 0.05 a        67.8  0.05  67.8
# 8 M      Mar   66.69 ± 0.13 a        66.7  0.13  66.8
# 
# $height
# # A tibble: 8 × 7
#   gender month height      letters  mean   s.e  y.pt
#   <chr>  <chr> <chr>       <chr>   <dbl> <dbl> <dbl>
# 1 F      Apr   6.04 ± 1.23 a        6.04  1.23  7.27
# 2 F      Feb   4.33 ± 0.28 a        4.33  0.28  4.61
# 3 F      Jan   5.98 ± 0.98 a        5.98  0.98  6.96
# 4 F      Mar   6.24 ± 1.13 a        6.24  1.13  7.37
# 5 M      Apr   5.19 ± 0.78 a        5.19  0.78  5.97
# 6 M      Feb   5.01 ± 0.59 a        5.01  0.59  5.6 
# 7 M      Jan   5.14 ± 1.06 a        5.14  1.06  6.2 
# 8 M      Mar   6.29 ± 0.73 a        6.29  0.73  7.02

```

### **Note**: Column `y.pt` is the y axis for anotating the `letters` column in a ggplot graph

## For generation a summary table with ANOVA p-values 

```{r}

# print(obj$table_summary()) a list
# 
# $F
#     month            age        height
# 1     Apr 60.65 ± 0.13 a 6.04 ± 1.23 a
# 2     Feb 60.60 ± 0.13 a 4.33 ± 0.28 a
# 3     Jan 61.28 ± 1.11 a 5.98 ± 0.98 a
# 4     Mar 61.08 ± 0.05 a 6.24 ± 1.13 a
# 5     ...            ...           ...
# 6 p-value           0.79          0.55
# 
# $M
#     month            age        height
# 1     Apr 66.67 ± 0.54 a 5.19 ± 0.78 a
# 2     Feb 66.59 ± 0.85 a 5.01 ± 0.59 a
# 3     Jan 67.80 ± 0.05 a 5.14 ± 1.06 a
# 4     Mar 66.69 ± 0.13 a 6.29 ± 0.73 a
# 5     ...            ...           ...
# 6 p-value           0.40          0.68

```
### **`result`** is a list of `grouping_vars` and thier respective dataframe without ANOVA p-value

@copyright *emperorDuke*
