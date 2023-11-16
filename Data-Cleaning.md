Data Cleaning
================
Final Project Group
2023-11-16

``` r
library(tidyverse)
library(readr)
```

### Student Weight Data

``` r
Student_Weight = read_csv("data/Student Weight.csv")
```

``` r
head(Student_Weight)
```

    ## # A tibble: 6 × 15
    ##   `Location Code` County  `Area Name` Region `Year Reported` `Number Overweight`
    ##             <dbl> <chr>   <chr>       <chr>  <chr>                         <dbl>
    ## 1               0 STATEW… STATEWIDE … STATE… 2010-2012                     77813
    ## 2               0 STATEW… STATEWIDE … STATE… 2010-2012                     38536
    ## 3               0 STATEW… STATEWIDE … STATE… 2010-2012                     39277
    ## 4               0 STATEW… STATEWIDE … STATE… 2010-2012                     44970
    ## 5               0 STATEW… STATEWIDE … STATE… 2010-2012                     22092
    ## 6               0 STATEW… STATEWIDE … STATE… 2010-2012                     22878
    ## # ℹ 9 more variables: `Percent Overweight` <dbl>, `Number Obese` <dbl>,
    ## #   `Percent Obese` <dbl>, `Number Overweight or Obese` <dbl>,
    ## #   `Percent Overweight or Obese` <dbl>, `Grade Level` <chr>,
    ## #   `Number Healthy Weight` <dbl>, `Percent Healthy Weight` <dbl>, Sex <chr>

``` r
Student_Weight  = 
  Student_Weight |>
  janitor::clean_names() |>
  rename('district'='location_code')

Student_Weight$year_reported = sub(".*-", "", Student_Weight$year_reported)

Student_Weight = 
  Student_Weight |>
  mutate(year_reported = as.numeric(Student_Weight$year_reported))
```

``` r
Student_Weight
```

    ## # A tibble: 32,025 × 15
    ##    district county              area_name region year_reported number_overweight
    ##       <dbl> <chr>               <chr>     <chr>          <dbl>             <dbl>
    ##  1        0 STATEWIDE (EXCLUDI… STATEWID… STATE…          2012             77813
    ##  2        0 STATEWIDE (EXCLUDI… STATEWID… STATE…          2012             38536
    ##  3        0 STATEWIDE (EXCLUDI… STATEWID… STATE…          2012             39277
    ##  4        0 STATEWIDE (EXCLUDI… STATEWID… STATE…          2012             44970
    ##  5        0 STATEWIDE (EXCLUDI… STATEWID… STATE…          2012             22092
    ##  6        0 STATEWIDE (EXCLUDI… STATEWID… STATE…          2012             22878
    ##  7        0 STATEWIDE (EXCLUDI… STATEWID… STATE…          2012             33004
    ##  8        0 STATEWIDE (EXCLUDI… STATEWID… STATE…          2012             16387
    ##  9        0 STATEWIDE (EXCLUDI… STATEWID… STATE…          2012             16617
    ## 10        0 STATEWIDE (EXCLUDI… STATEWID… STATE…          2013             76896
    ## # ℹ 32,015 more rows
    ## # ℹ 9 more variables: percent_overweight <dbl>, number_obese <dbl>,
    ## #   percent_obese <dbl>, number_overweight_or_obese <dbl>,
    ## #   percent_overweight_or_obese <dbl>, grade_level <chr>,
    ## #   number_healthy_weight <dbl>, percent_healthy_weight <dbl>, sex <chr>

``` r
Student_Weight = Student_Weight |> filter(year_reported==2019 & county!='STATEWIDE (EXCLUDING NYC)') |> select(-region, -area_name)
```

``` r
show_student_weight = 
  Student_Weight |>
  head(5) |>
  knitr::kable(digits = 3)
```

``` r
show_student_weight
```

| district | county | year_reported | number_overweight | percent_overweight | number_obese | percent_obese | number_overweight_or_obese | percent_overweight_or_obese | grade_level    | number_healthy_weight | percent_healthy_weight | sex    |
|---------:|:-------|--------------:|------------------:|-------------------:|-------------:|--------------:|---------------------------:|----------------------------:|:---------------|----------------------:|-----------------------:|:-------|
|    10000 | ALBANY |          2019 |              1808 |               16.3 |         1809 |          16.3 |                       3617 |                        32.6 | DISTRICT TOTAL |                  7149 |                  0.644 | ALL    |
|    10000 | ALBANY |          2019 |               904 |               16.6 |          823 |          15.1 |                       1727 |                        31.6 | DISTRICT TOTAL |                  3570 |                  0.654 | FEMALE |
|    10000 | ALBANY |          2019 |               904 |               16.0 |          986 |          17.5 |                       1890 |                        33.5 | DISTRICT TOTAL |                  3579 |                  0.634 | MALE   |
|    10000 | ALBANY |          2019 |              1106 |               15.6 |         1107 |          15.6 |                       2213 |                        31.2 | ELEMENTARY     |                  4651 |                  0.656 | ALL    |
|    10000 | ALBANY |          2019 |               527 |               15.2 |          513 |          14.8 |                       1040 |                        30.0 | ELEMENTARY     |                  2313 |                  0.668 | FEMALE |

Description and what we did: xxxxx

### Demographics Data

``` r
Demographics_all = read_csv("data/Demographics_all.csv")
```

这里先只选了2019年的，我们可以把2015-2019年的分开成4个dataset（每一年的处理code都复制这个就行），这样每一个dataset的size能小点，不至于load不进来

``` r
Demographics_all = 
  Demographics_all |> 
  janitor::clean_names() |>
  mutate(district = as.numeric(district)) |>
  filter(year==2019)

Demographics_all
```

    ## # A tibble: 719 × 16
    ##    district  year num_asian num_black num_hisp num_am_ind num_white num_female
    ##       <dbl> <dbl>     <dbl>     <dbl>    <dbl>      <dbl>     <dbl>      <dbl>
    ##  1    10100  2019       836      4388     1765         29      1883       4641
    ##  2    10201  2019         4         5       19          1       706        369
    ##  3    10306  2019       290       112      155          2      3645       2131
    ##  4    10402  2019        14        74      152          1      1504        909
    ##  5    10500  2019        33       208      174          7      1429       1008
    ##  6    10601  2019       498       395      425          6      3105       2401
    ##  7    10615  2019        95       105       13          3        81        144
    ##  8    10623  2019      1121       382      334         16      3703       2914
    ##  9    10701  2019        NA        33        8         NA       222        153
    ## 10    10802  2019       701       201      187          7      3568       2390
    ## # ℹ 709 more rows
    ## # ℹ 8 more variables: num_male <dbl>, num_lep <dbl>, num_free_lunch <dbl>,
    ## #   num_reduced_lunch <dbl>, num_multi <dbl>, num_swd <dbl>, num_ecdis <dbl>,
    ## #   num_ell <dbl>

### Join Demographics with Student Weight

``` r
result = merge(Student_Weight, Demographics_all, by = "district")|>
  select(-num_free_lunch, -num_reduced_lunch, -num_ell) |>
  filter(grade_level!='DISTRICT TOTAL' & sex!='ALL') |>
  drop_na()

head(result)
```

    ##   district county year_reported number_overweight percent_overweight
    ## 1    10402 ALBANY          2019                28               11.4
    ## 2    10402 ALBANY          2019                35               13.9
    ## 3    10402 ALBANY          2019                21               23.6
    ## 4    10402 ALBANY          2019                 9                9.9
    ## 5    10500 ALBANY          2019                39               17.7
    ## 6    10500 ALBANY          2019                36               20.0
    ##   number_obese percent_obese number_overweight_or_obese
    ## 1           28          11.4                         56
    ## 2           42          16.7                         77
    ## 3           17          19.1                         38
    ## 4           28          30.8                         37
    ## 5           43          19.5                         82
    ## 6           38          21.1                         74
    ##   percent_overweight_or_obese grade_level number_healthy_weight
    ## 1                        22.8  ELEMENTARY                   181
    ## 2                        30.6  ELEMENTARY                   159
    ## 3                        42.7 MIDDLE/HIGH                    51
    ## 4                        40.7 MIDDLE/HIGH                    54
    ## 5                        37.3  ELEMENTARY                   121
    ## 6                        41.1  ELEMENTARY                    96
    ##   percent_healthy_weight    sex year num_asian num_black num_hisp num_am_ind
    ## 1                  0.736 FEMALE 2019        14        74      152          1
    ## 2                  0.631   MALE 2019        14        74      152          1
    ## 3                  0.573 FEMALE 2019        14        74      152          1
    ## 4                  0.593   MALE 2019        14        74      152          1
    ## 5                  0.550   MALE 2019        33       208      174          7
    ## 6                  0.533 FEMALE 2019        33       208      174          7
    ##   num_white num_female num_male num_lep num_multi num_swd num_ecdis
    ## 1      1504        909      934      13        98     320       786
    ## 2      1504        909      934      13        98     320       786
    ## 3      1504        909      934      13        98     320       786
    ## 4      1504        909      934      13        98     320       786
    ## 5      1429       1008     1007      25       164     338      1175
    ## 6      1429       1008     1007      25       164     338      1175

``` r
result  = 
  result |> 
  mutate(total_by_overweight = number_overweight / (percent_overweight/100),
            total_by_obese = number_obese / (percent_obese/100),
            total_by_or = number_overweight_or_obese / (percent_overweight_or_obese/100),
            total_by_healthy = number_healthy_weight / (percent_healthy_weight/100),
            total_students = mean(c(total_by_overweight, total_by_obese, total_by_or, total_by_healthy)))
```

``` r
unique(result$total_students)
```

    ## [1] 6590.401

See the unique grade_level:

``` r
unique(result$grade_level)
```

    ## [1] "ELEMENTARY"  "MIDDLE/HIGH"

``` r
#write.csv(result, file = "result.csv", row.names = FALSE)
```
