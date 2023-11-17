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
Student_Weight  = 
  Student_Weight |>
  janitor::clean_names() |>
  rename('district'='location_code') |>
  filter(year_reported %in% c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019"))
```

``` r
Student_Weight$year_reported = sub(".*-", "", Student_Weight$year_reported)

Student_Weight = 
  Student_Weight |>
  mutate(year_reported = as.numeric(Student_Weight$year_reported)) |>
  filter(grade_level!="DISTRICT TOTAL" & sex!="ALL")
```

``` r
Student_Weight = Student_Weight |> filter(year_reported==2019 & county!='STATEWIDE (EXCLUDING NYC)') |> select(-region, -area_name)
```

``` r
Student_Weight |> group_by(county)
```

    ## # A tibble: 1,360 × 13
    ## # Groups:   county [57]
    ##    district county year_reported number_overweight percent_overweight
    ##       <dbl> <chr>          <dbl>             <dbl>              <dbl>
    ##  1    10402 ALBANY          2019                28               11.4
    ##  2    10402 ALBANY          2019                35               13.9
    ##  3    10402 ALBANY          2019                21               23.6
    ##  4    10402 ALBANY          2019                 9                9.9
    ##  5    10500 ALBANY          2019                36               20  
    ##  6    10500 ALBANY          2019                39               17.7
    ##  7    10500 ALBANY          2019                18               16.7
    ##  8    10500 ALBANY          2019                16               20  
    ##  9    10601 ALBANY          2019                70               13.8
    ## 10    10601 ALBANY          2019                81               16.5
    ## # ℹ 1,350 more rows
    ## # ℹ 8 more variables: number_obese <dbl>, percent_obese <dbl>,
    ## #   number_overweight_or_obese <dbl>, percent_overweight_or_obese <dbl>,
    ## #   grade_level <chr>, number_healthy_weight <dbl>,
    ## #   percent_healthy_weight <dbl>, sex <chr>

``` r
show_student_weight = 
  Student_Weight |>
  head(5) |>
  knitr::kable(digits = 3)
```

Description and what we did: xxxxx

### Demographics Data

``` r
Demographics_all = read_csv("data/Demographics_all.csv")
```

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
  drop_na()

head(result)
```

    ##   district county year_reported number_overweight percent_overweight
    ## 1    10402 ALBANY          2019                28               11.4
    ## 2    10402 ALBANY          2019                35               13.9
    ## 3    10402 ALBANY          2019                21               23.6
    ## 4    10402 ALBANY          2019                 9                9.9
    ## 5    10500 ALBANY          2019                16               20.0
    ## 6    10500 ALBANY          2019                36               20.0
    ##   number_obese percent_obese number_overweight_or_obese
    ## 1           28          11.4                         56
    ## 2           42          16.7                         77
    ## 3           17          19.1                         38
    ## 4           28          30.8                         37
    ## 5           22          27.5                         38
    ## 6           38          21.1                         74
    ##   percent_overweight_or_obese grade_level number_healthy_weight
    ## 1                        22.8  ELEMENTARY                   181
    ## 2                        30.6  ELEMENTARY                   159
    ## 3                        42.7 MIDDLE/HIGH                    51
    ## 4                        40.7 MIDDLE/HIGH                    54
    ## 5                        47.5 MIDDLE/HIGH                    42
    ## 6                        41.1  ELEMENTARY                    96
    ##   percent_healthy_weight    sex year num_asian num_black num_hisp num_am_ind
    ## 1                  0.736 FEMALE 2019        14        74      152          1
    ## 2                  0.631   MALE 2019        14        74      152          1
    ## 3                  0.573 FEMALE 2019        14        74      152          1
    ## 4                  0.593   MALE 2019        14        74      152          1
    ## 5                  0.525   MALE 2019        33       208      174          7
    ## 6                  0.533 FEMALE 2019        33       208      174          7
    ##   num_white num_female num_male num_lep num_multi num_swd num_ecdis
    ## 1      1504        909      934      13        98     320       786
    ## 2      1504        909      934      13        98     320       786
    ## 3      1504        909      934      13        98     320       786
    ## 4      1504        909      934      13        98     320       786
    ## 5      1429       1008     1007      25       164     338      1175
    ## 6      1429       1008     1007      25       164     338      1175

``` r
result  = result |>
  group_by(county, grade_level, sex) |>
  summarise(total_overweight = sum(number_overweight), 
            total_obese = sum(number_overweight),
            total_overweight_or_obese = sum(number_overweight_or_obese),
            total_healthy = sum(number_healthy_weight),
            num_asian = num_asian,
            num_black = num_black,
            num_hisp = num_hisp,
            num_am_ind = num_am_ind, 
            num_white = num_white,
            num_female = num_female, 
            num_male = num_male, 
            num_lep = num_lep,
            num_multi = num_multi,
            num_swd = num_swd,
            num_ecdis = num_ecdis
)
```

    ## Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr 1.1.0.
    ## ℹ Please use `reframe()` instead.
    ## ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()` always returns an
    ##   ungrouped data frame and adjust accordingly.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

    ## `summarise()` has grouped output by 'county', 'grade_level', 'sex'. You can override using the
    ## `.groups` argument.

``` r
result
```

    ## # A tibble: 831 × 18
    ## # Groups:   county, grade_level, sex [198]
    ##    county grade_level sex    total_overweight total_obese total_overweight_or_…¹
    ##    <chr>  <chr>       <chr>             <dbl>       <dbl>                  <dbl>
    ##  1 ALBANY ELEMENTARY  FEMALE              297         297                    590
    ##  2 ALBANY ELEMENTARY  FEMALE              297         297                    590
    ##  3 ALBANY ELEMENTARY  FEMALE              297         297                    590
    ##  4 ALBANY ELEMENTARY  FEMALE              297         297                    590
    ##  5 ALBANY ELEMENTARY  FEMALE              297         297                    590
    ##  6 ALBANY ELEMENTARY  MALE                316         316                    635
    ##  7 ALBANY ELEMENTARY  MALE                316         316                    635
    ##  8 ALBANY ELEMENTARY  MALE                316         316                    635
    ##  9 ALBANY ELEMENTARY  MALE                316         316                    635
    ## 10 ALBANY ELEMENTARY  MALE                316         316                    635
    ## # ℹ 821 more rows
    ## # ℹ abbreviated name: ¹​total_overweight_or_obese
    ## # ℹ 12 more variables: total_healthy <dbl>, num_asian <dbl>, num_black <dbl>,
    ## #   num_hisp <dbl>, num_am_ind <dbl>, num_white <dbl>, num_female <dbl>,
    ## #   num_male <dbl>, num_lep <dbl>, num_multi <dbl>, num_swd <dbl>,
    ## #   num_ecdis <dbl>

``` r
write.csv(result, file = "result_2019.csv", row.names = FALSE)
```
