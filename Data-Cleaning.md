Untitled
================
Manye Dong
2023-11-16

``` r
library(tidyverse)
library(readr)
```

``` r
Student_Weight = read_csv("data/Student Weight.csv")
```

``` r
Student_Weight  = 
  Student_Weight |>
  janitor::clean_names() |>
  rename('district'='location_code')

Student_Weight$year_reported = sub(".*-", "", Student_Weight$year_reported)
  
Student_Weight = 
  Student_Weight |>
  mutate(year_reported = as.numeric(Student_Weight$year_reported))

head(Student_Weight)
```

    ## # A tibble: 6 × 15
    ##   district county               area_name region year_reported number_overweight
    ##      <dbl> <chr>                <chr>     <chr>          <dbl>             <dbl>
    ## 1        0 STATEWIDE (EXCLUDIN… STATEWID… STATE…          2012             77813
    ## 2        0 STATEWIDE (EXCLUDIN… STATEWID… STATE…          2012             38536
    ## 3        0 STATEWIDE (EXCLUDIN… STATEWID… STATE…          2012             39277
    ## 4        0 STATEWIDE (EXCLUDIN… STATEWID… STATE…          2012             44970
    ## 5        0 STATEWIDE (EXCLUDIN… STATEWID… STATE…          2012             22092
    ## 6        0 STATEWIDE (EXCLUDIN… STATEWID… STATE…          2012             22878
    ## # ℹ 9 more variables: percent_overweight <dbl>, number_obese <dbl>,
    ## #   percent_obese <dbl>, number_overweight_or_obese <dbl>,
    ## #   percent_overweight_or_obese <dbl>, grade_level <chr>,
    ## #   number_healthy_weight <dbl>, percent_healthy_weight <dbl>, sex <chr>

``` r
Student_Weight = Student_Weight |> filter(year_reported==2019 & county!='STATEWIDE (EXCLUDING NYC)') |> select(-region)
Student_Weight
```

    ## # A tibble: 3,636 × 14
    ##    district county  area_name year_reported number_overweight percent_overweight
    ##       <dbl> <chr>   <chr>             <dbl>             <dbl>              <dbl>
    ##  1    10000 ALBANY  ALBANY             2019              1808               16.3
    ##  2    10000 ALBANY  ALBANY             2019               904               16.6
    ##  3    10000 ALBANY  ALBANY             2019               904               16  
    ##  4    10000 ALBANY  ALBANY             2019              1106               15.6
    ##  5    10000 ALBANY  ALBANY             2019               527               15.2
    ##  6    10000 ALBANY  ALBANY             2019               579               16  
    ##  7    10000 ALBANY  ALBANY             2019               689               17.3
    ##  8    10000 ALBANY  ALBANY             2019               369               18.6
    ##  9    10000 ALBANY  ALBANY             2019               320               16  
    ## 10    20000 ALLEGA… ALLEGANY           2019               344               12.9
    ## # ℹ 3,626 more rows
    ## # ℹ 8 more variables: number_obese <dbl>, percent_obese <dbl>,
    ## #   number_overweight_or_obese <dbl>, percent_overweight_or_obese <dbl>,
    ## #   grade_level <chr>, number_healthy_weight <dbl>,
    ## #   percent_healthy_weight <dbl>, sex <chr>

``` r
Demographics_all = read_csv("data/Demographics_all.csv")
```

    ## Rows: 21333 Columns: 16
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): DISTRICT
    ## dbl (15): YEAR, NUM_ASIAN, NUM_BLACK, NUM_HISP, NUM_AM_IND, NUM_WHITE, NUM_F...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

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

``` r
Enrollments_all = read_csv("data/Enrollments_all.csv")
```

    ## Rows: 21331 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): DISTRID
    ## dbl (21): YEAR, PREK, K, G1, G2, G3, G4, G5, G6, G7, G8, G9, G10, G11, G12, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Enrollments_all = 
  Enrollments_all |>
  janitor::clean_names() |>
  mutate(distrid=as.numeric(distrid)) |>
  rename('district' = 'distrid') |>
  filter(year==2019)

head(Enrollments_all)
```

    ## # A tibble: 6 × 22
    ##   district  year  prek     k    g1    g2    g3    g4    g5    g6    g7    g8
    ##      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1    10100  2019   938   632   615   660   660   698   691   679   681   663
    ## 2    10201  2019    36    49    51    65    45    48    54    59    60    69
    ## 3    10306  2019     0   271   314   280   304   294   332   337   334   350
    ## 4    10402  2019    75   137   141   137   130   146   124   150   130   148
    ## 5    10500  2019   142   148   126   155   156   151   155   148   139   132
    ## 6    10601  2019   143   342   323   311   362   359   359   337   368   371
    ## # ℹ 10 more variables: g9 <dbl>, g10 <dbl>, g11 <dbl>, g12 <dbl>, unelem <dbl>,
    ## #   unsec <dbl>, total <dbl>, k12 <dbl>, k6 <dbl>, g712 <dbl>

Join these datasets:

``` r
result = merge(Student_Weight, Demographics_all, by = "district", all.y = TRUE) |>
  select(-num_free_lunch, -num_reduced_lunch, -num_ell) |>
  # 再考虑一下 只drop没有district和county的data
  drop_na()

head(result)
```

    ##   district county                                       area_name year_reported
    ## 1    10402 ALBANY RAVENA COEYMANS SELKIRK CENTRAL SCHOOL DISTRICT          2019
    ## 2    10402 ALBANY RAVENA COEYMANS SELKIRK CENTRAL SCHOOL DISTRICT          2019
    ## 3    10402 ALBANY RAVENA COEYMANS SELKIRK CENTRAL SCHOOL DISTRICT          2019
    ## 4    10402 ALBANY RAVENA COEYMANS SELKIRK CENTRAL SCHOOL DISTRICT          2019
    ## 5    10402 ALBANY RAVENA COEYMANS SELKIRK CENTRAL SCHOOL DISTRICT          2019
    ## 6    10402 ALBANY RAVENA COEYMANS SELKIRK CENTRAL SCHOOL DISTRICT          2019
    ##   number_overweight percent_overweight number_obese percent_obese
    ## 1                93               13.7          115          16.9
    ## 2                49               14.6           45          13.4
    ## 3                44               12.7           70          20.2
    ## 4                63               12.7           70          14.1
    ## 5                28               11.4           28          11.4
    ## 6                35               13.9           42          16.7
    ##   number_overweight_or_obese percent_overweight_or_obese    grade_level
    ## 1                        208                        30.5 DISTRICT TOTAL
    ## 2                         94                        28.1 DISTRICT TOTAL
    ## 3                        114                        32.9 DISTRICT TOTAL
    ## 4                        133                        26.7     ELEMENTARY
    ## 5                         56                        22.8     ELEMENTARY
    ## 6                         77                        30.6     ELEMENTARY
    ##   number_healthy_weight percent_healthy_weight    sex year num_asian num_black
    ## 1                   445                  0.653    ALL 2019        14        74
    ## 2                   232                  0.693 FEMALE 2019        14        74
    ## 3                   213                  0.616   MALE 2019        14        74
    ## 4                   340                  0.683    ALL 2019        14        74
    ## 5                   181                  0.736 FEMALE 2019        14        74
    ## 6                   159                  0.631   MALE 2019        14        74
    ##   num_hisp num_am_ind num_white num_female num_male num_lep num_multi num_swd
    ## 1      152          1      1504        909      934      13        98     320
    ## 2      152          1      1504        909      934      13        98     320
    ## 3      152          1      1504        909      934      13        98     320
    ## 4      152          1      1504        909      934      13        98     320
    ## 5      152          1      1504        909      934      13        98     320
    ## 6      152          1      1504        909      934      13        98     320
    ##   num_ecdis
    ## 1       786
    ## 2       786
    ## 3       786
    ## 4       786
    ## 5       786
    ## 6       786

``` r
#write.csv(result, file = "result.csv", row.names = FALSE)
```
