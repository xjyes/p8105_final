Data Cleaning
================
Manye Dong
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

其实另一个enrollment这个dataset有没有都不太重要哈哈哈，最后也不太需要它的info，我就先给删了

### Join Demographics with Student Weight

不知道有没有办法让free
lunch这几个column都不要变成NA哈哈哈，实在不行就算了

``` r
result = merge(Student_Weight, Demographics_all, by = "district")|>
  select(-num_free_lunch, -num_reduced_lunch, -num_ell) |>
  drop_na()

result
```

    ##      district       county year_reported number_overweight percent_overweight
    ## 1       10402       ALBANY          2019                93               13.7
    ## 2       10402       ALBANY          2019                49               14.6
    ## 3       10402       ALBANY          2019                44               12.7
    ## 4       10402       ALBANY          2019                63               12.7
    ## 5       10402       ALBANY          2019                28               11.4
    ## 6       10402       ALBANY          2019                35               13.9
    ## 7       10402       ALBANY          2019                30               16.7
    ## 8       10402       ALBANY          2019                21               23.6
    ## 9       10402       ALBANY          2019                 9                9.9
    ## 10      10500       ALBANY          2019                39               17.7
    ## 11      10500       ALBANY          2019                34               18.1
    ## 12      10500       ALBANY          2019                75               18.8
    ## 13      10500       ALBANY          2019                36               20.0
    ## 14      10500       ALBANY          2019               109               18.5
    ## 15      10500       ALBANY          2019                54               18.8
    ## 16      10500       ALBANY          2019                55               18.3
    ## 17      10500       ALBANY          2019                18               16.7
    ## 18      10500       ALBANY          2019                16               20.0
    ## 19      10601       ALBANY          2019                60               19.1
    ## 20      10601       ALBANY          2019               257               16.0
    ## 21      10601       ALBANY          2019               130               15.8
    ## 22      10601       ALBANY          2019                46               15.7
    ## 23      10601       ALBANY          2019               151               15.1
    ## 24      10601       ALBANY          2019                70               13.8
    ## 25      10601       ALBANY          2019                81               16.5
    ## 26      10601       ALBANY          2019               127               16.2
    ## 27      10601       ALBANY          2019               106               17.5
    ## 28      10623       ALBANY          2019               334               17.2
    ## 29      10623       ALBANY          2019               179               18.6
    ## 30      10623       ALBANY          2019               155               15.8
    ## 31      10623       ALBANY          2019               165               15.8
    ## 32      10623       ALBANY          2019                89               16.7
    ## 33      10623       ALBANY          2019                76               14.9
    ## 34      10623       ALBANY          2019               169               18.7
    ## 35      10623       ALBANY          2019                90               21.0
    ## 36      10623       ALBANY          2019                79               16.7
    ## 37      10802       ALBANY          2019               284               17.4
    ## 38      10802       ALBANY          2019               143               17.4
    ## 39      10802       ALBANY          2019                69               21.7
    ## 40      10802       ALBANY          2019                56               17.3
    ## 41      10802       ALBANY          2019                74               14.7
    ## 42      10802       ALBANY          2019                85               17.4
    ## 43      10802       ALBANY          2019               141               17.3
    ## 44      10802       ALBANY          2019               159               16.0
    ## 45      10802       ALBANY          2019               125               19.5
    ## 46      22601     ALLEGANY          2019                21               16.7
    ## 47      22601     ALLEGANY          2019                35               16.1
    ## 48      22601     ALLEGANY          2019                40               16.1
    ## 49      22601     ALLEGANY          2019                19               15.4
    ## 50      22601     ALLEGANY          2019                76               17.7
    ## 51      22601     ALLEGANY          2019                41               19.3
    ## 52      22601     ALLEGANY          2019                14               16.7
    ## 53      22601     ALLEGANY          2019                36               21.1
    ## 54      22601     ALLEGANY          2019                22               25.3
    ## 55      30101       BROOME          2019                86               17.8
    ## 56      30101       BROOME          2019                50               22.8
    ## 57      30101       BROOME          2019                36               13.6
    ## 58      30101       BROOME          2019                48               18.5
    ## 59      30101       BROOME          2019                25               21.9
    ## 60      30101       BROOME          2019                23               15.8
    ## 61      30101       BROOME          2019                38               17.0
    ## 62      30101       BROOME          2019                25               23.8
    ## 63      30101       BROOME          2019                13               11.0
    ## 64      30200       BROOME          2019                80               17.1
    ## 65      30200       BROOME          2019                55               14.4
    ## 66      30200       BROOME          2019               144               15.6
    ## 67      30200       BROOME          2019                64               14.0
    ## 68      30200       BROOME          2019               199               15.2
    ## 69      30200       BROOME          2019                93               14.4
    ## 70      30200       BROOME          2019               106               16.0
    ## 71      30200       BROOME          2019                29               15.5
    ## 72      30200       BROOME          2019                26               13.3
    ## 73      30501       BROOME          2019                39               14.2
    ## 74      30501       BROOME          2019                15               12.3
    ## 75      30501       BROOME          2019                 6               12.5
    ## 76      30501       BROOME          2019                 9               17.3
    ## 77      30501       BROOME          2019                 9               12.2
    ## 78      30501       BROOME          2019                15               14.9
    ## 79      30501       BROOME          2019                24               15.7
    ## 80      30501       BROOME          2019                24               13.7
    ## 81      30501       BROOME          2019                15               15.0
    ## 82      30601       BROOME          2019                90               18.4
    ## 83      30601       BROOME          2019                39               18.1
    ## 84      30601       BROOME          2019                31               19.3
    ## 85      30601       BROOME          2019                51               18.7
    ## 86      30601       BROOME          2019                60               19.0
    ## 87      30601       BROOME          2019                29               18.8
    ## 88      30601       BROOME          2019                30               17.3
    ## 89      30601       BROOME          2019                10               16.1
    ## 90      30601       BROOME          2019                20               18.0
    ## 91      30701       BROOME          2019                68               16.7
    ## 92      30701       BROOME          2019                45               19.5
    ## 93      30701       BROOME          2019                83               15.1
    ## 94      30701       BROOME          2019                39               14.7
    ## 95      30701       BROOME          2019                44               15.5
    ## 96      30701       BROOME          2019               128               16.3
    ## 97      30701       BROOME          2019                60               15.9
    ## 98      30701       BROOME          2019                21               19.1
    ## 99      30701       BROOME          2019                24               19.8
    ## 100     40302  CATTARAUGUS          2019                83               16.5
    ## 101     40302  CATTARAUGUS          2019                48               20.3
    ## 102     40302  CATTARAUGUS          2019                35               13.1
    ## 103     40302  CATTARAUGUS          2019                51               16.8
    ## 104     40302  CATTARAUGUS          2019                31               21.1
    ## 105     40302  CATTARAUGUS          2019                20               12.7
    ## 106     40302  CATTARAUGUS          2019                32               16.3
    ## 107     40302  CATTARAUGUS          2019                17               18.9
    ## 108     40302  CATTARAUGUS          2019                15               14.2
    ## 109     40901  CATTARAUGUS          2019                11               11.8
    ## 110     40901  CATTARAUGUS          2019                23               16.9
    ## 111     40901  CATTARAUGUS          2019                23               13.7
    ## 112     40901  CATTARAUGUS          2019                12               16.0
    ## 113     40901  CATTARAUGUS          2019                48               18.8
    ## 114     40901  CATTARAUGUS          2019                25               21.0
    ## 115     40901  CATTARAUGUS          2019                25               28.7
    ## 116     40901  CATTARAUGUS          2019                13               29.5
    ## 117     40901  CATTARAUGUS          2019                12               27.9
    ## 118     41401  CATTARAUGUS          2019                 5                9.6
    ## 119     41401  CATTARAUGUS          2019                29               19.7
    ## 120     41401  CATTARAUGUS          2019                22               24.7
    ## 121     41401  CATTARAUGUS          2019                14               28.6
    ## 122     41401  CATTARAUGUS          2019                16               20.8
    ## 123     41401  CATTARAUGUS          2019                13               18.6
    ## 124     41401  CATTARAUGUS          2019                 8               20.0
    ## 125     42400  CATTARAUGUS          2019               134               16.9
    ## 126     42400  CATTARAUGUS          2019                66               17.7
    ## 127     42400  CATTARAUGUS          2019                68               16.1
    ## 128     42400  CATTARAUGUS          2019                85               16.3
    ## 129     42400  CATTARAUGUS          2019                40               16.5
    ## 130     42400  CATTARAUGUS          2019                45               16.1
    ## 131     42400  CATTARAUGUS          2019                49               17.9
    ## 132     42400  CATTARAUGUS          2019                26               20.0
    ## 133     42400  CATTARAUGUS          2019                23               16.0
    ## 134     43001  CATTARAUGUS          2019                12               13.6
    ## 135     43001  CATTARAUGUS          2019                23               14.2
    ## 136     43001  CATTARAUGUS          2019                30               17.6
    ## 137     43001  CATTARAUGUS          2019                18               22.0
    ## 138     43001  CATTARAUGUS          2019                53               16.7
    ## 139     43001  CATTARAUGUS          2019                30               19.2
    ## 140     43001  CATTARAUGUS          2019                23               16.2
    ## 141     43001  CATTARAUGUS          2019                12               16.2
    ## 142     43001  CATTARAUGUS          2019                11               16.2
    ## 143     43200  CATTARAUGUS          2019                75               21.6
    ## 144     43200  CATTARAUGUS          2019                22               18.6
    ## 145     43200  CATTARAUGUS          2019                10               22.7
    ## 146     43200  CATTARAUGUS          2019                12               16.2
    ## 147     43200  CATTARAUGUS          2019                24               23.3
    ## 148     43200  CATTARAUGUS          2019                34               23.1
    ## 149     43200  CATTARAUGUS          2019                41               20.4
    ## 150     43200  CATTARAUGUS          2019                53               23.0
    ## 151     43200  CATTARAUGUS          2019                29               22.8
    ## 152     50401       CAYUGA          2019                18               11.4
    ## 153     50401       CAYUGA          2019                22               16.8
    ## 154     50401       CAYUGA          2019                14                8.8
    ## 155     50401       CAYUGA          2019                 7                9.3
    ## 156     50401       CAYUGA          2019                 7                8.2
    ## 157     50401       CAYUGA          2019                18               13.0
    ## 158     50401       CAYUGA          2019                36               12.2
    ## 159     50401       CAYUGA          2019                11               17.5
    ## 160     50401       CAYUGA          2019                11               16.2
    ## 161     51301       CAYUGA          2019                64               18.5
    ## 162     51301       CAYUGA          2019                34               19.1
    ## 163     51301       CAYUGA          2019                13               21.3
    ## 164     51301       CAYUGA          2019                10               17.9
    ## 165     51301       CAYUGA          2019                21               17.9
    ## 166     51301       CAYUGA          2019                20               18.2
    ## 167     51301       CAYUGA          2019                30               17.9
    ## 168     51301       CAYUGA          2019                41               18.1
    ## 169     51301       CAYUGA          2019                23               19.7
    ## 170     60301   CHAUTAUQUA          2019                53               21.5
    ## 171     60301   CHAUTAUQUA          2019                34               28.1
    ## 172     60301   CHAUTAUQUA          2019                19               15.2
    ## 173     60301   CHAUTAUQUA          2019                34               23.6
    ## 174     60301   CHAUTAUQUA          2019                22               28.9
    ## 175     60301   CHAUTAUQUA          2019                12               17.6
    ## 176     60301   CHAUTAUQUA          2019                19               18.6
    ## 177     60301   CHAUTAUQUA          2019                12               26.7
    ## 178     60301   CHAUTAUQUA          2019                 7               12.3
    ## 179     60401   CHAUTAUQUA          2019                23               21.7
    ## 180     60401   CHAUTAUQUA          2019                34               19.0
    ## 181     60401   CHAUTAUQUA          2019                49               24.6
    ## 182     60401   CHAUTAUQUA          2019                26               28.0
    ## 183     60401   CHAUTAUQUA          2019                76               22.4
    ## 184     60401   CHAUTAUQUA          2019                42               26.3
    ## 185     60401   CHAUTAUQUA          2019                27               19.3
    ## 186     60401   CHAUTAUQUA          2019                16               23.9
    ## 187     60401   CHAUTAUQUA          2019                11               15.1
    ## 188     60800   CHAUTAUQUA          2019                96               15.6
    ## 189     60800   CHAUTAUQUA          2019                47               15.6
    ## 190     60800   CHAUTAUQUA          2019                49               15.7
    ## 191     60800   CHAUTAUQUA          2019                46               12.9
    ## 192     60800   CHAUTAUQUA          2019                26               14.9
    ## 193     60800   CHAUTAUQUA          2019                20               10.9
    ## 194     60800   CHAUTAUQUA          2019                50               19.5
    ## 195     60800   CHAUTAUQUA          2019                21               16.4
    ## 196     60800   CHAUTAUQUA          2019                29               22.5
    ## 197     61501   CHAUTAUQUA          2019                13               12.7
    ## 198     61501   CHAUTAUQUA          2019                26               14.5
    ## 199     61501   CHAUTAUQUA          2019                33               14.9
    ## 200     61501   CHAUTAUQUA          2019                20               16.7
    ## 201     61501   CHAUTAUQUA          2019                64               16.1
    ## 202     61501   CHAUTAUQUA          2019                38               17.4
    ## 203     61501   CHAUTAUQUA          2019                31               17.7
    ## 204     61501   CHAUTAUQUA          2019                18               18.4
    ## 205     61501   CHAUTAUQUA          2019                13               16.9
    ## 206     62301   CHAUTAUQUA          2019                42               21.5
    ## 207     62301   CHAUTAUQUA          2019                18               26.9
    ## 208     62301   CHAUTAUQUA          2019                22               22.9
    ## 209     62301   CHAUTAUQUA          2019                20               20.2
    ## 210     62301   CHAUTAUQUA          2019                32               23.7
    ## 211     62301   CHAUTAUQUA          2019                 6               19.4
    ## 212     62301   CHAUTAUQUA          2019                14               20.6
    ## 213     62301   CHAUTAUQUA          2019                 6               10.7
    ## 214     62601   CHAUTAUQUA          2019                12               17.6
    ## 215     62601   CHAUTAUQUA          2019                 7               10.0
    ## 216     62601   CHAUTAUQUA          2019                12               14.1
    ## 217     62601   CHAUTAUQUA          2019                 5               12.8
    ## 218     62601   CHAUTAUQUA          2019                 7               15.2
    ## 219     62601   CHAUTAUQUA          2019                19               13.8
    ## 220     62601   CHAUTAUQUA          2019                 7               13.2
    ## 221     62601   CHAUTAUQUA          2019                 7               24.1
    ## 222     62901   CHAUTAUQUA          2019                43               15.8
    ## 223     62901   CHAUTAUQUA          2019                13               15.3
    ## 224     62901   CHAUTAUQUA          2019                 7               15.6
    ## 225     62901   CHAUTAUQUA          2019                 6               15.0
    ## 226     62901   CHAUTAUQUA          2019                14               16.3
    ## 227     62901   CHAUTAUQUA          2019                21               15.9
    ## 228     62901   CHAUTAUQUA          2019                22               15.6
    ## 229     62901   CHAUTAUQUA          2019                30               16.6
    ## 230     62901   CHAUTAUQUA          2019                16               16.8
    ## 231     70600      CHEMUNG          2019               472               21.3
    ## 232     70600      CHEMUNG          2019               238               22.2
    ## 233     70600      CHEMUNG          2019               234               20.5
    ## 234     70600      CHEMUNG          2019               283               21.2
    ## 235     70600      CHEMUNG          2019               140               21.9
    ## 236     70600      CHEMUNG          2019               143               20.6
    ## 237     70600      CHEMUNG          2019               189               21.6
    ## 238     70600      CHEMUNG          2019                98               22.7
    ## 239     70600      CHEMUNG          2019                91               20.4
    ## 240     80601     CHENANGO          2019                32               18.0
    ## 241     80601     CHENANGO          2019                24               15.3
    ## 242     80601     CHENANGO          2019                31               15.3
    ## 243     80601     CHENANGO          2019                20               19.0
    ## 244     80601     CHENANGO          2019                56               16.7
    ## 245     80601     CHENANGO          2019                25               18.9
    ## 246     80601     CHENANGO          2019                12               16.4
    ## 247     80601     CHENANGO          2019                13               22.0
    ## 248     80601     CHENANGO          2019                11               11.2
    ## 249     81003     CHENANGO          2019                10               12.7
    ## 250     81003     CHENANGO          2019                19               18.1
    ## 251     81003     CHENANGO          2019                11               21.6
    ## 252     81003     CHENANGO          2019                 8               14.8
    ## 253     81003     CHENANGO          2019                28               18.1
    ## 254     81003     CHENANGO          2019                18               13.5
    ## 255     81003     CHENANGO          2019                27               14.8
    ## 256     81003     CHENANGO          2019                46               16.0
    ## 257     81003     CHENANGO          2019                17               16.3
    ## 258     81200     CHENANGO          2019               119               18.0
    ## 259     81200     CHENANGO          2019                63               18.9
    ## 260     81200     CHENANGO          2019                56               17.1
    ## 261     81200     CHENANGO          2019                66               16.5
    ## 262     81200     CHENANGO          2019                32               16.0
    ## 263     81200     CHENANGO          2019                34               17.1
    ## 264     81200     CHENANGO          2019                53               20.2
    ## 265     81200     CHENANGO          2019                31               23.1
    ## 266     81200     CHENANGO          2019                22               17.2
    ## 267     90201      CLINTON          2019                65               15.5
    ## 268     90201      CLINTON          2019                36               17.4
    ## 269     90201      CLINTON          2019                13               18.3
    ## 270     90201      CLINTON          2019                 9               11.5
    ## 271     90201      CLINTON          2019                23               17.6
    ## 272     90201      CLINTON          2019                20               15.5
    ## 273     90201      CLINTON          2019                29               13.6
    ## 274     90201      CLINTON          2019                43               16.5
    ## 275     90201      CLINTON          2019                22               14.8
    ## 276     90301      CLINTON          2019               116               16.2
    ## 277     90301      CLINTON          2019                60               17.1
    ## 278     90301      CLINTON          2019                37               15.0
    ## 279     90301      CLINTON          2019                56               15.2
    ## 280     90301      CLINTON          2019                73               15.6
    ## 281     90301      CLINTON          2019                36               16.2
    ## 282     90301      CLINTON          2019                43               17.2
    ## 283     90301      CLINTON          2019                24               18.8
    ## 284     90301      CLINTON          2019                19               15.6
    ## 285     91101      CLINTON          2019               114               15.8
    ## 286     91101      CLINTON          2019                70               19.0
    ## 287     91101      CLINTON          2019                34               23.4
    ## 288     91101      CLINTON          2019                17               11.3
    ## 289     91101      CLINTON          2019                36               16.4
    ## 290     91101      CLINTON          2019                27               13.2
    ## 291     91101      CLINTON          2019                44               12.4
    ## 292     91101      CLINTON          2019                63               14.9
    ## 293     91101      CLINTON          2019                51               17.2
    ## 294    100501     COLUMBIA          2019                76               17.7
    ## 295    100501     COLUMBIA          2019                40               19.3
    ## 296    100501     COLUMBIA          2019                36               16.1
    ## 297    100501     COLUMBIA          2019                39               15.2
    ## 298    100501     COLUMBIA          2019                20               17.1
    ## 299    100501     COLUMBIA          2019                19               13.7
    ## 300    100501     COLUMBIA          2019                37               22.3
    ## 301    100501     COLUMBIA          2019                20               23.3
    ## 302    100501     COLUMBIA          2019                17               21.3
    ## 303    101300     COLUMBIA          2019                31               13.5
    ## 304    101300     COLUMBIA          2019                41               13.2
    ## 305    101300     COLUMBIA          2019                71               15.6
    ## 306    101300     COLUMBIA          2019                40               17.7
    ## 307    101300     COLUMBIA          2019                97               14.7
    ## 308    101300     COLUMBIA          2019                56               16.1
    ## 309    101300     COLUMBIA          2019                26               13.1
    ## 310    101300     COLUMBIA          2019                16               13.4
    ## 311    101300     COLUMBIA          2019                10               12.7
    ## 312    131601     DUTCHESS          2019               367               15.5
    ## 313    131601     DUTCHESS          2019               190               16.9
    ## 314    131601     DUTCHESS          2019                98               17.1
    ## 315    131601     DUTCHESS          2019                92               16.7
    ## 316    131601     DUTCHESS          2019                91               15.1
    ## 317    131601     DUTCHESS          2019               189               16.1
    ## 318    131601     DUTCHESS          2019               178               15.0
    ## 319    131601     DUTCHESS          2019               177               14.3
    ## 320    131601     DUTCHESS          2019                86               13.5
    ## 321    131602     DUTCHESS          2019                82               16.9
    ## 322    131602     DUTCHESS          2019                19               15.3
    ## 323    131602     DUTCHESS          2019                43               19.5
    ## 324    131602     DUTCHESS          2019                39               14.7
    ## 325    131602     DUTCHESS          2019                46               16.4
    ## 326    131602     DUTCHESS          2019                12               11.3
    ## 327    131602     DUTCHESS          2019                27               17.3
    ## 328    131602     DUTCHESS          2019                36               17.8
    ## 329    131602     DUTCHESS          2019                24               25.0
    ## 330    131701     DUTCHESS          2019                42               17.1
    ## 331    131701     DUTCHESS          2019                40               14.1
    ## 332    131701     DUTCHESS          2019                39               13.2
    ## 333    131701     DUTCHESS          2019                20               14.4
    ## 334    131701     DUTCHESS          2019                19               12.2
    ## 335    131701     DUTCHESS          2019                82               15.5
    ## 336    131701     DUTCHESS          2019                43               18.5
    ## 337    131701     DUTCHESS          2019                22               20.8
    ## 338    131701     DUTCHESS          2019                21               16.5
    ## 339    131801     DUTCHESS          2019                42               15.1
    ## 340    131801     DUTCHESS          2019                22               15.5
    ## 341    131801     DUTCHESS          2019                14               21.9
    ## 342    131801     DUTCHESS          2019                 8               10.3
    ## 343    131801     DUTCHESS          2019                12               18.2
    ## 344    131801     DUTCHESS          2019                26               20.0
    ## 345    131801     DUTCHESS          2019                16               10.8
    ## 346    131801     DUTCHESS          2019                20               15.0
    ## 347    131801     DUTCHESS          2019                 8               11.9
    ## 348    140101         ERIE          2019                50               13.5
    ## 349    140101         ERIE          2019                46               15.0
    ## 350    140101         ERIE          2019                59               13.6
    ## 351    140101         ERIE          2019                33               14.0
    ## 352    140101         ERIE          2019                96               14.2
    ## 353    140101         ERIE          2019                17               13.0
    ## 354    140101         ERIE          2019                20               19.2
    ## 355    140101         ERIE          2019                26               13.1
    ## 356    140101         ERIE          2019                37               15.7
    ## 357    140203         ERIE          2019               132               13.5
    ## 358    140203         ERIE          2019               268               16.5
    ## 359    140203         ERIE          2019               135               16.5
    ## 360    140203         ERIE          2019               133               16.6
    ## 361    140203         ERIE          2019               265               14.9
    ## 362    140203         ERIE          2019               271               14.0
    ## 363    140203         ERIE          2019               539               15.2
    ## 364    140203         ERIE          2019               274               15.5
    ## 365    140203         ERIE          2019               139               14.6
    ## 366    140207         ERIE          2019               178               14.2
    ## 367    140207         ERIE          2019                74               12.8
    ## 368    140207         ERIE          2019               104               15.4
    ## 369    140207         ERIE          2019               112               14.6
    ## 370    140207         ERIE          2019                46               13.0
    ## 371    140207         ERIE          2019                66               15.9
    ## 372    140207         ERIE          2019                66               13.7
    ## 373    140207         ERIE          2019                28               12.6
    ## 374    140207         ERIE          2019                38               14.6
    ## 375    140701         ERIE          2019                48               19.4
    ## 376    140701         ERIE          2019                69               16.2
    ## 377    140701         ERIE          2019                90               17.8
    ## 378    140701         ERIE          2019                42               16.3
    ## 379    140701         ERIE          2019               140               16.6
    ## 380    140701         ERIE          2019                71               17.1
    ## 381    140701         ERIE          2019                50               15.2
    ## 382    140701         ERIE          2019                29               18.8
    ## 383    140701         ERIE          2019                21               12.1
    ## 384    141101         ERIE          2019               105               16.0
    ## 385    141101         ERIE          2019                39               15.2
    ## 386    141101         ERIE          2019                18               14.2
    ## 387    141101         ERIE          2019                21               16.3
    ## 388    141101         ERIE          2019                32               16.5
    ## 389    141101         ERIE          2019                50               15.2
    ## 390    141101         ERIE          2019                55               16.8
    ## 391    141101         ERIE          2019                66               16.9
    ## 392    141101         ERIE          2019                34               17.3
    ## 393    141401         ERIE          2019                86               16.9
    ## 394    141401         ERIE          2019                27               18.1
    ## 395    141401         ERIE          2019                45               17.5
    ## 396    141401         ERIE          2019                41               16.2
    ## 397    141401         ERIE          2019                48               16.1
    ## 398    141401         ERIE          2019                20               19.2
    ## 399    141401         ERIE          2019                21               14.1
    ## 400    141401         ERIE          2019                38               18.0
    ## 401    141401         ERIE          2019                18               16.8
    ## 402    141501         ERIE          2019                77               17.1
    ## 403    141501         ERIE          2019                71               14.5
    ## 404    141501         ERIE          2019                81               14.3
    ## 405    141501         ERIE          2019                37               13.5
    ## 406    141501         ERIE          2019                44               15.2
    ## 407    141501         ERIE          2019               148               15.8
    ## 408    141501         ERIE          2019                67               18.1
    ## 409    141501         ERIE          2019                40               23.5
    ## 410    141501         ERIE          2019                27               13.5
    ## 411    141601         ERIE          2019               122               15.1
    ## 412    141601         ERIE          2019                36               12.5
    ## 413    141601         ERIE          2019                21               13.5
    ## 414    141601         ERIE          2019                15               11.3
    ## 415    141601         ERIE          2019                46               19.0
    ## 416    141601         ERIE          2019                67               16.9
    ## 417    141601         ERIE          2019                55               13.4
    ## 418    141601         ERIE          2019                86               16.6
    ## 419    141601         ERIE          2019                40               14.5
    ## 420    141604         ERIE          2019               258               15.1
    ## 421    141604         ERIE          2019               131               15.0
    ## 422    141604         ERIE          2019               127               15.2
    ## 423    141604         ERIE          2019               149               13.6
    ## 424    141604         ERIE          2019                75               13.4
    ## 425    141604         ERIE          2019                74               13.8
    ## 426    141604         ERIE          2019               109               17.7
    ## 427    141604         ERIE          2019                56               17.7
    ## 428    141604         ERIE          2019                53               17.7
    ## 429    141800         ERIE          2019                63               20.3
    ## 430    141800         ERIE          2019                57               13.7
    ## 431    141800         ERIE          2019                81               15.4
    ## 432    141800         ERIE          2019                44               20.3
    ## 433    141800         ERIE          2019               120               16.5
    ## 434    141800         ERIE          2019                20               19.6
    ## 435    141800         ERIE          2019                37               11.9
    ## 436    141800         ERIE          2019                39               20.1
    ## 437    141800         ERIE          2019                19               20.7
    ## 438    141901         ERIE          2019                96               17.0
    ## 439    141901         ERIE          2019               128               15.6
    ## 440    141901         ERIE          2019                76               19.6
    ## 441    141901         ERIE          2019                52               12.1
    ## 442    141901         ERIE          2019               182               16.9
    ## 443    141901         ERIE          2019               310               16.3
    ## 444    141901         ERIE          2019               162               18.0
    ## 445    141901         ERIE          2019               148               14.8
    ## 446    141901         ERIE          2019                86               16.8
    ## 447    142101         ERIE          2019               114               22.5
    ## 448    142101         ERIE          2019                55               23.2
    ## 449    142101         ERIE          2019                59               21.9
    ## 450    142101         ERIE          2019                81               29.2
    ## 451    142101         ERIE          2019                34               26.0
    ## 452    142101         ERIE          2019                47               32.2
    ## 453    142101         ERIE          2019                33               14.9
    ## 454    142101         ERIE          2019                21               19.8
    ## 455    142101         ERIE          2019                12               10.4
    ## 456    142301         ERIE          2019               110               14.4
    ## 457    142301         ERIE          2019                87               11.2
    ## 458    142301         ERIE          2019               114               12.3
    ## 459    142301         ERIE          2019                72               16.3
    ## 460    142301         ERIE          2019               197               12.8
    ## 461    142301         ERIE          2019                42                8.7
    ## 462    142301         ERIE          2019                83               13.4
    ## 463    142301         ERIE          2019                38               11.8
    ## 464    142301         ERIE          2019                45               15.2
    ## 465    142601         ERIE          2019               135               15.1
    ## 466    142601         ERIE          2019               156               19.6
    ## 467    142601         ERIE          2019                93               22.6
    ## 468    142601         ERIE          2019                63               16.4
    ## 469    142601         ERIE          2019               447               16.9
    ## 470    142601         ERIE          2019               249               18.3
    ## 471    142601         ERIE          2019               198               15.5
    ## 472    142601         ERIE          2019               291               15.8
    ## 473    142601         ERIE          2019               156               16.5
    ## 474    160101     FRANKLIN          2019                61               19.9
    ## 475    160101     FRANKLIN          2019                31               20.9
    ## 476    160101     FRANKLIN          2019                30               19.0
    ## 477    160101     FRANKLIN          2019                45               22.0
    ## 478    160101     FRANKLIN          2019                24               24.7
    ## 479    160101     FRANKLIN          2019                21               19.4
    ## 480    160101     FRANKLIN          2019                16               15.8
    ## 481    160101     FRANKLIN          2019                 7               13.7
    ## 482    160101     FRANKLIN          2019                 9               18.0
    ## 483    161201     FRANKLIN          2019               104               17.8
    ## 484    161201     FRANKLIN          2019                50               18.2
    ## 485    161201     FRANKLIN          2019                54               17.5
    ## 486    161201     FRANKLIN          2019                66               17.0
    ## 487    161201     FRANKLIN          2019                30               16.0
    ## 488    161201     FRANKLIN          2019                36               17.8
    ## 489    161201     FRANKLIN          2019                38               20.2
    ## 490    161201     FRANKLIN          2019                20               22.7
    ## 491    161201     FRANKLIN          2019                18               18.0
    ## 492    170600       FULTON          2019                66               18.4
    ## 493    170600       FULTON          2019                41               24.3
    ## 494    170600       FULTON          2019                25               13.2
    ## 495    170600       FULTON          2019                42               18.1
    ## 496    170600       FULTON          2019                24               22.9
    ## 497    170600       FULTON          2019                18               14.2
    ## 498    170600       FULTON          2019                24               19.2
    ## 499    170600       FULTON          2019                17               26.6
    ## 500    170600       FULTON          2019                 7               11.5
    ## 501    180300      GENESEE          2019               144               17.0
    ## 502    180300      GENESEE          2019                73               18.6
    ## 503    180300      GENESEE          2019                71               15.7
    ## 504    180300      GENESEE          2019                75               14.4
    ## 505    180300      GENESEE          2019                36               14.3
    ## 506    180300      GENESEE          2019                39               14.5
    ## 507    180300      GENESEE          2019                69               21.3
    ## 508    180300      GENESEE          2019                37               26.2
    ## 509    180300      GENESEE          2019                32               17.5
    ## 510    190301       GREENE          2019                72               19.6
    ## 511    190301       GREENE          2019                36               20.7
    ## 512    190301       GREENE          2019                36               18.7
    ## 513    190301       GREENE          2019                50               20.9
    ## 514    190301       GREENE          2019                25               23.1
    ## 515    190301       GREENE          2019                25               19.1
    ## 516    190301       GREENE          2019                22               17.2
    ## 517    190301       GREENE          2019                11               16.7
    ## 518    190301       GREENE          2019                11               17.7
    ## 519    190501       GREENE          2019                99               21.1
    ## 520    190501       GREENE          2019                44               19.6
    ## 521    190501       GREENE          2019                55               22.5
    ## 522    190501       GREENE          2019                60               22.0
    ## 523    190501       GREENE          2019                24               18.6
    ## 524    190501       GREENE          2019                36               25.0
    ## 525    190501       GREENE          2019                39               20.4
    ## 526    190501       GREENE          2019                20               20.8
    ## 527    190501       GREENE          2019                19               20.0
    ## 528    220101    JEFFERSON          2019               116               17.5
    ## 529    220101    JEFFERSON          2019                54               16.6
    ## 530    220101    JEFFERSON          2019                62               18.4
    ## 531    220101    JEFFERSON          2019                78               18.6
    ## 532    220101    JEFFERSON          2019                34               15.6
    ## 533    220101    JEFFERSON          2019                44               21.9
    ## 534    220101    JEFFERSON          2019                38               16.0
    ## 535    220101    JEFFERSON          2019                20               18.7
    ## 536    220101    JEFFERSON          2019                18               13.8
    ## 537    220301    JEFFERSON          2019               250               16.2
    ## 538    220301    JEFFERSON          2019               124               16.6
    ## 539    220301    JEFFERSON          2019               126               15.8
    ## 540    220301    JEFFERSON          2019               157               14.5
    ## 541    220301    JEFFERSON          2019                73               14.2
    ## 542    220301    JEFFERSON          2019                84               14.8
    ## 543    220301    JEFFERSON          2019                93               20.2
    ## 544    220301    JEFFERSON          2019                51               22.0
    ## 545    220301    JEFFERSON          2019                42               18.4
    ## 546    220701    JEFFERSON          2019                47               17.2
    ## 547    220701    JEFFERSON          2019                29               19.9
    ## 548    220701    JEFFERSON          2019                18               14.1
    ## 549    220701    JEFFERSON          2019                21               13.8
    ## 550    220701    JEFFERSON          2019                12               16.2
    ## 551    220701    JEFFERSON          2019                 9               11.5
    ## 552    220701    JEFFERSON          2019                26               21.3
    ## 553    220701    JEFFERSON          2019                17               23.6
    ## 554    220701    JEFFERSON          2019                 9               18.0
    ## 555    222201    JEFFERSON          2019               160               15.6
    ## 556    222201    JEFFERSON          2019                68               15.4
    ## 557    222201    JEFFERSON          2019                92               15.8
    ## 558    222201    JEFFERSON          2019                79               12.8
    ## 559    222201    JEFFERSON          2019                32               11.6
    ## 560    222201    JEFFERSON          2019                47               13.7
    ## 561    222201    JEFFERSON          2019                81               20.0
    ## 562    222201    JEFFERSON          2019                36               21.7
    ## 563    222201    JEFFERSON          2019                45               18.8
    ## 564    240401   LIVINGSTON          2019                41               14.9
    ## 565    240401   LIVINGSTON          2019                22               17.5
    ## 566    240401   LIVINGSTON          2019                19               12.8
    ## 567    240401   LIVINGSTON          2019                18               12.2
    ## 568    240401   LIVINGSTON          2019                 9               14.5
    ## 569    240401   LIVINGSTON          2019                 9               10.5
    ## 570    240401   LIVINGSTON          2019                23               18.9
    ## 571    240401   LIVINGSTON          2019                13               22.0
    ## 572    240401   LIVINGSTON          2019                10               15.9
    ## 573    250901      MADISON          2019                88               20.5
    ## 574    250901      MADISON          2019                49               22.3
    ## 575    250901      MADISON          2019                39               18.7
    ## 576    250901      MADISON          2019                56               19.4
    ## 577    250901      MADISON          2019                28               19.0
    ## 578    250901      MADISON          2019                28               19.9
    ## 579    250901      MADISON          2019                32               22.7
    ## 580    250901      MADISON          2019                21               28.8
    ## 581    250901      MADISON          2019                11               16.2
    ## 582    251601      MADISON          2019                98               15.7
    ## 583    251601      MADISON          2019                46               16.3
    ## 584    251601      MADISON          2019                52               15.2
    ## 585    251601      MADISON          2019                42               12.4
    ## 586    251601      MADISON          2019                21               13.8
    ## 587    251601      MADISON          2019                21               11.2
    ## 588    251601      MADISON          2019                56               19.7
    ## 589    251601      MADISON          2019                25               19.1
    ## 590    251601      MADISON          2019                31               20.3
    ## 591    260101       MONROE          2019               173               15.2
    ## 592    260101       MONROE          2019                93               16.3
    ## 593    260101       MONROE          2019                80               14.0
    ## 594    260101       MONROE          2019                97               14.1
    ## 595    260101       MONROE          2019                43               13.2
    ## 596    260101       MONROE          2019                54               14.9
    ## 597    260101       MONROE          2019                76               17.0
    ## 598    260101       MONROE          2019                50               20.5
    ## 599    260101       MONROE          2019                26               12.7
    ## 600    260801       MONROE          2019               233               17.8
    ## 601    260801       MONROE          2019               120               18.8
    ## 602    260801       MONROE          2019               113               16.8
    ## 603    260801       MONROE          2019               141               17.1
    ## 604    260801       MONROE          2019                72               17.9
    ## 605    260801       MONROE          2019                69               16.4
    ## 606    260801       MONROE          2019                92               18.9
    ## 607    260801       MONROE          2019                48               20.3
    ## 608    260801       MONROE          2019                44               17.5
    ## 609    260803       MONROE          2019               237               18.4
    ## 610    260803       MONROE          2019               111               18.0
    ## 611    260803       MONROE          2019               126               18.6
    ## 612    260803       MONROE          2019               143               18.4
    ## 613    260803       MONROE          2019                66               18.7
    ## 614    260803       MONROE          2019                77               18.1
    ## 615    260803       MONROE          2019                94               18.3
    ## 616    260803       MONROE          2019                45               17.2
    ## 617    260803       MONROE          2019                49               19.5
    ## 618    261101       MONROE          2019               267               17.5
    ## 619    261101       MONROE          2019               135               18.2
    ## 620    261101       MONROE          2019               132               16.9
    ## 621    261101       MONROE          2019               163               18.2
    ## 622    261101       MONROE          2019                85               19.9
    ## 623    261101       MONROE          2019                78               16.6
    ## 624    261101       MONROE          2019               104               16.7
    ## 625    261101       MONROE          2019                50               16.1
    ## 626    261101       MONROE          2019                54               17.4
    ## 627    261313       MONROE          2019                56               13.5
    ## 628    261313       MONROE          2019                34               17.2
    ## 629    261313       MONROE          2019                22               10.2
    ## 630    261313       MONROE          2019                31               12.1
    ## 631    261313       MONROE          2019                19               15.7
    ## 632    261313       MONROE          2019                12                8.9
    ## 633    261313       MONROE          2019                25               17.2
    ## 634    261313       MONROE          2019                15               20.8
    ## 635    261313       MONROE          2019                10               13.7
    ## 636    261501       MONROE          2019               284               19.2
    ## 637    261501       MONROE          2019               128               18.4
    ## 638    261501       MONROE          2019               156               20.0
    ## 639    261501       MONROE          2019               167               19.4
    ## 640    261501       MONROE          2019                64               15.9
    ## 641    261501       MONROE          2019               103               22.5
    ## 642    261501       MONROE          2019               117               19.0
    ## 643    261501       MONROE          2019                64               21.7
    ## 644    261501       MONROE          2019                53               16.5
    ## 645    261701       MONROE          2019               285               15.5
    ## 646    261701       MONROE          2019               131               14.7
    ## 647    261701       MONROE          2019               154               16.3
    ## 648    261701       MONROE          2019               163               14.2
    ## 649    261701       MONROE          2019                82               14.7
    ## 650    261701       MONROE          2019                81               13.7
    ## 651    261701       MONROE          2019               122               17.7
    ## 652    261701       MONROE          2019                49               14.6
    ## 653    261701       MONROE          2019                73               20.6
    ## 654    261801       MONROE          2019               163               16.2
    ## 655    261801       MONROE          2019                80               16.3
    ## 656    261801       MONROE          2019                83               16.2
    ## 657    261801       MONROE          2019                91               14.5
    ## 658    261801       MONROE          2019                46               15.1
    ## 659    261801       MONROE          2019                45               14.0
    ## 660    261801       MONROE          2019                72               19.2
    ## 661    261801       MONROE          2019                34               18.3
    ## 662    261801       MONROE          2019                38               20.1
    ## 663    261901       MONROE          2019               499               15.4
    ## 664    261901       MONROE          2019               267               16.5
    ## 665    261901       MONROE          2019               232               14.2
    ## 666    261901       MONROE          2019               261               13.8
    ## 667    261901       MONROE          2019               143               15.1
    ## 668    261901       MONROE          2019               118               12.5
    ## 669    261901       MONROE          2019               238               17.6
    ## 670    261901       MONROE          2019               124               18.5
    ## 671    261901       MONROE          2019               114               16.7
    ## 672    270100   MONTGOMERY          2019               196               17.7
    ## 673    270100   MONTGOMERY          2019               100               18.7
    ## 674    270100   MONTGOMERY          2019                96               16.8
    ## 675    270100   MONTGOMERY          2019               146               18.6
    ## 676    270100   MONTGOMERY          2019                77               19.6
    ## 677    270100   MONTGOMERY          2019                69               17.6
    ## 678    270100   MONTGOMERY          2019                50               15.7
    ## 679    270100   MONTGOMERY          2019                23               16.7
    ## 680    270100   MONTGOMERY          2019                27               14.9
    ## 681    280202       NASSAU          2019               539               20.4
    ## 682    280202       NASSAU          2019               290               22.0
    ## 683    280202       NASSAU          2019               249               18.8
    ## 684    280202       NASSAU          2019               314               19.0
    ## 685    280202       NASSAU          2019               177               20.6
    ## 686    280202       NASSAU          2019               137               17.2
    ## 687    280202       NASSAU          2019               225               22.7
    ## 688    280202       NASSAU          2019               113               24.5
    ## 689    280202       NASSAU          2019               112               21.2
    ## 690    280203       NASSAU          2019               372               17.5
    ## 691    280203       NASSAU          2019               188               17.4
    ## 692    280203       NASSAU          2019               184               17.6
    ## 693    280203       NASSAU          2019               171               15.0
    ## 694    280203       NASSAU          2019                81               13.9
    ## 695    280203       NASSAU          2019                90               16.3
    ## 696    280203       NASSAU          2019               201               20.4
    ## 697    280203       NASSAU          2019               107               21.7
    ## 698    280203       NASSAU          2019                94               19.2
    ## 699    280205       NASSAU          2019               339               14.7
    ## 700    280205       NASSAU          2019               177               15.4
    ## 701    280205       NASSAU          2019               162               14.0
    ## 702    280205       NASSAU          2019               184               12.9
    ## 703    280205       NASSAU          2019                96               13.3
    ## 704    280205       NASSAU          2019                88               12.6
    ## 705    280205       NASSAU          2019               155               17.5
    ## 706    280205       NASSAU          2019                81               19.0
    ## 707    280205       NASSAU          2019                74               16.1
    ## 708    280206       NASSAU          2019               111               16.3
    ## 709    280206       NASSAU          2019                51               16.3
    ## 710    280206       NASSAU          2019                60               16.2
    ## 711    280206       NASSAU          2019                60               17.6
    ## 712    280206       NASSAU          2019                27               16.9
    ## 713    280206       NASSAU          2019                33               18.3
    ## 714    280206       NASSAU          2019                51               15.1
    ## 715    280206       NASSAU          2019                24               16.2
    ## 716    280206       NASSAU          2019                27               14.2
    ## 717    280209       NASSAU          2019               527               21.4
    ## 718    280209       NASSAU          2019               269               23.4
    ## 719    280209       NASSAU          2019               258               19.6
    ## 720    280209       NASSAU          2019               352               20.9
    ## 721    280209       NASSAU          2019               171               22.3
    ## 722    280209       NASSAU          2019               181               19.8
    ## 723    280209       NASSAU          2019               175               22.4
    ## 724    280209       NASSAU          2019                98               25.7
    ## 725    280209       NASSAU          2019                77               19.3
    ## 726    280213       NASSAU          2019                50               10.4
    ## 727    280213       NASSAU          2019                26               11.5
    ## 728    280213       NASSAU          2019                24                9.5
    ## 729    280213       NASSAU          2019                50               10.4
    ## 730    280213       NASSAU          2019                26               11.5
    ## 731    280213       NASSAU          2019                24                9.5
    ## 732    280215       NASSAU          2019               151               18.3
    ## 733    280215       NASSAU          2019                74               17.5
    ## 734    280215       NASSAU          2019                77               19.1
    ## 735    280215       NASSAU          2019               100               16.9
    ## 736    280215       NASSAU          2019                52               17.2
    ## 737    280215       NASSAU          2019                48               16.6
    ## 738    280215       NASSAU          2019                51               21.6
    ## 739    280215       NASSAU          2019                22               18.0
    ## 740    280215       NASSAU          2019                29               25.4
    ## 741    280221       NASSAU          2019               196               14.9
    ## 742    280221       NASSAU          2019                91               14.6
    ## 743    280221       NASSAU          2019               105               15.2
    ## 744    280221       NASSAU          2019               120               15.6
    ## 745    280221       NASSAU          2019                55               15.0
    ## 746    280221       NASSAU          2019                65               16.1
    ## 747    280221       NASSAU          2019                76               14.0
    ## 748    280221       NASSAU          2019                36               14.0
    ## 749    280221       NASSAU          2019                40               14.0
    ## 750    280222       NASSAU          2019                91               12.5
    ## 751    280222       NASSAU          2019                32               13.7
    ## 752    280222       NASSAU          2019                59               11.9
    ## 753    280222       NASSAU          2019                91               12.5
    ## 754    280222       NASSAU          2019                32               13.7
    ## 755    280222       NASSAU          2019                59               11.9
    ## 756    280227       NASSAU          2019               109               17.6
    ## 757    280227       NASSAU          2019                55               17.3
    ## 758    280227       NASSAU          2019                54               17.9
    ## 759    280227       NASSAU          2019                59               17.8
    ## 760    280227       NASSAU          2019                27               15.4
    ## 761    280227       NASSAU          2019                32               20.5
    ## 762    280227       NASSAU          2019                50               17.5
    ## 763    280227       NASSAU          2019                28               19.9
    ## 764    280227       NASSAU          2019                22               15.2
    ## 765    280229       NASSAU          2019                48               12.7
    ## 766    280229       NASSAU          2019                18               10.9
    ## 767    280229       NASSAU          2019                30               14.2
    ## 768    280229       NASSAU          2019                48               12.7
    ## 769    280229       NASSAU          2019                18               10.9
    ## 770    280229       NASSAU          2019                30               14.2
    ## 771    280230       NASSAU          2019                69               16.9
    ## 772    280230       NASSAU          2019                37               18.6
    ## 773    280230       NASSAU          2019                32               15.3
    ## 774    280230       NASSAU          2019                69               16.9
    ## 775    280230       NASSAU          2019                37               18.6
    ## 776    280230       NASSAU          2019                32               15.3
    ## 777    280403       NASSAU          2019               122               14.1
    ## 778    280403       NASSAU          2019                61               14.4
    ## 779    280403       NASSAU          2019                61               13.8
    ## 780    280403       NASSAU          2019                57               11.8
    ## 781    280403       NASSAU          2019                25                9.9
    ## 782    280403       NASSAU          2019                32               13.7
    ## 783    280403       NASSAU          2019                65               17.2
    ## 784    280403       NASSAU          2019                36               21.1
    ## 785    280403       NASSAU          2019                29               13.9
    ## 786    280405       NASSAU          2019                93               13.7
    ## 787    280405       NASSAU          2019                46               13.6
    ## 788    280405       NASSAU          2019                47               13.7
    ## 789    280405       NASSAU          2019                93               13.7
    ## 790    280405       NASSAU          2019                46               13.6
    ## 791    280405       NASSAU          2019                47               13.7
    ## 792    280407       NASSAU          2019               310               12.5
    ## 793    280407       NASSAU          2019               144               12.0
    ## 794    280407       NASSAU          2019               166               13.0
    ## 795    280407       NASSAU          2019               169               11.1
    ## 796    280407       NASSAU          2019                82               11.1
    ## 797    280407       NASSAU          2019                87               11.1
    ## 798    280407       NASSAU          2019               141               14.7
    ## 799    280407       NASSAU          2019                62               13.5
    ## 800    280407       NASSAU          2019                79               15.9
    ## 801    280501       NASSAU          2019               108               13.1
    ## 802    280501       NASSAU          2019                59               14.9
    ## 803    280501       NASSAU          2019                49               11.3
    ## 804    280501       NASSAU          2019                65               12.6
    ## 805    280501       NASSAU          2019                34               14.4
    ## 806    280501       NASSAU          2019                31               11.1
    ## 807    280501       NASSAU          2019                43               13.9
    ## 808    280501       NASSAU          2019                25               15.8
    ## 809    280501       NASSAU          2019                18               11.8
    ## 810    280504       NASSAU          2019               280               17.1
    ## 811    280504       NASSAU          2019               136               17.9
    ## 812    280504       NASSAU          2019               144               16.3
    ## 813    280504       NASSAU          2019               159               17.0
    ## 814    280504       NASSAU          2019                66               15.7
    ## 815    280504       NASSAU          2019                93               18.0
    ## 816    280504       NASSAU          2019               121               17.2
    ## 817    280504       NASSAU          2019                70               20.6
    ## 818    280504       NASSAU          2019                51               14.0
    ## 819    280506       NASSAU          2019                57               15.3
    ## 820    280506       NASSAU          2019                33               17.1
    ## 821    280506       NASSAU          2019                24               13.4
    ## 822    280506       NASSAU          2019                33               17.1
    ## 823    280506       NASSAU          2019                14               14.7
    ## 824    280506       NASSAU          2019                24               13.7
    ## 825    280506       NASSAU          2019                14               15.4
    ## 826    280506       NASSAU          2019                10               11.9
    ## 827    280515       NASSAU          2019               132               13.3
    ## 828    280515       NASSAU          2019                53               11.6
    ## 829    280515       NASSAU          2019                79               14.7
    ## 830    280515       NASSAU          2019                53               10.5
    ## 831    280515       NASSAU          2019                25               10.1
    ## 832    280515       NASSAU          2019                28               10.9
    ## 833    280515       NASSAU          2019                79               16.2
    ## 834    280515       NASSAU          2019                28               13.5
    ## 835    280515       NASSAU          2019                51               18.3
    ## 836    280517       NASSAU          2019               272               16.9
    ## 837    280517       NASSAU          2019               130               16.9
    ## 838    280517       NASSAU          2019               142               16.8
    ## 839    280517       NASSAU          2019               162               16.1
    ## 840    280517       NASSAU          2019                78               16.5
    ## 841    280517       NASSAU          2019                84               15.7
    ## 842    280517       NASSAU          2019               110               18.2
    ## 843    280517       NASSAU          2019                52               17.7
    ## 844    280517       NASSAU          2019                58               18.6
    ## 845    280523       NASSAU          2019               309               14.7
    ## 846    280523       NASSAU          2019               145               14.2
    ## 847    280523       NASSAU          2019               164               15.2
    ## 848    280523       NASSAU          2019               158               13.4
    ## 849    280523       NASSAU          2019                68               12.4
    ## 850    280523       NASSAU          2019                90               14.3
    ## 851    280523       NASSAU          2019               151               16.4
    ## 852    280523       NASSAU          2019                77               16.2
    ## 853    280523       NASSAU          2019                74               16.6
    ## 854    400301      NIAGARA          2019               120               16.0
    ## 855    400301      NIAGARA          2019                50               14.3
    ## 856    400301      NIAGARA          2019                70               17.4
    ## 857    400301      NIAGARA          2019                60               12.5
    ## 858    400301      NIAGARA          2019                26               11.6
    ## 859    400301      NIAGARA          2019                34               13.4
    ## 860    400301      NIAGARA          2019                60               22.1
    ## 861    400301      NIAGARA          2019                24               19.4
    ## 862    400301      NIAGARA          2019                36               24.5
    ## 863    400400      NIAGARA          2019               227               14.5
    ## 864    400400      NIAGARA          2019               128               16.7
    ## 865    400400      NIAGARA          2019                99               12.4
    ## 866    400400      NIAGARA          2019               131               13.0
    ## 867    400400      NIAGARA          2019                73               14.1
    ## 868    400400      NIAGARA          2019                58               11.9
    ## 869    400400      NIAGARA          2019                96               17.1
    ## 870    400400      NIAGARA          2019                55               22.0
    ## 871    400400      NIAGARA          2019                41               13.2
    ## 872    401001      NIAGARA          2019               131               17.1
    ## 873    401001      NIAGARA          2019                69               17.9
    ## 874    401001      NIAGARA          2019                62               16.2
    ## 875    401001      NIAGARA          2019                58               14.5
    ## 876    401001      NIAGARA          2019                26               12.7
    ## 877    401001      NIAGARA          2019                32               16.2
    ## 878    401001      NIAGARA          2019                73               20.0
    ## 879    401001      NIAGARA          2019                43               23.9
    ## 880    401001      NIAGARA          2019                30               16.2
    ## 881    401501      NIAGARA          2019                52               16.9
    ## 882    401501      NIAGARA          2019                38               25.5
    ## 883    401501      NIAGARA          2019                14                8.9
    ## 884    401501      NIAGARA          2019                18               13.1
    ## 885    401501      NIAGARA          2019                18               24.0
    ## 886    401501      NIAGARA          2019                30               18.6
    ## 887    401501      NIAGARA          2019                20               27.0
    ## 888    401501      NIAGARA          2019                10               11.5
    ## 889    411504       ONEIDA          2019                22               13.3
    ## 890    411504       ONEIDA          2019                13               17.3
    ## 891    411504       ONEIDA          2019                 9                9.9
    ## 892    411504       ONEIDA          2019                13               12.1
    ## 893    411504       ONEIDA          2019                 7               14.9
    ## 894    411504       ONEIDA          2019                 6               10.0
    ## 895    411504       ONEIDA          2019                 6               11.3
    ## 896    411603       ONEIDA          2019                64               17.2
    ## 897    411603       ONEIDA          2019                34               17.0
    ## 898    411603       ONEIDA          2019                30               17.3
    ## 899    411603       ONEIDA          2019                45               18.4
    ## 900    411603       ONEIDA          2019                24               18.3
    ## 901    411603       ONEIDA          2019                21               18.6
    ## 902    411603       ONEIDA          2019                19               14.7
    ## 903    411603       ONEIDA          2019                10               14.5
    ## 904    411603       ONEIDA          2019                 9               15.0
    ## 905    411800       ONEIDA          2019               306               15.0
    ## 906    411800       ONEIDA          2019               156               15.5
    ## 907    411800       ONEIDA          2019               150               14.6
    ## 908    411800       ONEIDA          2019               241               15.8
    ## 909    411800       ONEIDA          2019               123               16.2
    ## 910    411800       ONEIDA          2019               118               15.4
    ## 911    411800       ONEIDA          2019                65               12.8
    ## 912    411800       ONEIDA          2019                33               13.3
    ## 913    411800       ONEIDA          2019                32               12.3
    ## 914    412902       ONEIDA          2019               139               13.3
    ## 915    412902       ONEIDA          2019                60               12.8
    ## 916    412902       ONEIDA          2019                79               13.7
    ## 917    412902       ONEIDA          2019                79               13.8
    ## 918    412902       ONEIDA          2019                37               13.8
    ## 919    412902       ONEIDA          2019                42               13.9
    ## 920    412902       ONEIDA          2019                60               12.8
    ## 921    412902       ONEIDA          2019                23               11.8
    ## 922    412902       ONEIDA          2019                37               13.6
    ## 923    420401     ONONDAGA          2019               168               16.4
    ## 924    420401     ONONDAGA          2019                84               16.6
    ## 925    420401     ONONDAGA          2019                84               16.3
    ## 926    420401     ONONDAGA          2019               118               17.0
    ## 927    420401     ONONDAGA          2019                59               17.3
    ## 928    420401     ONONDAGA          2019                59               16.7
    ## 929    420401     ONONDAGA          2019                50               15.2
    ## 930    420401     ONONDAGA          2019                25               15.2
    ## 931    420401     ONONDAGA          2019                25               15.3
    ## 932    420411     ONONDAGA          2019               137               15.2
    ## 933    420411     ONONDAGA          2019                69               15.8
    ## 934    420411     ONONDAGA          2019                68               14.7
    ## 935    420411     ONONDAGA          2019                73               15.1
    ## 936    420411     ONONDAGA          2019                33               14.2
    ## 937    420411     ONONDAGA          2019                40               15.9
    ## 938    420411     ONONDAGA          2019                64               15.4
    ## 939    420411     ONONDAGA          2019                36               17.7
    ## 940    420411     ONONDAGA          2019                28               13.2
    ## 941    420601     ONONDAGA          2019                36               16.9
    ## 942    420601     ONONDAGA          2019                18               17.8
    ## 943    420601     ONONDAGA          2019                18               16.1
    ## 944    420601     ONONDAGA          2019                22               16.9
    ## 945    420601     ONONDAGA          2019                11               18.0
    ## 946    420601     ONONDAGA          2019                11               15.9
    ## 947    420601     ONONDAGA          2019                14               16.9
    ## 948    420601     ONONDAGA          2019                 7               17.5
    ## 949    420601     ONONDAGA          2019                 7               16.3
    ## 950    420702     ONONDAGA          2019                91               17.1
    ## 951    420702     ONONDAGA          2019                51               18.2
    ## 952    420702     ONONDAGA          2019                40               15.9
    ## 953    420702     ONONDAGA          2019                51               16.3
    ## 954    420702     ONONDAGA          2019                23               14.3
    ## 955    420702     ONONDAGA          2019                28               18.4
    ## 956    420702     ONONDAGA          2019                40               18.3
    ## 957    420702     ONONDAGA          2019                28               23.5
    ## 958    420702     ONONDAGA          2019                12               12.1
    ## 959    420901     ONONDAGA          2019               270               14.5
    ## 960    420901     ONONDAGA          2019               150               16.9
    ## 961    420901     ONONDAGA          2019               120               12.3
    ## 962    420901     ONONDAGA          2019               157               14.0
    ## 963    420901     ONONDAGA          2019                83               15.8
    ## 964    420901     ONONDAGA          2019                74               12.5
    ## 965    420901     ONONDAGA          2019               113               15.2
    ## 966    420901     ONONDAGA          2019                67               18.4
    ## 967    420901     ONONDAGA          2019                46               12.1
    ## 968    421001     ONONDAGA          2019               173               14.1
    ## 969    421001     ONONDAGA          2019                89               14.8
    ## 970    421001     ONONDAGA          2019                84               13.4
    ## 971    421001     ONONDAGA          2019               102               14.1
    ## 972    421001     ONONDAGA          2019                53               15.2
    ## 973    421001     ONONDAGA          2019                49               13.1
    ## 974    421001     ONONDAGA          2019                71               14.1
    ## 975    421001     ONONDAGA          2019                36               14.3
    ## 976    421001     ONONDAGA          2019                35               13.8
    ## 977    421201     ONONDAGA          2019                52               17.9
    ## 978    421201     ONONDAGA          2019                34               24.6
    ## 979    421201     ONONDAGA          2019                18               11.8
    ## 980    421201     ONONDAGA          2019                28               16.3
    ## 981    421201     ONONDAGA          2019                17               21.5
    ## 982    421201     ONONDAGA          2019                11               11.8
    ## 983    421201     ONONDAGA          2019                24               20.3
    ## 984    421201     ONONDAGA          2019                17               28.8
    ## 985    421201     ONONDAGA          2019                 7               11.9
    ## 986    421800     ONONDAGA          2019              1021               16.6
    ## 987    421800     ONONDAGA          2019               535               18.3
    ## 988    421800     ONONDAGA          2019               486               15.0
    ## 989    421800     ONONDAGA          2019               673               15.9
    ## 990    421800     ONONDAGA          2019               335               16.7
    ## 991    421800     ONONDAGA          2019               338               15.2
    ## 992    421800     ONONDAGA          2019               348               18.0
    ## 993    421800     ONONDAGA          2019               200               22.0
    ## 994    421800     ONONDAGA          2019               148               14.6
    ## 995    421902     ONONDAGA          2019                46               16.7
    ## 996    421902     ONONDAGA          2019                31               23.1
    ## 997    421902     ONONDAGA          2019                15               10.6
    ## 998    421902     ONONDAGA          2019                28               15.1
    ## 999    421902     ONONDAGA          2019                19               21.1
    ## 1000   421902     ONONDAGA          2019                 9                9.4
    ## 1001   421902     ONONDAGA          2019                18               20.0
    ## 1002   421902     ONONDAGA          2019                12               27.3
    ## 1003   421902     ONONDAGA          2019                 6               13.0
    ## 1004   430700      ONTARIO          2019               143               19.6
    ## 1005   430700      ONTARIO          2019                77               20.1
    ## 1006   430700      ONTARIO          2019                66               19.0
    ## 1007   430700      ONTARIO          2019                81               18.6
    ## 1008   430700      ONTARIO          2019                43               18.4
    ## 1009   430700      ONTARIO          2019                38               18.9
    ## 1010   430700      ONTARIO          2019                62               21.5
    ## 1011   430700      ONTARIO          2019                34               23.1
    ## 1012   430700      ONTARIO          2019                28               19.7
    ## 1013   430901      ONTARIO          2019                63               16.7
    ## 1014   430901      ONTARIO          2019                32               17.3
    ## 1015   430901      ONTARIO          2019                31               16.1
    ## 1016   430901      ONTARIO          2019                39               17.7
    ## 1017   430901      ONTARIO          2019                16               14.5
    ## 1018   430901      ONTARIO          2019                23               20.9
    ## 1019   430901      ONTARIO          2019                24               15.2
    ## 1020   430901      ONTARIO          2019                16               21.3
    ## 1021   430901      ONTARIO          2019                 8                9.6
    ## 1022   431101      ONTARIO          2019                60               21.8
    ## 1023   431101      ONTARIO          2019                31               24.6
    ## 1024   431101      ONTARIO          2019                29               19.5
    ## 1025   431101      ONTARIO          2019                36               22.2
    ## 1026   431101      ONTARIO          2019                19               23.8
    ## 1027   431101      ONTARIO          2019                17               20.7
    ## 1028   431101      ONTARIO          2019                24               21.2
    ## 1029   431101      ONTARIO          2019                12               26.1
    ## 1030   431101      ONTARIO          2019                12               17.9
    ## 1031   431701      ONTARIO          2019               261               14.6
    ## 1032   431701      ONTARIO          2019               132               14.4
    ## 1033   431701      ONTARIO          2019               129               14.7
    ## 1034   431701      ONTARIO          2019               185               16.3
    ## 1035   431701      ONTARIO          2019               102               17.5
    ## 1036   431701      ONTARIO          2019                83               15.0
    ## 1037   431701      ONTARIO          2019                76               11.6
    ## 1038   431701      ONTARIO          2019                30                9.0
    ## 1039   431701      ONTARIO          2019                46               14.2
    ## 1040   440102       ORANGE          2019               199               16.0
    ## 1041   440102       ORANGE          2019               103               17.2
    ## 1042   440102       ORANGE          2019                96               15.0
    ## 1043   440102       ORANGE          2019                89               13.9
    ## 1044   440102       ORANGE          2019                49               15.4
    ## 1045   440102       ORANGE          2019                40               12.5
    ## 1046   440102       ORANGE          2019               110               18.4
    ## 1047   440102       ORANGE          2019                54               19.6
    ## 1048   440102       ORANGE          2019                56               17.4
    ## 1049   440301       ORANGE          2019               151               17.4
    ## 1050   440301       ORANGE          2019                73               18.0
    ## 1051   440301       ORANGE          2019                78               16.8
    ## 1052   440301       ORANGE          2019                72               15.1
    ## 1053   440301       ORANGE          2019                35               15.8
    ## 1054   440301       ORANGE          2019                37               14.4
    ## 1055   440301       ORANGE          2019                79               20.6
    ## 1056   440301       ORANGE          2019                38               21.1
    ## 1057   440301       ORANGE          2019                41               20.2
    ## 1058   441201       ORANGE          2019               350               17.9
    ## 1059   441201       ORANGE          2019               180               18.9
    ## 1060   441201       ORANGE          2019               170               17.1
    ## 1061   441201       ORANGE          2019               176               15.7
    ## 1062   441201       ORANGE          2019                84               15.3
    ## 1063   441201       ORANGE          2019                92               16.1
    ## 1064   441201       ORANGE          2019               174               21.0
    ## 1065   441201       ORANGE          2019                96               23.8
    ## 1066   441201       ORANGE          2019                78               18.3
    ## 1067   441600       ORANGE          2019               758               17.6
    ## 1068   441600       ORANGE          2019               388               18.8
    ## 1069   441600       ORANGE          2019               370               16.5
    ## 1070   441600       ORANGE          2019               497               17.6
    ## 1071   441600       ORANGE          2019               238               17.3
    ## 1072   441600       ORANGE          2019               259               18.0
    ## 1073   441600       ORANGE          2019               261               17.5
    ## 1074   441600       ORANGE          2019               150               21.7
    ## 1075   441600       ORANGE          2019               111               13.9
    ## 1076   442101       ORANGE          2019               254               14.6
    ## 1077   442101       ORANGE          2019               129               15.1
    ## 1078   442101       ORANGE          2019               125               14.1
    ## 1079   442101       ORANGE          2019               162               13.8
    ## 1080   442101       ORANGE          2019                86               14.8
    ## 1081   442101       ORANGE          2019                76               12.8
    ## 1082   442101       ORANGE          2019                92               16.1
    ## 1083   442101       ORANGE          2019                43               15.6
    ## 1084   442101       ORANGE          2019                49               16.6
    ## 1085   450607      ORLEANS          2019                49               19.5
    ## 1086   450607      ORLEANS          2019                24               19.8
    ## 1087   450607      ORLEANS          2019                25               19.2
    ## 1088   450607      ORLEANS          2019                31               20.8
    ## 1089   450607      ORLEANS          2019                14               18.4
    ## 1090   450607      ORLEANS          2019                17               23.3
    ## 1091   450607      ORLEANS          2019                18               18.6
    ## 1092   450607      ORLEANS          2019                10               22.2
    ## 1093   450607      ORLEANS          2019                 8               15.4
    ## 1094   450704      ORLEANS          2019                58               14.4
    ## 1095   450704      ORLEANS          2019                29               14.7
    ## 1096   450704      ORLEANS          2019                29               14.1
    ## 1097   450704      ORLEANS          2019                33               13.3
    ## 1098   450704      ORLEANS          2019                16               13.2
    ## 1099   450704      ORLEANS          2019                17               13.3
    ## 1100   450704      ORLEANS          2019                25               16.6
    ## 1101   450704      ORLEANS          2019                13               17.1
    ## 1102   450704      ORLEANS          2019                12               16.0
    ## 1103   460102       OSWEGO          2019                87               19.8
    ## 1104   460102       OSWEGO          2019                49               20.7
    ## 1105   460102       OSWEGO          2019                38               18.8
    ## 1106   460102       OSWEGO          2019                58               20.6
    ## 1107   460102       OSWEGO          2019                29               19.6
    ## 1108   460102       OSWEGO          2019                29               21.6
    ## 1109   460102       OSWEGO          2019                29               18.5
    ## 1110   460102       OSWEGO          2019                20               22.5
    ## 1111   460102       OSWEGO          2019                 9               13.2
    ## 1112   460500       OSWEGO          2019               190               17.1
    ## 1113   460500       OSWEGO          2019                87               16.8
    ## 1114   460500       OSWEGO          2019               103               17.4
    ## 1115   460500       OSWEGO          2019               126               16.9
    ## 1116   460500       OSWEGO          2019                63               18.1
    ## 1117   460500       OSWEGO          2019                63               15.9
    ## 1118   460500       OSWEGO          2019                64               17.5
    ## 1119   460500       OSWEGO          2019                24               14.1
    ## 1120   460500       OSWEGO          2019                40               20.5
    ## 1121   460701       OSWEGO          2019                97               21.0
    ## 1122   460701       OSWEGO          2019                45               21.1
    ## 1123   460701       OSWEGO          2019                52               20.9
    ## 1124   460701       OSWEGO          2019                57               22.1
    ## 1125   460701       OSWEGO          2019                27               23.5
    ## 1126   460701       OSWEGO          2019                30               21.0
    ## 1127   460701       OSWEGO          2019                40               19.6
    ## 1128   460701       OSWEGO          2019                18               18.4
    ## 1129   460701       OSWEGO          2019                22               20.8
    ## 1130   461901       OSWEGO          2019                53               18.4
    ## 1131   461901       OSWEGO          2019                25               19.4
    ## 1132   461901       OSWEGO          2019                28               17.6
    ## 1133   461901       OSWEGO          2019                31               16.5
    ## 1134   461901       OSWEGO          2019                17               21.0
    ## 1135   461901       OSWEGO          2019                14               13.1
    ## 1136   461901       OSWEGO          2019                22               22.0
    ## 1137   461901       OSWEGO          2019                 8               16.7
    ## 1138   461901       OSWEGO          2019                14               26.9
    ## 1139   462001       OSWEGO          2019                93               14.1
    ## 1140   462001       OSWEGO          2019                53               16.3
    ## 1141   462001       OSWEGO          2019                40               12.0
    ## 1142   462001       OSWEGO          2019                56               13.1
    ## 1143   462001       OSWEGO          2019                29               14.6
    ## 1144   462001       OSWEGO          2019                27               11.9
    ## 1145   462001       OSWEGO          2019                37               15.9
    ## 1146   462001       OSWEGO          2019                24               19.0
    ## 1147   462001       OSWEGO          2019                13               12.3
    ## 1148   471400       OTSEGO          2019                84               12.9
    ## 1149   471400       OTSEGO          2019                43               14.0
    ## 1150   471400       OTSEGO          2019                41               11.9
    ## 1151   471400       OTSEGO          2019                45               11.7
    ## 1152   471400       OTSEGO          2019                24               13.4
    ## 1153   471400       OTSEGO          2019                21               10.2
    ## 1154   471400       OTSEGO          2019                39               14.8
    ## 1155   471400       OTSEGO          2019                19               15.0
    ## 1156   471400       OTSEGO          2019                20               14.7
    ## 1157   471701       OTSEGO          2019                45               14.2
    ## 1158   471701       OTSEGO          2019                26               16.4
    ## 1159   471701       OTSEGO          2019                19               12.0
    ## 1160   471701       OTSEGO          2019                22               13.4
    ## 1161   471701       OTSEGO          2019                12               15.8
    ## 1162   471701       OTSEGO          2019                10               11.4
    ## 1163   471701       OTSEGO          2019                23               16.1
    ## 1164   471701       OTSEGO          2019                14               17.9
    ## 1165   471701       OTSEGO          2019                 9               13.8
    ## 1166   480102       PUTNAM          2019               222               18.1
    ## 1167   480102       PUTNAM          2019               106               18.3
    ## 1168   480102       PUTNAM          2019               116               17.9
    ## 1169   480102       PUTNAM          2019               122               17.1
    ## 1170   480102       PUTNAM          2019                58               17.4
    ## 1171   480102       PUTNAM          2019                64               16.9
    ## 1172   480102       PUTNAM          2019               100               19.4
    ## 1173   480102       PUTNAM          2019                48               19.6
    ## 1174   480102       PUTNAM          2019                52               19.3
    ## 1175   480401       PUTNAM          2019                25                9.7
    ## 1176   480401       PUTNAM          2019                16               12.7
    ## 1177   480401       PUTNAM          2019                 9                6.8
    ## 1178   480401       PUTNAM          2019                 6                5.3
    ## 1179   480401       PUTNAM          2019                 6               10.2
    ## 1180   480401       PUTNAM          2019                16               12.5
    ## 1181   480401       PUTNAM          2019                 6                8.7
    ## 1182   480601       PUTNAM          2019               195               17.7
    ## 1183   480601       PUTNAM          2019                98               18.7
    ## 1184   480601       PUTNAM          2019                97               16.7
    ## 1185   480601       PUTNAM          2019                98               15.5
    ## 1186   480601       PUTNAM          2019                42               13.9
    ## 1187   480601       PUTNAM          2019                56               17.0
    ## 1188   480601       PUTNAM          2019                97               20.6
    ## 1189   480601       PUTNAM          2019                56               25.2
    ## 1190   480601       PUTNAM          2019                41               16.5
    ## 1191   490301   RENSSELAER          2019               287               20.7
    ## 1192   490301   RENSSELAER          2019               159               23.1
    ## 1193   490301   RENSSELAER          2019               128               18.3
    ## 1194   490301   RENSSELAER          2019               151               19.1
    ## 1195   490301   RENSSELAER          2019                85               21.3
    ## 1196   490301   RENSSELAER          2019                66               16.9
    ## 1197   490301   RENSSELAER          2019               136               22.7
    ## 1198   490301   RENSSELAER          2019                74               25.5
    ## 1199   490301   RENSSELAER          2019                62               20.1
    ## 1200   491302   RENSSELAER          2019               149               17.1
    ## 1201   491302   RENSSELAER          2019                82               19.6
    ## 1202   491302   RENSSELAER          2019                67               14.7
    ## 1203   491302   RENSSELAER          2019                83               17.0
    ## 1204   491302   RENSSELAER          2019                44               18.8
    ## 1205   491302   RENSSELAER          2019                39               15.4
    ## 1206   491302   RENSSELAER          2019                66               17.1
    ## 1207   491302   RENSSELAER          2019                38               20.7
    ## 1208   491302   RENSSELAER          2019                28               13.9
    ## 1209   491700   RENSSELAER          2019               192               16.6
    ## 1210   491700   RENSSELAER          2019                99               18.6
    ## 1211   491700   RENSSELAER          2019                93               15.0
    ## 1212   491700   RENSSELAER          2019               126               18.6
    ## 1213   491700   RENSSELAER          2019                65               21.2
    ## 1214   491700   RENSSELAER          2019                61               16.4
    ## 1215   491700   RENSSELAER          2019                66               13.9
    ## 1216   491700   RENSSELAER          2019                34               15.0
    ## 1217   491700   RENSSELAER          2019                32               12.9
    ## 1218   500108     ROCKLAND          2019                94               14.1
    ## 1219   500108     ROCKLAND          2019                52               15.2
    ## 1220   500108     ROCKLAND          2019                42               12.8
    ## 1221   500108     ROCKLAND          2019                49               12.2
    ## 1222   500108     ROCKLAND          2019                26               12.6
    ## 1223   500108     ROCKLAND          2019                23               11.8
    ## 1224   500108     ROCKLAND          2019                45               17.2
    ## 1225   500108     ROCKLAND          2019                26               19.7
    ## 1226   500108     ROCKLAND          2019                19               14.7
    ## 1227   500201     ROCKLAND          2019               545               17.3
    ## 1228   500201     ROCKLAND          2019               267               17.6
    ## 1229   500201     ROCKLAND          2019               278               16.9
    ## 1230   500201     ROCKLAND          2019               301               16.0
    ## 1231   500201     ROCKLAND          2019               146               16.2
    ## 1232   500201     ROCKLAND          2019               155               15.8
    ## 1233   500201     ROCKLAND          2019               244               19.0
    ## 1234   500201     ROCKLAND          2019               121               19.7
    ## 1235   500201     ROCKLAND          2019               123               18.4
    ## 1236   500301     ROCKLAND          2019               167               16.0
    ## 1237   500301     ROCKLAND          2019                83               16.3
    ## 1238   500301     ROCKLAND          2019                84               15.8
    ## 1239   500301     ROCKLAND          2019                78               14.4
    ## 1240   500301     ROCKLAND          2019                40               15.1
    ## 1241   500301     ROCKLAND          2019                38               13.7
    ## 1242   500301     ROCKLAND          2019                89               17.9
    ## 1243   500301     ROCKLAND          2019                43               17.7
    ## 1244   500301     ROCKLAND          2019                46               18.0
    ## 1245   500308     ROCKLAND          2019               137               17.2
    ## 1246   500308     ROCKLAND          2019                56               14.1
    ## 1247   500308     ROCKLAND          2019                81               20.2
    ## 1248   500308     ROCKLAND          2019                74               16.8
    ## 1249   500308     ROCKLAND          2019                28               13.7
    ## 1250   500308     ROCKLAND          2019                46               19.5
    ## 1251   500308     ROCKLAND          2019                63               17.7
    ## 1252   500308     ROCKLAND          2019                28               14.7
    ## 1253   500308     ROCKLAND          2019                35               21.2
    ## 1254   510401 ST. LAWRENCE          2019                15               14.6
    ## 1255   510401 ST. LAWRENCE          2019                 8               13.6
    ## 1256   510401 ST. LAWRENCE          2019                 7               15.9
    ## 1257   510401 ST. LAWRENCE          2019                 5                8.1
    ## 1258   510401 ST. LAWRENCE          2019                 5               13.9
    ## 1259   512201 ST. LAWRENCE          2019                64               17.5
    ## 1260   512201 ST. LAWRENCE          2019                37               21.0
    ## 1261   512201 ST. LAWRENCE          2019                27               14.2
    ## 1262   512201 ST. LAWRENCE          2019                39               18.1
    ## 1263   512201 ST. LAWRENCE          2019                21               19.8
    ## 1264   512201 ST. LAWRENCE          2019                18               16.5
    ## 1265   512201 ST. LAWRENCE          2019                25               16.6
    ## 1266   512201 ST. LAWRENCE          2019                16               22.9
    ## 1267   512201 ST. LAWRENCE          2019                 9               11.1
    ## 1268   520401     SARATOGA          2019                67               16.0
    ## 1269   520401     SARATOGA          2019                39               18.7
    ## 1270   520401     SARATOGA          2019                28               13.4
    ## 1271   520401     SARATOGA          2019                40               16.1
    ## 1272   520401     SARATOGA          2019                20               16.5
    ## 1273   520401     SARATOGA          2019                20               15.7
    ## 1274   520401     SARATOGA          2019                27               16.7
    ## 1275   520401     SARATOGA          2019                19               22.6
    ## 1276   520401     SARATOGA          2019                 8               10.3
    ## 1277   521401     SARATOGA          2019               212               18.0
    ## 1278   521401     SARATOGA          2019               120               21.4
    ## 1279   521401     SARATOGA          2019                92               15.0
    ## 1280   521401     SARATOGA          2019               139               18.2
    ## 1281   521401     SARATOGA          2019                74               20.9
    ## 1282   521401     SARATOGA          2019                65               15.9
    ## 1283   521401     SARATOGA          2019                73               17.7
    ## 1284   521401     SARATOGA          2019                46               22.3
    ## 1285   521401     SARATOGA          2019                27               13.0
    ## 1286   521800     SARATOGA          2019               317               14.0
    ## 1287   521800     SARATOGA          2019               167               14.7
    ## 1288   521800     SARATOGA          2019               150               13.3
    ## 1289   521800     SARATOGA          2019               174               13.2
    ## 1290   521800     SARATOGA          2019                85               13.0
    ## 1291   521800     SARATOGA          2019                89               13.4
    ## 1292   521800     SARATOGA          2019               143               15.1
    ## 1293   521800     SARATOGA          2019                82               17.1
    ## 1294   521800     SARATOGA          2019                61               13.1
    ## 1295   530301  SCHENECTADY          2019               190               14.8
    ## 1296   530301  SCHENECTADY          2019                94               15.0
    ## 1297   530301  SCHENECTADY          2019                96               14.6
    ## 1298   530301  SCHENECTADY          2019               108               14.3
    ## 1299   530301  SCHENECTADY          2019                48               13.3
    ## 1300   530301  SCHENECTADY          2019                60               15.2
    ## 1301   530301  SCHENECTADY          2019                82               15.6
    ## 1302   530301  SCHENECTADY          2019                46               17.4
    ## 1303   530301  SCHENECTADY          2019                36               13.7
    ## 1304   530515  SCHENECTADY          2019               150               15.7
    ## 1305   530515  SCHENECTADY          2019                73               16.4
    ## 1306   530515  SCHENECTADY          2019                77               15.2
    ## 1307   530515  SCHENECTADY          2019                74               13.5
    ## 1308   530515  SCHENECTADY          2019                36               14.4
    ## 1309   530515  SCHENECTADY          2019                38               12.7
    ## 1310   530515  SCHENECTADY          2019                76               18.8
    ## 1311   530515  SCHENECTADY          2019                37               19.0
    ## 1312   530515  SCHENECTADY          2019                39               18.7
    ## 1313   550101     SCHUYLER          2019                30               13.7
    ## 1314   550101     SCHUYLER          2019                16               13.9
    ## 1315   550101     SCHUYLER          2019                14               13.5
    ## 1316   550101     SCHUYLER          2019                25               15.1
    ## 1317   550101     SCHUYLER          2019                12               14.0
    ## 1318   550101     SCHUYLER          2019                13               16.3
    ## 1319   560701       SENECA          2019                85               21.4
    ## 1320   560701       SENECA          2019                44               24.0
    ## 1321   560701       SENECA          2019                41               19.1
    ## 1322   560701       SENECA          2019                54               23.9
    ## 1323   560701       SENECA          2019                32               32.7
    ## 1324   560701       SENECA          2019                22               17.2
    ## 1325   560701       SENECA          2019                31               18.0
    ## 1326   560701       SENECA          2019                12               14.1
    ## 1327   560701       SENECA          2019                19               21.8
    ## 1328   570302      STEUBEN          2019               101               17.2
    ## 1329   570302      STEUBEN          2019                53               19.1
    ## 1330   570302      STEUBEN          2019                48               15.5
    ## 1331   570302      STEUBEN          2019                69               18.5
    ## 1332   570302      STEUBEN          2019                36               20.8
    ## 1333   570302      STEUBEN          2019                33               16.5
    ## 1334   570302      STEUBEN          2019                32               15.0
    ## 1335   570302      STEUBEN          2019                17               16.2
    ## 1336   570302      STEUBEN          2019                15               13.8
    ## 1337   571800      STEUBEN          2019                91               16.0
    ## 1338   571800      STEUBEN          2019                44               16.7
    ## 1339   571800      STEUBEN          2019                47               15.4
    ## 1340   571800      STEUBEN          2019                43               13.2
    ## 1341   571800      STEUBEN          2019                23               14.0
    ## 1342   571800      STEUBEN          2019                20               12.4
    ## 1343   571800      STEUBEN          2019                48               20.0
    ## 1344   571800      STEUBEN          2019                21               21.4
    ## 1345   571800      STEUBEN          2019                27               19.0
    ## 1346   571901      STEUBEN          2019                21               13.9
    ## 1347   571901      STEUBEN          2019                10               13.0
    ## 1348   571901      STEUBEN          2019                11               14.9
    ## 1349   571901      STEUBEN          2019                 8                9.3
    ## 1350   571901      STEUBEN          2019                 8               19.0
    ## 1351   571901      STEUBEN          2019                 6               11.5
    ## 1352   572301      STEUBEN          2019                18               12.9
    ## 1353   572301      STEUBEN          2019                10               14.1
    ## 1354   572301      STEUBEN          2019                 8               11.8
    ## 1355   572301      STEUBEN          2019                 6                6.9
    ## 1356   572301      STEUBEN          2019                 6               13.3
    ## 1357   572301      STEUBEN          2019                 5               12.2
    ## 1358   572301      STEUBEN          2019                 5               21.7
    ## 1359   572702      STEUBEN          2019                21               18.9
    ## 1360   572702      STEUBEN          2019                 9               17.6
    ## 1361   572702      STEUBEN          2019                12               20.0
    ## 1362   572702      STEUBEN          2019                 8               18.6
    ## 1363   572702      STEUBEN          2019                 8               25.8
    ## 1364   572702      STEUBEN          2019                 7               11.7
    ## 1365   572702      STEUBEN          2019                 7               20.0
    ## 1366   580101      SUFFOLK          2019                93               17.0
    ## 1367   580101      SUFFOLK          2019                44               16.9
    ## 1368   580101      SUFFOLK          2019                49               17.1
    ## 1369   580101      SUFFOLK          2019                35               11.6
    ## 1370   580101      SUFFOLK          2019                12                8.6
    ## 1371   580101      SUFFOLK          2019                23               14.2
    ## 1372   580101      SUFFOLK          2019                58               24.0
    ## 1373   580101      SUFFOLK          2019                32               26.7
    ## 1374   580101      SUFFOLK          2019                26               21.3
    ## 1375   580103      SUFFOLK          2019               219               15.8
    ## 1376   580103      SUFFOLK          2019               118               16.8
    ## 1377   580103      SUFFOLK          2019               101               14.7
    ## 1378   580103      SUFFOLK          2019               117               13.7
    ## 1379   580103      SUFFOLK          2019                62               14.5
    ## 1380   580103      SUFFOLK          2019                55               13.0
    ## 1381   580103      SUFFOLK          2019               102               19.0
    ## 1382   580103      SUFFOLK          2019                56               20.4
    ## 1383   580103      SUFFOLK          2019                46               17.6
    ## 1384   580106      SUFFOLK          2019               184               16.3
    ## 1385   580106      SUFFOLK          2019                92               17.1
    ## 1386   580106      SUFFOLK          2019                92               15.6
    ## 1387   580106      SUFFOLK          2019               119               15.8
    ## 1388   580106      SUFFOLK          2019                57               16.0
    ## 1389   580106      SUFFOLK          2019                62               15.6
    ## 1390   580106      SUFFOLK          2019                65               17.3
    ## 1391   580106      SUFFOLK          2019                35               19.2
    ## 1392   580106      SUFFOLK          2019                30               15.5
    ## 1393   580107      SUFFOLK          2019               171               15.4
    ## 1394   580107      SUFFOLK          2019                82               15.0
    ## 1395   580107      SUFFOLK          2019                89               15.7
    ## 1396   580107      SUFFOLK          2019               101               14.0
    ## 1397   580107      SUFFOLK          2019                52               14.2
    ## 1398   580107      SUFFOLK          2019                49               13.8
    ## 1399   580107      SUFFOLK          2019                70               17.9
    ## 1400   580107      SUFFOLK          2019                30               16.7
    ## 1401   580107      SUFFOLK          2019                40               18.9
    ## 1402   580203      SUFFOLK          2019               161               15.4
    ## 1403   580203      SUFFOLK          2019                85               15.5
    ## 1404   580203      SUFFOLK          2019                76               15.3
    ## 1405   580203      SUFFOLK          2019                92               14.4
    ## 1406   580203      SUFFOLK          2019                49               14.2
    ## 1407   580203      SUFFOLK          2019                43               14.6
    ## 1408   580203      SUFFOLK          2019                69               17.2
    ## 1409   580203      SUFFOLK          2019                36               17.9
    ## 1410   580203      SUFFOLK          2019                33               16.5
    ## 1411   580205      SUFFOLK          2019               774               18.4
    ## 1412   580205      SUFFOLK          2019               367               17.9
    ## 1413   580205      SUFFOLK          2019               407               18.9
    ## 1414   580205      SUFFOLK          2019               439               17.0
    ## 1415   580205      SUFFOLK          2019               212               17.0
    ## 1416   580205      SUFFOLK          2019               227               17.1
    ## 1417   580205      SUFFOLK          2019               335               20.5
    ## 1418   580205      SUFFOLK          2019               155               19.3
    ## 1419   580205      SUFFOLK          2019               180               21.8
    ## 1420   580206      SUFFOLK          2019                50               13.6
    ## 1421   580206      SUFFOLK          2019                24               13.4
    ## 1422   580206      SUFFOLK          2019                26               13.8
    ## 1423   580206      SUFFOLK          2019                23               11.3
    ## 1424   580206      SUFFOLK          2019                12               11.1
    ## 1425   580206      SUFFOLK          2019                11               11.5
    ## 1426   580206      SUFFOLK          2019                27               17.8
    ## 1427   580206      SUFFOLK          2019                12               18.2
    ## 1428   580206      SUFFOLK          2019                15               17.4
    ## 1429   580207      SUFFOLK          2019               127               15.9
    ## 1430   580207      SUFFOLK          2019                58               15.9
    ## 1431   580207      SUFFOLK          2019                69               15.9
    ## 1432   580207      SUFFOLK          2019                67               15.1
    ## 1433   580207      SUFFOLK          2019                29               15.0
    ## 1434   580207      SUFFOLK          2019                38               15.1
    ## 1435   580207      SUFFOLK          2019                60               16.9
    ## 1436   580207      SUFFOLK          2019                29               16.9
    ## 1437   580207      SUFFOLK          2019                31               16.8
    ## 1438   580211      SUFFOLK          2019               492               17.3
    ## 1439   580211      SUFFOLK          2019               241               17.7
    ## 1440   580211      SUFFOLK          2019               251               17.0
    ## 1441   580211      SUFFOLK          2019               249               16.3
    ## 1442   580211      SUFFOLK          2019               130               17.0
    ## 1443   580211      SUFFOLK          2019               119               15.6
    ## 1444   580211      SUFFOLK          2019               243               18.5
    ## 1445   580211      SUFFOLK          2019               111               18.6
    ## 1446   580211      SUFFOLK          2019               132               18.5
    ## 1447   580224      SUFFOLK          2019               459               16.6
    ## 1448   580224      SUFFOLK          2019               230               16.8
    ## 1449   580224      SUFFOLK          2019               229               16.4
    ## 1450   580224      SUFFOLK          2019               268               15.5
    ## 1451   580224      SUFFOLK          2019               127               15.2
    ## 1452   580224      SUFFOLK          2019               141               15.9
    ## 1453   580224      SUFFOLK          2019               191               18.3
    ## 1454   580224      SUFFOLK          2019               103               19.3
    ## 1455   580224      SUFFOLK          2019                88               17.2
    ## 1456   580232      SUFFOLK          2019               630               18.6
    ## 1457   580232      SUFFOLK          2019               320               19.6
    ## 1458   580232      SUFFOLK          2019               310               17.7
    ## 1459   580232      SUFFOLK          2019               389               19.4
    ## 1460   580232      SUFFOLK          2019               195               20.8
    ## 1461   580232      SUFFOLK          2019               194               18.1
    ## 1462   580232      SUFFOLK          2019               241               17.5
    ## 1463   580232      SUFFOLK          2019               125               18.0
    ## 1464   580232      SUFFOLK          2019               116               17.0
    ## 1465   580233      SUFFOLK          2019                82               14.7
    ## 1466   580233      SUFFOLK          2019                39               13.5
    ## 1467   580233      SUFFOLK          2019                43               15.9
    ## 1468   580233      SUFFOLK          2019                44               15.0
    ## 1469   580233      SUFFOLK          2019                19               13.6
    ## 1470   580233      SUFFOLK          2019                25               16.2
    ## 1471   580233      SUFFOLK          2019                38               14.5
    ## 1472   580233      SUFFOLK          2019                20               13.5
    ## 1473   580233      SUFFOLK          2019                18               15.8
    ## 1474   580235      SUFFOLK          2019               274               20.7
    ## 1475   580235      SUFFOLK          2019               132               20.2
    ## 1476   580235      SUFFOLK          2019               142               21.2
    ## 1477   580235      SUFFOLK          2019               165               20.4
    ## 1478   580235      SUFFOLK          2019                77               18.4
    ## 1479   580235      SUFFOLK          2019                88               22.6
    ## 1480   580235      SUFFOLK          2019               109               21.0
    ## 1481   580235      SUFFOLK          2019                55               23.3
    ## 1482   580235      SUFFOLK          2019                54               19.1
    ## 1483   580404      SUFFOLK          2019               310               16.9
    ## 1484   580404      SUFFOLK          2019               151               16.7
    ## 1485   580404      SUFFOLK          2019               159               17.1
    ## 1486   580404      SUFFOLK          2019               159               16.2
    ## 1487   580404      SUFFOLK          2019                74               15.4
    ## 1488   580404      SUFFOLK          2019                85               17.0
    ## 1489   580404      SUFFOLK          2019               151               17.7
    ## 1490   580404      SUFFOLK          2019                77               18.1
    ## 1491   580404      SUFFOLK          2019                74               17.3
    ## 1492   580405      SUFFOLK          2019               413               15.2
    ## 1493   580405      SUFFOLK          2019               229               17.4
    ## 1494   580405      SUFFOLK          2019               184               13.2
    ## 1495   580405      SUFFOLK          2019               214               14.1
    ## 1496   580405      SUFFOLK          2019               121               16.5
    ## 1497   580405      SUFFOLK          2019                93               11.9
    ## 1498   580405      SUFFOLK          2019               199               16.7
    ## 1499   580405      SUFFOLK          2019               108               18.5
    ## 1500   580405      SUFFOLK          2019                91               14.9
    ## 1501   580406      SUFFOLK          2019               140               14.9
    ## 1502   580406      SUFFOLK          2019                71               15.6
    ## 1503   580406      SUFFOLK          2019                69               14.3
    ## 1504   580406      SUFFOLK          2019                75               15.9
    ## 1505   580406      SUFFOLK          2019                36               16.7
    ## 1506   580406      SUFFOLK          2019                39               15.2
    ## 1507   580406      SUFFOLK          2019                65               13.9
    ## 1508   580406      SUFFOLK          2019                35               14.6
    ## 1509   580406      SUFFOLK          2019                30               13.3
    ## 1510   580502      SUFFOLK          2019               141               15.7
    ## 1511   580502      SUFFOLK          2019                65               15.3
    ## 1512   580502      SUFFOLK          2019                76               16.0
    ## 1513   580502      SUFFOLK          2019                62               14.4
    ## 1514   580502      SUFFOLK          2019                32               16.0
    ## 1515   580502      SUFFOLK          2019                30               13.0
    ## 1516   580502      SUFFOLK          2019                79               16.8
    ## 1517   580502      SUFFOLK          2019                33               14.7
    ## 1518   580502      SUFFOLK          2019                46               18.8
    ## 1519   580503      SUFFOLK          2019               230               16.9
    ## 1520   580503      SUFFOLK          2019               103               15.6
    ## 1521   580503      SUFFOLK          2019               127               18.2
    ## 1522   580503      SUFFOLK          2019               124               15.6
    ## 1523   580503      SUFFOLK          2019                56               13.9
    ## 1524   580503      SUFFOLK          2019                68               17.3
    ## 1525   580503      SUFFOLK          2019               106               18.8
    ## 1526   580503      SUFFOLK          2019                47               18.2
    ## 1527   580503      SUFFOLK          2019                59               19.3
    ## 1528   580506      SUFFOLK          2019                72               12.3
    ## 1529   580506      SUFFOLK          2019                43               15.2
    ## 1530   580506      SUFFOLK          2019                29                9.7
    ## 1531   580506      SUFFOLK          2019                11                5.3
    ## 1532   580506      SUFFOLK          2019                 5                4.9
    ## 1533   580506      SUFFOLK          2019                 6                5.8
    ## 1534   580506      SUFFOLK          2019                61               16.2
    ## 1535   580506      SUFFOLK          2019                38               21.0
    ## 1536   580506      SUFFOLK          2019                23               11.7
    ## 1537   580509      SUFFOLK          2019               227               16.5
    ## 1538   580509      SUFFOLK          2019               109               16.3
    ## 1539   580509      SUFFOLK          2019               118               16.6
    ## 1540   580509      SUFFOLK          2019               118               15.6
    ## 1541   580509      SUFFOLK          2019                49               13.9
    ## 1542   580509      SUFFOLK          2019                69               17.1
    ## 1543   580509      SUFFOLK          2019               109               17.6
    ## 1544   580509      SUFFOLK          2019                60               19.0
    ## 1545   580509      SUFFOLK          2019                49               16.0
    ## 1546   580602      SUFFOLK          2019               341               19.3
    ## 1547   580602      SUFFOLK          2019               170               19.1
    ## 1548   580602      SUFFOLK          2019               171               19.5
    ## 1549   580602      SUFFOLK          2019               197               19.2
    ## 1550   580602      SUFFOLK          2019               103               19.6
    ## 1551   580602      SUFFOLK          2019                94               18.7
    ## 1552   580602      SUFFOLK          2019               144               19.5
    ## 1553   580602      SUFFOLK          2019                67               18.6
    ## 1554   580602      SUFFOLK          2019                77               20.4
    ## 1555   580801      SUFFOLK          2019               343               15.2
    ## 1556   580801      SUFFOLK          2019               173               15.5
    ## 1557   580801      SUFFOLK          2019               170               15.0
    ## 1558   580801      SUFFOLK          2019               155               13.4
    ## 1559   580801      SUFFOLK          2019                74               12.8
    ## 1560   580801      SUFFOLK          2019                81               14.0
    ## 1561   580801      SUFFOLK          2019               188               17.2
    ## 1562   580801      SUFFOLK          2019                99               18.4
    ## 1563   580801      SUFFOLK          2019                89               16.0
    ## 1564   580913      SUFFOLK          2019                48               38.4
    ## 1565   580913      SUFFOLK          2019                18               28.6
    ## 1566   580913      SUFFOLK          2019                30               48.4
    ## 1567   580913      SUFFOLK          2019                40               43.5
    ## 1568   580913      SUFFOLK          2019                15               28.8
    ## 1569   580913      SUFFOLK          2019                25               62.5
    ## 1570   590501     SULLIVAN          2019                61               15.8
    ## 1571   590501     SULLIVAN          2019                30               15.8
    ## 1572   590501     SULLIVAN          2019                31               15.8
    ## 1573   590501     SULLIVAN          2019                44               16.2
    ## 1574   590501     SULLIVAN          2019                22               16.4
    ## 1575   590501     SULLIVAN          2019                22               16.1
    ## 1576   590501     SULLIVAN          2019                17               14.8
    ## 1577   590501     SULLIVAN          2019                 8               14.3
    ## 1578   590501     SULLIVAN          2019                 9               15.3
    ## 1579   590801     SULLIVAN          2019                33               14.9
    ## 1580   590801     SULLIVAN          2019                19               16.4
    ## 1581   590801     SULLIVAN          2019                14               13.3
    ## 1582   590801     SULLIVAN          2019                14               10.8
    ## 1583   590801     SULLIVAN          2019                 5                7.6
    ## 1584   590801     SULLIVAN          2019                 9               14.1
    ## 1585   590801     SULLIVAN          2019                19               22.4
    ## 1586   590801     SULLIVAN          2019                14               28.6
    ## 1587   590801     SULLIVAN          2019                 5               13.9
    ## 1588   591502     SULLIVAN          2019                76               17.2
    ## 1589   591502     SULLIVAN          2019                41               19.6
    ## 1590   591502     SULLIVAN          2019                35               15.0
    ## 1591   591502     SULLIVAN          2019                48               17.8
    ## 1592   591502     SULLIVAN          2019                29               21.5
    ## 1593   591502     SULLIVAN          2019                19               14.1
    ## 1594   591502     SULLIVAN          2019                28               16.8
    ## 1595   591502     SULLIVAN          2019                12               16.2
    ## 1596   591502     SULLIVAN          2019                16               17.2
    ## 1597   600101        TIOGA          2019                71               19.4
    ## 1598   600101        TIOGA          2019                38               22.0
    ## 1599   600101        TIOGA          2019                33               17.1
    ## 1600   600101        TIOGA          2019                29               17.7
    ## 1601   600101        TIOGA          2019                12               16.9
    ## 1602   600101        TIOGA          2019                17               18.3
    ## 1603   600101        TIOGA          2019                42               20.8
    ## 1604   600101        TIOGA          2019                26               25.5
    ## 1605   600101        TIOGA          2019                16               16.0
    ## 1606   600301        TIOGA          2019                19               11.0
    ## 1607   600301        TIOGA          2019                10               12.2
    ## 1608   600301        TIOGA          2019                 9               10.0
    ## 1609   600301        TIOGA          2019                17               11.8
    ## 1610   600301        TIOGA          2019                10               14.5
    ## 1611   600301        TIOGA          2019                 7                9.3
    ## 1612   600801        TIOGA          2019                40               15.4
    ## 1613   600801        TIOGA          2019                18               13.3
    ## 1614   600801        TIOGA          2019                22               17.6
    ## 1615   600801        TIOGA          2019                23               13.2
    ## 1616   600801        TIOGA          2019                12               14.5
    ## 1617   600801        TIOGA          2019                11               12.1
    ## 1618   600801        TIOGA          2019                17               19.8
    ## 1619   600801        TIOGA          2019                 6               11.5
    ## 1620   600801        TIOGA          2019                11               32.4
    ## 1621   610501     TOMPKINS          2019                44               17.3
    ## 1622   610501     TOMPKINS          2019                26               20.0
    ## 1623   610501     TOMPKINS          2019                18               14.4
    ## 1624   610501     TOMPKINS          2019                28               18.4
    ## 1625   610501     TOMPKINS          2019                15               18.5
    ## 1626   610501     TOMPKINS          2019                13               18.3
    ## 1627   610501     TOMPKINS          2019                16               15.5
    ## 1628   610501     TOMPKINS          2019                11               22.4
    ## 1629   610501     TOMPKINS          2019                 5                9.3
    ## 1630   610600     TOMPKINS          2019               238               12.4
    ## 1631   610600     TOMPKINS          2019               121               12.6
    ## 1632   610600     TOMPKINS          2019               117               12.2
    ## 1633   610600     TOMPKINS          2019               152               12.3
    ## 1634   610600     TOMPKINS          2019                78               12.6
    ## 1635   610600     TOMPKINS          2019                74               12.0
    ## 1636   610600     TOMPKINS          2019                86               12.5
    ## 1637   610600     TOMPKINS          2019                43               12.5
    ## 1638   610600     TOMPKINS          2019                43               12.6
    ## 1639   610801     TOMPKINS          2019                61               15.2
    ## 1640   610801     TOMPKINS          2019                21               11.3
    ## 1641   610801     TOMPKINS          2019                40               18.6
    ## 1642   610801     TOMPKINS          2019                35               13.9
    ## 1643   610801     TOMPKINS          2019                11                8.7
    ## 1644   610801     TOMPKINS          2019                24               19.2
    ## 1645   610801     TOMPKINS          2019                26               17.4
    ## 1646   610801     TOMPKINS          2019                10               16.9
    ## 1647   610801     TOMPKINS          2019                16               17.8
    ## 1648   620600       ULSTER          2019               378               17.5
    ## 1649   620600       ULSTER          2019               198               18.1
    ## 1650   620600       ULSTER          2019               180               16.8
    ## 1651   620600       ULSTER          2019               234               16.6
    ## 1652   620600       ULSTER          2019               115               16.1
    ## 1653   620600       ULSTER          2019               119               17.0
    ## 1654   620600       ULSTER          2019               144               19.2
    ## 1655   620600       ULSTER          2019                83               22.0
    ## 1656   620600       ULSTER          2019                61               16.4
    ## 1657   620803       ULSTER          2019                94               16.3
    ## 1658   620803       ULSTER          2019                47               17.3
    ## 1659   620803       ULSTER          2019                47               15.5
    ## 1660   620803       ULSTER          2019                54               15.3
    ## 1661   620803       ULSTER          2019                29               17.0
    ## 1662   620803       ULSTER          2019                25               13.8
    ## 1663   620803       ULSTER          2019                40               17.9
    ## 1664   620803       ULSTER          2019                18               17.8
    ## 1665   620803       ULSTER          2019                22               17.9
    ## 1666   621101       ULSTER          2019               119               17.6
    ## 1667   621101       ULSTER          2019                59               17.9
    ## 1668   621101       ULSTER          2019                60               17.2
    ## 1669   621101       ULSTER          2019                66               18.1
    ## 1670   621101       ULSTER          2019                37               20.4
    ## 1671   621101       ULSTER          2019                29               15.8
    ## 1672   621101       ULSTER          2019                53               17.2
    ## 1673   621101       ULSTER          2019                22               14.9
    ## 1674   621101       ULSTER          2019                31               19.3
    ## 1675   621801       ULSTER          2019               186               17.4
    ## 1676   621801       ULSTER          2019                90               16.9
    ## 1677   621801       ULSTER          2019                96               17.8
    ## 1678   621801       ULSTER          2019                92               15.9
    ## 1679   621801       ULSTER          2019                43               14.9
    ## 1680   621801       ULSTER          2019                49               16.9
    ## 1681   621801       ULSTER          2019                94               19.3
    ## 1682   621801       ULSTER          2019                47               19.3
    ## 1683   621801       ULSTER          2019                47               19.2
    ## 1684   630300       WARREN          2019               115               16.0
    ## 1685   630300       WARREN          2019                66               16.9
    ## 1686   630300       WARREN          2019                49               14.9
    ## 1687   630300       WARREN          2019                70               16.4
    ## 1688   630300       WARREN          2019                37               15.8
    ## 1689   630300       WARREN          2019                33               17.1
    ## 1690   630300       WARREN          2019                45               15.8
    ## 1691   630300       WARREN          2019                29               19.0
    ## 1692   630300       WARREN          2019                16               12.2
    ## 1693   630902       WARREN          2019               188               16.7
    ## 1694   630902       WARREN          2019                99               18.5
    ## 1695   630902       WARREN          2019                89               15.1
    ## 1696   630902       WARREN          2019               107               16.2
    ## 1697   630902       WARREN          2019                58               19.0
    ## 1698   630902       WARREN          2019                49               13.8
    ## 1699   630902       WARREN          2019                81               17.5
    ## 1700   630902       WARREN          2019                41               17.8
    ## 1701   630902       WARREN          2019                40               17.2
    ## 1702   640701   WASHINGTON          2019                51               18.0
    ## 1703   640701   WASHINGTON          2019                19               14.0
    ## 1704   640701   WASHINGTON          2019                32               21.6
    ## 1705   640701   WASHINGTON          2019                34               20.6
    ## 1706   640701   WASHINGTON          2019                14               17.3
    ## 1707   640701   WASHINGTON          2019                20               23.8
    ## 1708   640701   WASHINGTON          2019                17               14.4
    ## 1709   640701   WASHINGTON          2019                 5                9.3
    ## 1710   640701   WASHINGTON          2019                12               18.8
    ## 1711   641301   WASHINGTON          2019               165               18.7
    ## 1712   641301   WASHINGTON          2019                89               20.1
    ## 1713   641301   WASHINGTON          2019                76               17.2
    ## 1714   641301   WASHINGTON          2019               105               17.8
    ## 1715   641301   WASHINGTON          2019                59               20.0
    ## 1716   641301   WASHINGTON          2019                46               15.6
    ## 1717   641301   WASHINGTON          2019                60               20.8
    ## 1718   641301   WASHINGTON          2019                30               21.0
    ## 1719   641301   WASHINGTON          2019                30               20.5
    ## 1720   650901        WAYNE          2019                71               19.2
    ## 1721   650901        WAYNE          2019                44               23.7
    ## 1722   650901        WAYNE          2019                27               14.7
    ## 1723   650901        WAYNE          2019                51               18.7
    ## 1724   650901        WAYNE          2019                29               20.7
    ## 1725   650901        WAYNE          2019                22               16.5
    ## 1726   650901        WAYNE          2019                20               22.2
    ## 1727   650901        WAYNE          2019                 5               10.2
    ## 1728   651503        WAYNE          2019                38               14.4
    ## 1729   651503        WAYNE          2019                18               14.4
    ## 1730   651503        WAYNE          2019                20               14.5
    ## 1731   651503        WAYNE          2019                15                9.5
    ## 1732   651503        WAYNE          2019                 8               10.5
    ## 1733   651503        WAYNE          2019                 7                8.5
    ## 1734   651503        WAYNE          2019                23               21.9
    ## 1735   651503        WAYNE          2019                10               20.4
    ## 1736   651503        WAYNE          2019                13               23.2
    ## 1737   660202  WESTCHESTER          2019                94               15.9
    ## 1738   660202  WESTCHESTER          2019                47               16.5
    ## 1739   660202  WESTCHESTER          2019                47               15.3
    ## 1740   660202  WESTCHESTER          2019                54               15.3
    ## 1741   660202  WESTCHESTER          2019                25               15.2
    ## 1742   660202  WESTCHESTER          2019                29               15.3
    ## 1743   660202  WESTCHESTER          2019                40               17.5
    ## 1744   660202  WESTCHESTER          2019                22               19.0
    ## 1745   660202  WESTCHESTER          2019                18               16.1
    ## 1746   660203  WESTCHESTER          2019               148               16.4
    ## 1747   660203  WESTCHESTER          2019                69               16.2
    ## 1748   660203  WESTCHESTER          2019                79               16.7
    ## 1749   660203  WESTCHESTER          2019                85               17.5
    ## 1750   660203  WESTCHESTER          2019                39               17.4
    ## 1751   660203  WESTCHESTER          2019                46               17.6
    ## 1752   660203  WESTCHESTER          2019                63               15.3
    ## 1753   660203  WESTCHESTER          2019                30               15.1
    ## 1754   660203  WESTCHESTER          2019                33               15.6
    ## 1755   660402  WESTCHESTER          2019                85               15.0
    ## 1756   660402  WESTCHESTER          2019                37               13.8
    ## 1757   660402  WESTCHESTER          2019                48               16.2
    ## 1758   660402  WESTCHESTER          2019                52               16.1
    ## 1759   660402  WESTCHESTER          2019                21               13.8
    ## 1760   660402  WESTCHESTER          2019                31               18.1
    ## 1761   660402  WESTCHESTER          2019                33               14.1
    ## 1762   660402  WESTCHESTER          2019                16               13.7
    ## 1763   660402  WESTCHESTER          2019                17               14.5
    ## 1764   660405  WESTCHESTER          2019               102               12.3
    ## 1765   660405  WESTCHESTER          2019                46               12.2
    ## 1766   660405  WESTCHESTER          2019                56               12.5
    ## 1767   660405  WESTCHESTER          2019                59               11.4
    ## 1768   660405  WESTCHESTER          2019                30               12.3
    ## 1769   660405  WESTCHESTER          2019                29               10.5
    ## 1770   660405  WESTCHESTER          2019                43               13.9
    ## 1771   660405  WESTCHESTER          2019                16               11.9
    ## 1772   660405  WESTCHESTER          2019                27               15.5
    ## 1773   660407  WESTCHESTER          2019               115               18.3
    ## 1774   660407  WESTCHESTER          2019                54               18.3
    ## 1775   660407  WESTCHESTER          2019                61               18.2
    ## 1776   660407  WESTCHESTER          2019                91               18.2
    ## 1777   660407  WESTCHESTER          2019                43               18.1
    ## 1778   660407  WESTCHESTER          2019                48               18.3
    ## 1779   660407  WESTCHESTER          2019                24               19.2
    ## 1780   660407  WESTCHESTER          2019                11               20.0
    ## 1781   660407  WESTCHESTER          2019                13               18.6
    ## 1782   660501  WESTCHESTER          2019               182               13.6
    ## 1783   660501  WESTCHESTER          2019               102               15.0
    ## 1784   660501  WESTCHESTER          2019                80               12.1
    ## 1785   660501  WESTCHESTER          2019               108               12.6
    ## 1786   660501  WESTCHESTER          2019                55               12.8
    ## 1787   660501  WESTCHESTER          2019                53               12.4
    ## 1788   660501  WESTCHESTER          2019                74               15.4
    ## 1789   660501  WESTCHESTER          2019                47               18.7
    ## 1790   660501  WESTCHESTER          2019                27               11.7
    ## 1791   660801  WESTCHESTER          2019               101               14.2
    ## 1792   660801  WESTCHESTER          2019                50               15.7
    ## 1793   660801  WESTCHESTER          2019                51               13.0
    ## 1794   660801  WESTCHESTER          2019                61               13.6
    ## 1795   660801  WESTCHESTER          2019                30               15.7
    ## 1796   660801  WESTCHESTER          2019                31               12.1
    ## 1797   660801  WESTCHESTER          2019                40               15.7
    ## 1798   660801  WESTCHESTER          2019                20               16.3
    ## 1799   660801  WESTCHESTER          2019                20               15.2
    ## 1800   660805  WESTCHESTER          2019                92               18.4
    ## 1801   660805  WESTCHESTER          2019                54               21.6
    ## 1802   660805  WESTCHESTER          2019                38               15.1
    ## 1803   660805  WESTCHESTER          2019                47               15.9
    ## 1804   660805  WESTCHESTER          2019                27               18.9
    ## 1805   660805  WESTCHESTER          2019                20               13.2
    ## 1806   660805  WESTCHESTER          2019                45               21.8
    ## 1807   660805  WESTCHESTER          2019                27               25.2
    ## 1808   660805  WESTCHESTER          2019                18               18.2
    ## 1809   660809  WESTCHESTER          2019                66               12.4
    ## 1810   660809  WESTCHESTER          2019                33               13.4
    ## 1811   660809  WESTCHESTER          2019                33               11.5
    ## 1812   660809  WESTCHESTER          2019                31               10.1
    ## 1813   660809  WESTCHESTER          2019                16               11.0
    ## 1814   660809  WESTCHESTER          2019                15                9.4
    ## 1815   660809  WESTCHESTER          2019                35               15.4
    ## 1816   660809  WESTCHESTER          2019                17               17.0
    ## 1817   660809  WESTCHESTER          2019                18               14.1
    ## 1818   660900  WESTCHESTER          2019               216               13.8
    ## 1819   660900  WESTCHESTER          2019               109               13.8
    ## 1820   660900  WESTCHESTER          2019               107               13.8
    ## 1821   660900  WESTCHESTER          2019               151               15.4
    ## 1822   660900  WESTCHESTER          2019                80               15.7
    ## 1823   660900  WESTCHESTER          2019                71               15.1
    ## 1824   660900  WESTCHESTER          2019                65               11.1
    ## 1825   660900  WESTCHESTER          2019                29               10.4
    ## 1826   660900  WESTCHESTER          2019                36               11.8
    ## 1827   661100  WESTCHESTER          2019               474               15.3
    ## 1828   661100  WESTCHESTER          2019               238               16.3
    ## 1829   661100  WESTCHESTER          2019               236               14.4
    ## 1830   661100  WESTCHESTER          2019               267               15.0
    ## 1831   661100  WESTCHESTER          2019               134               15.7
    ## 1832   661100  WESTCHESTER          2019               133               14.3
    ## 1833   661100  WESTCHESTER          2019               207               15.8
    ## 1834   661100  WESTCHESTER          2019               104               17.2
    ## 1835   661100  WESTCHESTER          2019               103               14.5
    ## 1836   661301  WESTCHESTER          2019                53               16.4
    ## 1837   661301  WESTCHESTER          2019                29               17.4
    ## 1838   661301  WESTCHESTER          2019                24               15.3
    ## 1839   661301  WESTCHESTER          2019                29               14.1
    ## 1840   661301  WESTCHESTER          2019                18               17.5
    ## 1841   661301  WESTCHESTER          2019                11               10.7
    ## 1842   661301  WESTCHESTER          2019                24               20.3
    ## 1843   661301  WESTCHESTER          2019                11               17.2
    ## 1844   661301  WESTCHESTER          2019                13               24.1
    ## 1845   661601  WESTCHESTER          2019                82               10.5
    ## 1846   661601  WESTCHESTER          2019                42               11.2
    ## 1847   661601  WESTCHESTER          2019                40                9.8
    ## 1848   661601  WESTCHESTER          2019                51               10.5
    ## 1849   661601  WESTCHESTER          2019                29               12.9
    ## 1850   661601  WESTCHESTER          2019                22                8.4
    ## 1851   661601  WESTCHESTER          2019                31               10.6
    ## 1852   661601  WESTCHESTER          2019                13                9.0
    ## 1853   661601  WESTCHESTER          2019                18               12.2
    ## 1854   661901  WESTCHESTER          2019                50                9.2
    ## 1855   661901  WESTCHESTER          2019                21                8.5
    ## 1856   661901  WESTCHESTER          2019                29                9.8
    ## 1857   661901  WESTCHESTER          2019                26                8.2
    ## 1858   661901  WESTCHESTER          2019                13                8.8
    ## 1859   661901  WESTCHESTER          2019                13                7.7
    ## 1860   661901  WESTCHESTER          2019                24               11.1
    ## 1861   661901  WESTCHESTER          2019                 8                8.5
    ## 1862   661901  WESTCHESTER          2019                16               13.0
    ## 1863   662001  WESTCHESTER          2019               204               12.5
    ## 1864   662001  WESTCHESTER          2019               114               14.0
    ## 1865   662001  WESTCHESTER          2019                90               11.0
    ## 1866   662001  WESTCHESTER          2019               121               12.7
    ## 1867   662001  WESTCHESTER          2019                77               15.9
    ## 1868   662001  WESTCHESTER          2019                44                9.4
    ## 1869   662001  WESTCHESTER          2019                83               12.2
    ## 1870   662001  WESTCHESTER          2019                37               11.1
    ## 1871   662001  WESTCHESTER          2019                46               13.1
    ## 1872   662101  WESTCHESTER          2019                79               14.2
    ## 1873   662101  WESTCHESTER          2019                40               15.2
    ## 1874   662101  WESTCHESTER          2019                39               13.4
    ## 1875   662101  WESTCHESTER          2019                44               16.6
    ## 1876   662101  WESTCHESTER          2019                23               18.4
    ## 1877   662101  WESTCHESTER          2019                21               15.0
    ## 1878   662101  WESTCHESTER          2019                35               12.2
    ## 1879   662101  WESTCHESTER          2019                17               12.7
    ## 1880   662101  WESTCHESTER          2019                18               11.8
    ## 1881   670401      WYOMING          2019                48               17.1
    ## 1882   670401      WYOMING          2019                22               17.2
    ## 1883   670401      WYOMING          2019                26               17.0
    ## 1884   670401      WYOMING          2019                23               13.1
    ## 1885   670401      WYOMING          2019                10               13.5
    ## 1886   670401      WYOMING          2019                13               12.7
    ## 1887   670401      WYOMING          2019                25               23.8
    ## 1888   670401      WYOMING          2019                12               22.2
    ## 1889   670401      WYOMING          2019                13               25.5
    ## 1890   671501      WYOMING          2019                53               18.7
    ## 1891   671501      WYOMING          2019                27               18.5
    ## 1892   671501      WYOMING          2019                26               18.8
    ## 1893   671501      WYOMING          2019                38               22.4
    ## 1894   671501      WYOMING          2019                21               25.3
    ## 1895   671501      WYOMING          2019                17               19.5
    ## 1896   671501      WYOMING          2019                15               13.2
    ## 1897   671501      WYOMING          2019                 6                9.5
    ## 1898   671501      WYOMING          2019                 9               17.6
    ## 1899   680801        YATES          2019                41               17.8
    ## 1900   680801        YATES          2019                19               17.3
    ## 1901   680801        YATES          2019                22               18.3
    ## 1902   680801        YATES          2019                26               18.6
    ## 1903   680801        YATES          2019                12               17.9
    ## 1904   680801        YATES          2019                14               19.2
    ## 1905   680801        YATES          2019                15               16.7
    ## 1906   680801        YATES          2019                 7               16.3
    ## 1907   680801        YATES          2019                 8               17.0
    ##      number_obese percent_obese number_overweight_or_obese
    ## 1             115          16.9                        208
    ## 2              45          13.4                         94
    ## 3              70          20.2                        114
    ## 4              70          14.1                        133
    ## 5              28          11.4                         56
    ## 6              42          16.7                         77
    ## 7              45          25.0                         75
    ## 8              17          19.1                         38
    ## 9              28          30.8                         37
    ## 10             43          19.5                         82
    ## 11             52          27.7                         86
    ## 12             81          20.3                        156
    ## 13             38          21.1                         74
    ## 14            133          22.6                        242
    ## 15             68          23.6                        122
    ## 16             65          21.7                        120
    ## 17             30          27.8                         48
    ## 18             22          27.5                         38
    ## 19             63          20.1                        123
    ## 20            312          19.5                        569
    ## 21            160          19.5                        290
    ## 22             69          23.5                        115
    ## 23            180          18.1                        331
    ## 24             97          19.1                        167
    ## 25             83          16.9                        164
    ## 26            152          19.4                        279
    ## 27            132          21.7                        238
    ## 28            268          13.8                        602
    ## 29            107          11.1                        286
    ## 30            161          16.4                        316
    ## 31            149          14.3                        314
    ## 32             68          12.8                        157
    ## 33             81          15.9                        157
    ## 34            119          13.2                        288
    ## 35             39           9.1                        129
    ## 36             80          16.9                        159
    ## 37            220          13.5                        504
    ## 38             94          11.5                        237
    ## 39             32          10.1                        101
    ## 40             56          17.3                        112
    ## 41             62          12.4                        136
    ## 42             70          14.3                        155
    ## 43            126          15.5                        267
    ## 44            132          13.3                        291
    ## 45             88          13.7                        213
    ## 46             22          17.5                         43
    ## 47             46          21.2                         81
    ## 48             39          15.7                         79
    ## 49             17          13.8                         36
    ## 50             86          20.0                        162
    ## 51             40          18.9                         81
    ## 52             24          28.6                         38
    ## 53             47          27.5                         83
    ## 54             23          26.4                         45
    ## 55             94          19.4                        180
    ## 56             38          17.4                         88
    ## 57             56          21.1                         92
    ## 58             36          13.8                         84
    ## 59             14          12.3                         39
    ## 60             22          15.1                         45
    ## 61             58          26.0                         96
    ## 62             24          22.9                         49
    ## 63             34          28.8                         47
    ## 64             91          19.4                        171
    ## 65            115          30.0                        170
    ## 66            193          20.9                        337
    ## 67            102          22.4                        166
    ## 68            308          23.5                        507
    ## 69            155          24.0                        248
    ## 70            153          23.0                        259
    ## 71             53          28.3                         82
    ## 72             62          31.6                         88
    ## 73             64          23.3                        103
    ## 74             30          24.6                         45
    ## 75             15          31.3                         21
    ## 76             12          23.1                         21
    ## 77             15          20.3                         24
    ## 78             22          21.8                         37
    ## 79             34          22.2                         58
    ## 80             37          21.1                         61
    ## 81             27          27.0                         42
    ## 82             90          18.4                        180
    ## 83             39          18.1                         78
    ## 84             27          16.8                         58
    ## 85             51          18.7                        102
    ## 86             55          17.5                        115
    ## 87             28          18.2                         57
    ## 88             35          20.2                         65
    ## 89             11          17.7                         21
    ## 90             24          21.6                         44
    ## 91             69          17.0                        137
    ## 92             46          19.9                         91
    ## 93             73          13.3                        156
    ## 94             36          13.6                         75
    ## 95             37          13.0                         81
    ## 96            119          15.2                        247
    ## 97             50          13.3                        110
    ## 98             14          12.7                         35
    ## 99             32          26.4                         56
    ## 100            85          16.9                        168
    ## 101            32          13.5                         80
    ## 102            53          19.9                         88
    ## 103            53          17.4                        104
    ## 104            21          14.3                         52
    ## 105            32          20.4                         52
    ## 106            32          16.3                         64
    ## 107            11          12.2                         28
    ## 108            21          19.8                         36
    ## 109            19          20.4                         30
    ## 110            28          20.6                         51
    ## 111            29          17.3                         52
    ## 112            10          13.3                         22
    ## 113            47          18.4                         95
    ## 114            19          16.0                         44
    ## 115            18          20.7                         43
    ## 116             9          20.5                         22
    ## 117             9          20.9                         21
    ## 118             5           9.6                         10
    ## 119            26          17.7                         55
    ## 120            17          19.1                         39
    ## 121            11          22.4                         25
    ## 122            16          20.8                         32
    ## 123            10          14.3                         23
    ## 124             6          15.0                         14
    ## 125           153          19.2                        287
    ## 126            67          18.0                        133
    ## 127            86          20.3                        154
    ## 128            79          15.2                        164
    ## 129            29          12.0                         69
    ## 130            50          17.9                         95
    ## 131            74          27.0                        123
    ## 132            38          29.2                         64
    ## 133            36          25.0                         59
    ## 134            14          15.9                         26
    ## 135            35          21.6                         58
    ## 136            29          17.1                         59
    ## 137            15          18.3                         33
    ## 138            71          22.3                        124
    ## 139            36          23.1                         66
    ## 140            42          29.6                         65
    ## 141            21          28.4                         33
    ## 142            21          30.9                         32
    ## 143            65          18.7                        140
    ## 144            37          31.4                         59
    ## 145            13          29.5                         23
    ## 146            24          32.4                         36
    ## 147            12          11.7                         36
    ## 148            25          17.0                         59
    ## 149            40          19.9                         81
    ## 150            28          12.2                         81
    ## 151            16          12.6                         45
    ## 152            36          22.8                         54
    ## 153            30          22.9                         52
    ## 154            30          18.8                         44
    ## 155            13          17.3                         20
    ## 156            17          20.0                         24
    ## 157            24          17.4                         42
    ## 158            60          20.3                         96
    ## 159            11          17.5                         22
    ## 160            19          27.9                         30
    ## 161            83          24.0                        147
    ## 162            35          19.7                         69
    ## 163            13          21.3                         26
    ## 164            19          33.9                         29
    ## 165            22          18.8                         43
    ## 166            29          26.4                         49
    ## 167            48          28.6                         78
    ## 168            51          22.5                         92
    ## 169            32          27.4                         55
    ## 170            35          14.2                         88
    ## 171            21          17.4                         55
    ## 172            14          11.2                         33
    ## 173            19          13.2                         53
    ## 174            11          14.5                         33
    ## 175             8          11.8                         20
    ## 176            16          15.7                         35
    ## 177            10          22.2                         22
    ## 178             6          10.5                         13
    ## 179            17          16.0                         40
    ## 180            33          18.4                         67
    ## 181            30          15.1                         79
    ## 182            13          14.0                         39
    ## 183            57          16.8                        133
    ## 184            24          15.0                         66
    ## 185            27          19.3                         54
    ## 186            11          16.4                         27
    ## 187            16          21.9                         27
    ## 188           125          20.4                        221
    ## 189            62          20.5                        109
    ## 190            63          20.2                        112
    ## 191            47          13.2                         93
    ## 192            20          11.5                         46
    ## 193            27          14.8                         47
    ## 194            78          30.4                        128
    ## 195            42          32.8                         63
    ## 196            36          27.9                         65
    ## 197            33          32.4                         46
    ## 198            53          29.6                         79
    ## 199            63          28.4                         96
    ## 200            30          25.0                         50
    ## 201           105          26.4                        169
    ## 202            52          23.9                         90
    ## 203            42          24.0                         73
    ## 204            22          22.4                         40
    ## 205            20          26.0                         33
    ## 206            52          26.7                         94
    ## 207            17          25.4                         35
    ## 208            29          30.2                         51
    ## 209            23          23.2                         43
    ## 210            30          22.2                         62
    ## 211            10          32.3                         16
    ## 212            13          19.1                         27
    ## 213            22          39.3                         28
    ## 214            10          14.7                         22
    ## 215            12          17.1                         19
    ## 216            12          14.1                         24
    ## 217             5          12.8                         10
    ## 218             7          15.2                         14
    ## 219            22          15.9                         41
    ## 220            10          18.9                         17
    ## 221             5          17.2                         12
    ## 222            58          21.2                        101
    ## 223            18          21.2                         31
    ## 224            10          22.2                         17
    ## 225             8          20.0                         14
    ## 226            20          23.3                         34
    ## 227            30          22.7                         51
    ## 228            28          19.9                         50
    ## 229            40          22.1                         70
    ## 230            20          21.1                         36
    ## 231           491          22.2                        963
    ## 232           253          23.6                        491
    ## 233           238          20.9                        472
    ## 234           295          22.1                        578
    ## 235           154          24.1                        294
    ## 236           141          20.3                        284
    ## 237           196          22.3                        385
    ## 238            99          23.0                        197
    ## 239            97          21.7                        188
    ## 240            28          15.7                         60
    ## 241            30          19.1                         54
    ## 242            36          17.7                         67
    ## 243            15          14.3                         35
    ## 244            58          17.3                        114
    ## 245            22          16.7                         47
    ## 246            13          17.8                         25
    ## 247             9          15.3                         22
    ## 248            21          21.4                         32
    ## 249            13          16.5                         23
    ## 250            28          26.7                         47
    ## 251            12          23.5                         23
    ## 252            16          29.6                         24
    ## 253            37          23.9                         65
    ## 254            29          21.8                         47
    ## 255            38          20.8                         65
    ## 256            66          22.9                        112
    ## 257            25          24.0                         42
    ## 258           153          23.1                        272
    ## 259            75          22.5                        138
    ## 260            78          23.9                        134
    ## 261            83          20.8                        149
    ## 262            43          21.5                         75
    ## 263            40          20.1                         74
    ## 264            70          26.7                        123
    ## 265            32          23.9                         63
    ## 266            38          29.7                         60
    ## 267            98          23.3                        163
    ## 268            45          21.7                         81
    ## 269            22          31.0                         35
    ## 270            22          28.2                         31
    ## 271            23          17.6                         46
    ## 272            31          24.0                         51
    ## 273            53          24.9                         82
    ## 274            54          20.8                         97
    ## 275            44          29.5                         66
    ## 276           165          23.0                        281
    ## 277            74          21.1                        134
    ## 278            53          21.5                         90
    ## 279            91          24.7                        147
    ## 280            91          19.4                        164
    ## 281            38          17.1                         74
    ## 282            74          29.6                        117
    ## 283            36          28.1                         60
    ## 284            38          31.1                         57
    ## 285           157          21.7                        271
    ## 286            95          25.8                        165
    ## 287            41          28.3                         75
    ## 288            34          22.5                         51
    ## 289            54          24.7                         90
    ## 290            28          13.7                         55
    ## 291            62          17.5                        106
    ## 292            82          19.4                        145
    ## 293            75          25.3                        126
    ## 294            91          21.2                        167
    ## 295            37          17.9                         77
    ## 296            54          24.2                         90
    ## 297            53          20.7                         92
    ## 298            20          17.1                         40
    ## 299            33          23.7                         52
    ## 300            38          22.9                         75
    ## 301            17          19.8                         37
    ## 302            21          26.3                         38
    ## 303            44          19.1                         75
    ## 304            58          18.6                         99
    ## 305            92          20.2                        163
    ## 306            48          21.2                         88
    ## 307           123          18.7                        220
    ## 308            65          18.7                        121
    ## 309            31          15.7                         57
    ## 310            17          14.3                         33
    ## 311            14          17.7                         24
    ## 312           355          15.0                        722
    ## 313           189          16.8                        379
    ## 314            79          13.8                        177
    ## 315           110          20.0                        202
    ## 316            76          12.6                        167
    ## 317           155          13.2                        344
    ## 318           200          16.8                        378
    ## 319           166          13.4                        343
    ## 320            90          14.1                        176
    ## 321            76          15.7                        158
    ## 322            16          12.9                         35
    ## 323            34          15.5                         77
    ## 324            42          15.8                         81
    ## 325            38          13.6                         84
    ## 326            20          18.9                         32
    ## 327            22          14.1                         49
    ## 328            38          18.8                         74
    ## 329            18          18.8                         42
    ## 330            35          14.2                         77
    ## 331            37          13.1                         77
    ## 332            37          12.5                         76
    ## 333            19          13.7                         39
    ## 334            18          11.5                         37
    ## 335            72          13.6                        154
    ## 336            35          15.0                         78
    ## 337            16          15.1                         38
    ## 338            19          15.0                         40
    ## 339            46          16.5                         88
    ## 340            27          19.0                         49
    ## 341            15          23.4                         29
    ## 342            12          15.4                         20
    ## 343             6           9.1                         18
    ## 344            21          16.2                         47
    ## 345            25          16.9                         41
    ## 346            19          14.3                         39
    ## 347            13          19.4                         21
    ## 348            51          13.8                        101
    ## 349            53          17.3                         99
    ## 350            56          12.9                        115
    ## 351            30          12.8                         63
    ## 352           104          15.4                        200
    ## 353            21          16.0                         38
    ## 354            27          26.0                         47
    ## 355            26          13.1                         52
    ## 356            48          20.4                         85
    ## 357           108          11.1                        240
    ## 358           193          11.9                        461
    ## 359            89          10.9                        224
    ## 360           104          13.0                        237
    ## 361           212          11.9                        477
    ## 362           213          11.0                        484
    ## 363           406          11.4                        945
    ## 364           194          10.9                        468
    ## 365           105          11.0                        244
    ## 366           229          18.3                        407
    ## 367            98          17.0                        172
    ## 368           131          19.4                        235
    ## 369           111          14.4                        223
    ## 370            51          14.4                         97
    ## 371            60          14.5                        126
    ## 372           118          24.4                        184
    ## 373            47          21.1                         75
    ## 374            71          27.3                        109
    ## 375            44          17.7                         92
    ## 376            90          21.1                        159
    ## 377            87          17.2                        177
    ## 378            43          16.7                         85
    ## 379           178          21.1                        318
    ## 380            88          21.2                        159
    ## 381            91          27.7                        141
    ## 382            45          29.2                         74
    ## 383            46          26.4                         67
    ## 384           109          16.6                        214
    ## 385            60          23.4                         99
    ## 386            27          21.3                         45
    ## 387            33          25.6                         54
    ## 388            16           8.2                         48
    ## 389            43          13.1                         93
    ## 390            66          20.1                        121
    ## 391            49          12.5                        115
    ## 392            33          16.8                         67
    ## 393            59          11.6                        145
    ## 394            12           8.1                         39
    ## 395            23           8.9                         68
    ## 396            36          14.2                         77
    ## 397            36          12.1                         84
    ## 398            12          11.5                         32
    ## 399            24          16.1                         45
    ## 400            23          10.9                         61
    ## 401            11          10.3                         29
    ## 402            52          11.6                        129
    ## 403            78          15.9                        149
    ## 404            65          11.5                        146
    ## 405            28          10.2                         65
    ## 406            37          12.8                         81
    ## 407           130          13.8                        278
    ## 408            65          17.6                        132
    ## 409            24          14.1                         64
    ## 410            41          20.5                         68
    ## 411           101          12.5                        223
    ## 412            37          12.8                         73
    ## 413            22          14.2                         43
    ## 414            15          11.3                         30
    ## 415            26          10.7                         72
    ## 416            48          12.1                        115
    ## 417            53          13.0                        108
    ## 418            64          12.4                        150
    ## 419            38          13.8                         78
    ## 420           237          13.8                        495
    ## 421           131          15.0                        262
    ## 422           106          12.7                        233
    ## 423           141          12.9                        290
    ## 424            79          14.2                        154
    ## 425            62          11.5                        136
    ## 426            96          15.6                        205
    ## 427            52          16.4                        108
    ## 428            44          14.7                         97
    ## 429            49          15.8                        112
    ## 430            76          18.3                        133
    ## 431            90          17.1                        171
    ## 432            33          15.2                         77
    ## 433           125          17.2                        245
    ## 434            19          18.6                         39
    ## 435            57          18.4                         94
    ## 436            35          18.0                         74
    ## 437            16          17.4                         35
    ## 438            66          11.7                        162
    ## 439           136          16.6                        264
    ## 440            54          13.9                        130
    ## 441            82          19.0                        134
    ## 442           114          10.6                        296
    ## 443           250          13.2                        560
    ## 444           102          11.3                        264
    ## 445           148          14.8                        296
    ## 446            48           9.4                        134
    ## 447            59          11.7                        173
    ## 448            21           8.9                         76
    ## 449            38          14.1                         97
    ## 450            12           4.3                         93
    ## 451             5           3.8                         39
    ## 452             7           4.8                         54
    ## 453            47          21.3                         80
    ## 454            16          15.1                         37
    ## 455            31          27.0                         43
    ## 456            73           9.5                        183
    ## 457            98          12.6                        185
    ## 458            99          10.7                        213
    ## 459            36           8.1                        108
    ## 460           171          11.1                        368
    ## 461            63          13.0                        105
    ## 462            72          11.6                        155
    ## 463            37          11.5                         75
    ## 464            35          11.8                         80
    ## 465           160          17.9                        295
    ## 466           172          21.6                        328
    ## 467            82          20.0                        175
    ## 468            90          23.4                        153
    ## 469           472          17.9                        919
    ## 470           222          16.3                        471
    ## 471           250          19.5                        448
    ## 472           300          16.3                        591
    ## 473           140          14.8                        296
    ## 474            88          28.8                        149
    ## 475            45          30.4                         76
    ## 476            43          27.2                         73
    ## 477            57          27.8                        102
    ## 478            30          30.9                         54
    ## 479            27          25.0                         48
    ## 480            31          30.7                         47
    ## 481            15          29.4                         22
    ## 482            16          32.0                         25
    ## 483           170          29.2                        274
    ## 484            80          29.1                        130
    ## 485            90          29.2                        144
    ## 486           103          26.5                        169
    ## 487            52          27.8                         82
    ## 488            51          25.2                         87
    ## 489            67          35.6                        105
    ## 490            28          31.8                         48
    ## 491            39          39.0                         57
    ## 492            57          15.9                        123
    ## 493            23          13.6                         64
    ## 494            34          18.0                         59
    ## 495            30          12.9                         72
    ## 496            10           9.5                         34
    ## 497            20          15.7                         38
    ## 498            27          21.6                         51
    ## 499            13          20.3                         30
    ## 500            14          23.0                         21
    ## 501           184          21.8                        328
    ## 502            89          22.6                        162
    ## 503            95          21.0                        166
    ## 504           110          21.1                        185
    ## 505            55          21.8                         91
    ## 506            55          20.4                         94
    ## 507            74          22.8                        143
    ## 508            34          24.1                         71
    ## 509            40          21.9                         72
    ## 510            93          25.3                        165
    ## 511            42          24.1                         78
    ## 512            51          26.4                         87
    ## 513            54          22.6                        104
    ## 514            22          20.4                         47
    ## 515            32          24.4                         57
    ## 516            39          30.5                         61
    ## 517            20          30.3                         31
    ## 518            19          30.6                         30
    ## 519            96          20.5                        195
    ## 520            44          19.6                         88
    ## 521            52          21.3                        107
    ## 522            50          18.3                        110
    ## 523            19          14.7                         43
    ## 524            31          21.5                         67
    ## 525            46          24.1                         85
    ## 526            25          26.0                         45
    ## 527            21          22.1                         40
    ## 528           130          19.6                        246
    ## 529            57          17.5                        111
    ## 530            73          21.7                        135
    ## 531            73          17.4                        151
    ## 532            34          15.6                         68
    ## 533            39          19.4                         83
    ## 534            57          24.1                         95
    ## 535            23          21.5                         43
    ## 536            34          26.2                         52
    ## 537           270          17.5                        520
    ## 538           122          16.3                        246
    ## 539           148          18.6                        274
    ## 540           182          16.8                        339
    ## 541            84          16.3                        157
    ## 542            98          17.3                        182
    ## 543            88          19.1                        181
    ## 544            38          16.4                         89
    ## 545            50          21.9                         92
    ## 546            61          22.3                        108
    ## 547            30          20.5                         59
    ## 548            31          24.2                         49
    ## 549            31          20.4                         52
    ## 550            14          18.9                         26
    ## 551            17          21.8                         26
    ## 552            30          24.6                         56
    ## 553            16          22.2                         33
    ## 554            14          28.0                         23
    ## 555           208          20.3                        368
    ## 556            86          19.5                        154
    ## 557           122          21.0                        214
    ## 558           104          16.8                        183
    ## 559            45          16.3                         77
    ## 560            59          17.2                        106
    ## 561           104          25.7                        185
    ## 562            41          24.7                         77
    ## 563            63          26.4                        108
    ## 564            45          16.4                         86
    ## 565            21          16.7                         43
    ## 566            24          16.1                         43
    ## 567            24          16.2                         42
    ## 568            12          19.4                         21
    ## 569            12          14.0                         21
    ## 570            21          17.2                         44
    ## 571             9          15.3                         22
    ## 572            12          19.0                         22
    ## 573            99          23.1                        187
    ## 574            47          21.4                         96
    ## 575            52          24.9                         91
    ## 576            59          20.5                        115
    ## 577            29          19.7                         57
    ## 578            30          21.3                         58
    ## 579            40          28.4                         72
    ## 580            18          24.7                         39
    ## 581            22          32.4                         33
    ## 582           135          21.6                        233
    ## 583            59          20.8                        105
    ## 584            76          22.3                        128
    ## 585            66          19.4                        108
    ## 586            25          16.4                         46
    ## 587            41          21.8                         62
    ## 588            69          24.3                        125
    ## 589            34          26.0                         59
    ## 590            35          22.9                         66
    ## 591           114          10.0                        287
    ## 592            55           9.7                        148
    ## 593            59          10.3                        139
    ## 594            62           9.0                        159
    ## 595            28           8.6                         71
    ## 596            34           9.4                         88
    ## 597            52          11.6                        128
    ## 598            27          11.1                         77
    ## 599            25          12.3                         51
    ## 600           272          20.7                        505
    ## 601           119          18.6                        239
    ## 602           153          22.7                        266
    ## 603           154          18.7                        295
    ## 604            68          16.9                        140
    ## 605            86          20.4                        155
    ## 606           118          24.2                        210
    ## 607            51          21.5                         99
    ## 608            67          26.7                        111
    ## 609           177          13.7                        414
    ## 610            80          13.0                        191
    ## 611            97          14.3                        223
    ## 612           114          14.7                        257
    ## 613            51          14.4                        117
    ## 614            63          14.8                        140
    ## 615            63          12.3                        157
    ## 616            29          11.1                         74
    ## 617            34          13.5                         83
    ## 618           236          15.5                        503
    ## 619           116          15.6                        251
    ## 620           120          15.4                        252
    ## 621           114          12.7                        277
    ## 622            58          13.6                        143
    ## 623            56          11.9                        134
    ## 624           122          19.6                        226
    ## 625            58          18.6                        108
    ## 626            64          20.6                        118
    ## 627            83          20.0                        139
    ## 628            39          19.7                         73
    ## 629            44          20.4                         66
    ## 630            45          17.6                         76
    ## 631            21          17.4                         40
    ## 632            24          17.8                         36
    ## 633            38          26.2                         63
    ## 634            18          25.0                         33
    ## 635            20          27.4                         30
    ## 636           240          16.2                        524
    ## 637           106          15.2                        234
    ## 638           134          17.2                        290
    ## 639           131          15.2                        298
    ## 640            56          13.9                        120
    ## 641            75          16.4                        178
    ## 642           109          17.7                        226
    ## 643            50          16.9                        114
    ## 644            59          18.3                        112
    ## 645           327          17.8                        612
    ## 646           147          16.5                        278
    ## 647           180          19.0                        334
    ## 648           173          15.1                        336
    ## 649            76          13.7                        158
    ## 650            97          16.4                        178
    ## 651           154          22.3                        276
    ## 652            71          21.2                        120
    ## 653            83          23.4                        156
    ## 654           169          16.8                        332
    ## 655            70          14.3                        150
    ## 656            99          19.3                        182
    ## 657            95          15.2                        186
    ## 658            38          12.5                         84
    ## 659            57          17.8                        102
    ## 660            74          19.7                        146
    ## 661            32          17.2                         66
    ## 662            42          22.2                         80
    ## 663           444          13.7                        943
    ## 664           203          12.5                        470
    ## 665           241          14.8                        473
    ## 666           235          12.4                        496
    ## 667           107          11.3                        250
    ## 668           128          13.5                        246
    ## 669           209          15.4                        447
    ## 670            96          14.3                        220
    ## 671           113          16.5                        227
    ## 672           271          24.5                        467
    ## 673           130          24.3                        230
    ## 674           141          24.7                        237
    ## 675           177          22.6                        323
    ## 676            81          20.6                        158
    ## 677            96          24.6                        165
    ## 678            94          29.5                        144
    ## 679            49          35.5                         72
    ## 680            45          24.9                         72
    ## 681           622          23.5                       1161
    ## 682           287          21.7                        577
    ## 683           335          25.3                        584
    ## 684           413          24.9                        727
    ## 685           198          23.1                        375
    ## 686           215          26.9                        352
    ## 687           209          21.1                        434
    ## 688            89          19.3                        202
    ## 689           120          22.7                        232
    ## 690           283          13.3                        655
    ## 691           127          11.8                        315
    ## 692           156          15.0                        340
    ## 693           131          11.5                        302
    ## 694            64          11.0                        145
    ## 695            67          12.1                        157
    ## 696           152          15.4                        353
    ## 697            63          12.8                        170
    ## 698            89          18.2                        183
    ## 699           259          11.2                        598
    ## 700           106           9.2                        283
    ## 701           153          13.2                        315
    ## 702           138           9.7                        322
    ## 703            61           8.4                        157
    ## 704            77          11.0                        165
    ## 705           121          13.6                        276
    ## 706            45          10.5                        126
    ## 707            76          16.5                        150
    ## 708           103          15.1                        214
    ## 709            45          14.4                         96
    ## 710            58          15.7                        118
    ## 711            36          10.6                         96
    ## 712            18          11.3                         45
    ## 713            18          10.0                         51
    ## 714            67          19.8                        118
    ## 715            27          18.2                         51
    ## 716            40          21.1                         67
    ## 717           547          22.2                       1074
    ## 718           220          19.1                        489
    ## 719           327          24.9                        585
    ## 720           401          23.8                        753
    ## 721           157          20.4                        328
    ## 722           244          26.6                        425
    ## 723           146          18.7                        321
    ## 724            63          16.5                        161
    ## 725            83          20.9                        160
    ## 726            65          13.5                        115
    ## 727            27          11.9                         53
    ## 728            38          15.0                         62
    ## 729            65          13.5                        115
    ## 730            27          11.9                         53
    ## 731            38          15.0                         62
    ## 732           178          21.5                        329
    ## 733            77          18.2                        151
    ## 734           101          25.1                        178
    ## 735           118          20.0                        218
    ## 736            49          16.2                        101
    ## 737            69          23.9                        117
    ## 738            60          25.4                        111
    ## 739            28          23.0                         50
    ## 740            32          28.1                         61
    ## 741           130           9.9                        326
    ## 742            52           8.3                        143
    ## 743            78          11.3                        183
    ## 744            76           9.9                        196
    ## 745            33           9.0                         88
    ## 746            43          10.6                        108
    ## 747            54          10.0                        130
    ## 748            19           7.4                         55
    ## 749            35          12.3                         75
    ## 750           113          15.5                        204
    ## 751            35          15.0                         67
    ## 752            78          15.8                        137
    ## 753           113          15.5                        204
    ## 754            35          15.0                         67
    ## 755            78          15.8                        137
    ## 756           146          23.6                        255
    ## 757            80          25.2                        135
    ## 758            66          21.9                        120
    ## 759            80          24.2                        139
    ## 760            42          24.0                         69
    ## 761            38          24.4                         70
    ## 762            66          23.1                        116
    ## 763            38          27.0                         66
    ## 764            28          19.3                         50
    ## 765            48          12.7                         96
    ## 766            18          10.9                         36
    ## 767            30          14.2                         60
    ## 768            48          12.7                         96
    ## 769            18          10.9                         36
    ## 770            30          14.2                         60
    ## 771            63          15.4                        132
    ## 772            33          16.6                         70
    ## 773            30          14.4                         62
    ## 774            63          15.4                        132
    ## 775            33          16.6                         70
    ## 776            30          14.4                         62
    ## 777            76           8.8                        198
    ## 778            27           6.4                         88
    ## 779            49          11.1                        110
    ## 780            46           9.5                        103
    ## 781            18           7.1                         43
    ## 782            28          12.0                         60
    ## 783            30           7.9                         95
    ## 784             9           5.3                         45
    ## 785            21          10.1                         50
    ## 786            84          12.3                        177
    ## 787            32           9.5                         78
    ## 788            52          15.2                         99
    ## 789            84          12.3                        177
    ## 790            32           9.5                         78
    ## 791            52          15.2                         99
    ## 792           244           9.8                        554
    ## 793            91           7.6                        235
    ## 794           153          11.9                        319
    ## 795           157          10.3                        326
    ## 796            61           8.3                        143
    ## 797            96          12.2                        183
    ## 798            87           9.1                        228
    ## 799            30           6.5                         92
    ## 800            57          11.5                        136
    ## 801            83          10.0                        191
    ## 802            34           8.6                         93
    ## 803            49          11.3                         98
    ## 804            63          12.2                        128
    ## 805            27          11.4                         61
    ## 806            36          12.9                         67
    ## 807            20           6.5                         63
    ## 808             7           4.4                         32
    ## 809            13           8.6                         31
    ## 810           149           9.1                        429
    ## 811            58           7.6                        194
    ## 812            91          10.3                        235
    ## 813            67           7.1                        226
    ## 814            28           6.7                         94
    ## 815            39           7.5                        132
    ## 816            82          11.6                        203
    ## 817            30           8.8                        100
    ## 818            52          14.3                        103
    ## 819            36           9.7                         93
    ## 820            13           6.7                         46
    ## 821            23          12.8                         47
    ## 822            15           7.8                         48
    ## 823            15          15.8                         29
    ## 824            17           9.7                         41
    ## 825             9           9.9                         23
    ## 826             8           9.5                         18
    ## 827            67           6.8                        199
    ## 828            25           5.5                         78
    ## 829            42           7.8                        121
    ## 830            19           3.8                         72
    ## 831             8           3.2                         33
    ## 832            11           4.3                         39
    ## 833            48           9.9                        127
    ## 834            17           8.2                         45
    ## 835            31          11.1                         82
    ## 836           294          18.2                        566
    ## 837           117          15.3                        247
    ## 838           177          20.9                        319
    ## 839           166          16.5                        328
    ## 840            64          13.5                        142
    ## 841           102          19.1                        186
    ## 842           128          21.2                        238
    ## 843            53          18.0                        105
    ## 844            75          24.1                        133
    ## 845           229          10.9                        538
    ## 846            97           9.5                        242
    ## 847           132          12.2                        296
    ## 848            95           8.1                        253
    ## 849            42           7.7                        110
    ## 850            53           8.4                        143
    ## 851           134          14.5                        285
    ## 852            55          11.6                        132
    ## 853            79          17.7                        153
    ## 854            97          12.9                        217
    ## 855            40          11.4                         90
    ## 856            57          14.2                        127
    ## 857            64          13.4                        124
    ## 858            28          12.4                         54
    ## 859            36          14.2                         70
    ## 860            33          12.2                         93
    ## 861            12           9.7                         36
    ## 862            21          14.3                         57
    ## 863           284          18.1                        511
    ## 864           117          15.2                        245
    ## 865           167          20.8                        266
    ## 866           177          17.6                        308
    ## 867            82          15.8                        155
    ## 868            95          19.5                        153
    ## 869           107          19.1                        203
    ## 870            35          14.0                         90
    ## 871            72          23.2                        113
    ## 872           103          13.4                        234
    ## 873            44          11.4                        113
    ## 874            59          15.4                        121
    ## 875            50          12.5                        108
    ## 876            23          11.3                         49
    ## 877            27          13.7                         59
    ## 878            53          14.5                        126
    ## 879            21          11.7                         64
    ## 880            32          17.3                         62
    ## 881            58          18.9                        110
    ## 882            20          13.4                         58
    ## 883            38          24.1                         52
    ## 884            27          19.7                         45
    ## 885            13          17.3                         31
    ## 886            31          19.3                         61
    ## 887             7           9.5                         27
    ## 888            24          27.6                         34
    ## 889            33          19.9                         55
    ## 890            16          21.3                         29
    ## 891            17          18.7                         26
    ## 892            23          21.5                         36
    ## 893            13          27.7                         20
    ## 894            10          16.7                         16
    ## 895             7          13.2                         13
    ## 896            84          22.5                        148
    ## 897            50          25.0                         84
    ## 898            34          19.7                         64
    ## 899            47          19.3                         92
    ## 900            28          21.4                         52
    ## 901            19          16.8                         40
    ## 902            37          28.7                         56
    ## 903            22          31.9                         32
    ## 904            15          25.0                         24
    ## 905           464          22.8                        770
    ## 906           238          23.6                        394
    ## 907           226          22.0                        376
    ## 908           314          20.5                        555
    ## 909           159          20.9                        282
    ## 910           155          20.2                        273
    ## 911           150          29.5                        215
    ## 912            79          31.9                        112
    ## 913            71          27.3                        103
    ## 914           186          17.8                        325
    ## 915            69          14.7                        129
    ## 916           117          20.3                        196
    ## 917            98          17.1                        177
    ## 918            44          16.4                         81
    ## 919            54          17.8                         96
    ## 920            88          18.8                        148
    ## 921            25          12.8                         48
    ## 922            63          23.2                        100
    ## 923           128          12.5                        296
    ## 924            65          12.8                        149
    ## 925            63          12.2                        147
    ## 926            84          12.1                        202
    ## 927            42          12.3                        101
    ## 928            42          11.9                        101
    ## 929            44          13.4                         94
    ## 930            23          13.9                         48
    ## 931            21          12.9                         46
    ## 932           113          12.6                        250
    ## 933            49          11.2                        118
    ## 934            64          13.8                        132
    ## 935            55          11.4                        128
    ## 936            23           9.9                         56
    ## 937            32          12.7                         72
    ## 938            58          14.0                        122
    ## 939            26          12.8                         62
    ## 940            32          15.1                         60
    ## 941            42          19.7                         78
    ## 942            20          19.8                         38
    ## 943            22          19.6                         40
    ## 944            23          17.7                         45
    ## 945            14          23.0                         25
    ## 946             9          13.0                         20
    ## 947            19          22.9                         33
    ## 948             6          15.0                         13
    ## 949            13          30.2                         20
    ## 950           111          20.9                        202
    ## 951            67          23.9                        118
    ## 952            44          17.5                         84
    ## 953            58          18.5                        109
    ## 954            37          23.0                         60
    ## 955            21          13.8                         49
    ## 956            53          24.3                         93
    ## 957            30          25.2                         58
    ## 958            23          23.2                         35
    ## 959           316          17.0                        586
    ## 960           142          16.0                        292
    ## 961           174          17.9                        294
    ## 962           178          15.9                        335
    ## 963            77          14.6                        160
    ## 964           101          17.0                        175
    ## 965           138          18.6                        251
    ## 966            65          17.9                        132
    ## 967            73          19.3                        119
    ## 968           119           9.7                        292
    ## 969            50           8.3                        139
    ## 970            69          11.0                        153
    ## 971            58           8.0                        160
    ## 972            27           7.7                         80
    ## 973            31           8.3                         80
    ## 974            61          12.1                        132
    ## 975            23           9.2                         59
    ## 976            38          15.0                         73
    ## 977            66          22.7                        118
    ## 978            27          19.6                         61
    ## 979            39          25.5                         57
    ## 980            39          22.7                         67
    ## 981            14          17.7                         31
    ## 982            25          26.9                         36
    ## 983            27          22.9                         51
    ## 984            13          22.0                         30
    ## 985            14          23.7                         21
    ## 986          1412          22.9                       2433
    ## 987           653          22.3                       1188
    ## 988           759          23.4                       1245
    ## 989           877          20.7                       1550
    ## 990           397          19.7                        732
    ## 991           480          21.5                        818
    ## 992           535          27.7                        883
    ## 993           256          28.1                        456
    ## 994           279          27.4                        427
    ## 995            47          17.0                         93
    ## 996            22          16.4                         53
    ## 997            25          17.6                         40
    ## 998            20          10.8                         48
    ## 999             9          10.0                         28
    ## 1000           11          11.5                         20
    ## 1001           27          30.0                         45
    ## 1002           13          29.5                         25
    ## 1003           14          30.4                         20
    ## 1004          150          20.5                        293
    ## 1005           83          21.7                        160
    ## 1006           67          19.3                        133
    ## 1007           78          17.9                        159
    ## 1008           46          19.7                         89
    ## 1009           32          15.9                         70
    ## 1010           72          24.9                        134
    ## 1011           37          25.2                         71
    ## 1012           35          24.6                         63
    ## 1013           83          22.0                        146
    ## 1014           50          27.0                         82
    ## 1015           33          17.1                         64
    ## 1016           41          18.6                         80
    ## 1017           23          20.9                         39
    ## 1018           18          16.4                         41
    ## 1019           42          26.6                         66
    ## 1020           27          36.0                         43
    ## 1021           15          18.1                         23
    ## 1022           56          20.4                        116
    ## 1023           25          19.8                         56
    ## 1024           31          20.8                         60
    ## 1025           29          17.9                         65
    ## 1026           17          21.3                         36
    ## 1027           12          14.6                         29
    ## 1028           27          23.9                         51
    ## 1029            8          17.4                         20
    ## 1030           19          28.4                         31
    ## 1031          154           8.6                        415
    ## 1032           56           6.1                        188
    ## 1033           98          11.2                        227
    ## 1034           70           6.2                        255
    ## 1035           27           4.6                        129
    ## 1036           43           7.8                        126
    ## 1037           84          12.8                        160
    ## 1038           29           8.7                         59
    ## 1039           55          17.0                        101
    ## 1040          202          16.3                        401
    ## 1041           87          14.5                        190
    ## 1042          115          17.9                        211
    ## 1043           91          14.2                        180
    ## 1044           42          13.2                         91
    ## 1045           49          15.3                         89
    ## 1046          111          18.6                        221
    ## 1047           45          16.3                         99
    ## 1048           66          20.6                        122
    ## 1049          121          13.9                        272
    ## 1050           46          11.4                        119
    ## 1051           75          16.2                        153
    ## 1052           55          11.5                        127
    ## 1053           24          10.9                         59
    ## 1054           31          12.1                         68
    ## 1055           66          17.2                        145
    ## 1056           22          12.2                         60
    ## 1057           44          21.7                         85
    ## 1058          231          11.8                        581
    ## 1059           85           8.9                        265
    ## 1060          146          14.6                        316
    ## 1061          123          11.0                        299
    ## 1062           44           8.0                        128
    ## 1063           79          13.9                        171
    ## 1064          108          13.0                        282
    ## 1065           41          10.2                        137
    ## 1066           67          15.7                        145
    ## 1067         1203          27.9                       1961
    ## 1068          532          25.7                        920
    ## 1069          671          30.0                       1041
    ## 1070          766          27.2                       1263
    ## 1071          345          25.0                        583
    ## 1072          421          29.3                        680
    ## 1073          437          29.4                        698
    ## 1074          187          27.1                        337
    ## 1075          250          31.3                        361
    ## 1076          188          10.8                        442
    ## 1077           83           9.7                        212
    ## 1078          105          11.8                        230
    ## 1079          108           9.2                        270
    ## 1080           48           8.2                        134
    ## 1081           60          10.1                        136
    ## 1082           80          14.0                        172
    ## 1083           35          12.7                         78
    ## 1084           45          15.3                         94
    ## 1085           52          20.7                        101
    ## 1086           26          21.5                         50
    ## 1087           26          20.0                         51
    ## 1088           31          20.8                         62
    ## 1089           16          21.1                         30
    ## 1090           15          20.5                         32
    ## 1091           21          21.6                         39
    ## 1092           10          22.2                         20
    ## 1093           11          21.2                         19
    ## 1094          112          27.9                        170
    ## 1095           57          28.9                         86
    ## 1096           55          26.8                         84
    ## 1097           56          22.5                         89
    ## 1098           31          25.6                         47
    ## 1099           25          19.5                         42
    ## 1100           56          37.1                         81
    ## 1101           26          34.2                         39
    ## 1102           30          40.0                         42
    ## 1103          121          27.6                        208
    ## 1104           67          28.3                        116
    ## 1105           54          26.7                         92
    ## 1106           66          23.4                        124
    ## 1107           36          24.3                         65
    ## 1108           30          22.4                         59
    ## 1109           55          35.0                         84
    ## 1110           31          34.8                         51
    ## 1111           24          35.3                         33
    ## 1112          236          21.2                        426
    ## 1113          105          20.2                        192
    ## 1114          131          22.1                        234
    ## 1115          141          19.0                        267
    ## 1116           62          17.8                        125
    ## 1117           79          19.9                        142
    ## 1118           95          26.0                        159
    ## 1119           43          25.3                         67
    ## 1120           52          26.7                         92
    ## 1121          117          25.3                        214
    ## 1122           57          26.8                        102
    ## 1123           60          24.1                        112
    ## 1124           59          22.9                        116
    ## 1125           27          23.5                         54
    ## 1126           32          22.4                         62
    ## 1127           58          28.4                         98
    ## 1128           30          30.6                         48
    ## 1129           28          26.4                         50
    ## 1130           75          26.0                        128
    ## 1131           32          24.8                         57
    ## 1132           43          27.0                         71
    ## 1133           45          23.9                         76
    ## 1134           16          19.8                         33
    ## 1135           29          27.1                         43
    ## 1136           30          30.0                         52
    ## 1137           16          33.3                         24
    ## 1138           14          26.9                         28
    ## 1139          137          20.8                        230
    ## 1140           74          22.7                        127
    ## 1141           63          18.9                        103
    ## 1142           72          16.9                        128
    ## 1143           41          20.6                         70
    ## 1144           31          13.7                         58
    ## 1145           65          28.0                        102
    ## 1146           33          26.2                         57
    ## 1147           32          30.2                         45
    ## 1148          114          17.5                        198
    ## 1149           54          17.6                         97
    ## 1150           60          17.4                        101
    ## 1151           67          17.4                        112
    ## 1152           36          20.1                         60
    ## 1153           31          15.1                         52
    ## 1154           47          17.9                         86
    ## 1155           18          14.2                         37
    ## 1156           29          21.3                         49
    ## 1157           35          11.0                         80
    ## 1158           14           8.8                         40
    ## 1159           21          13.3                         40
    ## 1160           16           9.8                         38
    ## 1161            8          10.5                         20
    ## 1162            8           9.1                         18
    ## 1163           19          13.3                         42
    ## 1164            6           7.7                         20
    ## 1165           13          20.0                         22
    ## 1166          253          20.6                        475
    ## 1167          103          17.8                        209
    ## 1168          150          23.1                        266
    ## 1169          130          18.3                        252
    ## 1170           53          15.9                        111
    ## 1171           77          20.4                        141
    ## 1172          123          23.9                        223
    ## 1173           50          20.4                         98
    ## 1174           73          27.0                        125
    ## 1175           14           5.4                         39
    ## 1176            7           5.6                         23
    ## 1177            7           5.3                         16
    ## 1178            5           4.4                         11
    ## 1179            5           8.5                         11
    ## 1180            5           3.9                         21
    ## 1181            5           7.2                         11
    ## 1182          215          19.5                        410
    ## 1183           95          18.1                        193
    ## 1184          120          20.7                        217
    ## 1185          117          18.5                        215
    ## 1186           51          16.9                         93
    ## 1187           66          20.1                        122
    ## 1188           98          20.9                        195
    ## 1189           44          19.8                        100
    ## 1190           54          21.8                         95
    ## 1191          212          15.3                        499
    ## 1192           98          14.2                        257
    ## 1193          114          16.3                        242
    ## 1194          118          14.9                        269
    ## 1195           56          14.0                        141
    ## 1196           62          15.9                        128
    ## 1197           94          15.7                        230
    ## 1198           42          14.5                        116
    ## 1199           52          16.9                        114
    ## 1200          112          12.8                        261
    ## 1201           49          11.7                        131
    ## 1202           63          13.8                        130
    ## 1203           60          12.3                        143
    ## 1204           26          11.1                         70
    ## 1205           34          13.4                         73
    ## 1206           52          13.5                        118
    ## 1207           23          12.5                         61
    ## 1208           29          14.4                         57
    ## 1209          211          18.3                        403
    ## 1210          118          22.2                        217
    ## 1211           93          15.0                        186
    ## 1212          126          18.6                        252
    ## 1213           74          24.2                        139
    ## 1214           52          13.9                        113
    ## 1215           85          17.9                        151
    ## 1216           44          19.5                         78
    ## 1217           41          16.5                         73
    ## 1218          112          16.7                        206
    ## 1219           57          16.7                        109
    ## 1220           55          16.8                         97
    ## 1221           75          18.7                        124
    ## 1222           37          18.0                         63
    ## 1223           38          19.5                         61
    ## 1224           37          14.2                         82
    ## 1225           20          15.2                         46
    ## 1226           17          13.2                         36
    ## 1227          628          19.9                       1173
    ## 1228          282          18.6                        549
    ## 1229          346          21.0                        624
    ## 1230          378          20.1                        679
    ## 1231          167          18.6                        313
    ## 1232          211          21.6                        366
    ## 1233          250          19.5                        494
    ## 1234          115          18.7                        236
    ## 1235          135          20.2                        258
    ## 1236          145          13.9                        312
    ## 1237           65          12.8                        148
    ## 1238           80          15.0                        164
    ## 1239           64          11.8                        142
    ## 1240           37          14.0                         77
    ## 1241           27           9.7                         65
    ## 1242           81          16.3                        170
    ## 1243           28          11.5                         71
    ## 1244           53          20.8                         99
    ## 1245           69           8.7                        206
    ## 1246           31           7.8                         87
    ## 1247           38           9.5                        119
    ## 1248           35           7.9                        109
    ## 1249           18           8.8                         46
    ## 1250           17           7.2                         63
    ## 1251           34           9.6                         97
    ## 1252           13           6.8                         41
    ## 1253           21          12.7                         56
    ## 1254           26          25.2                         41
    ## 1255           13          22.0                         21
    ## 1256           13          29.5                         20
    ## 1257           14          22.6                         19
    ## 1258            8          22.2                         13
    ## 1259          116          31.7                        180
    ## 1260           51          29.0                         88
    ## 1261           65          34.2                         92
    ## 1262           57          26.5                         96
    ## 1263           25          23.6                         46
    ## 1264           32          29.4                         50
    ## 1265           59          39.1                         84
    ## 1266           26          37.1                         42
    ## 1267           33          40.7                         42
    ## 1268           72          17.2                        139
    ## 1269           34          16.3                         73
    ## 1270           38          18.2                         66
    ## 1271           36          14.5                         76
    ## 1272           18          14.9                         38
    ## 1273           18          14.2                         38
    ## 1274           36          22.2                         63
    ## 1275           16          19.0                         35
    ## 1276           20          25.6                         28
    ## 1277          177          15.1                        389
    ## 1278           82          14.6                        202
    ## 1279           95          15.4                        187
    ## 1280          102          13.4                        241
    ## 1281           42          11.9                        116
    ## 1282           60          14.7                        125
    ## 1283           75          18.2                        148
    ## 1284           40          19.4                         86
    ## 1285           35          16.9                         62
    ## 1286          256          11.3                        573
    ## 1287          116          10.2                        283
    ## 1288          140          12.4                        290
    ## 1289          133          10.1                        307
    ## 1290           56           8.6                        141
    ## 1291           77          11.6                        166
    ## 1292          123          13.0                        266
    ## 1293           60          12.5                        142
    ## 1294           63          13.5                        124
    ## 1295          110           8.6                        300
    ## 1296           38           6.1                        132
    ## 1297           72          11.0                        168
    ## 1298           66           8.8                        174
    ## 1299           22           6.1                         70
    ## 1300           44          11.2                        104
    ## 1301           44           8.3                        126
    ## 1302           16           6.0                         62
    ## 1303           28          10.7                         64
    ## 1304          224          23.5                        374
    ## 1305          109          24.5                        182
    ## 1306          115          22.6                        192
    ## 1307          108          19.7                        182
    ## 1308           55          22.0                         91
    ## 1309           53          17.7                         91
    ## 1310          116          28.7                        192
    ## 1311           54          27.7                         91
    ## 1312           62          29.7                        101
    ## 1313           22          10.0                         52
    ## 1314           12          10.4                         28
    ## 1315           10           9.6                         24
    ## 1316           16           9.6                         41
    ## 1317            8           9.3                         20
    ## 1318            8          10.0                         21
    ## 1319           81          20.4                        166
    ## 1320           32          17.5                         76
    ## 1321           49          22.8                         90
    ## 1322           35          15.5                         89
    ## 1323           14          14.3                         46
    ## 1324           21          16.4                         43
    ## 1325           46          26.7                         77
    ## 1326           18          21.2                         30
    ## 1327           28          32.2                         47
    ## 1328          141          24.0                        242
    ## 1329           60          21.6                        113
    ## 1330           81          26.2                        129
    ## 1331           76          20.4                        145
    ## 1332           33          19.1                         69
    ## 1333           43          21.5                         76
    ## 1334           65          30.4                         97
    ## 1335           27          25.7                         44
    ## 1336           38          34.9                         53
    ## 1337          124          21.8                        215
    ## 1338           55          20.8                         99
    ## 1339           69          22.5                        116
    ## 1340           66          20.3                        109
    ## 1341           34          20.7                         57
    ## 1342           32          19.9                         52
    ## 1343           58          24.2                        106
    ## 1344           21          21.4                         42
    ## 1345           37          26.1                         64
    ## 1346           25          16.6                         46
    ## 1347           11          14.3                         21
    ## 1348           14          18.9                         25
    ## 1349           15          17.4                         23
    ## 1350            7          16.7                         15
    ## 1351            7          13.5                         13
    ## 1352           44          31.7                         62
    ## 1353           27          38.0                         37
    ## 1354           17          25.0                         25
    ## 1355           25          28.7                         31
    ## 1356           14          31.1                         20
    ## 1357           19          46.3                         24
    ## 1358            6          26.1                         11
    ## 1359           20          18.0                         41
    ## 1360            7          13.7                         16
    ## 1361           13          21.7                         25
    ## 1362            6          14.0                         14
    ## 1363            6          19.4                         14
    ## 1364           12          20.0                         19
    ## 1365            5          14.3                         12
    ## 1366           63          11.5                        156
    ## 1367           33          12.7                         77
    ## 1368           30          10.5                         79
    ## 1369           33          11.0                         68
    ## 1370           16          11.5                         28
    ## 1371           17          10.5                         40
    ## 1372           30          12.4                         88
    ## 1373           17          14.2                         49
    ## 1374           13          10.7                         39
    ## 1375          307          22.1                        526
    ## 1376          153          21.8                        271
    ## 1377          154          22.4                        255
    ## 1378          174          20.4                        291
    ## 1379           86          20.1                        148
    ## 1380           88          20.8                        143
    ## 1381          133          24.8                        235
    ## 1382           67          24.4                        123
    ## 1383           66          25.2                        112
    ## 1384          278          24.6                        462
    ## 1385          136          25.3                        228
    ## 1386          142          24.1                        234
    ## 1387          186          24.7                        305
    ## 1388           91          25.6                        148
    ## 1389           95          23.9                        157
    ## 1390           92          24.5                        157
    ## 1391           45          24.7                         80
    ## 1392           47          24.4                         77
    ## 1393          229          20.6                        400
    ## 1394          114          20.9                        196
    ## 1395          115          20.2                        204
    ## 1396          147          20.4                        248
    ## 1397           75          20.5                        127
    ## 1398           72          20.2                        121
    ## 1399           82          20.9                        152
    ## 1400           39          21.7                         69
    ## 1401           43          20.3                         83
    ## 1402          238          22.7                        399
    ## 1403          120          21.9                        205
    ## 1404          118          23.7                        194
    ## 1405          131          20.4                        223
    ## 1406           76          22.0                        125
    ## 1407           55          18.6                         98
    ## 1408          107          26.7                        176
    ## 1409           44          21.9                         80
    ## 1410           63          31.5                         96
    ## 1411          765          18.2                       1539
    ## 1412          337          16.4                        704
    ## 1413          428          19.8                        835
    ## 1414          455          17.6                        894
    ## 1415          211          16.9                        423
    ## 1416          244          18.3                        471
    ## 1417          310          19.0                        645
    ## 1418          126          15.7                        281
    ## 1419          184          22.2                        364
    ## 1420           41          11.2                         91
    ## 1421           16           8.9                         40
    ## 1422           25          13.3                         51
    ## 1423           19           9.3                         42
    ## 1424            8           7.4                         20
    ## 1425           11          11.5                         22
    ## 1426           22          14.5                         49
    ## 1427            8          12.1                         20
    ## 1428           14          16.3                         29
    ## 1429           91          11.4                        218
    ## 1430           38          10.4                         96
    ## 1431           53          12.2                        122
    ## 1432           50          11.3                        117
    ## 1433           19           9.8                         48
    ## 1434           31          12.4                         69
    ## 1435           41          11.5                        101
    ## 1436           19          11.0                         48
    ## 1437           22          12.0                         53
    ## 1438          504          17.8                        996
    ## 1439          197          14.5                        438
    ## 1440          307          20.8                        558
    ## 1441          241          15.8                        490
    ## 1442          105          13.7                        235
    ## 1443          136          17.9                        255
    ## 1444          263          20.1                        506
    ## 1445           92          15.4                        203
    ## 1446          171          23.9                        303
    ## 1447          669          24.2                       1128
    ## 1448          302          22.1                        532
    ## 1449          367          26.2                        596
    ## 1450          357          20.7                        625
    ## 1451          160          19.1                        287
    ## 1452          197          22.2                        338
    ## 1453          312          29.9                        503
    ## 1454          142          26.6                        245
    ## 1455          170          33.3                        258
    ## 1456          841          24.9                       1471
    ## 1457          380          23.3                        700
    ## 1458          461          26.3                        771
    ## 1459          466          23.2                        855
    ## 1460          181          19.3                        376
    ## 1461          285          26.7                        479
    ## 1462          375          27.3                        616
    ## 1463          199          28.7                        324
    ## 1464          176          25.8                        292
    ## 1465          111          19.9                        193
    ## 1466           49          17.0                         88
    ## 1467           62          23.0                        105
    ## 1468           52          17.7                         96
    ## 1469           18          12.9                         37
    ## 1470           34          22.1                         59
    ## 1471           59          22.5                         97
    ## 1472           31          20.9                         51
    ## 1473           28          24.6                         46
    ## 1474          202          15.2                        476
    ## 1475           96          14.7                        228
    ## 1476          106          15.8                        248
    ## 1477          109          13.5                        274
    ## 1478           55          13.2                        132
    ## 1479           54          13.9                        142
    ## 1480           93          18.0                        202
    ## 1481           41          17.4                         96
    ## 1482           52          18.4                        106
    ## 1483          209          11.4                        519
    ## 1484           85           9.4                        236
    ## 1485          124          13.3                        283
    ## 1486          105          10.7                        264
    ## 1487           42           8.8                        116
    ## 1488           63          12.6                        148
    ## 1489          104          12.2                        255
    ## 1490           43          10.1                        120
    ## 1491           61          14.3                        135
    ## 1492          333          12.3                        746
    ## 1493          128           9.7                        357
    ## 1494          205          14.7                        389
    ## 1495          177          11.7                        391
    ## 1496           67           9.1                        188
    ## 1497          110          14.1                        203
    ## 1498          156          13.1                        355
    ## 1499           61          10.4                        169
    ## 1500           95          15.5                        186
    ## 1501           81           8.6                        221
    ## 1502           31           6.8                        102
    ## 1503           50          10.4                        119
    ## 1504           36           7.6                        111
    ## 1505           13           6.0                         49
    ## 1506           23           8.9                         62
    ## 1507           45           9.7                        110
    ## 1508           18           7.5                         53
    ## 1509           27          11.9                         57
    ## 1510          117          13.0                        258
    ## 1511           58          13.6                        123
    ## 1512           59          12.4                        135
    ## 1513           78          18.1                        140
    ## 1514           39          19.5                         71
    ## 1515           39          17.0                         69
    ## 1516           39           8.3                        118
    ## 1517           19           8.4                         52
    ## 1518           20           8.2                         66
    ## 1519          231          17.0                        461
    ## 1520           91          13.8                        194
    ## 1521          140          20.1                        267
    ## 1522          146          18.4                        270
    ## 1523           64          15.9                        120
    ## 1524           82          20.9                        150
    ## 1525           85          15.1                        191
    ## 1526           27          10.5                         74
    ## 1527           58          19.0                        117
    ## 1528           80          13.7                        152
    ## 1529           33          11.7                         76
    ## 1530           47          15.7                         76
    ## 1531           32          15.5                         43
    ## 1532           13          12.7                         18
    ## 1533           19          18.3                         25
    ## 1534           48          12.7                        109
    ## 1535           20          11.0                         58
    ## 1536           28          14.3                         51
    ## 1537          225          16.3                        452
    ## 1538          104          15.6                        213
    ## 1539          121          17.1                        239
    ## 1540          122          16.1                        240
    ## 1541           54          15.3                        103
    ## 1542           68          16.9                        137
    ## 1543          103          16.6                        212
    ## 1544           50          15.9                        110
    ## 1545           53          17.3                        102
    ## 1546          410          23.2                        751
    ## 1547          179          20.1                        349
    ## 1548          231          26.3                        402
    ## 1549          256          24.9                        453
    ## 1550          110          20.9                        213
    ## 1551          146          29.1                        240
    ## 1552          154          20.9                        298
    ## 1553           69          19.1                        136
    ## 1554           85          22.5                        162
    ## 1555          305          13.6                        648
    ## 1556          137          12.3                        310
    ## 1557          168          14.8                        338
    ## 1558          164          14.2                        319
    ## 1559           78          13.5                        152
    ## 1560           86          14.8                        167
    ## 1561          141          12.9                        329
    ## 1562           59          11.0                        158
    ## 1563           82          14.8                        171
    ## 1564           11           8.8                         59
    ## 1565            5           7.9                         23
    ## 1566            6           9.7                         36
    ## 1567           10          10.9                         50
    ## 1568            5           9.6                         20
    ## 1569            5          12.5                         30
    ## 1570           93          24.1                        154
    ## 1571           40          21.1                         70
    ## 1572           53          27.0                         84
    ## 1573           62          22.9                        106
    ## 1574           26          19.4                         48
    ## 1575           36          26.3                         58
    ## 1576           31          27.0                         48
    ## 1577           14          25.0                         22
    ## 1578           17          28.8                         26
    ## 1579           33          14.9                         66
    ## 1580           17          14.7                         36
    ## 1581           16          15.2                         30
    ## 1582           14          10.8                         28
    ## 1583            7          10.6                         12
    ## 1584            7          10.9                         16
    ## 1585           19          22.4                         38
    ## 1586           10          20.4                         24
    ## 1587            9          25.0                         14
    ## 1588           99          22.3                        175
    ## 1589           43          20.6                         84
    ## 1590           56          23.9                         91
    ## 1591           36          13.3                         84
    ## 1592           19          14.1                         48
    ## 1593           17          12.6                         36
    ## 1594           63          37.7                         91
    ## 1595           24          32.4                         36
    ## 1596           39          41.9                         55
    ## 1597           80          21.9                        151
    ## 1598           37          21.4                         75
    ## 1599           43          22.3                         76
    ## 1600           26          15.9                         55
    ## 1601           13          18.3                         25
    ## 1602           13          14.0                         30
    ## 1603           54          26.7                         96
    ## 1604           24          23.5                         50
    ## 1605           30          30.0                         46
    ## 1606           42          24.4                         61
    ## 1607           20          24.4                         30
    ## 1608           22          24.4                         31
    ## 1609           31          21.5                         48
    ## 1610           15          21.7                         25
    ## 1611           16          21.3                         23
    ## 1612           55          21.2                         95
    ## 1613           25          18.5                         43
    ## 1614           30          24.0                         52
    ## 1615           35          20.1                         58
    ## 1616           14          16.9                         26
    ## 1617           21          23.1                         32
    ## 1618           20          23.3                         37
    ## 1619           11          21.2                         17
    ## 1620            9          26.5                         20
    ## 1621           48          18.8                         92
    ## 1622           18          13.8                         44
    ## 1623           30          24.0                         48
    ## 1624           28          18.4                         56
    ## 1625           11          13.6                         26
    ## 1626           17          23.9                         30
    ## 1627           20          19.4                         36
    ## 1628            7          14.3                         18
    ## 1629           13          24.1                         18
    ## 1630          231          12.0                        469
    ## 1631          118          12.3                        239
    ## 1632          113          11.8                        230
    ## 1633          146          11.8                        298
    ## 1634           76          12.3                        154
    ## 1635           70          11.3                        144
    ## 1636           85          12.4                        171
    ## 1637           42          12.2                         85
    ## 1638           43          12.6                         86
    ## 1639           47          11.7                        108
    ## 1640           23          12.4                         44
    ## 1641           24          11.2                         64
    ## 1642           22           8.7                         57
    ## 1643           12           9.4                         23
    ## 1644           10           8.0                         34
    ## 1645           25          16.8                         51
    ## 1646           11          18.6                         21
    ## 1647           14          15.6                         30
    ## 1648          503          23.2                        881
    ## 1649          242          22.2                        440
    ## 1650          261          24.3                        441
    ## 1651          297          21.0                        531
    ## 1652          139          19.4                        254
    ## 1653          158          22.6                        277
    ## 1654          206          27.5                        350
    ## 1655          103          27.3                        186
    ## 1656          103          27.7                        164
    ## 1657          104          18.1                        198
    ## 1658           45          16.5                         92
    ## 1659           59          19.4                        106
    ## 1660           60          17.0                        114
    ## 1661           27          15.8                         56
    ## 1662           33          18.2                         58
    ## 1663           44          19.6                         84
    ## 1664           18          17.8                         36
    ## 1665           26          21.1                         48
    ## 1666           98          14.5                        217
    ## 1667           35          10.6                         94
    ## 1668           63          18.1                        123
    ## 1669           53          14.6                        119
    ## 1670           19          10.5                         56
    ## 1671           34          18.6                         63
    ## 1672           45          14.6                         98
    ## 1673           16          10.8                         38
    ## 1674           29          18.0                         60
    ## 1675          226          21.1                        412
    ## 1676          106          19.9                        196
    ## 1677          120          22.3                        216
    ## 1678          109          18.8                        201
    ## 1679           51          17.6                         94
    ## 1680           58          20.0                        107
    ## 1681          117          24.0                        211
    ## 1682           55          22.6                        102
    ## 1683           62          25.3                        109
    ## 1684          132          18.4                        247
    ## 1685           64          16.4                        130
    ## 1686           68          20.7                        117
    ## 1687           80          18.7                        150
    ## 1688           40          17.1                         77
    ## 1689           40          20.7                         73
    ## 1690           52          18.3                         97
    ## 1691           24          15.7                         53
    ## 1692           28          21.4                         44
    ## 1693          170          15.1                        358
    ## 1694           73          13.6                        172
    ## 1695           97          16.5                        186
    ## 1696          101          15.3                        208
    ## 1697           43          14.1                        101
    ## 1698           58          16.3                        107
    ## 1699           69          14.9                        150
    ## 1700           30          13.0                         71
    ## 1701           39          16.7                         79
    ## 1702           67          23.6                        118
    ## 1703           24          17.6                         43
    ## 1704           43          29.1                         75
    ## 1705           40          24.2                         74
    ## 1706           15          18.5                         29
    ## 1707           25          29.8                         45
    ## 1708           27          22.9                         44
    ## 1709            9          16.7                         14
    ## 1710           18          28.1                         30
    ## 1711          208          23.6                        373
    ## 1712           93          21.0                        182
    ## 1713          115          26.1                        191
    ## 1714          121          20.5                        226
    ## 1715           54          18.3                        113
    ## 1716           67          22.7                        113
    ## 1717           87          30.1                        147
    ## 1718           39          27.3                         69
    ## 1719           48          32.9                         78
    ## 1720           39          10.5                        110
    ## 1721           13           7.0                         57
    ## 1722           26          14.1                         53
    ## 1723           27           9.9                         78
    ## 1724           10           7.1                         39
    ## 1725           17          12.8                         39
    ## 1726            9          10.0                         29
    ## 1727            9          18.4                         14
    ## 1728           63          24.0                        101
    ## 1729           31          24.8                         49
    ## 1730           32          23.2                         52
    ## 1731           30          19.0                         45
    ## 1732           14          18.4                         22
    ## 1733           16          19.5                         23
    ## 1734           33          31.4                         56
    ## 1735           17          34.7                         27
    ## 1736           16          28.6                         29
    ## 1737           59          10.0                        153
    ## 1738           25           8.8                         72
    ## 1739           34          11.1                         81
    ## 1740           36          10.2                         90
    ## 1741           18          11.0                         43
    ## 1742           18           9.5                         47
    ## 1743           23          10.1                         63
    ## 1744            7           6.0                         29
    ## 1745           16          14.3                         34
    ## 1746          145          16.1                        293
    ## 1747           69          16.2                        138
    ## 1748           76          16.1                        155
    ## 1749           74          15.3                        159
    ## 1750           37          16.5                         76
    ## 1751           37          14.2                         83
    ## 1752           71          17.3                        134
    ## 1753           32          16.1                         62
    ## 1754           39          18.4                         72
    ## 1755           47           8.3                        132
    ## 1756           20           7.4                         57
    ## 1757           27           9.1                         75
    ## 1758           22           6.8                         74
    ## 1759           11           7.2                         32
    ## 1760           11           6.4                         42
    ## 1761           25          10.7                         58
    ## 1762            9           7.7                         25
    ## 1763           16          13.7                         33
    ## 1764           85          10.3                        187
    ## 1765           36           9.5                         82
    ## 1766           49          10.9                        105
    ## 1767           44           8.5                        103
    ## 1768           21           8.6                         51
    ## 1769           23           8.4                         52
    ## 1770           41          13.3                         84
    ## 1771           15          11.1                         31
    ## 1772           26          14.9                         53
    ## 1773          117          18.6                        232
    ## 1774           48          16.3                        102
    ## 1775           69          20.6                        130
    ## 1776           77          15.4                        168
    ## 1777           32          13.5                         75
    ## 1778           45          17.2                         93
    ## 1779           40          32.0                         64
    ## 1780           16          29.1                         27
    ## 1781           24          34.3                         37
    ## 1782          168          12.5                        350
    ## 1783           78          11.5                        180
    ## 1784           90          13.7                        170
    ## 1785          105          12.2                        213
    ## 1786           54          12.6                        109
    ## 1787           51          11.9                        104
    ## 1788           63          13.1                        137
    ## 1789           24           9.6                         71
    ## 1790           39          17.0                         66
    ## 1791           88          12.4                        189
    ## 1792           33          10.4                         83
    ## 1793           55          14.1                        106
    ## 1794           49          11.0                        110
    ## 1795           18           9.4                         48
    ## 1796           31          12.1                         62
    ## 1797           39          15.3                         79
    ## 1798           15          12.2                         35
    ## 1799           24          18.2                         44
    ## 1800           66          13.2                        158
    ## 1801           34          13.6                         88
    ## 1802           32          12.7                         70
    ## 1803           39          13.2                         86
    ## 1804           21          14.7                         48
    ## 1805           18          11.8                         38
    ## 1806           27          13.1                         72
    ## 1807           13          12.1                         40
    ## 1808           14          14.1                         32
    ## 1809           51           9.6                        117
    ## 1810           17           6.9                         50
    ## 1811           34          11.8                         67
    ## 1812           31          10.1                         62
    ## 1813           12           8.2                         28
    ## 1814           19          11.9                         34
    ## 1815           20           8.8                         55
    ## 1816            5           5.0                         22
    ## 1817           15          11.7                         33
    ## 1818          257          16.4                        473
    ## 1819          128          16.2                        237
    ## 1820          129          16.6                        236
    ## 1821          177          18.0                        328
    ## 1822           87          17.0                        167
    ## 1823           90          19.1                        161
    ## 1824           80          13.7                        145
    ## 1825           41          14.7                         70
    ## 1826           39          12.8                         75
    ## 1827          602          19.4                       1076
    ## 1828          260          17.8                        498
    ## 1829          342          20.9                        578
    ## 1830          307          17.2                        574
    ## 1831          142          16.6                        276
    ## 1832          165          17.8                        298
    ## 1833          295          22.5                        502
    ## 1834          118          19.5                        222
    ## 1835          177          25.0                        280
    ## 1836           45          13.9                         98
    ## 1837           23          13.8                         52
    ## 1838           22          14.0                         46
    ## 1839           30          14.6                         59
    ## 1840           16          15.5                         34
    ## 1841           14          13.6                         25
    ## 1842           15          12.7                         39
    ## 1843            7          10.9                         18
    ## 1844            8          14.8                         21
    ## 1845           69           8.8                        151
    ## 1846           27           7.2                         69
    ## 1847           42          10.3                         82
    ## 1848           35           7.2                         86
    ## 1849           14           6.2                         43
    ## 1850           21           8.0                         43
    ## 1851           34          11.6                         65
    ## 1852           13           9.0                         26
    ## 1853           21          14.2                         39
    ## 1854           48           8.8                         98
    ## 1855           20           8.1                         41
    ## 1856           28           9.4                         57
    ## 1857           27           8.5                         53
    ## 1858           13           8.8                         26
    ## 1859           14           8.3                         27
    ## 1860           21           9.7                         45
    ## 1861            7           7.4                         15
    ## 1862           14          11.4                         30
    ## 1863           70           4.3                        274
    ## 1864           32           3.9                        146
    ## 1865           38           4.6                        128
    ## 1866           35           3.7                        156
    ## 1867           15           3.1                         92
    ## 1868           20           4.3                         64
    ## 1869           35           5.1                        118
    ## 1870           17           5.1                         54
    ## 1871           18           5.1                         64
    ## 1872           54           9.7                        133
    ## 1873           21           8.0                         61
    ## 1874           33          11.3                         72
    ## 1875           21           7.9                         65
    ## 1876            8           6.4                         31
    ## 1877           13           9.3                         34
    ## 1878           33          11.5                         68
    ## 1879           13           9.7                         30
    ## 1880           20          13.2                         38
    ## 1881           49          17.4                         97
    ## 1882           25          19.5                         47
    ## 1883           24          15.7                         50
    ## 1884           30          17.0                         53
    ## 1885           15          20.3                         25
    ## 1886           15          14.7                         28
    ## 1887           19          18.1                         44
    ## 1888           10          18.5                         22
    ## 1889            9          17.6                         22
    ## 1890           53          18.7                        106
    ## 1891           25          17.1                         52
    ## 1892           28          20.3                         54
    ## 1893           20          11.8                         58
    ## 1894            9          10.8                         30
    ## 1895           11          12.6                         28
    ## 1896           33          28.9                         48
    ## 1897           16          25.4                         22
    ## 1898           17          33.3                         26
    ## 1899           55          23.9                         96
    ## 1900           24          21.8                         43
    ## 1901           31          25.8                         53
    ## 1902           34          24.3                         60
    ## 1903           17          25.4                         29
    ## 1904           17          23.3                         31
    ## 1905           21          23.3                         36
    ## 1906            7          16.3                         14
    ## 1907           14          29.8                         22
    ##      percent_overweight_or_obese    grade_level number_healthy_weight
    ## 1                           30.5 DISTRICT TOTAL                   445
    ## 2                           28.1 DISTRICT TOTAL                   232
    ## 3                           32.9 DISTRICT TOTAL                   213
    ## 4                           26.7     ELEMENTARY                   340
    ## 5                           22.8     ELEMENTARY                   181
    ## 6                           30.6     ELEMENTARY                   159
    ## 7                           41.7    MIDDLE/HIGH                   105
    ## 8                           42.7    MIDDLE/HIGH                    51
    ## 9                           40.7    MIDDLE/HIGH                    54
    ## 10                          37.3     ELEMENTARY                   121
    ## 11                          45.7    MIDDLE/HIGH                   102
    ## 12                          39.0     ELEMENTARY                   217
    ## 13                          41.1     ELEMENTARY                    96
    ## 14                          41.2 DISTRICT TOTAL                   319
    ## 15                          42.4 DISTRICT TOTAL                   156
    ## 16                          40.0 DISTRICT TOTAL                   163
    ## 17                          44.4    MIDDLE/HIGH                    60
    ## 18                          47.5    MIDDLE/HIGH                    42
    ## 19                          39.2    MIDDLE/HIGH                   175
    ## 20                          35.5 DISTRICT TOTAL                   982
    ## 21                          35.3 DISTRICT TOTAL                   496
    ## 22                          39.2    MIDDLE/HIGH                   169
    ## 23                          33.2     ELEMENTARY                   638
    ## 24                          32.9     ELEMENTARY                   321
    ## 25                          33.5     ELEMENTARY                   317
    ## 26                          35.6 DISTRICT TOTAL                   486
    ## 27                          39.2    MIDDLE/HIGH                   344
    ## 28                          31.0 DISTRICT TOTAL                  1268
    ## 29                          29.8 DISTRICT TOTAL                   641
    ## 30                          32.1 DISTRICT TOTAL                   627
    ## 31                          30.1     ELEMENTARY                   681
    ## 32                          29.5     ELEMENTARY                   353
    ## 33                          30.8     ELEMENTARY                   328
    ## 34                          31.9    MIDDLE/HIGH                   587
    ## 35                          30.1    MIDDLE/HIGH                   288
    ## 36                          33.5    MIDDLE/HIGH                   299
    ## 37                          30.9 DISTRICT TOTAL                  1074
    ## 38                          28.9 DISTRICT TOTAL                   556
    ## 39                          31.8    MIDDLE/HIGH                   209
    ## 40                          34.6    MIDDLE/HIGH                   202
    ## 41                          27.1     ELEMENTARY                   347
    ## 42                          31.7     ELEMENTARY                   316
    ## 43                          32.8 DISTRICT TOTAL                   518
    ## 44                          29.4     ELEMENTARY                   663
    ## 45                          33.2    MIDDLE/HIGH                   411
    ## 46                          34.1     ELEMENTARY                    83
    ## 47                          37.3 DISTRICT TOTAL                   129
    ## 48                          31.7     ELEMENTARY                   162
    ## 49                          29.3     ELEMENTARY                    79
    ## 50                          37.8 DISTRICT TOTAL                   250
    ## 51                          38.2 DISTRICT TOTAL                   121
    ## 52                          45.2    MIDDLE/HIGH                    46
    ## 53                          48.5    MIDDLE/HIGH                    88
    ## 54                          51.7    MIDDLE/HIGH                    42
    ## 55                          37.2 DISTRICT TOTAL                   297
    ## 56                          40.2 DISTRICT TOTAL                   131
    ## 57                          34.7 DISTRICT TOTAL                   166
    ## 58                          32.3     ELEMENTARY                   176
    ## 59                          34.2     ELEMENTARY                    75
    ## 60                          30.8     ELEMENTARY                   101
    ## 61                          43.0    MIDDLE/HIGH                   121
    ## 62                          46.7    MIDDLE/HIGH                    56
    ## 63                          39.8    MIDDLE/HIGH                    65
    ## 64                          36.5     ELEMENTARY                   277
    ## 65                          44.4    MIDDLE/HIGH                   206
    ## 66                          36.5     ELEMENTARY                   552
    ## 67                          36.4     ELEMENTARY                   275
    ## 68                          38.7 DISTRICT TOTAL                   758
    ## 69                          38.3 DISTRICT TOTAL                   380
    ## 70                          39.0 DISTRICT TOTAL                   378
    ## 71                          43.9    MIDDLE/HIGH                   105
    ## 72                          44.9    MIDDLE/HIGH                   101
    ## 73                          37.5 DISTRICT TOTAL                   172
    ## 74                          36.9 DISTRICT TOTAL                    77
    ## 75                          43.8    MIDDLE/HIGH                    27
    ## 76                          40.4    MIDDLE/HIGH                    31
    ## 77                          32.4     ELEMENTARY                    50
    ## 78                          36.6     ELEMENTARY                    64
    ## 79                          37.9 DISTRICT TOTAL                    95
    ## 80                          34.9     ELEMENTARY                   114
    ## 81                          42.0    MIDDLE/HIGH                    58
    ## 82                          36.8 DISTRICT TOTAL                   303
    ## 83                          36.1 DISTRICT TOTAL                   138
    ## 84                          36.0     ELEMENTARY                    98
    ## 85                          37.4 DISTRICT TOTAL                   165
    ## 86                          36.5     ELEMENTARY                   195
    ## 87                          37.0     ELEMENTARY                    97
    ## 88                          37.6    MIDDLE/HIGH                   108
    ## 89                          33.9    MIDDLE/HIGH                    41
    ## 90                          39.6    MIDDLE/HIGH                    67
    ## 91                          33.7 DISTRICT TOTAL                   257
    ## 92                          39.4    MIDDLE/HIGH                   140
    ## 93                          28.4     ELEMENTARY                   377
    ## 94                          28.3     ELEMENTARY                   185
    ## 95                          28.5     ELEMENTARY                   192
    ## 96                          31.5 DISTRICT TOTAL                   517
    ## 97                          29.2 DISTRICT TOTAL                   260
    ## 98                          31.8    MIDDLE/HIGH                    75
    ## 99                          46.3    MIDDLE/HIGH                    65
    ## 100                         33.3 DISTRICT TOTAL                   325
    ## 101                         33.8 DISTRICT TOTAL                   157
    ## 102                         33.0 DISTRICT TOTAL                   168
    ## 103                         34.2     ELEMENTARY                   200
    ## 104                         35.4     ELEMENTARY                    95
    ## 105                         33.1     ELEMENTARY                   105
    ## 106                         32.7    MIDDLE/HIGH                   125
    ## 107                         31.1    MIDDLE/HIGH                    62
    ## 108                         34.0    MIDDLE/HIGH                    63
    ## 109                         32.3     ELEMENTARY                    63
    ## 110                         37.5 DISTRICT TOTAL                    85
    ## 111                         31.0     ELEMENTARY                   116
    ## 112                         29.3     ELEMENTARY                    53
    ## 113                         37.3 DISTRICT TOTAL                   160
    ## 114                         37.0 DISTRICT TOTAL                    75
    ## 115                         49.4    MIDDLE/HIGH                    44
    ## 116                         50.0    MIDDLE/HIGH                    22
    ## 117                         48.8    MIDDLE/HIGH                    22
    ## 118                         19.2    MIDDLE/HIGH                    42
    ## 119                         37.4 DISTRICT TOTAL                    92
    ## 120                         43.8     ELEMENTARY                    50
    ## 121                         51.0     ELEMENTARY                    24
    ## 122                         41.6 DISTRICT TOTAL                    45
    ## 123                         32.9 DISTRICT TOTAL                    47
    ## 124                         35.0     ELEMENTARY                    26
    ## 125                         36.1 DISTRICT TOTAL                   487
    ## 126                         35.8 DISTRICT TOTAL                   229
    ## 127                         36.4 DISTRICT TOTAL                   258
    ## 128                         31.5     ELEMENTARY                   346
    ## 129                         28.5     ELEMENTARY                   168
    ## 130                         34.1     ELEMENTARY                   178
    ## 131                         44.9    MIDDLE/HIGH                   141
    ## 132                         49.2    MIDDLE/HIGH                    61
    ## 133                         41.0    MIDDLE/HIGH                    80
    ## 134                         29.5     ELEMENTARY                    62
    ## 135                         35.8 DISTRICT TOTAL                    98
    ## 136                         34.7     ELEMENTARY                   111
    ## 137                         40.2     ELEMENTARY                    49
    ## 138                         39.0 DISTRICT TOTAL                   188
    ## 139                         42.3 DISTRICT TOTAL                    90
    ## 140                         45.8    MIDDLE/HIGH                    77
    ## 141                         44.6    MIDDLE/HIGH                    41
    ## 142                         47.1    MIDDLE/HIGH                    36
    ## 143                         40.2 DISTRICT TOTAL                   194
    ## 144                         50.0    MIDDLE/HIGH                    45
    ## 145                         52.3    MIDDLE/HIGH                    16
    ## 146                         48.6    MIDDLE/HIGH                    29
    ## 147                         35.0     ELEMENTARY                    67
    ## 148                         40.1 DISTRICT TOTAL                    83
    ## 149                         40.3 DISTRICT TOTAL                   111
    ## 150                         35.2     ELEMENTARY                   149
    ## 151                         35.4     ELEMENTARY                    82
    ## 152                         34.2 DISTRICT TOTAL                    99
    ## 153                         39.7    MIDDLE/HIGH                    79
    ## 154                         27.5     ELEMENTARY                   116
    ## 155                         26.7     ELEMENTARY                    55
    ## 156                         28.2     ELEMENTARY                    61
    ## 157                         30.4 DISTRICT TOTAL                    96
    ## 158                         32.4 DISTRICT TOTAL                   195
    ## 159                         34.9    MIDDLE/HIGH                    41
    ## 160                         44.1    MIDDLE/HIGH                    38
    ## 161                         42.5 DISTRICT TOTAL                   192
    ## 162                         38.8 DISTRICT TOTAL                   109
    ## 163                         42.6    MIDDLE/HIGH                    35
    ## 164                         51.8    MIDDLE/HIGH                    27
    ## 165                         36.8     ELEMENTARY                    74
    ## 166                         44.5     ELEMENTARY                    56
    ## 167                         46.4 DISTRICT TOTAL                    83
    ## 168                         40.5     ELEMENTARY                   130
    ## 169                         47.0    MIDDLE/HIGH                    62
    ## 170                         35.8 DISTRICT TOTAL                   158
    ## 171                         45.5 DISTRICT TOTAL                    66
    ## 172                         26.4 DISTRICT TOTAL                    92
    ## 173                         36.8     ELEMENTARY                    91
    ## 174                         43.4     ELEMENTARY                    43
    ## 175                         29.4     ELEMENTARY                    48
    ## 176                         34.3    MIDDLE/HIGH                    67
    ## 177                         48.9    MIDDLE/HIGH                    23
    ## 178                         22.8    MIDDLE/HIGH                    44
    ## 179                         37.7     ELEMENTARY                    66
    ## 180                         37.4 DISTRICT TOTAL                   112
    ## 181                         39.7     ELEMENTARY                   120
    ## 182                         41.9     ELEMENTARY                    54
    ## 183                         39.2 DISTRICT TOTAL                   206
    ## 184                         41.3 DISTRICT TOTAL                    94
    ## 185                         38.6    MIDDLE/HIGH                    86
    ## 186                         40.3    MIDDLE/HIGH                    40
    ## 187                         37.0    MIDDLE/HIGH                    46
    ## 188                         36.0 DISTRICT TOTAL                   393
    ## 189                         36.1 DISTRICT TOTAL                   193
    ## 190                         35.9 DISTRICT TOTAL                   200
    ## 191                         26.1     ELEMENTARY                   264
    ## 192                         26.4     ELEMENTARY                   128
    ## 193                         25.7     ELEMENTARY                   136
    ## 194                         49.8    MIDDLE/HIGH                   129
    ## 195                         49.2    MIDDLE/HIGH                    65
    ## 196                         50.4    MIDDLE/HIGH                    64
    ## 197                         45.1     ELEMENTARY                    56
    ## 198                         44.1 DISTRICT TOTAL                   100
    ## 199                         43.2     ELEMENTARY                   126
    ## 200                         41.7     ELEMENTARY                    70
    ## 201                         42.6 DISTRICT TOTAL                   228
    ## 202                         41.3 DISTRICT TOTAL                   128
    ## 203                         41.7    MIDDLE/HIGH                   102
    ## 204                         40.8    MIDDLE/HIGH                    58
    ## 205                         42.9    MIDDLE/HIGH                    44
    ## 206                         48.2 DISTRICT TOTAL                   101
    ## 207                         52.2     ELEMENTARY                    32
    ## 208                         53.1 DISTRICT TOTAL                    45
    ## 209                         43.4 DISTRICT TOTAL                    56
    ## 210                         45.9     ELEMENTARY                    73
    ## 211                         51.6    MIDDLE/HIGH                    15
    ## 212                         39.7     ELEMENTARY                    41
    ## 213                         50.0    MIDDLE/HIGH                    28
    ## 214                         32.4 DISTRICT TOTAL                    46
    ## 215                         27.1 DISTRICT TOTAL                    51
    ## 216                         28.2     ELEMENTARY                    61
    ## 217                         25.6     ELEMENTARY                    29
    ## 218                         30.4     ELEMENTARY                    32
    ## 219                         29.7 DISTRICT TOTAL                    97
    ## 220                         32.1    MIDDLE/HIGH                    36
    ## 221                         41.4    MIDDLE/HIGH                    17
    ## 222                         37.0 DISTRICT TOTAL                   159
    ## 223                         36.5    MIDDLE/HIGH                    54
    ## 224                         37.8    MIDDLE/HIGH                    28
    ## 225                         35.0    MIDDLE/HIGH                    26
    ## 226                         39.5     ELEMENTARY                    46
    ## 227                         38.6 DISTRICT TOTAL                    74
    ## 228                         35.5 DISTRICT TOTAL                    85
    ## 229                         38.7     ELEMENTARY                   105
    ## 230                         37.9     ELEMENTARY                    59
    ## 231                         43.5 DISTRICT TOTAL                  1207
    ## 232                         45.8 DISTRICT TOTAL                   567
    ## 233                         41.4 DISTRICT TOTAL                   640
    ## 234                         43.4     ELEMENTARY                   722
    ## 235                         45.9     ELEMENTARY                   333
    ## 236                         41.0     ELEMENTARY                   389
    ## 237                         43.9    MIDDLE/HIGH                   485
    ## 238                         45.7    MIDDLE/HIGH                   234
    ## 239                         42.2    MIDDLE/HIGH                   251
    ## 240                         33.7 DISTRICT TOTAL                   118
    ## 241                         34.4 DISTRICT TOTAL                   103
    ## 242                         33.0     ELEMENTARY                   136
    ## 243                         33.3     ELEMENTARY                    70
    ## 244                         34.0 DISTRICT TOTAL                   221
    ## 245                         35.6    MIDDLE/HIGH                    85
    ## 246                         34.2    MIDDLE/HIGH                    48
    ## 247                         37.3    MIDDLE/HIGH                    37
    ## 248                         32.7     ELEMENTARY                    66
    ## 249                         29.1     ELEMENTARY                    56
    ## 250                         44.8    MIDDLE/HIGH                    58
    ## 251                         45.1    MIDDLE/HIGH                    28
    ## 252                         44.4    MIDDLE/HIGH                    30
    ## 253                         41.9 DISTRICT TOTAL                    90
    ## 254                         35.3 DISTRICT TOTAL                    86
    ## 255                         35.5     ELEMENTARY                   118
    ## 256                         38.9 DISTRICT TOTAL                   176
    ## 257                         40.4     ELEMENTARY                    62
    ## 258                         41.1 DISTRICT TOTAL                   383
    ## 259                         41.3 DISTRICT TOTAL                   196
    ## 260                         41.0 DISTRICT TOTAL                   187
    ## 261                         37.3     ELEMENTARY                   244
    ## 262                         37.5     ELEMENTARY                   125
    ## 263                         37.2     ELEMENTARY                   119
    ## 264                         46.9    MIDDLE/HIGH                   139
    ## 265                         47.0    MIDDLE/HIGH                    71
    ## 266                         46.9    MIDDLE/HIGH                    68
    ## 267                         38.8 DISTRICT TOTAL                   246
    ## 268                         39.1 DISTRICT TOTAL                   121
    ## 269                         49.3    MIDDLE/HIGH                    36
    ## 270                         39.7    MIDDLE/HIGH                    47
    ## 271                         35.1     ELEMENTARY                    85
    ## 272                         39.5     ELEMENTARY                    78
    ## 273                         38.5 DISTRICT TOTAL                   125
    ## 274                         37.3     ELEMENTARY                   163
    ## 275                         44.3    MIDDLE/HIGH                    83
    ## 276                         39.1 DISTRICT TOTAL                   437
    ## 277                         38.3 DISTRICT TOTAL                   216
    ## 278                         36.6     ELEMENTARY                   156
    ## 279                         39.9 DISTRICT TOTAL                   221
    ## 280                         35.0     ELEMENTARY                   304
    ## 281                         33.3     ELEMENTARY                   148
    ## 282                         46.8    MIDDLE/HIGH                   133
    ## 283                         46.9    MIDDLE/HIGH                    68
    ## 284                         46.7    MIDDLE/HIGH                    65
    ## 285                         37.5 DISTRICT TOTAL                   426
    ## 286                         44.8 DISTRICT TOTAL                   192
    ## 287                         51.7    MIDDLE/HIGH                    70
    ## 288                         33.8    MIDDLE/HIGH                    94
    ## 289                         41.1     ELEMENTARY                   122
    ## 290                         27.0     ELEMENTARY                   140
    ## 291                         29.9 DISTRICT TOTAL                   234
    ## 292                         34.3     ELEMENTARY                   262
    ## 293                         42.6    MIDDLE/HIGH                   164
    ## 294                         38.8 DISTRICT TOTAL                   244
    ## 295                         37.2 DISTRICT TOTAL                   120
    ## 296                         40.4 DISTRICT TOTAL                   124
    ## 297                         35.9     ELEMENTARY                   164
    ## 298                         34.2     ELEMENTARY                    77
    ## 299                         37.4     ELEMENTARY                    87
    ## 300                         45.2    MIDDLE/HIGH                    80
    ## 301                         43.0    MIDDLE/HIGH                    43
    ## 302                         47.5    MIDDLE/HIGH                    37
    ## 303                         32.6     ELEMENTARY                   143
    ## 304                         31.8 DISTRICT TOTAL                   198
    ## 305                         35.7     ELEMENTARY                   272
    ## 306                         38.9     ELEMENTARY                   129
    ## 307                         33.4 DISTRICT TOTAL                   413
    ## 308                         34.9 DISTRICT TOTAL                   215
    ## 309                         28.8    MIDDLE/HIGH                   141
    ## 310                         27.7    MIDDLE/HIGH                    86
    ## 311                         30.4    MIDDLE/HIGH                    55
    ## 312                         30.5 DISTRICT TOTAL                  1580
    ## 313                         33.7    MIDDLE/HIGH                   712
    ## 314                         30.8    MIDDLE/HIGH                   384
    ## 315                         36.7    MIDDLE/HIGH                   328
    ## 316                         27.7     ELEMENTARY                   421
    ## 317                         29.2 DISTRICT TOTAL                   805
    ## 318                         31.8 DISTRICT TOTAL                   775
    ## 319                         27.7     ELEMENTARY                   868
    ## 320                         27.6     ELEMENTARY                   447
    ## 321                         32.6 DISTRICT TOTAL                   319
    ## 322                         28.2     ELEMENTARY                    89
    ## 323                         35.0 DISTRICT TOTAL                   143
    ## 324                         30.6 DISTRICT TOTAL                   176
    ## 325                         30.0     ELEMENTARY                   191
    ## 326                         30.2    MIDDLE/HIGH                    74
    ## 327                         31.4     ELEMENTARY                   102
    ## 328                         36.6    MIDDLE/HIGH                   128
    ## 329                         43.8    MIDDLE/HIGH                    54
    ## 330                         31.3 DISTRICT TOTAL                   163
    ## 331                         27.2 DISTRICT TOTAL                   196
    ## 332                         25.8     ELEMENTARY                   209
    ## 333                         28.1     ELEMENTARY                    95
    ## 334                         23.7     ELEMENTARY                   114
    ## 335                         29.1 DISTRICT TOTAL                   359
    ## 336                         33.5    MIDDLE/HIGH                   150
    ## 337                         35.8    MIDDLE/HIGH                    68
    ## 338                         31.5    MIDDLE/HIGH                    82
    ## 339                         31.7 DISTRICT TOTAL                   182
    ## 340                         34.5    MIDDLE/HIGH                    88
    ## 341                         45.3    MIDDLE/HIGH                    35
    ## 342                         25.6    MIDDLE/HIGH                    53
    ## 343                         27.3     ELEMENTARY                    48
    ## 344                         36.2 DISTRICT TOTAL                    83
    ## 345                         27.7 DISTRICT TOTAL                    99
    ## 346                         29.3     ELEMENTARY                    94
    ## 347                         31.3     ELEMENTARY                    46
    ## 348                         27.3 DISTRICT TOTAL                   258
    ## 349                         32.2 DISTRICT TOTAL                   198
    ## 350                         26.5     ELEMENTARY                   306
    ## 351                         26.8     ELEMENTARY                   165
    ## 352                         29.5 DISTRICT TOTAL                   456
    ## 353                         29.0    MIDDLE/HIGH                    93
    ## 354                         45.2    MIDDLE/HIGH                    57
    ## 355                         26.1     ELEMENTARY                   141
    ## 356                         36.2    MIDDLE/HIGH                   150
    ## 357                         24.6     ELEMENTARY                   710
    ## 358                         28.5    MIDDLE/HIGH                  1115
    ## 359                         27.4    MIDDLE/HIGH                   574
    ## 360                         29.6    MIDDLE/HIGH                   541
    ## 361                         26.8 DISTRICT TOTAL                  1251
    ## 362                         25.1     ELEMENTARY                  1390
    ## 363                         26.6 DISTRICT TOTAL                  2505
    ## 364                         26.4 DISTRICT TOTAL                  1254
    ## 365                         25.5     ELEMENTARY                   680
    ## 366                         32.5 DISTRICT TOTAL                   811
    ## 367                         29.8 DISTRICT TOTAL                   393
    ## 368                         34.9 DISTRICT TOTAL                   418
    ## 369                         29.0     ELEMENTARY                   517
    ## 370                         27.3     ELEMENTARY                   245
    ## 371                         30.4     ELEMENTARY                   272
    ## 372                         38.1    MIDDLE/HIGH                   294
    ## 373                         33.6    MIDDLE/HIGH                   148
    ## 374                         41.9    MIDDLE/HIGH                   146
    ## 375                         37.1     ELEMENTARY                   156
    ## 376                         37.3 DISTRICT TOTAL                   256
    ## 377                         35.0     ELEMENTARY                   322
    ## 378                         32.9     ELEMENTARY                   166
    ## 379                         37.8 DISTRICT TOTAL                   502
    ## 380                         38.2 DISTRICT TOTAL                   246
    ## 381                         43.0    MIDDLE/HIGH                   180
    ## 382                         48.1    MIDDLE/HIGH                    80
    ## 383                         38.5    MIDDLE/HIGH                   100
    ## 384                         32.6 DISTRICT TOTAL                   428
    ## 385                         38.7    MIDDLE/HIGH                   157
    ## 386                         35.4    MIDDLE/HIGH                    82
    ## 387                         41.9    MIDDLE/HIGH                    75
    ## 388                         24.7     ELEMENTARY                   146
    ## 389                         28.4 DISTRICT TOTAL                   228
    ## 390                         36.9 DISTRICT TOTAL                   200
    ## 391                         29.4     ELEMENTARY                   271
    ## 392                         34.0     ELEMENTARY                   125
    ## 393                         28.4 DISTRICT TOTAL                   348
    ## 394                         26.2     ELEMENTARY                   103
    ## 395                         26.5 DISTRICT TOTAL                   181
    ## 396                         30.4 DISTRICT TOTAL                   167
    ## 397                         28.2     ELEMENTARY                   198
    ## 398                         30.8    MIDDLE/HIGH                    72
    ## 399                         30.2     ELEMENTARY                    95
    ## 400                         28.9    MIDDLE/HIGH                   150
    ## 401                         27.1    MIDDLE/HIGH                    78
    ## 402                         28.7 DISTRICT TOTAL                   299
    ## 403                         30.4 DISTRICT TOTAL                   323
    ## 404                         25.8     ELEMENTARY                   392
    ## 405                         23.6     ELEMENTARY                   193
    ## 406                         27.9     ELEMENTARY                   199
    ## 407                         29.6 DISTRICT TOTAL                   622
    ## 408                         35.7    MIDDLE/HIGH                   230
    ## 409                         37.6    MIDDLE/HIGH                   106
    ## 410                         34.0    MIDDLE/HIGH                   124
    ## 411                         27.7 DISTRICT TOTAL                   552
    ## 412                         25.3    MIDDLE/HIGH                   205
    ## 413                         27.7    MIDDLE/HIGH                   107
    ## 414                         22.6    MIDDLE/HIGH                    98
    ## 415                         29.8     ELEMENTARY                   163
    ## 416                         29.0 DISTRICT TOTAL                   270
    ## 417                         26.4 DISTRICT TOTAL                   282
    ## 418                         29.0     ELEMENTARY                   347
    ## 419                         28.3     ELEMENTARY                   184
    ## 420                         28.9 DISTRICT TOTAL                  1166
    ## 421                         29.9 DISTRICT TOTAL                   590
    ## 422                         27.8 DISTRICT TOTAL                   576
    ## 423                         26.5     ELEMENTARY                   766
    ## 424                         27.6     ELEMENTARY                   381
    ## 425                         25.3     ELEMENTARY                   385
    ## 426                         33.2    MIDDLE/HIGH                   400
    ## 427                         34.1    MIDDLE/HIGH                   209
    ## 428                         32.3    MIDDLE/HIGH                   191
    ## 429                         36.0 DISTRICT TOTAL                   188
    ## 430                         32.0 DISTRICT TOTAL                   263
    ## 431                         32.4     ELEMENTARY                   331
    ## 432                         35.5     ELEMENTARY                   131
    ## 433                         33.7 DISTRICT TOTAL                   451
    ## 434                         38.2    MIDDLE/HIGH                    63
    ## 435                         30.3     ELEMENTARY                   200
    ## 436                         38.1    MIDDLE/HIGH                   120
    ## 437                         38.0    MIDDLE/HIGH                    57
    ## 438                         28.6     ELEMENTARY                   391
    ## 439                         32.2    MIDDLE/HIGH                   531
    ## 440                         33.5    MIDDLE/HIGH                   248
    ## 441                         31.1    MIDDLE/HIGH                   283
    ## 442                         27.4     ELEMENTARY                   752
    ## 443                         29.5 DISTRICT TOTAL                  1283
    ## 444                         29.3 DISTRICT TOTAL                   609
    ## 445                         29.7 DISTRICT TOTAL                   674
    ## 446                         26.1     ELEMENTARY                   361
    ## 447                         34.2 DISTRICT TOTAL                   325
    ## 448                         32.1 DISTRICT TOTAL                   161
    ## 449                         36.1 DISTRICT TOTAL                   164
    ## 450                         33.6     ELEMENTARY                   184
    ## 451                         29.8     ELEMENTARY                    92
    ## 452                         37.0     ELEMENTARY                    92
    ## 453                         36.2    MIDDLE/HIGH                   141
    ## 454                         34.9    MIDDLE/HIGH                    69
    ## 455                         37.4    MIDDLE/HIGH                    72
    ## 456                         23.9 DISTRICT TOTAL                   562
    ## 457                         23.7 DISTRICT TOTAL                   568
    ## 458                         23.0     ELEMENTARY                   687
    ## 459                         24.4     ELEMENTARY                   321
    ## 460                         23.8 DISTRICT TOTAL                  1130
    ## 461                         21.7     ELEMENTARY                   366
    ## 462                         25.0    MIDDLE/HIGH                   443
    ## 463                         23.3    MIDDLE/HIGH                   241
    ## 464                         26.9    MIDDLE/HIGH                   202
    ## 465                         33.0     ELEMENTARY                   560
    ## 466                         41.2    MIDDLE/HIGH                   445
    ## 467                         42.6    MIDDLE/HIGH                   229
    ## 468                         39.7    MIDDLE/HIGH                   216
    ## 469                         34.8 DISTRICT TOTAL                  1626
    ## 470                         34.7 DISTRICT TOTAL                   850
    ## 471                         35.0 DISTRICT TOTAL                   776
    ## 472                         32.1     ELEMENTARY                  1181
    ## 473                         31.2     ELEMENTARY                   621
    ## 474                         48.7 DISTRICT TOTAL                   157
    ## 475                         51.4 DISTRICT TOTAL                    72
    ## 476                         46.2 DISTRICT TOTAL                    85
    ## 477                         49.8     ELEMENTARY                   103
    ## 478                         55.7     ELEMENTARY                    43
    ## 479                         44.4     ELEMENTARY                    60
    ## 480                         46.5    MIDDLE/HIGH                    54
    ## 481                         43.1    MIDDLE/HIGH                    29
    ## 482                         50.0    MIDDLE/HIGH                    25
    ## 483                         47.0 DISTRICT TOTAL                   303
    ## 484                         47.3 DISTRICT TOTAL                   145
    ## 485                         46.8 DISTRICT TOTAL                   158
    ## 486                         43.4     ELEMENTARY                   220
    ## 487                         43.9     ELEMENTARY                   105
    ## 488                         43.1     ELEMENTARY                   115
    ## 489                         55.9    MIDDLE/HIGH                    83
    ## 490                         54.5    MIDDLE/HIGH                    40
    ## 491                         57.0    MIDDLE/HIGH                    43
    ## 492                         34.4 DISTRICT TOTAL                   228
    ## 493                         37.9 DISTRICT TOTAL                   105
    ## 494                         31.2 DISTRICT TOTAL                   123
    ## 495                         31.0     ELEMENTARY                   154
    ## 496                         32.4     ELEMENTARY                    71
    ## 497                         29.9     ELEMENTARY                    83
    ## 498                         40.8    MIDDLE/HIGH                    74
    ## 499                         46.9    MIDDLE/HIGH                    34
    ## 500                         34.4    MIDDLE/HIGH                    40
    ## 501                         38.8 DISTRICT TOTAL                   500
    ## 502                         41.2 DISTRICT TOTAL                   225
    ## 503                         36.7 DISTRICT TOTAL                   275
    ## 504                         35.5     ELEMENTARY                   324
    ## 505                         36.1     ELEMENTARY                   155
    ## 506                         34.9     ELEMENTARY                   169
    ## 507                         44.1    MIDDLE/HIGH                   176
    ## 508                         50.4    MIDDLE/HIGH                    70
    ## 509                         39.3    MIDDLE/HIGH                   106
    ## 510                         45.0 DISTRICT TOTAL                   202
    ## 511                         44.8 DISTRICT TOTAL                    96
    ## 512                         45.1 DISTRICT TOTAL                   106
    ## 513                         43.5     ELEMENTARY                   135
    ## 514                         43.5     ELEMENTARY                    61
    ## 515                         43.5     ELEMENTARY                    74
    ## 516                         47.7    MIDDLE/HIGH                    67
    ## 517                         47.0    MIDDLE/HIGH                    35
    ## 518                         48.4    MIDDLE/HIGH                    32
    ## 519                         41.6 DISTRICT TOTAL                   262
    ## 520                         39.1 DISTRICT TOTAL                   130
    ## 521                         43.9 DISTRICT TOTAL                   132
    ## 522                         40.3     ELEMENTARY                   156
    ## 523                         33.3     ELEMENTARY                    79
    ## 524                         46.5     ELEMENTARY                    77
    ## 525                         44.5    MIDDLE/HIGH                   106
    ## 526                         46.9    MIDDLE/HIGH                    51
    ## 527                         42.1    MIDDLE/HIGH                    55
    ## 528                         37.2 DISTRICT TOTAL                   410
    ## 529                         34.2 DISTRICT TOTAL                   214
    ## 530                         40.1 DISTRICT TOTAL                   196
    ## 531                         36.0     ELEMENTARY                   268
    ## 532                         31.2     ELEMENTARY                   150
    ## 533                         41.3     ELEMENTARY                   118
    ## 534                         40.1    MIDDLE/HIGH                   142
    ## 535                         40.2    MIDDLE/HIGH                    64
    ## 536                         40.0    MIDDLE/HIGH                    78
    ## 537                         33.7 DISTRICT TOTAL                   960
    ## 538                         32.9 DISTRICT TOTAL                   476
    ## 539                         34.5 DISTRICT TOTAL                   484
    ## 540                         31.3     ELEMENTARY                   695
    ## 541                         30.5     ELEMENTARY                   338
    ## 542                         32.1     ELEMENTARY                   357
    ## 543                         39.3    MIDDLE/HIGH                   265
    ## 544                         38.4    MIDDLE/HIGH                   138
    ## 545                         40.4    MIDDLE/HIGH                   127
    ## 546                         39.4 DISTRICT TOTAL                   161
    ## 547                         40.4 DISTRICT TOTAL                    87
    ## 548                         38.3 DISTRICT TOTAL                    74
    ## 549                         34.2     ELEMENTARY                    95
    ## 550                         35.1     ELEMENTARY                    48
    ## 551                         33.3     ELEMENTARY                    47
    ## 552                         45.9    MIDDLE/HIGH                    66
    ## 553                         45.8    MIDDLE/HIGH                    39
    ## 554                         46.0    MIDDLE/HIGH                    27
    ## 555                         35.9 DISTRICT TOTAL                   631
    ## 556                         34.8 DISTRICT TOTAL                   277
    ## 557                         36.8 DISTRICT TOTAL                   354
    ## 558                         29.6     ELEMENTARY                   411
    ## 559                         27.9     ELEMENTARY                   188
    ## 560                         30.9     ELEMENTARY                   223
    ## 561                         45.7    MIDDLE/HIGH                   220
    ## 562                         46.4    MIDDLE/HIGH                    89
    ## 563                         45.2    MIDDLE/HIGH                   131
    ## 564                         31.3 DISTRICT TOTAL                   184
    ## 565                         34.1 DISTRICT TOTAL                    78
    ## 566                         28.9 DISTRICT TOTAL                   106
    ## 567                         28.4     ELEMENTARY                   106
    ## 568                         33.9     ELEMENTARY                    41
    ## 569                         24.4     ELEMENTARY                    65
    ## 570                         36.1    MIDDLE/HIGH                    78
    ## 571                         37.3    MIDDLE/HIGH                    37
    ## 572                         34.9    MIDDLE/HIGH                    41
    ## 573                         43.6 DISTRICT TOTAL                   237
    ## 574                         43.6 DISTRICT TOTAL                   119
    ## 575                         43.5 DISTRICT TOTAL                   118
    ## 576                         39.9     ELEMENTARY                   168
    ## 577                         38.8     ELEMENTARY                    85
    ## 578                         41.1     ELEMENTARY                    83
    ## 579                         51.1    MIDDLE/HIGH                    69
    ## 580                         53.4    MIDDLE/HIGH                    34
    ## 581                         48.5    MIDDLE/HIGH                    35
    ## 582                         37.3 DISTRICT TOTAL                   386
    ## 583                         37.1 DISTRICT TOTAL                   178
    ## 584                         37.5 DISTRICT TOTAL                   208
    ## 585                         31.8     ELEMENTARY                   232
    ## 586                         30.3     ELEMENTARY                   106
    ## 587                         33.0     ELEMENTARY                   126
    ## 588                         44.0    MIDDLE/HIGH                   154
    ## 589                         45.0    MIDDLE/HIGH                    72
    ## 590                         43.1    MIDDLE/HIGH                    82
    ## 591                         25.2 DISTRICT TOTAL                   810
    ## 592                         26.0 DISTRICT TOTAL                   395
    ## 593                         24.3 DISTRICT TOTAL                   415
    ## 594                         23.1     ELEMENTARY                   501
    ## 595                         21.8     ELEMENTARY                   239
    ## 596                         24.2     ELEMENTARY                   262
    ## 597                         28.6    MIDDLE/HIGH                   309
    ## 598                         31.6    MIDDLE/HIGH                   156
    ## 599                         25.0    MIDDLE/HIGH                   153
    ## 600                         38.5 DISTRICT TOTAL                   771
    ## 601                         37.4 DISTRICT TOTAL                   380
    ## 602                         39.5 DISTRICT TOTAL                   391
    ## 603                         35.8     ELEMENTARY                   503
    ## 604                         34.8     ELEMENTARY                   252
    ## 605                         36.8     ELEMENTARY                   251
    ## 606                         43.0    MIDDLE/HIGH                   268
    ## 607                         41.8    MIDDLE/HIGH                   128
    ## 608                         44.2    MIDDLE/HIGH                   140
    ## 609                         32.1 DISTRICT TOTAL                   840
    ## 610                         31.1 DISTRICT TOTAL                   401
    ## 611                         33.0 DISTRICT TOTAL                   439
    ## 612                         33.0     ELEMENTARY                   495
    ## 613                         33.1     ELEMENTARY                   218
    ## 614                         32.9     ELEMENTARY                   277
    ## 615                         30.6    MIDDLE/HIGH                   345
    ## 616                         28.2    MIDDLE/HIGH                   183
    ## 617                         33.1    MIDDLE/HIGH                   162
    ## 618                         33.0 DISTRICT TOTAL                   975
    ## 619                         33.8 DISTRICT TOTAL                   475
    ## 620                         32.3 DISTRICT TOTAL                   500
    ## 621                         30.8     ELEMENTARY                   593
    ## 622                         33.5     ELEMENTARY                   272
    ## 623                         28.5     ELEMENTARY                   321
    ## 624                         36.4    MIDDLE/HIGH                   382
    ## 625                         34.7    MIDDLE/HIGH                   203
    ## 626                         38.1    MIDDLE/HIGH                   179
    ## 627                         33.6 DISTRICT TOTAL                   262
    ## 628                         36.9 DISTRICT TOTAL                   120
    ## 629                         30.6 DISTRICT TOTAL                   142
    ## 630                         29.7     ELEMENTARY                   180
    ## 631                         33.1     ELEMENTARY                    81
    ## 632                         26.7     ELEMENTARY                    99
    ## 633                         43.4    MIDDLE/HIGH                    82
    ## 634                         45.8    MIDDLE/HIGH                    39
    ## 635                         41.1    MIDDLE/HIGH                    43
    ## 636                         35.5 DISTRICT TOTAL                   919
    ## 637                         33.6 DISTRICT TOTAL                   446
    ## 638                         37.2 DISTRICT TOTAL                   473
    ## 639                         34.7     ELEMENTARY                   545
    ## 640                         29.9     ELEMENTARY                   272
    ## 641                         38.9     ELEMENTARY                   273
    ## 642                         36.6    MIDDLE/HIGH                   374
    ## 643                         38.6    MIDDLE/HIGH                   174
    ## 644                         34.8    MIDDLE/HIGH                   200
    ## 645                         33.3 DISTRICT TOTAL                  1164
    ## 646                         31.2 DISTRICT TOTAL                   583
    ## 647                         35.3 DISTRICT TOTAL                   581
    ## 648                         29.3     ELEMENTARY                   764
    ## 649                         28.4     ELEMENTARY                   374
    ## 650                         30.2     ELEMENTARY                   390
    ## 651                         40.0    MIDDLE/HIGH                   400
    ## 652                         35.8    MIDDLE/HIGH                   209
    ## 653                         43.9    MIDDLE/HIGH                   191
    ## 654                         33.1 DISTRICT TOTAL                   645
    ## 655                         30.5 DISTRICT TOTAL                   325
    ## 656                         35.5 DISTRICT TOTAL                   320
    ## 657                         29.7     ELEMENTARY                   421
    ## 658                         27.5     ELEMENTARY                   210
    ## 659                         31.8     ELEMENTARY                   211
    ## 660                         38.9    MIDDLE/HIGH                   224
    ## 661                         35.5    MIDDLE/HIGH                   115
    ## 662                         42.3    MIDDLE/HIGH                   109
    ## 663                         29.0 DISTRICT TOTAL                  2188
    ## 664                         29.0 DISTRICT TOTAL                  1095
    ## 665                         29.0 DISTRICT TOTAL                  1093
    ## 666                         26.2     ELEMENTARY                  1328
    ## 667                         26.4     ELEMENTARY                   664
    ## 668                         26.0     ELEMENTARY                   664
    ## 669                         33.0    MIDDLE/HIGH                   860
    ## 670                         32.8    MIDDLE/HIGH                   431
    ## 671                         33.2    MIDDLE/HIGH                   429
    ## 672                         42.2 DISTRICT TOTAL                   603
    ## 673                         43.0 DISTRICT TOTAL                   289
    ## 674                         41.4 DISTRICT TOTAL                   314
    ## 675                         41.2     ELEMENTARY                   443
    ## 676                         40.2     ELEMENTARY                   223
    ## 677                         42.2     ELEMENTARY                   220
    ## 678                         45.1    MIDDLE/HIGH                   160
    ## 679                         52.2    MIDDLE/HIGH                    66
    ## 680                         39.8    MIDDLE/HIGH                    94
    ## 681                         43.9 DISTRICT TOTAL                  1403
    ## 682                         43.7 DISTRICT TOTAL                   706
    ## 683                         44.0 DISTRICT TOTAL                   697
    ## 684                         43.9     ELEMENTARY                   871
    ## 685                         43.7     ELEMENTARY                   451
    ## 686                         44.1     ELEMENTARY                   420
    ## 687                         43.8    MIDDLE/HIGH                   532
    ## 688                         43.7    MIDDLE/HIGH                   255
    ## 689                         43.9    MIDDLE/HIGH                   277
    ## 690                         30.9 DISTRICT TOTAL                  1413
    ## 691                         29.2 DISTRICT TOTAL                   738
    ## 692                         32.6 DISTRICT TOTAL                   675
    ## 693                         26.6     ELEMENTARY                   805
    ## 694                         24.8     ELEMENTARY                   425
    ## 695                         28.4     ELEMENTARY                   380
    ## 696                         35.9    MIDDLE/HIGH                   608
    ## 697                         34.4    MIDDLE/HIGH                   313
    ## 698                         37.3    MIDDLE/HIGH                   295
    ## 699                         25.9 DISTRICT TOTAL                  1608
    ## 700                         24.6 DISTRICT TOTAL                   802
    ## 701                         27.1 DISTRICT TOTAL                   806
    ## 702                         22.6     ELEMENTARY                  1020
    ## 703                         21.7     ELEMENTARY                   511
    ## 704                         23.5     ELEMENTARY                   509
    ## 705                         31.1    MIDDLE/HIGH                   588
    ## 706                         29.5    MIDDLE/HIGH                   291
    ## 707                         32.6    MIDDLE/HIGH                   297
    ## 708                         31.3 DISTRICT TOTAL                   446
    ## 709                         30.7 DISTRICT TOTAL                   212
    ## 710                         31.9 DISTRICT TOTAL                   234
    ## 711                         28.2     ELEMENTARY                   233
    ## 712                         28.1     ELEMENTARY                   115
    ## 713                         28.3     ELEMENTARY                   118
    ## 714                         34.9    MIDDLE/HIGH                   213
    ## 715                         34.5    MIDDLE/HIGH                    97
    ## 716                         35.3    MIDDLE/HIGH                   116
    ## 717                         43.6 DISTRICT TOTAL                  1327
    ## 718                         42.5 DISTRICT TOTAL                   632
    ## 719                         44.5 DISTRICT TOTAL                   695
    ## 720                         44.7     ELEMENTARY                   899
    ## 721                         42.7     ELEMENTARY                   421
    ## 722                         46.4     ELEMENTARY                   478
    ## 723                         41.2    MIDDLE/HIGH                   428
    ## 724                         42.1    MIDDLE/HIGH                   211
    ## 725                         40.2    MIDDLE/HIGH                   217
    ## 726                         24.0 DISTRICT TOTAL                   343
    ## 727                         23.3 DISTRICT TOTAL                   163
    ## 728                         24.5 DISTRICT TOTAL                   180
    ## 729                         24.0     ELEMENTARY                   343
    ## 730                         23.3     ELEMENTARY                   163
    ## 731                         24.5     ELEMENTARY                   180
    ## 732                         39.8 DISTRICT TOTAL                   460
    ## 733                         35.6 DISTRICT TOTAL                   256
    ## 734                         44.2 DISTRICT TOTAL                   204
    ## 735                         36.9     ELEMENTARY                   345
    ## 736                         33.4     ELEMENTARY                   189
    ## 737                         40.5     ELEMENTARY                   156
    ## 738                         47.0    MIDDLE/HIGH                   115
    ## 739                         41.0    MIDDLE/HIGH                    67
    ## 740                         53.5    MIDDLE/HIGH                    48
    ## 741                         24.8 DISTRICT TOTAL                   945
    ## 742                         22.9 DISTRICT TOTAL                   453
    ## 743                         26.5 DISTRICT TOTAL                   492
    ## 744                         25.4     ELEMENTARY                   545
    ## 745                         24.0     ELEMENTARY                   263
    ## 746                         26.7     ELEMENTARY                   282
    ## 747                         24.0    MIDDLE/HIGH                   400
    ## 748                         21.4    MIDDLE/HIGH                   190
    ## 749                         26.3    MIDDLE/HIGH                   210
    ## 750                         28.0 DISTRICT TOTAL                   499
    ## 751                         28.6 DISTRICT TOTAL                   158
    ## 752                         27.7 DISTRICT TOTAL                   341
    ## 753                         28.0     ELEMENTARY                   499
    ## 754                         28.6     ELEMENTARY                   158
    ## 755                         27.7     ELEMENTARY                   341
    ## 756                         41.2 DISTRICT TOTAL                   357
    ## 757                         42.5 DISTRICT TOTAL                   176
    ## 758                         39.9 DISTRICT TOTAL                   181
    ## 759                         42.0     ELEMENTARY                   192
    ## 760                         39.4     ELEMENTARY                   106
    ## 761                         44.9     ELEMENTARY                    86
    ## 762                         40.6    MIDDLE/HIGH                   165
    ## 763                         46.8    MIDDLE/HIGH                    70
    ## 764                         34.5    MIDDLE/HIGH                    95
    ## 765                         25.5 DISTRICT TOTAL                   266
    ## 766                         21.8 DISTRICT TOTAL                   124
    ## 767                         28.3 DISTRICT TOTAL                   142
    ## 768                         25.5     ELEMENTARY                   266
    ## 769                         21.8     ELEMENTARY                   124
    ## 770                         28.3     ELEMENTARY                   142
    ## 771                         32.4 DISTRICT TOTAL                   252
    ## 772                         35.2 DISTRICT TOTAL                   116
    ## 773                         29.7 DISTRICT TOTAL                   136
    ## 774                         32.4     ELEMENTARY                   252
    ## 775                         35.2     ELEMENTARY                   116
    ## 776                         29.7     ELEMENTARY                   136
    ## 777                         22.9 DISTRICT TOTAL                   638
    ## 778                         20.7 DISTRICT TOTAL                   323
    ## 779                         24.9 DISTRICT TOTAL                   315
    ## 780                         21.2     ELEMENTARY                   362
    ## 781                         17.1     ELEMENTARY                   197
    ## 782                         25.8     ELEMENTARY                   165
    ## 783                         25.1    MIDDLE/HIGH                   276
    ## 784                         26.3    MIDDLE/HIGH                   126
    ## 785                         24.0    MIDDLE/HIGH                   150
    ## 786                         26.0 DISTRICT TOTAL                   481
    ## 787                         23.1 DISTRICT TOTAL                   248
    ## 788                         28.9 DISTRICT TOTAL                   233
    ## 789                         26.0     ELEMENTARY                   481
    ## 790                         23.1     ELEMENTARY                   248
    ## 791                         28.9     ELEMENTARY                   233
    ## 792                         22.4 DISTRICT TOTAL                  1813
    ## 793                         19.6 DISTRICT TOTAL                   925
    ## 794                         24.9 DISTRICT TOTAL                   888
    ## 795                         21.4     ELEMENTARY                  1122
    ## 796                         19.4     ELEMENTARY                   570
    ## 797                         23.3     ELEMENTARY                   552
    ## 798                         23.8    MIDDLE/HIGH                   691
    ## 799                         20.0    MIDDLE/HIGH                   355
    ## 800                         27.4    MIDDLE/HIGH                   336
    ## 801                         23.1 DISTRICT TOTAL                   607
    ## 802                         23.5 DISTRICT TOTAL                   294
    ## 803                         22.7 DISTRICT TOTAL                   313
    ## 804                         24.8     ELEMENTARY                   367
    ## 805                         25.8     ELEMENTARY                   168
    ## 806                         23.9     ELEMENTARY                   199
    ## 807                         20.3    MIDDLE/HIGH                   240
    ## 808                         20.3    MIDDLE/HIGH                   126
    ## 809                         20.4    MIDDLE/HIGH                   114
    ## 810                         26.1 DISTRICT TOTAL                  1148
    ## 811                         25.5 DISTRICT TOTAL                   545
    ## 812                         26.7 DISTRICT TOTAL                   603
    ## 813                         24.1     ELEMENTARY                   667
    ## 814                         22.3     ELEMENTARY                   310
    ## 815                         25.5     ELEMENTARY                   357
    ## 816                         28.8    MIDDLE/HIGH                   481
    ## 817                         29.4    MIDDLE/HIGH                   235
    ## 818                         28.3    MIDDLE/HIGH                   246
    ## 819                         25.0 DISTRICT TOTAL                   245
    ## 820                         23.8 DISTRICT TOTAL                   129
    ## 821                         26.3 DISTRICT TOTAL                   116
    ## 822                         24.9     ELEMENTARY                   133
    ## 823                         30.5     ELEMENTARY                    60
    ## 824                         23.4    MIDDLE/HIGH                   112
    ## 825                         25.3    MIDDLE/HIGH                    56
    ## 826                         21.4    MIDDLE/HIGH                    56
    ## 827                         20.1 DISTRICT TOTAL                   765
    ## 828                         17.1 DISTRICT TOTAL                   367
    ## 829                         22.6 DISTRICT TOTAL                   398
    ## 830                         14.3     ELEMENTARY                   422
    ## 831                         13.4     ELEMENTARY                   209
    ## 832                         15.2     ELEMENTARY                   213
    ## 833                         26.1    MIDDLE/HIGH                   343
    ## 834                         21.6    MIDDLE/HIGH                   158
    ## 835                         29.4    MIDDLE/HIGH                   185
    ## 836                         35.1 DISTRICT TOTAL                   962
    ## 837                         32.2 DISTRICT TOTAL                   481
    ## 838                         37.7 DISTRICT TOTAL                   481
    ## 839                         32.5     ELEMENTARY                   617
    ## 840                         30.0     ELEMENTARY                   299
    ## 841                         34.8     ELEMENTARY                   318
    ## 842                         39.3    MIDDLE/HIGH                   345
    ## 843                         35.7    MIDDLE/HIGH                   182
    ## 844                         42.8    MIDDLE/HIGH                   163
    ## 845                         25.6 DISTRICT TOTAL                  1525
    ## 846                         23.7 DISTRICT TOTAL                   771
    ## 847                         27.5 DISTRICT TOTAL                   754
    ## 848                         21.5     ELEMENTARY                   898
    ## 849                         20.1     ELEMENTARY                   429
    ## 850                         22.7     ELEMENTARY                   469
    ## 851                         30.9    MIDDLE/HIGH                   627
    ## 852                         27.8    MIDDLE/HIGH                   342
    ## 853                         34.2    MIDDLE/HIGH                   285
    ## 854                         28.9 DISTRICT TOTAL                   519
    ## 855                         25.7 DISTRICT TOTAL                   254
    ## 856                         31.6 DISTRICT TOTAL                   265
    ## 857                         25.9     ELEMENTARY                   341
    ## 858                         24.0     ELEMENTARY                   166
    ## 859                         27.6     ELEMENTARY                   175
    ## 860                         34.3    MIDDLE/HIGH                   178
    ## 861                         29.0    MIDDLE/HIGH                    88
    ## 862                         38.8    MIDDLE/HIGH                    90
    ## 863                         32.6 DISTRICT TOTAL                  1020
    ## 864                         31.9 DISTRICT TOTAL                   503
    ## 865                         33.2 DISTRICT TOTAL                   517
    ## 866                         30.6     ELEMENTARY                   663
    ## 867                         29.9     ELEMENTARY                   343
    ## 868                         31.4     ELEMENTARY                   320
    ## 869                         36.3    MIDDLE/HIGH                   357
    ## 870                         36.0    MIDDLE/HIGH                   160
    ## 871                         36.5    MIDDLE/HIGH                   197
    ## 872                         30.5 DISTRICT TOTAL                   517
    ## 873                         29.3 DISTRICT TOTAL                   266
    ## 874                         31.7 DISTRICT TOTAL                   251
    ## 875                         26.9     ELEMENTARY                   283
    ## 876                         24.0     ELEMENTARY                   150
    ## 877                         29.9     ELEMENTARY                   133
    ## 878                         34.5    MIDDLE/HIGH                   234
    ## 879                         35.6    MIDDLE/HIGH                   116
    ## 880                         33.5    MIDDLE/HIGH                   118
    ## 881                         35.8 DISTRICT TOTAL                   192
    ## 882                         38.9 DISTRICT TOTAL                    91
    ## 883                         32.9 DISTRICT TOTAL                   101
    ## 884                         32.8     ELEMENTARY                    92
    ## 885                         41.3     ELEMENTARY                    44
    ## 886                         37.9    MIDDLE/HIGH                   100
    ## 887                         36.5    MIDDLE/HIGH                    47
    ## 888                         39.1    MIDDLE/HIGH                    53
    ## 889                         33.1 DISTRICT TOTAL                   111
    ## 890                         38.7 DISTRICT TOTAL                    46
    ## 891                         28.6 DISTRICT TOTAL                    65
    ## 892                         33.6     ELEMENTARY                    71
    ## 893                         42.6     ELEMENTARY                    27
    ## 894                         26.7     ELEMENTARY                    44
    ## 895                         24.5    MIDDLE/HIGH                    40
    ## 896                         39.7 DISTRICT TOTAL                   219
    ## 897                         42.0 DISTRICT TOTAL                   110
    ## 898                         37.0 DISTRICT TOTAL                   109
    ## 899                         37.7     ELEMENTARY                   146
    ## 900                         39.7     ELEMENTARY                    73
    ## 901                         35.4     ELEMENTARY                    73
    ## 902                         43.4    MIDDLE/HIGH                    73
    ## 903                         46.4    MIDDLE/HIGH                    37
    ## 904                         40.0    MIDDLE/HIGH                    36
    ## 905                         37.8 DISTRICT TOTAL                  1215
    ## 906                         39.1 DISTRICT TOTAL                   596
    ## 907                         36.6 DISTRICT TOTAL                   619
    ## 908                         36.3     ELEMENTARY                   934
    ## 909                         37.1     ELEMENTARY                   465
    ## 910                         35.5     ELEMENTARY                   469
    ## 911                         42.3    MIDDLE/HIGH                   281
    ## 912                         45.2    MIDDLE/HIGH                   131
    ## 913                         39.6    MIDDLE/HIGH                   150
    ## 914                         31.2 DISTRICT TOTAL                   691
    ## 915                         27.6 DISTRICT TOTAL                   327
    ## 916                         34.1 DISTRICT TOTAL                   364
    ## 917                         30.9     ELEMENTARY                   381
    ## 918                         30.1     ELEMENTARY                   180
    ## 919                         31.7     ELEMENTARY                   201
    ## 920                         31.7    MIDDLE/HIGH                   310
    ## 921                         24.6    MIDDLE/HIGH                   147
    ## 922                         36.8    MIDDLE/HIGH                   163
    ## 923                         29.0 DISTRICT TOTAL                   682
    ## 924                         29.4 DISTRICT TOTAL                   341
    ## 925                         28.5 DISTRICT TOTAL                   341
    ## 926                         29.1     ELEMENTARY                   461
    ## 927                         29.6     ELEMENTARY                   229
    ## 928                         28.6     ELEMENTARY                   232
    ## 929                         28.7    MIDDLE/HIGH                   221
    ## 930                         29.1    MIDDLE/HIGH                   112
    ## 931                         28.2    MIDDLE/HIGH                   109
    ## 932                         27.8 DISTRICT TOTAL                   628
    ## 933                         27.1 DISTRICT TOTAL                   309
    ## 934                         28.5 DISTRICT TOTAL                   319
    ## 935                         26.5     ELEMENTARY                   340
    ## 936                         24.1     ELEMENTARY                   168
    ## 937                         28.7     ELEMENTARY                   172
    ## 938                         29.4    MIDDLE/HIGH                   288
    ## 939                         30.5    MIDDLE/HIGH                   141
    ## 940                         28.3    MIDDLE/HIGH                   147
    ## 941                         36.6 DISTRICT TOTAL                   135
    ## 942                         37.6 DISTRICT TOTAL                    63
    ## 943                         35.7 DISTRICT TOTAL                    72
    ## 944                         34.6     ELEMENTARY                    85
    ## 945                         41.0     ELEMENTARY                    36
    ## 946                         29.0     ELEMENTARY                    49
    ## 947                         39.8    MIDDLE/HIGH                    50
    ## 948                         32.5    MIDDLE/HIGH                    27
    ## 949                         46.5    MIDDLE/HIGH                    23
    ## 950                         38.0 DISTRICT TOTAL                   324
    ## 951                         42.1 DISTRICT TOTAL                   162
    ## 952                         33.3 DISTRICT TOTAL                   162
    ## 953                         34.8     ELEMENTARY                   199
    ## 954                         37.3     ELEMENTARY                   101
    ## 955                         32.2     ELEMENTARY                    98
    ## 956                         42.7    MIDDLE/HIGH                   125
    ## 957                         48.7    MIDDLE/HIGH                    61
    ## 958                         35.4    MIDDLE/HIGH                    64
    ## 959                         31.5 DISTRICT TOTAL                  1240
    ## 960                         32.8 DISTRICT TOTAL                   591
    ## 961                         30.2 DISTRICT TOTAL                   649
    ## 962                         29.9     ELEMENTARY                   757
    ## 963                         30.4     ELEMENTARY                   359
    ## 964                         29.5     ELEMENTARY                   398
    ## 965                         33.8    MIDDLE/HIGH                   483
    ## 966                         36.3    MIDDLE/HIGH                   232
    ## 967                         31.4    MIDDLE/HIGH                   251
    ## 968                         23.8 DISTRICT TOTAL                   898
    ## 969                         23.2 DISTRICT TOTAL                   442
    ## 970                         24.4 DISTRICT TOTAL                   456
    ## 971                         22.1     ELEMENTARY                   537
    ## 972                         22.9     ELEMENTARY                   256
    ## 973                         21.4     ELEMENTARY                   281
    ## 974                         26.2    MIDDLE/HIGH                   361
    ## 975                         23.5    MIDDLE/HIGH                   186
    ## 976                         28.9    MIDDLE/HIGH                   175
    ## 977                         40.5 DISTRICT TOTAL                   167
    ## 978                         44.2 DISTRICT TOTAL                    77
    ## 979                         37.3 DISTRICT TOTAL                    90
    ## 980                         39.0     ELEMENTARY                   100
    ## 981                         39.2     ELEMENTARY                    48
    ## 982                         38.7     ELEMENTARY                    52
    ## 983                         43.2    MIDDLE/HIGH                    67
    ## 984                         50.8    MIDDLE/HIGH                    29
    ## 985                         35.6    MIDDLE/HIGH                    38
    ## 986                         39.4 DISTRICT TOTAL                  3496
    ## 987                         40.7 DISTRICT TOTAL                  1627
    ## 988                         38.3 DISTRICT TOTAL                  1869
    ## 989                         36.5     ELEMENTARY                  2504
    ## 990                         36.4     ELEMENTARY                  1191
    ## 991                         36.7     ELEMENTARY                  1313
    ## 992                         45.8    MIDDLE/HIGH                   992
    ## 993                         50.1    MIDDLE/HIGH                   436
    ## 994                         42.0    MIDDLE/HIGH                   556
    ## 995                         33.7 DISTRICT TOTAL                   183
    ## 996                         39.6 DISTRICT TOTAL                    81
    ## 997                         28.2 DISTRICT TOTAL                   102
    ## 998                         25.8     ELEMENTARY                   138
    ## 999                         31.1     ELEMENTARY                    62
    ## 1000                        20.8     ELEMENTARY                    76
    ## 1001                        50.0    MIDDLE/HIGH                    45
    ## 1002                        56.8    MIDDLE/HIGH                    19
    ## 1003                        43.5    MIDDLE/HIGH                    26
    ## 1004                        40.1 DISTRICT TOTAL                   416
    ## 1005                        41.8 DISTRICT TOTAL                   215
    ## 1006                        38.3 DISTRICT TOTAL                   201
    ## 1007                        36.6     ELEMENTARY                   261
    ## 1008                        38.0     ELEMENTARY                   139
    ## 1009                        34.8     ELEMENTARY                   122
    ## 1010                        46.4    MIDDLE/HIGH                   155
    ## 1011                        48.3    MIDDLE/HIGH                    76
    ## 1012                        44.4    MIDDLE/HIGH                    79
    ## 1013                        38.6 DISTRICT TOTAL                   214
    ## 1014                        44.3 DISTRICT TOTAL                    97
    ## 1015                        33.2 DISTRICT TOTAL                   117
    ## 1016                        36.4     ELEMENTARY                   134
    ## 1017                        35.5     ELEMENTARY                    65
    ## 1018                        37.3     ELEMENTARY                    69
    ## 1019                        41.8    MIDDLE/HIGH                    80
    ## 1020                        57.3    MIDDLE/HIGH                    32
    ## 1021                        27.7    MIDDLE/HIGH                    48
    ## 1022                        42.2 DISTRICT TOTAL                   159
    ## 1023                        44.4 DISTRICT TOTAL                    70
    ## 1024                        40.3 DISTRICT TOTAL                    89
    ## 1025                        40.1     ELEMENTARY                    97
    ## 1026                        45.0     ELEMENTARY                    44
    ## 1027                        35.4     ELEMENTARY                    53
    ## 1028                        45.1    MIDDLE/HIGH                    62
    ## 1029                        43.5    MIDDLE/HIGH                    26
    ## 1030                        46.3    MIDDLE/HIGH                    36
    ## 1031                        23.2 DISTRICT TOTAL                  1296
    ## 1032                        20.5 DISTRICT TOTAL                   690
    ## 1033                        25.9 DISTRICT TOTAL                   606
    ## 1034                        22.4     ELEMENTARY                   821
    ## 1035                        22.1     ELEMENTARY                   424
    ## 1036                        22.8     ELEMENTARY                   397
    ## 1037                        24.4    MIDDLE/HIGH                   475
    ## 1038                        17.8    MIDDLE/HIGH                   266
    ## 1039                        31.2    MIDDLE/HIGH                   209
    ## 1040                        32.3 DISTRICT TOTAL                   814
    ## 1041                        31.7 DISTRICT TOTAL                   400
    ## 1042                        32.9 DISTRICT TOTAL                   414
    ## 1043                        28.1     ELEMENTARY                   449
    ## 1044                        28.5     ELEMENTARY                   223
    ## 1045                        27.7     ELEMENTARY                   226
    ## 1046                        37.0    MIDDLE/HIGH                   365
    ## 1047                        35.9    MIDDLE/HIGH                   177
    ## 1048                        38.0    MIDDLE/HIGH                   188
    ## 1049                        31.3 DISTRICT TOTAL                   566
    ## 1050                        29.4 DISTRICT TOTAL                   270
    ## 1051                        33.0 DISTRICT TOTAL                   296
    ## 1052                        26.6     ELEMENTARY                   328
    ## 1053                        26.7     ELEMENTARY                   150
    ## 1054                        26.5     ELEMENTARY                   178
    ## 1055                        37.9    MIDDLE/HIGH                   238
    ## 1056                        33.3    MIDDLE/HIGH                   120
    ## 1057                        41.9    MIDDLE/HIGH                   118
    ## 1058                        29.8 DISTRICT TOTAL                  1310
    ## 1059                        27.8 DISTRICT TOTAL                   653
    ## 1060                        31.7 DISTRICT TOTAL                   657
    ## 1061                        26.7     ELEMENTARY                   788
    ## 1062                        23.3     ELEMENTARY                   406
    ## 1063                        30.0     ELEMENTARY                   382
    ## 1064                        34.0    MIDDLE/HIGH                   522
    ## 1065                        34.0    MIDDLE/HIGH                   247
    ## 1066                        34.0    MIDDLE/HIGH                   275
    ## 1067                        45.6 DISTRICT TOTAL                  2218
    ## 1068                        44.5 DISTRICT TOTAL                  1088
    ## 1069                        46.6 DISTRICT TOTAL                  1130
    ## 1070                        44.8     ELEMENTARY                  1454
    ## 1071                        42.3     ELEMENTARY                   750
    ## 1072                        47.3     ELEMENTARY                   704
    ## 1073                        46.9    MIDDLE/HIGH                   764
    ## 1074                        48.8    MIDDLE/HIGH                   338
    ## 1075                        45.2    MIDDLE/HIGH                   426
    ## 1076                        25.3 DISTRICT TOTAL                  1251
    ## 1077                        24.7 DISTRICT TOTAL                   623
    ## 1078                        25.9 DISTRICT TOTAL                   628
    ## 1079                        23.0     ELEMENTARY                   870
    ## 1080                        23.0     ELEMENTARY                   432
    ## 1081                        23.0     ELEMENTARY                   438
    ## 1082                        30.2    MIDDLE/HIGH                   381
    ## 1083                        28.4    MIDDLE/HIGH                   191
    ## 1084                        31.9    MIDDLE/HIGH                   190
    ## 1085                        40.2 DISTRICT TOTAL                   145
    ## 1086                        41.3 DISTRICT TOTAL                    71
    ## 1087                        39.2 DISTRICT TOTAL                    74
    ## 1088                        41.6     ELEMENTARY                    87
    ## 1089                        39.5     ELEMENTARY                    46
    ## 1090                        43.8     ELEMENTARY                    41
    ## 1091                        40.2    MIDDLE/HIGH                    58
    ## 1092                        44.4    MIDDLE/HIGH                    25
    ## 1093                        36.5    MIDDLE/HIGH                    33
    ## 1094                        42.3 DISTRICT TOTAL                   225
    ## 1095                        43.7 DISTRICT TOTAL                   111
    ## 1096                        41.0 DISTRICT TOTAL                   114
    ## 1097                        35.7     ELEMENTARY                   155
    ## 1098                        38.8     ELEMENTARY                    74
    ## 1099                        32.8     ELEMENTARY                    81
    ## 1100                        53.6    MIDDLE/HIGH                    70
    ## 1101                        51.3    MIDDLE/HIGH                    37
    ## 1102                        56.0    MIDDLE/HIGH                    33
    ## 1103                        47.4 DISTRICT TOTAL                   231
    ## 1104                        48.9 DISTRICT TOTAL                   121
    ## 1105                        45.5 DISTRICT TOTAL                   110
    ## 1106                        44.0     ELEMENTARY                   158
    ## 1107                        43.9     ELEMENTARY                    83
    ## 1108                        44.0     ELEMENTARY                    75
    ## 1109                        53.5    MIDDLE/HIGH                    73
    ## 1110                        57.3    MIDDLE/HIGH                    38
    ## 1111                        48.5    MIDDLE/HIGH                    35
    ## 1112                        38.3 DISTRICT TOTAL                   667
    ## 1113                        37.0 DISTRICT TOTAL                   318
    ## 1114                        39.5 DISTRICT TOTAL                   349
    ## 1115                        35.9     ELEMENTARY                   461
    ## 1116                        35.9     ELEMENTARY                   215
    ## 1117                        35.9     ELEMENTARY                   246
    ## 1118                        43.6    MIDDLE/HIGH                   206
    ## 1119                        39.4    MIDDLE/HIGH                   103
    ## 1120                        47.2    MIDDLE/HIGH                   103
    ## 1121                        46.3 DISTRICT TOTAL                   248
    ## 1122                        47.9 DISTRICT TOTAL                   111
    ## 1123                        45.0 DISTRICT TOTAL                   137
    ## 1124                        45.0     ELEMENTARY                   142
    ## 1125                        47.0     ELEMENTARY                    61
    ## 1126                        43.4     ELEMENTARY                    81
    ## 1127                        48.0    MIDDLE/HIGH                   106
    ## 1128                        49.0    MIDDLE/HIGH                    50
    ## 1129                        47.2    MIDDLE/HIGH                    56
    ## 1130                        44.4 DISTRICT TOTAL                   160
    ## 1131                        44.2 DISTRICT TOTAL                    72
    ## 1132                        44.7 DISTRICT TOTAL                    88
    ## 1133                        40.4     ELEMENTARY                   112
    ## 1134                        40.7     ELEMENTARY                    48
    ## 1135                        40.2     ELEMENTARY                    64
    ## 1136                        52.0    MIDDLE/HIGH                    48
    ## 1137                        50.0    MIDDLE/HIGH                    24
    ## 1138                        53.8    MIDDLE/HIGH                    24
    ## 1139                        34.9 DISTRICT TOTAL                   419
    ## 1140                        39.0 DISTRICT TOTAL                   189
    ## 1141                        30.9 DISTRICT TOTAL                   230
    ## 1142                        30.0     ELEMENTARY                   289
    ## 1143                        35.2     ELEMENTARY                   120
    ## 1144                        25.6     ELEMENTARY                   169
    ## 1145                        44.0    MIDDLE/HIGH                   130
    ## 1146                        45.2    MIDDLE/HIGH                    69
    ## 1147                        42.5    MIDDLE/HIGH                    61
    ## 1148                        30.4 DISTRICT TOTAL                   438
    ## 1149                        31.6 DISTRICT TOTAL                   203
    ## 1150                        29.4 DISTRICT TOTAL                   235
    ## 1151                        29.2     ELEMENTARY                   261
    ## 1152                        33.5     ELEMENTARY                   113
    ## 1153                        25.4     ELEMENTARY                   148
    ## 1154                        32.7    MIDDLE/HIGH                   177
    ## 1155                        29.1    MIDDLE/HIGH                    90
    ## 1156                        36.0    MIDDLE/HIGH                    87
    ## 1157                        25.2 DISTRICT TOTAL                   227
    ## 1158                        25.2 DISTRICT TOTAL                   114
    ## 1159                        25.3 DISTRICT TOTAL                   113
    ## 1160                        23.2     ELEMENTARY                   126
    ## 1161                        26.3     ELEMENTARY                    56
    ## 1162                        20.5     ELEMENTARY                    70
    ## 1163                        29.4    MIDDLE/HIGH                   101
    ## 1164                        25.6    MIDDLE/HIGH                    58
    ## 1165                        33.8    MIDDLE/HIGH                    43
    ## 1166                        38.7 DISTRICT TOTAL                   700
    ## 1167                        36.1 DISTRICT TOTAL                   334
    ## 1168                        41.0 DISTRICT TOTAL                   366
    ## 1169                        35.4     ELEMENTARY                   425
    ## 1170                        33.2     ELEMENTARY                   199
    ## 1171                        37.3     ELEMENTARY                   226
    ## 1172                        43.3    MIDDLE/HIGH                   275
    ## 1173                        40.0    MIDDLE/HIGH                   135
    ## 1174                        46.3    MIDDLE/HIGH                   140
    ## 1175                        15.1 DISTRICT TOTAL                   205
    ## 1176                        18.3 DISTRICT TOTAL                    97
    ## 1177                        12.1 DISTRICT TOTAL                   108
    ## 1178                         9.6     ELEMENTARY                    98
    ## 1179                        18.6     ELEMENTARY                    48
    ## 1180                        16.4    MIDDLE/HIGH                   107
    ## 1181                        15.9    MIDDLE/HIGH                    58
    ## 1182                        37.1 DISTRICT TOTAL                   666
    ## 1183                        36.8 DISTRICT TOTAL                   318
    ## 1184                        37.4 DISTRICT TOTAL                   348
    ## 1185                        34.1     ELEMENTARY                   396
    ## 1186                        30.8     ELEMENTARY                   201
    ## 1187                        37.1     ELEMENTARY                   195
    ## 1188                        41.5    MIDDLE/HIGH                   270
    ## 1189                        45.0    MIDDLE/HIGH                   117
    ## 1190                        38.3    MIDDLE/HIGH                   153
    ## 1191                        36.0 DISTRICT TOTAL                   889
    ## 1192                        37.3 DISTRICT TOTAL                   432
    ## 1193                        34.6 DISTRICT TOTAL                   457
    ## 1194                        34.1     ELEMENTARY                   521
    ## 1195                        35.3     ELEMENTARY                   258
    ## 1196                        32.7     ELEMENTARY                   263
    ## 1197                        38.5    MIDDLE/HIGH                   368
    ## 1198                        40.0    MIDDLE/HIGH                   174
    ## 1199                        37.0    MIDDLE/HIGH                   194
    ## 1200                        29.9 DISTRICT TOTAL                   584
    ## 1201                        31.3 DISTRICT TOTAL                   272
    ## 1202                        28.6 DISTRICT TOTAL                   312
    ## 1203                        29.3     ELEMENTARY                   328
    ## 1204                        29.9     ELEMENTARY                   154
    ## 1205                        28.7     ELEMENTARY                   174
    ## 1206                        30.6    MIDDLE/HIGH                   256
    ## 1207                        33.2    MIDDLE/HIGH                   118
    ## 1208                        28.4    MIDDLE/HIGH                   138
    ## 1209                        34.9 DISTRICT TOTAL                   726
    ## 1210                        40.8 DISTRICT TOTAL                   300
    ## 1211                        29.9 DISTRICT TOTAL                   426
    ## 1212                        37.1     ELEMENTARY                   413
    ## 1213                        45.4     ELEMENTARY                   158
    ## 1214                        30.3     ELEMENTARY                   255
    ## 1215                        31.8    MIDDLE/HIGH                   313
    ## 1216                        34.5    MIDDLE/HIGH                   142
    ## 1217                        29.3    MIDDLE/HIGH                   171
    ## 1218                        30.8 DISTRICT TOTAL                   444
    ## 1219                        31.9 DISTRICT TOTAL                   223
    ## 1220                        29.7 DISTRICT TOTAL                   221
    ## 1221                        30.9     ELEMENTARY                   271
    ## 1222                        30.6     ELEMENTARY                   137
    ## 1223                        31.3     ELEMENTARY                   134
    ## 1224                        31.4    MIDDLE/HIGH                   173
    ## 1225                        34.8    MIDDLE/HIGH                    86
    ## 1226                        27.9    MIDDLE/HIGH                    87
    ## 1227                        37.1 DISTRICT TOTAL                  1864
    ## 1228                        36.3 DISTRICT TOTAL                   912
    ## 1229                        37.9 DISTRICT TOTAL                   952
    ## 1230                        36.2     ELEMENTARY                  1118
    ## 1231                        34.8     ELEMENTARY                   546
    ## 1232                        37.4     ELEMENTARY                   572
    ## 1233                        38.6    MIDDLE/HIGH                   746
    ## 1234                        38.4    MIDDLE/HIGH                   366
    ## 1235                        38.7    MIDDLE/HIGH                   380
    ## 1236                        30.0 DISTRICT TOTAL                   693
    ## 1237                        29.1 DISTRICT TOTAL                   344
    ## 1238                        30.8 DISTRICT TOTAL                   349
    ## 1239                        26.2     ELEMENTARY                   383
    ## 1240                        29.1     ELEMENTARY                   181
    ## 1241                        23.4     ELEMENTARY                   202
    ## 1242                        34.1    MIDDLE/HIGH                   310
    ## 1243                        29.2    MIDDLE/HIGH                   163
    ## 1244                        38.8    MIDDLE/HIGH                   147
    ## 1245                        25.8 DISTRICT TOTAL                   568
    ## 1246                        22.0 DISTRICT TOTAL                   298
    ## 1247                        29.7 DISTRICT TOTAL                   270
    ## 1248                        24.7     ELEMENTARY                   320
    ## 1249                        22.4     ELEMENTARY                   154
    ## 1250                        26.7     ELEMENTARY                   166
    ## 1251                        27.2    MIDDLE/HIGH                   248
    ## 1252                        21.5    MIDDLE/HIGH                   144
    ## 1253                        33.9    MIDDLE/HIGH                   104
    ## 1254                        39.8 DISTRICT TOTAL                    62
    ## 1255                        35.6 DISTRICT TOTAL                    38
    ## 1256                        45.5 DISTRICT TOTAL                    24
    ## 1257                        30.6     ELEMENTARY                    43
    ## 1258                        36.1     ELEMENTARY                    23
    ## 1259                        49.2 DISTRICT TOTAL                   186
    ## 1260                        50.0 DISTRICT TOTAL                    88
    ## 1261                        48.4 DISTRICT TOTAL                    98
    ## 1262                        44.7     ELEMENTARY                   119
    ## 1263                        43.4     ELEMENTARY                    60
    ## 1264                        45.9     ELEMENTARY                    59
    ## 1265                        55.6    MIDDLE/HIGH                    67
    ## 1266                        60.0    MIDDLE/HIGH                    28
    ## 1267                        51.9    MIDDLE/HIGH                    39
    ## 1268                        33.3 DISTRICT TOTAL                   261
    ## 1269                        34.9 DISTRICT TOTAL                   127
    ## 1270                        31.6 DISTRICT TOTAL                   134
    ## 1271                        30.6     ELEMENTARY                   162
    ## 1272                        31.4     ELEMENTARY                    78
    ## 1273                        29.9     ELEMENTARY                    84
    ## 1274                        38.9    MIDDLE/HIGH                    99
    ## 1275                        41.7    MIDDLE/HIGH                    49
    ## 1276                        35.9    MIDDLE/HIGH                    50
    ## 1277                        33.1 DISTRICT TOTAL                   726
    ## 1278                        36.1 DISTRICT TOTAL                   332
    ## 1279                        30.4 DISTRICT TOTAL                   394
    ## 1280                        31.6     ELEMENTARY                   485
    ## 1281                        32.8     ELEMENTARY                   220
    ## 1282                        30.6     ELEMENTARY                   265
    ## 1283                        35.8    MIDDLE/HIGH                   241
    ## 1284                        41.7    MIDDLE/HIGH                   112
    ## 1285                        30.0    MIDDLE/HIGH                   129
    ## 1286                        25.3 DISTRICT TOTAL                  1622
    ## 1287                        25.0 DISTRICT TOTAL                   814
    ## 1288                        25.7 DISTRICT TOTAL                   808
    ## 1289                        23.3     ELEMENTARY                   964
    ## 1290                        21.6     ELEMENTARY                   485
    ## 1291                        25.0     ELEMENTARY                   479
    ## 1292                        28.1    MIDDLE/HIGH                   658
    ## 1293                        29.6    MIDDLE/HIGH                   329
    ## 1294                        26.6    MIDDLE/HIGH                   329
    ## 1295                        23.4 DISTRICT TOTAL                   938
    ## 1296                        21.1 DISTRICT TOTAL                   474
    ## 1297                        25.6 DISTRICT TOTAL                   464
    ## 1298                        23.1     ELEMENTARY                   558
    ## 1299                        19.4     ELEMENTARY                   277
    ## 1300                        26.4     ELEMENTARY                   281
    ## 1301                        23.9    MIDDLE/HIGH                   380
    ## 1302                        23.4    MIDDLE/HIGH                   197
    ## 1303                        24.4    MIDDLE/HIGH                   183
    ## 1304                        39.2 DISTRICT TOTAL                   549
    ## 1305                        40.9 DISTRICT TOTAL                   251
    ## 1306                        37.8 DISTRICT TOTAL                   298
    ## 1307                        33.2     ELEMENTARY                   347
    ## 1308                        36.4     ELEMENTARY                   152
    ## 1309                        30.4     ELEMENTARY                   195
    ## 1310                        47.5    MIDDLE/HIGH                   202
    ## 1311                        46.7    MIDDLE/HIGH                    99
    ## 1312                        48.3    MIDDLE/HIGH                   103
    ## 1313                        23.7 DISTRICT TOTAL                   167
    ## 1314                        24.3 DISTRICT TOTAL                    87
    ## 1315                        23.1 DISTRICT TOTAL                    80
    ## 1316                        24.7     ELEMENTARY                   125
    ## 1317                        23.3     ELEMENTARY                    66
    ## 1318                        26.3     ELEMENTARY                    59
    ## 1319                        41.7 DISTRICT TOTAL                   226
    ## 1320                        41.5 DISTRICT TOTAL                   107
    ## 1321                        41.9 DISTRICT TOTAL                   119
    ## 1322                        39.4     ELEMENTARY                   131
    ## 1323                        46.9     ELEMENTARY                    52
    ## 1324                        33.6     ELEMENTARY                    79
    ## 1325                        44.8    MIDDLE/HIGH                    95
    ## 1326                        35.3    MIDDLE/HIGH                    55
    ## 1327                        54.0    MIDDLE/HIGH                    40
    ## 1328                        41.2 DISTRICT TOTAL                   338
    ## 1329                        40.6 DISTRICT TOTAL                   165
    ## 1330                        41.7 DISTRICT TOTAL                   173
    ## 1331                        38.9     ELEMENTARY                   221
    ## 1332                        39.9     ELEMENTARY                   104
    ## 1333                        38.0     ELEMENTARY                   117
    ## 1334                        45.3    MIDDLE/HIGH                   117
    ## 1335                        41.9    MIDDLE/HIGH                    61
    ## 1336                        48.6    MIDDLE/HIGH                    56
    ## 1337                        37.7 DISTRICT TOTAL                   336
    ## 1338                        37.5 DISTRICT TOTAL                   157
    ## 1339                        37.9 DISTRICT TOTAL                   179
    ## 1340                        33.5     ELEMENTARY                   202
    ## 1341                        34.8     ELEMENTARY                   101
    ## 1342                        32.3     ELEMENTARY                   101
    ## 1343                        44.2    MIDDLE/HIGH                   134
    ## 1344                        42.9    MIDDLE/HIGH                    56
    ## 1345                        45.1    MIDDLE/HIGH                    78
    ## 1346                        30.5 DISTRICT TOTAL                   105
    ## 1347                        27.3 DISTRICT TOTAL                    56
    ## 1348                        33.8 DISTRICT TOTAL                    49
    ## 1349                        26.7     ELEMENTARY                    63
    ## 1350                        35.7     ELEMENTARY                    27
    ## 1351                        25.0    MIDDLE/HIGH                    39
    ## 1352                        44.6 DISTRICT TOTAL                    77
    ## 1353                        52.1 DISTRICT TOTAL                    34
    ## 1354                        36.8 DISTRICT TOTAL                    43
    ## 1355                        35.6     ELEMENTARY                    56
    ## 1356                        44.4     ELEMENTARY                    25
    ## 1357                        58.5    MIDDLE/HIGH                    17
    ## 1358                        47.8    MIDDLE/HIGH                    12
    ## 1359                        36.9 DISTRICT TOTAL                    70
    ## 1360                        31.4 DISTRICT TOTAL                    35
    ## 1361                        41.7 DISTRICT TOTAL                    35
    ## 1362                        32.6     ELEMENTARY                    29
    ## 1363                        45.2     ELEMENTARY                    17
    ## 1364                        31.7    MIDDLE/HIGH                    41
    ## 1365                        34.3    MIDDLE/HIGH                    23
    ## 1366                        28.6 DISTRICT TOTAL                   375
    ## 1367                        29.6 DISTRICT TOTAL                   177
    ## 1368                        27.6 DISTRICT TOTAL                   198
    ## 1369                        22.6     ELEMENTARY                   228
    ## 1370                        20.1     ELEMENTARY                   106
    ## 1371                        24.7     ELEMENTARY                   122
    ## 1372                        36.4    MIDDLE/HIGH                   147
    ## 1373                        40.8    MIDDLE/HIGH                    71
    ## 1374                        32.0    MIDDLE/HIGH                    76
    ## 1375                        37.9 DISTRICT TOTAL                   805
    ## 1376                        38.5 DISTRICT TOTAL                   398
    ## 1377                        37.2 DISTRICT TOTAL                   407
    ## 1378                        34.2     ELEMENTARY                   521
    ## 1379                        34.6     ELEMENTARY                   259
    ## 1380                        33.7     ELEMENTARY                   262
    ## 1381                        43.8    MIDDLE/HIGH                   284
    ## 1382                        44.7    MIDDLE/HIGH                   139
    ## 1383                        42.7    MIDDLE/HIGH                   145
    ## 1384                        41.0 DISTRICT TOTAL                   621
    ## 1385                        42.4 DISTRICT TOTAL                   285
    ## 1386                        39.7 DISTRICT TOTAL                   336
    ## 1387                        40.5     ELEMENTARY                   421
    ## 1388                        41.6     ELEMENTARY                   193
    ## 1389                        39.5     ELEMENTARY                   228
    ## 1390                        41.9    MIDDLE/HIGH                   200
    ## 1391                        44.0    MIDDLE/HIGH                    92
    ## 1392                        39.9    MIDDLE/HIGH                   108
    ## 1393                        35.9 DISTRICT TOTAL                   679
    ## 1394                        35.9 DISTRICT TOTAL                   331
    ## 1395                        35.9 DISTRICT TOTAL                   348
    ## 1396                        34.3     ELEMENTARY                   449
    ## 1397                        34.7     ELEMENTARY                   225
    ## 1398                        34.0     ELEMENTARY                   224
    ## 1399                        38.8    MIDDLE/HIGH                   230
    ## 1400                        38.3    MIDDLE/HIGH                   106
    ## 1401                        39.2    MIDDLE/HIGH                   124
    ## 1402                        38.1 DISTRICT TOTAL                   632
    ## 1403                        37.3 DISTRICT TOTAL                   336
    ## 1404                        39.0 DISTRICT TOTAL                   296
    ## 1405                        34.8     ELEMENTARY                   412
    ## 1406                        36.1     ELEMENTARY                   215
    ## 1407                        33.2     ELEMENTARY                   197
    ## 1408                        43.9    MIDDLE/HIGH                   220
    ## 1409                        39.8    MIDDLE/HIGH                   121
    ## 1410                        48.0    MIDDLE/HIGH                    99
    ## 1411                        36.5 DISTRICT TOTAL                  2558
    ## 1412                        34.3 DISTRICT TOTAL                  1301
    ## 1413                        38.7 DISTRICT TOTAL                  1257
    ## 1414                        34.7     ELEMENTARY                  1617
    ## 1415                        33.9     ELEMENTARY                   798
    ## 1416                        35.4     ELEMENTARY                   819
    ## 1417                        39.5    MIDDLE/HIGH                   941
    ## 1418                        34.9    MIDDLE/HIGH                   503
    ## 1419                        44.0    MIDDLE/HIGH                   438
    ## 1420                        24.8 DISTRICT TOTAL                   265
    ## 1421                        22.3 DISTRICT TOTAL                   134
    ## 1422                        27.1 DISTRICT TOTAL                   131
    ## 1423                        20.6     ELEMENTARY                   162
    ## 1424                        18.5     ELEMENTARY                    88
    ## 1425                        22.9     ELEMENTARY                    74
    ## 1426                        32.2    MIDDLE/HIGH                   103
    ## 1427                        30.3    MIDDLE/HIGH                    46
    ## 1428                        33.7    MIDDLE/HIGH                    57
    ## 1429                        27.3 DISTRICT TOTAL                   556
    ## 1430                        26.3 DISTRICT TOTAL                   257
    ## 1431                        28.0 DISTRICT TOTAL                   299
    ## 1432                        26.4     ELEMENTARY                   313
    ## 1433                        24.9     ELEMENTARY                   138
    ## 1434                        27.5     ELEMENTARY                   175
    ## 1435                        28.4    MIDDLE/HIGH                   243
    ## 1436                        27.9    MIDDLE/HIGH                   119
    ## 1437                        28.8    MIDDLE/HIGH                   124
    ## 1438                        35.1 DISTRICT TOTAL                  1711
    ## 1439                        32.2 DISTRICT TOTAL                   867
    ## 1440                        37.8 DISTRICT TOTAL                   844
    ## 1441                        32.1     ELEMENTARY                   987
    ## 1442                        30.8     ELEMENTARY                   506
    ## 1443                        33.5     ELEMENTARY                   481
    ## 1444                        38.6    MIDDLE/HIGH                   724
    ## 1445                        34.0    MIDDLE/HIGH                   361
    ## 1446                        42.4    MIDDLE/HIGH                   363
    ## 1447                        40.8 DISTRICT TOTAL                  1580
    ## 1448                        38.9 DISTRICT TOTAL                   807
    ## 1449                        42.6 DISTRICT TOTAL                   773
    ## 1450                        36.3     ELEMENTARY                  1058
    ## 1451                        34.3     ELEMENTARY                   530
    ## 1452                        38.1     ELEMENTARY                   528
    ## 1453                        48.2    MIDDLE/HIGH                   522
    ## 1454                        46.0    MIDDLE/HIGH                   277
    ## 1455                        50.5    MIDDLE/HIGH                   245
    ## 1456                        43.5 DISTRICT TOTAL                  1817
    ## 1457                        42.9 DISTRICT TOTAL                   896
    ## 1458                        44.1 DISTRICT TOTAL                   921
    ## 1459                        42.6     ELEMENTARY                  1091
    ## 1460                        40.0     ELEMENTARY                   537
    ## 1461                        44.8     ELEMENTARY                   554
    ## 1462                        44.8    MIDDLE/HIGH                   726
    ## 1463                        46.7    MIDDLE/HIGH                   359
    ## 1464                        42.9    MIDDLE/HIGH                   367
    ## 1465                        34.6 DISTRICT TOTAL                   340
    ## 1466                        30.6 DISTRICT TOTAL                   187
    ## 1467                        38.9 DISTRICT TOTAL                   153
    ## 1468                        32.7     ELEMENTARY                   193
    ## 1469                        26.4     ELEMENTARY                    98
    ## 1470                        38.3     ELEMENTARY                    95
    ## 1471                        37.0    MIDDLE/HIGH                   147
    ## 1472                        34.5    MIDDLE/HIGH                    89
    ## 1473                        40.4    MIDDLE/HIGH                    58
    ## 1474                        35.9 DISTRICT TOTAL                   811
    ## 1475                        34.9 DISTRICT TOTAL                   408
    ## 1476                        37.0 DISTRICT TOTAL                   403
    ## 1477                        34.0     ELEMENTARY                   512
    ## 1478                        31.6     ELEMENTARY                   276
    ## 1479                        36.5     ELEMENTARY                   236
    ## 1480                        39.0    MIDDLE/HIGH                   299
    ## 1481                        40.7    MIDDLE/HIGH                   132
    ## 1482                        37.6    MIDDLE/HIGH                   167
    ## 1483                        28.3 DISTRICT TOTAL                  1250
    ## 1484                        26.1 DISTRICT TOTAL                   649
    ## 1485                        30.5 DISTRICT TOTAL                   601
    ## 1486                        26.9     ELEMENTARY                   678
    ## 1487                        24.2     ELEMENTARY                   354
    ## 1488                        29.5     ELEMENTARY                   324
    ## 1489                        29.9    MIDDLE/HIGH                   572
    ## 1490                        28.2    MIDDLE/HIGH                   295
    ## 1491                        31.5    MIDDLE/HIGH                   277
    ## 1492                        27.5 DISTRICT TOTAL                  1852
    ## 1493                        27.1 DISTRICT TOTAL                   924
    ## 1494                        27.9 DISTRICT TOTAL                   928
    ## 1495                        25.8     ELEMENTARY                  1054
    ## 1496                        25.6     ELEMENTARY                   520
    ## 1497                        26.0     ELEMENTARY                   534
    ## 1498                        29.7    MIDDLE/HIGH                   798
    ## 1499                        28.9    MIDDLE/HIGH                   404
    ## 1500                        30.4    MIDDLE/HIGH                   394
    ## 1501                        23.5 DISTRICT TOTAL                   685
    ## 1502                        22.4 DISTRICT TOTAL                   342
    ## 1503                        24.6 DISTRICT TOTAL                   343
    ## 1504                        23.5     ELEMENTARY                   349
    ## 1505                        22.7     ELEMENTARY                   161
    ## 1506                        24.1     ELEMENTARY                   188
    ## 1507                        23.6    MIDDLE/HIGH                   336
    ## 1508                        22.1    MIDDLE/HIGH                   181
    ## 1509                        25.2    MIDDLE/HIGH                   155
    ## 1510                        28.7 DISTRICT TOTAL                   618
    ## 1511                        28.9 DISTRICT TOTAL                   290
    ## 1512                        28.4 DISTRICT TOTAL                   328
    ## 1513                        32.6     ELEMENTARY                   278
    ## 1514                        35.5     ELEMENTARY                   122
    ## 1515                        30.0     ELEMENTARY                   156
    ## 1516                        25.1    MIDDLE/HIGH                   340
    ## 1517                        23.1    MIDDLE/HIGH                   168
    ## 1518                        26.9    MIDDLE/HIGH                   172
    ## 1519                        33.9 DISTRICT TOTAL                   851
    ## 1520                        29.3 DISTRICT TOTAL                   452
    ## 1521                        38.3 DISTRICT TOTAL                   399
    ## 1522                        34.0     ELEMENTARY                   501
    ## 1523                        29.8     ELEMENTARY                   275
    ## 1524                        38.3     ELEMENTARY                   226
    ## 1525                        33.9    MIDDLE/HIGH                   350
    ## 1526                        28.7    MIDDLE/HIGH                   177
    ## 1527                        38.2    MIDDLE/HIGH                   173
    ## 1528                        26.1 DISTRICT TOTAL                   354
    ## 1529                        26.9 DISTRICT TOTAL                   169
    ## 1530                        25.3 DISTRICT TOTAL                   185
    ## 1531                        20.9     ELEMENTARY                    99
    ## 1532                        17.6     ELEMENTARY                    51
    ## 1533                        24.0     ELEMENTARY                    48
    ## 1534                        28.9    MIDDLE/HIGH                   255
    ## 1535                        32.0    MIDDLE/HIGH                   118
    ## 1536                        26.0    MIDDLE/HIGH                   137
    ## 1537                        32.8 DISTRICT TOTAL                   898
    ## 1538                        31.9 DISTRICT TOTAL                   441
    ## 1539                        33.7 DISTRICT TOTAL                   457
    ## 1540                        31.7     ELEMENTARY                   500
    ## 1541                        29.2     ELEMENTARY                   241
    ## 1542                        34.0     ELEMENTARY                   259
    ## 1543                        34.1    MIDDLE/HIGH                   398
    ## 1544                        34.9    MIDDLE/HIGH                   200
    ## 1545                        33.3    MIDDLE/HIGH                   198
    ## 1546                        42.5 DISTRICT TOTAL                   988
    ## 1547                        39.3 DISTRICT TOTAL                   531
    ## 1548                        45.7 DISTRICT TOTAL                   457
    ## 1549                        44.1     ELEMENTARY                   558
    ## 1550                        40.5     ELEMENTARY                   306
    ## 1551                        47.8     ELEMENTARY                   252
    ## 1552                        40.4    MIDDLE/HIGH                   430
    ## 1553                        37.7    MIDDLE/HIGH                   225
    ## 1554                        43.0    MIDDLE/HIGH                   205
    ## 1555                        28.8 DISTRICT TOTAL                  1528
    ## 1556                        27.8 DISTRICT TOTAL                   766
    ## 1557                        29.8 DISTRICT TOTAL                   762
    ## 1558                        27.5     ELEMENTARY                   803
    ## 1559                        26.3     ELEMENTARY                   405
    ## 1560                        28.8     ELEMENTARY                   398
    ## 1561                        30.1    MIDDLE/HIGH                   725
    ## 1562                        29.4    MIDDLE/HIGH                   361
    ## 1563                        30.8    MIDDLE/HIGH                   364
    ## 1564                        47.2 DISTRICT TOTAL                    66
    ## 1565                        36.5 DISTRICT TOTAL                    40
    ## 1566                        58.1 DISTRICT TOTAL                    26
    ## 1567                        54.3     ELEMENTARY                    42
    ## 1568                        38.5     ELEMENTARY                    32
    ## 1569                        75.0     ELEMENTARY                    10
    ## 1570                        39.9 DISTRICT TOTAL                   232
    ## 1571                        36.8 DISTRICT TOTAL                   120
    ## 1572                        42.9 DISTRICT TOTAL                   112
    ## 1573                        39.1     ELEMENTARY                   165
    ## 1574                        35.8     ELEMENTARY                    86
    ## 1575                        42.3     ELEMENTARY                    79
    ## 1576                        41.7    MIDDLE/HIGH                    67
    ## 1577                        39.3    MIDDLE/HIGH                    34
    ## 1578                        44.1    MIDDLE/HIGH                    33
    ## 1579                        29.9 DISTRICT TOTAL                   144
    ## 1580                        31.0 DISTRICT TOTAL                    74
    ## 1581                        28.6 DISTRICT TOTAL                    70
    ## 1582                        21.5     ELEMENTARY                    97
    ## 1583                        18.2     ELEMENTARY                    49
    ## 1584                        25.0     ELEMENTARY                    48
    ## 1585                        44.7    MIDDLE/HIGH                    47
    ## 1586                        49.0    MIDDLE/HIGH                    25
    ## 1587                        38.9    MIDDLE/HIGH                    22
    ## 1588                        39.5 DISTRICT TOTAL                   262
    ## 1589                        40.2 DISTRICT TOTAL                   125
    ## 1590                        38.9 DISTRICT TOTAL                   137
    ## 1591                        31.1     ELEMENTARY                   186
    ## 1592                        35.6     ELEMENTARY                    87
    ## 1593                        26.7     ELEMENTARY                    99
    ## 1594                        54.5    MIDDLE/HIGH                    76
    ## 1595                        48.6    MIDDLE/HIGH                    38
    ## 1596                        59.1    MIDDLE/HIGH                    38
    ## 1597                        41.3 DISTRICT TOTAL                   210
    ## 1598                        43.4 DISTRICT TOTAL                    93
    ## 1599                        39.4 DISTRICT TOTAL                   117
    ## 1600                        33.5     ELEMENTARY                   104
    ## 1601                        35.2     ELEMENTARY                    41
    ## 1602                        32.3     ELEMENTARY                    63
    ## 1603                        47.5    MIDDLE/HIGH                   106
    ## 1604                        49.0    MIDDLE/HIGH                    52
    ## 1605                        46.0    MIDDLE/HIGH                    54
    ## 1606                        35.5 DISTRICT TOTAL                   111
    ## 1607                        36.6 DISTRICT TOTAL                    52
    ## 1608                        34.4 DISTRICT TOTAL                    59
    ## 1609                        33.3     ELEMENTARY                    96
    ## 1610                        36.2     ELEMENTARY                    44
    ## 1611                        30.7     ELEMENTARY                    52
    ## 1612                        36.5 DISTRICT TOTAL                   165
    ## 1613                        31.9 DISTRICT TOTAL                    92
    ## 1614                        41.6 DISTRICT TOTAL                    73
    ## 1615                        33.3     ELEMENTARY                   116
    ## 1616                        31.3     ELEMENTARY                    57
    ## 1617                        35.2     ELEMENTARY                    59
    ## 1618                        43.0    MIDDLE/HIGH                    49
    ## 1619                        32.7    MIDDLE/HIGH                    35
    ## 1620                        58.8    MIDDLE/HIGH                    14
    ## 1621                        36.1 DISTRICT TOTAL                   163
    ## 1622                        33.8 DISTRICT TOTAL                    86
    ## 1623                        38.4 DISTRICT TOTAL                    77
    ## 1624                        36.8     ELEMENTARY                    96
    ## 1625                        32.1     ELEMENTARY                    55
    ## 1626                        42.3     ELEMENTARY                    41
    ## 1627                        35.0    MIDDLE/HIGH                    67
    ## 1628                        36.7    MIDDLE/HIGH                    31
    ## 1629                        33.3    MIDDLE/HIGH                    36
    ## 1630                        24.4 DISTRICT TOTAL                  1400
    ## 1631                        24.8 DISTRICT TOTAL                   701
    ## 1632                        24.0 DISTRICT TOTAL                   699
    ## 1633                        24.1     ELEMENTARY                   905
    ## 1634                        24.9     ELEMENTARY                   450
    ## 1635                        23.3     ELEMENTARY                   455
    ## 1636                        24.9    MIDDLE/HIGH                   495
    ## 1637                        24.6    MIDDLE/HIGH                   251
    ## 1638                        25.2    MIDDLE/HIGH                   244
    ## 1639                        26.9 DISTRICT TOTAL                   282
    ## 1640                        23.7 DISTRICT TOTAL                   142
    ## 1641                        29.8 DISTRICT TOTAL                   140
    ## 1642                        22.6     ELEMENTARY                   189
    ## 1643                        18.1     ELEMENTARY                   104
    ## 1644                        27.2     ELEMENTARY                    85
    ## 1645                        34.2    MIDDLE/HIGH                    93
    ## 1646                        35.6    MIDDLE/HIGH                    38
    ## 1647                        33.3    MIDDLE/HIGH                    55
    ## 1648                        40.7 DISTRICT TOTAL                  1242
    ## 1649                        40.3 DISTRICT TOTAL                   631
    ## 1650                        41.1 DISTRICT TOTAL                   611
    ## 1651                        37.6     ELEMENTARY                   848
    ## 1652                        35.5     ELEMENTARY                   445
    ## 1653                        39.7     ELEMENTARY                   403
    ## 1654                        46.7    MIDDLE/HIGH                   394
    ## 1655                        49.3    MIDDLE/HIGH                   186
    ## 1656                        44.1    MIDDLE/HIGH                   208
    ## 1657                        34.4 DISTRICT TOTAL                   366
    ## 1658                        33.8 DISTRICT TOTAL                   180
    ## 1659                        34.9 DISTRICT TOTAL                   186
    ## 1660                        32.4     ELEMENTARY                   233
    ## 1661                        32.7     ELEMENTARY                   115
    ## 1662                        32.0     ELEMENTARY                   118
    ## 1663                        37.5    MIDDLE/HIGH                   133
    ## 1664                        35.6    MIDDLE/HIGH                    65
    ## 1665                        39.0    MIDDLE/HIGH                    68
    ## 1666                        32.1 DISTRICT TOTAL                   451
    ## 1667                        28.6 DISTRICT TOTAL                   235
    ## 1668                        35.3 DISTRICT TOTAL                   216
    ## 1669                        32.7     ELEMENTARY                   240
    ## 1670                        30.9     ELEMENTARY                   125
    ## 1671                        34.4     ELEMENTARY                   115
    ## 1672                        31.7    MIDDLE/HIGH                   211
    ## 1673                        25.7    MIDDLE/HIGH                   110
    ## 1674                        37.3    MIDDLE/HIGH                   101
    ## 1675                        38.5 DISTRICT TOTAL                   630
    ## 1676                        36.8 DISTRICT TOTAL                   318
    ## 1677                        40.1 DISTRICT TOTAL                   312
    ## 1678                        34.7     ELEMENTARY                   366
    ## 1679                        32.5     ELEMENTARY                   183
    ## 1680                        36.9     ELEMENTARY                   183
    ## 1681                        43.2    MIDDLE/HIGH                   264
    ## 1682                        42.0    MIDDLE/HIGH                   135
    ## 1683                        44.5    MIDDLE/HIGH                   129
    ## 1684                        34.4 DISTRICT TOTAL                   452
    ## 1685                        33.2 DISTRICT TOTAL                   251
    ## 1686                        35.7 DISTRICT TOTAL                   201
    ## 1687                        35.1     ELEMENTARY                   271
    ## 1688                        32.9     ELEMENTARY                   157
    ## 1689                        37.8     ELEMENTARY                   114
    ## 1690                        34.2    MIDDLE/HIGH                   181
    ## 1691                        34.6    MIDDLE/HIGH                    94
    ## 1692                        33.6    MIDDLE/HIGH                    87
    ## 1693                        31.9 DISTRICT TOTAL                   723
    ## 1694                        32.1 DISTRICT TOTAL                   344
    ## 1695                        31.6 DISTRICT TOTAL                   379
    ## 1696                        31.5     ELEMENTARY                   424
    ## 1697                        33.1     ELEMENTARY                   192
    ## 1698                        30.1     ELEMENTARY                   232
    ## 1699                        32.4    MIDDLE/HIGH                   299
    ## 1700                        30.9    MIDDLE/HIGH                   152
    ## 1701                        33.9    MIDDLE/HIGH                   147
    ## 1702                        41.5 DISTRICT TOTAL                   160
    ## 1703                        31.6 DISTRICT TOTAL                    87
    ## 1704                        50.7 DISTRICT TOTAL                    73
    ## 1705                        44.8     ELEMENTARY                    86
    ## 1706                        35.8     ELEMENTARY                    47
    ## 1707                        53.6     ELEMENTARY                    39
    ## 1708                        37.3    MIDDLE/HIGH                    74
    ## 1709                        25.9    MIDDLE/HIGH                    40
    ## 1710                        46.9    MIDDLE/HIGH                    34
    ## 1711                        42.2 DISTRICT TOTAL                   483
    ## 1712                        41.2 DISTRICT TOTAL                   245
    ## 1713                        43.3 DISTRICT TOTAL                   238
    ## 1714                        38.3     ELEMENTARY                   346
    ## 1715                        38.3     ELEMENTARY                   171
    ## 1716                        38.3     ELEMENTARY                   175
    ## 1717                        50.9    MIDDLE/HIGH                   137
    ## 1718                        48.3    MIDDLE/HIGH                    74
    ## 1719                        53.4    MIDDLE/HIGH                    63
    ## 1720                        29.7 DISTRICT TOTAL                   245
    ## 1721                        30.6 DISTRICT TOTAL                   122
    ## 1722                        28.8 DISTRICT TOTAL                   123
    ## 1723                        28.6     ELEMENTARY                   184
    ## 1724                        27.9     ELEMENTARY                    96
    ## 1725                        29.3     ELEMENTARY                    88
    ## 1726                        32.2    MIDDLE/HIGH                    61
    ## 1727                        28.6    MIDDLE/HIGH                    35
    ## 1728                        38.4 DISTRICT TOTAL                   162
    ## 1729                        39.2 DISTRICT TOTAL                    76
    ## 1730                        37.7 DISTRICT TOTAL                    86
    ## 1731                        28.5     ELEMENTARY                   113
    ## 1732                        28.9     ELEMENTARY                    54
    ## 1733                        28.0     ELEMENTARY                    59
    ## 1734                        53.3    MIDDLE/HIGH                    49
    ## 1735                        55.1    MIDDLE/HIGH                    22
    ## 1736                        51.8    MIDDLE/HIGH                    27
    ## 1737                        25.8 DISTRICT TOTAL                   429
    ## 1738                        25.3 DISTRICT TOTAL                   208
    ## 1739                        26.4 DISTRICT TOTAL                   221
    ## 1740                        25.4     ELEMENTARY                   264
    ## 1741                        26.2     ELEMENTARY                   121
    ## 1742                        24.7     ELEMENTARY                   143
    ## 1743                        27.6    MIDDLE/HIGH                   165
    ## 1744                        25.0    MIDDLE/HIGH                    87
    ## 1745                        30.4    MIDDLE/HIGH                    78
    ## 1746                        32.6 DISTRICT TOTAL                   578
    ## 1747                        32.3 DISTRICT TOTAL                   280
    ## 1748                        32.8 DISTRICT TOTAL                   298
    ## 1749                        32.8     ELEMENTARY                   312
    ## 1750                        33.9     ELEMENTARY                   148
    ## 1751                        31.8     ELEMENTARY                   164
    ## 1752                        32.6    MIDDLE/HIGH                   266
    ## 1753                        31.2    MIDDLE/HIGH                   132
    ## 1754                        34.0    MIDDLE/HIGH                   134
    ## 1755                        23.4 DISTRICT TOTAL                   425
    ## 1756                        21.2 DISTRICT TOTAL                   212
    ## 1757                        25.3 DISTRICT TOTAL                   213
    ## 1758                        22.9     ELEMENTARY                   249
    ## 1759                        21.1     ELEMENTARY                   120
    ## 1760                        24.6     ELEMENTARY                   129
    ## 1761                        24.8    MIDDLE/HIGH                   176
    ## 1762                        21.4    MIDDLE/HIGH                    92
    ## 1763                        28.2    MIDDLE/HIGH                    84
    ## 1764                        22.6 DISTRICT TOTAL                   592
    ## 1765                        21.7 DISTRICT TOTAL                   269
    ## 1766                        23.4 DISTRICT TOTAL                   323
    ## 1767                        19.9     ELEMENTARY                   379
    ## 1768                        21.0     ELEMENTARY                   171
    ## 1769                        18.9     ELEMENTARY                   208
    ## 1770                        27.2    MIDDLE/HIGH                   213
    ## 1771                        23.0    MIDDLE/HIGH                    98
    ## 1772                        30.5    MIDDLE/HIGH                   115
    ## 1773                        36.8 DISTRICT TOTAL                   378
    ## 1774                        34.6 DISTRICT TOTAL                   185
    ## 1775                        38.8 DISTRICT TOTAL                   193
    ## 1776                        33.7     ELEMENTARY                   317
    ## 1777                        31.6     ELEMENTARY                   157
    ## 1778                        35.5     ELEMENTARY                   160
    ## 1779                        51.2    MIDDLE/HIGH                    61
    ## 1780                        49.1    MIDDLE/HIGH                    28
    ## 1781                        52.9    MIDDLE/HIGH                    33
    ## 1782                        26.1 DISTRICT TOTAL                   932
    ## 1783                        26.4 DISTRICT TOTAL                   468
    ## 1784                        25.8 DISTRICT TOTAL                   464
    ## 1785                        24.8     ELEMENTARY                   603
    ## 1786                        25.3     ELEMENTARY                   296
    ## 1787                        24.2     ELEMENTARY                   307
    ## 1788                        28.5    MIDDLE/HIGH                   329
    ## 1789                        28.3    MIDDLE/HIGH                   172
    ## 1790                        28.7    MIDDLE/HIGH                   157
    ## 1791                        26.7 DISTRICT TOTAL                   499
    ## 1792                        26.1 DISTRICT TOTAL                   225
    ## 1793                        27.1 DISTRICT TOTAL                   274
    ## 1794                        24.6     ELEMENTARY                   323
    ## 1795                        25.1     ELEMENTARY                   137
    ## 1796                        24.2     ELEMENTARY                   186
    ## 1797                        31.0    MIDDLE/HIGH                   176
    ## 1798                        28.5    MIDDLE/HIGH                    88
    ## 1799                        33.3    MIDDLE/HIGH                    88
    ## 1800                        31.5 DISTRICT TOTAL                   335
    ## 1801                        35.2 DISTRICT TOTAL                   162
    ## 1802                        27.9 DISTRICT TOTAL                   173
    ## 1803                        29.2     ELEMENTARY                   209
    ## 1804                        33.6     ELEMENTARY                    95
    ## 1805                        25.0     ELEMENTARY                   114
    ## 1806                        35.0    MIDDLE/HIGH                   126
    ## 1807                        37.4    MIDDLE/HIGH                    67
    ## 1808                        32.3    MIDDLE/HIGH                    59
    ## 1809                        21.9 DISTRICT TOTAL                   393
    ## 1810                        20.3 DISTRICT TOTAL                   189
    ## 1811                        23.3 DISTRICT TOTAL                   204
    ## 1812                        20.3     ELEMENTARY                   225
    ## 1813                        19.2     ELEMENTARY                   111
    ## 1814                        21.3     ELEMENTARY                   114
    ## 1815                        24.1    MIDDLE/HIGH                   168
    ## 1816                        22.0    MIDDLE/HIGH                    78
    ## 1817                        25.8    MIDDLE/HIGH                    90
    ## 1818                        30.2 DISTRICT TOTAL                  1013
    ## 1819                        30.0 DISTRICT TOTAL                   511
    ## 1820                        30.4 DISTRICT TOTAL                   502
    ## 1821                        33.4     ELEMENTARY                   589
    ## 1822                        32.7     ELEMENTARY                   308
    ## 1823                        34.2     ELEMENTARY                   281
    ## 1824                        24.8    MIDDLE/HIGH                   424
    ## 1825                        25.1    MIDDLE/HIGH                   203
    ## 1826                        24.6    MIDDLE/HIGH                   221
    ## 1827                        34.7 DISTRICT TOTAL                  1906
    ## 1828                        34.1 DISTRICT TOTAL                   907
    ## 1829                        35.3 DISTRICT TOTAL                   999
    ## 1830                        32.2     ELEMENTARY                  1126
    ## 1831                        32.3     ELEMENTARY                   535
    ## 1832                        32.1     ELEMENTARY                   591
    ## 1833                        38.2    MIDDLE/HIGH                   780
    ## 1834                        36.6    MIDDLE/HIGH                   372
    ## 1835                        39.5    MIDDLE/HIGH                   408
    ## 1836                        30.2 DISTRICT TOTAL                   226
    ## 1837                        31.1 DISTRICT TOTAL                   115
    ## 1838                        29.3 DISTRICT TOTAL                   111
    ## 1839                        28.6     ELEMENTARY                   147
    ## 1840                        33.0     ELEMENTARY                    69
    ## 1841                        24.3     ELEMENTARY                    78
    ## 1842                        33.1    MIDDLE/HIGH                    79
    ## 1843                        28.1    MIDDLE/HIGH                    46
    ## 1844                        38.9    MIDDLE/HIGH                    33
    ## 1845                        19.3 DISTRICT TOTAL                   617
    ## 1846                        18.4 DISTRICT TOTAL                   301
    ## 1847                        20.0 DISTRICT TOTAL                   316
    ## 1848                        17.7     ELEMENTARY                   395
    ## 1849                        19.1     ELEMENTARY                   182
    ## 1850                        16.5     ELEMENTARY                   213
    ## 1851                        22.2    MIDDLE/HIGH                   222
    ## 1852                        17.9    MIDDLE/HIGH                   119
    ## 1853                        26.4    MIDDLE/HIGH                   103
    ## 1854                        18.0 DISTRICT TOTAL                   425
    ## 1855                        16.7 DISTRICT TOTAL                   190
    ## 1856                        19.2 DISTRICT TOTAL                   235
    ## 1857                        16.7     ELEMENTARY                   253
    ## 1858                        17.6     ELEMENTARY                   111
    ## 1859                        16.0     ELEMENTARY                   142
    ## 1860                        20.7    MIDDLE/HIGH                   172
    ## 1861                        16.0    MIDDLE/HIGH                    79
    ## 1862                        24.4    MIDDLE/HIGH                    93
    ## 1863                        16.8 DISTRICT TOTAL                  1289
    ## 1864                        17.9 DISTRICT TOTAL                   629
    ## 1865                        15.6 DISTRICT TOTAL                   660
    ## 1866                        16.4     ELEMENTARY                   750
    ## 1867                        19.0     ELEMENTARY                   367
    ## 1868                        13.6     ELEMENTARY                   383
    ## 1869                        17.3    MIDDLE/HIGH                   539
    ## 1870                        16.2    MIDDLE/HIGH                   262
    ## 1871                        18.3    MIDDLE/HIGH                   277
    ## 1872                        24.0 DISTRICT TOTAL                   396
    ## 1873                        23.2 DISTRICT TOTAL                   193
    ## 1874                        24.7 DISTRICT TOTAL                   203
    ## 1875                        24.5     ELEMENTARY                   185
    ## 1876                        24.8     ELEMENTARY                    89
    ## 1877                        24.3     ELEMENTARY                    96
    ## 1878                        23.8    MIDDLE/HIGH                   211
    ## 1879                        22.4    MIDDLE/HIGH                   104
    ## 1880                        25.0    MIDDLE/HIGH                   107
    ## 1881                        34.5 DISTRICT TOTAL                   184
    ## 1882                        36.7 DISTRICT TOTAL                    81
    ## 1883                        32.7 DISTRICT TOTAL                   103
    ## 1884                        30.1     ELEMENTARY                   123
    ## 1885                        33.8     ELEMENTARY                    49
    ## 1886                        27.5     ELEMENTARY                    74
    ## 1887                        41.9    MIDDLE/HIGH                    61
    ## 1888                        40.7    MIDDLE/HIGH                    32
    ## 1889                        43.1    MIDDLE/HIGH                    29
    ## 1890                        37.3 DISTRICT TOTAL                   178
    ## 1891                        35.6 DISTRICT TOTAL                    94
    ## 1892                        39.1 DISTRICT TOTAL                    84
    ## 1893                        34.1     ELEMENTARY                   112
    ## 1894                        36.1     ELEMENTARY                    53
    ## 1895                        32.2     ELEMENTARY                    59
    ## 1896                        42.1    MIDDLE/HIGH                    66
    ## 1897                        34.9    MIDDLE/HIGH                    41
    ## 1898                        51.0    MIDDLE/HIGH                    25
    ## 1899                        41.7 DISTRICT TOTAL                   134
    ## 1900                        39.1 DISTRICT TOTAL                    67
    ## 1901                        44.2 DISTRICT TOTAL                    67
    ## 1902                        42.9     ELEMENTARY                    80
    ## 1903                        43.3     ELEMENTARY                    38
    ## 1904                        42.5     ELEMENTARY                    42
    ## 1905                        40.0    MIDDLE/HIGH                    54
    ## 1906                        32.6    MIDDLE/HIGH                    29
    ## 1907                        46.8    MIDDLE/HIGH                    25
    ##      percent_healthy_weight    sex year num_asian num_black num_hisp num_am_ind
    ## 1                     0.653    ALL 2019        14        74      152          1
    ## 2                     0.693 FEMALE 2019        14        74      152          1
    ## 3                     0.616   MALE 2019        14        74      152          1
    ## 4                     0.683    ALL 2019        14        74      152          1
    ## 5                     0.736 FEMALE 2019        14        74      152          1
    ## 6                     0.631   MALE 2019        14        74      152          1
    ## 7                     0.583    ALL 2019        14        74      152          1
    ## 8                     0.573 FEMALE 2019        14        74      152          1
    ## 9                     0.593   MALE 2019        14        74      152          1
    ## 10                    0.550   MALE 2019        33       208      174          7
    ## 11                    0.543    ALL 2019        33       208      174          7
    ## 12                    0.543    ALL 2019        33       208      174          7
    ## 13                    0.533 FEMALE 2019        33       208      174          7
    ## 14                    0.543    ALL 2019        33       208      174          7
    ## 15                    0.542 FEMALE 2019        33       208      174          7
    ## 16                    0.543   MALE 2019        33       208      174          7
    ## 17                    0.556 FEMALE 2019        33       208      174          7
    ## 18                    0.525   MALE 2019        33       208      174          7
    ## 19                    0.557 FEMALE 2019       498       395      425          6
    ## 20                    0.612    ALL 2019       498       395      425          6
    ## 21                    0.604 FEMALE 2019       498       395      425          6
    ## 22                    0.577   MALE 2019       498       395      425          6
    ## 23                    0.640    ALL 2019       498       395      425          6
    ## 24                    0.633 FEMALE 2019       498       395      425          6
    ## 25                    0.647   MALE 2019       498       395      425          6
    ## 26                    0.621   MALE 2019       498       395      425          6
    ## 27                    0.567    ALL 2019       498       395      425          6
    ## 28                    0.652    ALL 2019      1121       382      334         16
    ## 29                    0.667 FEMALE 2019      1121       382      334         16
    ## 30                    0.637   MALE 2019      1121       382      334         16
    ## 31                    0.653    ALL 2019      1121       382      334         16
    ## 32                    0.662 FEMALE 2019      1121       382      334         16
    ## 33                    0.643   MALE 2019      1121       382      334         16
    ## 34                    0.651    ALL 2019      1121       382      334         16
    ## 35                    0.673 FEMALE 2019      1121       382      334         16
    ## 36                    0.631   MALE 2019      1121       382      334         16
    ## 37                    0.658    ALL 2019       701       201      187          7
    ## 38                    0.678 FEMALE 2019       701       201      187          7
    ## 39                    0.657 FEMALE 2019       701       201      187          7
    ## 40                    0.623   MALE 2019       701       201      187          7
    ## 41                    0.691 FEMALE 2019       701       201      187          7
    ## 42                    0.646   MALE 2019       701       201      187          7
    ## 43                    0.637   MALE 2019       701       201      187          7
    ## 44                    0.669    ALL 2019       701       201      187          7
    ## 45                    0.640    ALL 2019       701       201      187          7
    ## 46                    0.659   MALE 2019         6        15       21          4
    ## 47                    0.594   MALE 2019         6        15       21          4
    ## 48                    0.651    ALL 2019         6        15       21          4
    ## 49                    0.642 FEMALE 2019         6        15       21          4
    ## 50                    0.583    ALL 2019         6        15       21          4
    ## 51                    0.571 FEMALE 2019         6        15       21          4
    ## 52                    0.548   MALE 2019         6        15       21          4
    ## 53                    0.515    ALL 2019         6        15       21          4
    ## 54                    0.483 FEMALE 2019         6        15       21          4
    ## 55                    0.614    ALL 2019         7        16       28          3
    ## 56                    0.598 FEMALE 2019         7        16       28          3
    ## 57                    0.626   MALE 2019         7        16       28          3
    ## 58                    0.677    ALL 2019         7        16       28          3
    ## 59                    0.658 FEMALE 2019         7        16       28          3
    ## 60                    0.692   MALE 2019         7        16       28          3
    ## 61                    0.543    ALL 2019         7        16       28          3
    ## 62                    0.533 FEMALE 2019         7        16       28          3
    ## 63                    0.551   MALE 2019         7        16       28          3
    ## 64                    0.592   MALE 2019       153      1491      778          9
    ## 65                    0.538    ALL 2019       153      1491      778          9
    ## 66                    0.597    ALL 2019       153      1491      778          9
    ## 67                    0.603 FEMALE 2019       153      1491      778          9
    ## 68                    0.578    ALL 2019       153      1491      778          9
    ## 69                    0.587 FEMALE 2019       153      1491      778          9
    ## 70                    0.569   MALE 2019       153      1491      778          9
    ## 71                    0.561 FEMALE 2019       153      1491      778          9
    ## 72                    0.515   MALE 2019       153      1491      778          9
    ## 73                    0.625    ALL 2019         1         7        3          2
    ## 74                    0.631 FEMALE 2019         1         7        3          2
    ## 75                    0.563 FEMALE 2019         1         7        3          2
    ## 76                    0.596   MALE 2019         1         7        3          2
    ## 77                    0.676 FEMALE 2019         1         7        3          2
    ## 78                    0.634   MALE 2019         1         7        3          2
    ## 79                    0.621   MALE 2019         1         7        3          2
    ## 80                    0.651    ALL 2019         1         7        3          2
    ## 81                    0.580    ALL 2019         1         7        3          2
    ## 82                    0.620    ALL 2019        18        29       42          2
    ## 83                    0.639 FEMALE 2019        18        29       42          2
    ## 84                    0.609   MALE 2019        18        29       42          2
    ## 85                    0.604   MALE 2019        18        29       42          2
    ## 86                    0.619    ALL 2019        18        29       42          2
    ## 87                    0.630 FEMALE 2019        18        29       42          2
    ## 88                    0.624    ALL 2019        18        29       42          2
    ## 89                    0.661 FEMALE 2019        18        29       42          2
    ## 90                    0.604   MALE 2019        18        29       42          2
    ## 91                    0.631   MALE 2019        24        27       38          6
    ## 92                    0.606    ALL 2019        24        27       38          6
    ## 93                    0.687    ALL 2019        24        27       38          6
    ## 94                    0.698 FEMALE 2019        24        27       38          6
    ## 95                    0.676   MALE 2019        24        27       38          6
    ## 96                    0.659    ALL 2019        24        27       38          6
    ## 97                    0.690 FEMALE 2019        24        27       38          6
    ## 98                    0.682 FEMALE 2019        24        27       38          6
    ## 99                    0.537   MALE 2019        24        27       38          6
    ## 100                   0.645    ALL 2019        11        17       19          4
    ## 101                   0.662 FEMALE 2019        11        17       19          4
    ## 102                   0.629   MALE 2019        11        17       19          4
    ## 103                   0.658    ALL 2019        11        17       19          4
    ## 104                   0.646 FEMALE 2019        11        17       19          4
    ## 105                   0.669   MALE 2019        11        17       19          4
    ## 106                   0.638    ALL 2019        11        17       19          4
    ## 107                   0.689 FEMALE 2019        11        17       19          4
    ## 108                   0.594   MALE 2019        11        17       19          4
    ## 109                   0.677   MALE 2019         5         2       11         11
    ## 110                   0.625   MALE 2019         5         2       11         11
    ## 111                   0.690    ALL 2019         5         2       11         11
    ## 112                   0.707 FEMALE 2019         5         2       11         11
    ## 113                   0.627    ALL 2019         5         2       11         11
    ## 114                   0.630 FEMALE 2019         5         2       11         11
    ## 115                   0.506    ALL 2019         5         2       11         11
    ## 116                   0.500 FEMALE 2019         5         2       11         11
    ## 117                   0.512   MALE 2019         5         2       11         11
    ## 118                   0.808    ALL 2019         4         4        7          1
    ## 119                   0.626    ALL 2019         4         4        7          1
    ## 120                   0.562    ALL 2019         4         4        7          1
    ## 121                   0.490 FEMALE 2019         4         4        7          1
    ## 122                   0.584 FEMALE 2019         4         4        7          1
    ## 123                   0.671   MALE 2019         4         4        7          1
    ## 124                   0.650   MALE 2019         4         4        7          1
    ## 125                   0.613    ALL 2019        53        77       66         11
    ## 126                   0.616 FEMALE 2019        53        77       66         11
    ## 127                   0.610   MALE 2019        53        77       66         11
    ## 128                   0.664    ALL 2019        53        77       66         11
    ## 129                   0.694 FEMALE 2019        53        77       66         11
    ## 130                   0.638   MALE 2019        53        77       66         11
    ## 131                   0.515    ALL 2019        53        77       66         11
    ## 132                   0.469 FEMALE 2019        53        77       66         11
    ## 133                   0.556   MALE 2019        53        77       66         11
    ## 134                   0.705   MALE 2019         2         5       18         43
    ## 135                   0.605   MALE 2019         2         5       18         43
    ## 136                   0.653    ALL 2019         2         5       18         43
    ## 137                   0.598 FEMALE 2019         2         5       18         43
    ## 138                   0.591    ALL 2019         2         5       18         43
    ## 139                   0.577 FEMALE 2019         2         5       18         43
    ## 140                   0.542    ALL 2019         2         5       18         43
    ## 141                   0.554 FEMALE 2019         2         5       18         43
    ## 142                   0.529   MALE 2019         2         5       18         43
    ## 143                   0.557    ALL 2019         2         8       78        429
    ## 144                   0.381    ALL 2019         2         8       78        429
    ## 145                   0.364 FEMALE 2019         2         8       78        429
    ## 146                   0.392   MALE 2019         2         8       78        429
    ## 147                   0.650 FEMALE 2019         2         8       78        429
    ## 148                   0.565 FEMALE 2019         2         8       78        429
    ## 149                   0.552   MALE 2019         2         8       78        429
    ## 150                   0.648    ALL 2019         2         8       78        429
    ## 151                   0.646   MALE 2019         2         8       78        429
    ## 152                   0.627   MALE 2019         4         7       19          6
    ## 153                   0.603    ALL 2019         4         7       19          6
    ## 154                   0.725    ALL 2019         4         7       19          6
    ## 155                   0.733 FEMALE 2019         4         7       19          6
    ## 156                   0.718   MALE 2019         4         7       19          6
    ## 157                   0.696 FEMALE 2019         4         7       19          6
    ## 158                   0.659    ALL 2019         4         7       19          6
    ## 159                   0.651 FEMALE 2019         4         7       19          6
    ## 160                   0.559   MALE 2019         4         7       19          6
    ## 161                   0.555    ALL 2019         1         3       15          1
    ## 162                   0.612 FEMALE 2019         1         3       15          1
    ## 163                   0.574 FEMALE 2019         1         3       15          1
    ## 164                   0.482   MALE 2019         1         3       15          1
    ## 165                   0.632 FEMALE 2019         1         3       15          1
    ## 166                   0.509   MALE 2019         1         3       15          1
    ## 167                   0.494   MALE 2019         1         3       15          1
    ## 168                   0.573    ALL 2019         1         3       15          1
    ## 169                   0.530    ALL 2019         1         3       15          1
    ## 170                   0.642    ALL 2019         4         4       18          5
    ## 171                   0.545 FEMALE 2019         4         4       18          5
    ## 172                   0.736   MALE 2019         4         4       18          5
    ## 173                   0.632    ALL 2019         4         4       18          5
    ## 174                   0.566 FEMALE 2019         4         4       18          5
    ## 175                   0.706   MALE 2019         4         4       18          5
    ## 176                   0.657    ALL 2019         4         4       18          5
    ## 177                   0.511 FEMALE 2019         4         4       18          5
    ## 178                   0.772   MALE 2019         4         4       18          5
    ## 179                   0.623   MALE 2019         6         8       18          1
    ## 180                   0.626   MALE 2019         6         8       18          1
    ## 181                   0.603    ALL 2019         6         8       18          1
    ## 182                   0.581 FEMALE 2019         6         8       18          1
    ## 183                   0.608    ALL 2019         6         8       18          1
    ## 184                   0.588 FEMALE 2019         6         8       18          1
    ## 185                   0.614    ALL 2019         6         8       18          1
    ## 186                   0.597 FEMALE 2019         6         8       18          1
    ## 187                   0.630   MALE 2019         6         8       18          1
    ## 188                   0.640    ALL 2019        10        91     1192          6
    ## 189                   0.639 FEMALE 2019        10        91     1192          6
    ## 190                   0.641   MALE 2019        10        91     1192          6
    ## 191                   0.739    ALL 2019        10        91     1192          6
    ## 192                   0.736 FEMALE 2019        10        91     1192          6
    ## 193                   0.743   MALE 2019        10        91     1192          6
    ## 194                   0.502    ALL 2019        10        91     1192          6
    ## 195                   0.508 FEMALE 2019        10        91     1192          6
    ## 196                   0.496   MALE 2019        10        91     1192          6
    ## 197                   0.549   MALE 2019         5         6      100        128
    ## 198                   0.559   MALE 2019         5         6      100        128
    ## 199                   0.568    ALL 2019         5         6      100        128
    ## 200                   0.583 FEMALE 2019         5         6      100        128
    ## 201                   0.574    ALL 2019         5         6      100        128
    ## 202                   0.587 FEMALE 2019         5         6      100        128
    ## 203                   0.583    ALL 2019         5         6      100        128
    ## 204                   0.592 FEMALE 2019         5         6      100        128
    ## 205                   0.571   MALE 2019         5         6      100        128
    ## 206                   0.518    ALL 2019         1         3       73          3
    ## 207                   0.478 FEMALE 2019         1         3       73          3
    ## 208                   0.469 FEMALE 2019         1         3       73          3
    ## 209                   0.566   MALE 2019         1         3       73          3
    ## 210                   0.541    ALL 2019         1         3       73          3
    ## 211                   0.484   MALE 2019         1         3       73          3
    ## 212                   0.603   MALE 2019         1         3       73          3
    ## 213                   0.500    ALL 2019         1         3       73          3
    ## 214                   0.676 FEMALE 2019         1         3        9          3
    ## 215                   0.729   MALE 2019         1         3        9          3
    ## 216                   0.718    ALL 2019         1         3        9          3
    ## 217                   0.744 FEMALE 2019         1         3        9          3
    ## 218                   0.696   MALE 2019         1         3        9          3
    ## 219                   0.703    ALL 2019         1         3        9          3
    ## 220                   0.679    ALL 2019         1         3        9          3
    ## 221                   0.586 FEMALE 2019         1         3        9          3
    ## 222                   0.582    ALL 2019         5        11       43          4
    ## 223                   0.635    ALL 2019         5        11       43          4
    ## 224                   0.622 FEMALE 2019         5        11       43          4
    ## 225                   0.650   MALE 2019         5        11       43          4
    ## 226                   0.535 FEMALE 2019         5        11       43          4
    ## 227                   0.561 FEMALE 2019         5        11       43          4
    ## 228                   0.603   MALE 2019         5        11       43          4
    ## 229                   0.580    ALL 2019         5        11       43          4
    ## 230                   0.621   MALE 2019         5        11       43          4
    ## 231                   0.546    ALL 2019        22       777      203          7
    ## 232                   0.528 FEMALE 2019        22       777      203          7
    ## 233                   0.562   MALE 2019        22       777      203          7
    ## 234                   0.542    ALL 2019        22       777      203          7
    ## 235                   0.520 FEMALE 2019        22       777      203          7
    ## 236                   0.561   MALE 2019        22       777      203          7
    ## 237                   0.553    ALL 2019        22       777      203          7
    ## 238                   0.543 FEMALE 2019        22       777      203          7
    ## 239                   0.563   MALE 2019        22       777      203          7
    ## 240                   0.663 FEMALE 2019         8        10       24          7
    ## 241                   0.656   MALE 2019         8        10       24          7
    ## 242                   0.670    ALL 2019         8        10       24          7
    ## 243                   0.667 FEMALE 2019         8        10       24          7
    ## 244                   0.660    ALL 2019         8        10       24          7
    ## 245                   0.644    ALL 2019         8        10       24          7
    ## 246                   0.658 FEMALE 2019         8        10       24          7
    ## 247                   0.627   MALE 2019         8        10       24          7
    ## 248                   0.673   MALE 2019         8        10       24          7
    ## 249                   0.709   MALE 2019         3         6        9          7
    ## 250                   0.552    ALL 2019         3         6        9          7
    ## 251                   0.549 FEMALE 2019         3         6        9          7
    ## 252                   0.556   MALE 2019         3         6        9          7
    ## 253                   0.581 FEMALE 2019         3         6        9          7
    ## 254                   0.647   MALE 2019         3         6        9          7
    ## 255                   0.645    ALL 2019         3         6        9          7
    ## 256                   0.611    ALL 2019         3         6        9          7
    ## 257                   0.596 FEMALE 2019         3         6        9          7
    ## 258                   0.579    ALL 2019        24        47       53          3
    ## 259                   0.587 FEMALE 2019        24        47       53          3
    ## 260                   0.572   MALE 2019        24        47       53          3
    ## 261                   0.612    ALL 2019        24        47       53          3
    ## 262                   0.625 FEMALE 2019        24        47       53          3
    ## 263                   0.598   MALE 2019        24        47       53          3
    ## 264                   0.531    ALL 2019        24        47       53          3
    ## 265                   0.530 FEMALE 2019        24        47       53          3
    ## 266                   0.531   MALE 2019        24        47       53          3
    ## 267                   0.586    ALL 2019         1        30        4          1
    ## 268                   0.585 FEMALE 2019         1        30        4          1
    ## 269                   0.507 FEMALE 2019         1        30        4          1
    ## 270                   0.603   MALE 2019         1        30        4          1
    ## 271                   0.649 FEMALE 2019         1        30        4          1
    ## 272                   0.605   MALE 2019         1        30        4          1
    ## 273                   0.587   MALE 2019         1        30        4          1
    ## 274                   0.627    ALL 2019         1        30        4          1
    ## 275                   0.557    ALL 2019         1        30        4          1
    ## 276                   0.609    ALL 2019        15        29       52          8
    ## 277                   0.617 FEMALE 2019        15        29       52          8
    ## 278                   0.634   MALE 2019        15        29       52          8
    ## 279                   0.601   MALE 2019        15        29       52          8
    ## 280                   0.650    ALL 2019        15        29       52          8
    ## 281                   0.667 FEMALE 2019        15        29       52          8
    ## 282                   0.532    ALL 2019        15        29       52          8
    ## 283                   0.531 FEMALE 2019        15        29       52          8
    ## 284                   0.533   MALE 2019        15        29       52          8
    ## 285                   0.589    ALL 2019        21        20       18          7
    ## 286                   0.522 FEMALE 2019        21        20       18          7
    ## 287                   0.483 FEMALE 2019        21        20       18          7
    ## 288                   0.623   MALE 2019        21        20       18          7
    ## 289                   0.557 FEMALE 2019        21        20       18          7
    ## 290                   0.686   MALE 2019        21        20       18          7
    ## 291                   0.659   MALE 2019        21        20       18          7
    ## 292                   0.619    ALL 2019        21        20       18          7
    ## 293                   0.554    ALL 2019        21        20       18          7
    ## 294                   0.567    ALL 2019         5        25      136          2
    ## 295                   0.580 FEMALE 2019         5        25      136          2
    ## 296                   0.556   MALE 2019         5        25      136          2
    ## 297                   0.641    ALL 2019         5        25      136          2
    ## 298                   0.658 FEMALE 2019         5        25      136          2
    ## 299                   0.626   MALE 2019         5        25      136          2
    ## 300                   0.482    ALL 2019         5        25      136          2
    ## 301                   0.500 FEMALE 2019         5        25      136          2
    ## 302                   0.463   MALE 2019         5        25      136          2
    ## 303                   0.622   MALE 2019       224       329      271          2
    ## 304                   0.637   MALE 2019       224       329      271          2
    ## 305                   0.596    ALL 2019       224       329      271          2
    ## 306                   0.571 FEMALE 2019       224       329      271          2
    ## 307                   0.628    ALL 2019       224       329      271          2
    ## 308                   0.620 FEMALE 2019       224       329      271          2
    ## 309                   0.712    ALL 2019       224       329      271          2
    ## 310                   0.723 FEMALE 2019       224       329      271          2
    ## 311                   0.696   MALE 2019       224       329      271          2
    ## 312                   0.668    ALL 2019       282       587     1474          4
    ## 313                   0.633    ALL 2019       282       587     1474          4
    ## 314                   0.669 FEMALE 2019       282       587     1474          4
    ## 315                   0.596   MALE 2019       282       587     1474          4
    ## 316                   0.698 FEMALE 2019       282       587     1474          4
    ## 317                   0.684 FEMALE 2019       282       587     1474          4
    ## 318                   0.653   MALE 2019       282       587     1474          4
    ## 319                   0.700    ALL 2019       282       587     1474          4
    ## 320                   0.702   MALE 2019       282       587     1474          4
    ## 321                   0.658    ALL 2019       176       167      200          1
    ## 322                   0.718 FEMALE 2019       176       167      200          1
    ## 323                   0.650 FEMALE 2019       176       167      200          1
    ## 324                   0.664   MALE 2019       176       167      200          1
    ## 325                   0.682    ALL 2019       176       167      200          1
    ## 326                   0.698   MALE 2019       176       167      200          1
    ## 327                   0.654   MALE 2019       176       167      200          1
    ## 328                   0.634    ALL 2019       176       167      200          1
    ## 329                   0.563 FEMALE 2019       176       167      200          1
    ## 330                   0.663 FEMALE 2019        44        38      188          6
    ## 331                   0.693   MALE 2019        44        38      188          6
    ## 332                   0.708    ALL 2019        44        38      188          6
    ## 333                   0.683 FEMALE 2019        44        38      188          6
    ## 334                   0.731   MALE 2019        44        38      188          6
    ## 335                   0.679    ALL 2019        44        38      188          6
    ## 336                   0.644    ALL 2019        44        38      188          6
    ## 337                   0.642 FEMALE 2019        44        38      188          6
    ## 338                   0.646   MALE 2019        44        38      188          6
    ## 339                   0.655    ALL 2019        27         9       84          1
    ## 340                   0.620    ALL 2019        27         9       84          1
    ## 341                   0.547 FEMALE 2019        27         9       84          1
    ## 342                   0.679   MALE 2019        27         9       84          1
    ## 343                   0.727 FEMALE 2019        27         9       84          1
    ## 344                   0.638 FEMALE 2019        27         9       84          1
    ## 345                   0.669   MALE 2019        27         9       84          1
    ## 346                   0.707    ALL 2019        27         9       84          1
    ## 347                   0.687   MALE 2019        27         9       84          1
    ## 348                   0.697 FEMALE 2019         8        12       20          8
    ## 349                   0.645   MALE 2019         8        12       20          8
    ## 350                   0.705    ALL 2019         8        12       20          8
    ## 351                   0.702 FEMALE 2019         8        12       20          8
    ## 352                   0.674    ALL 2019         8        12       20          8
    ## 353                   0.710 FEMALE 2019         8        12       20          8
    ## 354                   0.548   MALE 2019         8        12       20          8
    ## 355                   0.709   MALE 2019         8        12       20          8
    ## 356                   0.638    ALL 2019         8        12       20          8
    ## 357                   0.728   MALE 2019      1422       402      409         22
    ## 358                   0.688    ALL 2019      1422       402      409         22
    ## 359                   0.702 FEMALE 2019      1422       402      409         22
    ## 360                   0.675   MALE 2019      1422       402      409         22
    ## 361                   0.704   MALE 2019      1422       402      409         22
    ## 362                   0.720    ALL 2019      1422       402      409         22
    ## 363                   0.706    ALL 2019      1422       402      409         22
    ## 364                   0.707 FEMALE 2019      1422       402      409         22
    ## 365                   0.712 FEMALE 2019      1422       402      409         22
    ## 366                   0.648    ALL 2019       262       666      171          6
    ## 367                   0.680 FEMALE 2019       262       666      171          6
    ## 368                   0.620   MALE 2019       262       666      171          6
    ## 369                   0.672    ALL 2019       262       666      171          6
    ## 370                   0.690 FEMALE 2019       262       666      171          6
    ## 371                   0.657   MALE 2019       262       666      171          6
    ## 372                   0.609    ALL 2019       262       666      171          6
    ## 373                   0.664 FEMALE 2019       262       666      171          6
    ## 374                   0.562   MALE 2019       262       666      171          6
    ## 375                   0.629   MALE 2019       157       843      201          7
    ## 376                   0.601   MALE 2019       157       843      201          7
    ## 377                   0.636    ALL 2019       157       843      201          7
    ## 378                   0.643 FEMALE 2019       157       843      201          7
    ## 379                   0.596    ALL 2019       157       843      201          7
    ## 380                   0.591 FEMALE 2019       157       843      201          7
    ## 381                   0.549    ALL 2019       157       843      201          7
    ## 382                   0.519 FEMALE 2019       157       843      201          7
    ## 383                   0.575   MALE 2019       157       843      201          7
    ## 384                   0.652    ALL 2019        10         6       37         13
    ## 385                   0.613    ALL 2019        10         6       37         13
    ## 386                   0.646 FEMALE 2019        10         6       37         13
    ## 387                   0.581   MALE 2019        10         6       37         13
    ## 388                   0.753 FEMALE 2019        10         6       37         13
    ## 389                   0.695 FEMALE 2019        10         6       37         13
    ## 390                   0.610   MALE 2019        10         6       37         13
    ## 391                   0.693    ALL 2019        10         6       37         13
    ## 392                   0.635   MALE 2019        10         6       37         13
    ## 393                   0.682    ALL 2019        12        17       82        324
    ## 394                   0.691 FEMALE 2019        12        17       82        324
    ## 395                   0.704 FEMALE 2019        12        17       82        324
    ## 396                   0.660   MALE 2019        12        17       82        324
    ## 397                   0.664    ALL 2019        12        17       82        324
    ## 398                   0.692   MALE 2019        12        17       82        324
    ## 399                   0.638   MALE 2019        12        17       82        324
    ## 400                   0.711    ALL 2019        12        17       82        324
    ## 401                   0.729 FEMALE 2019        12        17       82        324
    ## 402                   0.666 FEMALE 2019       102        48      142         11
    ## 403                   0.659   MALE 2019       102        48      142         11
    ## 404                   0.694    ALL 2019       102        48      142         11
    ## 405                   0.702 FEMALE 2019       102        48      142         11
    ## 406                   0.686   MALE 2019       102        48      142         11
    ## 407                   0.662    ALL 2019       102        48      142         11
    ## 408                   0.622    ALL 2019       102        48      142         11
    ## 409                   0.624 FEMALE 2019       102        48      142         11
    ## 410                   0.620   MALE 2019       102        48      142         11
    ## 411                   0.685    ALL 2019        29        35       90          9
    ## 412                   0.712    ALL 2019        29        35       90          9
    ## 413                   0.690 FEMALE 2019        29        35       90          9
    ## 414                   0.737   MALE 2019        29        35       90          9
    ## 415                   0.674 FEMALE 2019        29        35       90          9
    ## 416                   0.680 FEMALE 2019        29        35       90          9
    ## 417                   0.689   MALE 2019        29        35       90          9
    ## 418                   0.670    ALL 2019        29        35       90          9
    ## 419                   0.667   MALE 2019        29        35       90          9
    ## 420                   0.681    ALL 2019        58        52      250         13
    ## 421                   0.674 FEMALE 2019        58        52      250         13
    ## 422                   0.688   MALE 2019        58        52      250         13
    ## 423                   0.700    ALL 2019        58        52      250         13
    ## 424                   0.683 FEMALE 2019        58        52      250         13
    ## 425                   0.717   MALE 2019        58        52      250         13
    ## 426                   0.648    ALL 2019        58        52      250         13
    ## 427                   0.659 FEMALE 2019        58        52      250         13
    ## 428                   0.637   MALE 2019        58        52      250         13
    ## 429                   0.605 FEMALE 2019        52       289      269          9
    ## 430                   0.634   MALE 2019        52       289      269          9
    ## 431                   0.628    ALL 2019        52       289      269          9
    ## 432                   0.604 FEMALE 2019        52       289      269          9
    ## 433                   0.621    ALL 2019        52       289      269          9
    ## 434                   0.618   MALE 2019        52       289      269          9
    ## 435                   0.645   MALE 2019        52       289      269          9
    ## 436                   0.619    ALL 2019        52       289      269          9
    ## 437                   0.620 FEMALE 2019        52       289      269          9
    ## 438                   0.691   MALE 2019       102        66      181         12
    ## 439                   0.648    ALL 2019       102        66      181         12
    ## 440                   0.639 FEMALE 2019       102        66      181         12
    ## 441                   0.657   MALE 2019       102        66      181         12
    ## 442                   0.697    ALL 2019       102        66      181         12
    ## 443                   0.676    ALL 2019       102        66      181         12
    ## 444                   0.676 FEMALE 2019       102        66      181         12
    ## 445                   0.676   MALE 2019       102        66      181         12
    ## 446                   0.704 FEMALE 2019       102        66      181         12
    ## 447                   0.642    ALL 2019         5         7       15        167
    ## 448                   0.679 FEMALE 2019         5         7       15        167
    ## 449                   0.610   MALE 2019         5         7       15        167
    ## 450                   0.664    ALL 2019         5         7       15        167
    ## 451                   0.702 FEMALE 2019         5         7       15        167
    ## 452                   0.630   MALE 2019         5         7       15        167
    ## 453                   0.638    ALL 2019         5         7       15        167
    ## 454                   0.651 FEMALE 2019         5         7       15        167
    ## 455                   0.626   MALE 2019         5         7       15        167
    ## 456                   0.735 FEMALE 2019        80        43      112          1
    ## 457                   0.728   MALE 2019        80        43      112          1
    ## 458                   0.742    ALL 2019        80        43      112          1
    ## 459                   0.725 FEMALE 2019        80        43      112          1
    ## 460                   0.731    ALL 2019        80        43      112          1
    ## 461                   0.758   MALE 2019        80        43      112          1
    ## 462                   0.716    ALL 2019        80        43      112          1
    ## 463                   0.748 FEMALE 2019        80        43      112          1
    ## 464                   0.680   MALE 2019        80        43      112          1
    ## 465                   0.626   MALE 2019       222       511      620         35
    ## 466                   0.559    ALL 2019       222       511      620         35
    ## 467                   0.557 FEMALE 2019       222       511      620         35
    ## 468                   0.561   MALE 2019       222       511      620         35
    ## 469                   0.616    ALL 2019       222       511      620         35
    ## 470                   0.625 FEMALE 2019       222       511      620         35
    ## 471                   0.606   MALE 2019       222       511      620         35
    ## 472                   0.641    ALL 2019       222       511      620         35
    ## 473                   0.655 FEMALE 2019       222       511      620         35
    ## 474                   0.513    ALL 2019         4         1       14          3
    ## 475                   0.486 FEMALE 2019         4         1       14          3
    ## 476                   0.538   MALE 2019         4         1       14          3
    ## 477                   0.502    ALL 2019         4         1       14          3
    ## 478                   0.443 FEMALE 2019         4         1       14          3
    ## 479                   0.556   MALE 2019         4         1       14          3
    ## 480                   0.535    ALL 2019         4         1       14          3
    ## 481                   0.569 FEMALE 2019         4         1       14          3
    ## 482                   0.500   MALE 2019         4         1       14          3
    ## 483                   0.520    ALL 2019         1         1       34        869
    ## 484                   0.527 FEMALE 2019         1         1       34        869
    ## 485                   0.513   MALE 2019         1         1       34        869
    ## 486                   0.566    ALL 2019         1         1       34        869
    ## 487                   0.561 FEMALE 2019         1         1       34        869
    ## 488                   0.569   MALE 2019         1         1       34        869
    ## 489                   0.441    ALL 2019         1         1       34        869
    ## 490                   0.455 FEMALE 2019         1         1       34        869
    ## 491                   0.430   MALE 2019         1         1       34        869
    ## 492                   0.637    ALL 2019        17        12       61          3
    ## 493                   0.621 FEMALE 2019        17        12       61          3
    ## 494                   0.651   MALE 2019        17        12       61          3
    ## 495                   0.664    ALL 2019        17        12       61          3
    ## 496                   0.676 FEMALE 2019        17        12       61          3
    ## 497                   0.654   MALE 2019        17        12       61          3
    ## 498                   0.592    ALL 2019        17        12       61          3
    ## 499                   0.531 FEMALE 2019        17        12       61          3
    ## 500                   0.656   MALE 2019        17        12       61          3
    ## 501                   0.592    ALL 2019        22       298      213         26
    ## 502                   0.573 FEMALE 2019        22       298      213         26
    ## 503                   0.608   MALE 2019        22       298      213         26
    ## 504                   0.622    ALL 2019        22       298      213         26
    ## 505                   0.615 FEMALE 2019        22       298      213         26
    ## 506                   0.628   MALE 2019        22       298      213         26
    ## 507                   0.543    ALL 2019        22       298      213         26
    ## 508                   0.496 FEMALE 2019        22       298      213         26
    ## 509                   0.579   MALE 2019        22       298      213         26
    ## 510                   0.550    ALL 2019         9        22      104          3
    ## 511                   0.552 FEMALE 2019         9        22      104          3
    ## 512                   0.549   MALE 2019         9        22      104          3
    ## 513                   0.565    ALL 2019         9        22      104          3
    ## 514                   0.565 FEMALE 2019         9        22      104          3
    ## 515                   0.565   MALE 2019         9        22      104          3
    ## 516                   0.523    ALL 2019         9        22      104          3
    ## 517                   0.530 FEMALE 2019         9        22      104          3
    ## 518                   0.516   MALE 2019         9        22      104          3
    ## 519                   0.559    ALL 2019         9        35       96          1
    ## 520                   0.578 FEMALE 2019         9        35       96          1
    ## 521                   0.541   MALE 2019         9        35       96          1
    ## 522                   0.571    ALL 2019         9        35       96          1
    ## 523                   0.612 FEMALE 2019         9        35       96          1
    ## 524                   0.535   MALE 2019         9        35       96          1
    ## 525                   0.555    ALL 2019         9        35       96          1
    ## 526                   0.531 FEMALE 2019         9        35       96          1
    ## 527                   0.579   MALE 2019         9        35       96          1
    ## 528                   0.619    ALL 2019        11        14       34          4
    ## 529                   0.658 FEMALE 2019        11        14       34          4
    ## 530                   0.582   MALE 2019        11        14       34          4
    ## 531                   0.640    ALL 2019        11        14       34          4
    ## 532                   0.688 FEMALE 2019        11        14       34          4
    ## 533                   0.587   MALE 2019        11        14       34          4
    ## 534                   0.599    ALL 2019        11        14       34          4
    ## 535                   0.598 FEMALE 2019        11        14       34          4
    ## 536                   0.600   MALE 2019        11        14       34          4
    ## 537                   0.623    ALL 2019       101       280      600         16
    ## 538                   0.637 FEMALE 2019       101       280      600         16
    ## 539                   0.609   MALE 2019       101       280      600         16
    ## 540                   0.642    ALL 2019       101       280      600         16
    ## 541                   0.656 FEMALE 2019       101       280      600         16
    ## 542                   0.630   MALE 2019       101       280      600         16
    ## 543                   0.576    ALL 2019       101       280      600         16
    ## 544                   0.595 FEMALE 2019       101       280      600         16
    ## 545                   0.557   MALE 2019       101       280      600         16
    ## 546                   0.588    ALL 2019         3         6       29          5
    ## 547                   0.596 FEMALE 2019         3         6       29          5
    ## 548                   0.578   MALE 2019         3         6       29          5
    ## 549                   0.625    ALL 2019         3         6       29          5
    ## 550                   0.649 FEMALE 2019         3         6       29          5
    ## 551                   0.603   MALE 2019         3         6       29          5
    ## 552                   0.541    ALL 2019         3         6       29          5
    ## 553                   0.542 FEMALE 2019         3         6       29          5
    ## 554                   0.540   MALE 2019         3         6       29          5
    ## 555                   0.616    ALL 2019        49       155      357         14
    ## 556                   0.627 FEMALE 2019        49       155      357         14
    ## 557                   0.608   MALE 2019        49       155      357         14
    ## 558                   0.664    ALL 2019        49       155      357         14
    ## 559                   0.681 FEMALE 2019        49       155      357         14
    ## 560                   0.650   MALE 2019        49       155      357         14
    ## 561                   0.543    ALL 2019        49       155      357         14
    ## 562                   0.536 FEMALE 2019        49       155      357         14
    ## 563                   0.548   MALE 2019        49       155      357         14
    ## 564                   0.669    ALL 2019        17        10       89          2
    ## 565                   0.619 FEMALE 2019        17        10       89          2
    ## 566                   0.711   MALE 2019        17        10       89          2
    ## 567                   0.716    ALL 2019        17        10       89          2
    ## 568                   0.661 FEMALE 2019        17        10       89          2
    ## 569                   0.756   MALE 2019        17        10       89          2
    ## 570                   0.639    ALL 2019        17        10       89          2
    ## 571                   0.627 FEMALE 2019        17        10       89          2
    ## 572                   0.651   MALE 2019        17        10       89          2
    ## 573                   0.552    ALL 2019         9        13       40          7
    ## 574                   0.541 FEMALE 2019         9        13       40          7
    ## 575                   0.565   MALE 2019         9        13       40          7
    ## 576                   0.583    ALL 2019         9        13       40          7
    ## 577                   0.578 FEMALE 2019         9        13       40          7
    ## 578                   0.589   MALE 2019         9        13       40          7
    ## 579                   0.489    ALL 2019         9        13       40          7
    ## 580                   0.466 FEMALE 2019         9        13       40          7
    ## 581                   0.515   MALE 2019         9        13       40          7
    ## 582                   0.619    ALL 2019        16        11       59          3
    ## 583                   0.629 FEMALE 2019        16        11       59          3
    ## 584                   0.610   MALE 2019        16        11       59          3
    ## 585                   0.682    ALL 2019        16        11       59          3
    ## 586                   0.697 FEMALE 2019        16        11       59          3
    ## 587                   0.670   MALE 2019        16        11       59          3
    ## 588                   0.542    ALL 2019        16        11       59          3
    ## 589                   0.550 FEMALE 2019        16        11       59          3
    ## 590                   0.536   MALE 2019        16        11       59          3
    ## 591                   0.711    ALL 2019       385       238      232          1
    ## 592                   0.694 FEMALE 2019       385       238      232          1
    ## 593                   0.727   MALE 2019       385       238      232          1
    ## 594                   0.728    ALL 2019       385       238      232          1
    ## 595                   0.735 FEMALE 2019       385       238      232          1
    ## 596                   0.722   MALE 2019       385       238      232          1
    ## 597                   0.690    ALL 2019       385       238      232          1
    ## 598                   0.639 FEMALE 2019       385       238      232          1
    ## 599                   0.750   MALE 2019       385       238      232          1
    ## 600                   0.588    ALL 2019        72       636      744          5
    ## 601                   0.595 FEMALE 2019        72       636      744          5
    ## 602                   0.581   MALE 2019        72       636      744          5
    ## 603                   0.611    ALL 2019        72       636      744          5
    ## 604                   0.627 FEMALE 2019        72       636      744          5
    ## 605                   0.596   MALE 2019        72       636      744          5
    ## 606                   0.549    ALL 2019        72       636      744          5
    ## 607                   0.540 FEMALE 2019        72       636      744          5
    ## 608                   0.558   MALE 2019        72       636      744          5
    ## 609                   0.651    ALL 2019       114       340      448          9
    ## 610                   0.652 FEMALE 2019       114       340      448          9
    ## 611                   0.649   MALE 2019       114       340      448          9
    ## 612                   0.636    ALL 2019       114       340      448          9
    ## 613                   0.618 FEMALE 2019       114       340      448          9
    ## 614                   0.652   MALE 2019       114       340      448          9
    ## 615                   0.673    ALL 2019       114       340      448          9
    ## 616                   0.698 FEMALE 2019       114       340      448          9
    ## 617                   0.645   MALE 2019       114       340      448          9
    ## 618                   0.640    ALL 2019        47        94      254          6
    ## 619                   0.640 FEMALE 2019        47        94      254          6
    ## 620                   0.640   MALE 2019        47        94      254          6
    ## 621                   0.660    ALL 2019        47        94      254          6
    ## 622                   0.637 FEMALE 2019        47        94      254          6
    ## 623                   0.682   MALE 2019        47        94      254          6
    ## 624                   0.615    ALL 2019        47        94      254          6
    ## 625                   0.653 FEMALE 2019        47        94      254          6
    ## 626                   0.577   MALE 2019        47        94      254          6
    ## 627                   0.633    ALL 2019         4        74      132          4
    ## 628                   0.606 FEMALE 2019         4        74      132          4
    ## 629                   0.657   MALE 2019         4        74      132          4
    ## 630                   0.703    ALL 2019         4        74      132          4
    ## 631                   0.669 FEMALE 2019         4        74      132          4
    ## 632                   0.733   MALE 2019         4        74      132          4
    ## 633                   0.566    ALL 2019         4        74      132          4
    ## 634                   0.542 FEMALE 2019         4        74      132          4
    ## 635                   0.589   MALE 2019         4        74      132          4
    ## 636                   0.622    ALL 2019        81       301      261          9
    ## 637                   0.640 FEMALE 2019        81       301      261          9
    ## 638                   0.606   MALE 2019        81       301      261          9
    ## 639                   0.634    ALL 2019        81       301      261          9
    ## 640                   0.677 FEMALE 2019        81       301      261          9
    ## 641                   0.596   MALE 2019        81       301      261          9
    ## 642                   0.606    ALL 2019        81       301      261          9
    ## 643                   0.590 FEMALE 2019        81       301      261          9
    ## 644                   0.621   MALE 2019        81       301      261          9
    ## 645                   0.634    ALL 2019       834       752      440         10
    ## 646                   0.654 FEMALE 2019       834       752      440         10
    ## 647                   0.615   MALE 2019       834       752      440         10
    ## 648                   0.667    ALL 2019       834       752      440         10
    ## 649                   0.673 FEMALE 2019       834       752      440         10
    ## 650                   0.661   MALE 2019       834       752      440         10
    ## 651                   0.580    ALL 2019       834       752      440         10
    ## 652                   0.624 FEMALE 2019       834       752      440         10
    ## 653                   0.538   MALE 2019       834       752      440         10
    ## 654                   0.642    ALL 2019        39       128      316          8
    ## 655                   0.662 FEMALE 2019        39       128      316          8
    ## 656                   0.624   MALE 2019        39       128      316          8
    ## 657                   0.673    ALL 2019        39       128      316          8
    ## 658                   0.689 FEMALE 2019        39       128      316          8
    ## 659                   0.657   MALE 2019        39       128      316          8
    ## 660                   0.597    ALL 2019        39       128      316          8
    ## 661                   0.618 FEMALE 2019        39       128      316          8
    ## 662                   0.577   MALE 2019        39       128      316          8
    ## 663                   0.674    ALL 2019       244       312      582         13
    ## 664                   0.677 FEMALE 2019       244       312      582         13
    ## 665                   0.671   MALE 2019       244       312      582         13
    ## 666                   0.702    ALL 2019       244       312      582         13
    ## 667                   0.701 FEMALE 2019       244       312      582         13
    ## 668                   0.702   MALE 2019       244       312      582         13
    ## 669                   0.635    ALL 2019       244       312      582         13
    ## 670                   0.642 FEMALE 2019       244       312      582         13
    ## 671                   0.628   MALE 2019       244       312      582         13
    ## 672                   0.545    ALL 2019        48       176     1885         16
    ## 673                   0.540 FEMALE 2019        48       176     1885         16
    ## 674                   0.549   MALE 2019        48       176     1885         16
    ## 675                   0.565    ALL 2019        48       176     1885         16
    ## 676                   0.567 FEMALE 2019        48       176     1885         16
    ## 677                   0.563   MALE 2019        48       176     1885         16
    ## 678                   0.502    ALL 2019        48       176     1885         16
    ## 679                   0.478 FEMALE 2019        48       176     1885         16
    ## 680                   0.519   MALE 2019        48       176     1885         16
    ## 681                   0.530    ALL 2019        90      2537     4274          4
    ## 682                   0.535 FEMALE 2019        90      2537     4274          4
    ## 683                   0.526   MALE 2019        90      2537     4274          4
    ## 684                   0.526    ALL 2019        90      2537     4274          4
    ## 685                   0.526 FEMALE 2019        90      2537     4274          4
    ## 686                   0.526   MALE 2019        90      2537     4274          4
    ## 687                   0.537    ALL 2019        90      2537     4274          4
    ## 688                   0.552 FEMALE 2019        90      2537     4274          4
    ## 689                   0.525   MALE 2019        90      2537     4274          4
    ## 690                   0.666    ALL 2019      1773       333     1575          5
    ## 691                   0.685 FEMALE 2019      1773       333     1575          5
    ## 692                   0.647   MALE 2019      1773       333     1575          5
    ## 693                   0.708    ALL 2019      1773       333     1575          5
    ## 694                   0.728 FEMALE 2019      1773       333     1575          5
    ## 695                   0.687   MALE 2019      1773       333     1575          5
    ## 696                   0.618    ALL 2019      1773       333     1575          5
    ## 697                   0.634 FEMALE 2019      1773       333     1575          5
    ## 698                   0.602   MALE 2019      1773       333     1575          5
    ## 699                   0.696    ALL 2019       843       104     1579         12
    ## 700                   0.698 FEMALE 2019       843       104     1579         12
    ## 701                   0.694   MALE 2019       843       104     1579         12
    ## 702                   0.717    ALL 2019       843       104     1579         12
    ## 703                   0.708 FEMALE 2019       843       104     1579         12
    ## 704                   0.726   MALE 2019       843       104     1579         12
    ## 705                   0.663    ALL 2019       843       104     1579         12
    ## 706                   0.681 FEMALE 2019       843       104     1579         12
    ## 707                   0.646   MALE 2019       843       104     1579         12
    ## 708                   0.653    ALL 2019        54        18      178          1
    ## 709                   0.677 FEMALE 2019        54        18      178          1
    ## 710                   0.632   MALE 2019        54        18      178          1
    ## 711                   0.685    ALL 2019        54        18      178          1
    ## 712                   0.719 FEMALE 2019        54        18      178          1
    ## 713                   0.656   MALE 2019        54        18      178          1
    ## 714                   0.630    ALL 2019        54        18      178          1
    ## 715                   0.655 FEMALE 2019        54        18      178          1
    ## 716                   0.611   MALE 2019        54        18      178          1
    ## 717                   0.539    ALL 2019        62      1511     4952         29
    ## 718                   0.550 FEMALE 2019        62      1511     4952         29
    ## 719                   0.529   MALE 2019        62      1511     4952         29
    ## 720                   0.534    ALL 2019        62      1511     4952         29
    ## 721                   0.548 FEMALE 2019        62      1511     4952         29
    ## 722                   0.522   MALE 2019        62      1511     4952         29
    ## 723                   0.549    ALL 2019        62      1511     4952         29
    ## 724                   0.552 FEMALE 2019        62      1511     4952         29
    ## 725                   0.545   MALE 2019        62      1511     4952         29
    ## 726                   0.715    ALL 2019       455       412      565         13
    ## 727                   0.718 FEMALE 2019       455       412      565         13
    ## 728                   0.711   MALE 2019       455       412      565         13
    ## 729                   0.715    ALL 2019       455       412      565         13
    ## 730                   0.718 FEMALE 2019       455       412      565         13
    ## 731                   0.711   MALE 2019       455       412      565         13
    ## 732                   0.556    ALL 2019       114       435     1624         26
    ## 733                   0.604 FEMALE 2019       114       435     1624         26
    ## 734                   0.506   MALE 2019       114       435     1624         26
    ## 735                   0.584    ALL 2019       114       435     1624         26
    ## 736                   0.626 FEMALE 2019       114       435     1624         26
    ## 737                   0.540   MALE 2019       114       435     1624         26
    ## 738                   0.487    ALL 2019       114       435     1624         26
    ## 739                   0.549 FEMALE 2019       114       435     1624         26
    ## 740                   0.421   MALE 2019       114       435     1624         26
    ## 741                   0.719    ALL 2019        85       224      525          1
    ## 742                   0.726 FEMALE 2019        85       224      525          1
    ## 743                   0.712   MALE 2019        85       224      525          1
    ## 744                   0.707    ALL 2019        85       224      525          1
    ## 745                   0.717 FEMALE 2019        85       224      525          1
    ## 746                   0.698   MALE 2019        85       224      525          1
    ## 747                   0.738    ALL 2019        85       224      525          1
    ## 748                   0.739 FEMALE 2019        85       224      525          1
    ## 749                   0.737   MALE 2019        85       224      525          1
    ## 750                   0.684    ALL 2019       274        45      257          3
    ## 751                   0.675 FEMALE 2019       274        45      257          3
    ## 752                   0.689   MALE 2019       274        45      257          3
    ## 753                   0.684    ALL 2019       274        45      257          3
    ## 754                   0.675 FEMALE 2019       274        45      257          3
    ## 755                   0.689   MALE 2019       274        45      257          3
    ## 756                   0.577    ALL 2019       124       369      813          3
    ## 757                   0.553 FEMALE 2019       124       369      813          3
    ## 758                   0.601   MALE 2019       124       369      813          3
    ## 759                   0.580    ALL 2019       124       369      813          3
    ## 760                   0.606 FEMALE 2019       124       369      813          3
    ## 761                   0.551   MALE 2019       124       369      813          3
    ## 762                   0.577    ALL 2019       124       369      813          3
    ## 763                   0.496 FEMALE 2019       124       369      813          3
    ## 764                   0.655   MALE 2019       124       369      813          3
    ## 765                   0.706    ALL 2019        91        37      164          2
    ## 766                   0.752 FEMALE 2019        91        37      164          2
    ## 767                   0.670   MALE 2019        91        37      164          2
    ## 768                   0.706    ALL 2019        91        37      164          2
    ## 769                   0.752 FEMALE 2019        91        37      164          2
    ## 770                   0.670   MALE 2019        91        37      164          2
    ## 771                   0.618    ALL 2019       387       534      422          8
    ## 772                   0.583 FEMALE 2019       387       534      422          8
    ## 773                   0.651   MALE 2019       387       534      422          8
    ## 774                   0.618    ALL 2019       387       534      422          8
    ## 775                   0.583 FEMALE 2019       387       534      422          8
    ## 776                   0.651   MALE 2019       387       534      422          8
    ## 777                   0.737    ALL 2019       619       105      271          2
    ## 778                   0.760 FEMALE 2019       619       105      271          2
    ## 779                   0.714   MALE 2019       619       105      271          2
    ## 780                   0.746    ALL 2019       619       105      271          2
    ## 781                   0.782 FEMALE 2019       619       105      271          2
    ## 782                   0.708   MALE 2019       619       105      271          2
    ## 783                   0.728    ALL 2019       619       105      271          2
    ## 784                   0.737 FEMALE 2019       619       105      271          2
    ## 785                   0.721   MALE 2019       619       105      271          2
    ## 786                   0.706    ALL 2019      1057         9      264          5
    ## 787                   0.734 FEMALE 2019      1057         9      264          5
    ## 788                   0.679   MALE 2019      1057         9      264          5
    ## 789                   0.706    ALL 2019      1057         9      264          5
    ## 790                   0.734 FEMALE 2019      1057         9      264          5
    ## 791                   0.679   MALE 2019      1057         9      264          5
    ## 792                   0.732    ALL 2019      2959        70      618          2
    ## 793                   0.773 FEMALE 2019      2959        70      618          2
    ## 794                   0.693   MALE 2019      2959        70      618          2
    ## 795                   0.737    ALL 2019      2959        70      618          2
    ## 796                   0.772 FEMALE 2019      2959        70      618          2
    ## 797                   0.704   MALE 2019      2959        70      618          2
    ## 798                   0.723    ALL 2019      2959        70      618          2
    ## 799                   0.773 FEMALE 2019      2959        70      618          2
    ## 800                   0.676   MALE 2019      2959        70      618          2
    ## 801                   0.734    ALL 2019       163         7      317          6
    ## 802                   0.744 FEMALE 2019       163         7      317          6
    ## 803                   0.725   MALE 2019       163         7      317          6
    ## 804                   0.711    ALL 2019       163         7      317          6
    ## 805                   0.712 FEMALE 2019       163         7      317          6
    ## 806                   0.711   MALE 2019       163         7      317          6
    ## 807                   0.774    ALL 2019       163         7      317          6
    ## 808                   0.797 FEMALE 2019       163         7      317          6
    ## 809                   0.750   MALE 2019       163         7      317          6
    ## 810                   0.699    ALL 2019      1268        10      302          1
    ## 811                   0.716 FEMALE 2019      1268        10      302          1
    ## 812                   0.684   MALE 2019      1268        10      302          1
    ## 813                   0.711    ALL 2019      1268        10      302          1
    ## 814                   0.736 FEMALE 2019      1268        10      302          1
    ## 815                   0.691   MALE 2019      1268        10      302          1
    ## 816                   0.683    ALL 2019      1268        10      302          1
    ## 817                   0.691 FEMALE 2019      1268        10      302          1
    ## 818                   0.676   MALE 2019      1268        10      302          1
    ## 819                   0.659    ALL 2019        58        40      414          4
    ## 820                   0.668 FEMALE 2019        58        40      414          4
    ## 821                   0.648   MALE 2019        58        40      414          4
    ## 822                   0.689    ALL 2019        58        40      414          4
    ## 823                   0.632   MALE 2019        58        40      414          4
    ## 824                   0.640    ALL 2019        58        40      414          4
    ## 825                   0.615 FEMALE 2019        58        40      414          4
    ## 826                   0.667   MALE 2019        58        40      414          4
    ## 827                   0.772    ALL 2019      2015        71       95          3
    ## 828                   0.807 FEMALE 2019      2015        71       95          3
    ## 829                   0.743   MALE 2019      2015        71       95          3
    ## 830                   0.837    ALL 2019      2015        71       95          3
    ## 831                   0.846 FEMALE 2019      2015        71       95          3
    ## 832                   0.829   MALE 2019      2015        71       95          3
    ## 833                   0.704    ALL 2019      2015        71       95          3
    ## 834                   0.760 FEMALE 2019      2015        71       95          3
    ## 835                   0.663   MALE 2019      2015        71       95          3
    ## 836                   0.596    ALL 2019      1941       128     1812          9
    ## 837                   0.627 FEMALE 2019      1941       128     1812          9
    ## 838                   0.569   MALE 2019      1941       128     1812          9
    ## 839                   0.612    ALL 2019      1941       128     1812          9
    ## 840                   0.632 FEMALE 2019      1941       128     1812          9
    ## 841                   0.594   MALE 2019      1941       128     1812          9
    ## 842                   0.570    ALL 2019      1941       128     1812          9
    ## 843                   0.619 FEMALE 2019      1941       128     1812          9
    ## 844                   0.524   MALE 2019      1941       128     1812          9
    ## 845                   0.726    ALL 2019       122        27      432         11
    ## 846                   0.754 FEMALE 2019       122        27      432         11
    ## 847                   0.699   MALE 2019       122        27      432         11
    ## 848                   0.762    ALL 2019       122        27      432         11
    ## 849                   0.783 FEMALE 2019       122        27      432         11
    ## 850                   0.743   MALE 2019       122        27      432         11
    ## 851                   0.681    ALL 2019       122        27      432         11
    ## 852                   0.722 FEMALE 2019       122        27      432         11
    ## 853                   0.638   MALE 2019       122        27      432         11
    ## 854                   0.690    ALL 2019        39        26       50          6
    ## 855                   0.726 FEMALE 2019        39        26       50          6
    ## 856                   0.659   MALE 2019        39        26       50          6
    ## 857                   0.712    ALL 2019        39        26       50          6
    ## 858                   0.738 FEMALE 2019        39        26       50          6
    ## 859                   0.689   MALE 2019        39        26       50          6
    ## 860                   0.657    ALL 2019        39        26       50          6
    ## 861                   0.710 FEMALE 2019        39        26       50          6
    ## 862                   0.612   MALE 2019        39        26       50          6
    ## 863                   0.650    ALL 2019        25       475      427         21
    ## 864                   0.655 FEMALE 2019        25       475      427         21
    ## 865                   0.645   MALE 2019        25       475      427         21
    ## 866                   0.659    ALL 2019        25       475      427         21
    ## 867                   0.662 FEMALE 2019        25       475      427         21
    ## 868                   0.656   MALE 2019        25       475      427         21
    ## 869                   0.638    ALL 2019        25       475      427         21
    ## 870                   0.640 FEMALE 2019        25       475      427         21
    ## 871                   0.635   MALE 2019        25       475      427         21
    ## 872                   0.673    ALL 2019        34        21       53          6
    ## 873                   0.689 FEMALE 2019        34        21       53          6
    ## 874                   0.657   MALE 2019        34        21       53          6
    ## 875                   0.706    ALL 2019        34        21       53          6
    ## 876                   0.735 FEMALE 2019        34        21       53          6
    ## 877                   0.675   MALE 2019        34        21       53          6
    ## 878                   0.641    ALL 2019        34        21       53          6
    ## 879                   0.644 FEMALE 2019        34        21       53          6
    ## 880                   0.638   MALE 2019        34        21       53          6
    ## 881                   0.625    ALL 2019         2         5       33         11
    ## 882                   0.611 FEMALE 2019         2         5       33         11
    ## 883                   0.639   MALE 2019         2         5       33         11
    ## 884                   0.672    ALL 2019         2         5       33         11
    ## 885                   0.587 FEMALE 2019         2         5       33         11
    ## 886                   0.621    ALL 2019         2         5       33         11
    ## 887                   0.635 FEMALE 2019         2         5       33         11
    ## 888                   0.609   MALE 2019         2         5       33         11
    ## 889                   0.669    ALL 2019        10        15       21          1
    ## 890                   0.613 FEMALE 2019        10        15       21          1
    ## 891                   0.714   MALE 2019        10        15       21          1
    ## 892                   0.664    ALL 2019        10        15       21          1
    ## 893                   0.574 FEMALE 2019        10        15       21          1
    ## 894                   0.733   MALE 2019        10        15       21          1
    ## 895                   0.755    ALL 2019        10        15       21          1
    ## 896                   0.587    ALL 2019         5         5       32          1
    ## 897                   0.550 FEMALE 2019         5         5       32          1
    ## 898                   0.630   MALE 2019         5         5       32          1
    ## 899                   0.598    ALL 2019         5         5       32          1
    ## 900                   0.557 FEMALE 2019         5         5       32          1
    ## 901                   0.646   MALE 2019         5         5       32          1
    ## 902                   0.566    ALL 2019         5         5       32          1
    ## 903                   0.536 FEMALE 2019         5         5       32          1
    ## 904                   0.600   MALE 2019         5         5       32          1
    ## 905                   0.597    ALL 2019        71       458      489         14
    ## 906                   0.591 FEMALE 2019        71       458      489         14
    ## 907                   0.602   MALE 2019        71       458      489         14
    ## 908                   0.611    ALL 2019        71       458      489         14
    ## 909                   0.612 FEMALE 2019        71       458      489         14
    ## 910                   0.611   MALE 2019        71       458      489         14
    ## 911                   0.553    ALL 2019        71       458      489         14
    ## 912                   0.528 FEMALE 2019        71       458      489         14
    ## 913                   0.577   MALE 2019        71       458      489         14
    ## 914                   0.663    ALL 2019        39        43      134          2
    ## 915                   0.699 FEMALE 2019        39        43      134          2
    ## 916                   0.633   MALE 2019        39        43      134          2
    ## 917                   0.666    ALL 2019        39        43      134          2
    ## 918                   0.669 FEMALE 2019        39        43      134          2
    ## 919                   0.663   MALE 2019        39        43      134          2
    ## 920                   0.664    ALL 2019        39        43      134          2
    ## 921                   0.754 FEMALE 2019        39        43      134          2
    ## 922                   0.599   MALE 2019        39        43      134          2
    ## 923                   0.667    ALL 2019       110       178       97         20
    ## 924                   0.674 FEMALE 2019       110       178       97         20
    ## 925                   0.661   MALE 2019       110       178       97         20
    ## 926                   0.664    ALL 2019       110       178       97         20
    ## 927                   0.672 FEMALE 2019       110       178       97         20
    ## 928                   0.657   MALE 2019       110       178       97         20
    ## 929                   0.674    ALL 2019       110       178       97         20
    ## 930                   0.679 FEMALE 2019       110       178       97         20
    ## 931                   0.669   MALE 2019       110       178       97         20
    ## 932                   0.699    ALL 2019       254       195      104          9
    ## 933                   0.709 FEMALE 2019       254       195      104          9
    ## 934                   0.689   MALE 2019       254       195      104          9
    ## 935                   0.704    ALL 2019       254       195      104          9
    ## 936                   0.724 FEMALE 2019       254       195      104          9
    ## 937                   0.685   MALE 2019       254       195      104          9
    ## 938                   0.694    ALL 2019       254       195      104          9
    ## 939                   0.695 FEMALE 2019       254       195      104          9
    ## 940                   0.693   MALE 2019       254       195      104          9
    ## 941                   0.634    ALL 2019         8         2       17          1
    ## 942                   0.624 FEMALE 2019         8         2       17          1
    ## 943                   0.643   MALE 2019         8         2       17          1
    ## 944                   0.654    ALL 2019         8         2       17          1
    ## 945                   0.590 FEMALE 2019         8         2       17          1
    ## 946                   0.710   MALE 2019         8         2       17          1
    ## 947                   0.602    ALL 2019         8         2       17          1
    ## 948                   0.675 FEMALE 2019         8         2       17          1
    ## 949                   0.535   MALE 2019         8         2       17          1
    ## 950                   0.609    ALL 2019        11       108      135         23
    ## 951                   0.579 FEMALE 2019        11       108      135         23
    ## 952                   0.643   MALE 2019        11       108      135         23
    ## 953                   0.636    ALL 2019        11       108      135         23
    ## 954                   0.627 FEMALE 2019        11       108      135         23
    ## 955                   0.645   MALE 2019        11       108      135         23
    ## 956                   0.573    ALL 2019        11       108      135         23
    ## 957                   0.513 FEMALE 2019        11       108      135         23
    ## 958                   0.646   MALE 2019        11       108      135         23
    ## 959                   0.666    ALL 2019        46       122      232         12
    ## 960                   0.664 FEMALE 2019        46       122      232         12
    ## 961                   0.668   MALE 2019        46       122      232         12
    ## 962                   0.676    ALL 2019        46       122      232         12
    ## 963                   0.683 FEMALE 2019        46       122      232         12
    ## 964                   0.671   MALE 2019        46       122      232         12
    ## 965                   0.650    ALL 2019        46       122      232         12
    ## 966                   0.637 FEMALE 2019        46       122      232         12
    ## 967                   0.662   MALE 2019        46       122      232         12
    ## 968                   0.732    ALL 2019       442       118      134         13
    ## 969                   0.737 FEMALE 2019       442       118      134         13
    ## 970                   0.727   MALE 2019       442       118      134         13
    ## 971                   0.743    ALL 2019       442       118      134         13
    ## 972                   0.734 FEMALE 2019       442       118      134         13
    ## 973                   0.751   MALE 2019       442       118      134         13
    ## 974                   0.716    ALL 2019       442       118      134         13
    ## 975                   0.741 FEMALE 2019       442       118      134         13
    ## 976                   0.692   MALE 2019       442       118      134         13
    ## 977                   0.574    ALL 2019         5        92       41         28
    ## 978                   0.558 FEMALE 2019         5        92       41         28
    ## 979                   0.588   MALE 2019         5        92       41         28
    ## 980                   0.581    ALL 2019         5        92       41         28
    ## 981                   0.608 FEMALE 2019         5        92       41         28
    ## 982                   0.559   MALE 2019         5        92       41         28
    ## 983                   0.568    ALL 2019         5        92       41         28
    ## 984                   0.492 FEMALE 2019         5        92       41         28
    ## 985                   0.644   MALE 2019         5        92       41         28
    ## 986                   0.567    ALL 2019      1529     10230     2991        215
    ## 987                   0.557 FEMALE 2019      1529     10230     2991        215
    ## 988                   0.576   MALE 2019      1529     10230     2991        215
    ## 989                   0.590    ALL 2019      1529     10230     2991        215
    ## 990                   0.592 FEMALE 2019      1529     10230     2991        215
    ## 991                   0.589   MALE 2019      1529     10230     2991        215
    ## 992                   0.515    ALL 2019      1529     10230     2991        215
    ## 993                   0.479 FEMALE 2019      1529     10230     2991        215
    ## 994                   0.547   MALE 2019      1529     10230     2991        215
    ## 995                   0.663    ALL 2019         6         5       12          7
    ## 996                   0.604 FEMALE 2019         6         5       12          7
    ## 997                   0.718   MALE 2019         6         5       12          7
    ## 998                   0.742    ALL 2019         6         5       12          7
    ## 999                   0.689 FEMALE 2019         6         5       12          7
    ## 1000                  0.792   MALE 2019         6         5       12          7
    ## 1001                  0.500    ALL 2019         6         5       12          7
    ## 1002                  0.432 FEMALE 2019         6         5       12          7
    ## 1003                  0.565   MALE 2019         6         5       12          7
    ## 1004                  0.570    ALL 2019        42       233      708          1
    ## 1005                  0.561 FEMALE 2019        42       233      708          1
    ## 1006                  0.579   MALE 2019        42       233      708          1
    ## 1007                  0.600    ALL 2019        42       233      708          1
    ## 1008                  0.594 FEMALE 2019        42       233      708          1
    ## 1009                  0.607   MALE 2019        42       233      708          1
    ## 1010                  0.536    ALL 2019        42       233      708          1
    ## 1011                  0.517 FEMALE 2019        42       233      708          1
    ## 1012                  0.556   MALE 2019        42       233      708          1
    ## 1013                  0.566    ALL 2019         4         3       22          1
    ## 1014                  0.524 FEMALE 2019         4         3       22          1
    ## 1015                  0.606   MALE 2019         4         3       22          1
    ## 1016                  0.609    ALL 2019         4         3       22          1
    ## 1017                  0.591 FEMALE 2019         4         3       22          1
    ## 1018                  0.627   MALE 2019         4         3       22          1
    ## 1019                  0.506    ALL 2019         4         3       22          1
    ## 1020                  0.427 FEMALE 2019         4         3       22          1
    ## 1021                  0.578   MALE 2019         4         3       22          1
    ## 1022                  0.578    ALL 2019         4         2       51          2
    ## 1023                  0.556 FEMALE 2019         4         2       51          2
    ## 1024                  0.597   MALE 2019         4         2       51          2
    ## 1025                  0.599    ALL 2019         4         2       51          2
    ## 1026                  0.550 FEMALE 2019         4         2       51          2
    ## 1027                  0.646   MALE 2019         4         2       51          2
    ## 1028                  0.549    ALL 2019         4         2       51          2
    ## 1029                  0.565 FEMALE 2019         4         2       51          2
    ## 1030                  0.537   MALE 2019         4         2       51          2
    ## 1031                  0.723    ALL 2019       127       118      322         17
    ## 1032                  0.754 FEMALE 2019       127       118      322         17
    ## 1033                  0.691   MALE 2019       127       118      322         17
    ## 1034                  0.723    ALL 2019       127       118      322         17
    ## 1035                  0.727 FEMALE 2019       127       118      322         17
    ## 1036                  0.718   MALE 2019       127       118      322         17
    ## 1037                  0.724    ALL 2019       127       118      322         17
    ## 1038                  0.801 FEMALE 2019       127       118      322         17
    ## 1039                  0.645   MALE 2019       127       118      322         17
    ## 1040                  0.656    ALL 2019        95       357      996         10
    ## 1041                  0.668 FEMALE 2019        95       357      996         10
    ## 1042                  0.645   MALE 2019        95       357      996         10
    ## 1043                  0.702    ALL 2019        95       357      996         10
    ## 1044                  0.699 FEMALE 2019        95       357      996         10
    ## 1045                  0.704   MALE 2019        95       357      996         10
    ## 1046                  0.611    ALL 2019        95       357      996         10
    ## 1047                  0.641 FEMALE 2019        95       357      996         10
    ## 1048                  0.586   MALE 2019        95       357      996         10
    ## 1049                  0.651    ALL 2019       157       188      651          6
    ## 1050                  0.667 FEMALE 2019       157       188      651          6
    ## 1051                  0.638   MALE 2019       157       188      651          6
    ## 1052                  0.686    ALL 2019       157       188      651          6
    ## 1053                  0.679 FEMALE 2019       157       188      651          6
    ## 1054                  0.693   MALE 2019       157       188      651          6
    ## 1055                  0.621    ALL 2019       157       188      651          6
    ## 1056                  0.667 FEMALE 2019       157       188      651          6
    ## 1057                  0.581   MALE 2019       157       188      651          6
    ## 1058                  0.672    ALL 2019       444       563     2202         10
    ## 1059                  0.685 FEMALE 2019       444       563     2202         10
    ## 1060                  0.659   MALE 2019       444       563     2202         10
    ## 1061                  0.704    ALL 2019       444       563     2202         10
    ## 1062                  0.738 FEMALE 2019       444       563     2202         10
    ## 1063                  0.670   MALE 2019       444       563     2202         10
    ## 1064                  0.629    ALL 2019       444       563     2202         10
    ## 1065                  0.613 FEMALE 2019       444       563     2202         10
    ## 1066                  0.644   MALE 2019       444       563     2202         10
    ## 1067                  0.515    ALL 2019       261      2517     6367         29
    ## 1068                  0.526 FEMALE 2019       261      2517     6367         29
    ## 1069                  0.505   MALE 2019       261      2517     6367         29
    ## 1070                  0.516    ALL 2019       261      2517     6367         29
    ## 1071                  0.544 FEMALE 2019       261      2517     6367         29
    ## 1072                  0.490   MALE 2019       261      2517     6367         29
    ## 1073                  0.513    ALL 2019       261      2517     6367         29
    ## 1074                  0.490 FEMALE 2019       261      2517     6367         29
    ## 1075                  0.534   MALE 2019       261      2517     6367         29
    ## 1076                  0.717    ALL 2019        83       136      551          4
    ## 1077                  0.727 FEMALE 2019        83       136      551          4
    ## 1078                  0.708   MALE 2019        83       136      551          4
    ## 1079                  0.741    ALL 2019        83       136      551          4
    ## 1080                  0.742 FEMALE 2019        83       136      551          4
    ## 1081                  0.740   MALE 2019        83       136      551          4
    ## 1082                  0.668    ALL 2019        83       136      551          4
    ## 1083                  0.695 FEMALE 2019        83       136      551          4
    ## 1084                  0.644   MALE 2019        83       136      551          4
    ## 1085                  0.578    ALL 2019         1         4       41          1
    ## 1086                  0.587 FEMALE 2019         1         4       41          1
    ## 1087                  0.569   MALE 2019         1         4       41          1
    ## 1088                  0.584    ALL 2019         1         4       41          1
    ## 1089                  0.605 FEMALE 2019         1         4       41          1
    ## 1090                  0.562   MALE 2019         1         4       41          1
    ## 1091                  0.598    ALL 2019         1         4       41          1
    ## 1092                  0.556 FEMALE 2019         1         4       41          1
    ## 1093                  0.635   MALE 2019         1         4       41          1
    ## 1094                  0.560    ALL 2019         5        11       76          1
    ## 1095                  0.563 FEMALE 2019         5        11       76          1
    ## 1096                  0.556   MALE 2019         5        11       76          1
    ## 1097                  0.622    ALL 2019         5        11       76          1
    ## 1098                  0.612 FEMALE 2019         5        11       76          1
    ## 1099                  0.633   MALE 2019         5        11       76          1
    ## 1100                  0.464    ALL 2019         5        11       76          1
    ## 1101                  0.487 FEMALE 2019         5        11       76          1
    ## 1102                  0.440   MALE 2019         5        11       76          1
    ## 1103                  0.526    ALL 2019         6         8       18          6
    ## 1104                  0.511 FEMALE 2019         6         8       18          6
    ## 1105                  0.545   MALE 2019         6         8       18          6
    ## 1106                  0.560    ALL 2019         6         8       18          6
    ## 1107                  0.561 FEMALE 2019         6         8       18          6
    ## 1108                  0.560   MALE 2019         6         8       18          6
    ## 1109                  0.465    ALL 2019         6         8       18          6
    ## 1110                  0.427 FEMALE 2019         6         8       18          6
    ## 1111                  0.515   MALE 2019         6         8       18          6
    ## 1112                  0.600    ALL 2019        19        39      190          8
    ## 1113                  0.613 FEMALE 2019        19        39      190          8
    ## 1114                  0.590   MALE 2019        19        39      190          8
    ## 1115                  0.620    ALL 2019        19        39      190          8
    ## 1116                  0.618 FEMALE 2019        19        39      190          8
    ## 1117                  0.621   MALE 2019        19        39      190          8
    ## 1118                  0.564    ALL 2019        19        39      190          8
    ## 1119                  0.606 FEMALE 2019        19        39      190          8
    ## 1120                  0.528   MALE 2019        19        39      190          8
    ## 1121                  0.537    ALL 2019         2         7       25          3
    ## 1122                  0.521 FEMALE 2019         2         7       25          3
    ## 1123                  0.550   MALE 2019         2         7       25          3
    ## 1124                  0.550    ALL 2019         2         7       25          3
    ## 1125                  0.530 FEMALE 2019         2         7       25          3
    ## 1126                  0.566   MALE 2019         2         7       25          3
    ## 1127                  0.520    ALL 2019         2         7       25          3
    ## 1128                  0.510 FEMALE 2019         2         7       25          3
    ## 1129                  0.528   MALE 2019         2         7       25          3
    ## 1130                  0.556    ALL 2019         5         3       10          3
    ## 1131                  0.558 FEMALE 2019         5         3       10          3
    ## 1132                  0.553   MALE 2019         5         3       10          3
    ## 1133                  0.596    ALL 2019         5         3       10          3
    ## 1134                  0.593 FEMALE 2019         5         3       10          3
    ## 1135                  0.598   MALE 2019         5         3       10          3
    ## 1136                  0.480    ALL 2019         5         3       10          3
    ## 1137                  0.500 FEMALE 2019         5         3       10          3
    ## 1138                  0.462   MALE 2019         5         3       10          3
    ## 1139                  0.636    ALL 2019         6         9       24          2
    ## 1140                  0.580 FEMALE 2019         6         9       24          2
    ## 1141                  0.691   MALE 2019         6         9       24          2
    ## 1142                  0.678    ALL 2019         6         9       24          2
    ## 1143                  0.603 FEMALE 2019         6         9       24          2
    ## 1144                  0.744   MALE 2019         6         9       24          2
    ## 1145                  0.560    ALL 2019         6         9       24          2
    ## 1146                  0.548 FEMALE 2019         6         9       24          2
    ## 1147                  0.575   MALE 2019         6         9       24          2
    ## 1148                  0.673    ALL 2019        71       101      158          1
    ## 1149                  0.661 FEMALE 2019        71       101      158          1
    ## 1150                  0.683   MALE 2019        71       101      158          1
    ## 1151                  0.680    ALL 2019        71       101      158          1
    ## 1152                  0.631 FEMALE 2019        71       101      158          1
    ## 1153                  0.722   MALE 2019        71       101      158          1
    ## 1154                  0.673    ALL 2019        71       101      158          1
    ## 1155                  0.709 FEMALE 2019        71       101      158          1
    ## 1156                  0.640   MALE 2019        71       101      158          1
    ## 1157                  0.716    ALL 2019        27        15       26          1
    ## 1158                  0.717 FEMALE 2019        27        15       26          1
    ## 1159                  0.715   MALE 2019        27        15       26          1
    ## 1160                  0.768    ALL 2019        27        15       26          1
    ## 1161                  0.737 FEMALE 2019        27        15       26          1
    ## 1162                  0.795   MALE 2019        27        15       26          1
    ## 1163                  0.706    ALL 2019        27        15       26          1
    ## 1164                  0.744 FEMALE 2019        27        15       26          1
    ## 1165                  0.662   MALE 2019        27        15       26          1
    ## 1166                  0.570    ALL 2019        91       107     1170          7
    ## 1167                  0.577 FEMALE 2019        91       107     1170          7
    ## 1168                  0.565   MALE 2019        91       107     1170          7
    ## 1169                  0.597    ALL 2019        91       107     1170          7
    ## 1170                  0.596 FEMALE 2019        91       107     1170          7
    ## 1171                  0.598   MALE 2019        91       107     1170          7
    ## 1172                  0.534    ALL 2019        91       107     1170          7
    ## 1173                  0.551 FEMALE 2019        91       107     1170          7
    ## 1174                  0.519   MALE 2019        91       107     1170          7
    ## 1175                  0.795    ALL 2019        31        12       80          1
    ## 1176                  0.770 FEMALE 2019        31        12       80          1
    ## 1177                  0.818   MALE 2019        31        12       80          1
    ## 1178                  0.860    ALL 2019        31        12       80          1
    ## 1179                  0.814 FEMALE 2019        31        12       80          1
    ## 1180                  0.836    ALL 2019        31        12       80          1
    ## 1181                  0.841   MALE 2019        31        12       80          1
    ## 1182                  0.603    ALL 2019        87        74     1256          4
    ## 1183                  0.607 FEMALE 2019        87        74     1256          4
    ## 1184                  0.600   MALE 2019        87        74     1256          4
    ## 1185                  0.628    ALL 2019        87        74     1256          4
    ## 1186                  0.666 FEMALE 2019        87        74     1256          4
    ## 1187                  0.593   MALE 2019        87        74     1256          4
    ## 1188                  0.574    ALL 2019        87        74     1256          4
    ## 1189                  0.527 FEMALE 2019        87        74     1256          4
    ## 1190                  0.617   MALE 2019        87        74     1256          4
    ## 1191                  0.640    ALL 2019       274       131      231          8
    ## 1192                  0.627 FEMALE 2019       274       131      231          8
    ## 1193                  0.654   MALE 2019       274       131      231          8
    ## 1194                  0.659    ALL 2019       274       131      231          8
    ## 1195                  0.647 FEMALE 2019       274       131      231          8
    ## 1196                  0.673   MALE 2019       274       131      231          8
    ## 1197                  0.615    ALL 2019       274       131      231          8
    ## 1198                  0.600 FEMALE 2019       274       131      231          8
    ## 1199                  0.630   MALE 2019       274       131      231          8
    ## 1200                  0.669    ALL 2019        23        21       90          1
    ## 1201                  0.651 FEMALE 2019        23        21       90          1
    ## 1202                  0.686   MALE 2019        23        21       90          1
    ## 1203                  0.672    ALL 2019        23        21       90          1
    ## 1204                  0.658 FEMALE 2019        23        21       90          1
    ## 1205                  0.685   MALE 2019        23        21       90          1
    ## 1206                  0.665    ALL 2019        23        21       90          1
    ## 1207                  0.641 FEMALE 2019        23        21       90          1
    ## 1208                  0.687   MALE 2019        23        21       90          1
    ## 1209                  0.629    ALL 2019        70      1221      774          9
    ## 1210                  0.564 FEMALE 2019        70      1221      774          9
    ## 1211                  0.685   MALE 2019        70      1221      774          9
    ## 1212                  0.608    ALL 2019        70      1221      774          9
    ## 1213                  0.516 FEMALE 2019        70      1221      774          9
    ## 1214                  0.684   MALE 2019        70      1221      774          9
    ## 1215                  0.659    ALL 2019        70      1221      774          9
    ## 1216                  0.628 FEMALE 2019        70      1221      774          9
    ## 1217                  0.687   MALE 2019        70      1221      774          9
    ## 1218                  0.664    ALL 2019       362       159      622          2
    ## 1219                  0.652 FEMALE 2019       362       159      622          2
    ## 1220                  0.676   MALE 2019       362       159      622          2
    ## 1221                  0.676    ALL 2019       362       159      622          2
    ## 1222                  0.665 FEMALE 2019       362       159      622          2
    ## 1223                  0.687   MALE 2019       362       159      622          2
    ## 1224                  0.663    ALL 2019       362       159      622          2
    ## 1225                  0.652 FEMALE 2019       362       159      622          2
    ## 1226                  0.674   MALE 2019       362       159      622          2
    ## 1227                  0.590    ALL 2019       281      1090     4854         16
    ## 1228                  0.603 FEMALE 2019       281      1090     4854         16
    ## 1229                  0.578   MALE 2019       281      1090     4854         16
    ## 1230                  0.595    ALL 2019       281      1090     4854         16
    ## 1231                  0.607 FEMALE 2019       281      1090     4854         16
    ## 1232                  0.584   MALE 2019       281      1090     4854         16
    ## 1233                  0.582    ALL 2019       281      1090     4854         16
    ## 1234                  0.596 FEMALE 2019       281      1090     4854         16
    ## 1235                  0.570   MALE 2019       281      1090     4854         16
    ## 1236                  0.666    ALL 2019       249        58      525          1
    ## 1237                  0.677 FEMALE 2019       249        58      525          1
    ## 1238                  0.655   MALE 2019       249        58      525          1
    ## 1239                  0.705    ALL 2019       249        58      525          1
    ## 1240                  0.683 FEMALE 2019       249        58      525          1
    ## 1241                  0.727   MALE 2019       249        58      525          1
    ## 1242                  0.622    ALL 2019       249        58      525          1
    ## 1243                  0.671 FEMALE 2019       249        58      525          1
    ## 1244                  0.576   MALE 2019       249        58      525          1
    ## 1245                  0.713    ALL 2019       117        36      343          1
    ## 1246                  0.753 FEMALE 2019       117        36      343          1
    ## 1247                  0.673   MALE 2019       117        36      343          1
    ## 1248                  0.726    ALL 2019       117        36      343          1
    ## 1249                  0.751 FEMALE 2019       117        36      343          1
    ## 1250                  0.703   MALE 2019       117        36      343          1
    ## 1251                  0.697    ALL 2019       117        36      343          1
    ## 1252                  0.754 FEMALE 2019       117        36      343          1
    ## 1253                  0.630   MALE 2019       117        36      343          1
    ## 1254                  0.602    ALL 2019         4         3        5          2
    ## 1255                  0.644 FEMALE 2019         4         3        5          2
    ## 1256                  0.545   MALE 2019         4         3        5          2
    ## 1257                  0.694    ALL 2019         4         3        5          2
    ## 1258                  0.639 FEMALE 2019         4         3        5          2
    ## 1259                  0.508    ALL 2019         6         3       10          6
    ## 1260                  0.500 FEMALE 2019         6         3       10          6
    ## 1261                  0.516   MALE 2019         6         3       10          6
    ## 1262                  0.553    ALL 2019         6         3       10          6
    ## 1263                  0.566 FEMALE 2019         6         3       10          6
    ## 1264                  0.541   MALE 2019         6         3       10          6
    ## 1265                  0.444    ALL 2019         6         3       10          6
    ## 1266                  0.400 FEMALE 2019         6         3       10          6
    ## 1267                  0.481   MALE 2019         6         3       10          6
    ## 1268                  0.624    ALL 2019         3         3       18          2
    ## 1269                  0.608 FEMALE 2019         3         3       18          2
    ## 1270                  0.641   MALE 2019         3         3       18          2
    ## 1271                  0.653    ALL 2019         3         3       18          2
    ## 1272                  0.645 FEMALE 2019         3         3       18          2
    ## 1273                  0.661   MALE 2019         3         3       18          2
    ## 1274                  0.611    ALL 2019         3         3       18          2
    ## 1275                  0.583 FEMALE 2019         3         3       18          2
    ## 1276                  0.641   MALE 2019         3         3       18          2
    ## 1277                  0.618    ALL 2019        22        24      116          2
    ## 1278                  0.593 FEMALE 2019        22        24      116          2
    ## 1279                  0.641   MALE 2019        22        24      116          2
    ## 1280                  0.636    ALL 2019        22        24      116          2
    ## 1281                  0.621 FEMALE 2019        22        24      116          2
    ## 1282                  0.650   MALE 2019        22        24      116          2
    ## 1283                  0.584    ALL 2019        22        24      116          2
    ## 1284                  0.544 FEMALE 2019        22        24      116          2
    ## 1285                  0.623   MALE 2019        22        24      116          2
    ## 1286                  0.717    ALL 2019       129       106      250          1
    ## 1287                  0.718 FEMALE 2019       129       106      250          1
    ## 1288                  0.715   MALE 2019       129       106      250          1
    ## 1289                  0.733    ALL 2019       129       106      250          1
    ## 1290                  0.743 FEMALE 2019       129       106      250          1
    ## 1291                  0.722   MALE 2019       129       106      250          1
    ## 1292                  0.695    ALL 2019       129       106      250          1
    ## 1293                  0.685 FEMALE 2019       129       106      250          1
    ## 1294                  0.704   MALE 2019       129       106      250          1
    ## 1295                  0.732    ALL 2019       694       196      234         10
    ## 1296                  0.758 FEMALE 2019       694       196      234         10
    ## 1297                  0.707   MALE 2019       694       196      234         10
    ## 1298                  0.740    ALL 2019       694       196      234         10
    ## 1299                  0.769 FEMALE 2019       694       196      234         10
    ## 1300                  0.713   MALE 2019       694       196      234         10
    ## 1301                  0.721    ALL 2019       694       196      234         10
    ## 1302                  0.743 FEMALE 2019       694       196      234         10
    ## 1303                  0.698   MALE 2019       694       196      234         10
    ## 1304                  0.576    ALL 2019        99       128      248          9
    ## 1305                  0.564 FEMALE 2019        99       128      248          9
    ## 1306                  0.587   MALE 2019        99       128      248          9
    ## 1307                  0.632    ALL 2019        99       128      248          9
    ## 1308                  0.608 FEMALE 2019        99       128      248          9
    ## 1309                  0.652   MALE 2019        99       128      248          9
    ## 1310                  0.500    ALL 2019        99       128      248          9
    ## 1311                  0.508 FEMALE 2019        99       128      248          9
    ## 1312                  0.493   MALE 2019        99       128      248          9
    ## 1313                  0.763    ALL 2019         4         2       13          2
    ## 1314                  0.757 FEMALE 2019         4         2       13          2
    ## 1315                  0.769   MALE 2019         4         2       13          2
    ## 1316                  0.753    ALL 2019         4         2       13          2
    ## 1317                  0.767 FEMALE 2019         4         2       13          2
    ## 1318                  0.738   MALE 2019         4         2       13          2
    ## 1319                  0.568    ALL 2019        27        33       75         21
    ## 1320                  0.585 FEMALE 2019        27        33       75         21
    ## 1321                  0.553   MALE 2019        27        33       75         21
    ## 1322                  0.580    ALL 2019        27        33       75         21
    ## 1323                  0.531 FEMALE 2019        27        33       75         21
    ## 1324                  0.617   MALE 2019        27        33       75         21
    ## 1325                  0.552    ALL 2019        27        33       75         21
    ## 1326                  0.647 FEMALE 2019        27        33       75         21
    ## 1327                  0.460   MALE 2019        27        33       75         21
    ## 1328                  0.576    ALL 2019         3        35       15          1
    ## 1329                  0.594 FEMALE 2019         3        35       15          1
    ## 1330                  0.560   MALE 2019         3        35       15          1
    ## 1331                  0.592    ALL 2019         3        35       15          1
    ## 1332                  0.601 FEMALE 2019         3        35       15          1
    ## 1333                  0.585   MALE 2019         3        35       15          1
    ## 1334                  0.547    ALL 2019         3        35       15          1
    ## 1335                  0.581 FEMALE 2019         3        35       15          1
    ## 1336                  0.514   MALE 2019         3        35       15          1
    ## 1337                  0.589    ALL 2019        18        63       72          1
    ## 1338                  0.595 FEMALE 2019        18        63       72          1
    ## 1339                  0.585   MALE 2019        18        63       72          1
    ## 1340                  0.622    ALL 2019        18        63       72          1
    ## 1341                  0.616 FEMALE 2019        18        63       72          1
    ## 1342                  0.627   MALE 2019        18        63       72          1
    ## 1343                  0.558    ALL 2019        18        63       72          1
    ## 1344                  0.571 FEMALE 2019        18        63       72          1
    ## 1345                  0.549   MALE 2019        18        63       72          1
    ## 1346                  0.695    ALL 2019         6         2       20          2
    ## 1347                  0.727 FEMALE 2019         6         2       20          2
    ## 1348                  0.662   MALE 2019         6         2       20          2
    ## 1349                  0.733    ALL 2019         6         2       20          2
    ## 1350                  0.643   MALE 2019         6         2       20          2
    ## 1351                  0.750    ALL 2019         6         2       20          2
    ## 1352                  0.554    ALL 2019         2         7        4          1
    ## 1353                  0.479 FEMALE 2019         2         7        4          1
    ## 1354                  0.632   MALE 2019         2         7        4          1
    ## 1355                  0.644    ALL 2019         2         7        4          1
    ## 1356                  0.556 FEMALE 2019         2         7        4          1
    ## 1357                  0.415    ALL 2019         2         7        4          1
    ## 1358                  0.522   MALE 2019         2         7        4          1
    ## 1359                  0.631    ALL 2019         1         1        3          1
    ## 1360                  0.686 FEMALE 2019         1         1        3          1
    ## 1361                  0.583   MALE 2019         1         1        3          1
    ## 1362                  0.674    ALL 2019         1         1        3          1
    ## 1363                  0.548   MALE 2019         1         1        3          1
    ## 1364                  0.683    ALL 2019         1         1        3          1
    ## 1365                  0.657 FEMALE 2019         1         1        3          1
    ## 1366                  0.687    ALL 2019        53        49      236          1
    ## 1367                  0.681 FEMALE 2019        53        49      236          1
    ## 1368                  0.692   MALE 2019        53        49      236          1
    ## 1369                  0.757    ALL 2019        53        49      236          1
    ## 1370                  0.763 FEMALE 2019        53        49      236          1
    ## 1371                  0.753   MALE 2019        53        49      236          1
    ## 1372                  0.607    ALL 2019        53        49      236          1
    ## 1373                  0.592 FEMALE 2019        53        49      236          1
    ## 1374                  0.623   MALE 2019        53        49      236          1
    ## 1375                  0.580    ALL 2019       271      1085     1402         18
    ## 1376                  0.566 FEMALE 2019       271      1085     1402         18
    ## 1377                  0.593   MALE 2019       271      1085     1402         18
    ## 1378                  0.612    ALL 2019       271      1085     1402         18
    ## 1379                  0.605 FEMALE 2019       271      1085     1402         18
    ## 1380                  0.618   MALE 2019       271      1085     1402         18
    ## 1381                  0.529    ALL 2019       271      1085     1402         18
    ## 1382                  0.505 FEMALE 2019       271      1085     1402         18
    ## 1383                  0.553   MALE 2019       271      1085     1402         18
    ## 1384                  0.551    ALL 2019        40      1097     1703         22
    ## 1385                  0.530 FEMALE 2019        40      1097     1703         22
    ## 1386                  0.569   MALE 2019        40      1097     1703         22
    ## 1387                  0.559    ALL 2019        40      1097     1703         22
    ## 1388                  0.542 FEMALE 2019        40      1097     1703         22
    ## 1389                  0.574   MALE 2019        40      1097     1703         22
    ## 1390                  0.533    ALL 2019        40      1097     1703         22
    ## 1391                  0.505 FEMALE 2019        40      1097     1703         22
    ## 1392                  0.560   MALE 2019        40      1097     1703         22
    ## 1393                  0.610    ALL 2019       550       795     1086          4
    ## 1394                  0.606 FEMALE 2019       550       795     1086          4
    ## 1395                  0.613   MALE 2019       550       795     1086          4
    ## 1396                  0.622    ALL 2019       550       795     1086          4
    ## 1397                  0.615 FEMALE 2019       550       795     1086          4
    ## 1398                  0.629   MALE 2019       550       795     1086          4
    ## 1399                  0.587    ALL 2019       550       795     1086          4
    ## 1400                  0.589 FEMALE 2019       550       795     1086          4
    ## 1401                  0.585   MALE 2019       550       795     1086          4
    ## 1402                  0.604    ALL 2019       105       125     1177          9
    ## 1403                  0.612 FEMALE 2019       105       125     1177          9
    ## 1404                  0.594   MALE 2019       105       125     1177          9
    ## 1405                  0.643    ALL 2019       105       125     1177          9
    ## 1406                  0.621 FEMALE 2019       105       125     1177          9
    ## 1407                  0.668   MALE 2019       105       125     1177          9
    ## 1408                  0.549    ALL 2019       105       125     1177          9
    ## 1409                  0.602 FEMALE 2019       105       125     1177          9
    ## 1410                  0.495   MALE 2019       105       125     1177          9
    ## 1411                  0.607    ALL 2019       771       424     2221         28
    ## 1412                  0.633 FEMALE 2019       771       424     2221         28
    ## 1413                  0.583   MALE 2019       771       424     2221         28
    ## 1414                  0.627    ALL 2019       771       424     2221         28
    ## 1415                  0.639 FEMALE 2019       771       424     2221         28
    ## 1416                  0.616   MALE 2019       771       424     2221         28
    ## 1417                  0.577    ALL 2019       771       424     2221         28
    ## 1418                  0.625 FEMALE 2019       771       424     2221         28
    ## 1419                  0.530   MALE 2019       771       424     2221         28
    ## 1420                  0.722    ALL 2019        91         6      117          2
    ## 1421                  0.749 FEMALE 2019        91         6      117          2
    ## 1422                  0.697   MALE 2019        91         6      117          2
    ## 1423                  0.794    ALL 2019        91         6      117          2
    ## 1424                  0.815 FEMALE 2019        91         6      117          2
    ## 1425                  0.771   MALE 2019        91         6      117          2
    ## 1426                  0.678    ALL 2019        91         6      117          2
    ## 1427                  0.697 FEMALE 2019        91         6      117          2
    ## 1428                  0.663   MALE 2019        91         6      117          2
    ## 1429                  0.695    ALL 2019       131        29      180          1
    ## 1430                  0.704 FEMALE 2019       131        29      180          1
    ## 1431                  0.687   MALE 2019       131        29      180          1
    ## 1432                  0.705    ALL 2019       131        29      180          1
    ## 1433                  0.715 FEMALE 2019       131        29      180          1
    ## 1434                  0.697   MALE 2019       131        29      180          1
    ## 1435                  0.683    ALL 2019       131        29      180          1
    ## 1436                  0.692 FEMALE 2019       131        29      180          1
    ## 1437                  0.674   MALE 2019       131        29      180          1
    ## 1438                  0.603    ALL 2019       767       464     2391          3
    ## 1439                  0.637 FEMALE 2019       767       464     2391          3
    ## 1440                  0.572   MALE 2019       767       464     2391          3
    ## 1441                  0.647    ALL 2019       767       464     2391          3
    ## 1442                  0.662 FEMALE 2019       767       464     2391          3
    ## 1443                  0.632   MALE 2019       767       464     2391          3
    ## 1444                  0.552    ALL 2019       767       464     2391          3
    ## 1445                  0.605 FEMALE 2019       767       464     2391          3
    ## 1446                  0.508   MALE 2019       767       464     2391          3
    ## 1447                  0.571    ALL 2019       174       381     3393         12
    ## 1448                  0.589 FEMALE 2019       174       381     3393         12
    ## 1449                  0.553   MALE 2019       174       381     3393         12
    ## 1450                  0.614    ALL 2019       174       381     3393         12
    ## 1451                  0.634 FEMALE 2019       174       381     3393         12
    ## 1452                  0.595   MALE 2019       174       381     3393         12
    ## 1453                  0.500    ALL 2019       174       381     3393         12
    ## 1454                  0.520 FEMALE 2019       174       381     3393         12
    ## 1455                  0.479   MALE 2019       174       381     3393         12
    ## 1456                  0.537    ALL 2019       240      1336     3424         40
    ## 1457                  0.549 FEMALE 2019       240      1336     3424         40
    ## 1458                  0.526   MALE 2019       240      1336     3424         40
    ## 1459                  0.543    ALL 2019       240      1336     3424         40
    ## 1460                  0.572 FEMALE 2019       240      1336     3424         40
    ## 1461                  0.518   MALE 2019       240      1336     3424         40
    ## 1462                  0.528    ALL 2019       240      1336     3424         40
    ## 1463                  0.517 FEMALE 2019       240      1336     3424         40
    ## 1464                  0.539   MALE 2019       240      1336     3424         40
    ## 1465                  0.609    ALL 2019        21        75      259         59
    ## 1466                  0.649 FEMALE 2019        21        75      259         59
    ## 1467                  0.567   MALE 2019        21        75      259         59
    ## 1468                  0.656    ALL 2019        21        75      259         59
    ## 1469                  0.700 FEMALE 2019        21        75      259         59
    ## 1470                  0.617   MALE 2019        21        75      259         59
    ## 1471                  0.561    ALL 2019        21        75      259         59
    ## 1472                  0.601 FEMALE 2019        21        75      259         59
    ## 1473                  0.509   MALE 2019        21        75      259         59
    ## 1474                  0.612    ALL 2019        75       699     1689         16
    ## 1475                  0.624 FEMALE 2019        75       699     1689         16
    ## 1476                  0.601   MALE 2019        75       699     1689         16
    ## 1477                  0.634    ALL 2019        75       699     1689         16
    ## 1478                  0.660 FEMALE 2019        75       699     1689         16
    ## 1479                  0.607   MALE 2019        75       699     1689         16
    ## 1480                  0.577    ALL 2019        75       699     1689         16
    ## 1481                  0.559 FEMALE 2019        75       699     1689         16
    ## 1482                  0.592   MALE 2019        75       699     1689         16
    ## 1483                  0.682    ALL 2019       150        39      641          3
    ## 1484                  0.718 FEMALE 2019       150        39      641          3
    ## 1485                  0.647   MALE 2019       150        39      641          3
    ## 1486                  0.692    ALL 2019       150        39      641          3
    ## 1487                  0.739 FEMALE 2019       150        39      641          3
    ## 1488                  0.647   MALE 2019       150        39      641          3
    ## 1489                  0.671    ALL 2019       150        39      641          3
    ## 1490                  0.694 FEMALE 2019       150        39      641          3
    ## 1491                  0.647   MALE 2019       150        39      641          3
    ## 1492                  0.683    ALL 2019      1365       953      998         20
    ## 1493                  0.701 FEMALE 2019      1365       953      998         20
    ## 1494                  0.667   MALE 2019      1365       953      998         20
    ## 1495                  0.696    ALL 2019      1365       953      998         20
    ## 1496                  0.708 FEMALE 2019      1365       953      998         20
    ## 1497                  0.684   MALE 2019      1365       953      998         20
    ## 1498                  0.668    ALL 2019      1365       953      998         20
    ## 1499                  0.692 FEMALE 2019      1365       953      998         20
    ## 1500                  0.645   MALE 2019      1365       953      998         20
    ## 1501                  0.729    ALL 2019       168       130      435          3
    ## 1502                  0.750 FEMALE 2019       168       130      435          3
    ## 1503                  0.710   MALE 2019       168       130      435          3
    ## 1504                  0.738    ALL 2019       168       130      435          3
    ## 1505                  0.745 FEMALE 2019       168       130      435          3
    ## 1506                  0.732   MALE 2019       168       130      435          3
    ## 1507                  0.721    ALL 2019       168       130      435          3
    ## 1508                  0.754 FEMALE 2019       168       130      435          3
    ## 1509                  0.686   MALE 2019       168       130      435          3
    ## 1510                  0.687    ALL 2019       129       145      804          3
    ## 1511                  0.682 FEMALE 2019       129       145      804          3
    ## 1512                  0.691   MALE 2019       129       145      804          3
    ## 1513                  0.647    ALL 2019       129       145      804          3
    ## 1514                  0.610 FEMALE 2019       129       145      804          3
    ## 1515                  0.678   MALE 2019       129       145      804          3
    ## 1516                  0.723    ALL 2019       129       145      804          3
    ## 1517                  0.747 FEMALE 2019       129       145      804          3
    ## 1518                  0.702   MALE 2019       129       145      804          3
    ## 1519                  0.626    ALL 2019       102        81      687          5
    ## 1520                  0.684 FEMALE 2019       102        81      687          5
    ## 1521                  0.572   MALE 2019       102        81      687          5
    ## 1522                  0.630    ALL 2019       102        81      687          5
    ## 1523                  0.682 FEMALE 2019       102        81      687          5
    ## 1524                  0.577   MALE 2019       102        81      687          5
    ## 1525                  0.621    ALL 2019       102        81      687          5
    ## 1526                  0.686 FEMALE 2019       102        81      687          5
    ## 1527                  0.565   MALE 2019       102        81      687          5
    ## 1528                  0.607    ALL 2019       295       115      424          4
    ## 1529                  0.597 FEMALE 2019       295       115      424          4
    ## 1530                  0.617   MALE 2019       295       115      424          4
    ## 1531                  0.481    ALL 2019       295       115      424          4
    ## 1532                  0.500 FEMALE 2019       295       115      424          4
    ## 1533                  0.462   MALE 2019       295       115      424          4
    ## 1534                  0.676    ALL 2019       295       115      424          4
    ## 1535                  0.652 FEMALE 2019       295       115      424          4
    ## 1536                  0.699   MALE 2019       295       115      424          4
    ## 1537                  0.652    ALL 2019        77        15      506          2
    ## 1538                  0.660 FEMALE 2019        77        15      506          2
    ## 1539                  0.645   MALE 2019        77        15      506          2
    ## 1540                  0.661    ALL 2019        77        15      506          2
    ## 1541                  0.683 FEMALE 2019        77        15      506          2
    ## 1542                  0.643   MALE 2019        77        15      506          2
    ## 1543                  0.641    ALL 2019        77        15      506          2
    ## 1544                  0.635 FEMALE 2019        77        15      506          2
    ## 1545                  0.647   MALE 2019        77        15      506          2
    ## 1546                  0.559    ALL 2019        69       565     3341          8
    ## 1547                  0.597 FEMALE 2019        69       565     3341          8
    ## 1548                  0.520   MALE 2019        69       565     3341          8
    ## 1549                  0.543    ALL 2019        69       565     3341          8
    ## 1550                  0.582 FEMALE 2019        69       565     3341          8
    ## 1551                  0.502   MALE 2019        69       565     3341          8
    ## 1552                  0.583    ALL 2019        69       565     3341          8
    ## 1553                  0.623 FEMALE 2019        69       565     3341          8
    ## 1554                  0.544   MALE 2019        69       565     3341          8
    ## 1555                  0.679    ALL 2019       515       118      832         14
    ## 1556                  0.687 FEMALE 2019       515       118      832         14
    ## 1557                  0.671   MALE 2019       515       118      832         14
    ## 1558                  0.693    ALL 2019       515       118      832         14
    ## 1559                  0.701 FEMALE 2019       515       118      832         14
    ## 1560                  0.686   MALE 2019       515       118      832         14
    ## 1561                  0.664    ALL 2019       515       118      832         14
    ## 1562                  0.672 FEMALE 2019       515       118      832         14
    ## 1563                  0.656   MALE 2019       515       118      832         14
    ## 1564                  0.528    ALL 2019         8         9      165          7
    ## 1565                  0.635 FEMALE 2019         8         9      165          7
    ## 1566                  0.419   MALE 2019         8         9      165          7
    ## 1567                  0.457    ALL 2019         8         9      165          7
    ## 1568                  0.615 FEMALE 2019         8         9      165          7
    ## 1569                  0.250   MALE 2019         8         9      165          7
    ## 1570                  0.601    ALL 2019        22       128      789          7
    ## 1571                  0.632 FEMALE 2019        22       128      789          7
    ## 1572                  0.571   MALE 2019        22       128      789          7
    ## 1573                  0.609    ALL 2019        22       128      789          7
    ## 1574                  0.642 FEMALE 2019        22       128      789          7
    ## 1575                  0.577   MALE 2019        22       128      789          7
    ## 1576                  0.583    ALL 2019        22       128      789          7
    ## 1577                  0.607 FEMALE 2019        22       128      789          7
    ## 1578                  0.559   MALE 2019        22       128      789          7
    ## 1579                  0.652    ALL 2019         2         3       50          2
    ## 1580                  0.638 FEMALE 2019         2         3       50          2
    ## 1581                  0.667   MALE 2019         2         3       50          2
    ## 1582                  0.746    ALL 2019         2         3       50          2
    ## 1583                  0.742 FEMALE 2019         2         3       50          2
    ## 1584                  0.750   MALE 2019         2         3       50          2
    ## 1585                  0.553    ALL 2019         2         3       50          2
    ## 1586                  0.510 FEMALE 2019         2         3       50          2
    ## 1587                  0.611   MALE 2019         2         3       50          2
    ## 1588                  0.591    ALL 2019        10         8      103          3
    ## 1589                  0.598 FEMALE 2019        10         8      103          3
    ## 1590                  0.585   MALE 2019        10         8      103          3
    ## 1591                  0.689    ALL 2019        10         8      103          3
    ## 1592                  0.644 FEMALE 2019        10         8      103          3
    ## 1593                  0.733   MALE 2019        10         8      103          3
    ## 1594                  0.455    ALL 2019        10         8      103          3
    ## 1595                  0.514 FEMALE 2019        10         8      103          3
    ## 1596                  0.409   MALE 2019        10         8      103          3
    ## 1597                  0.574    ALL 2019        14        12       19          1
    ## 1598                  0.538 FEMALE 2019        14        12       19          1
    ## 1599                  0.606   MALE 2019        14        12       19          1
    ## 1600                  0.634    ALL 2019        14        12       19          1
    ## 1601                  0.577 FEMALE 2019        14        12       19          1
    ## 1602                  0.677   MALE 2019        14        12       19          1
    ## 1603                  0.525    ALL 2019        14        12       19          1
    ## 1604                  0.510 FEMALE 2019        14        12       19          1
    ## 1605                  0.540   MALE 2019        14        12       19          1
    ## 1606                  0.645    ALL 2019         3         6       20          1
    ## 1607                  0.634 FEMALE 2019         3         6       20          1
    ## 1608                  0.656   MALE 2019         3         6       20          1
    ## 1609                  0.667    ALL 2019         3         6       20          1
    ## 1610                  0.638 FEMALE 2019         3         6       20          1
    ## 1611                  0.693   MALE 2019         3         6       20          1
    ## 1612                  0.635    ALL 2019         7         8       14          4
    ## 1613                  0.681 FEMALE 2019         7         8       14          4
    ## 1614                  0.584   MALE 2019         7         8       14          4
    ## 1615                  0.667    ALL 2019         7         8       14          4
    ## 1616                  0.687 FEMALE 2019         7         8       14          4
    ## 1617                  0.648   MALE 2019         7         8       14          4
    ## 1618                  0.570    ALL 2019         7         8       14          4
    ## 1619                  0.673 FEMALE 2019         7         8       14          4
    ## 1620                  0.412   MALE 2019         7         8       14          4
    ## 1621                  0.639    ALL 2019         5        17       25          1
    ## 1622                  0.662 FEMALE 2019         5        17       25          1
    ## 1623                  0.616   MALE 2019         5        17       25          1
    ## 1624                  0.632    ALL 2019         5        17       25          1
    ## 1625                  0.679 FEMALE 2019         5        17       25          1
    ## 1626                  0.577   MALE 2019         5        17       25          1
    ## 1627                  0.650    ALL 2019         5        17       25          1
    ## 1628                  0.633 FEMALE 2019         5        17       25          1
    ## 1629                  0.667   MALE 2019         5        17       25          1
    ## 1630                  0.728    ALL 2019       547       402      406         17
    ## 1631                  0.728 FEMALE 2019       547       402      406         17
    ## 1632                  0.728   MALE 2019       547       402      406         17
    ## 1633                  0.732    ALL 2019       547       402      406         17
    ## 1634                  0.728 FEMALE 2019       547       402      406         17
    ## 1635                  0.735   MALE 2019       547       402      406         17
    ## 1636                  0.722    ALL 2019       547       402      406         17
    ## 1637                  0.728 FEMALE 2019       547       402      406         17
    ## 1638                  0.716   MALE 2019       547       402      406         17
    ## 1639                  0.703    ALL 2019        46        11       45          4
    ## 1640                  0.763 FEMALE 2019        46        11       45          4
    ## 1641                  0.651   MALE 2019        46        11       45          4
    ## 1642                  0.750    ALL 2019        46        11       45          4
    ## 1643                  0.819 FEMALE 2019        46        11       45          4
    ## 1644                  0.680   MALE 2019        46        11       45          4
    ## 1645                  0.624    ALL 2019        46        11       45          4
    ## 1646                  0.644 FEMALE 2019        46        11       45          4
    ## 1647                  0.611   MALE 2019        46        11       45          4
    ## 1648                  0.573    ALL 2019       169       795     1611          6
    ## 1649                  0.578 FEMALE 2019       169       795     1611          6
    ## 1650                  0.569   MALE 2019       169       795     1611          6
    ## 1651                  0.600    ALL 2019       169       795     1611          6
    ## 1652                  0.622 FEMALE 2019       169       795     1611          6
    ## 1653                  0.577   MALE 2019       169       795     1611          6
    ## 1654                  0.526    ALL 2019       169       795     1611          6
    ## 1655                  0.493 FEMALE 2019       169       795     1611          6
    ## 1656                  0.559   MALE 2019       169       795     1611          6
    ## 1657                  0.635    ALL 2019        69       107      249          3
    ## 1658                  0.662 FEMALE 2019        69       107      249          3
    ## 1659                  0.612   MALE 2019        69       107      249          3
    ## 1660                  0.662    ALL 2019        69       107      249          3
    ## 1661                  0.673 FEMALE 2019        69       107      249          3
    ## 1662                  0.652   MALE 2019        69       107      249          3
    ## 1663                  0.594    ALL 2019        69       107      249          3
    ## 1664                  0.644 FEMALE 2019        69       107      249          3
    ## 1665                  0.553   MALE 2019        69       107      249          3
    ## 1666                  0.666    ALL 2019        61        90      264          2
    ## 1667                  0.714 FEMALE 2019        61        90      264          2
    ## 1668                  0.621   MALE 2019        61        90      264          2
    ## 1669                  0.659    ALL 2019        61        90      264          2
    ## 1670                  0.691 FEMALE 2019        61        90      264          2
    ## 1671                  0.628   MALE 2019        61        90      264          2
    ## 1672                  0.683    ALL 2019        61        90      264          2
    ## 1673                  0.743 FEMALE 2019        61        90      264          2
    ## 1674                  0.627   MALE 2019        61        90      264          2
    ## 1675                  0.588    ALL 2019        32       179      698          2
    ## 1676                  0.598 FEMALE 2019        32       179      698          2
    ## 1677                  0.579   MALE 2019        32       179      698          2
    ## 1678                  0.632    ALL 2019        32       179      698          2
    ## 1679                  0.633 FEMALE 2019        32       179      698          2
    ## 1680                  0.631   MALE 2019        32       179      698          2
    ## 1681                  0.541    ALL 2019        32       179      698          2
    ## 1682                  0.556 FEMALE 2019        32       179      698          2
    ## 1683                  0.527   MALE 2019        32       179      698          2
    ## 1684                  0.629    ALL 2019        25        37      107          2
    ## 1685                  0.642 FEMALE 2019        25        37      107          2
    ## 1686                  0.613   MALE 2019        25        37      107          2
    ## 1687                  0.635    ALL 2019        25        37      107          2
    ## 1688                  0.671 FEMALE 2019        25        37      107          2
    ## 1689                  0.591   MALE 2019        25        37      107          2
    ## 1690                  0.637    ALL 2019        25        37      107          2
    ## 1691                  0.614 FEMALE 2019        25        37      107          2
    ## 1692                  0.664   MALE 2019        25        37      107          2
    ## 1693                  0.644    ALL 2019        44        33       82          1
    ## 1694                  0.643 FEMALE 2019        44        33       82          1
    ## 1695                  0.645   MALE 2019        44        33       82          1
    ## 1696                  0.642    ALL 2019        44        33       82          1
    ## 1697                  0.630 FEMALE 2019        44        33       82          1
    ## 1698                  0.654   MALE 2019        44        33       82          1
    ## 1699                  0.646    ALL 2019        44        33       82          1
    ## 1700                  0.661 FEMALE 2019        44        33       82          1
    ## 1701                  0.631   MALE 2019        44        33       82          1
    ## 1702                  0.563    ALL 2019         4        14       10          2
    ## 1703                  0.640 FEMALE 2019         4        14       10          2
    ## 1704                  0.493   MALE 2019         4        14       10          2
    ## 1705                  0.521    ALL 2019         4        14       10          2
    ## 1706                  0.580 FEMALE 2019         4        14       10          2
    ## 1707                  0.464   MALE 2019         4        14       10          2
    ## 1708                  0.627    ALL 2019         4        14       10          2
    ## 1709                  0.741 FEMALE 2019         4        14       10          2
    ## 1710                  0.531   MALE 2019         4        14       10          2
    ## 1711                  0.547    ALL 2019        14        24       76          4
    ## 1712                  0.554 FEMALE 2019        14        24       76          4
    ## 1713                  0.540   MALE 2019        14        24       76          4
    ## 1714                  0.586    ALL 2019        14        24       76          4
    ## 1715                  0.580 FEMALE 2019        14        24       76          4
    ## 1716                  0.593   MALE 2019        14        24       76          4
    ## 1717                  0.474    ALL 2019        14        24       76          4
    ## 1718                  0.517 FEMALE 2019        14        24       76          4
    ## 1719                  0.432   MALE 2019        14        24       76          4
    ## 1720                  0.662    ALL 2019         6        16       65          4
    ## 1721                  0.656 FEMALE 2019         6        16       65          4
    ## 1722                  0.668   MALE 2019         6        16       65          4
    ## 1723                  0.674    ALL 2019         6        16       65          4
    ## 1724                  0.686 FEMALE 2019         6        16       65          4
    ## 1725                  0.662   MALE 2019         6        16       65          4
    ## 1726                  0.678    ALL 2019         6        16       65          4
    ## 1727                  0.714   MALE 2019         6        16       65          4
    ## 1728                  0.616    ALL 2019         6         8       17          2
    ## 1729                  0.608 FEMALE 2019         6         8       17          2
    ## 1730                  0.623   MALE 2019         6         8       17          2
    ## 1731                  0.715    ALL 2019         6         8       17          2
    ## 1732                  0.711 FEMALE 2019         6         8       17          2
    ## 1733                  0.720   MALE 2019         6         8       17          2
    ## 1734                  0.467    ALL 2019         6         8       17          2
    ## 1735                  0.449 FEMALE 2019         6         8       17          2
    ## 1736                  0.482   MALE 2019         6         8       17          2
    ## 1737                  0.725    ALL 2019       107        30      268          1
    ## 1738                  0.730 FEMALE 2019       107        30      268          1
    ## 1739                  0.720   MALE 2019       107        30      268          1
    ## 1740                  0.746    ALL 2019       107        30      268          1
    ## 1741                  0.738 FEMALE 2019       107        30      268          1
    ## 1742                  0.753   MALE 2019       107        30      268          1
    ## 1743                  0.724    ALL 2019       107        30      268          1
    ## 1744                  0.750 FEMALE 2019       107        30      268          1
    ## 1745                  0.696   MALE 2019       107        30      268          1
    ## 1746                  0.642    ALL 2019        73       131      687          2
    ## 1747                  0.656 FEMALE 2019        73       131      687          2
    ## 1748                  0.630   MALE 2019        73       131      687          2
    ## 1749                  0.643    ALL 2019        73       131      687          2
    ## 1750                  0.661 FEMALE 2019        73       131      687          2
    ## 1751                  0.628   MALE 2019        73       131      687          2
    ## 1752                  0.647    ALL 2019        73       131      687          2
    ## 1753                  0.663 FEMALE 2019        73       131      687          2
    ## 1754                  0.632   MALE 2019        73       131      687          2
    ## 1755                  0.752    ALL 2019       196        94      167          2
    ## 1756                  0.788 FEMALE 2019       196        94      167          2
    ## 1757                  0.720   MALE 2019       196        94      167          2
    ## 1758                  0.771    ALL 2019       196        94      167          2
    ## 1759                  0.789 FEMALE 2019       196        94      167          2
    ## 1760                  0.754   MALE 2019       196        94      167          2
    ## 1761                  0.752    ALL 2019       196        94      167          2
    ## 1762                  0.786 FEMALE 2019       196        94      167          2
    ## 1763                  0.718   MALE 2019       196        94      167          2
    ## 1764                  0.716    ALL 2019       484        90      299          1
    ## 1765                  0.712 FEMALE 2019       484        90      299          1
    ## 1766                  0.719   MALE 2019       484        90      299          1
    ## 1767                  0.732    ALL 2019       484        90      299          1
    ## 1768                  0.704 FEMALE 2019       484        90      299          1
    ## 1769                  0.756   MALE 2019       484        90      299          1
    ## 1770                  0.689    ALL 2019       484        90      299          1
    ## 1771                  0.726 FEMALE 2019       484        90      299          1
    ## 1772                  0.661   MALE 2019       484        90      299          1
    ## 1773                  0.600    ALL 2019       109       622      745          2
    ## 1774                  0.627 FEMALE 2019       109       622      745          2
    ## 1775                  0.576   MALE 2019       109       622      745          2
    ## 1776                  0.635    ALL 2019       109       622      745          2
    ## 1777                  0.662 FEMALE 2019       109       622      745          2
    ## 1778                  0.611   MALE 2019       109       622      745          2
    ## 1779                  0.488    ALL 2019       109       622      745          2
    ## 1780                  0.509 FEMALE 2019       109       622      745          2
    ## 1781                  0.471   MALE 2019       109       622      745          2
    ## 1782                  0.696    ALL 2019       445        78      661          2
    ## 1783                  0.687 FEMALE 2019       445        78      661          2
    ## 1784                  0.704   MALE 2019       445        78      661          2
    ## 1785                  0.702    ALL 2019       445        78      661          2
    ## 1786                  0.688 FEMALE 2019       445        78      661          2
    ## 1787                  0.716   MALE 2019       445        78      661          2
    ## 1788                  0.684    ALL 2019       445        78      661          2
    ## 1789                  0.685 FEMALE 2019       445        78      661          2
    ## 1790                  0.683   MALE 2019       445        78      661          2
    ## 1791                  0.704    ALL 2019        86         8      271          1
    ## 1792                  0.708 FEMALE 2019        86         8      271          1
    ## 1793                  0.701   MALE 2019        86         8      271          1
    ## 1794                  0.723    ALL 2019        86         8      271          1
    ## 1795                  0.717 FEMALE 2019        86         8      271          1
    ## 1796                  0.727   MALE 2019        86         8      271          1
    ## 1797                  0.690    ALL 2019        86         8      271          1
    ## 1798                  0.715 FEMALE 2019        86         8      271          1
    ## 1799                  0.667   MALE 2019        86         8      271          1
    ## 1800                  0.669    ALL 2019        91       103      379          3
    ## 1801                  0.648 FEMALE 2019        91       103      379          3
    ## 1802                  0.689   MALE 2019        91       103      379          3
    ## 1803                  0.708    ALL 2019        91       103      379          3
    ## 1804                  0.664 FEMALE 2019        91       103      379          3
    ## 1805                  0.750   MALE 2019        91       103      379          3
    ## 1806                  0.612    ALL 2019        91       103      379          3
    ## 1807                  0.626 FEMALE 2019        91       103      379          3
    ## 1808                  0.596   MALE 2019        91       103      379          3
    ## 1809                  0.736    ALL 2019        89        38      219          1
    ## 1810                  0.768 FEMALE 2019        89        38      219          1
    ## 1811                  0.708   MALE 2019        89        38      219          1
    ## 1812                  0.735    ALL 2019        89        38      219          1
    ## 1813                  0.760 FEMALE 2019        89        38      219          1
    ## 1814                  0.713   MALE 2019        89        38      219          1
    ## 1815                  0.737    ALL 2019        89        38      219          1
    ## 1816                  0.780 FEMALE 2019        89        38      219          1
    ## 1817                  0.703   MALE 2019        89        38      219          1
    ## 1818                  0.647    ALL 2019       138      5307     1964         14
    ## 1819                  0.647 FEMALE 2019       138      5307     1964         14
    ## 1820                  0.647   MALE 2019       138      5307     1964         14
    ## 1821                  0.600    ALL 2019       138      5307     1964         14
    ## 1822                  0.603 FEMALE 2019       138      5307     1964         14
    ## 1823                  0.597   MALE 2019       138      5307     1964         14
    ## 1824                  0.726    ALL 2019       138      5307     1964         14
    ## 1825                  0.728 FEMALE 2019       138      5307     1964         14
    ## 1826                  0.725   MALE 2019       138      5307     1964         14
    ## 1827                  0.615    ALL 2019       506      2144     5266         13
    ## 1828                  0.621 FEMALE 2019       506      2144     5266         13
    ## 1829                  0.610   MALE 2019       506      2144     5266         13
    ## 1830                  0.631    ALL 2019       506      2144     5266         13
    ## 1831                  0.626 FEMALE 2019       506      2144     5266         13
    ## 1832                  0.636   MALE 2019       506      2144     5266         13
    ## 1833                  0.594    ALL 2019       506      2144     5266         13
    ## 1834                  0.614 FEMALE 2019       506      2144     5266         13
    ## 1835                  0.576   MALE 2019       506      2144     5266         13
    ## 1836                  0.698    ALL 2019        31        16      186          1
    ## 1837                  0.689 FEMALE 2019        31        16      186          1
    ## 1838                  0.707   MALE 2019        31        16      186          1
    ## 1839                  0.714    ALL 2019        31        16      186          1
    ## 1840                  0.670 FEMALE 2019        31        16      186          1
    ## 1841                  0.757   MALE 2019        31        16      186          1
    ## 1842                  0.669    ALL 2019        31        16      186          1
    ## 1843                  0.719 FEMALE 2019        31        16      186          1
    ## 1844                  0.611   MALE 2019        31        16      186          1
    ## 1845                  0.787    ALL 2019       167       150      478          1
    ## 1846                  0.803 FEMALE 2019       167       150      478          1
    ## 1847                  0.773   MALE 2019       167       150      478          1
    ## 1848                  0.813    ALL 2019       167       150      478          1
    ## 1849                  0.809 FEMALE 2019       167       150      478          1
    ## 1850                  0.816   MALE 2019       167       150      478          1
    ## 1851                  0.758    ALL 2019       167       150      478          1
    ## 1852                  0.821 FEMALE 2019       167       150      478          1
    ## 1853                  0.696   MALE 2019       167       150      478          1
    ## 1854                  0.783    ALL 2019       130        44      323          1
    ## 1855                  0.772 FEMALE 2019       130        44      323          1
    ## 1856                  0.791   MALE 2019       130        44      323          1
    ## 1857                  0.798    ALL 2019       130        44      323          1
    ## 1858                  0.750 FEMALE 2019       130        44      323          1
    ## 1859                  0.840   MALE 2019       130        44      323          1
    ## 1860                  0.793    ALL 2019       130        44      323          1
    ## 1861                  0.840 FEMALE 2019       130        44      323          1
    ## 1862                  0.756   MALE 2019       130        44      323          1
    ## 1863                  0.788    ALL 2019      1024        52      334          3
    ## 1864                  0.771 FEMALE 2019      1024        52      334          3
    ## 1865                  0.806   MALE 2019      1024        52      334          3
    ## 1866                  0.788    ALL 2019      1024        52      334          3
    ## 1867                  0.760 FEMALE 2019      1024        52      334          3
    ## 1868                  0.817   MALE 2019      1024        52      334          3
    ## 1869                  0.789    ALL 2019      1024        52      334          3
    ## 1870                  0.787 FEMALE 2019      1024        52      334          3
    ## 1871                  0.791   MALE 2019      1024        52      334          3
    ## 1872                  0.714    ALL 2019       101        32      293          1
    ## 1873                  0.734 FEMALE 2019       101        32      293          1
    ## 1874                  0.695   MALE 2019       101        32      293          1
    ## 1875                  0.698    ALL 2019       101        32      293          1
    ## 1876                  0.712 FEMALE 2019       101        32      293          1
    ## 1877                  0.686   MALE 2019       101        32      293          1
    ## 1878                  0.738    ALL 2019       101        32      293          1
    ## 1879                  0.776 FEMALE 2019       101        32      293          1
    ## 1880                  0.704   MALE 2019       101        32      293          1
    ## 1881                  0.655    ALL 2019         5         6       20          3
    ## 1882                  0.633 FEMALE 2019         5         6       20          3
    ## 1883                  0.673   MALE 2019         5         6       20          3
    ## 1884                  0.699    ALL 2019         5         6       20          3
    ## 1885                  0.662 FEMALE 2019         5         6       20          3
    ## 1886                  0.725   MALE 2019         5         6       20          3
    ## 1887                  0.581    ALL 2019         5         6       20          3
    ## 1888                  0.593 FEMALE 2019         5         6       20          3
    ## 1889                  0.569   MALE 2019         5         6       20          3
    ## 1890                  0.627    ALL 2019         9         7       30          1
    ## 1891                  0.644 FEMALE 2019         9         7       30          1
    ## 1892                  0.609   MALE 2019         9         7       30          1
    ## 1893                  0.659    ALL 2019         9         7       30          1
    ## 1894                  0.639 FEMALE 2019         9         7       30          1
    ## 1895                  0.678   MALE 2019         9         7       30          1
    ## 1896                  0.579    ALL 2019         9         7       30          1
    ## 1897                  0.651 FEMALE 2019         9         7       30          1
    ## 1898                  0.490   MALE 2019         9         7       30          1
    ## 1899                  0.583    ALL 2019         1         3       27          1
    ## 1900                  0.609 FEMALE 2019         1         3       27          1
    ## 1901                  0.558   MALE 2019         1         3       27          1
    ## 1902                  0.571    ALL 2019         1         3       27          1
    ## 1903                  0.567 FEMALE 2019         1         3       27          1
    ## 1904                  0.575   MALE 2019         1         3       27          1
    ## 1905                  0.600    ALL 2019         1         3       27          1
    ## 1906                  0.674 FEMALE 2019         1         3       27          1
    ## 1907                  0.532   MALE 2019         1         3       27          1
    ##      num_white num_female num_male num_lep num_multi num_swd num_ecdis
    ## 1         1504        909      934      13        98     320       786
    ## 2         1504        909      934      13        98     320       786
    ## 3         1504        909      934      13        98     320       786
    ## 4         1504        909      934      13        98     320       786
    ## 5         1504        909      934      13        98     320       786
    ## 6         1504        909      934      13        98     320       786
    ## 7         1504        909      934      13        98     320       786
    ## 8         1504        909      934      13        98     320       786
    ## 9         1504        909      934      13        98     320       786
    ## 10        1429       1008     1007      25       164     338      1175
    ## 11        1429       1008     1007      25       164     338      1175
    ## 12        1429       1008     1007      25       164     338      1175
    ## 13        1429       1008     1007      25       164     338      1175
    ## 14        1429       1008     1007      25       164     338      1175
    ## 15        1429       1008     1007      25       164     338      1175
    ## 16        1429       1008     1007      25       164     338      1175
    ## 17        1429       1008     1007      25       164     338      1175
    ## 18        1429       1008     1007      25       164     338      1175
    ## 19        3105       2401     2418     182       390     717      1708
    ## 20        3105       2401     2418     182       390     717      1708
    ## 21        3105       2401     2418     182       390     717      1708
    ## 22        3105       2401     2418     182       390     717      1708
    ## 23        3105       2401     2418     182       390     717      1708
    ## 24        3105       2401     2418     182       390     717      1708
    ## 25        3105       2401     2418     182       390     717      1708
    ## 26        3105       2401     2418     182       390     717      1708
    ## 27        3105       2401     2418     182       390     717      1708
    ## 28        3703       2914     2973     313       331     792      1442
    ## 29        3703       2914     2973     313       331     792      1442
    ## 30        3703       2914     2973     313       331     792      1442
    ## 31        3703       2914     2973     313       331     792      1442
    ## 32        3703       2914     2973     313       331     792      1442
    ## 33        3703       2914     2973     313       331     792      1442
    ## 34        3703       2914     2973     313       331     792      1442
    ## 35        3703       2914     2973     313       331     792      1442
    ## 36        3703       2914     2973     313       331     792      1442
    ## 37        3568       2390     2451     227       177     587       999
    ## 38        3568       2390     2451     227       177     587       999
    ## 39        3568       2390     2451     227       177     587       999
    ## 40        3568       2390     2451     227       177     587       999
    ## 41        3568       2390     2451     227       177     587       999
    ## 42        3568       2390     2451     227       177     587       999
    ## 43        3568       2390     2451     227       177     587       999
    ## 44        3568       2390     2451     227       177     587       999
    ## 45        3568       2390     2451     227       177     587       999
    ## 46        1059        569      549       1        13     164       562
    ## 47        1059        569      549       1        13     164       562
    ## 48        1059        569      549       1        13     164       562
    ## 49        1059        569      549       1        13     164       562
    ## 50        1059        569      549       1        13     164       562
    ## 51        1059        569      549       1        13     164       562
    ## 52        1059        569      549       1        13     164       562
    ## 53        1059        569      549       1        13     164       562
    ## 54        1059        569      549       1        13     164       562
    ## 55        1301        648      756       2        49     158       564
    ## 56        1301        648      756       2        49     158       564
    ## 57        1301        648      756       2        49     158       564
    ## 58        1301        648      756       2        49     158       564
    ## 59        1301        648      756       2        49     158       564
    ## 60        1301        648      756       2        49     158       564
    ## 61        1301        648      756       2        49     158       564
    ## 62        1301        648      756       2        49     158       564
    ## 63        1301        648      756       2        49     158       564
    ## 64        2206       2562     2717     197       642     961      3843
    ## 65        2206       2562     2717     197       642     961      3843
    ## 66        2206       2562     2717     197       642     961      3843
    ## 67        2206       2562     2717     197       642     961      3843
    ## 68        2206       2562     2717     197       642     961      3843
    ## 69        2206       2562     2717     197       642     961      3843
    ## 70        2206       2562     2717     197       642     961      3843
    ## 71        2206       2562     2717     197       642     961      3843
    ## 72        2206       2562     2717     197       642     961      3843
    ## 73         645        293      384       0        19     102       458
    ## 74         645        293      384       0        19     102       458
    ## 75         645        293      384       0        19     102       458
    ## 76         645        293      384       0        19     102       458
    ## 77         645        293      384       0        19     102       458
    ## 78         645        293      384       0        19     102       458
    ## 79         645        293      384       0        19     102       458
    ## 80         645        293      384       0        19     102       458
    ## 81         645        293      384       0        19     102       458
    ## 82        1256        656      739       4        48     181       695
    ## 83        1256        656      739       4        48     181       695
    ## 84        1256        656      739       4        48     181       695
    ## 85        1256        656      739       4        48     181       695
    ## 86        1256        656      739       4        48     181       695
    ## 87        1256        656      739       4        48     181       695
    ## 88        1256        656      739       4        48     181       695
    ## 89        1256        656      739       4        48     181       695
    ## 90        1256        656      739       4        48     181       695
    ## 91        1536        850      872       8        91     244       728
    ## 92        1536        850      872       8        91     244       728
    ## 93        1536        850      872       8        91     244       728
    ## 94        1536        850      872       8        91     244       728
    ## 95        1536        850      872       8        91     244       728
    ## 96        1536        850      872       8        91     244       728
    ## 97        1536        850      872       8        91     244       728
    ## 98        1536        850      872       8        91     244       728
    ## 99        1536        850      872       8        91     244       728
    ## 100       1062        554      602       2        43     131       485
    ## 101       1062        554      602       2        43     131       485
    ## 102       1062        554      602       2        43     131       485
    ## 103       1062        554      602       2        43     131       485
    ## 104       1062        554      602       2        43     131       485
    ## 105       1062        554      602       2        43     131       485
    ## 106       1062        554      602       2        43     131       485
    ## 107       1062        554      602       2        43     131       485
    ## 108       1062        554      602       2        43     131       485
    ## 109        549        294      306       0        22      65       214
    ## 110        549        294      306       0        22      65       214
    ## 111        549        294      306       0        22      65       214
    ## 112        549        294      306       0        22      65       214
    ## 113        549        294      306       0        22      65       214
    ## 114        549        294      306       0        22      65       214
    ## 115        549        294      306       0        22      65       214
    ## 116        549        294      306       0        22      65       214
    ## 117        549        294      306       0        22      65       214
    ## 118        369        222      182       0        19      49       231
    ## 119        369        222      182       0        19      49       231
    ## 120        369        222      182       0        19      49       231
    ## 121        369        222      182       0        19      49       231
    ## 122        369        222      182       0        19      49       231
    ## 123        369        222      182       0        19      49       231
    ## 124        369        222      182       0        19      49       231
    ## 125       1684        976     1076       7       161     339      1128
    ## 126       1684        976     1076       7       161     339      1128
    ## 127       1684        976     1076       7       161     339      1128
    ## 128       1684        976     1076       7       161     339      1128
    ## 129       1684        976     1076       7       161     339      1128
    ## 130       1684        976     1076       7       161     339      1128
    ## 131       1684        976     1076       7       161     339      1128
    ## 132       1684        976     1076       7       161     339      1128
    ## 133       1684        976     1076       7       161     339      1128
    ## 134        805        452      451       0        30     133       466
    ## 135        805        452      451       0        30     133       466
    ## 136        805        452      451       0        30     133       466
    ## 137        805        452      451       0        30     133       466
    ## 138        805        452      451       0        30     133       466
    ## 139        805        452      451       0        30     133       466
    ## 140        805        452      451       0        30     133       466
    ## 141        805        452      451       0        30     133       466
    ## 142        805        452      451       0        30     133       466
    ## 143        642        624      652       3       117     156       844
    ## 144        642        624      652       3       117     156       844
    ## 145        642        624      652       3       117     156       844
    ## 146        642        624      652       3       117     156       844
    ## 147        642        624      652       3       117     156       844
    ## 148        642        624      652       3       117     156       844
    ## 149        642        624      652       3       117     156       844
    ## 150        642        624      652       3       117     156       844
    ## 151        642        624      652       3       117     156       844
    ## 152        848        440      482       5        38     105       416
    ## 153        848        440      482       5        38     105       416
    ## 154        848        440      482       5        38     105       416
    ## 155        848        440      482       5        38     105       416
    ## 156        848        440      482       5        38     105       416
    ## 157        848        440      482       5        38     105       416
    ## 158        848        440      482       5        38     105       416
    ## 159        848        440      482       5        38     105       416
    ## 160        848        440      482       5        38     105       416
    ## 161        870        452      455       6        17     132       441
    ## 162        870        452      455       6        17     132       441
    ## 163        870        452      455       6        17     132       441
    ## 164        870        452      455       6        17     132       441
    ## 165        870        452      455       6        17     132       441
    ## 166        870        452      455       6        17     132       441
    ## 167        870        452      455       6        17     132       441
    ## 168        870        452      455       6        17     132       441
    ## 169        870        452      455       6        17     132       441
    ## 170        758        416      392       1        19     122       359
    ## 171        758        416      392       1        19     122       359
    ## 172        758        416      392       1        19     122       359
    ## 173        758        416      392       1        19     122       359
    ## 174        758        416      392       1        19     122       359
    ## 175        758        416      392       1        19     122       359
    ## 176        758        416      392       1        19     122       359
    ## 177        758        416      392       1        19     122       359
    ## 178        758        416      392       1        19     122       359
    ## 179        797        407      444       2        21     122       499
    ## 180        797        407      444       2        21     122       499
    ## 181        797        407      444       2        21     122       499
    ## 182        797        407      444       2        21     122       499
    ## 183        797        407      444       2        21     122       499
    ## 184        797        407      444       2        21     122       499
    ## 185        797        407      444       2        21     122       499
    ## 186        797        407      444       2        21     122       499
    ## 187        797        407      444       2        21     122       499
    ## 188        781       1068     1093     341        81     265      1397
    ## 189        781       1068     1093     341        81     265      1397
    ## 190        781       1068     1093     341        81     265      1397
    ## 191        781       1068     1093     341        81     265      1397
    ## 192        781       1068     1093     341        81     265      1397
    ## 193        781       1068     1093     341        81     265      1397
    ## 194        781       1068     1093     341        81     265      1397
    ## 195        781       1068     1093     341        81     265      1397
    ## 196        781       1068     1093     341        81     265      1397
    ## 197        761        553      518      10        71     202       639
    ## 198        761        553      518      10        71     202       639
    ## 199        761        553      518      10        71     202       639
    ## 200        761        553      518      10        71     202       639
    ## 201        761        553      518      10        71     202       639
    ## 202        761        553      518      10        71     202       639
    ## 203        761        553      518      10        71     202       639
    ## 204        761        553      518      10        71     202       639
    ## 205        761        553      518      10        71     202       639
    ## 206        441        298      240      16        17      87       260
    ## 207        441        298      240      16        17      87       260
    ## 208        441        298      240      16        17      87       260
    ## 209        441        298      240      16        17      87       260
    ## 210        441        298      240      16        17      87       260
    ## 211        441        298      240      16        17      87       260
    ## 212        441        298      240      16        17      87       260
    ## 213        441        298      240      16        17      87       260
    ## 214        414        198      236       5         4      74       254
    ## 215        414        198      236       5         4      74       254
    ## 216        414        198      236       5         4      74       254
    ## 217        414        198      236       5         4      74       254
    ## 218        414        198      236       5         4      74       254
    ## 219        414        198      236       5         4      74       254
    ## 220        414        198      236       5         4      74       254
    ## 221        414        198      236       5         4      74       254
    ## 222        609        341      333       5         2     135       396
    ## 223        609        341      333       5         2     135       396
    ## 224        609        341      333       5         2     135       396
    ## 225        609        341      333       5         2     135       396
    ## 226        609        341      333       5         2     135       396
    ## 227        609        341      333       5         2     135       396
    ## 228        609        341      333       5         2     135       396
    ## 229        609        341      333       5         2     135       396
    ## 230        609        341      333       5         2     135       396
    ## 231       4207       2859     3025      17       668     813      3374
    ## 232       4207       2859     3025      17       668     813      3374
    ## 233       4207       2859     3025      17       668     813      3374
    ## 234       4207       2859     3025      17       668     813      3374
    ## 235       4207       2859     3025      17       668     813      3374
    ## 236       4207       2859     3025      17       668     813      3374
    ## 237       4207       2859     3025      17       668     813      3374
    ## 238       4207       2859     3025      17       668     813      3374
    ## 239       4207       2859     3025      17       668     813      3374
    ## 240        904        461      493       3         1     111       503
    ## 241        904        461      493       3         1     111       503
    ## 242        904        461      493       3         1     111       503
    ## 243        904        461      493       3         1     111       503
    ## 244        904        461      493       3         1     111       503
    ## 245        904        461      493       3         1     111       503
    ## 246        904        461      493       3         1     111       503
    ## 247        904        461      493       3         1     111       503
    ## 248        904        461      493       3         1     111       503
    ## 249        760        408      387       0        10     155       425
    ## 250        760        408      387       0        10     155       425
    ## 251        760        408      387       0        10     155       425
    ## 252        760        408      387       0        10     155       425
    ## 253        760        408      387       0        10     155       425
    ## 254        760        408      387       0        10     155       425
    ## 255        760        408      387       0        10     155       425
    ## 256        760        408      387       0        10     155       425
    ## 257        760        408      387       0        10     155       425
    ## 258       1605        869      895      12        32     265      1021
    ## 259       1605        869      895      12        32     265      1021
    ## 260       1605        869      895      12        32     265      1021
    ## 261       1605        869      895      12        32     265      1021
    ## 262       1605        869      895      12        32     265      1021
    ## 263       1605        869      895      12        32     265      1021
    ## 264       1605        869      895      12        32     265      1021
    ## 265       1605        869      895      12        32     265      1021
    ## 266       1605        869      895      12        32     265      1021
    ## 267       1125        582      585       0         6     158       670
    ## 268       1125        582      585       0         6     158       670
    ## 269       1125        582      585       0         6     158       670
    ## 270       1125        582      585       0         6     158       670
    ## 271       1125        582      585       0         6     158       670
    ## 272       1125        582      585       0         6     158       670
    ## 273       1125        582      585       0         6     158       670
    ## 274       1125        582      585       0         6     158       670
    ## 275       1125        582      585       0         6     158       670
    ## 276       1918        968     1055       7         1     368       870
    ## 277       1918        968     1055       7         1     368       870
    ## 278       1918        968     1055       7         1     368       870
    ## 279       1918        968     1055       7         1     368       870
    ## 280       1918        968     1055       7         1     368       870
    ## 281       1918        968     1055       7         1     368       870
    ## 282       1918        968     1055       7         1     368       870
    ## 283       1918        968     1055       7         1     368       870
    ## 284       1918        968     1055       7         1     368       870
    ## 285       1748        895      962       8        43     361       871
    ## 286       1748        895      962       8        43     361       871
    ## 287       1748        895      962       8        43     361       871
    ## 288       1748        895      962       8        43     361       871
    ## 289       1748        895      962       8        43     361       871
    ## 290       1748        895      962       8        43     361       871
    ## 291       1748        895      962       8        43     361       871
    ## 292       1748        895      962       8        43     361       871
    ## 293       1748        895      962       8        43     361       871
    ## 294       1004        612      633      33        73     147       438
    ## 295       1004        612      633      33        73     147       438
    ## 296       1004        612      633      33        73     147       438
    ## 297       1004        612      633      33        73     147       438
    ## 298       1004        612      633      33        73     147       438
    ## 299       1004        612      633      33        73     147       438
    ## 300       1004        612      633      33        73     147       438
    ## 301       1004        612      633      33        73     147       438
    ## 302       1004        612      633      33        73     147       438
    ## 303        663        831      832     139       174     266      1167
    ## 304        663        831      832     139       174     266      1167
    ## 305        663        831      832     139       174     266      1167
    ## 306        663        831      832     139       174     266      1167
    ## 307        663        831      832     139       174     266      1167
    ## 308        663        831      832     139       174     266      1167
    ## 309        663        831      832     139       174     266      1167
    ## 310        663        831      832     139       174     266      1167
    ## 311        663        831      832     139       174     266      1167
    ## 312       5269       3995     3968     196       347    1222      2255
    ## 313       5269       3995     3968     196       347    1222      2255
    ## 314       5269       3995     3968     196       347    1222      2255
    ## 315       5269       3995     3968     196       347    1222      2255
    ## 316       5269       3995     3968     196       347    1222      2255
    ## 317       5269       3995     3968     196       347    1222      2255
    ## 318       5269       3995     3968     196       347    1222      2255
    ## 319       5269       3995     3968     196       347    1222      2255
    ## 320       5269       3995     3968     196       347    1222      2255
    ## 321        841        697      751      23        63     183       340
    ## 322        841        697      751      23        63     183       340
    ## 323        841        697      751      23        63     183       340
    ## 324        841        697      751      23        63     183       340
    ## 325        841        697      751      23        63     183       340
    ## 326        841        697      751      23        63     183       340
    ## 327        841        697      751      23        63     183       340
    ## 328        841        697      751      23        63     183       340
    ## 329        841        697      751      23        63     183       340
    ## 330       1490        860      928      35        22     232       485
    ## 331       1490        860      928      35        22     232       485
    ## 332       1490        860      928      35        22     232       485
    ## 333       1490        860      928      35        22     232       485
    ## 334       1490        860      928      35        22     232       485
    ## 335       1490        860      928      35        22     232       485
    ## 336       1490        860      928      35        22     232       485
    ## 337       1490        860      928      35        22     232       485
    ## 338       1490        860      928      35        22     232       485
    ## 339        787        479      468      17        39     107       211
    ## 340        787        479      468      17        39     107       211
    ## 341        787        479      468      17        39     107       211
    ## 342        787        479      468      17        39     107       211
    ## 343        787        479      468      17        39     107       211
    ## 344        787        479      468      17        39     107       211
    ## 345        787        479      468      17        39     107       211
    ## 346        787        479      468      17        39     107       211
    ## 347        787        479      468      17        39     107       211
    ## 348       1565        826      807       5        20     192       499
    ## 349       1565        826      807       5        20     192       499
    ## 350       1565        826      807       5        20     192       499
    ## 351       1565        826      807       5        20     192       499
    ## 352       1565        826      807       5        20     192       499
    ## 353       1565        826      807       5        20     192       499
    ## 354       1565        826      807       5        20     192       499
    ## 355       1565        826      807       5        20     192       499
    ## 356       1565        826      807       5        20     192       499
    ## 357       7406       4993     5121     388       453    1154      1688
    ## 358       7406       4993     5121     388       453    1154      1688
    ## 359       7406       4993     5121     388       453    1154      1688
    ## 360       7406       4993     5121     388       453    1154      1688
    ## 361       7406       4993     5121     388       453    1154      1688
    ## 362       7406       4993     5121     388       453    1154      1688
    ## 363       7406       4993     5121     388       453    1154      1688
    ## 364       7406       4993     5121     388       453    1154      1688
    ## 365       7406       4993     5121     388       453    1154      1688
    ## 366       2115       1587     1797     135       164     338      1730
    ## 367       2115       1587     1797     135       164     338      1730
    ## 368       2115       1587     1797     135       164     338      1730
    ## 369       2115       1587     1797     135       164     338      1730
    ## 370       2115       1587     1797     135       164     338      1730
    ## 371       2115       1587     1797     135       164     338      1730
    ## 372       2115       1587     1797     135       164     338      1730
    ## 373       2115       1587     1797     135       164     338      1730
    ## 374       2115       1587     1797     135       164     338      1730
    ## 375        914       1105     1178       0       161     343      1508
    ## 376        914       1105     1178       0       161     343      1508
    ## 377        914       1105     1178       0       161     343      1508
    ## 378        914       1105     1178       0       161     343      1508
    ## 379        914       1105     1178       0       161     343      1508
    ## 380        914       1105     1178       0       161     343      1508
    ## 381        914       1105     1178       0       161     343      1508
    ## 382        914       1105     1178       0       161     343      1508
    ## 383        914       1105     1178       0       161     343      1508
    ## 384       1613        833      873       5        27     207       606
    ## 385       1613        833      873       5        27     207       606
    ## 386       1613        833      873       5        27     207       606
    ## 387       1613        833      873       5        27     207       606
    ## 388       1613        833      873       5        27     207       606
    ## 389       1613        833      873       5        27     207       606
    ## 390       1613        833      873       5        27     207       606
    ## 391       1613        833      873       5        27     207       606
    ## 392       1613        833      873       5        27     207       606
    ## 393       1736       1086     1156       1        71     436      1166
    ## 394       1736       1086     1156       1        71     436      1166
    ## 395       1736       1086     1156       1        71     436      1166
    ## 396       1736       1086     1156       1        71     436      1166
    ## 397       1736       1086     1156       1        71     436      1166
    ## 398       1736       1086     1156       1        71     436      1166
    ## 399       1736       1086     1156       1        71     436      1166
    ## 400       1736       1086     1156       1        71     436      1166
    ## 401       1736       1086     1156       1        71     436      1166
    ## 402       2466       1431     1481      46       143     364       711
    ## 403       2466       1431     1481      46       143     364       711
    ## 404       2466       1431     1481      46       143     364       711
    ## 405       2466       1431     1481      46       143     364       711
    ## 406       2466       1431     1481      46       143     364       711
    ## 407       2466       1431     1481      46       143     364       711
    ## 408       2466       1431     1481      46       143     364       711
    ## 409       2466       1431     1481      46       143     364       711
    ## 410       2466       1431     1481      46       143     364       711
    ## 411       3225       1754     1721       8        87     566       855
    ## 412       3225       1754     1721       8        87     566       855
    ## 413       3225       1754     1721       8        87     566       855
    ## 414       3225       1754     1721       8        87     566       855
    ## 415       3225       1754     1721       8        87     566       855
    ## 416       3225       1754     1721       8        87     566       855
    ## 417       3225       1754     1721       8        87     566       855
    ## 418       3225       1754     1721       8        87     566       855
    ## 419       3225       1754     1721       8        87     566       855
    ## 420       4199       2330     2423      39       181     783      1471
    ## 421       4199       2330     2423      39       181     783      1471
    ## 422       4199       2330     2423      39       181     783      1471
    ## 423       4199       2330     2423      39       181     783      1471
    ## 424       4199       2330     2423      39       181     783      1471
    ## 425       4199       2330     2423      39       181     783      1471
    ## 426       4199       2330     2423      39       181     783      1471
    ## 427       4199       2330     2423      39       181     783      1471
    ## 428       4199       2330     2423      39       181     783      1471
    ## 429       1279        956     1021     334        79     353      1542
    ## 430       1279        956     1021     334        79     353      1542
    ## 431       1279        956     1021     334        79     353      1542
    ## 432       1279        956     1021     334        79     353      1542
    ## 433       1279        956     1021     334        79     353      1542
    ## 434       1279        956     1021     334        79     353      1542
    ## 435       1279        956     1021     334        79     353      1542
    ## 436       1279        956     1021     334        79     353      1542
    ## 437       1279        956     1021     334        79     353      1542
    ## 438       5270       2830     2940      47       139     941      1095
    ## 439       5270       2830     2940      47       139     941      1095
    ## 440       5270       2830     2940      47       139     941      1095
    ## 441       5270       2830     2940      47       139     941      1095
    ## 442       5270       2830     2940      47       139     941      1095
    ## 443       5270       2830     2940      47       139     941      1095
    ## 444       5270       2830     2940      47       139     941      1095
    ## 445       5270       2830     2940      47       139     941      1095
    ## 446       5270       2830     2940      47       139     941      1095
    ## 447       1140        691      695       6        52     163       582
    ## 448       1140        691      695       6        52     163       582
    ## 449       1140        691      695       6        52     163       582
    ## 450       1140        691      695       6        52     163       582
    ## 451       1140        691      695       6        52     163       582
    ## 452       1140        691      695       6        52     163       582
    ## 453       1140        691      695       6        52     163       582
    ## 454       1140        691      695       6        52     163       582
    ## 455       1140        691      695       6        52     163       582
    ## 456       4398       2371     2379      26       116     710       679
    ## 457       4398       2371     2379      26       116     710       679
    ## 458       4398       2371     2379      26       116     710       679
    ## 459       4398       2371     2379      26       116     710       679
    ## 460       4398       2371     2379      26       116     710       679
    ## 461       4398       2371     2379      26       116     710       679
    ## 462       4398       2371     2379      26       116     710       679
    ## 463       4398       2371     2379      26       116     710       679
    ## 464       4398       2371     2379      26       116     710       679
    ## 465       5088       3404     3490     207       418    1250      3397
    ## 466       5088       3404     3490     207       418    1250      3397
    ## 467       5088       3404     3490     207       418    1250      3397
    ## 468       5088       3404     3490     207       418    1250      3397
    ## 469       5088       3404     3490     207       418    1250      3397
    ## 470       5088       3404     3490     207       418    1250      3397
    ## 471       5088       3404     3490     207       418    1250      3397
    ## 472       5088       3404     3490     207       418    1250      3397
    ## 473       5088       3404     3490     207       418    1250      3397
    ## 474        706        369      380       0        21     119       380
    ## 475        706        369      380       0        21     119       380
    ## 476        706        369      380       0        21     119       380
    ## 477        706        369      380       0        21     119       380
    ## 478        706        369      380       0        21     119       380
    ## 479        706        369      380       0        21     119       380
    ## 480        706        369      380       0        21     119       380
    ## 481        706        369      380       0        21     119       380
    ## 482        706        369      380       0        21     119       380
    ## 483        475        724      731       0        75     209      1089
    ## 484        475        724      731       0        75     209      1089
    ## 485        475        724      731       0        75     209      1089
    ## 486        475        724      731       0        75     209      1089
    ## 487        475        724      731       0        75     209      1089
    ## 488        475        724      731       0        75     209      1089
    ## 489        475        724      731       0        75     209      1089
    ## 490        475        724      731       0        75     209      1089
    ## 491        475        724      731       0        75     209      1089
    ## 492       1424        729      821      11        33     183       783
    ## 493       1424        729      821      11        33     183       783
    ## 494       1424        729      821      11        33     183       783
    ## 495       1424        729      821      11        33     183       783
    ## 496       1424        729      821      11        33     183       783
    ## 497       1424        729      821      11        33     183       783
    ## 498       1424        729      821      11        33     183       783
    ## 499       1424        729      821      11        33     183       783
    ## 500       1424        729      821      11        33     183       783
    ## 501       1672       1084     1222      47        75     282      1338
    ## 502       1672       1084     1222      47        75     282      1338
    ## 503       1672       1084     1222      47        75     282      1338
    ## 504       1672       1084     1222      47        75     282      1338
    ## 505       1672       1084     1222      47        75     282      1338
    ## 506       1672       1084     1222      47        75     282      1338
    ## 507       1672       1084     1222      47        75     282      1338
    ## 508       1672       1084     1222      47        75     282      1338
    ## 509       1672       1084     1222      47        75     282      1338
    ## 510        983        591      614      11        84     215       615
    ## 511        983        591      614      11        84     215       615
    ## 512        983        591      614      11        84     215       615
    ## 513        983        591      614      11        84     215       615
    ## 514        983        591      614      11        84     215       615
    ## 515        983        591      614      11        84     215       615
    ## 516        983        591      614      11        84     215       615
    ## 517        983        591      614      11        84     215       615
    ## 518        983        591      614      11        84     215       615
    ## 519       1059        568      683       9        51     154       475
    ## 520       1059        568      683       9        51     154       475
    ## 521       1059        568      683       9        51     154       475
    ## 522       1059        568      683       9        51     154       475
    ## 523       1059        568      683       9        51     154       475
    ## 524       1059        568      683       9        51     154       475
    ## 525       1059        568      683       9        51     154       475
    ## 526       1059        568      683       9        51     154       475
    ## 527       1059        568      683       9        51     154       475
    ## 528       1787        946      948       4        44     333       944
    ## 529       1787        946      948       4        44     333       944
    ## 530       1787        946      948       4        44     333       944
    ## 531       1787        946      948       4        44     333       944
    ## 532       1787        946      948       4        44     333       944
    ## 533       1787        946      948       4        44     333       944
    ## 534       1787        946      948       4        44     333       944
    ## 535       1787        946      948       4        44     333       944
    ## 536       1787        946      948       4        44     333       944
    ## 537       2441       1845     1872      96       279     533      1910
    ## 538       2441       1845     1872      96       279     533      1910
    ## 539       2441       1845     1872      96       279     533      1910
    ## 540       2441       1845     1872      96       279     533      1910
    ## 541       2441       1845     1872      96       279     533      1910
    ## 542       2441       1845     1872      96       279     533      1910
    ## 543       2441       1845     1872      96       279     533      1910
    ## 544       2441       1845     1872      96       279     533      1910
    ## 545       2441       1845     1872      96       279     533      1910
    ## 546        794        442      425       1        30     108       412
    ## 547        794        442      425       1        30     108       412
    ## 548        794        442      425       1        30     108       412
    ## 549        794        442      425       1        30     108       412
    ## 550        794        442      425       1        30     108       412
    ## 551        794        442      425       1        30     108       412
    ## 552        794        442      425       1        30     108       412
    ## 553        794        442      425       1        30     108       412
    ## 554        794        442      425       1        30     108       412
    ## 555       2374       1532     1659      40       242     542      1636
    ## 556       2374       1532     1659      40       242     542      1636
    ## 557       2374       1532     1659      40       242     542      1636
    ## 558       2374       1532     1659      40       242     542      1636
    ## 559       2374       1532     1659      40       242     542      1636
    ## 560       2374       1532     1659      40       242     542      1636
    ## 561       2374       1532     1659      40       242     542      1636
    ## 562       2374       1532     1659      40       242     542      1636
    ## 563       2374       1532     1659      40       242     542      1636
    ## 564        719        421      452      35        36     120       339
    ## 565        719        421      452      35        36     120       339
    ## 566        719        421      452      35        36     120       339
    ## 567        719        421      452      35        36     120       339
    ## 568        719        421      452      35        36     120       339
    ## 569        719        421      452      35        36     120       339
    ## 570        719        421      452      35        36     120       339
    ## 571        719        421      452      35        36     120       339
    ## 572        719        421      452      35        36     120       339
    ## 573       1164        641      652       7        60     188       682
    ## 574       1164        641      652       7        60     188       682
    ## 575       1164        641      652       7        60     188       682
    ## 576       1164        641      652       7        60     188       682
    ## 577       1164        641      652       7        60     188       682
    ## 578       1164        641      652       7        60     188       682
    ## 579       1164        641      652       7        60     188       682
    ## 580       1164        641      652       7        60     188       682
    ## 581       1164        641      652       7        60     188       682
    ## 582       1745        933      971       3        70     295       755
    ## 583       1745        933      971       3        70     295       755
    ## 584       1745        933      971       3        70     295       755
    ## 585       1745        933      971       3        70     295       755
    ## 586       1745        933      971       3        70     295       755
    ## 587       1745        933      971       3        70     295       755
    ## 588       1745        933      971       3        70     295       755
    ## 589       1745        933      971       3        70     295       755
    ## 590       1745        933      971       3        70     295       755
    ## 591       2395       1751     1767     151       267     405       687
    ## 592       2395       1751     1767     151       267     405       687
    ## 593       2395       1751     1767     151       267     405       687
    ## 594       2395       1751     1767     151       267     405       687
    ## 595       2395       1751     1767     151       267     405       687
    ## 596       2395       1751     1767     151       267     405       687
    ## 597       2395       1751     1767     151       267     405       687
    ## 598       2395       1751     1767     151       267     405       687
    ## 599       2395       1751     1767     151       267     405       687
    ## 600       1367       1510     1534     121       220     314      1841
    ## 601       1367       1510     1534     121       220     314      1841
    ## 602       1367       1510     1534     121       220     314      1841
    ## 603       1367       1510     1534     121       220     314      1841
    ## 604       1367       1510     1534     121       220     314      1841
    ## 605       1367       1510     1534     121       220     314      1841
    ## 606       1367       1510     1534     121       220     314      1841
    ## 607       1367       1510     1534     121       220     314      1841
    ## 608       1367       1510     1534     121       220     314      1841
    ## 609       2537       1733     1883      79       168     384      1272
    ## 610       2537       1733     1883      79       168     384      1272
    ## 611       2537       1733     1883      79       168     384      1272
    ## 612       2537       1733     1883      79       168     384      1272
    ## 613       2537       1733     1883      79       168     384      1272
    ## 614       2537       1733     1883      79       168     384      1272
    ## 615       2537       1733     1883      79       168     384      1272
    ## 616       2537       1733     1883      79       168     384      1272
    ## 617       2537       1733     1883      79       168     384      1272
    ## 618       3885       2140     2270      24       124     487      1206
    ## 619       3885       2140     2270      24       124     487      1206
    ## 620       3885       2140     2270      24       124     487      1206
    ## 621       3885       2140     2270      24       124     487      1206
    ## 622       3885       2140     2270      24       124     487      1206
    ## 623       3885       2140     2270      24       124     487      1206
    ## 624       3885       2140     2270      24       124     487      1206
    ## 625       3885       2140     2270      24       124     487      1206
    ## 626       3885       2140     2270      24       124     487      1206
    ## 627        732        490      517      10        61     179       521
    ## 628        732        490      517      10        61     179       521
    ## 629        732        490      517      10        61     179       521
    ## 630        732        490      517      10        61     179       521
    ## 631        732        490      517      10        61     179       521
    ## 632        732        490      517      10        61     179       521
    ## 633        732        490      517      10        61     179       521
    ## 634        732        490      517      10        61     179       521
    ## 635        732        490      517      10        61     179       521
    ## 636       2886       1809     1944      56       215     389      1205
    ## 637       2886       1809     1944      56       215     389      1205
    ## 638       2886       1809     1944      56       215     389      1205
    ## 639       2886       1809     1944      56       215     389      1205
    ## 640       2886       1809     1944      56       215     389      1205
    ## 641       2886       1809     1944      56       215     389      1205
    ## 642       2886       1809     1944      56       215     389      1205
    ## 643       2886       1809     1944      56       215     389      1205
    ## 644       2886       1809     1944      56       215     389      1205
    ## 645       3220       2760     2843     354       347     540      2382
    ## 646       3220       2760     2843     354       347     540      2382
    ## 647       3220       2760     2843     354       347     540      2382
    ## 648       3220       2760     2843     354       347     540      2382
    ## 649       3220       2760     2843     354       347     540      2382
    ## 650       3220       2760     2843     354       347     540      2382
    ## 651       3220       2760     2843     354       347     540      2382
    ## 652       3220       2760     2843     354       347     540      2382
    ## 653       3220       2760     2843     354       347     540      2382
    ## 654       2635       1615     1693      41       182     482      1454
    ## 655       2635       1615     1693      41       182     482      1454
    ## 656       2635       1615     1693      41       182     482      1454
    ## 657       2635       1615     1693      41       182     482      1454
    ## 658       2635       1615     1693      41       182     482      1454
    ## 659       2635       1615     1693      41       182     482      1454
    ## 660       2635       1615     1693      41       182     482      1454
    ## 661       2635       1615     1693      41       182     482      1454
    ## 662       2635       1615     1693      41       182     482      1454
    ## 663       6880       4039     4283     165       291     766      1823
    ## 664       6880       4039     4283     165       291     766      1823
    ## 665       6880       4039     4283     165       291     766      1823
    ## 666       6880       4039     4283     165       291     766      1823
    ## 667       6880       4039     4283     165       291     766      1823
    ## 668       6880       4039     4283     165       291     766      1823
    ## 669       6880       4039     4283     165       291     766      1823
    ## 670       6880       4039     4283     165       291     766      1823
    ## 671       6880       4039     4283     165       291     766      1823
    ## 672       1571       1905     1965     237       174     585      2616
    ## 673       1571       1905     1965     237       174     585      2616
    ## 674       1571       1905     1965     237       174     585      2616
    ## 675       1571       1905     1965     237       174     585      2616
    ## 676       1571       1905     1965     237       174     585      2616
    ## 677       1571       1905     1965     237       174     585      2616
    ## 678       1571       1905     1965     237       174     585      2616
    ## 679       1571       1905     1965     237       174     585      2616
    ## 680       1571       1905     1965     237       174     585      2616
    ## 681        105       3466     3590    1349        46     766      4340
    ## 682        105       3466     3590    1349        46     766      4340
    ## 683        105       3466     3590    1349        46     766      4340
    ## 684        105       3466     3590    1349        46     766      4340
    ## 685        105       3466     3590    1349        46     766      4340
    ## 686        105       3466     3590    1349        46     766      4340
    ## 687        105       3466     3590    1349        46     766      4340
    ## 688        105       3466     3590    1349        46     766      4340
    ## 689        105       3466     3590    1349        46     766      4340
    ## 690       3499       3598     3642     237        55     845      1771
    ## 691       3499       3598     3642     237        55     845      1771
    ## 692       3499       3598     3642     237        55     845      1771
    ## 693       3499       3598     3642     237        55     845      1771
    ## 694       3499       3598     3642     237        55     845      1771
    ## 695       3499       3598     3642     237        55     845      1771
    ## 696       3499       3598     3642     237        55     845      1771
    ## 697       3499       3598     3642     237        55     845      1771
    ## 698       3499       3598     3642     237        55     845      1771
    ## 699       4598       3540     3701     188       105     835      1417
    ## 700       4598       3540     3701     188       105     835      1417
    ## 701       4598       3540     3701     188       105     835      1417
    ## 702       4598       3540     3701     188       105     835      1417
    ## 703       4598       3540     3701     188       105     835      1417
    ## 704       4598       3540     3701     188       105     835      1417
    ## 705       4598       3540     3701     188       105     835      1417
    ## 706       4598       3540     3701     188       105     835      1417
    ## 707       4598       3540     3701     188       105     835      1417
    ## 708       1970       1103     1146      26        28     326       272
    ## 709       1970       1103     1146      26        28     326       272
    ## 710       1970       1103     1146      26        28     326       272
    ## 711       1970       1103     1146      26        28     326       272
    ## 712       1970       1103     1146      26        28     326       272
    ## 713       1970       1103     1146      26        28     326       272
    ## 714       1970       1103     1146      26        28     326       272
    ## 715       1970       1103     1146      26        28     326       272
    ## 716       1970       1103     1146      26        28     326       272
    ## 717        394       3344     3724    1491       120     940      4250
    ## 718        394       3344     3724    1491       120     940      4250
    ## 719        394       3344     3724    1491       120     940      4250
    ## 720        394       3344     3724    1491       120     940      4250
    ## 721        394       3344     3724    1491       120     940      4250
    ## 722        394       3344     3724    1491       120     940      4250
    ## 723        394       3344     3724    1491       120     940      4250
    ## 724        394       3344     3724    1491       120     940      4250
    ## 725        394       3344     3724    1491       120     940      4250
    ## 726        465        914     1050     128        54     265       717
    ## 727        465        914     1050     128        54     265       717
    ## 728        465        914     1050     128        54     265       717
    ## 729        465        914     1050     128        54     265       717
    ## 730        465        914     1050     128        54     265       717
    ## 731        465        914     1050     128        54     265       717
    ## 732        319       1200     1331     519        13     428      1913
    ## 733        319       1200     1331     519        13     428      1913
    ## 734        319       1200     1331     519        13     428      1913
    ## 735        319       1200     1331     519        13     428      1913
    ## 736        319       1200     1331     519        13     428      1913
    ## 737        319       1200     1331     519        13     428      1913
    ## 738        319       1200     1331     519        13     428      1913
    ## 739        319       1200     1331     519        13     428      1913
    ## 740        319       1200     1331     519        13     428      1913
    ## 741       2632       1725     1856      45       114     478       514
    ## 742       2632       1725     1856      45       114     478       514
    ## 743       2632       1725     1856      45       114     478       514
    ## 744       2632       1725     1856      45       114     478       514
    ## 745       2632       1725     1856      45       114     478       514
    ## 746       2632       1725     1856      45       114     478       514
    ## 747       2632       1725     1856      45       114     478       514
    ## 748       2632       1725     1856      45       114     478       514
    ## 749       2632       1725     1856      45       114     478       514
    ## 750        862        726      753      33        38     222       220
    ## 751        862        726      753      33        38     222       220
    ## 752        862        726      753      33        38     222       220
    ## 753        862        726      753      33        38     222       220
    ## 754        862        726      753      33        38     222       220
    ## 755        862        726      753      33        38     222       220
    ## 756        403        829      932     183        49     317       896
    ## 757        403        829      932     183        49     317       896
    ## 758        403        829      932     183        49     317       896
    ## 759        403        829      932     183        49     317       896
    ## 760        403        829      932     183        49     317       896
    ## 761        403        829      932     183        49     317       896
    ## 762        403        829      932     183        49     317       896
    ## 763        403        829      932     183        49     317       896
    ## 764        403        829      932     183        49     317       896
    ## 765        861        572      605      30        22     181       121
    ## 766        861        572      605      30        22     181       121
    ## 767        861        572      605      30        22     181       121
    ## 768        861        572      605      30        22     181       121
    ## 769        861        572      605      30        22     181       121
    ## 770        861        572      605      30        22     181       121
    ## 771         69        720      744     114        44     138       706
    ## 772         69        720      744     114        44     138       706
    ## 773         69        720      744     114        44     138       706
    ## 774         69        720      744     114        44     138       706
    ## 775         69        720      744     114        44     138       706
    ## 776         69        720      744     114        44     138       706
    ## 777       2169       1583     1633       0        50     317       460
    ## 778       2169       1583     1633       0        50     317       460
    ## 779       2169       1583     1633       0        50     317       460
    ## 780       2169       1583     1633       0        50     317       460
    ## 781       2169       1583     1633       0        50     317       460
    ## 782       2169       1583     1633       0        50     317       460
    ## 783       2169       1583     1633       0        50     317       460
    ## 784       2169       1583     1633       0        50     317       460
    ## 785       2169       1583     1633       0        50     317       460
    ## 786        413        861      924     130        37     179       373
    ## 787        413        861      924     130        37     179       373
    ## 788        413        861      924     130        37     179       373
    ## 789        413        861      924     130        37     179       373
    ## 790        413        861      924     130        37     179       373
    ## 791        413        861      924     130        37     179       373
    ## 792       3010       3245     3591     351       177    1023      1349
    ## 793       3010       3245     3591     351       177    1023      1349
    ## 794       3010       3245     3591     351       177    1023      1349
    ## 795       3010       3245     3591     351       177    1023      1349
    ## 796       3010       3245     3591     351       177    1023      1349
    ## 797       3010       3245     3591     351       177    1023      1349
    ## 798       3010       3245     3591     351       177    1023      1349
    ## 799       3010       3245     3591     351       177    1023      1349
    ## 800       3010       3245     3591     351       177    1023      1349
    ## 801       1960       1227     1321       0        95     356         1
    ## 802       1960       1227     1321       0        95     356         1
    ## 803       1960       1227     1321       0        95     356         1
    ## 804       1960       1227     1321       0        95     356         1
    ## 805       1960       1227     1321       0        95     356         1
    ## 806       1960       1227     1321       0        95     356         1
    ## 807       1960       1227     1321       0        95     356         1
    ## 808       1960       1227     1321       0        95     356         1
    ## 809       1960       1227     1321       0        95     356         1
    ## 810       3287       2409     2547     124        88     698       549
    ## 811       3287       2409     2547     124        88     698       549
    ## 812       3287       2409     2547     124        88     698       549
    ## 813       3287       2409     2547     124        88     698       549
    ## 814       3287       2409     2547     124        88     698       549
    ## 815       3287       2409     2547     124        88     698       549
    ## 816       3287       2409     2547     124        88     698       549
    ## 817       3287       2409     2547     124        88     698       549
    ## 818       3287       2409     2547     124        88     698       549
    ## 819        996        776      789     153        53     220       420
    ## 820        996        776      789     153        53     220       420
    ## 821        996        776      789     153        53     220       420
    ## 822        996        776      789     153        53     220       420
    ## 823        996        776      789     153        53     220       420
    ## 824        996        776      789     153        53     220       420
    ## 825        996        776      789     153        53     220       420
    ## 826        996        776      789     153        53     220       420
    ## 827        931       1465     1690     151        40     338       388
    ## 828        931       1465     1690     151        40     338       388
    ## 829        931       1465     1690     151        40     338       388
    ## 830        931       1465     1690     151        40     338       388
    ## 831        931       1465     1690     151        40     338       388
    ## 832        931       1465     1690     151        40     338       388
    ## 833        931       1465     1690     151        40     338       388
    ## 834        931       1465     1690     151        40     338       388
    ## 835        931       1465     1690     151        40     338       388
    ## 836       1392       2582     2810     526       110     668      1848
    ## 837       1392       2582     2810     526       110     668      1848
    ## 838       1392       2582     2810     526       110     668      1848
    ## 839       1392       2582     2810     526       110     668      1848
    ## 840       1392       2582     2810     526       110     668      1848
    ## 841       1392       2582     2810     526       110     668      1848
    ## 842       1392       2582     2810     526       110     668      1848
    ## 843       1392       2582     2810     526       110     668      1848
    ## 844       1392       2582     2810     526       110     668      1848
    ## 845       6004       3376     3337      28       117     827       593
    ## 846       6004       3376     3337      28       117     827       593
    ## 847       6004       3376     3337      28       117     827       593
    ## 848       6004       3376     3337      28       117     827       593
    ## 849       6004       3376     3337      28       117     827       593
    ## 850       6004       3376     3337      28       117     827       593
    ## 851       6004       3376     3337      28       117     827       593
    ## 852       6004       3376     3337      28       117     827       593
    ## 853       6004       3376     3337      28       117     827       593
    ## 854       1809        948     1045      13        63     341       464
    ## 855       1809        948     1045      13        63     341       464
    ## 856       1809        948     1045      13        63     341       464
    ## 857       1809        948     1045      13        63     341       464
    ## 858       1809        948     1045      13        63     341       464
    ## 859       1809        948     1045      13        63     341       464
    ## 860       1809        948     1045      13        63     341       464
    ## 861       1809        948     1045      13        63     341       464
    ## 862       1809        948     1045      13        63     341       464
    ## 863       3186       2208     2294      42       368     794      2563
    ## 864       3186       2208     2294      42       368     794      2563
    ## 865       3186       2208     2294      42       368     794      2563
    ## 866       3186       2208     2294      42       368     794      2563
    ## 867       3186       2208     2294      42       368     794      2563
    ## 868       3186       2208     2294      42       368     794      2563
    ## 869       3186       2208     2294      42       368     794      2563
    ## 870       3186       2208     2294      42       368     794      2563
    ## 871       3186       2208     2294      42       368     794      2563
    ## 872       2749       1460     1486      30        83     328       529
    ## 873       2749       1460     1486      30        83     328       529
    ## 874       2749       1460     1486      30        83     328       529
    ## 875       2749       1460     1486      30        83     328       529
    ## 876       2749       1460     1486      30        83     328       529
    ## 877       2749       1460     1486      30        83     328       529
    ## 878       2749       1460     1486      30        83     328       529
    ## 879       2749       1460     1486      30        83     328       529
    ## 880       2749       1460     1486      30        83     328       529
    ## 881        977        515      560      13        47     149       459
    ## 882        977        515      560      13        47     149       459
    ## 883        977        515      560      13        47     149       459
    ## 884        977        515      560      13        47     149       459
    ## 885        977        515      560      13        47     149       459
    ## 886        977        515      560      13        47     149       459
    ## 887        977        515      560      13        47     149       459
    ## 888        977        515      560      13        47     149       459
    ## 889        480        274      267       4        14      67       258
    ## 890        480        274      267       4        14      67       258
    ## 891        480        274      267       4        14      67       258
    ## 892        480        274      267       4        14      67       258
    ## 893        480        274      267       4        14      67       258
    ## 894        480        274      267       4        14      67       258
    ## 895        480        274      267       4        14      67       258
    ## 896        903        498      477       4        29     105       383
    ## 897        903        498      477       4        29     105       383
    ## 898        903        498      477       4        29     105       383
    ## 899        903        498      477       4        29     105       383
    ## 900        903        498      477       4        29     105       383
    ## 901        903        498      477       4        29     105       383
    ## 902        903        498      477       4        29     105       383
    ## 903        903        498      477       4        29     105       383
    ## 904        903        498      477       4        29     105       383
    ## 905       4443       2745     2868      67       138    1024      3625
    ## 906       4443       2745     2868      67       138    1024      3625
    ## 907       4443       2745     2868      67       138    1024      3625
    ## 908       4443       2745     2868      67       138    1024      3625
    ## 909       4443       2745     2868      67       138    1024      3625
    ## 910       4443       2745     2868      67       138    1024      3625
    ## 911       4443       2745     2868      67       138    1024      3625
    ## 912       4443       2745     2868      67       138    1024      3625
    ## 913       4443       2745     2868      67       138    1024      3625
    ## 914       2826       1544     1599      37        99     471      1120
    ## 915       2826       1544     1599      37        99     471      1120
    ## 916       2826       1544     1599      37        99     471      1120
    ## 917       2826       1544     1599      37        99     471      1120
    ## 918       2826       1544     1599      37        99     471      1120
    ## 919       2826       1544     1599      37        99     471      1120
    ## 920       2826       1544     1599      37        99     471      1120
    ## 921       2826       1544     1599      37        99     471      1120
    ## 922       2826       1544     1599      37        99     471      1120
    ## 923       2900       1659     1752      71       106     586      1362
    ## 924       2900       1659     1752      71       106     586      1362
    ## 925       2900       1659     1752      71       106     586      1362
    ## 926       2900       1659     1752      71       106     586      1362
    ## 927       2900       1659     1752      71       106     586      1362
    ## 928       2900       1659     1752      71       106     586      1362
    ## 929       2900       1659     1752      71       106     586      1362
    ## 930       2900       1659     1752      71       106     586      1362
    ## 931       2900       1659     1752      71       106     586      1362
    ## 932       1977       1289     1405      72       155     277       684
    ## 933       1977       1289     1405      72       155     277       684
    ## 934       1977       1289     1405      72       155     277       684
    ## 935       1977       1289     1405      72       155     277       684
    ## 936       1977       1289     1405      72       155     277       684
    ## 937       1977       1289     1405      72       155     277       684
    ## 938       1977       1289     1405      72       155     277       684
    ## 939       1977       1289     1405      72       155     277       684
    ## 940       1977       1289     1405      72       155     277       684
    ## 941        577        309      302       9         6      51       213
    ## 942        577        309      302       9         6      51       213
    ## 943        577        309      302       9         6      51       213
    ## 944        577        309      302       9         6      51       213
    ## 945        577        309      302       9         6      51       213
    ## 946        577        309      302       9         6      51       213
    ## 947        577        309      302       9         6      51       213
    ## 948        577        309      302       9         6      51       213
    ## 949        577        309      302       9         6      51       213
    ## 950       1045        720      733      59       131     229       962
    ## 951       1045        720      733      59       131     229       962
    ## 952       1045        720      733      59       131     229       962
    ## 953       1045        720      733      59       131     229       962
    ## 954       1045        720      733      59       131     229       962
    ## 955       1045        720      733      59       131     229       962
    ## 956       1045        720      733      59       131     229       962
    ## 957       1045        720      733      59       131     229       962
    ## 958       1045        720      733      59       131     229       962
    ## 959       4720       2573     2816      29       257     758      1511
    ## 960       4720       2573     2816      29       257     758      1511
    ## 961       4720       2573     2816      29       257     758      1511
    ## 962       4720       2573     2816      29       257     758      1511
    ## 963       4720       2573     2816      29       257     758      1511
    ## 964       4720       2573     2816      29       257     758      1511
    ## 965       4720       2573     2816      29       257     758      1511
    ## 966       4720       2573     2816      29       257     758      1511
    ## 967       4720       2573     2816      29       257     758      1511
    ## 968       3370       2093     2110      35       126     391       622
    ## 969       3370       2093     2110      35       126     391       622
    ## 970       3370       2093     2110      35       126     391       622
    ## 971       3370       2093     2110      35       126     391       622
    ## 972       3370       2093     2110      35       126     391       622
    ## 973       3370       2093     2110      35       126     391       622
    ## 974       3370       2093     2110      35       126     391       622
    ## 975       3370       2093     2110      35       126     391       622
    ## 976       3370       2093     2110      35       126     391       622
    ## 977        611        432      398      19        53     118       412
    ## 978        611        432      398      19        53     118       412
    ## 979        611        432      398      19        53     118       412
    ## 980        611        432      398      19        53     118       412
    ## 981        611        432      398      19        53     118       412
    ## 982        611        432      398      19        53     118       412
    ## 983        611        432      398      19        53     118       412
    ## 984        611        432      398      19        53     118       412
    ## 985        611        432      398      19        53     118       412
    ## 986       4566      10154    10903    3382      1526    4222     15638
    ## 987       4566      10154    10903    3382      1526    4222     15638
    ## 988       4566      10154    10903    3382      1526    4222     15638
    ## 989       4566      10154    10903    3382      1526    4222     15638
    ## 990       4566      10154    10903    3382      1526    4222     15638
    ## 991       4566      10154    10903    3382      1526    4222     15638
    ## 992       4566      10154    10903    3382      1526    4222     15638
    ## 993       4566      10154    10903    3382      1526    4222     15638
    ## 994       4566      10154    10903    3382      1526    4222     15638
    ## 995        721        361      409       4        19      92       260
    ## 996        721        361      409       4        19      92       260
    ## 997        721        361      409       4        19      92       260
    ## 998        721        361      409       4        19      92       260
    ## 999        721        361      409       4        19      92       260
    ## 1000       721        361      409       4        19      92       260
    ## 1001       721        361      409       4        19      92       260
    ## 1002       721        361      409       4        19      92       260
    ## 1003       721        361      409       4        19      92       260
    ## 1004       935       1097     1077     195       255     269      1299
    ## 1005       935       1097     1077     195       255     269      1299
    ## 1006       935       1097     1077     195       255     269      1299
    ## 1007       935       1097     1077     195       255     269      1299
    ## 1008       935       1097     1077     195       255     269      1299
    ## 1009       935       1097     1077     195       255     269      1299
    ## 1010       935       1097     1077     195       255     269      1299
    ## 1011       935       1097     1077     195       255     269      1299
    ## 1012       935       1097     1077     195       255     269      1299
    ## 1013      1133        559      611       2         7     181       642
    ## 1014      1133        559      611       2         7     181       642
    ## 1015      1133        559      611       2         7     181       642
    ## 1016      1133        559      611       2         7     181       642
    ## 1017      1133        559      611       2         7     181       642
    ## 1018      1133        559      611       2         7     181       642
    ## 1019      1133        559      611       2         7     181       642
    ## 1020      1133        559      611       2         7     181       642
    ## 1021      1133        559      611       2         7     181       642
    ## 1022       705        378      421       5        35      94       394
    ## 1023       705        378      421       5        35      94       394
    ## 1024       705        378      421       5        35      94       394
    ## 1025       705        378      421       5        35      94       394
    ## 1026       705        378      421       5        35      94       394
    ## 1027       705        378      421       5        35      94       394
    ## 1028       705        378      421       5        35      94       394
    ## 1029       705        378      421       5        35      94       394
    ## 1030       705        378      421       5        35      94       394
    ## 1031      3656       2204     2218     104       182     496       953
    ## 1032      3656       2204     2218     104       182     496       953
    ## 1033      3656       2204     2218     104       182     496       953
    ## 1034      3656       2204     2218     104       182     496       953
    ## 1035      3656       2204     2218     104       182     496       953
    ## 1036      3656       2204     2218     104       182     496       953
    ## 1037      3656       2204     2218     104       182     496       953
    ## 1038      3656       2204     2218     104       182     496       953
    ## 1039      3656       2204     2218     104       182     496       953
    ## 1040      2355       1944     1973      56       104     489      1187
    ## 1041      2355       1944     1973      56       104     489      1187
    ## 1042      2355       1944     1973      56       104     489      1187
    ## 1043      2355       1944     1973      56       104     489      1187
    ## 1044      2355       1944     1973      56       104     489      1187
    ## 1045      2355       1944     1973      56       104     489      1187
    ## 1046      2355       1944     1973      56       104     489      1187
    ## 1047      2355       1944     1973      56       104     489      1187
    ## 1048      2355       1944     1973      56       104     489      1187
    ## 1049      2001       1469     1633      47        99     378       622
    ## 1050      2001       1469     1633      47        99     378       622
    ## 1051      2001       1469     1633      47        99     378       622
    ## 1052      2001       1469     1633      47        99     378       622
    ## 1053      2001       1469     1633      47        99     378       622
    ## 1054      2001       1469     1633      47        99     378       622
    ## 1055      2001       1469     1633      47        99     378       622
    ## 1056      2001       1469     1633      47        99     378       622
    ## 1057      2001       1469     1633      47        99     378       622
    ## 1058      3077       3196     3270     351       170    1053      2118
    ## 1059      3077       3196     3270     351       170    1053      2118
    ## 1060      3077       3196     3270     351       170    1053      2118
    ## 1061      3077       3196     3270     351       170    1053      2118
    ## 1062      3077       3196     3270     351       170    1053      2118
    ## 1063      3077       3196     3270     351       170    1053      2118
    ## 1064      3077       3196     3270     351       170    1053      2118
    ## 1065      3077       3196     3270     351       170    1053      2118
    ## 1066      3077       3196     3270     351       170    1053      2118
    ## 1067      1921       5554     6005    1648       464    1713      7014
    ## 1068      1921       5554     6005    1648       464    1713      7014
    ## 1069      1921       5554     6005    1648       464    1713      7014
    ## 1070      1921       5554     6005    1648       464    1713      7014
    ## 1071      1921       5554     6005    1648       464    1713      7014
    ## 1072      1921       5554     6005    1648       464    1713      7014
    ## 1073      1921       5554     6005    1648       464    1713      7014
    ## 1074      1921       5554     6005    1648       464    1713      7014
    ## 1075      1921       5554     6005    1648       464    1713      7014
    ## 1076      2669       1751     1818      55       126     482       726
    ## 1077      2669       1751     1818      55       126     482       726
    ## 1078      2669       1751     1818      55       126     482       726
    ## 1079      2669       1751     1818      55       126     482       726
    ## 1080      2669       1751     1818      55       126     482       726
    ## 1081      2669       1751     1818      55       126     482       726
    ## 1082      2669       1751     1818      55       126     482       726
    ## 1083      2669       1751     1818      55       126     482       726
    ## 1084      2669       1751     1818      55       126     482       726
    ## 1085       650        351      370       2        24     113       383
    ## 1086       650        351      370       2        24     113       383
    ## 1087       650        351      370       2        24     113       383
    ## 1088       650        351      370       2        24     113       383
    ## 1089       650        351      370       2        24     113       383
    ## 1090       650        351      370       2        24     113       383
    ## 1091       650        351      370       2        24     113       383
    ## 1092       650        351      370       2        24     113       383
    ## 1093       650        351      370       2        24     113       383
    ## 1094       827        493      485       9        58     137       520
    ## 1095       827        493      485       9        58     137       520
    ## 1096       827        493      485       9        58     137       520
    ## 1097       827        493      485       9        58     137       520
    ## 1098       827        493      485       9        58     137       520
    ## 1099       827        493      485       9        58     137       520
    ## 1100       827        493      485       9        58     137       520
    ## 1101       827        493      485       9        58     137       520
    ## 1102       827        493      485       9        58     137       520
    ## 1103      1063        569      562       0        30     231       746
    ## 1104      1063        569      562       0        30     231       746
    ## 1105      1063        569      562       0        30     231       746
    ## 1106      1063        569      562       0        30     231       746
    ## 1107      1063        569      562       0        30     231       746
    ## 1108      1063        569      562       0        30     231       746
    ## 1109      1063        569      562       0        30     231       746
    ## 1110      1063        569      562       0        30     231       746
    ## 1111      1063        569      562       0        30     231       746
    ## 1112      3016       1619     1742      37        89     591      2088
    ## 1113      3016       1619     1742      37        89     591      2088
    ## 1114      3016       1619     1742      37        89     591      2088
    ## 1115      3016       1619     1742      37        89     591      2088
    ## 1116      3016       1619     1742      37        89     591      2088
    ## 1117      3016       1619     1742      37        89     591      2088
    ## 1118      3016       1619     1742      37        89     591      2088
    ## 1119      3016       1619     1742      37        89     591      2088
    ## 1120      3016       1619     1742      37        89     591      2088
    ## 1121      1261        621      689       7        12     200       946
    ## 1122      1261        621      689       7        12     200       946
    ## 1123      1261        621      689       7        12     200       946
    ## 1124      1261        621      689       7        12     200       946
    ## 1125      1261        621      689       7        12     200       946
    ## 1126      1261        621      689       7        12     200       946
    ## 1127      1261        621      689       7        12     200       946
    ## 1128      1261        621      689       7        12     200       946
    ## 1129      1261        621      689       7        12     200       946
    ## 1130       748        361      423       1        15     103       383
    ## 1131       748        361      423       1        15     103       383
    ## 1132       748        361      423       1        15     103       383
    ## 1133       748        361      423       1        15     103       383
    ## 1134       748        361      423       1        15     103       383
    ## 1135       748        361      423       1        15     103       383
    ## 1136       748        361      423       1        15     103       383
    ## 1137       748        361      423       1        15     103       383
    ## 1138       748        361      423       1        15     103       383
    ## 1139      1615        838      866       1        48     172       885
    ## 1140      1615        838      866       1        48     172       885
    ## 1141      1615        838      866       1        48     172       885
    ## 1142      1615        838      866       1        48     172       885
    ## 1143      1615        838      866       1        48     172       885
    ## 1144      1615        838      866       1        48     172       885
    ## 1145      1615        838      866       1        48     172       885
    ## 1146      1615        838      866       1        48     172       885
    ## 1147      1615        838      866       1        48     172       885
    ## 1148      1438        881      949      18        61     328       434
    ## 1149      1438        881      949      18        61     328       434
    ## 1150      1438        881      949      18        61     328       434
    ## 1151      1438        881      949      18        61     328       434
    ## 1152      1438        881      949      18        61     328       434
    ## 1153      1438        881      949      18        61     328       434
    ## 1154      1438        881      949      18        61     328       434
    ## 1155      1438        881      949      18        61     328       434
    ## 1156      1438        881      949      18        61     328       434
    ## 1157       746        405      413       0         3      88       231
    ## 1158       746        405      413       0         3      88       231
    ## 1159       746        405      413       0         3      88       231
    ## 1160       746        405      413       0         3      88       231
    ## 1161       746        405      413       0         3      88       231
    ## 1162       746        405      413       0         3      88       231
    ## 1163       746        405      413       0         3      88       231
    ## 1164       746        405      413       0         3      88       231
    ## 1165       746        405      413       0         3      88       231
    ## 1166      2478       1945     2028     158       120     608      1257
    ## 1167      2478       1945     2028     158       120     608      1257
    ## 1168      2478       1945     2028     158       120     608      1257
    ## 1169      2478       1945     2028     158       120     608      1257
    ## 1170      2478       1945     2028     158       120     608      1257
    ## 1171      2478       1945     2028     158       120     608      1257
    ## 1172      2478       1945     2028     158       120     608      1257
    ## 1173      2478       1945     2028     158       120     608      1257
    ## 1174      2478       1945     2028     158       120     608      1257
    ## 1175       657        373      421      12        13      99       134
    ## 1176       657        373      421      12        13      99       134
    ## 1177       657        373      421      12        13      99       134
    ## 1178       657        373      421      12        13      99       134
    ## 1179       657        373      421      12        13      99       134
    ## 1180       657        373      421      12        13      99       134
    ## 1181       657        373      421      12        13      99       134
    ## 1182      1561       1450     1616     361        84     411      1212
    ## 1183      1561       1450     1616     361        84     411      1212
    ## 1184      1561       1450     1616     361        84     411      1212
    ## 1185      1561       1450     1616     361        84     411      1212
    ## 1186      1561       1450     1616     361        84     411      1212
    ## 1187      1561       1450     1616     361        84     411      1212
    ## 1188      1561       1450     1616     361        84     411      1212
    ## 1189      1561       1450     1616     361        84     411      1212
    ## 1190      1561       1450     1616     361        84     411      1212
    ## 1191      3225       2038     2050      56       219     587      1037
    ## 1192      3225       2038     2050      56       219     587      1037
    ## 1193      3225       2038     2050      56       219     587      1037
    ## 1194      3225       2038     2050      56       219     587      1037
    ## 1195      3225       2038     2050      56       219     587      1037
    ## 1196      3225       2038     2050      56       219     587      1037
    ## 1197      3225       2038     2050      56       219     587      1037
    ## 1198      3225       2038     2050      56       219     587      1037
    ## 1199      3225       2038     2050      56       219     587      1037
    ## 1200      2434       1328     1337      13        96     448       658
    ## 1201      2434       1328     1337      13        96     448       658
    ## 1202      2434       1328     1337      13        96     448       658
    ## 1203      2434       1328     1337      13        96     448       658
    ## 1204      2434       1328     1337      13        96     448       658
    ## 1205      2434       1328     1337      13        96     448       658
    ## 1206      2434       1328     1337      13        96     448       658
    ## 1207      2434       1328     1337      13        96     448       658
    ## 1208      2434       1328     1337      13        96     448       658
    ## 1209      1624       2046     2092     116       440     744      3074
    ## 1210      1624       2046     2092     116       440     744      3074
    ## 1211      1624       2046     2092     116       440     744      3074
    ## 1212      1624       2046     2092     116       440     744      3074
    ## 1213      1624       2046     2092     116       440     744      3074
    ## 1214      1624       2046     2092     116       440     744      3074
    ## 1215      1624       2046     2092     116       440     744      3074
    ## 1216      1624       2046     2092     116       440     744      3074
    ## 1217      1624       2046     2092     116       440     744      3074
    ## 1218      1063       1116     1146     151        54     270       603
    ## 1219      1063       1116     1146     151        54     270       603
    ## 1220      1063       1116     1146     151        54     270       603
    ## 1221      1063       1116     1146     151        54     270       603
    ## 1222      1063       1116     1146     151        54     270       603
    ## 1223      1063       1116     1146     151        54     270       603
    ## 1224      1063       1116     1146     151        54     270       603
    ## 1225      1063       1116     1146     151        54     270       603
    ## 1226      1063       1116     1146     151        54     270       603
    ## 1227      1849       4024     4156    1109        90    1183      4464
    ## 1228      1849       4024     4156    1109        90    1183      4464
    ## 1229      1849       4024     4156    1109        90    1183      4464
    ## 1230      1849       4024     4156    1109        90    1183      4464
    ## 1231      1849       4024     4156    1109        90    1183      4464
    ## 1232      1849       4024     4156    1109        90    1183      4464
    ## 1233      1849       4024     4156    1109        90    1183      4464
    ## 1234      1849       4024     4156    1109        90    1183      4464
    ## 1235      1849       4024     4156    1109        90    1183      4464
    ## 1236      2047       1529     1482     146       131     333       456
    ## 1237      2047       1529     1482     146       131     333       456
    ## 1238      2047       1529     1482     146       131     333       456
    ## 1239      2047       1529     1482     146       131     333       456
    ## 1240      2047       1529     1482     146       131     333       456
    ## 1241      2047       1529     1482     146       131     333       456
    ## 1242      2047       1529     1482     146       131     333       456
    ## 1243      2047       1529     1482     146       131     333       456
    ## 1244      2047       1529     1482     146       131     333       456
    ## 1245      1832       1160     1221      83        52     276       338
    ## 1246      1832       1160     1221      83        52     276       338
    ## 1247      1832       1160     1221      83        52     276       338
    ## 1248      1832       1160     1221      83        52     276       338
    ## 1249      1832       1160     1221      83        52     276       338
    ## 1250      1832       1160     1221      83        52     276       338
    ## 1251      1832       1160     1221      83        52     276       338
    ## 1252      1832       1160     1221      83        52     276       338
    ## 1253      1832       1160     1221      83        52     276       338
    ## 1254       260        144      139       0         9      57       202
    ## 1255       260        144      139       0         9      57       202
    ## 1256       260        144      139       0         9      57       202
    ## 1257       260        144      139       0         9      57       202
    ## 1258       260        144      139       0         9      57       202
    ## 1259       951        496      491       0        11     130       562
    ## 1260       951        496      491       0        11     130       562
    ## 1261       951        496      491       0        11     130       562
    ## 1262       951        496      491       0        11     130       562
    ## 1263       951        496      491       0        11     130       562
    ## 1264       951        496      491       0        11     130       562
    ## 1265       951        496      491       0        11     130       562
    ## 1266       951        496      491       0        11     130       562
    ## 1267       951        496      491       0        11     130       562
    ## 1268      1088        554      589       1        29     121       552
    ## 1269      1088        554      589       1        29     121       552
    ## 1270      1088        554      589       1        29     121       552
    ## 1271      1088        554      589       1        29     121       552
    ## 1272      1088        554      589       1        29     121       552
    ## 1273      1088        554      589       1        29     121       552
    ## 1274      1088        554      589       1        29     121       552
    ## 1275      1088        554      589       1        29     121       552
    ## 1276      1088        554      589       1        29     121       552
    ## 1277      2778       1441     1586      10        85     404      1079
    ## 1278      2778       1441     1586      10        85     404      1079
    ## 1279      2778       1441     1586      10        85     404      1079
    ## 1280      2778       1441     1586      10        85     404      1079
    ## 1281      2778       1441     1586      10        85     404      1079
    ## 1282      2778       1441     1586      10        85     404      1079
    ## 1283      2778       1441     1586      10        85     404      1079
    ## 1284      2778       1441     1586      10        85     404      1079
    ## 1285      2778       1441     1586      10        85     404      1079
    ## 1286      5623       3142     3227      67       260     682      1508
    ## 1287      5623       3142     3227      67       260     682      1508
    ## 1288      5623       3142     3227      67       260     682      1508
    ## 1289      5623       3142     3227      67       260     682      1508
    ## 1290      5623       3142     3227      67       260     682      1508
    ## 1291      5623       3142     3227      67       260     682      1508
    ## 1292      5623       3142     3227      67       260     682      1508
    ## 1293      5623       3142     3227      67       260     682      1508
    ## 1294      5623       3142     3227      67       260     682      1508
    ## 1295      2946       2056     2230     118       206     459       675
    ## 1296      2946       2056     2230     118       206     459       675
    ## 1297      2946       2056     2230     118       206     459       675
    ## 1298      2946       2056     2230     118       206     459       675
    ## 1299      2946       2056     2230     118       206     459       675
    ## 1300      2946       2056     2230     118       206     459       675
    ## 1301      2946       2056     2230     118       206     459       675
    ## 1302      2946       2056     2230     118       206     459       675
    ## 1303      2946       2056     2230     118       206     459       675
    ## 1304      2130       1379     1420      34       185     382      1258
    ## 1305      2130       1379     1420      34       185     382      1258
    ## 1306      2130       1379     1420      34       185     382      1258
    ## 1307      2130       1379     1420      34       185     382      1258
    ## 1308      2130       1379     1420      34       185     382      1258
    ## 1309      2130       1379     1420      34       185     382      1258
    ## 1310      2130       1379     1420      34       185     382      1258
    ## 1311      2130       1379     1420      34       185     382      1258
    ## 1312      2130       1379     1420      34       185     382      1258
    ## 1313       746        370      427       0        30     108       394
    ## 1314       746        370      427       0        30     108       394
    ## 1315       746        370      427       0        30     108       394
    ## 1316       746        370      427       0        30     108       394
    ## 1317       746        370      427       0        30     108       394
    ## 1318       746        370      427       0        30     108       394
    ## 1319      1016        595      634      12        57     155       622
    ## 1320      1016        595      634      12        57     155       622
    ## 1321      1016        595      634      12        57     155       622
    ## 1322      1016        595      634      12        57     155       622
    ## 1323      1016        595      634      12        57     155       622
    ## 1324      1016        595      634      12        57     155       622
    ## 1325      1016        595      634      12        57     155       622
    ## 1326      1016        595      634      12        57     155       622
    ## 1327      1016        595      634      12        57     155       622
    ## 1328      1485        763      784       2         8     203       842
    ## 1329      1485        763      784       2         8     203       842
    ## 1330      1485        763      784       2         8     203       842
    ## 1331      1485        763      784       2         8     203       842
    ## 1332      1485        763      784       2         8     203       842
    ## 1333      1485        763      784       2         8     203       842
    ## 1334      1485        763      784       2         8     203       842
    ## 1335      1485        763      784       2         8     203       842
    ## 1336      1485        763      784       2         8     203       842
    ## 1337      1372        817      801      33        92     290       837
    ## 1338      1372        817      801      33        92     290       837
    ## 1339      1372        817      801      33        92     290       837
    ## 1340      1372        817      801      33        92     290       837
    ## 1341      1372        817      801      33        92     290       837
    ## 1342      1372        817      801      33        92     290       837
    ## 1343      1372        817      801      33        92     290       837
    ## 1344      1372        817      801      33        92     290       837
    ## 1345      1372        817      801      33        92     290       837
    ## 1346       444        223      253       9         2      55       223
    ## 1347       444        223      253       9         2      55       223
    ## 1348       444        223      253       9         2      55       223
    ## 1349       444        223      253       9         2      55       223
    ## 1350       444        223      253       9         2      55       223
    ## 1351       444        223      253       9         2      55       223
    ## 1352       358        197      191       0        16      67       182
    ## 1353       358        197      191       0        16      67       182
    ## 1354       358        197      191       0        16      67       182
    ## 1355       358        197      191       0        16      67       182
    ## 1356       358        197      191       0        16      67       182
    ## 1357       358        197      191       0        16      67       182
    ## 1358       358        197      191       0        16      67       182
    ## 1359       412        195      226       0         3      85       199
    ## 1360       412        195      226       0         3      85       199
    ## 1361       412        195      226       0         3      85       199
    ## 1362       412        195      226       0         3      85       199
    ## 1363       412        195      226       0         3      85       199
    ## 1364       412        195      226       0         3      85       199
    ## 1365       412        195      226       0         3      85       199
    ## 1366      1179        768      786      38        36     197       230
    ## 1367      1179        768      786      38        36     197       230
    ## 1368      1179        768      786      38        36     197       230
    ## 1369      1179        768      786      38        36     197       230
    ## 1370      1179        768      786      38        36     197       230
    ## 1371      1179        768      786      38        36     197       230
    ## 1372      1179        768      786      38        36     197       230
    ## 1373      1179        768      786      38        36     197       230
    ## 1374      1179        768      786      38        36     197       230
    ## 1375      1636       2208     2341     302       137     663      2182
    ## 1376      1636       2208     2341     302       137     663      2182
    ## 1377      1636       2208     2341     302       137     663      2182
    ## 1378      1636       2208     2341     302       137     663      2182
    ## 1379      1636       2208     2341     302       137     663      2182
    ## 1380      1636       2208     2341     302       137     663      2182
    ## 1381      1636       2208     2341     302       137     663      2182
    ## 1382      1636       2208     2341     302       137     663      2182
    ## 1383      1636       2208     2341     302       137     663      2182
    ## 1384       147       1459     1649     637        99     403      2277
    ## 1385       147       1459     1649     637        99     403      2277
    ## 1386       147       1459     1649     637        99     403      2277
    ## 1387       147       1459     1649     637        99     403      2277
    ## 1388       147       1459     1649     637        99     403      2277
    ## 1389       147       1459     1649     637        99     403      2277
    ## 1390       147       1459     1649     637        99     403      2277
    ## 1391       147       1459     1649     637        99     403      2277
    ## 1392       147       1459     1649     637        99     403      2277
    ## 1393      1454       1952     2063     262       126     609      1913
    ## 1394      1454       1952     2063     262       126     609      1913
    ## 1395      1454       1952     2063     262       126     609      1913
    ## 1396      1454       1952     2063     262       126     609      1913
    ## 1397      1454       1952     2063     262       126     609      1913
    ## 1398      1454       1952     2063     262       126     609      1913
    ## 1399      1454       1952     2063     262       126     609      1913
    ## 1400      1454       1952     2063     262       126     609      1913
    ## 1401      1454       1952     2063     262       126     609      1913
    ## 1402      2227       1911     1859     264       127     591      1336
    ## 1403      2227       1911     1859     264       127     591      1336
    ## 1404      2227       1911     1859     264       127     591      1336
    ## 1405      2227       1911     1859     264       127     591      1336
    ## 1406      2227       1911     1859     264       127     591      1336
    ## 1407      2227       1911     1859     264       127     591      1336
    ## 1408      2227       1911     1859     264       127     591      1336
    ## 1409      2227       1911     1859     264       127     591      1336
    ## 1410      2227       1911     1859     264       127     591      1336
    ## 1411      8826       6207     6395     343       332    2177      3445
    ## 1412      8826       6207     6395     343       332    2177      3445
    ## 1413      8826       6207     6395     343       332    2177      3445
    ## 1414      8826       6207     6395     343       332    2177      3445
    ## 1415      8826       6207     6395     343       332    2177      3445
    ## 1416      8826       6207     6395     343       332    2177      3445
    ## 1417      8826       6207     6395     343       332    2177      3445
    ## 1418      8826       6207     6395     343       332    2177      3445
    ## 1419      8826       6207     6395     343       332    2177      3445
    ## 1420       800        512      540      33        36     178        77
    ## 1421       800        512      540      33        36     178        77
    ## 1422       800        512      540      33        36     178        77
    ## 1423       800        512      540      33        36     178        77
    ## 1424       800        512      540      33        36     178        77
    ## 1425       800        512      540      33        36     178        77
    ## 1426       800        512      540      33        36     178        77
    ## 1427       800        512      540      33        36     178        77
    ## 1428       800        512      540      33        36     178        77
    ## 1429      1796       1039     1128      15        30     299       223
    ## 1430      1796       1039     1128      15        30     299       223
    ## 1431      1796       1039     1128      15        30     299       223
    ## 1432      1796       1039     1128      15        30     299       223
    ## 1433      1796       1039     1128      15        30     299       223
    ## 1434      1796       1039     1128      15        30     299       223
    ## 1435      1796       1039     1128      15        30     299       223
    ## 1436      1796       1039     1128      15        30     299       223
    ## 1437      1796       1039     1128      15        30     299       223
    ## 1438      5665       4645     4911     596       266    1458      3705
    ## 1439      5665       4645     4911     596       266    1458      3705
    ## 1440      5665       4645     4911     596       266    1458      3705
    ## 1441      5665       4645     4911     596       266    1458      3705
    ## 1442      5665       4645     4911     596       266    1458      3705
    ## 1443      5665       4645     4911     596       266    1458      3705
    ## 1444      5665       4645     4911     596       266    1458      3705
    ## 1445      5665       4645     4911     596       266    1458      3705
    ## 1446      5665       4645     4911     596       266    1458      3705
    ## 1447      3368       3672     3905    1003       249    1142      4133
    ## 1448      3368       3672     3905    1003       249    1142      4133
    ## 1449      3368       3672     3905    1003       249    1142      4133
    ## 1450      3368       3672     3905    1003       249    1142      4133
    ## 1451      3368       3672     3905    1003       249    1142      4133
    ## 1452      3368       3672     3905    1003       249    1142      4133
    ## 1453      3368       3672     3905    1003       249    1142      4133
    ## 1454      3368       3672     3905    1003       249    1142      4133
    ## 1455      3368       3672     3905    1003       249    1142      4133
    ## 1456      3702       4507     4710     611       475    1347      5593
    ## 1457      3702       4507     4710     611       475    1347      5593
    ## 1458      3702       4507     4710     611       475    1347      5593
    ## 1459      3702       4507     4710     611       475    1347      5593
    ## 1460      3702       4507     4710     611       475    1347      5593
    ## 1461      3702       4507     4710     611       475    1347      5593
    ## 1462      3702       4507     4710     611       475    1347      5593
    ## 1463      3702       4507     4710     611       475    1347      5593
    ## 1464      3702       4507     4710     611       475    1347      5593
    ## 1465      1149        750      814      87         1     298       363
    ## 1466      1149        750      814      87         1     298       363
    ## 1467      1149        750      814      87         1     298       363
    ## 1468      1149        750      814      87         1     298       363
    ## 1469      1149        750      814      87         1     298       363
    ## 1470      1149        750      814      87         1     298       363
    ## 1471      1149        750      814      87         1     298       363
    ## 1472      1149        750      814      87         1     298       363
    ## 1473      1149        750      814      87         1     298       363
    ## 1474      1557       2006     2185       0       155     599      2553
    ## 1475      1557       2006     2185       0       155     599      2553
    ## 1476      1557       2006     2185       0       155     599      2553
    ## 1477      1557       2006     2185       0       155     599      2553
    ## 1478      1557       2006     2185       0       155     599      2553
    ## 1479      1557       2006     2185       0       155     599      2553
    ## 1480      1557       2006     2185       0       155     599      2553
    ## 1481      1557       2006     2185       0       155     599      2553
    ## 1482      1557       2006     2185       0       155     599      2553
    ## 1483      4110       2423     2652     167       132     795       797
    ## 1484      4110       2423     2652     167       132     795       797
    ## 1485      4110       2423     2652     167       132     795       797
    ## 1486      4110       2423     2652     167       132     795       797
    ## 1487      4110       2423     2652     167       132     795       797
    ## 1488      4110       2423     2652     167       132     795       797
    ## 1489      4110       2423     2652     167       132     795       797
    ## 1490      4110       2423     2652     167       132     795       797
    ## 1491      4110       2423     2652     167       132     795       797
    ## 1492      4120       3810     4019     221       373    1464      1670
    ## 1493      4120       3810     4019     221       373    1464      1670
    ## 1494      4120       3810     4019     221       373    1464      1670
    ## 1495      4120       3810     4019     221       373    1464      1670
    ## 1496      4120       3810     4019     221       373    1464      1670
    ## 1497      4120       3810     4019     221       373    1464      1670
    ## 1498      4120       3810     4019     221       373    1464      1670
    ## 1499      4120       3810     4019     221       373    1464      1670
    ## 1500      4120       3810     4019     221       373    1464      1670
    ## 1501      2173       1488     1540     105       119     458       488
    ## 1502      2173       1488     1540     105       119     458       488
    ## 1503      2173       1488     1540     105       119     458       488
    ## 1504      2173       1488     1540     105       119     458       488
    ## 1505      2173       1488     1540     105       119     458       488
    ## 1506      2173       1488     1540     105       119     458       488
    ## 1507      2173       1488     1540     105       119     458       488
    ## 1508      2173       1488     1540     105       119     458       488
    ## 1509      2173       1488     1540     105       119     458       488
    ## 1510      1553       1379     1347     149        92     394       903
    ## 1511      1553       1379     1347     149        92     394       903
    ## 1512      1553       1379     1347     149        92     394       903
    ## 1513      1553       1379     1347     149        92     394       903
    ## 1514      1553       1379     1347     149        92     394       903
    ## 1515      1553       1379     1347     149        92     394       903
    ## 1516      1553       1379     1347     149        92     394       903
    ## 1517      1553       1379     1347     149        92     394       903
    ## 1518      1553       1379     1347     149        92     394       903
    ## 1519      2655       1733     1895      77        98     579       872
    ## 1520      2655       1733     1895      77        98     579       872
    ## 1521      2655       1733     1895      77        98     579       872
    ## 1522      2655       1733     1895      77        98     579       872
    ## 1523      2655       1733     1895      77        98     579       872
    ## 1524      2655       1733     1895      77        98     579       872
    ## 1525      2655       1733     1895      77        98     579       872
    ## 1526      2655       1733     1895      77        98     579       872
    ## 1527      2655       1733     1895      77        98     579       872
    ## 1528      2367       1552     1728     109        75     467       565
    ## 1529      2367       1552     1728     109        75     467       565
    ## 1530      2367       1552     1728     109        75     467       565
    ## 1531      2367       1552     1728     109        75     467       565
    ## 1532      2367       1552     1728     109        75     467       565
    ## 1533      2367       1552     1728     109        75     467       565
    ## 1534      2367       1552     1728     109        75     467       565
    ## 1535      2367       1552     1728     109        75     467       565
    ## 1536      2367       1552     1728     109        75     467       565
    ## 1537      3395       1951     2154      26       110     638       652
    ## 1538      3395       1951     2154      26       110     638       652
    ## 1539      3395       1951     2154      26       110     638       652
    ## 1540      3395       1951     2154      26       110     638       652
    ## 1541      3395       1951     2154      26       110     638       652
    ## 1542      3395       1951     2154      26       110     638       652
    ## 1543      3395       1951     2154      26       110     638       652
    ## 1544      3395       1951     2154      26       110     638       652
    ## 1545      3395       1951     2154      26       110     638       652
    ## 1546      1983       2928     3143       0       105     841         0
    ## 1547      1983       2928     3143       0       105     841         0
    ## 1548      1983       2928     3143       0       105     841         0
    ## 1549      1983       2928     3143       0       105     841         0
    ## 1550      1983       2928     3143       0       105     841         0
    ## 1551      1983       2928     3143       0       105     841         0
    ## 1552      1983       2928     3143       0       105     841         0
    ## 1553      1983       2928     3143       0       105     841         0
    ## 1554      1983       2928     3143       0       105     841         0
    ## 1555      6747       4174     4220     134       168    1281       843
    ## 1556      6747       4174     4220     134       168    1281       843
    ## 1557      6747       4174     4220     134       168    1281       843
    ## 1558      6747       4174     4220     134       168    1281       843
    ## 1559      6747       4174     4220     134       168    1281       843
    ## 1560      6747       4174     4220     134       168    1281       843
    ## 1561      6747       4174     4220     134       168    1281       843
    ## 1562      6747       4174     4220     134       168    1281       843
    ## 1563      6747       4174     4220     134       168    1281       843
    ## 1564        68        139      128       0        10      32         0
    ## 1565        68        139      128       0        10      32         0
    ## 1566        68        139      128       0        10      32         0
    ## 1567        68        139      128       0        10      32         0
    ## 1568        68        139      128       0        10      32         0
    ## 1569        68        139      128       0        10      32         0
    ## 1570       416        737      707     273        82     192       999
    ## 1571       416        737      707     273        82     192       999
    ## 1572       416        737      707     273        82     192       999
    ## 1573       416        737      707     273        82     192       999
    ## 1574       416        737      707     273        82     192       999
    ## 1575       416        737      707     273        82     192       999
    ## 1576       416        737      707     273        82     192       999
    ## 1577       416        737      707     273        82     192       999
    ## 1578       416        737      707     273        82     192       999
    ## 1579       413        249      233       1        12      44       222
    ## 1580       413        249      233       1        12      44       222
    ## 1581       413        249      233       1        12      44       222
    ## 1582       413        249      233       1        12      44       222
    ## 1583       413        249      233       1        12      44       222
    ## 1584       413        249      233       1        12      44       222
    ## 1585       413        249      233       1        12      44       222
    ## 1586       413        249      233       1        12      44       222
    ## 1587       413        249      233       1        12      44       222
    ## 1588       907        507      561      13        37     142       409
    ## 1589       907        507      561      13        37     142       409
    ## 1590       907        507      561      13        37     142       409
    ## 1591       907        507      561      13        37     142       409
    ## 1592       907        507      561      13        37     142       409
    ## 1593       907        507      561      13        37     142       409
    ## 1594       907        507      561      13        37     142       409
    ## 1595       907        507      561      13        37     142       409
    ## 1596       907        507      561      13        37     142       409
    ## 1597      1429        758      743       1        26     211       739
    ## 1598      1429        758      743       1        26     211       739
    ## 1599      1429        758      743       1        26     211       739
    ## 1600      1429        758      743       1        26     211       739
    ## 1601      1429        758      743       1        26     211       739
    ## 1602      1429        758      743       1        26     211       739
    ## 1603      1429        758      743       1        26     211       739
    ## 1604      1429        758      743       1        26     211       739
    ## 1605      1429        758      743       1        26     211       739
    ## 1606       703        376      380       1        23     112       444
    ## 1607       703        376      380       1        23     112       444
    ## 1608       703        376      380       1        23     112       444
    ## 1609       703        376      380       1        23     112       444
    ## 1610       703        376      380       1        23     112       444
    ## 1611       703        376      380       1        23     112       444
    ## 1612       857        445      456       1        11     138       524
    ## 1613       857        445      456       1        11     138       524
    ## 1614       857        445      456       1        11     138       524
    ## 1615       857        445      456       1        11     138       524
    ## 1616       857        445      456       1        11     138       524
    ## 1617       857        445      456       1        11     138       524
    ## 1618       857        445      456       1        11     138       524
    ## 1619       857        445      456       1        11     138       524
    ## 1620       857        445      456       1        11     138       524
    ## 1621       772        428      424       4        32     109       357
    ## 1622       772        428      424       4        32     109       357
    ## 1623       772        428      424       4        32     109       357
    ## 1624       772        428      424       4        32     109       357
    ## 1625       772        428      424       4        32     109       357
    ## 1626       772        428      424       4        32     109       357
    ## 1627       772        428      424       4        32     109       357
    ## 1628       772        428      424       4        32     109       357
    ## 1629       772        428      424       4        32     109       357
    ## 1630      3365       2574     2693     210       530     715      1877
    ## 1631      3365       2574     2693     210       530     715      1877
    ## 1632      3365       2574     2693     210       530     715      1877
    ## 1633      3365       2574     2693     210       530     715      1877
    ## 1634      3365       2574     2693     210       530     715      1877
    ## 1635      3365       2574     2693     210       530     715      1877
    ## 1636      3365       2574     2693     210       530     715      1877
    ## 1637      3365       2574     2693     210       530     715      1877
    ## 1638      3365       2574     2693     210       530     715      1877
    ## 1639       952        585      602      21       129     131       342
    ## 1640       952        585      602      21       129     131       342
    ## 1641       952        585      602      21       129     131       342
    ## 1642       952        585      602      21       129     131       342
    ## 1643       952        585      602      21       129     131       342
    ## 1644       952        585      602      21       129     131       342
    ## 1645       952        585      602      21       129     131       342
    ## 1646       952        585      602      21       129     131       342
    ## 1647       952        585      602      21       129     131       342
    ## 1648      3422       3282     3314     506       593    1326      3635
    ## 1649      3422       3282     3314     506       593    1326      3635
    ## 1650      3422       3282     3314     506       593    1326      3635
    ## 1651      3422       3282     3314     506       593    1326      3635
    ## 1652      3422       3282     3314     506       593    1326      3635
    ## 1653      3422       3282     3314     506       593    1326      3635
    ## 1654      3422       3282     3314     506       593    1326      3635
    ## 1655      3422       3282     3314     506       593    1326      3635
    ## 1656      3422       3282     3314     506       593    1326      3635
    ## 1657      1163        827      840      14        76     226       665
    ## 1658      1163        827      840      14        76     226       665
    ## 1659      1163        827      840      14        76     226       665
    ## 1660      1163        827      840      14        76     226       665
    ## 1661      1163        827      840      14        76     226       665
    ## 1662      1163        827      840      14        76     226       665
    ## 1663      1163        827      840      14        76     226       665
    ## 1664      1163        827      840      14        76     226       665
    ## 1665      1163        827      840      14        76     226       665
    ## 1666      1532       1001     1034      48        86     313       557
    ## 1667      1532       1001     1034      48        86     313       557
    ## 1668      1532       1001     1034      48        86     313       557
    ## 1669      1532       1001     1034      48        86     313       557
    ## 1670      1532       1001     1034      48        86     313       557
    ## 1671      1532       1001     1034      48        86     313       557
    ## 1672      1532       1001     1034      48        86     313       557
    ## 1673      1532       1001     1034      48        86     313       557
    ## 1674      1532       1001     1034      48        86     313       557
    ## 1675      1847       1349     1482      64        73     387       991
    ## 1676      1847       1349     1482      64        73     387       991
    ## 1677      1847       1349     1482      64        73     387       991
    ## 1678      1847       1349     1482      64        73     387       991
    ## 1679      1847       1349     1482      64        73     387       991
    ## 1680      1847       1349     1482      64        73     387       991
    ## 1681      1847       1349     1482      64        73     387       991
    ## 1682      1847       1349     1482      64        73     387       991
    ## 1683      1847       1349     1482      64        73     387       991
    ## 1684      1777       1003     1070       3       125     368       959
    ## 1685      1777       1003     1070       3       125     368       959
    ## 1686      1777       1003     1070       3       125     368       959
    ## 1687      1777       1003     1070       3       125     368       959
    ## 1688      1777       1003     1070       3       125     368       959
    ## 1689      1777       1003     1070       3       125     368       959
    ## 1690      1777       1003     1070       3       125     368       959
    ## 1691      1777       1003     1070       3       125     368       959
    ## 1692      1777       1003     1070       3       125     368       959
    ## 1693      2916       1516     1620      14        60     490      1152
    ## 1694      2916       1516     1620      14        60     490      1152
    ## 1695      2916       1516     1620      14        60     490      1152
    ## 1696      2916       1516     1620      14        60     490      1152
    ## 1697      2916       1516     1620      14        60     490      1152
    ## 1698      2916       1516     1620      14        60     490      1152
    ## 1699      2916       1516     1620      14        60     490      1152
    ## 1700      2916       1516     1620      14        60     490      1152
    ## 1701      2916       1516     1620      14        60     490      1152
    ## 1702      1014        502      570       0        28     187       483
    ## 1703      1014        502      570       0        28     187       483
    ## 1704      1014        502      570       0        28     187       483
    ## 1705      1014        502      570       0        28     187       483
    ## 1706      1014        502      570       0        28     187       483
    ## 1707      1014        502      570       0        28     187       483
    ## 1708      1014        502      570       0        28     187       483
    ## 1709      1014        502      570       0        28     187       483
    ## 1710      1014        502      570       0        28     187       483
    ## 1711      2146       1181     1202       6       119     488       999
    ## 1712      2146       1181     1202       6       119     488       999
    ## 1713      2146       1181     1202       6       119     488       999
    ## 1714      2146       1181     1202       6       119     488       999
    ## 1715      2146       1181     1202       6       119     488       999
    ## 1716      2146       1181     1202       6       119     488       999
    ## 1717      2146       1181     1202       6       119     488       999
    ## 1718      2146       1181     1202       6       119     488       999
    ## 1719      2146       1181     1202       6       119     488       999
    ## 1720      1726        899      969      11        51     231       819
    ## 1721      1726        899      969      11        51     231       819
    ## 1722      1726        899      969      11        51     231       819
    ## 1723      1726        899      969      11        51     231       819
    ## 1724      1726        899      969      11        51     231       819
    ## 1725      1726        899      969      11        51     231       819
    ## 1726      1726        899      969      11        51     231       819
    ## 1727      1726        899      969      11        51     231       819
    ## 1728       784        378      446       5         7     150       423
    ## 1729       784        378      446       5         7     150       423
    ## 1730       784        378      446       5         7     150       423
    ## 1731       784        378      446       5         7     150       423
    ## 1732       784        378      446       5         7     150       423
    ## 1733       784        378      446       5         7     150       423
    ## 1734       784        378      446       5         7     150       423
    ## 1735       784        378      446       5         7     150       423
    ## 1736       784        378      446       5         7     150       423
    ## 1737      1130        764      820      28        48     217       190
    ## 1738      1130        764      820      28        48     217       190
    ## 1739      1130        764      820      28        48     217       190
    ## 1740      1130        764      820      28        48     217       190
    ## 1741      1130        764      820      28        48     217       190
    ## 1742      1130        764      820      28        48     217       190
    ## 1743      1130        764      820      28        48     217       190
    ## 1744      1130        764      820      28        48     217       190
    ## 1745      1130        764      820      28        48     217       190
    ## 1746      1266       1122     1146      67       109     335       688
    ## 1747      1266       1122     1146      67       109     335       688
    ## 1748      1266       1122     1146      67       109     335       688
    ## 1749      1266       1122     1146      67       109     335       688
    ## 1750      1266       1122     1146      67       109     335       688
    ## 1751      1266       1122     1146      67       109     335       688
    ## 1752      1266       1122     1146      67       109     335       688
    ## 1753      1266       1122     1146      67       109     335       688
    ## 1754      1266       1122     1146      67       109     335       688
    ## 1755      1215        841      940      23       107     186       160
    ## 1756      1215        841      940      23       107     186       160
    ## 1757      1215        841      940      23       107     186       160
    ## 1758      1215        841      940      23       107     186       160
    ## 1759      1215        841      940      23       107     186       160
    ## 1760      1215        841      940      23       107     186       160
    ## 1761      1215        841      940      23       107     186       160
    ## 1762      1215        841      940      23       107     186       160
    ## 1763      1215        841      940      23       107     186       160
    ## 1764      1336       1068     1258      44       116     273       219
    ## 1765      1336       1068     1258      44       116     273       219
    ## 1766      1336       1068     1258      44       116     273       219
    ## 1767      1336       1068     1258      44       116     273       219
    ## 1768      1336       1068     1258      44       116     273       219
    ## 1769      1336       1068     1258      44       116     273       219
    ## 1770      1336       1068     1258      44       116     273       219
    ## 1771      1336       1068     1258      44       116     273       219
    ## 1772      1336       1068     1258      44       116     273       219
    ## 1773       212        838      880      90        28     281       962
    ## 1774       212        838      880      90        28     281       962
    ## 1775       212        838      880      90        28     281       962
    ## 1776       212        838      880      90        28     281       962
    ## 1777       212        838      880      90        28     281       962
    ## 1778       212        838      880      90        28     281       962
    ## 1779       212        838      880      90        28     281       962
    ## 1780       212        838      880      90        28     281       962
    ## 1781       212        838      880      90        28     281       962
    ## 1782      2348       1821     1801     315        88     465       698
    ## 1783      2348       1821     1801     315        88     465       698
    ## 1784      2348       1821     1801     315        88     465       698
    ## 1785      2348       1821     1801     315        88     465       698
    ## 1786      2348       1821     1801     315        88     465       698
    ## 1787      2348       1821     1801     315        88     465       698
    ## 1788      2348       1821     1801     315        88     465       698
    ## 1789      2348       1821     1801     315        88     465       698
    ## 1790      2348       1821     1801     315        88     465       698
    ## 1791      1508        907      996      48        29     309       195
    ## 1792      1508        907      996      48        29     309       195
    ## 1793      1508        907      996      48        29     309       195
    ## 1794      1508        907      996      48        29     309       195
    ## 1795      1508        907      996      48        29     309       195
    ## 1796      1508        907      996      48        29     309       195
    ## 1797      1508        907      996      48        29     309       195
    ## 1798      1508        907      996      48        29     309       195
    ## 1799      1508        907      996      48        29     309       195
    ## 1800       749        681      706      55        62     168       235
    ## 1801       749        681      706      55        62     168       235
    ## 1802       749        681      706      55        62     168       235
    ## 1803       749        681      706      55        62     168       235
    ## 1804       749        681      706      55        62     168       235
    ## 1805       749        681      706      55        62     168       235
    ## 1806       749        681      706      55        62     168       235
    ## 1807       749        681      706      55        62     168       235
    ## 1808       749        681      706      55        62     168       235
    ## 1809      1283        818      877      41        65     315       223
    ## 1810      1283        818      877      41        65     315       223
    ## 1811      1283        818      877      41        65     315       223
    ## 1812      1283        818      877      41        65     315       223
    ## 1813      1283        818      877      41        65     315       223
    ## 1814      1283        818      877      41        65     315       223
    ## 1815      1283        818      877      41        65     315       223
    ## 1816      1283        818      877      41        65     315       223
    ## 1817      1283        818      877      41        65     315       223
    ## 1818       364       3866     4015     577        94    1264      5474
    ## 1819       364       3866     4015     577        94    1264      5474
    ## 1820       364       3866     4015     577        94    1264      5474
    ## 1821       364       3866     4015     577        94    1264      5474
    ## 1822       364       3866     4015     577        94    1264      5474
    ## 1823       364       3866     4015     577        94    1264      5474
    ## 1824       364       3866     4015     577        94    1264      5474
    ## 1825       364       3866     4015     577        94    1264      5474
    ## 1826       364       3866     4015     577        94    1264      5474
    ## 1827      2697       5136     5630    1279       140    1529      5870
    ## 1828      2697       5136     5630    1279       140    1529      5870
    ## 1829      2697       5136     5630    1279       140    1529      5870
    ## 1830      2697       5136     5630    1279       140    1529      5870
    ## 1831      2697       5136     5630    1279       140    1529      5870
    ## 1832      2697       5136     5630    1279       140    1529      5870
    ## 1833      2697       5136     5630    1279       140    1529      5870
    ## 1834      2697       5136     5630    1279       140    1529      5870
    ## 1835      2697       5136     5630    1279       140    1529      5870
    ## 1836       775        523      523      28        37     161       195
    ## 1837       775        523      523      28        37     161       195
    ## 1838       775        523      523      28        37     161       195
    ## 1839       775        523      523      28        37     161       195
    ## 1840       775        523      523      28        37     161       195
    ## 1841       775        523      523      28        37     161       195
    ## 1842       775        523      523      28        37     161       195
    ## 1843       775        523      523      28        37     161       195
    ## 1844       775        523      523      28        37     161       195
    ## 1845      1902       1398     1515      22       215     291       314
    ## 1846      1902       1398     1515      22       215     291       314
    ## 1847      1902       1398     1515      22       215     291       314
    ## 1848      1902       1398     1515      22       215     291       314
    ## 1849      1902       1398     1515      22       215     291       314
    ## 1850      1902       1398     1515      22       215     291       314
    ## 1851      1902       1398     1515      22       215     291       314
    ## 1852      1902       1398     1515      22       215     291       314
    ## 1853      1902       1398     1515      22       215     291       314
    ## 1854       983        763      809      80        91     151       219
    ## 1855       983        763      809      80        91     151       219
    ## 1856       983        763      809      80        91     151       219
    ## 1857       983        763      809      80        91     151       219
    ## 1858       983        763      809      80        91     151       219
    ## 1859       983        763      809      80        91     151       219
    ## 1860       983        763      809      80        91     151       219
    ## 1861       983        763      809      80        91     151       219
    ## 1862       983        763      809      80        91     151       219
    ## 1863      3022       2297     2448      86       310     455         0
    ## 1864      3022       2297     2448      86       310     455         0
    ## 1865      3022       2297     2448      86       310     455         0
    ## 1866      3022       2297     2448      86       310     455         0
    ## 1867      3022       2297     2448      86       310     455         0
    ## 1868      3022       2297     2448      86       310     455         0
    ## 1869      3022       2297     2448      86       310     455         0
    ## 1870      3022       2297     2448      86       310     455         0
    ## 1871      3022       2297     2448      86       310     455         0
    ## 1872      2384       1400     1466      35        55     481       305
    ## 1873      2384       1400     1466      35        55     481       305
    ## 1874      2384       1400     1466      35        55     481       305
    ## 1875      2384       1400     1466      35        55     481       305
    ## 1876      2384       1400     1466      35        55     481       305
    ## 1877      2384       1400     1466      35        55     481       305
    ## 1878      2384       1400     1466      35        55     481       305
    ## 1879      2384       1400     1466      35        55     481       305
    ## 1880      2384       1400     1466      35        55     481       305
    ## 1881       899        455      481       3         3     112       422
    ## 1882       899        455      481       3         3     112       422
    ## 1883       899        455      481       3         3     112       422
    ## 1884       899        455      481       3         3     112       422
    ## 1885       899        455      481       3         3     112       422
    ## 1886       899        455      481       3         3     112       422
    ## 1887       899        455      481       3         3     112       422
    ## 1888       899        455      481       3         3     112       422
    ## 1889       899        455      481       3         3     112       422
    ## 1890       799        457      415       8        26      87       415
    ## 1891       799        457      415       8        26      87       415
    ## 1892       799        457      415       8        26      87       415
    ## 1893       799        457      415       8        26      87       415
    ## 1894       799        457      415       8        26      87       415
    ## 1895       799        457      415       8        26      87       415
    ## 1896       799        457      415       8        26      87       415
    ## 1897       799        457      415       8        26      87       415
    ## 1898       799        457      415       8        26      87       415
    ## 1899       621        317      359       0        23     140       384
    ## 1900       621        317      359       0        23     140       384
    ## 1901       621        317      359       0        23     140       384
    ## 1902       621        317      359       0        23     140       384
    ## 1903       621        317      359       0        23     140       384
    ## 1904       621        317      359       0        23     140       384
    ## 1905       621        317      359       0        23     140       384
    ## 1906       621        317      359       0        23     140       384
    ## 1907       621        317      359       0        23     140       384

See the unique grade_level:

``` r
unique(result$grade_level)
```

    ## [1] "DISTRICT TOTAL" "ELEMENTARY"     "MIDDLE/HIGH"

``` r
#write.csv(result, file = "result.csv", row.names = FALSE)
```
