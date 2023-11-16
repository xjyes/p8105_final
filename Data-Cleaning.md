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
show_student_weight = 
  Student_Weight |>
  head(5) |>
  knitr::kable(digits = 3)
```

``` r
show_student_weight
```

| district | county | year_reported | number_overweight | percent_overweight | number_obese | percent_obese | number_overweight_or_obese | percent_overweight_or_obese | grade_level | number_healthy_weight | percent_healthy_weight | sex    |
|---------:|:-------|--------------:|------------------:|-------------------:|-------------:|--------------:|---------------------------:|----------------------------:|:------------|----------------------:|-----------------------:|:-------|
|    10402 | ALBANY |          2019 |                28 |               11.4 |           28 |          11.4 |                         56 |                        22.8 | ELEMENTARY  |                   181 |                  0.736 | FEMALE |
|    10402 | ALBANY |          2019 |                35 |               13.9 |           42 |          16.7 |                         77 |                        30.6 | ELEMENTARY  |                   159 |                  0.631 | MALE   |
|    10402 | ALBANY |          2019 |                21 |               23.6 |           17 |          19.1 |                         38 |                        42.7 | MIDDLE/HIGH |                    51 |                  0.573 | FEMALE |
|    10402 | ALBANY |          2019 |                 9 |                9.9 |           28 |          30.8 |                         37 |                        40.7 | MIDDLE/HIGH |                    54 |                  0.593 | MALE   |
|    10500 | ALBANY |          2019 |                36 |               20.0 |           38 |          21.1 |                         74 |                        41.1 | ELEMENTARY  |                    96 |                  0.533 | FEMALE |

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
  drop_na()

result
```

    ##     district       county year_reported number_overweight percent_overweight
    ## 1      10402       ALBANY          2019                28               11.4
    ## 2      10402       ALBANY          2019                35               13.9
    ## 3      10402       ALBANY          2019                21               23.6
    ## 4      10402       ALBANY          2019                 9                9.9
    ## 5      10500       ALBANY          2019                16               20.0
    ## 6      10500       ALBANY          2019                36               20.0
    ## 7      10500       ALBANY          2019                39               17.7
    ## 8      10500       ALBANY          2019                18               16.7
    ## 9      10601       ALBANY          2019                70               13.8
    ## 10     10601       ALBANY          2019                81               16.5
    ## 11     10601       ALBANY          2019                60               19.1
    ## 12     10601       ALBANY          2019                46               15.7
    ## 13     10623       ALBANY          2019                89               16.7
    ## 14     10623       ALBANY          2019                76               14.9
    ## 15     10623       ALBANY          2019                90               21.0
    ## 16     10623       ALBANY          2019                79               16.7
    ## 17     10802       ALBANY          2019                85               17.4
    ## 18     10802       ALBANY          2019                74               14.7
    ## 19     10802       ALBANY          2019                69               21.7
    ## 20     10802       ALBANY          2019                56               17.3
    ## 21     22601     ALLEGANY          2019                19               15.4
    ## 22     22601     ALLEGANY          2019                21               16.7
    ## 23     22601     ALLEGANY          2019                22               25.3
    ## 24     22601     ALLEGANY          2019                14               16.7
    ## 25     30101       BROOME          2019                25               21.9
    ## 26     30101       BROOME          2019                23               15.8
    ## 27     30101       BROOME          2019                25               23.8
    ## 28     30101       BROOME          2019                13               11.0
    ## 29     30200       BROOME          2019                64               14.0
    ## 30     30200       BROOME          2019                80               17.1
    ## 31     30200       BROOME          2019                29               15.5
    ## 32     30200       BROOME          2019                26               13.3
    ## 33     30501       BROOME          2019                 9               12.2
    ## 34     30501       BROOME          2019                15               14.9
    ## 35     30501       BROOME          2019                 6               12.5
    ## 36     30501       BROOME          2019                 9               17.3
    ## 37     30601       BROOME          2019                31               19.3
    ## 38     30601       BROOME          2019                29               18.8
    ## 39     30601       BROOME          2019                10               16.1
    ## 40     30601       BROOME          2019                20               18.0
    ## 41     30701       BROOME          2019                39               14.7
    ## 42     30701       BROOME          2019                44               15.5
    ## 43     30701       BROOME          2019                21               19.1
    ## 44     30701       BROOME          2019                24               19.8
    ## 45     40302  CATTARAUGUS          2019                15               14.2
    ## 46     40302  CATTARAUGUS          2019                20               12.7
    ## 47     40302  CATTARAUGUS          2019                17               18.9
    ## 48     40302  CATTARAUGUS          2019                31               21.1
    ## 49     40901  CATTARAUGUS          2019                12               16.0
    ## 50     40901  CATTARAUGUS          2019                11               11.8
    ## 51     40901  CATTARAUGUS          2019                13               29.5
    ## 52     40901  CATTARAUGUS          2019                12               27.9
    ## 53     41401  CATTARAUGUS          2019                14               28.6
    ## 54     41401  CATTARAUGUS          2019                 8               20.0
    ## 55     42400  CATTARAUGUS          2019                40               16.5
    ## 56     42400  CATTARAUGUS          2019                45               16.1
    ## 57     42400  CATTARAUGUS          2019                26               20.0
    ## 58     42400  CATTARAUGUS          2019                23               16.0
    ## 59     43001  CATTARAUGUS          2019                12               13.6
    ## 60     43001  CATTARAUGUS          2019                18               22.0
    ## 61     43001  CATTARAUGUS          2019                12               16.2
    ## 62     43001  CATTARAUGUS          2019                11               16.2
    ## 63     43200  CATTARAUGUS          2019                24               23.3
    ## 64     43200  CATTARAUGUS          2019                29               22.8
    ## 65     43200  CATTARAUGUS          2019                10               22.7
    ## 66     43200  CATTARAUGUS          2019                12               16.2
    ## 67     50401       CAYUGA          2019                11               17.5
    ## 68     50401       CAYUGA          2019                 7                9.3
    ## 69     50401       CAYUGA          2019                 7                8.2
    ## 70     50401       CAYUGA          2019                11               16.2
    ## 71     51301       CAYUGA          2019                21               17.9
    ## 72     51301       CAYUGA          2019                20               18.2
    ## 73     51301       CAYUGA          2019                13               21.3
    ## 74     51301       CAYUGA          2019                10               17.9
    ## 75     60301   CHAUTAUQUA          2019                22               28.9
    ## 76     60301   CHAUTAUQUA          2019                12               17.6
    ## 77     60301   CHAUTAUQUA          2019                12               26.7
    ## 78     60301   CHAUTAUQUA          2019                 7               12.3
    ## 79     60401   CHAUTAUQUA          2019                11               15.1
    ## 80     60401   CHAUTAUQUA          2019                16               23.9
    ## 81     60401   CHAUTAUQUA          2019                26               28.0
    ## 82     60401   CHAUTAUQUA          2019                23               21.7
    ## 83     60800   CHAUTAUQUA          2019                26               14.9
    ## 84     60800   CHAUTAUQUA          2019                20               10.9
    ## 85     60800   CHAUTAUQUA          2019                21               16.4
    ## 86     60800   CHAUTAUQUA          2019                29               22.5
    ## 87     61501   CHAUTAUQUA          2019                20               16.7
    ## 88     61501   CHAUTAUQUA          2019                13               12.7
    ## 89     61501   CHAUTAUQUA          2019                18               18.4
    ## 90     61501   CHAUTAUQUA          2019                13               16.9
    ## 91     62301   CHAUTAUQUA          2019                18               26.9
    ## 92     62301   CHAUTAUQUA          2019                14               20.6
    ## 93     62301   CHAUTAUQUA          2019                 6               19.4
    ## 94     62601   CHAUTAUQUA          2019                 5               12.8
    ## 95     62601   CHAUTAUQUA          2019                 7               15.2
    ## 96     62601   CHAUTAUQUA          2019                 7               24.1
    ## 97     62901   CHAUTAUQUA          2019                 7               15.6
    ## 98     62901   CHAUTAUQUA          2019                14               16.3
    ## 99     62901   CHAUTAUQUA          2019                16               16.8
    ## 100    62901   CHAUTAUQUA          2019                 6               15.0
    ## 101    70600      CHEMUNG          2019               140               21.9
    ## 102    70600      CHEMUNG          2019               143               20.6
    ## 103    70600      CHEMUNG          2019                98               22.7
    ## 104    70600      CHEMUNG          2019                91               20.4
    ## 105    80601     CHENANGO          2019                20               19.0
    ## 106    80601     CHENANGO          2019                11               11.2
    ## 107    80601     CHENANGO          2019                12               16.4
    ## 108    80601     CHENANGO          2019                13               22.0
    ## 109    81003     CHENANGO          2019                 8               14.8
    ## 110    81003     CHENANGO          2019                11               21.6
    ## 111    81003     CHENANGO          2019                17               16.3
    ## 112    81003     CHENANGO          2019                10               12.7
    ## 113    81200     CHENANGO          2019                32               16.0
    ## 114    81200     CHENANGO          2019                34               17.1
    ## 115    81200     CHENANGO          2019                31               23.1
    ## 116    81200     CHENANGO          2019                22               17.2
    ## 117    90201      CLINTON          2019                23               17.6
    ## 118    90201      CLINTON          2019                20               15.5
    ## 119    90201      CLINTON          2019                13               18.3
    ## 120    90201      CLINTON          2019                 9               11.5
    ## 121    90301      CLINTON          2019                37               15.0
    ## 122    90301      CLINTON          2019                36               16.2
    ## 123    90301      CLINTON          2019                24               18.8
    ## 124    90301      CLINTON          2019                19               15.6
    ## 125    91101      CLINTON          2019                36               16.4
    ## 126    91101      CLINTON          2019                27               13.2
    ## 127    91101      CLINTON          2019                34               23.4
    ## 128    91101      CLINTON          2019                17               11.3
    ## 129   100501     COLUMBIA          2019                20               23.3
    ## 130   100501     COLUMBIA          2019                20               17.1
    ## 131   100501     COLUMBIA          2019                19               13.7
    ## 132   100501     COLUMBIA          2019                17               21.3
    ## 133   101300     COLUMBIA          2019                40               17.7
    ## 134   101300     COLUMBIA          2019                31               13.5
    ## 135   101300     COLUMBIA          2019                16               13.4
    ## 136   101300     COLUMBIA          2019                10               12.7
    ## 137   131601     DUTCHESS          2019                86               13.5
    ## 138   131601     DUTCHESS          2019                91               15.1
    ## 139   131601     DUTCHESS          2019                98               17.1
    ## 140   131601     DUTCHESS          2019                92               16.7
    ## 141   131602     DUTCHESS          2019                19               15.3
    ## 142   131602     DUTCHESS          2019                27               17.3
    ## 143   131602     DUTCHESS          2019                24               25.0
    ## 144   131602     DUTCHESS          2019                12               11.3
    ## 145   131701     DUTCHESS          2019                20               14.4
    ## 146   131701     DUTCHESS          2019                19               12.2
    ## 147   131701     DUTCHESS          2019                22               20.8
    ## 148   131701     DUTCHESS          2019                21               16.5
    ## 149   131801     DUTCHESS          2019                14               21.9
    ## 150   131801     DUTCHESS          2019                12               18.2
    ## 151   131801     DUTCHESS          2019                 8               11.9
    ## 152   131801     DUTCHESS          2019                 8               10.3
    ## 153   140101         ERIE          2019                33               14.0
    ## 154   140101         ERIE          2019                26               13.1
    ## 155   140101         ERIE          2019                17               13.0
    ## 156   140101         ERIE          2019                20               19.2
    ## 157   140203         ERIE          2019               133               16.6
    ## 158   140203         ERIE          2019               139               14.6
    ## 159   140203         ERIE          2019               132               13.5
    ## 160   140203         ERIE          2019               135               16.5
    ## 161   140207         ERIE          2019                46               13.0
    ## 162   140207         ERIE          2019                66               15.9
    ## 163   140207         ERIE          2019                28               12.6
    ## 164   140207         ERIE          2019                38               14.6
    ## 165   140701         ERIE          2019                42               16.3
    ## 166   140701         ERIE          2019                48               19.4
    ## 167   140701         ERIE          2019                29               18.8
    ## 168   140701         ERIE          2019                21               12.1
    ## 169   141101         ERIE          2019                21               16.3
    ## 170   141101         ERIE          2019                32               16.5
    ## 171   141101         ERIE          2019                34               17.3
    ## 172   141101         ERIE          2019                18               14.2
    ## 173   141401         ERIE          2019                21               14.1
    ## 174   141401         ERIE          2019                27               18.1
    ## 175   141401         ERIE          2019                18               16.8
    ## 176   141401         ERIE          2019                20               19.2
    ## 177   141501         ERIE          2019                37               13.5
    ## 178   141501         ERIE          2019                44               15.2
    ## 179   141501         ERIE          2019                40               23.5
    ## 180   141501         ERIE          2019                27               13.5
    ## 181   141601         ERIE          2019                46               19.0
    ## 182   141601         ERIE          2019                40               14.5
    ## 183   141601         ERIE          2019                21               13.5
    ## 184   141601         ERIE          2019                15               11.3
    ## 185   141604         ERIE          2019                56               17.7
    ## 186   141604         ERIE          2019                75               13.4
    ## 187   141604         ERIE          2019                74               13.8
    ## 188   141604         ERIE          2019                53               17.7
    ## 189   141800         ERIE          2019                44               20.3
    ## 190   141800         ERIE          2019                37               11.9
    ## 191   141800         ERIE          2019                19               20.7
    ## 192   141800         ERIE          2019                20               19.6
    ## 193   141901         ERIE          2019                86               16.8
    ## 194   141901         ERIE          2019                96               17.0
    ## 195   141901         ERIE          2019                76               19.6
    ## 196   141901         ERIE          2019                52               12.1
    ## 197   142101         ERIE          2019                12               10.4
    ## 198   142101         ERIE          2019                34               26.0
    ## 199   142101         ERIE          2019                47               32.2
    ## 200   142101         ERIE          2019                21               19.8
    ## 201   142301         ERIE          2019                72               16.3
    ## 202   142301         ERIE          2019                42                8.7
    ## 203   142301         ERIE          2019                38               11.8
    ## 204   142301         ERIE          2019                45               15.2
    ## 205   142601         ERIE          2019               156               16.5
    ## 206   142601         ERIE          2019               135               15.1
    ## 207   142601         ERIE          2019                93               22.6
    ## 208   142601         ERIE          2019                63               16.4
    ## 209   160101     FRANKLIN          2019                24               24.7
    ## 210   160101     FRANKLIN          2019                21               19.4
    ## 211   160101     FRANKLIN          2019                 7               13.7
    ## 212   160101     FRANKLIN          2019                 9               18.0
    ## 213   161201     FRANKLIN          2019                30               16.0
    ## 214   161201     FRANKLIN          2019                36               17.8
    ## 215   161201     FRANKLIN          2019                20               22.7
    ## 216   161201     FRANKLIN          2019                18               18.0
    ## 217   170600       FULTON          2019                24               22.9
    ## 218   170600       FULTON          2019                18               14.2
    ## 219   170600       FULTON          2019                17               26.6
    ## 220   170600       FULTON          2019                 7               11.5
    ## 221   180300      GENESEE          2019                36               14.3
    ## 222   180300      GENESEE          2019                39               14.5
    ## 223   180300      GENESEE          2019                37               26.2
    ## 224   180300      GENESEE          2019                32               17.5
    ## 225   190301       GREENE          2019                25               23.1
    ## 226   190301       GREENE          2019                25               19.1
    ## 227   190301       GREENE          2019                11               16.7
    ## 228   190301       GREENE          2019                11               17.7
    ## 229   190501       GREENE          2019                24               18.6
    ## 230   190501       GREENE          2019                36               25.0
    ## 231   190501       GREENE          2019                20               20.8
    ## 232   190501       GREENE          2019                19               20.0
    ## 233   220101    JEFFERSON          2019                34               15.6
    ## 234   220101    JEFFERSON          2019                44               21.9
    ## 235   220101    JEFFERSON          2019                20               18.7
    ## 236   220101    JEFFERSON          2019                18               13.8
    ## 237   220301    JEFFERSON          2019                73               14.2
    ## 238   220301    JEFFERSON          2019                84               14.8
    ## 239   220301    JEFFERSON          2019                51               22.0
    ## 240   220301    JEFFERSON          2019                42               18.4
    ## 241   220701    JEFFERSON          2019                12               16.2
    ## 242   220701    JEFFERSON          2019                 9               11.5
    ## 243   220701    JEFFERSON          2019                17               23.6
    ## 244   220701    JEFFERSON          2019                 9               18.0
    ## 245   222201    JEFFERSON          2019                32               11.6
    ## 246   222201    JEFFERSON          2019                47               13.7
    ## 247   222201    JEFFERSON          2019                36               21.7
    ## 248   222201    JEFFERSON          2019                45               18.8
    ## 249   240401   LIVINGSTON          2019                 9               14.5
    ## 250   240401   LIVINGSTON          2019                 9               10.5
    ## 251   240401   LIVINGSTON          2019                13               22.0
    ## 252   240401   LIVINGSTON          2019                10               15.9
    ## 253   250901      MADISON          2019                28               19.0
    ## 254   250901      MADISON          2019                28               19.9
    ## 255   250901      MADISON          2019                21               28.8
    ## 256   250901      MADISON          2019                11               16.2
    ## 257   251601      MADISON          2019                21               13.8
    ## 258   251601      MADISON          2019                21               11.2
    ## 259   251601      MADISON          2019                25               19.1
    ## 260   251601      MADISON          2019                31               20.3
    ## 261   260101       MONROE          2019                43               13.2
    ## 262   260101       MONROE          2019                54               14.9
    ## 263   260101       MONROE          2019                50               20.5
    ## 264   260101       MONROE          2019                26               12.7
    ## 265   260801       MONROE          2019                72               17.9
    ## 266   260801       MONROE          2019                69               16.4
    ## 267   260801       MONROE          2019                48               20.3
    ## 268   260801       MONROE          2019                44               17.5
    ## 269   260803       MONROE          2019                66               18.7
    ## 270   260803       MONROE          2019                77               18.1
    ## 271   260803       MONROE          2019                45               17.2
    ## 272   260803       MONROE          2019                49               19.5
    ## 273   261101       MONROE          2019                85               19.9
    ## 274   261101       MONROE          2019                78               16.6
    ## 275   261101       MONROE          2019                50               16.1
    ## 276   261101       MONROE          2019                54               17.4
    ## 277   261313       MONROE          2019                19               15.7
    ## 278   261313       MONROE          2019                12                8.9
    ## 279   261313       MONROE          2019                15               20.8
    ## 280   261313       MONROE          2019                10               13.7
    ## 281   261501       MONROE          2019                64               15.9
    ## 282   261501       MONROE          2019               103               22.5
    ## 283   261501       MONROE          2019                64               21.7
    ## 284   261501       MONROE          2019                53               16.5
    ## 285   261701       MONROE          2019                82               14.7
    ## 286   261701       MONROE          2019                81               13.7
    ## 287   261701       MONROE          2019                49               14.6
    ## 288   261701       MONROE          2019                73               20.6
    ## 289   261801       MONROE          2019                46               15.1
    ## 290   261801       MONROE          2019                45               14.0
    ## 291   261801       MONROE          2019                34               18.3
    ## 292   261801       MONROE          2019                38               20.1
    ## 293   261901       MONROE          2019               143               15.1
    ## 294   261901       MONROE          2019               118               12.5
    ## 295   261901       MONROE          2019               124               18.5
    ## 296   261901       MONROE          2019               114               16.7
    ## 297   270100   MONTGOMERY          2019                77               19.6
    ## 298   270100   MONTGOMERY          2019                69               17.6
    ## 299   270100   MONTGOMERY          2019                23               16.7
    ## 300   270100   MONTGOMERY          2019                27               14.9
    ## 301   280202       NASSAU          2019               177               20.6
    ## 302   280202       NASSAU          2019               137               17.2
    ## 303   280202       NASSAU          2019               113               24.5
    ## 304   280202       NASSAU          2019               112               21.2
    ## 305   280203       NASSAU          2019                81               13.9
    ## 306   280203       NASSAU          2019                90               16.3
    ## 307   280203       NASSAU          2019               107               21.7
    ## 308   280203       NASSAU          2019                94               19.2
    ## 309   280205       NASSAU          2019                96               13.3
    ## 310   280205       NASSAU          2019                88               12.6
    ## 311   280205       NASSAU          2019                81               19.0
    ## 312   280205       NASSAU          2019                74               16.1
    ## 313   280206       NASSAU          2019                27               16.9
    ## 314   280206       NASSAU          2019                33               18.3
    ## 315   280206       NASSAU          2019                24               16.2
    ## 316   280206       NASSAU          2019                27               14.2
    ## 317   280209       NASSAU          2019               171               22.3
    ## 318   280209       NASSAU          2019               181               19.8
    ## 319   280209       NASSAU          2019                98               25.7
    ## 320   280209       NASSAU          2019                77               19.3
    ## 321   280213       NASSAU          2019                26               11.5
    ## 322   280213       NASSAU          2019                24                9.5
    ## 323   280215       NASSAU          2019                52               17.2
    ## 324   280215       NASSAU          2019                48               16.6
    ## 325   280215       NASSAU          2019                22               18.0
    ## 326   280215       NASSAU          2019                29               25.4
    ## 327   280221       NASSAU          2019                55               15.0
    ## 328   280221       NASSAU          2019                65               16.1
    ## 329   280221       NASSAU          2019                36               14.0
    ## 330   280221       NASSAU          2019                40               14.0
    ## 331   280222       NASSAU          2019                32               13.7
    ## 332   280222       NASSAU          2019                59               11.9
    ## 333   280227       NASSAU          2019                27               15.4
    ## 334   280227       NASSAU          2019                32               20.5
    ## 335   280227       NASSAU          2019                28               19.9
    ## 336   280227       NASSAU          2019                22               15.2
    ## 337   280229       NASSAU          2019                18               10.9
    ## 338   280229       NASSAU          2019                30               14.2
    ## 339   280230       NASSAU          2019                37               18.6
    ## 340   280230       NASSAU          2019                32               15.3
    ## 341   280403       NASSAU          2019                25                9.9
    ## 342   280403       NASSAU          2019                32               13.7
    ## 343   280403       NASSAU          2019                36               21.1
    ## 344   280403       NASSAU          2019                29               13.9
    ## 345   280405       NASSAU          2019                46               13.6
    ## 346   280405       NASSAU          2019                47               13.7
    ## 347   280407       NASSAU          2019                82               11.1
    ## 348   280407       NASSAU          2019                87               11.1
    ## 349   280407       NASSAU          2019                62               13.5
    ## 350   280407       NASSAU          2019                79               15.9
    ## 351   280501       NASSAU          2019                34               14.4
    ## 352   280501       NASSAU          2019                31               11.1
    ## 353   280501       NASSAU          2019                25               15.8
    ## 354   280501       NASSAU          2019                18               11.8
    ## 355   280504       NASSAU          2019                66               15.7
    ## 356   280504       NASSAU          2019                93               18.0
    ## 357   280504       NASSAU          2019                70               20.6
    ## 358   280504       NASSAU          2019                51               14.0
    ## 359   280506       NASSAU          2019                14               14.7
    ## 360   280506       NASSAU          2019                14               15.4
    ## 361   280506       NASSAU          2019                10               11.9
    ## 362   280515       NASSAU          2019                25               10.1
    ## 363   280515       NASSAU          2019                28               10.9
    ## 364   280515       NASSAU          2019                28               13.5
    ## 365   280515       NASSAU          2019                51               18.3
    ## 366   280517       NASSAU          2019                78               16.5
    ## 367   280517       NASSAU          2019                84               15.7
    ## 368   280517       NASSAU          2019                52               17.7
    ## 369   280517       NASSAU          2019                58               18.6
    ## 370   280523       NASSAU          2019                68               12.4
    ## 371   280523       NASSAU          2019                90               14.3
    ## 372   280523       NASSAU          2019                77               16.2
    ## 373   280523       NASSAU          2019                74               16.6
    ## 374   400301      NIAGARA          2019                26               11.6
    ## 375   400301      NIAGARA          2019                34               13.4
    ## 376   400301      NIAGARA          2019                24               19.4
    ## 377   400301      NIAGARA          2019                36               24.5
    ## 378   400400      NIAGARA          2019                73               14.1
    ## 379   400400      NIAGARA          2019                58               11.9
    ## 380   400400      NIAGARA          2019                55               22.0
    ## 381   400400      NIAGARA          2019                41               13.2
    ## 382   401001      NIAGARA          2019                26               12.7
    ## 383   401001      NIAGARA          2019                32               16.2
    ## 384   401001      NIAGARA          2019                43               23.9
    ## 385   401001      NIAGARA          2019                30               16.2
    ## 386   401501      NIAGARA          2019                18               24.0
    ## 387   401501      NIAGARA          2019                20               27.0
    ## 388   401501      NIAGARA          2019                10               11.5
    ## 389   411504       ONEIDA          2019                 7               14.9
    ## 390   411504       ONEIDA          2019                 6               10.0
    ## 391   411603       ONEIDA          2019                24               18.3
    ## 392   411603       ONEIDA          2019                21               18.6
    ## 393   411603       ONEIDA          2019                10               14.5
    ## 394   411603       ONEIDA          2019                 9               15.0
    ## 395   411800       ONEIDA          2019               123               16.2
    ## 396   411800       ONEIDA          2019               118               15.4
    ## 397   411800       ONEIDA          2019                33               13.3
    ## 398   411800       ONEIDA          2019                32               12.3
    ## 399   412902       ONEIDA          2019                37               13.8
    ## 400   412902       ONEIDA          2019                42               13.9
    ## 401   412902       ONEIDA          2019                23               11.8
    ## 402   412902       ONEIDA          2019                37               13.6
    ## 403   420401     ONONDAGA          2019                59               17.3
    ## 404   420401     ONONDAGA          2019                59               16.7
    ## 405   420401     ONONDAGA          2019                25               15.2
    ## 406   420401     ONONDAGA          2019                25               15.3
    ## 407   420411     ONONDAGA          2019                33               14.2
    ## 408   420411     ONONDAGA          2019                40               15.9
    ## 409   420411     ONONDAGA          2019                36               17.7
    ## 410   420411     ONONDAGA          2019                28               13.2
    ## 411   420601     ONONDAGA          2019                11               18.0
    ## 412   420601     ONONDAGA          2019                11               15.9
    ## 413   420601     ONONDAGA          2019                 7               17.5
    ## 414   420601     ONONDAGA          2019                 7               16.3
    ## 415   420702     ONONDAGA          2019                23               14.3
    ## 416   420702     ONONDAGA          2019                28               18.4
    ## 417   420702     ONONDAGA          2019                28               23.5
    ## 418   420702     ONONDAGA          2019                12               12.1
    ## 419   420901     ONONDAGA          2019                83               15.8
    ## 420   420901     ONONDAGA          2019                74               12.5
    ## 421   420901     ONONDAGA          2019                67               18.4
    ## 422   420901     ONONDAGA          2019                46               12.1
    ## 423   421001     ONONDAGA          2019                53               15.2
    ## 424   421001     ONONDAGA          2019                49               13.1
    ## 425   421001     ONONDAGA          2019                36               14.3
    ## 426   421001     ONONDAGA          2019                35               13.8
    ## 427   421201     ONONDAGA          2019                17               21.5
    ## 428   421201     ONONDAGA          2019                11               11.8
    ## 429   421201     ONONDAGA          2019                17               28.8
    ## 430   421201     ONONDAGA          2019                 7               11.9
    ## 431   421800     ONONDAGA          2019               335               16.7
    ## 432   421800     ONONDAGA          2019               338               15.2
    ## 433   421800     ONONDAGA          2019               200               22.0
    ## 434   421800     ONONDAGA          2019               148               14.6
    ## 435   421902     ONONDAGA          2019                19               21.1
    ## 436   421902     ONONDAGA          2019                 9                9.4
    ## 437   421902     ONONDAGA          2019                12               27.3
    ## 438   421902     ONONDAGA          2019                 6               13.0
    ## 439   430700      ONTARIO          2019                43               18.4
    ## 440   430700      ONTARIO          2019                38               18.9
    ## 441   430700      ONTARIO          2019                34               23.1
    ## 442   430700      ONTARIO          2019                28               19.7
    ## 443   430901      ONTARIO          2019                16               14.5
    ## 444   430901      ONTARIO          2019                23               20.9
    ## 445   430901      ONTARIO          2019                16               21.3
    ## 446   430901      ONTARIO          2019                 8                9.6
    ## 447   431101      ONTARIO          2019                19               23.8
    ## 448   431101      ONTARIO          2019                17               20.7
    ## 449   431101      ONTARIO          2019                12               26.1
    ## 450   431101      ONTARIO          2019                12               17.9
    ## 451   431701      ONTARIO          2019               102               17.5
    ## 452   431701      ONTARIO          2019                83               15.0
    ## 453   431701      ONTARIO          2019                30                9.0
    ## 454   431701      ONTARIO          2019                46               14.2
    ## 455   440102       ORANGE          2019                49               15.4
    ## 456   440102       ORANGE          2019                40               12.5
    ## 457   440102       ORANGE          2019                54               19.6
    ## 458   440102       ORANGE          2019                56               17.4
    ## 459   440301       ORANGE          2019                35               15.8
    ## 460   440301       ORANGE          2019                37               14.4
    ## 461   440301       ORANGE          2019                38               21.1
    ## 462   440301       ORANGE          2019                41               20.2
    ## 463   441201       ORANGE          2019                84               15.3
    ## 464   441201       ORANGE          2019                92               16.1
    ## 465   441201       ORANGE          2019                96               23.8
    ## 466   441201       ORANGE          2019                78               18.3
    ## 467   441600       ORANGE          2019               238               17.3
    ## 468   441600       ORANGE          2019               259               18.0
    ## 469   441600       ORANGE          2019               150               21.7
    ## 470   441600       ORANGE          2019               111               13.9
    ## 471   442101       ORANGE          2019                86               14.8
    ## 472   442101       ORANGE          2019                76               12.8
    ## 473   442101       ORANGE          2019                43               15.6
    ## 474   442101       ORANGE          2019                49               16.6
    ## 475   450607      ORLEANS          2019                14               18.4
    ## 476   450607      ORLEANS          2019                17               23.3
    ## 477   450607      ORLEANS          2019                10               22.2
    ## 478   450607      ORLEANS          2019                 8               15.4
    ## 479   450704      ORLEANS          2019                16               13.2
    ## 480   450704      ORLEANS          2019                17               13.3
    ## 481   450704      ORLEANS          2019                13               17.1
    ## 482   450704      ORLEANS          2019                12               16.0
    ## 483   460102       OSWEGO          2019                29               19.6
    ## 484   460102       OSWEGO          2019                29               21.6
    ## 485   460102       OSWEGO          2019                20               22.5
    ## 486   460102       OSWEGO          2019                 9               13.2
    ## 487   460500       OSWEGO          2019                63               18.1
    ## 488   460500       OSWEGO          2019                63               15.9
    ## 489   460500       OSWEGO          2019                24               14.1
    ## 490   460500       OSWEGO          2019                40               20.5
    ## 491   460701       OSWEGO          2019                27               23.5
    ## 492   460701       OSWEGO          2019                30               21.0
    ## 493   460701       OSWEGO          2019                18               18.4
    ## 494   460701       OSWEGO          2019                22               20.8
    ## 495   461901       OSWEGO          2019                17               21.0
    ## 496   461901       OSWEGO          2019                14               13.1
    ## 497   461901       OSWEGO          2019                 8               16.7
    ## 498   461901       OSWEGO          2019                14               26.9
    ## 499   462001       OSWEGO          2019                29               14.6
    ## 500   462001       OSWEGO          2019                27               11.9
    ## 501   462001       OSWEGO          2019                24               19.0
    ## 502   462001       OSWEGO          2019                13               12.3
    ## 503   471400       OTSEGO          2019                24               13.4
    ## 504   471400       OTSEGO          2019                21               10.2
    ## 505   471400       OTSEGO          2019                19               15.0
    ## 506   471400       OTSEGO          2019                20               14.7
    ## 507   471701       OTSEGO          2019                12               15.8
    ## 508   471701       OTSEGO          2019                10               11.4
    ## 509   471701       OTSEGO          2019                14               17.9
    ## 510   471701       OTSEGO          2019                 9               13.8
    ## 511   480102       PUTNAM          2019                58               17.4
    ## 512   480102       PUTNAM          2019                64               16.9
    ## 513   480102       PUTNAM          2019                48               19.6
    ## 514   480102       PUTNAM          2019                52               19.3
    ## 515   480401       PUTNAM          2019                 6               10.2
    ## 516   480401       PUTNAM          2019                 6                8.7
    ## 517   480601       PUTNAM          2019                42               13.9
    ## 518   480601       PUTNAM          2019                56               17.0
    ## 519   480601       PUTNAM          2019                56               25.2
    ## 520   480601       PUTNAM          2019                41               16.5
    ## 521   490301   RENSSELAER          2019                85               21.3
    ## 522   490301   RENSSELAER          2019                66               16.9
    ## 523   490301   RENSSELAER          2019                74               25.5
    ## 524   490301   RENSSELAER          2019                62               20.1
    ## 525   491302   RENSSELAER          2019                44               18.8
    ## 526   491302   RENSSELAER          2019                39               15.4
    ## 527   491302   RENSSELAER          2019                38               20.7
    ## 528   491302   RENSSELAER          2019                28               13.9
    ## 529   491700   RENSSELAER          2019                65               21.2
    ## 530   491700   RENSSELAER          2019                61               16.4
    ## 531   491700   RENSSELAER          2019                34               15.0
    ## 532   491700   RENSSELAER          2019                32               12.9
    ## 533   500108     ROCKLAND          2019                26               12.6
    ## 534   500108     ROCKLAND          2019                23               11.8
    ## 535   500108     ROCKLAND          2019                26               19.7
    ## 536   500108     ROCKLAND          2019                19               14.7
    ## 537   500201     ROCKLAND          2019               146               16.2
    ## 538   500201     ROCKLAND          2019               155               15.8
    ## 539   500201     ROCKLAND          2019               121               19.7
    ## 540   500201     ROCKLAND          2019               123               18.4
    ## 541   500301     ROCKLAND          2019                40               15.1
    ## 542   500301     ROCKLAND          2019                38               13.7
    ## 543   500301     ROCKLAND          2019                43               17.7
    ## 544   500301     ROCKLAND          2019                46               18.0
    ## 545   500308     ROCKLAND          2019                28               13.7
    ## 546   500308     ROCKLAND          2019                46               19.5
    ## 547   500308     ROCKLAND          2019                28               14.7
    ## 548   500308     ROCKLAND          2019                35               21.2
    ## 549   510401 ST. LAWRENCE          2019                 5               13.9
    ## 550   512201 ST. LAWRENCE          2019                21               19.8
    ## 551   512201 ST. LAWRENCE          2019                18               16.5
    ## 552   512201 ST. LAWRENCE          2019                16               22.9
    ## 553   512201 ST. LAWRENCE          2019                 9               11.1
    ## 554   520401     SARATOGA          2019                20               16.5
    ## 555   520401     SARATOGA          2019                20               15.7
    ## 556   520401     SARATOGA          2019                19               22.6
    ## 557   520401     SARATOGA          2019                 8               10.3
    ## 558   521401     SARATOGA          2019                74               20.9
    ## 559   521401     SARATOGA          2019                65               15.9
    ## 560   521401     SARATOGA          2019                46               22.3
    ## 561   521401     SARATOGA          2019                27               13.0
    ## 562   521800     SARATOGA          2019                85               13.0
    ## 563   521800     SARATOGA          2019                89               13.4
    ## 564   521800     SARATOGA          2019                82               17.1
    ## 565   521800     SARATOGA          2019                61               13.1
    ## 566   530301  SCHENECTADY          2019                48               13.3
    ## 567   530301  SCHENECTADY          2019                60               15.2
    ## 568   530301  SCHENECTADY          2019                46               17.4
    ## 569   530301  SCHENECTADY          2019                36               13.7
    ## 570   530515  SCHENECTADY          2019                36               14.4
    ## 571   530515  SCHENECTADY          2019                38               12.7
    ## 572   530515  SCHENECTADY          2019                37               19.0
    ## 573   530515  SCHENECTADY          2019                39               18.7
    ## 574   550101     SCHUYLER          2019                12               14.0
    ## 575   550101     SCHUYLER          2019                13               16.3
    ## 576   560701       SENECA          2019                32               32.7
    ## 577   560701       SENECA          2019                22               17.2
    ## 578   560701       SENECA          2019                12               14.1
    ## 579   560701       SENECA          2019                19               21.8
    ## 580   570302      STEUBEN          2019                36               20.8
    ## 581   570302      STEUBEN          2019                33               16.5
    ## 582   570302      STEUBEN          2019                17               16.2
    ## 583   570302      STEUBEN          2019                15               13.8
    ## 584   571800      STEUBEN          2019                23               14.0
    ## 585   571800      STEUBEN          2019                20               12.4
    ## 586   571800      STEUBEN          2019                21               21.4
    ## 587   571800      STEUBEN          2019                27               19.0
    ## 588   571901      STEUBEN          2019                 8               19.0
    ## 589   572301      STEUBEN          2019                 6               13.3
    ## 590   572301      STEUBEN          2019                 5               21.7
    ## 591   572702      STEUBEN          2019                 8               25.8
    ## 592   572702      STEUBEN          2019                 7               20.0
    ## 593   580101      SUFFOLK          2019                12                8.6
    ## 594   580101      SUFFOLK          2019                23               14.2
    ## 595   580101      SUFFOLK          2019                32               26.7
    ## 596   580101      SUFFOLK          2019                26               21.3
    ## 597   580103      SUFFOLK          2019                62               14.5
    ## 598   580103      SUFFOLK          2019                55               13.0
    ## 599   580103      SUFFOLK          2019                56               20.4
    ## 600   580103      SUFFOLK          2019                46               17.6
    ## 601   580106      SUFFOLK          2019                57               16.0
    ## 602   580106      SUFFOLK          2019                62               15.6
    ## 603   580106      SUFFOLK          2019                35               19.2
    ## 604   580106      SUFFOLK          2019                30               15.5
    ## 605   580107      SUFFOLK          2019                52               14.2
    ## 606   580107      SUFFOLK          2019                49               13.8
    ## 607   580107      SUFFOLK          2019                30               16.7
    ## 608   580107      SUFFOLK          2019                40               18.9
    ## 609   580203      SUFFOLK          2019                49               14.2
    ## 610   580203      SUFFOLK          2019                43               14.6
    ## 611   580203      SUFFOLK          2019                36               17.9
    ## 612   580203      SUFFOLK          2019                33               16.5
    ## 613   580205      SUFFOLK          2019               212               17.0
    ## 614   580205      SUFFOLK          2019               227               17.1
    ## 615   580205      SUFFOLK          2019               155               19.3
    ## 616   580205      SUFFOLK          2019               180               21.8
    ## 617   580206      SUFFOLK          2019                12               11.1
    ## 618   580206      SUFFOLK          2019                11               11.5
    ## 619   580206      SUFFOLK          2019                12               18.2
    ## 620   580206      SUFFOLK          2019                15               17.4
    ## 621   580207      SUFFOLK          2019                29               15.0
    ## 622   580207      SUFFOLK          2019                38               15.1
    ## 623   580207      SUFFOLK          2019                29               16.9
    ## 624   580207      SUFFOLK          2019                31               16.8
    ## 625   580211      SUFFOLK          2019               130               17.0
    ## 626   580211      SUFFOLK          2019               119               15.6
    ## 627   580211      SUFFOLK          2019               111               18.6
    ## 628   580211      SUFFOLK          2019               132               18.5
    ## 629   580224      SUFFOLK          2019               127               15.2
    ## 630   580224      SUFFOLK          2019               141               15.9
    ## 631   580224      SUFFOLK          2019               103               19.3
    ## 632   580224      SUFFOLK          2019                88               17.2
    ## 633   580232      SUFFOLK          2019               195               20.8
    ## 634   580232      SUFFOLK          2019               194               18.1
    ## 635   580232      SUFFOLK          2019               125               18.0
    ## 636   580232      SUFFOLK          2019               116               17.0
    ## 637   580233      SUFFOLK          2019                19               13.6
    ## 638   580233      SUFFOLK          2019                25               16.2
    ## 639   580233      SUFFOLK          2019                20               13.5
    ## 640   580233      SUFFOLK          2019                18               15.8
    ## 641   580235      SUFFOLK          2019                77               18.4
    ## 642   580235      SUFFOLK          2019                88               22.6
    ## 643   580235      SUFFOLK          2019                55               23.3
    ## 644   580235      SUFFOLK          2019                54               19.1
    ## 645   580404      SUFFOLK          2019                74               15.4
    ## 646   580404      SUFFOLK          2019                85               17.0
    ## 647   580404      SUFFOLK          2019                77               18.1
    ## 648   580404      SUFFOLK          2019                74               17.3
    ## 649   580405      SUFFOLK          2019               121               16.5
    ## 650   580405      SUFFOLK          2019                93               11.9
    ## 651   580405      SUFFOLK          2019               108               18.5
    ## 652   580405      SUFFOLK          2019                91               14.9
    ## 653   580406      SUFFOLK          2019                36               16.7
    ## 654   580406      SUFFOLK          2019                39               15.2
    ## 655   580406      SUFFOLK          2019                35               14.6
    ## 656   580406      SUFFOLK          2019                30               13.3
    ## 657   580502      SUFFOLK          2019                32               16.0
    ## 658   580502      SUFFOLK          2019                30               13.0
    ## 659   580502      SUFFOLK          2019                33               14.7
    ## 660   580502      SUFFOLK          2019                46               18.8
    ## 661   580503      SUFFOLK          2019                56               13.9
    ## 662   580503      SUFFOLK          2019                68               17.3
    ## 663   580503      SUFFOLK          2019                47               18.2
    ## 664   580503      SUFFOLK          2019                59               19.3
    ## 665   580506      SUFFOLK          2019                 5                4.9
    ## 666   580506      SUFFOLK          2019                 6                5.8
    ## 667   580506      SUFFOLK          2019                38               21.0
    ## 668   580506      SUFFOLK          2019                23               11.7
    ## 669   580509      SUFFOLK          2019                49               13.9
    ## 670   580509      SUFFOLK          2019                69               17.1
    ## 671   580509      SUFFOLK          2019                60               19.0
    ## 672   580509      SUFFOLK          2019                49               16.0
    ## 673   580602      SUFFOLK          2019               103               19.6
    ## 674   580602      SUFFOLK          2019                94               18.7
    ## 675   580602      SUFFOLK          2019                67               18.6
    ## 676   580602      SUFFOLK          2019                77               20.4
    ## 677   580801      SUFFOLK          2019                74               12.8
    ## 678   580801      SUFFOLK          2019                81               14.0
    ## 679   580801      SUFFOLK          2019                99               18.4
    ## 680   580801      SUFFOLK          2019                89               16.0
    ## 681   580913      SUFFOLK          2019                15               28.8
    ## 682   580913      SUFFOLK          2019                25               62.5
    ## 683   590501     SULLIVAN          2019                22               16.4
    ## 684   590501     SULLIVAN          2019                22               16.1
    ## 685   590501     SULLIVAN          2019                 8               14.3
    ## 686   590501     SULLIVAN          2019                 9               15.3
    ## 687   590801     SULLIVAN          2019                 5                7.6
    ## 688   590801     SULLIVAN          2019                 9               14.1
    ## 689   590801     SULLIVAN          2019                14               28.6
    ## 690   590801     SULLIVAN          2019                 5               13.9
    ## 691   591502     SULLIVAN          2019                29               21.5
    ## 692   591502     SULLIVAN          2019                19               14.1
    ## 693   591502     SULLIVAN          2019                12               16.2
    ## 694   591502     SULLIVAN          2019                16               17.2
    ## 695   600101        TIOGA          2019                12               16.9
    ## 696   600101        TIOGA          2019                17               18.3
    ## 697   600101        TIOGA          2019                26               25.5
    ## 698   600101        TIOGA          2019                16               16.0
    ## 699   600301        TIOGA          2019                10               14.5
    ## 700   600301        TIOGA          2019                 7                9.3
    ## 701   600801        TIOGA          2019                12               14.5
    ## 702   600801        TIOGA          2019                11               12.1
    ## 703   600801        TIOGA          2019                 6               11.5
    ## 704   600801        TIOGA          2019                11               32.4
    ## 705   610501     TOMPKINS          2019                15               18.5
    ## 706   610501     TOMPKINS          2019                13               18.3
    ## 707   610501     TOMPKINS          2019                11               22.4
    ## 708   610501     TOMPKINS          2019                 5                9.3
    ## 709   610600     TOMPKINS          2019                78               12.6
    ## 710   610600     TOMPKINS          2019                74               12.0
    ## 711   610600     TOMPKINS          2019                43               12.5
    ## 712   610600     TOMPKINS          2019                43               12.6
    ## 713   610801     TOMPKINS          2019                11                8.7
    ## 714   610801     TOMPKINS          2019                24               19.2
    ## 715   610801     TOMPKINS          2019                10               16.9
    ## 716   610801     TOMPKINS          2019                16               17.8
    ## 717   620600       ULSTER          2019               115               16.1
    ## 718   620600       ULSTER          2019               119               17.0
    ## 719   620600       ULSTER          2019                83               22.0
    ## 720   620600       ULSTER          2019                61               16.4
    ## 721   620803       ULSTER          2019                29               17.0
    ## 722   620803       ULSTER          2019                25               13.8
    ## 723   620803       ULSTER          2019                18               17.8
    ## 724   620803       ULSTER          2019                22               17.9
    ## 725   621101       ULSTER          2019                37               20.4
    ## 726   621101       ULSTER          2019                29               15.8
    ## 727   621101       ULSTER          2019                22               14.9
    ## 728   621101       ULSTER          2019                31               19.3
    ## 729   621801       ULSTER          2019                43               14.9
    ## 730   621801       ULSTER          2019                49               16.9
    ## 731   621801       ULSTER          2019                47               19.3
    ## 732   621801       ULSTER          2019                47               19.2
    ## 733   630300       WARREN          2019                37               15.8
    ## 734   630300       WARREN          2019                33               17.1
    ## 735   630300       WARREN          2019                29               19.0
    ## 736   630300       WARREN          2019                16               12.2
    ## 737   630902       WARREN          2019                58               19.0
    ## 738   630902       WARREN          2019                49               13.8
    ## 739   630902       WARREN          2019                41               17.8
    ## 740   630902       WARREN          2019                40               17.2
    ## 741   640701   WASHINGTON          2019                14               17.3
    ## 742   640701   WASHINGTON          2019                20               23.8
    ## 743   640701   WASHINGTON          2019                 5                9.3
    ## 744   640701   WASHINGTON          2019                12               18.8
    ## 745   641301   WASHINGTON          2019                59               20.0
    ## 746   641301   WASHINGTON          2019                46               15.6
    ## 747   641301   WASHINGTON          2019                30               21.0
    ## 748   641301   WASHINGTON          2019                30               20.5
    ## 749   650901        WAYNE          2019                29               20.7
    ## 750   650901        WAYNE          2019                22               16.5
    ## 751   650901        WAYNE          2019                 5               10.2
    ## 752   651503        WAYNE          2019                 8               10.5
    ## 753   651503        WAYNE          2019                 7                8.5
    ## 754   651503        WAYNE          2019                10               20.4
    ## 755   651503        WAYNE          2019                13               23.2
    ## 756   660202  WESTCHESTER          2019                25               15.2
    ## 757   660202  WESTCHESTER          2019                29               15.3
    ## 758   660202  WESTCHESTER          2019                22               19.0
    ## 759   660202  WESTCHESTER          2019                18               16.1
    ## 760   660203  WESTCHESTER          2019                39               17.4
    ## 761   660203  WESTCHESTER          2019                46               17.6
    ## 762   660203  WESTCHESTER          2019                30               15.1
    ## 763   660203  WESTCHESTER          2019                33               15.6
    ## 764   660402  WESTCHESTER          2019                21               13.8
    ## 765   660402  WESTCHESTER          2019                31               18.1
    ## 766   660402  WESTCHESTER          2019                16               13.7
    ## 767   660402  WESTCHESTER          2019                17               14.5
    ## 768   660405  WESTCHESTER          2019                30               12.3
    ## 769   660405  WESTCHESTER          2019                29               10.5
    ## 770   660405  WESTCHESTER          2019                16               11.9
    ## 771   660405  WESTCHESTER          2019                27               15.5
    ## 772   660407  WESTCHESTER          2019                43               18.1
    ## 773   660407  WESTCHESTER          2019                48               18.3
    ## 774   660407  WESTCHESTER          2019                11               20.0
    ## 775   660407  WESTCHESTER          2019                13               18.6
    ## 776   660501  WESTCHESTER          2019                55               12.8
    ## 777   660501  WESTCHESTER          2019                53               12.4
    ## 778   660501  WESTCHESTER          2019                47               18.7
    ## 779   660501  WESTCHESTER          2019                27               11.7
    ## 780   660801  WESTCHESTER          2019                30               15.7
    ## 781   660801  WESTCHESTER          2019                31               12.1
    ## 782   660801  WESTCHESTER          2019                20               16.3
    ## 783   660801  WESTCHESTER          2019                20               15.2
    ## 784   660805  WESTCHESTER          2019                27               18.9
    ## 785   660805  WESTCHESTER          2019                20               13.2
    ## 786   660805  WESTCHESTER          2019                27               25.2
    ## 787   660805  WESTCHESTER          2019                18               18.2
    ## 788   660809  WESTCHESTER          2019                16               11.0
    ## 789   660809  WESTCHESTER          2019                15                9.4
    ## 790   660809  WESTCHESTER          2019                17               17.0
    ## 791   660809  WESTCHESTER          2019                18               14.1
    ## 792   660900  WESTCHESTER          2019                80               15.7
    ## 793   660900  WESTCHESTER          2019                71               15.1
    ## 794   660900  WESTCHESTER          2019                29               10.4
    ## 795   660900  WESTCHESTER          2019                36               11.8
    ## 796   661100  WESTCHESTER          2019               134               15.7
    ## 797   661100  WESTCHESTER          2019               133               14.3
    ## 798   661100  WESTCHESTER          2019               104               17.2
    ## 799   661100  WESTCHESTER          2019               103               14.5
    ## 800   661301  WESTCHESTER          2019                18               17.5
    ## 801   661301  WESTCHESTER          2019                11               10.7
    ## 802   661301  WESTCHESTER          2019                11               17.2
    ## 803   661301  WESTCHESTER          2019                13               24.1
    ## 804   661601  WESTCHESTER          2019                29               12.9
    ## 805   661601  WESTCHESTER          2019                22                8.4
    ## 806   661601  WESTCHESTER          2019                13                9.0
    ## 807   661601  WESTCHESTER          2019                18               12.2
    ## 808   661901  WESTCHESTER          2019                13                8.8
    ## 809   661901  WESTCHESTER          2019                13                7.7
    ## 810   661901  WESTCHESTER          2019                 8                8.5
    ## 811   661901  WESTCHESTER          2019                16               13.0
    ## 812   662001  WESTCHESTER          2019                77               15.9
    ## 813   662001  WESTCHESTER          2019                44                9.4
    ## 814   662001  WESTCHESTER          2019                37               11.1
    ## 815   662001  WESTCHESTER          2019                46               13.1
    ## 816   662101  WESTCHESTER          2019                23               18.4
    ## 817   662101  WESTCHESTER          2019                21               15.0
    ## 818   662101  WESTCHESTER          2019                17               12.7
    ## 819   662101  WESTCHESTER          2019                18               11.8
    ## 820   670401      WYOMING          2019                10               13.5
    ## 821   670401      WYOMING          2019                13               12.7
    ## 822   670401      WYOMING          2019                12               22.2
    ## 823   670401      WYOMING          2019                13               25.5
    ## 824   671501      WYOMING          2019                21               25.3
    ## 825   671501      WYOMING          2019                17               19.5
    ## 826   671501      WYOMING          2019                 6                9.5
    ## 827   671501      WYOMING          2019                 9               17.6
    ## 828   680801        YATES          2019                12               17.9
    ## 829   680801        YATES          2019                14               19.2
    ## 830   680801        YATES          2019                 7               16.3
    ## 831   680801        YATES          2019                 8               17.0
    ##     number_obese percent_obese number_overweight_or_obese
    ## 1             28          11.4                         56
    ## 2             42          16.7                         77
    ## 3             17          19.1                         38
    ## 4             28          30.8                         37
    ## 5             22          27.5                         38
    ## 6             38          21.1                         74
    ## 7             43          19.5                         82
    ## 8             30          27.8                         48
    ## 9             97          19.1                        167
    ## 10            83          16.9                        164
    ## 11            63          20.1                        123
    ## 12            69          23.5                        115
    ## 13            68          12.8                        157
    ## 14            81          15.9                        157
    ## 15            39           9.1                        129
    ## 16            80          16.9                        159
    ## 17            70          14.3                        155
    ## 18            62          12.4                        136
    ## 19            32          10.1                        101
    ## 20            56          17.3                        112
    ## 21            17          13.8                         36
    ## 22            22          17.5                         43
    ## 23            23          26.4                         45
    ## 24            24          28.6                         38
    ## 25            14          12.3                         39
    ## 26            22          15.1                         45
    ## 27            24          22.9                         49
    ## 28            34          28.8                         47
    ## 29           102          22.4                        166
    ## 30            91          19.4                        171
    ## 31            53          28.3                         82
    ## 32            62          31.6                         88
    ## 33            15          20.3                         24
    ## 34            22          21.8                         37
    ## 35            15          31.3                         21
    ## 36            12          23.1                         21
    ## 37            27          16.8                         58
    ## 38            28          18.2                         57
    ## 39            11          17.7                         21
    ## 40            24          21.6                         44
    ## 41            36          13.6                         75
    ## 42            37          13.0                         81
    ## 43            14          12.7                         35
    ## 44            32          26.4                         56
    ## 45            21          19.8                         36
    ## 46            32          20.4                         52
    ## 47            11          12.2                         28
    ## 48            21          14.3                         52
    ## 49            10          13.3                         22
    ## 50            19          20.4                         30
    ## 51             9          20.5                         22
    ## 52             9          20.9                         21
    ## 53            11          22.4                         25
    ## 54             6          15.0                         14
    ## 55            29          12.0                         69
    ## 56            50          17.9                         95
    ## 57            38          29.2                         64
    ## 58            36          25.0                         59
    ## 59            14          15.9                         26
    ## 60            15          18.3                         33
    ## 61            21          28.4                         33
    ## 62            21          30.9                         32
    ## 63            12          11.7                         36
    ## 64            16          12.6                         45
    ## 65            13          29.5                         23
    ## 66            24          32.4                         36
    ## 67            11          17.5                         22
    ## 68            13          17.3                         20
    ## 69            17          20.0                         24
    ## 70            19          27.9                         30
    ## 71            22          18.8                         43
    ## 72            29          26.4                         49
    ## 73            13          21.3                         26
    ## 74            19          33.9                         29
    ## 75            11          14.5                         33
    ## 76             8          11.8                         20
    ## 77            10          22.2                         22
    ## 78             6          10.5                         13
    ## 79            16          21.9                         27
    ## 80            11          16.4                         27
    ## 81            13          14.0                         39
    ## 82            17          16.0                         40
    ## 83            20          11.5                         46
    ## 84            27          14.8                         47
    ## 85            42          32.8                         63
    ## 86            36          27.9                         65
    ## 87            30          25.0                         50
    ## 88            33          32.4                         46
    ## 89            22          22.4                         40
    ## 90            20          26.0                         33
    ## 91            17          25.4                         35
    ## 92            13          19.1                         27
    ## 93            10          32.3                         16
    ## 94             5          12.8                         10
    ## 95             7          15.2                         14
    ## 96             5          17.2                         12
    ## 97            10          22.2                         17
    ## 98            20          23.3                         34
    ## 99            20          21.1                         36
    ## 100            8          20.0                         14
    ## 101          154          24.1                        294
    ## 102          141          20.3                        284
    ## 103           99          23.0                        197
    ## 104           97          21.7                        188
    ## 105           15          14.3                         35
    ## 106           21          21.4                         32
    ## 107           13          17.8                         25
    ## 108            9          15.3                         22
    ## 109           16          29.6                         24
    ## 110           12          23.5                         23
    ## 111           25          24.0                         42
    ## 112           13          16.5                         23
    ## 113           43          21.5                         75
    ## 114           40          20.1                         74
    ## 115           32          23.9                         63
    ## 116           38          29.7                         60
    ## 117           23          17.6                         46
    ## 118           31          24.0                         51
    ## 119           22          31.0                         35
    ## 120           22          28.2                         31
    ## 121           53          21.5                         90
    ## 122           38          17.1                         74
    ## 123           36          28.1                         60
    ## 124           38          31.1                         57
    ## 125           54          24.7                         90
    ## 126           28          13.7                         55
    ## 127           41          28.3                         75
    ## 128           34          22.5                         51
    ## 129           17          19.8                         37
    ## 130           20          17.1                         40
    ## 131           33          23.7                         52
    ## 132           21          26.3                         38
    ## 133           48          21.2                         88
    ## 134           44          19.1                         75
    ## 135           17          14.3                         33
    ## 136           14          17.7                         24
    ## 137           90          14.1                        176
    ## 138           76          12.6                        167
    ## 139           79          13.8                        177
    ## 140          110          20.0                        202
    ## 141           16          12.9                         35
    ## 142           22          14.1                         49
    ## 143           18          18.8                         42
    ## 144           20          18.9                         32
    ## 145           19          13.7                         39
    ## 146           18          11.5                         37
    ## 147           16          15.1                         38
    ## 148           19          15.0                         40
    ## 149           15          23.4                         29
    ## 150            6           9.1                         18
    ## 151           13          19.4                         21
    ## 152           12          15.4                         20
    ## 153           30          12.8                         63
    ## 154           26          13.1                         52
    ## 155           21          16.0                         38
    ## 156           27          26.0                         47
    ## 157          104          13.0                        237
    ## 158          105          11.0                        244
    ## 159          108          11.1                        240
    ## 160           89          10.9                        224
    ## 161           51          14.4                         97
    ## 162           60          14.5                        126
    ## 163           47          21.1                         75
    ## 164           71          27.3                        109
    ## 165           43          16.7                         85
    ## 166           44          17.7                         92
    ## 167           45          29.2                         74
    ## 168           46          26.4                         67
    ## 169           33          25.6                         54
    ## 170           16           8.2                         48
    ## 171           33          16.8                         67
    ## 172           27          21.3                         45
    ## 173           24          16.1                         45
    ## 174           12           8.1                         39
    ## 175           11          10.3                         29
    ## 176           12          11.5                         32
    ## 177           28          10.2                         65
    ## 178           37          12.8                         81
    ## 179           24          14.1                         64
    ## 180           41          20.5                         68
    ## 181           26          10.7                         72
    ## 182           38          13.8                         78
    ## 183           22          14.2                         43
    ## 184           15          11.3                         30
    ## 185           52          16.4                        108
    ## 186           79          14.2                        154
    ## 187           62          11.5                        136
    ## 188           44          14.7                         97
    ## 189           33          15.2                         77
    ## 190           57          18.4                         94
    ## 191           16          17.4                         35
    ## 192           19          18.6                         39
    ## 193           48           9.4                        134
    ## 194           66          11.7                        162
    ## 195           54          13.9                        130
    ## 196           82          19.0                        134
    ## 197           31          27.0                         43
    ## 198            5           3.8                         39
    ## 199            7           4.8                         54
    ## 200           16          15.1                         37
    ## 201           36           8.1                        108
    ## 202           63          13.0                        105
    ## 203           37          11.5                         75
    ## 204           35          11.8                         80
    ## 205          140          14.8                        296
    ## 206          160          17.9                        295
    ## 207           82          20.0                        175
    ## 208           90          23.4                        153
    ## 209           30          30.9                         54
    ## 210           27          25.0                         48
    ## 211           15          29.4                         22
    ## 212           16          32.0                         25
    ## 213           52          27.8                         82
    ## 214           51          25.2                         87
    ## 215           28          31.8                         48
    ## 216           39          39.0                         57
    ## 217           10           9.5                         34
    ## 218           20          15.7                         38
    ## 219           13          20.3                         30
    ## 220           14          23.0                         21
    ## 221           55          21.8                         91
    ## 222           55          20.4                         94
    ## 223           34          24.1                         71
    ## 224           40          21.9                         72
    ## 225           22          20.4                         47
    ## 226           32          24.4                         57
    ## 227           20          30.3                         31
    ## 228           19          30.6                         30
    ## 229           19          14.7                         43
    ## 230           31          21.5                         67
    ## 231           25          26.0                         45
    ## 232           21          22.1                         40
    ## 233           34          15.6                         68
    ## 234           39          19.4                         83
    ## 235           23          21.5                         43
    ## 236           34          26.2                         52
    ## 237           84          16.3                        157
    ## 238           98          17.3                        182
    ## 239           38          16.4                         89
    ## 240           50          21.9                         92
    ## 241           14          18.9                         26
    ## 242           17          21.8                         26
    ## 243           16          22.2                         33
    ## 244           14          28.0                         23
    ## 245           45          16.3                         77
    ## 246           59          17.2                        106
    ## 247           41          24.7                         77
    ## 248           63          26.4                        108
    ## 249           12          19.4                         21
    ## 250           12          14.0                         21
    ## 251            9          15.3                         22
    ## 252           12          19.0                         22
    ## 253           29          19.7                         57
    ## 254           30          21.3                         58
    ## 255           18          24.7                         39
    ## 256           22          32.4                         33
    ## 257           25          16.4                         46
    ## 258           41          21.8                         62
    ## 259           34          26.0                         59
    ## 260           35          22.9                         66
    ## 261           28           8.6                         71
    ## 262           34           9.4                         88
    ## 263           27          11.1                         77
    ## 264           25          12.3                         51
    ## 265           68          16.9                        140
    ## 266           86          20.4                        155
    ## 267           51          21.5                         99
    ## 268           67          26.7                        111
    ## 269           51          14.4                        117
    ## 270           63          14.8                        140
    ## 271           29          11.1                         74
    ## 272           34          13.5                         83
    ## 273           58          13.6                        143
    ## 274           56          11.9                        134
    ## 275           58          18.6                        108
    ## 276           64          20.6                        118
    ## 277           21          17.4                         40
    ## 278           24          17.8                         36
    ## 279           18          25.0                         33
    ## 280           20          27.4                         30
    ## 281           56          13.9                        120
    ## 282           75          16.4                        178
    ## 283           50          16.9                        114
    ## 284           59          18.3                        112
    ## 285           76          13.7                        158
    ## 286           97          16.4                        178
    ## 287           71          21.2                        120
    ## 288           83          23.4                        156
    ## 289           38          12.5                         84
    ## 290           57          17.8                        102
    ## 291           32          17.2                         66
    ## 292           42          22.2                         80
    ## 293          107          11.3                        250
    ## 294          128          13.5                        246
    ## 295           96          14.3                        220
    ## 296          113          16.5                        227
    ## 297           81          20.6                        158
    ## 298           96          24.6                        165
    ## 299           49          35.5                         72
    ## 300           45          24.9                         72
    ## 301          198          23.1                        375
    ## 302          215          26.9                        352
    ## 303           89          19.3                        202
    ## 304          120          22.7                        232
    ## 305           64          11.0                        145
    ## 306           67          12.1                        157
    ## 307           63          12.8                        170
    ## 308           89          18.2                        183
    ## 309           61           8.4                        157
    ## 310           77          11.0                        165
    ## 311           45          10.5                        126
    ## 312           76          16.5                        150
    ## 313           18          11.3                         45
    ## 314           18          10.0                         51
    ## 315           27          18.2                         51
    ## 316           40          21.1                         67
    ## 317          157          20.4                        328
    ## 318          244          26.6                        425
    ## 319           63          16.5                        161
    ## 320           83          20.9                        160
    ## 321           27          11.9                         53
    ## 322           38          15.0                         62
    ## 323           49          16.2                        101
    ## 324           69          23.9                        117
    ## 325           28          23.0                         50
    ## 326           32          28.1                         61
    ## 327           33           9.0                         88
    ## 328           43          10.6                        108
    ## 329           19           7.4                         55
    ## 330           35          12.3                         75
    ## 331           35          15.0                         67
    ## 332           78          15.8                        137
    ## 333           42          24.0                         69
    ## 334           38          24.4                         70
    ## 335           38          27.0                         66
    ## 336           28          19.3                         50
    ## 337           18          10.9                         36
    ## 338           30          14.2                         60
    ## 339           33          16.6                         70
    ## 340           30          14.4                         62
    ## 341           18           7.1                         43
    ## 342           28          12.0                         60
    ## 343            9           5.3                         45
    ## 344           21          10.1                         50
    ## 345           32           9.5                         78
    ## 346           52          15.2                         99
    ## 347           61           8.3                        143
    ## 348           96          12.2                        183
    ## 349           30           6.5                         92
    ## 350           57          11.5                        136
    ## 351           27          11.4                         61
    ## 352           36          12.9                         67
    ## 353            7           4.4                         32
    ## 354           13           8.6                         31
    ## 355           28           6.7                         94
    ## 356           39           7.5                        132
    ## 357           30           8.8                        100
    ## 358           52          14.3                        103
    ## 359           15          15.8                         29
    ## 360            9           9.9                         23
    ## 361            8           9.5                         18
    ## 362            8           3.2                         33
    ## 363           11           4.3                         39
    ## 364           17           8.2                         45
    ## 365           31          11.1                         82
    ## 366           64          13.5                        142
    ## 367          102          19.1                        186
    ## 368           53          18.0                        105
    ## 369           75          24.1                        133
    ## 370           42           7.7                        110
    ## 371           53           8.4                        143
    ## 372           55          11.6                        132
    ## 373           79          17.7                        153
    ## 374           28          12.4                         54
    ## 375           36          14.2                         70
    ## 376           12           9.7                         36
    ## 377           21          14.3                         57
    ## 378           82          15.8                        155
    ## 379           95          19.5                        153
    ## 380           35          14.0                         90
    ## 381           72          23.2                        113
    ## 382           23          11.3                         49
    ## 383           27          13.7                         59
    ## 384           21          11.7                         64
    ## 385           32          17.3                         62
    ## 386           13          17.3                         31
    ## 387            7           9.5                         27
    ## 388           24          27.6                         34
    ## 389           13          27.7                         20
    ## 390           10          16.7                         16
    ## 391           28          21.4                         52
    ## 392           19          16.8                         40
    ## 393           22          31.9                         32
    ## 394           15          25.0                         24
    ## 395          159          20.9                        282
    ## 396          155          20.2                        273
    ## 397           79          31.9                        112
    ## 398           71          27.3                        103
    ## 399           44          16.4                         81
    ## 400           54          17.8                         96
    ## 401           25          12.8                         48
    ## 402           63          23.2                        100
    ## 403           42          12.3                        101
    ## 404           42          11.9                        101
    ## 405           23          13.9                         48
    ## 406           21          12.9                         46
    ## 407           23           9.9                         56
    ## 408           32          12.7                         72
    ## 409           26          12.8                         62
    ## 410           32          15.1                         60
    ## 411           14          23.0                         25
    ## 412            9          13.0                         20
    ## 413            6          15.0                         13
    ## 414           13          30.2                         20
    ## 415           37          23.0                         60
    ## 416           21          13.8                         49
    ## 417           30          25.2                         58
    ## 418           23          23.2                         35
    ## 419           77          14.6                        160
    ## 420          101          17.0                        175
    ## 421           65          17.9                        132
    ## 422           73          19.3                        119
    ## 423           27           7.7                         80
    ## 424           31           8.3                         80
    ## 425           23           9.2                         59
    ## 426           38          15.0                         73
    ## 427           14          17.7                         31
    ## 428           25          26.9                         36
    ## 429           13          22.0                         30
    ## 430           14          23.7                         21
    ## 431          397          19.7                        732
    ## 432          480          21.5                        818
    ## 433          256          28.1                        456
    ## 434          279          27.4                        427
    ## 435            9          10.0                         28
    ## 436           11          11.5                         20
    ## 437           13          29.5                         25
    ## 438           14          30.4                         20
    ## 439           46          19.7                         89
    ## 440           32          15.9                         70
    ## 441           37          25.2                         71
    ## 442           35          24.6                         63
    ## 443           23          20.9                         39
    ## 444           18          16.4                         41
    ## 445           27          36.0                         43
    ## 446           15          18.1                         23
    ## 447           17          21.3                         36
    ## 448           12          14.6                         29
    ## 449            8          17.4                         20
    ## 450           19          28.4                         31
    ## 451           27           4.6                        129
    ## 452           43           7.8                        126
    ## 453           29           8.7                         59
    ## 454           55          17.0                        101
    ## 455           42          13.2                         91
    ## 456           49          15.3                         89
    ## 457           45          16.3                         99
    ## 458           66          20.6                        122
    ## 459           24          10.9                         59
    ## 460           31          12.1                         68
    ## 461           22          12.2                         60
    ## 462           44          21.7                         85
    ## 463           44           8.0                        128
    ## 464           79          13.9                        171
    ## 465           41          10.2                        137
    ## 466           67          15.7                        145
    ## 467          345          25.0                        583
    ## 468          421          29.3                        680
    ## 469          187          27.1                        337
    ## 470          250          31.3                        361
    ## 471           48           8.2                        134
    ## 472           60          10.1                        136
    ## 473           35          12.7                         78
    ## 474           45          15.3                         94
    ## 475           16          21.1                         30
    ## 476           15          20.5                         32
    ## 477           10          22.2                         20
    ## 478           11          21.2                         19
    ## 479           31          25.6                         47
    ## 480           25          19.5                         42
    ## 481           26          34.2                         39
    ## 482           30          40.0                         42
    ## 483           36          24.3                         65
    ## 484           30          22.4                         59
    ## 485           31          34.8                         51
    ## 486           24          35.3                         33
    ## 487           62          17.8                        125
    ## 488           79          19.9                        142
    ## 489           43          25.3                         67
    ## 490           52          26.7                         92
    ## 491           27          23.5                         54
    ## 492           32          22.4                         62
    ## 493           30          30.6                         48
    ## 494           28          26.4                         50
    ## 495           16          19.8                         33
    ## 496           29          27.1                         43
    ## 497           16          33.3                         24
    ## 498           14          26.9                         28
    ## 499           41          20.6                         70
    ## 500           31          13.7                         58
    ## 501           33          26.2                         57
    ## 502           32          30.2                         45
    ## 503           36          20.1                         60
    ## 504           31          15.1                         52
    ## 505           18          14.2                         37
    ## 506           29          21.3                         49
    ## 507            8          10.5                         20
    ## 508            8           9.1                         18
    ## 509            6           7.7                         20
    ## 510           13          20.0                         22
    ## 511           53          15.9                        111
    ## 512           77          20.4                        141
    ## 513           50          20.4                         98
    ## 514           73          27.0                        125
    ## 515            5           8.5                         11
    ## 516            5           7.2                         11
    ## 517           51          16.9                         93
    ## 518           66          20.1                        122
    ## 519           44          19.8                        100
    ## 520           54          21.8                         95
    ## 521           56          14.0                        141
    ## 522           62          15.9                        128
    ## 523           42          14.5                        116
    ## 524           52          16.9                        114
    ## 525           26          11.1                         70
    ## 526           34          13.4                         73
    ## 527           23          12.5                         61
    ## 528           29          14.4                         57
    ## 529           74          24.2                        139
    ## 530           52          13.9                        113
    ## 531           44          19.5                         78
    ## 532           41          16.5                         73
    ## 533           37          18.0                         63
    ## 534           38          19.5                         61
    ## 535           20          15.2                         46
    ## 536           17          13.2                         36
    ## 537          167          18.6                        313
    ## 538          211          21.6                        366
    ## 539          115          18.7                        236
    ## 540          135          20.2                        258
    ## 541           37          14.0                         77
    ## 542           27           9.7                         65
    ## 543           28          11.5                         71
    ## 544           53          20.8                         99
    ## 545           18           8.8                         46
    ## 546           17           7.2                         63
    ## 547           13           6.8                         41
    ## 548           21          12.7                         56
    ## 549            8          22.2                         13
    ## 550           25          23.6                         46
    ## 551           32          29.4                         50
    ## 552           26          37.1                         42
    ## 553           33          40.7                         42
    ## 554           18          14.9                         38
    ## 555           18          14.2                         38
    ## 556           16          19.0                         35
    ## 557           20          25.6                         28
    ## 558           42          11.9                        116
    ## 559           60          14.7                        125
    ## 560           40          19.4                         86
    ## 561           35          16.9                         62
    ## 562           56           8.6                        141
    ## 563           77          11.6                        166
    ## 564           60          12.5                        142
    ## 565           63          13.5                        124
    ## 566           22           6.1                         70
    ## 567           44          11.2                        104
    ## 568           16           6.0                         62
    ## 569           28          10.7                         64
    ## 570           55          22.0                         91
    ## 571           53          17.7                         91
    ## 572           54          27.7                         91
    ## 573           62          29.7                        101
    ## 574            8           9.3                         20
    ## 575            8          10.0                         21
    ## 576           14          14.3                         46
    ## 577           21          16.4                         43
    ## 578           18          21.2                         30
    ## 579           28          32.2                         47
    ## 580           33          19.1                         69
    ## 581           43          21.5                         76
    ## 582           27          25.7                         44
    ## 583           38          34.9                         53
    ## 584           34          20.7                         57
    ## 585           32          19.9                         52
    ## 586           21          21.4                         42
    ## 587           37          26.1                         64
    ## 588            7          16.7                         15
    ## 589           14          31.1                         20
    ## 590            6          26.1                         11
    ## 591            6          19.4                         14
    ## 592            5          14.3                         12
    ## 593           16          11.5                         28
    ## 594           17          10.5                         40
    ## 595           17          14.2                         49
    ## 596           13          10.7                         39
    ## 597           86          20.1                        148
    ## 598           88          20.8                        143
    ## 599           67          24.4                        123
    ## 600           66          25.2                        112
    ## 601           91          25.6                        148
    ## 602           95          23.9                        157
    ## 603           45          24.7                         80
    ## 604           47          24.4                         77
    ## 605           75          20.5                        127
    ## 606           72          20.2                        121
    ## 607           39          21.7                         69
    ## 608           43          20.3                         83
    ## 609           76          22.0                        125
    ## 610           55          18.6                         98
    ## 611           44          21.9                         80
    ## 612           63          31.5                         96
    ## 613          211          16.9                        423
    ## 614          244          18.3                        471
    ## 615          126          15.7                        281
    ## 616          184          22.2                        364
    ## 617            8           7.4                         20
    ## 618           11          11.5                         22
    ## 619            8          12.1                         20
    ## 620           14          16.3                         29
    ## 621           19           9.8                         48
    ## 622           31          12.4                         69
    ## 623           19          11.0                         48
    ## 624           22          12.0                         53
    ## 625          105          13.7                        235
    ## 626          136          17.9                        255
    ## 627           92          15.4                        203
    ## 628          171          23.9                        303
    ## 629          160          19.1                        287
    ## 630          197          22.2                        338
    ## 631          142          26.6                        245
    ## 632          170          33.3                        258
    ## 633          181          19.3                        376
    ## 634          285          26.7                        479
    ## 635          199          28.7                        324
    ## 636          176          25.8                        292
    ## 637           18          12.9                         37
    ## 638           34          22.1                         59
    ## 639           31          20.9                         51
    ## 640           28          24.6                         46
    ## 641           55          13.2                        132
    ## 642           54          13.9                        142
    ## 643           41          17.4                         96
    ## 644           52          18.4                        106
    ## 645           42           8.8                        116
    ## 646           63          12.6                        148
    ## 647           43          10.1                        120
    ## 648           61          14.3                        135
    ## 649           67           9.1                        188
    ## 650          110          14.1                        203
    ## 651           61          10.4                        169
    ## 652           95          15.5                        186
    ## 653           13           6.0                         49
    ## 654           23           8.9                         62
    ## 655           18           7.5                         53
    ## 656           27          11.9                         57
    ## 657           39          19.5                         71
    ## 658           39          17.0                         69
    ## 659           19           8.4                         52
    ## 660           20           8.2                         66
    ## 661           64          15.9                        120
    ## 662           82          20.9                        150
    ## 663           27          10.5                         74
    ## 664           58          19.0                        117
    ## 665           13          12.7                         18
    ## 666           19          18.3                         25
    ## 667           20          11.0                         58
    ## 668           28          14.3                         51
    ## 669           54          15.3                        103
    ## 670           68          16.9                        137
    ## 671           50          15.9                        110
    ## 672           53          17.3                        102
    ## 673          110          20.9                        213
    ## 674          146          29.1                        240
    ## 675           69          19.1                        136
    ## 676           85          22.5                        162
    ## 677           78          13.5                        152
    ## 678           86          14.8                        167
    ## 679           59          11.0                        158
    ## 680           82          14.8                        171
    ## 681            5           9.6                         20
    ## 682            5          12.5                         30
    ## 683           26          19.4                         48
    ## 684           36          26.3                         58
    ## 685           14          25.0                         22
    ## 686           17          28.8                         26
    ## 687            7          10.6                         12
    ## 688            7          10.9                         16
    ## 689           10          20.4                         24
    ## 690            9          25.0                         14
    ## 691           19          14.1                         48
    ## 692           17          12.6                         36
    ## 693           24          32.4                         36
    ## 694           39          41.9                         55
    ## 695           13          18.3                         25
    ## 696           13          14.0                         30
    ## 697           24          23.5                         50
    ## 698           30          30.0                         46
    ## 699           15          21.7                         25
    ## 700           16          21.3                         23
    ## 701           14          16.9                         26
    ## 702           21          23.1                         32
    ## 703           11          21.2                         17
    ## 704            9          26.5                         20
    ## 705           11          13.6                         26
    ## 706           17          23.9                         30
    ## 707            7          14.3                         18
    ## 708           13          24.1                         18
    ## 709           76          12.3                        154
    ## 710           70          11.3                        144
    ## 711           42          12.2                         85
    ## 712           43          12.6                         86
    ## 713           12           9.4                         23
    ## 714           10           8.0                         34
    ## 715           11          18.6                         21
    ## 716           14          15.6                         30
    ## 717          139          19.4                        254
    ## 718          158          22.6                        277
    ## 719          103          27.3                        186
    ## 720          103          27.7                        164
    ## 721           27          15.8                         56
    ## 722           33          18.2                         58
    ## 723           18          17.8                         36
    ## 724           26          21.1                         48
    ## 725           19          10.5                         56
    ## 726           34          18.6                         63
    ## 727           16          10.8                         38
    ## 728           29          18.0                         60
    ## 729           51          17.6                         94
    ## 730           58          20.0                        107
    ## 731           55          22.6                        102
    ## 732           62          25.3                        109
    ## 733           40          17.1                         77
    ## 734           40          20.7                         73
    ## 735           24          15.7                         53
    ## 736           28          21.4                         44
    ## 737           43          14.1                        101
    ## 738           58          16.3                        107
    ## 739           30          13.0                         71
    ## 740           39          16.7                         79
    ## 741           15          18.5                         29
    ## 742           25          29.8                         45
    ## 743            9          16.7                         14
    ## 744           18          28.1                         30
    ## 745           54          18.3                        113
    ## 746           67          22.7                        113
    ## 747           39          27.3                         69
    ## 748           48          32.9                         78
    ## 749           10           7.1                         39
    ## 750           17          12.8                         39
    ## 751            9          18.4                         14
    ## 752           14          18.4                         22
    ## 753           16          19.5                         23
    ## 754           17          34.7                         27
    ## 755           16          28.6                         29
    ## 756           18          11.0                         43
    ## 757           18           9.5                         47
    ## 758            7           6.0                         29
    ## 759           16          14.3                         34
    ## 760           37          16.5                         76
    ## 761           37          14.2                         83
    ## 762           32          16.1                         62
    ## 763           39          18.4                         72
    ## 764           11           7.2                         32
    ## 765           11           6.4                         42
    ## 766            9           7.7                         25
    ## 767           16          13.7                         33
    ## 768           21           8.6                         51
    ## 769           23           8.4                         52
    ## 770           15          11.1                         31
    ## 771           26          14.9                         53
    ## 772           32          13.5                         75
    ## 773           45          17.2                         93
    ## 774           16          29.1                         27
    ## 775           24          34.3                         37
    ## 776           54          12.6                        109
    ## 777           51          11.9                        104
    ## 778           24           9.6                         71
    ## 779           39          17.0                         66
    ## 780           18           9.4                         48
    ## 781           31          12.1                         62
    ## 782           15          12.2                         35
    ## 783           24          18.2                         44
    ## 784           21          14.7                         48
    ## 785           18          11.8                         38
    ## 786           13          12.1                         40
    ## 787           14          14.1                         32
    ## 788           12           8.2                         28
    ## 789           19          11.9                         34
    ## 790            5           5.0                         22
    ## 791           15          11.7                         33
    ## 792           87          17.0                        167
    ## 793           90          19.1                        161
    ## 794           41          14.7                         70
    ## 795           39          12.8                         75
    ## 796          142          16.6                        276
    ## 797          165          17.8                        298
    ## 798          118          19.5                        222
    ## 799          177          25.0                        280
    ## 800           16          15.5                         34
    ## 801           14          13.6                         25
    ## 802            7          10.9                         18
    ## 803            8          14.8                         21
    ## 804           14           6.2                         43
    ## 805           21           8.0                         43
    ## 806           13           9.0                         26
    ## 807           21          14.2                         39
    ## 808           13           8.8                         26
    ## 809           14           8.3                         27
    ## 810            7           7.4                         15
    ## 811           14          11.4                         30
    ## 812           15           3.1                         92
    ## 813           20           4.3                         64
    ## 814           17           5.1                         54
    ## 815           18           5.1                         64
    ## 816            8           6.4                         31
    ## 817           13           9.3                         34
    ## 818           13           9.7                         30
    ## 819           20          13.2                         38
    ## 820           15          20.3                         25
    ## 821           15          14.7                         28
    ## 822           10          18.5                         22
    ## 823            9          17.6                         22
    ## 824            9          10.8                         30
    ## 825           11          12.6                         28
    ## 826           16          25.4                         22
    ## 827           17          33.3                         26
    ## 828           17          25.4                         29
    ## 829           17          23.3                         31
    ## 830            7          16.3                         14
    ## 831           14          29.8                         22
    ##     percent_overweight_or_obese grade_level number_healthy_weight
    ## 1                          22.8  ELEMENTARY                   181
    ## 2                          30.6  ELEMENTARY                   159
    ## 3                          42.7 MIDDLE/HIGH                    51
    ## 4                          40.7 MIDDLE/HIGH                    54
    ## 5                          47.5 MIDDLE/HIGH                    42
    ## 6                          41.1  ELEMENTARY                    96
    ## 7                          37.3  ELEMENTARY                   121
    ## 8                          44.4 MIDDLE/HIGH                    60
    ## 9                          32.9  ELEMENTARY                   321
    ## 10                         33.5  ELEMENTARY                   317
    ## 11                         39.2 MIDDLE/HIGH                   175
    ## 12                         39.2 MIDDLE/HIGH                   169
    ## 13                         29.5  ELEMENTARY                   353
    ## 14                         30.8  ELEMENTARY                   328
    ## 15                         30.1 MIDDLE/HIGH                   288
    ## 16                         33.5 MIDDLE/HIGH                   299
    ## 17                         31.7  ELEMENTARY                   316
    ## 18                         27.1  ELEMENTARY                   347
    ## 19                         31.8 MIDDLE/HIGH                   209
    ## 20                         34.6 MIDDLE/HIGH                   202
    ## 21                         29.3  ELEMENTARY                    79
    ## 22                         34.1  ELEMENTARY                    83
    ## 23                         51.7 MIDDLE/HIGH                    42
    ## 24                         45.2 MIDDLE/HIGH                    46
    ## 25                         34.2  ELEMENTARY                    75
    ## 26                         30.8  ELEMENTARY                   101
    ## 27                         46.7 MIDDLE/HIGH                    56
    ## 28                         39.8 MIDDLE/HIGH                    65
    ## 29                         36.4  ELEMENTARY                   275
    ## 30                         36.5  ELEMENTARY                   277
    ## 31                         43.9 MIDDLE/HIGH                   105
    ## 32                         44.9 MIDDLE/HIGH                   101
    ## 33                         32.4  ELEMENTARY                    50
    ## 34                         36.6  ELEMENTARY                    64
    ## 35                         43.8 MIDDLE/HIGH                    27
    ## 36                         40.4 MIDDLE/HIGH                    31
    ## 37                         36.0  ELEMENTARY                    98
    ## 38                         37.0  ELEMENTARY                    97
    ## 39                         33.9 MIDDLE/HIGH                    41
    ## 40                         39.6 MIDDLE/HIGH                    67
    ## 41                         28.3  ELEMENTARY                   185
    ## 42                         28.5  ELEMENTARY                   192
    ## 43                         31.8 MIDDLE/HIGH                    75
    ## 44                         46.3 MIDDLE/HIGH                    65
    ## 45                         34.0 MIDDLE/HIGH                    63
    ## 46                         33.1  ELEMENTARY                   105
    ## 47                         31.1 MIDDLE/HIGH                    62
    ## 48                         35.4  ELEMENTARY                    95
    ## 49                         29.3  ELEMENTARY                    53
    ## 50                         32.3  ELEMENTARY                    63
    ## 51                         50.0 MIDDLE/HIGH                    22
    ## 52                         48.8 MIDDLE/HIGH                    22
    ## 53                         51.0  ELEMENTARY                    24
    ## 54                         35.0  ELEMENTARY                    26
    ## 55                         28.5  ELEMENTARY                   168
    ## 56                         34.1  ELEMENTARY                   178
    ## 57                         49.2 MIDDLE/HIGH                    61
    ## 58                         41.0 MIDDLE/HIGH                    80
    ## 59                         29.5  ELEMENTARY                    62
    ## 60                         40.2  ELEMENTARY                    49
    ## 61                         44.6 MIDDLE/HIGH                    41
    ## 62                         47.1 MIDDLE/HIGH                    36
    ## 63                         35.0  ELEMENTARY                    67
    ## 64                         35.4  ELEMENTARY                    82
    ## 65                         52.3 MIDDLE/HIGH                    16
    ## 66                         48.6 MIDDLE/HIGH                    29
    ## 67                         34.9 MIDDLE/HIGH                    41
    ## 68                         26.7  ELEMENTARY                    55
    ## 69                         28.2  ELEMENTARY                    61
    ## 70                         44.1 MIDDLE/HIGH                    38
    ## 71                         36.8  ELEMENTARY                    74
    ## 72                         44.5  ELEMENTARY                    56
    ## 73                         42.6 MIDDLE/HIGH                    35
    ## 74                         51.8 MIDDLE/HIGH                    27
    ## 75                         43.4  ELEMENTARY                    43
    ## 76                         29.4  ELEMENTARY                    48
    ## 77                         48.9 MIDDLE/HIGH                    23
    ## 78                         22.8 MIDDLE/HIGH                    44
    ## 79                         37.0 MIDDLE/HIGH                    46
    ## 80                         40.3 MIDDLE/HIGH                    40
    ## 81                         41.9  ELEMENTARY                    54
    ## 82                         37.7  ELEMENTARY                    66
    ## 83                         26.4  ELEMENTARY                   128
    ## 84                         25.7  ELEMENTARY                   136
    ## 85                         49.2 MIDDLE/HIGH                    65
    ## 86                         50.4 MIDDLE/HIGH                    64
    ## 87                         41.7  ELEMENTARY                    70
    ## 88                         45.1  ELEMENTARY                    56
    ## 89                         40.8 MIDDLE/HIGH                    58
    ## 90                         42.9 MIDDLE/HIGH                    44
    ## 91                         52.2  ELEMENTARY                    32
    ## 92                         39.7  ELEMENTARY                    41
    ## 93                         51.6 MIDDLE/HIGH                    15
    ## 94                         25.6  ELEMENTARY                    29
    ## 95                         30.4  ELEMENTARY                    32
    ## 96                         41.4 MIDDLE/HIGH                    17
    ## 97                         37.8 MIDDLE/HIGH                    28
    ## 98                         39.5  ELEMENTARY                    46
    ## 99                         37.9  ELEMENTARY                    59
    ## 100                        35.0 MIDDLE/HIGH                    26
    ## 101                        45.9  ELEMENTARY                   333
    ## 102                        41.0  ELEMENTARY                   389
    ## 103                        45.7 MIDDLE/HIGH                   234
    ## 104                        42.2 MIDDLE/HIGH                   251
    ## 105                        33.3  ELEMENTARY                    70
    ## 106                        32.7  ELEMENTARY                    66
    ## 107                        34.2 MIDDLE/HIGH                    48
    ## 108                        37.3 MIDDLE/HIGH                    37
    ## 109                        44.4 MIDDLE/HIGH                    30
    ## 110                        45.1 MIDDLE/HIGH                    28
    ## 111                        40.4  ELEMENTARY                    62
    ## 112                        29.1  ELEMENTARY                    56
    ## 113                        37.5  ELEMENTARY                   125
    ## 114                        37.2  ELEMENTARY                   119
    ## 115                        47.0 MIDDLE/HIGH                    71
    ## 116                        46.9 MIDDLE/HIGH                    68
    ## 117                        35.1  ELEMENTARY                    85
    ## 118                        39.5  ELEMENTARY                    78
    ## 119                        49.3 MIDDLE/HIGH                    36
    ## 120                        39.7 MIDDLE/HIGH                    47
    ## 121                        36.6  ELEMENTARY                   156
    ## 122                        33.3  ELEMENTARY                   148
    ## 123                        46.9 MIDDLE/HIGH                    68
    ## 124                        46.7 MIDDLE/HIGH                    65
    ## 125                        41.1  ELEMENTARY                   122
    ## 126                        27.0  ELEMENTARY                   140
    ## 127                        51.7 MIDDLE/HIGH                    70
    ## 128                        33.8 MIDDLE/HIGH                    94
    ## 129                        43.0 MIDDLE/HIGH                    43
    ## 130                        34.2  ELEMENTARY                    77
    ## 131                        37.4  ELEMENTARY                    87
    ## 132                        47.5 MIDDLE/HIGH                    37
    ## 133                        38.9  ELEMENTARY                   129
    ## 134                        32.6  ELEMENTARY                   143
    ## 135                        27.7 MIDDLE/HIGH                    86
    ## 136                        30.4 MIDDLE/HIGH                    55
    ## 137                        27.6  ELEMENTARY                   447
    ## 138                        27.7  ELEMENTARY                   421
    ## 139                        30.8 MIDDLE/HIGH                   384
    ## 140                        36.7 MIDDLE/HIGH                   328
    ## 141                        28.2  ELEMENTARY                    89
    ## 142                        31.4  ELEMENTARY                   102
    ## 143                        43.8 MIDDLE/HIGH                    54
    ## 144                        30.2 MIDDLE/HIGH                    74
    ## 145                        28.1  ELEMENTARY                    95
    ## 146                        23.7  ELEMENTARY                   114
    ## 147                        35.8 MIDDLE/HIGH                    68
    ## 148                        31.5 MIDDLE/HIGH                    82
    ## 149                        45.3 MIDDLE/HIGH                    35
    ## 150                        27.3  ELEMENTARY                    48
    ## 151                        31.3  ELEMENTARY                    46
    ## 152                        25.6 MIDDLE/HIGH                    53
    ## 153                        26.8  ELEMENTARY                   165
    ## 154                        26.1  ELEMENTARY                   141
    ## 155                        29.0 MIDDLE/HIGH                    93
    ## 156                        45.2 MIDDLE/HIGH                    57
    ## 157                        29.6 MIDDLE/HIGH                   541
    ## 158                        25.5  ELEMENTARY                   680
    ## 159                        24.6  ELEMENTARY                   710
    ## 160                        27.4 MIDDLE/HIGH                   574
    ## 161                        27.3  ELEMENTARY                   245
    ## 162                        30.4  ELEMENTARY                   272
    ## 163                        33.6 MIDDLE/HIGH                   148
    ## 164                        41.9 MIDDLE/HIGH                   146
    ## 165                        32.9  ELEMENTARY                   166
    ## 166                        37.1  ELEMENTARY                   156
    ## 167                        48.1 MIDDLE/HIGH                    80
    ## 168                        38.5 MIDDLE/HIGH                   100
    ## 169                        41.9 MIDDLE/HIGH                    75
    ## 170                        24.7  ELEMENTARY                   146
    ## 171                        34.0  ELEMENTARY                   125
    ## 172                        35.4 MIDDLE/HIGH                    82
    ## 173                        30.2  ELEMENTARY                    95
    ## 174                        26.2  ELEMENTARY                   103
    ## 175                        27.1 MIDDLE/HIGH                    78
    ## 176                        30.8 MIDDLE/HIGH                    72
    ## 177                        23.6  ELEMENTARY                   193
    ## 178                        27.9  ELEMENTARY                   199
    ## 179                        37.6 MIDDLE/HIGH                   106
    ## 180                        34.0 MIDDLE/HIGH                   124
    ## 181                        29.8  ELEMENTARY                   163
    ## 182                        28.3  ELEMENTARY                   184
    ## 183                        27.7 MIDDLE/HIGH                   107
    ## 184                        22.6 MIDDLE/HIGH                    98
    ## 185                        34.1 MIDDLE/HIGH                   209
    ## 186                        27.6  ELEMENTARY                   381
    ## 187                        25.3  ELEMENTARY                   385
    ## 188                        32.3 MIDDLE/HIGH                   191
    ## 189                        35.5  ELEMENTARY                   131
    ## 190                        30.3  ELEMENTARY                   200
    ## 191                        38.0 MIDDLE/HIGH                    57
    ## 192                        38.2 MIDDLE/HIGH                    63
    ## 193                        26.1  ELEMENTARY                   361
    ## 194                        28.6  ELEMENTARY                   391
    ## 195                        33.5 MIDDLE/HIGH                   248
    ## 196                        31.1 MIDDLE/HIGH                   283
    ## 197                        37.4 MIDDLE/HIGH                    72
    ## 198                        29.8  ELEMENTARY                    92
    ## 199                        37.0  ELEMENTARY                    92
    ## 200                        34.9 MIDDLE/HIGH                    69
    ## 201                        24.4  ELEMENTARY                   321
    ## 202                        21.7  ELEMENTARY                   366
    ## 203                        23.3 MIDDLE/HIGH                   241
    ## 204                        26.9 MIDDLE/HIGH                   202
    ## 205                        31.2  ELEMENTARY                   621
    ## 206                        33.0  ELEMENTARY                   560
    ## 207                        42.6 MIDDLE/HIGH                   229
    ## 208                        39.7 MIDDLE/HIGH                   216
    ## 209                        55.7  ELEMENTARY                    43
    ## 210                        44.4  ELEMENTARY                    60
    ## 211                        43.1 MIDDLE/HIGH                    29
    ## 212                        50.0 MIDDLE/HIGH                    25
    ## 213                        43.9  ELEMENTARY                   105
    ## 214                        43.1  ELEMENTARY                   115
    ## 215                        54.5 MIDDLE/HIGH                    40
    ## 216                        57.0 MIDDLE/HIGH                    43
    ## 217                        32.4  ELEMENTARY                    71
    ## 218                        29.9  ELEMENTARY                    83
    ## 219                        46.9 MIDDLE/HIGH                    34
    ## 220                        34.4 MIDDLE/HIGH                    40
    ## 221                        36.1  ELEMENTARY                   155
    ## 222                        34.9  ELEMENTARY                   169
    ## 223                        50.4 MIDDLE/HIGH                    70
    ## 224                        39.3 MIDDLE/HIGH                   106
    ## 225                        43.5  ELEMENTARY                    61
    ## 226                        43.5  ELEMENTARY                    74
    ## 227                        47.0 MIDDLE/HIGH                    35
    ## 228                        48.4 MIDDLE/HIGH                    32
    ## 229                        33.3  ELEMENTARY                    79
    ## 230                        46.5  ELEMENTARY                    77
    ## 231                        46.9 MIDDLE/HIGH                    51
    ## 232                        42.1 MIDDLE/HIGH                    55
    ## 233                        31.2  ELEMENTARY                   150
    ## 234                        41.3  ELEMENTARY                   118
    ## 235                        40.2 MIDDLE/HIGH                    64
    ## 236                        40.0 MIDDLE/HIGH                    78
    ## 237                        30.5  ELEMENTARY                   338
    ## 238                        32.1  ELEMENTARY                   357
    ## 239                        38.4 MIDDLE/HIGH                   138
    ## 240                        40.4 MIDDLE/HIGH                   127
    ## 241                        35.1  ELEMENTARY                    48
    ## 242                        33.3  ELEMENTARY                    47
    ## 243                        45.8 MIDDLE/HIGH                    39
    ## 244                        46.0 MIDDLE/HIGH                    27
    ## 245                        27.9  ELEMENTARY                   188
    ## 246                        30.9  ELEMENTARY                   223
    ## 247                        46.4 MIDDLE/HIGH                    89
    ## 248                        45.2 MIDDLE/HIGH                   131
    ## 249                        33.9  ELEMENTARY                    41
    ## 250                        24.4  ELEMENTARY                    65
    ## 251                        37.3 MIDDLE/HIGH                    37
    ## 252                        34.9 MIDDLE/HIGH                    41
    ## 253                        38.8  ELEMENTARY                    85
    ## 254                        41.1  ELEMENTARY                    83
    ## 255                        53.4 MIDDLE/HIGH                    34
    ## 256                        48.5 MIDDLE/HIGH                    35
    ## 257                        30.3  ELEMENTARY                   106
    ## 258                        33.0  ELEMENTARY                   126
    ## 259                        45.0 MIDDLE/HIGH                    72
    ## 260                        43.1 MIDDLE/HIGH                    82
    ## 261                        21.8  ELEMENTARY                   239
    ## 262                        24.2  ELEMENTARY                   262
    ## 263                        31.6 MIDDLE/HIGH                   156
    ## 264                        25.0 MIDDLE/HIGH                   153
    ## 265                        34.8  ELEMENTARY                   252
    ## 266                        36.8  ELEMENTARY                   251
    ## 267                        41.8 MIDDLE/HIGH                   128
    ## 268                        44.2 MIDDLE/HIGH                   140
    ## 269                        33.1  ELEMENTARY                   218
    ## 270                        32.9  ELEMENTARY                   277
    ## 271                        28.2 MIDDLE/HIGH                   183
    ## 272                        33.1 MIDDLE/HIGH                   162
    ## 273                        33.5  ELEMENTARY                   272
    ## 274                        28.5  ELEMENTARY                   321
    ## 275                        34.7 MIDDLE/HIGH                   203
    ## 276                        38.1 MIDDLE/HIGH                   179
    ## 277                        33.1  ELEMENTARY                    81
    ## 278                        26.7  ELEMENTARY                    99
    ## 279                        45.8 MIDDLE/HIGH                    39
    ## 280                        41.1 MIDDLE/HIGH                    43
    ## 281                        29.9  ELEMENTARY                   272
    ## 282                        38.9  ELEMENTARY                   273
    ## 283                        38.6 MIDDLE/HIGH                   174
    ## 284                        34.8 MIDDLE/HIGH                   200
    ## 285                        28.4  ELEMENTARY                   374
    ## 286                        30.2  ELEMENTARY                   390
    ## 287                        35.8 MIDDLE/HIGH                   209
    ## 288                        43.9 MIDDLE/HIGH                   191
    ## 289                        27.5  ELEMENTARY                   210
    ## 290                        31.8  ELEMENTARY                   211
    ## 291                        35.5 MIDDLE/HIGH                   115
    ## 292                        42.3 MIDDLE/HIGH                   109
    ## 293                        26.4  ELEMENTARY                   664
    ## 294                        26.0  ELEMENTARY                   664
    ## 295                        32.8 MIDDLE/HIGH                   431
    ## 296                        33.2 MIDDLE/HIGH                   429
    ## 297                        40.2  ELEMENTARY                   223
    ## 298                        42.2  ELEMENTARY                   220
    ## 299                        52.2 MIDDLE/HIGH                    66
    ## 300                        39.8 MIDDLE/HIGH                    94
    ## 301                        43.7  ELEMENTARY                   451
    ## 302                        44.1  ELEMENTARY                   420
    ## 303                        43.7 MIDDLE/HIGH                   255
    ## 304                        43.9 MIDDLE/HIGH                   277
    ## 305                        24.8  ELEMENTARY                   425
    ## 306                        28.4  ELEMENTARY                   380
    ## 307                        34.4 MIDDLE/HIGH                   313
    ## 308                        37.3 MIDDLE/HIGH                   295
    ## 309                        21.7  ELEMENTARY                   511
    ## 310                        23.5  ELEMENTARY                   509
    ## 311                        29.5 MIDDLE/HIGH                   291
    ## 312                        32.6 MIDDLE/HIGH                   297
    ## 313                        28.1  ELEMENTARY                   115
    ## 314                        28.3  ELEMENTARY                   118
    ## 315                        34.5 MIDDLE/HIGH                    97
    ## 316                        35.3 MIDDLE/HIGH                   116
    ## 317                        42.7  ELEMENTARY                   421
    ## 318                        46.4  ELEMENTARY                   478
    ## 319                        42.1 MIDDLE/HIGH                   211
    ## 320                        40.2 MIDDLE/HIGH                   217
    ## 321                        23.3  ELEMENTARY                   163
    ## 322                        24.5  ELEMENTARY                   180
    ## 323                        33.4  ELEMENTARY                   189
    ## 324                        40.5  ELEMENTARY                   156
    ## 325                        41.0 MIDDLE/HIGH                    67
    ## 326                        53.5 MIDDLE/HIGH                    48
    ## 327                        24.0  ELEMENTARY                   263
    ## 328                        26.7  ELEMENTARY                   282
    ## 329                        21.4 MIDDLE/HIGH                   190
    ## 330                        26.3 MIDDLE/HIGH                   210
    ## 331                        28.6  ELEMENTARY                   158
    ## 332                        27.7  ELEMENTARY                   341
    ## 333                        39.4  ELEMENTARY                   106
    ## 334                        44.9  ELEMENTARY                    86
    ## 335                        46.8 MIDDLE/HIGH                    70
    ## 336                        34.5 MIDDLE/HIGH                    95
    ## 337                        21.8  ELEMENTARY                   124
    ## 338                        28.3  ELEMENTARY                   142
    ## 339                        35.2  ELEMENTARY                   116
    ## 340                        29.7  ELEMENTARY                   136
    ## 341                        17.1  ELEMENTARY                   197
    ## 342                        25.8  ELEMENTARY                   165
    ## 343                        26.3 MIDDLE/HIGH                   126
    ## 344                        24.0 MIDDLE/HIGH                   150
    ## 345                        23.1  ELEMENTARY                   248
    ## 346                        28.9  ELEMENTARY                   233
    ## 347                        19.4  ELEMENTARY                   570
    ## 348                        23.3  ELEMENTARY                   552
    ## 349                        20.0 MIDDLE/HIGH                   355
    ## 350                        27.4 MIDDLE/HIGH                   336
    ## 351                        25.8  ELEMENTARY                   168
    ## 352                        23.9  ELEMENTARY                   199
    ## 353                        20.3 MIDDLE/HIGH                   126
    ## 354                        20.4 MIDDLE/HIGH                   114
    ## 355                        22.3  ELEMENTARY                   310
    ## 356                        25.5  ELEMENTARY                   357
    ## 357                        29.4 MIDDLE/HIGH                   235
    ## 358                        28.3 MIDDLE/HIGH                   246
    ## 359                        30.5  ELEMENTARY                    60
    ## 360                        25.3 MIDDLE/HIGH                    56
    ## 361                        21.4 MIDDLE/HIGH                    56
    ## 362                        13.4  ELEMENTARY                   209
    ## 363                        15.2  ELEMENTARY                   213
    ## 364                        21.6 MIDDLE/HIGH                   158
    ## 365                        29.4 MIDDLE/HIGH                   185
    ## 366                        30.0  ELEMENTARY                   299
    ## 367                        34.8  ELEMENTARY                   318
    ## 368                        35.7 MIDDLE/HIGH                   182
    ## 369                        42.8 MIDDLE/HIGH                   163
    ## 370                        20.1  ELEMENTARY                   429
    ## 371                        22.7  ELEMENTARY                   469
    ## 372                        27.8 MIDDLE/HIGH                   342
    ## 373                        34.2 MIDDLE/HIGH                   285
    ## 374                        24.0  ELEMENTARY                   166
    ## 375                        27.6  ELEMENTARY                   175
    ## 376                        29.0 MIDDLE/HIGH                    88
    ## 377                        38.8 MIDDLE/HIGH                    90
    ## 378                        29.9  ELEMENTARY                   343
    ## 379                        31.4  ELEMENTARY                   320
    ## 380                        36.0 MIDDLE/HIGH                   160
    ## 381                        36.5 MIDDLE/HIGH                   197
    ## 382                        24.0  ELEMENTARY                   150
    ## 383                        29.9  ELEMENTARY                   133
    ## 384                        35.6 MIDDLE/HIGH                   116
    ## 385                        33.5 MIDDLE/HIGH                   118
    ## 386                        41.3  ELEMENTARY                    44
    ## 387                        36.5 MIDDLE/HIGH                    47
    ## 388                        39.1 MIDDLE/HIGH                    53
    ## 389                        42.6  ELEMENTARY                    27
    ## 390                        26.7  ELEMENTARY                    44
    ## 391                        39.7  ELEMENTARY                    73
    ## 392                        35.4  ELEMENTARY                    73
    ## 393                        46.4 MIDDLE/HIGH                    37
    ## 394                        40.0 MIDDLE/HIGH                    36
    ## 395                        37.1  ELEMENTARY                   465
    ## 396                        35.5  ELEMENTARY                   469
    ## 397                        45.2 MIDDLE/HIGH                   131
    ## 398                        39.6 MIDDLE/HIGH                   150
    ## 399                        30.1  ELEMENTARY                   180
    ## 400                        31.7  ELEMENTARY                   201
    ## 401                        24.6 MIDDLE/HIGH                   147
    ## 402                        36.8 MIDDLE/HIGH                   163
    ## 403                        29.6  ELEMENTARY                   229
    ## 404                        28.6  ELEMENTARY                   232
    ## 405                        29.1 MIDDLE/HIGH                   112
    ## 406                        28.2 MIDDLE/HIGH                   109
    ## 407                        24.1  ELEMENTARY                   168
    ## 408                        28.7  ELEMENTARY                   172
    ## 409                        30.5 MIDDLE/HIGH                   141
    ## 410                        28.3 MIDDLE/HIGH                   147
    ## 411                        41.0  ELEMENTARY                    36
    ## 412                        29.0  ELEMENTARY                    49
    ## 413                        32.5 MIDDLE/HIGH                    27
    ## 414                        46.5 MIDDLE/HIGH                    23
    ## 415                        37.3  ELEMENTARY                   101
    ## 416                        32.2  ELEMENTARY                    98
    ## 417                        48.7 MIDDLE/HIGH                    61
    ## 418                        35.4 MIDDLE/HIGH                    64
    ## 419                        30.4  ELEMENTARY                   359
    ## 420                        29.5  ELEMENTARY                   398
    ## 421                        36.3 MIDDLE/HIGH                   232
    ## 422                        31.4 MIDDLE/HIGH                   251
    ## 423                        22.9  ELEMENTARY                   256
    ## 424                        21.4  ELEMENTARY                   281
    ## 425                        23.5 MIDDLE/HIGH                   186
    ## 426                        28.9 MIDDLE/HIGH                   175
    ## 427                        39.2  ELEMENTARY                    48
    ## 428                        38.7  ELEMENTARY                    52
    ## 429                        50.8 MIDDLE/HIGH                    29
    ## 430                        35.6 MIDDLE/HIGH                    38
    ## 431                        36.4  ELEMENTARY                  1191
    ## 432                        36.7  ELEMENTARY                  1313
    ## 433                        50.1 MIDDLE/HIGH                   436
    ## 434                        42.0 MIDDLE/HIGH                   556
    ## 435                        31.1  ELEMENTARY                    62
    ## 436                        20.8  ELEMENTARY                    76
    ## 437                        56.8 MIDDLE/HIGH                    19
    ## 438                        43.5 MIDDLE/HIGH                    26
    ## 439                        38.0  ELEMENTARY                   139
    ## 440                        34.8  ELEMENTARY                   122
    ## 441                        48.3 MIDDLE/HIGH                    76
    ## 442                        44.4 MIDDLE/HIGH                    79
    ## 443                        35.5  ELEMENTARY                    65
    ## 444                        37.3  ELEMENTARY                    69
    ## 445                        57.3 MIDDLE/HIGH                    32
    ## 446                        27.7 MIDDLE/HIGH                    48
    ## 447                        45.0  ELEMENTARY                    44
    ## 448                        35.4  ELEMENTARY                    53
    ## 449                        43.5 MIDDLE/HIGH                    26
    ## 450                        46.3 MIDDLE/HIGH                    36
    ## 451                        22.1  ELEMENTARY                   424
    ## 452                        22.8  ELEMENTARY                   397
    ## 453                        17.8 MIDDLE/HIGH                   266
    ## 454                        31.2 MIDDLE/HIGH                   209
    ## 455                        28.5  ELEMENTARY                   223
    ## 456                        27.7  ELEMENTARY                   226
    ## 457                        35.9 MIDDLE/HIGH                   177
    ## 458                        38.0 MIDDLE/HIGH                   188
    ## 459                        26.7  ELEMENTARY                   150
    ## 460                        26.5  ELEMENTARY                   178
    ## 461                        33.3 MIDDLE/HIGH                   120
    ## 462                        41.9 MIDDLE/HIGH                   118
    ## 463                        23.3  ELEMENTARY                   406
    ## 464                        30.0  ELEMENTARY                   382
    ## 465                        34.0 MIDDLE/HIGH                   247
    ## 466                        34.0 MIDDLE/HIGH                   275
    ## 467                        42.3  ELEMENTARY                   750
    ## 468                        47.3  ELEMENTARY                   704
    ## 469                        48.8 MIDDLE/HIGH                   338
    ## 470                        45.2 MIDDLE/HIGH                   426
    ## 471                        23.0  ELEMENTARY                   432
    ## 472                        23.0  ELEMENTARY                   438
    ## 473                        28.4 MIDDLE/HIGH                   191
    ## 474                        31.9 MIDDLE/HIGH                   190
    ## 475                        39.5  ELEMENTARY                    46
    ## 476                        43.8  ELEMENTARY                    41
    ## 477                        44.4 MIDDLE/HIGH                    25
    ## 478                        36.5 MIDDLE/HIGH                    33
    ## 479                        38.8  ELEMENTARY                    74
    ## 480                        32.8  ELEMENTARY                    81
    ## 481                        51.3 MIDDLE/HIGH                    37
    ## 482                        56.0 MIDDLE/HIGH                    33
    ## 483                        43.9  ELEMENTARY                    83
    ## 484                        44.0  ELEMENTARY                    75
    ## 485                        57.3 MIDDLE/HIGH                    38
    ## 486                        48.5 MIDDLE/HIGH                    35
    ## 487                        35.9  ELEMENTARY                   215
    ## 488                        35.9  ELEMENTARY                   246
    ## 489                        39.4 MIDDLE/HIGH                   103
    ## 490                        47.2 MIDDLE/HIGH                   103
    ## 491                        47.0  ELEMENTARY                    61
    ## 492                        43.4  ELEMENTARY                    81
    ## 493                        49.0 MIDDLE/HIGH                    50
    ## 494                        47.2 MIDDLE/HIGH                    56
    ## 495                        40.7  ELEMENTARY                    48
    ## 496                        40.2  ELEMENTARY                    64
    ## 497                        50.0 MIDDLE/HIGH                    24
    ## 498                        53.8 MIDDLE/HIGH                    24
    ## 499                        35.2  ELEMENTARY                   120
    ## 500                        25.6  ELEMENTARY                   169
    ## 501                        45.2 MIDDLE/HIGH                    69
    ## 502                        42.5 MIDDLE/HIGH                    61
    ## 503                        33.5  ELEMENTARY                   113
    ## 504                        25.4  ELEMENTARY                   148
    ## 505                        29.1 MIDDLE/HIGH                    90
    ## 506                        36.0 MIDDLE/HIGH                    87
    ## 507                        26.3  ELEMENTARY                    56
    ## 508                        20.5  ELEMENTARY                    70
    ## 509                        25.6 MIDDLE/HIGH                    58
    ## 510                        33.8 MIDDLE/HIGH                    43
    ## 511                        33.2  ELEMENTARY                   199
    ## 512                        37.3  ELEMENTARY                   226
    ## 513                        40.0 MIDDLE/HIGH                   135
    ## 514                        46.3 MIDDLE/HIGH                   140
    ## 515                        18.6  ELEMENTARY                    48
    ## 516                        15.9 MIDDLE/HIGH                    58
    ## 517                        30.8  ELEMENTARY                   201
    ## 518                        37.1  ELEMENTARY                   195
    ## 519                        45.0 MIDDLE/HIGH                   117
    ## 520                        38.3 MIDDLE/HIGH                   153
    ## 521                        35.3  ELEMENTARY                   258
    ## 522                        32.7  ELEMENTARY                   263
    ## 523                        40.0 MIDDLE/HIGH                   174
    ## 524                        37.0 MIDDLE/HIGH                   194
    ## 525                        29.9  ELEMENTARY                   154
    ## 526                        28.7  ELEMENTARY                   174
    ## 527                        33.2 MIDDLE/HIGH                   118
    ## 528                        28.4 MIDDLE/HIGH                   138
    ## 529                        45.4  ELEMENTARY                   158
    ## 530                        30.3  ELEMENTARY                   255
    ## 531                        34.5 MIDDLE/HIGH                   142
    ## 532                        29.3 MIDDLE/HIGH                   171
    ## 533                        30.6  ELEMENTARY                   137
    ## 534                        31.3  ELEMENTARY                   134
    ## 535                        34.8 MIDDLE/HIGH                    86
    ## 536                        27.9 MIDDLE/HIGH                    87
    ## 537                        34.8  ELEMENTARY                   546
    ## 538                        37.4  ELEMENTARY                   572
    ## 539                        38.4 MIDDLE/HIGH                   366
    ## 540                        38.7 MIDDLE/HIGH                   380
    ## 541                        29.1  ELEMENTARY                   181
    ## 542                        23.4  ELEMENTARY                   202
    ## 543                        29.2 MIDDLE/HIGH                   163
    ## 544                        38.8 MIDDLE/HIGH                   147
    ## 545                        22.4  ELEMENTARY                   154
    ## 546                        26.7  ELEMENTARY                   166
    ## 547                        21.5 MIDDLE/HIGH                   144
    ## 548                        33.9 MIDDLE/HIGH                   104
    ## 549                        36.1  ELEMENTARY                    23
    ## 550                        43.4  ELEMENTARY                    60
    ## 551                        45.9  ELEMENTARY                    59
    ## 552                        60.0 MIDDLE/HIGH                    28
    ## 553                        51.9 MIDDLE/HIGH                    39
    ## 554                        31.4  ELEMENTARY                    78
    ## 555                        29.9  ELEMENTARY                    84
    ## 556                        41.7 MIDDLE/HIGH                    49
    ## 557                        35.9 MIDDLE/HIGH                    50
    ## 558                        32.8  ELEMENTARY                   220
    ## 559                        30.6  ELEMENTARY                   265
    ## 560                        41.7 MIDDLE/HIGH                   112
    ## 561                        30.0 MIDDLE/HIGH                   129
    ## 562                        21.6  ELEMENTARY                   485
    ## 563                        25.0  ELEMENTARY                   479
    ## 564                        29.6 MIDDLE/HIGH                   329
    ## 565                        26.6 MIDDLE/HIGH                   329
    ## 566                        19.4  ELEMENTARY                   277
    ## 567                        26.4  ELEMENTARY                   281
    ## 568                        23.4 MIDDLE/HIGH                   197
    ## 569                        24.4 MIDDLE/HIGH                   183
    ## 570                        36.4  ELEMENTARY                   152
    ## 571                        30.4  ELEMENTARY                   195
    ## 572                        46.7 MIDDLE/HIGH                    99
    ## 573                        48.3 MIDDLE/HIGH                   103
    ## 574                        23.3  ELEMENTARY                    66
    ## 575                        26.3  ELEMENTARY                    59
    ## 576                        46.9  ELEMENTARY                    52
    ## 577                        33.6  ELEMENTARY                    79
    ## 578                        35.3 MIDDLE/HIGH                    55
    ## 579                        54.0 MIDDLE/HIGH                    40
    ## 580                        39.9  ELEMENTARY                   104
    ## 581                        38.0  ELEMENTARY                   117
    ## 582                        41.9 MIDDLE/HIGH                    61
    ## 583                        48.6 MIDDLE/HIGH                    56
    ## 584                        34.8  ELEMENTARY                   101
    ## 585                        32.3  ELEMENTARY                   101
    ## 586                        42.9 MIDDLE/HIGH                    56
    ## 587                        45.1 MIDDLE/HIGH                    78
    ## 588                        35.7  ELEMENTARY                    27
    ## 589                        44.4  ELEMENTARY                    25
    ## 590                        47.8 MIDDLE/HIGH                    12
    ## 591                        45.2  ELEMENTARY                    17
    ## 592                        34.3 MIDDLE/HIGH                    23
    ## 593                        20.1  ELEMENTARY                   106
    ## 594                        24.7  ELEMENTARY                   122
    ## 595                        40.8 MIDDLE/HIGH                    71
    ## 596                        32.0 MIDDLE/HIGH                    76
    ## 597                        34.6  ELEMENTARY                   259
    ## 598                        33.7  ELEMENTARY                   262
    ## 599                        44.7 MIDDLE/HIGH                   139
    ## 600                        42.7 MIDDLE/HIGH                   145
    ## 601                        41.6  ELEMENTARY                   193
    ## 602                        39.5  ELEMENTARY                   228
    ## 603                        44.0 MIDDLE/HIGH                    92
    ## 604                        39.9 MIDDLE/HIGH                   108
    ## 605                        34.7  ELEMENTARY                   225
    ## 606                        34.0  ELEMENTARY                   224
    ## 607                        38.3 MIDDLE/HIGH                   106
    ## 608                        39.2 MIDDLE/HIGH                   124
    ## 609                        36.1  ELEMENTARY                   215
    ## 610                        33.2  ELEMENTARY                   197
    ## 611                        39.8 MIDDLE/HIGH                   121
    ## 612                        48.0 MIDDLE/HIGH                    99
    ## 613                        33.9  ELEMENTARY                   798
    ## 614                        35.4  ELEMENTARY                   819
    ## 615                        34.9 MIDDLE/HIGH                   503
    ## 616                        44.0 MIDDLE/HIGH                   438
    ## 617                        18.5  ELEMENTARY                    88
    ## 618                        22.9  ELEMENTARY                    74
    ## 619                        30.3 MIDDLE/HIGH                    46
    ## 620                        33.7 MIDDLE/HIGH                    57
    ## 621                        24.9  ELEMENTARY                   138
    ## 622                        27.5  ELEMENTARY                   175
    ## 623                        27.9 MIDDLE/HIGH                   119
    ## 624                        28.8 MIDDLE/HIGH                   124
    ## 625                        30.8  ELEMENTARY                   506
    ## 626                        33.5  ELEMENTARY                   481
    ## 627                        34.0 MIDDLE/HIGH                   361
    ## 628                        42.4 MIDDLE/HIGH                   363
    ## 629                        34.3  ELEMENTARY                   530
    ## 630                        38.1  ELEMENTARY                   528
    ## 631                        46.0 MIDDLE/HIGH                   277
    ## 632                        50.5 MIDDLE/HIGH                   245
    ## 633                        40.0  ELEMENTARY                   537
    ## 634                        44.8  ELEMENTARY                   554
    ## 635                        46.7 MIDDLE/HIGH                   359
    ## 636                        42.9 MIDDLE/HIGH                   367
    ## 637                        26.4  ELEMENTARY                    98
    ## 638                        38.3  ELEMENTARY                    95
    ## 639                        34.5 MIDDLE/HIGH                    89
    ## 640                        40.4 MIDDLE/HIGH                    58
    ## 641                        31.6  ELEMENTARY                   276
    ## 642                        36.5  ELEMENTARY                   236
    ## 643                        40.7 MIDDLE/HIGH                   132
    ## 644                        37.6 MIDDLE/HIGH                   167
    ## 645                        24.2  ELEMENTARY                   354
    ## 646                        29.5  ELEMENTARY                   324
    ## 647                        28.2 MIDDLE/HIGH                   295
    ## 648                        31.5 MIDDLE/HIGH                   277
    ## 649                        25.6  ELEMENTARY                   520
    ## 650                        26.0  ELEMENTARY                   534
    ## 651                        28.9 MIDDLE/HIGH                   404
    ## 652                        30.4 MIDDLE/HIGH                   394
    ## 653                        22.7  ELEMENTARY                   161
    ## 654                        24.1  ELEMENTARY                   188
    ## 655                        22.1 MIDDLE/HIGH                   181
    ## 656                        25.2 MIDDLE/HIGH                   155
    ## 657                        35.5  ELEMENTARY                   122
    ## 658                        30.0  ELEMENTARY                   156
    ## 659                        23.1 MIDDLE/HIGH                   168
    ## 660                        26.9 MIDDLE/HIGH                   172
    ## 661                        29.8  ELEMENTARY                   275
    ## 662                        38.3  ELEMENTARY                   226
    ## 663                        28.7 MIDDLE/HIGH                   177
    ## 664                        38.2 MIDDLE/HIGH                   173
    ## 665                        17.6  ELEMENTARY                    51
    ## 666                        24.0  ELEMENTARY                    48
    ## 667                        32.0 MIDDLE/HIGH                   118
    ## 668                        26.0 MIDDLE/HIGH                   137
    ## 669                        29.2  ELEMENTARY                   241
    ## 670                        34.0  ELEMENTARY                   259
    ## 671                        34.9 MIDDLE/HIGH                   200
    ## 672                        33.3 MIDDLE/HIGH                   198
    ## 673                        40.5  ELEMENTARY                   306
    ## 674                        47.8  ELEMENTARY                   252
    ## 675                        37.7 MIDDLE/HIGH                   225
    ## 676                        43.0 MIDDLE/HIGH                   205
    ## 677                        26.3  ELEMENTARY                   405
    ## 678                        28.8  ELEMENTARY                   398
    ## 679                        29.4 MIDDLE/HIGH                   361
    ## 680                        30.8 MIDDLE/HIGH                   364
    ## 681                        38.5  ELEMENTARY                    32
    ## 682                        75.0  ELEMENTARY                    10
    ## 683                        35.8  ELEMENTARY                    86
    ## 684                        42.3  ELEMENTARY                    79
    ## 685                        39.3 MIDDLE/HIGH                    34
    ## 686                        44.1 MIDDLE/HIGH                    33
    ## 687                        18.2  ELEMENTARY                    49
    ## 688                        25.0  ELEMENTARY                    48
    ## 689                        49.0 MIDDLE/HIGH                    25
    ## 690                        38.9 MIDDLE/HIGH                    22
    ## 691                        35.6  ELEMENTARY                    87
    ## 692                        26.7  ELEMENTARY                    99
    ## 693                        48.6 MIDDLE/HIGH                    38
    ## 694                        59.1 MIDDLE/HIGH                    38
    ## 695                        35.2  ELEMENTARY                    41
    ## 696                        32.3  ELEMENTARY                    63
    ## 697                        49.0 MIDDLE/HIGH                    52
    ## 698                        46.0 MIDDLE/HIGH                    54
    ## 699                        36.2  ELEMENTARY                    44
    ## 700                        30.7  ELEMENTARY                    52
    ## 701                        31.3  ELEMENTARY                    57
    ## 702                        35.2  ELEMENTARY                    59
    ## 703                        32.7 MIDDLE/HIGH                    35
    ## 704                        58.8 MIDDLE/HIGH                    14
    ## 705                        32.1  ELEMENTARY                    55
    ## 706                        42.3  ELEMENTARY                    41
    ## 707                        36.7 MIDDLE/HIGH                    31
    ## 708                        33.3 MIDDLE/HIGH                    36
    ## 709                        24.9  ELEMENTARY                   450
    ## 710                        23.3  ELEMENTARY                   455
    ## 711                        24.6 MIDDLE/HIGH                   251
    ## 712                        25.2 MIDDLE/HIGH                   244
    ## 713                        18.1  ELEMENTARY                   104
    ## 714                        27.2  ELEMENTARY                    85
    ## 715                        35.6 MIDDLE/HIGH                    38
    ## 716                        33.3 MIDDLE/HIGH                    55
    ## 717                        35.5  ELEMENTARY                   445
    ## 718                        39.7  ELEMENTARY                   403
    ## 719                        49.3 MIDDLE/HIGH                   186
    ## 720                        44.1 MIDDLE/HIGH                   208
    ## 721                        32.7  ELEMENTARY                   115
    ## 722                        32.0  ELEMENTARY                   118
    ## 723                        35.6 MIDDLE/HIGH                    65
    ## 724                        39.0 MIDDLE/HIGH                    68
    ## 725                        30.9  ELEMENTARY                   125
    ## 726                        34.4  ELEMENTARY                   115
    ## 727                        25.7 MIDDLE/HIGH                   110
    ## 728                        37.3 MIDDLE/HIGH                   101
    ## 729                        32.5  ELEMENTARY                   183
    ## 730                        36.9  ELEMENTARY                   183
    ## 731                        42.0 MIDDLE/HIGH                   135
    ## 732                        44.5 MIDDLE/HIGH                   129
    ## 733                        32.9  ELEMENTARY                   157
    ## 734                        37.8  ELEMENTARY                   114
    ## 735                        34.6 MIDDLE/HIGH                    94
    ## 736                        33.6 MIDDLE/HIGH                    87
    ## 737                        33.1  ELEMENTARY                   192
    ## 738                        30.1  ELEMENTARY                   232
    ## 739                        30.9 MIDDLE/HIGH                   152
    ## 740                        33.9 MIDDLE/HIGH                   147
    ## 741                        35.8  ELEMENTARY                    47
    ## 742                        53.6  ELEMENTARY                    39
    ## 743                        25.9 MIDDLE/HIGH                    40
    ## 744                        46.9 MIDDLE/HIGH                    34
    ## 745                        38.3  ELEMENTARY                   171
    ## 746                        38.3  ELEMENTARY                   175
    ## 747                        48.3 MIDDLE/HIGH                    74
    ## 748                        53.4 MIDDLE/HIGH                    63
    ## 749                        27.9  ELEMENTARY                    96
    ## 750                        29.3  ELEMENTARY                    88
    ## 751                        28.6 MIDDLE/HIGH                    35
    ## 752                        28.9  ELEMENTARY                    54
    ## 753                        28.0  ELEMENTARY                    59
    ## 754                        55.1 MIDDLE/HIGH                    22
    ## 755                        51.8 MIDDLE/HIGH                    27
    ## 756                        26.2  ELEMENTARY                   121
    ## 757                        24.7  ELEMENTARY                   143
    ## 758                        25.0 MIDDLE/HIGH                    87
    ## 759                        30.4 MIDDLE/HIGH                    78
    ## 760                        33.9  ELEMENTARY                   148
    ## 761                        31.8  ELEMENTARY                   164
    ## 762                        31.2 MIDDLE/HIGH                   132
    ## 763                        34.0 MIDDLE/HIGH                   134
    ## 764                        21.1  ELEMENTARY                   120
    ## 765                        24.6  ELEMENTARY                   129
    ## 766                        21.4 MIDDLE/HIGH                    92
    ## 767                        28.2 MIDDLE/HIGH                    84
    ## 768                        21.0  ELEMENTARY                   171
    ## 769                        18.9  ELEMENTARY                   208
    ## 770                        23.0 MIDDLE/HIGH                    98
    ## 771                        30.5 MIDDLE/HIGH                   115
    ## 772                        31.6  ELEMENTARY                   157
    ## 773                        35.5  ELEMENTARY                   160
    ## 774                        49.1 MIDDLE/HIGH                    28
    ## 775                        52.9 MIDDLE/HIGH                    33
    ## 776                        25.3  ELEMENTARY                   296
    ## 777                        24.2  ELEMENTARY                   307
    ## 778                        28.3 MIDDLE/HIGH                   172
    ## 779                        28.7 MIDDLE/HIGH                   157
    ## 780                        25.1  ELEMENTARY                   137
    ## 781                        24.2  ELEMENTARY                   186
    ## 782                        28.5 MIDDLE/HIGH                    88
    ## 783                        33.3 MIDDLE/HIGH                    88
    ## 784                        33.6  ELEMENTARY                    95
    ## 785                        25.0  ELEMENTARY                   114
    ## 786                        37.4 MIDDLE/HIGH                    67
    ## 787                        32.3 MIDDLE/HIGH                    59
    ## 788                        19.2  ELEMENTARY                   111
    ## 789                        21.3  ELEMENTARY                   114
    ## 790                        22.0 MIDDLE/HIGH                    78
    ## 791                        25.8 MIDDLE/HIGH                    90
    ## 792                        32.7  ELEMENTARY                   308
    ## 793                        34.2  ELEMENTARY                   281
    ## 794                        25.1 MIDDLE/HIGH                   203
    ## 795                        24.6 MIDDLE/HIGH                   221
    ## 796                        32.3  ELEMENTARY                   535
    ## 797                        32.1  ELEMENTARY                   591
    ## 798                        36.6 MIDDLE/HIGH                   372
    ## 799                        39.5 MIDDLE/HIGH                   408
    ## 800                        33.0  ELEMENTARY                    69
    ## 801                        24.3  ELEMENTARY                    78
    ## 802                        28.1 MIDDLE/HIGH                    46
    ## 803                        38.9 MIDDLE/HIGH                    33
    ## 804                        19.1  ELEMENTARY                   182
    ## 805                        16.5  ELEMENTARY                   213
    ## 806                        17.9 MIDDLE/HIGH                   119
    ## 807                        26.4 MIDDLE/HIGH                   103
    ## 808                        17.6  ELEMENTARY                   111
    ## 809                        16.0  ELEMENTARY                   142
    ## 810                        16.0 MIDDLE/HIGH                    79
    ## 811                        24.4 MIDDLE/HIGH                    93
    ## 812                        19.0  ELEMENTARY                   367
    ## 813                        13.6  ELEMENTARY                   383
    ## 814                        16.2 MIDDLE/HIGH                   262
    ## 815                        18.3 MIDDLE/HIGH                   277
    ## 816                        24.8  ELEMENTARY                    89
    ## 817                        24.3  ELEMENTARY                    96
    ## 818                        22.4 MIDDLE/HIGH                   104
    ## 819                        25.0 MIDDLE/HIGH                   107
    ## 820                        33.8  ELEMENTARY                    49
    ## 821                        27.5  ELEMENTARY                    74
    ## 822                        40.7 MIDDLE/HIGH                    32
    ## 823                        43.1 MIDDLE/HIGH                    29
    ## 824                        36.1  ELEMENTARY                    53
    ## 825                        32.2  ELEMENTARY                    59
    ## 826                        34.9 MIDDLE/HIGH                    41
    ## 827                        51.0 MIDDLE/HIGH                    25
    ## 828                        43.3  ELEMENTARY                    38
    ## 829                        42.5  ELEMENTARY                    42
    ## 830                        32.6 MIDDLE/HIGH                    29
    ## 831                        46.8 MIDDLE/HIGH                    25
    ##     percent_healthy_weight    sex year num_asian num_black num_hisp num_am_ind
    ## 1                    0.736 FEMALE 2019        14        74      152          1
    ## 2                    0.631   MALE 2019        14        74      152          1
    ## 3                    0.573 FEMALE 2019        14        74      152          1
    ## 4                    0.593   MALE 2019        14        74      152          1
    ## 5                    0.525   MALE 2019        33       208      174          7
    ## 6                    0.533 FEMALE 2019        33       208      174          7
    ## 7                    0.550   MALE 2019        33       208      174          7
    ## 8                    0.556 FEMALE 2019        33       208      174          7
    ## 9                    0.633 FEMALE 2019       498       395      425          6
    ## 10                   0.647   MALE 2019       498       395      425          6
    ## 11                   0.557 FEMALE 2019       498       395      425          6
    ## 12                   0.577   MALE 2019       498       395      425          6
    ## 13                   0.662 FEMALE 2019      1121       382      334         16
    ## 14                   0.643   MALE 2019      1121       382      334         16
    ## 15                   0.673 FEMALE 2019      1121       382      334         16
    ## 16                   0.631   MALE 2019      1121       382      334         16
    ## 17                   0.646   MALE 2019       701       201      187          7
    ## 18                   0.691 FEMALE 2019       701       201      187          7
    ## 19                   0.657 FEMALE 2019       701       201      187          7
    ## 20                   0.623   MALE 2019       701       201      187          7
    ## 21                   0.642 FEMALE 2019         6        15       21          4
    ## 22                   0.659   MALE 2019         6        15       21          4
    ## 23                   0.483 FEMALE 2019         6        15       21          4
    ## 24                   0.548   MALE 2019         6        15       21          4
    ## 25                   0.658 FEMALE 2019         7        16       28          3
    ## 26                   0.692   MALE 2019         7        16       28          3
    ## 27                   0.533 FEMALE 2019         7        16       28          3
    ## 28                   0.551   MALE 2019         7        16       28          3
    ## 29                   0.603 FEMALE 2019       153      1491      778          9
    ## 30                   0.592   MALE 2019       153      1491      778          9
    ## 31                   0.561 FEMALE 2019       153      1491      778          9
    ## 32                   0.515   MALE 2019       153      1491      778          9
    ## 33                   0.676 FEMALE 2019         1         7        3          2
    ## 34                   0.634   MALE 2019         1         7        3          2
    ## 35                   0.563 FEMALE 2019         1         7        3          2
    ## 36                   0.596   MALE 2019         1         7        3          2
    ## 37                   0.609   MALE 2019        18        29       42          2
    ## 38                   0.630 FEMALE 2019        18        29       42          2
    ## 39                   0.661 FEMALE 2019        18        29       42          2
    ## 40                   0.604   MALE 2019        18        29       42          2
    ## 41                   0.698 FEMALE 2019        24        27       38          6
    ## 42                   0.676   MALE 2019        24        27       38          6
    ## 43                   0.682 FEMALE 2019        24        27       38          6
    ## 44                   0.537   MALE 2019        24        27       38          6
    ## 45                   0.594   MALE 2019        11        17       19          4
    ## 46                   0.669   MALE 2019        11        17       19          4
    ## 47                   0.689 FEMALE 2019        11        17       19          4
    ## 48                   0.646 FEMALE 2019        11        17       19          4
    ## 49                   0.707 FEMALE 2019         5         2       11         11
    ## 50                   0.677   MALE 2019         5         2       11         11
    ## 51                   0.500 FEMALE 2019         5         2       11         11
    ## 52                   0.512   MALE 2019         5         2       11         11
    ## 53                   0.490 FEMALE 2019         4         4        7          1
    ## 54                   0.650   MALE 2019         4         4        7          1
    ## 55                   0.694 FEMALE 2019        53        77       66         11
    ## 56                   0.638   MALE 2019        53        77       66         11
    ## 57                   0.469 FEMALE 2019        53        77       66         11
    ## 58                   0.556   MALE 2019        53        77       66         11
    ## 59                   0.705   MALE 2019         2         5       18         43
    ## 60                   0.598 FEMALE 2019         2         5       18         43
    ## 61                   0.554 FEMALE 2019         2         5       18         43
    ## 62                   0.529   MALE 2019         2         5       18         43
    ## 63                   0.650 FEMALE 2019         2         8       78        429
    ## 64                   0.646   MALE 2019         2         8       78        429
    ## 65                   0.364 FEMALE 2019         2         8       78        429
    ## 66                   0.392   MALE 2019         2         8       78        429
    ## 67                   0.651 FEMALE 2019         4         7       19          6
    ## 68                   0.733 FEMALE 2019         4         7       19          6
    ## 69                   0.718   MALE 2019         4         7       19          6
    ## 70                   0.559   MALE 2019         4         7       19          6
    ## 71                   0.632 FEMALE 2019         1         3       15          1
    ## 72                   0.509   MALE 2019         1         3       15          1
    ## 73                   0.574 FEMALE 2019         1         3       15          1
    ## 74                   0.482   MALE 2019         1         3       15          1
    ## 75                   0.566 FEMALE 2019         4         4       18          5
    ## 76                   0.706   MALE 2019         4         4       18          5
    ## 77                   0.511 FEMALE 2019         4         4       18          5
    ## 78                   0.772   MALE 2019         4         4       18          5
    ## 79                   0.630   MALE 2019         6         8       18          1
    ## 80                   0.597 FEMALE 2019         6         8       18          1
    ## 81                   0.581 FEMALE 2019         6         8       18          1
    ## 82                   0.623   MALE 2019         6         8       18          1
    ## 83                   0.736 FEMALE 2019        10        91     1192          6
    ## 84                   0.743   MALE 2019        10        91     1192          6
    ## 85                   0.508 FEMALE 2019        10        91     1192          6
    ## 86                   0.496   MALE 2019        10        91     1192          6
    ## 87                   0.583 FEMALE 2019         5         6      100        128
    ## 88                   0.549   MALE 2019         5         6      100        128
    ## 89                   0.592 FEMALE 2019         5         6      100        128
    ## 90                   0.571   MALE 2019         5         6      100        128
    ## 91                   0.478 FEMALE 2019         1         3       73          3
    ## 92                   0.603   MALE 2019         1         3       73          3
    ## 93                   0.484   MALE 2019         1         3       73          3
    ## 94                   0.744 FEMALE 2019         1         3        9          3
    ## 95                   0.696   MALE 2019         1         3        9          3
    ## 96                   0.586 FEMALE 2019         1         3        9          3
    ## 97                   0.622 FEMALE 2019         5        11       43          4
    ## 98                   0.535 FEMALE 2019         5        11       43          4
    ## 99                   0.621   MALE 2019         5        11       43          4
    ## 100                  0.650   MALE 2019         5        11       43          4
    ## 101                  0.520 FEMALE 2019        22       777      203          7
    ## 102                  0.561   MALE 2019        22       777      203          7
    ## 103                  0.543 FEMALE 2019        22       777      203          7
    ## 104                  0.563   MALE 2019        22       777      203          7
    ## 105                  0.667 FEMALE 2019         8        10       24          7
    ## 106                  0.673   MALE 2019         8        10       24          7
    ## 107                  0.658 FEMALE 2019         8        10       24          7
    ## 108                  0.627   MALE 2019         8        10       24          7
    ## 109                  0.556   MALE 2019         3         6        9          7
    ## 110                  0.549 FEMALE 2019         3         6        9          7
    ## 111                  0.596 FEMALE 2019         3         6        9          7
    ## 112                  0.709   MALE 2019         3         6        9          7
    ## 113                  0.625 FEMALE 2019        24        47       53          3
    ## 114                  0.598   MALE 2019        24        47       53          3
    ## 115                  0.530 FEMALE 2019        24        47       53          3
    ## 116                  0.531   MALE 2019        24        47       53          3
    ## 117                  0.649 FEMALE 2019         1        30        4          1
    ## 118                  0.605   MALE 2019         1        30        4          1
    ## 119                  0.507 FEMALE 2019         1        30        4          1
    ## 120                  0.603   MALE 2019         1        30        4          1
    ## 121                  0.634   MALE 2019        15        29       52          8
    ## 122                  0.667 FEMALE 2019        15        29       52          8
    ## 123                  0.531 FEMALE 2019        15        29       52          8
    ## 124                  0.533   MALE 2019        15        29       52          8
    ## 125                  0.557 FEMALE 2019        21        20       18          7
    ## 126                  0.686   MALE 2019        21        20       18          7
    ## 127                  0.483 FEMALE 2019        21        20       18          7
    ## 128                  0.623   MALE 2019        21        20       18          7
    ## 129                  0.500 FEMALE 2019         5        25      136          2
    ## 130                  0.658 FEMALE 2019         5        25      136          2
    ## 131                  0.626   MALE 2019         5        25      136          2
    ## 132                  0.463   MALE 2019         5        25      136          2
    ## 133                  0.571 FEMALE 2019       224       329      271          2
    ## 134                  0.622   MALE 2019       224       329      271          2
    ## 135                  0.723 FEMALE 2019       224       329      271          2
    ## 136                  0.696   MALE 2019       224       329      271          2
    ## 137                  0.702   MALE 2019       282       587     1474          4
    ## 138                  0.698 FEMALE 2019       282       587     1474          4
    ## 139                  0.669 FEMALE 2019       282       587     1474          4
    ## 140                  0.596   MALE 2019       282       587     1474          4
    ## 141                  0.718 FEMALE 2019       176       167      200          1
    ## 142                  0.654   MALE 2019       176       167      200          1
    ## 143                  0.563 FEMALE 2019       176       167      200          1
    ## 144                  0.698   MALE 2019       176       167      200          1
    ## 145                  0.683 FEMALE 2019        44        38      188          6
    ## 146                  0.731   MALE 2019        44        38      188          6
    ## 147                  0.642 FEMALE 2019        44        38      188          6
    ## 148                  0.646   MALE 2019        44        38      188          6
    ## 149                  0.547 FEMALE 2019        27         9       84          1
    ## 150                  0.727 FEMALE 2019        27         9       84          1
    ## 151                  0.687   MALE 2019        27         9       84          1
    ## 152                  0.679   MALE 2019        27         9       84          1
    ## 153                  0.702 FEMALE 2019         8        12       20          8
    ## 154                  0.709   MALE 2019         8        12       20          8
    ## 155                  0.710 FEMALE 2019         8        12       20          8
    ## 156                  0.548   MALE 2019         8        12       20          8
    ## 157                  0.675   MALE 2019      1422       402      409         22
    ## 158                  0.712 FEMALE 2019      1422       402      409         22
    ## 159                  0.728   MALE 2019      1422       402      409         22
    ## 160                  0.702 FEMALE 2019      1422       402      409         22
    ## 161                  0.690 FEMALE 2019       262       666      171          6
    ## 162                  0.657   MALE 2019       262       666      171          6
    ## 163                  0.664 FEMALE 2019       262       666      171          6
    ## 164                  0.562   MALE 2019       262       666      171          6
    ## 165                  0.643 FEMALE 2019       157       843      201          7
    ## 166                  0.629   MALE 2019       157       843      201          7
    ## 167                  0.519 FEMALE 2019       157       843      201          7
    ## 168                  0.575   MALE 2019       157       843      201          7
    ## 169                  0.581   MALE 2019        10         6       37         13
    ## 170                  0.753 FEMALE 2019        10         6       37         13
    ## 171                  0.635   MALE 2019        10         6       37         13
    ## 172                  0.646 FEMALE 2019        10         6       37         13
    ## 173                  0.638   MALE 2019        12        17       82        324
    ## 174                  0.691 FEMALE 2019        12        17       82        324
    ## 175                  0.729 FEMALE 2019        12        17       82        324
    ## 176                  0.692   MALE 2019        12        17       82        324
    ## 177                  0.702 FEMALE 2019       102        48      142         11
    ## 178                  0.686   MALE 2019       102        48      142         11
    ## 179                  0.624 FEMALE 2019       102        48      142         11
    ## 180                  0.620   MALE 2019       102        48      142         11
    ## 181                  0.674 FEMALE 2019        29        35       90          9
    ## 182                  0.667   MALE 2019        29        35       90          9
    ## 183                  0.690 FEMALE 2019        29        35       90          9
    ## 184                  0.737   MALE 2019        29        35       90          9
    ## 185                  0.659 FEMALE 2019        58        52      250         13
    ## 186                  0.683 FEMALE 2019        58        52      250         13
    ## 187                  0.717   MALE 2019        58        52      250         13
    ## 188                  0.637   MALE 2019        58        52      250         13
    ## 189                  0.604 FEMALE 2019        52       289      269          9
    ## 190                  0.645   MALE 2019        52       289      269          9
    ## 191                  0.620 FEMALE 2019        52       289      269          9
    ## 192                  0.618   MALE 2019        52       289      269          9
    ## 193                  0.704 FEMALE 2019       102        66      181         12
    ## 194                  0.691   MALE 2019       102        66      181         12
    ## 195                  0.639 FEMALE 2019       102        66      181         12
    ## 196                  0.657   MALE 2019       102        66      181         12
    ## 197                  0.626   MALE 2019         5         7       15        167
    ## 198                  0.702 FEMALE 2019         5         7       15        167
    ## 199                  0.630   MALE 2019         5         7       15        167
    ## 200                  0.651 FEMALE 2019         5         7       15        167
    ## 201                  0.725 FEMALE 2019        80        43      112          1
    ## 202                  0.758   MALE 2019        80        43      112          1
    ## 203                  0.748 FEMALE 2019        80        43      112          1
    ## 204                  0.680   MALE 2019        80        43      112          1
    ## 205                  0.655 FEMALE 2019       222       511      620         35
    ## 206                  0.626   MALE 2019       222       511      620         35
    ## 207                  0.557 FEMALE 2019       222       511      620         35
    ## 208                  0.561   MALE 2019       222       511      620         35
    ## 209                  0.443 FEMALE 2019         4         1       14          3
    ## 210                  0.556   MALE 2019         4         1       14          3
    ## 211                  0.569 FEMALE 2019         4         1       14          3
    ## 212                  0.500   MALE 2019         4         1       14          3
    ## 213                  0.561 FEMALE 2019         1         1       34        869
    ## 214                  0.569   MALE 2019         1         1       34        869
    ## 215                  0.455 FEMALE 2019         1         1       34        869
    ## 216                  0.430   MALE 2019         1         1       34        869
    ## 217                  0.676 FEMALE 2019        17        12       61          3
    ## 218                  0.654   MALE 2019        17        12       61          3
    ## 219                  0.531 FEMALE 2019        17        12       61          3
    ## 220                  0.656   MALE 2019        17        12       61          3
    ## 221                  0.615 FEMALE 2019        22       298      213         26
    ## 222                  0.628   MALE 2019        22       298      213         26
    ## 223                  0.496 FEMALE 2019        22       298      213         26
    ## 224                  0.579   MALE 2019        22       298      213         26
    ## 225                  0.565 FEMALE 2019         9        22      104          3
    ## 226                  0.565   MALE 2019         9        22      104          3
    ## 227                  0.530 FEMALE 2019         9        22      104          3
    ## 228                  0.516   MALE 2019         9        22      104          3
    ## 229                  0.612 FEMALE 2019         9        35       96          1
    ## 230                  0.535   MALE 2019         9        35       96          1
    ## 231                  0.531 FEMALE 2019         9        35       96          1
    ## 232                  0.579   MALE 2019         9        35       96          1
    ## 233                  0.688 FEMALE 2019        11        14       34          4
    ## 234                  0.587   MALE 2019        11        14       34          4
    ## 235                  0.598 FEMALE 2019        11        14       34          4
    ## 236                  0.600   MALE 2019        11        14       34          4
    ## 237                  0.656 FEMALE 2019       101       280      600         16
    ## 238                  0.630   MALE 2019       101       280      600         16
    ## 239                  0.595 FEMALE 2019       101       280      600         16
    ## 240                  0.557   MALE 2019       101       280      600         16
    ## 241                  0.649 FEMALE 2019         3         6       29          5
    ## 242                  0.603   MALE 2019         3         6       29          5
    ## 243                  0.542 FEMALE 2019         3         6       29          5
    ## 244                  0.540   MALE 2019         3         6       29          5
    ## 245                  0.681 FEMALE 2019        49       155      357         14
    ## 246                  0.650   MALE 2019        49       155      357         14
    ## 247                  0.536 FEMALE 2019        49       155      357         14
    ## 248                  0.548   MALE 2019        49       155      357         14
    ## 249                  0.661 FEMALE 2019        17        10       89          2
    ## 250                  0.756   MALE 2019        17        10       89          2
    ## 251                  0.627 FEMALE 2019        17        10       89          2
    ## 252                  0.651   MALE 2019        17        10       89          2
    ## 253                  0.578 FEMALE 2019         9        13       40          7
    ## 254                  0.589   MALE 2019         9        13       40          7
    ## 255                  0.466 FEMALE 2019         9        13       40          7
    ## 256                  0.515   MALE 2019         9        13       40          7
    ## 257                  0.697 FEMALE 2019        16        11       59          3
    ## 258                  0.670   MALE 2019        16        11       59          3
    ## 259                  0.550 FEMALE 2019        16        11       59          3
    ## 260                  0.536   MALE 2019        16        11       59          3
    ## 261                  0.735 FEMALE 2019       385       238      232          1
    ## 262                  0.722   MALE 2019       385       238      232          1
    ## 263                  0.639 FEMALE 2019       385       238      232          1
    ## 264                  0.750   MALE 2019       385       238      232          1
    ## 265                  0.627 FEMALE 2019        72       636      744          5
    ## 266                  0.596   MALE 2019        72       636      744          5
    ## 267                  0.540 FEMALE 2019        72       636      744          5
    ## 268                  0.558   MALE 2019        72       636      744          5
    ## 269                  0.618 FEMALE 2019       114       340      448          9
    ## 270                  0.652   MALE 2019       114       340      448          9
    ## 271                  0.698 FEMALE 2019       114       340      448          9
    ## 272                  0.645   MALE 2019       114       340      448          9
    ## 273                  0.637 FEMALE 2019        47        94      254          6
    ## 274                  0.682   MALE 2019        47        94      254          6
    ## 275                  0.653 FEMALE 2019        47        94      254          6
    ## 276                  0.577   MALE 2019        47        94      254          6
    ## 277                  0.669 FEMALE 2019         4        74      132          4
    ## 278                  0.733   MALE 2019         4        74      132          4
    ## 279                  0.542 FEMALE 2019         4        74      132          4
    ## 280                  0.589   MALE 2019         4        74      132          4
    ## 281                  0.677 FEMALE 2019        81       301      261          9
    ## 282                  0.596   MALE 2019        81       301      261          9
    ## 283                  0.590 FEMALE 2019        81       301      261          9
    ## 284                  0.621   MALE 2019        81       301      261          9
    ## 285                  0.673 FEMALE 2019       834       752      440         10
    ## 286                  0.661   MALE 2019       834       752      440         10
    ## 287                  0.624 FEMALE 2019       834       752      440         10
    ## 288                  0.538   MALE 2019       834       752      440         10
    ## 289                  0.689 FEMALE 2019        39       128      316          8
    ## 290                  0.657   MALE 2019        39       128      316          8
    ## 291                  0.618 FEMALE 2019        39       128      316          8
    ## 292                  0.577   MALE 2019        39       128      316          8
    ## 293                  0.701 FEMALE 2019       244       312      582         13
    ## 294                  0.702   MALE 2019       244       312      582         13
    ## 295                  0.642 FEMALE 2019       244       312      582         13
    ## 296                  0.628   MALE 2019       244       312      582         13
    ## 297                  0.567 FEMALE 2019        48       176     1885         16
    ## 298                  0.563   MALE 2019        48       176     1885         16
    ## 299                  0.478 FEMALE 2019        48       176     1885         16
    ## 300                  0.519   MALE 2019        48       176     1885         16
    ## 301                  0.526 FEMALE 2019        90      2537     4274          4
    ## 302                  0.526   MALE 2019        90      2537     4274          4
    ## 303                  0.552 FEMALE 2019        90      2537     4274          4
    ## 304                  0.525   MALE 2019        90      2537     4274          4
    ## 305                  0.728 FEMALE 2019      1773       333     1575          5
    ## 306                  0.687   MALE 2019      1773       333     1575          5
    ## 307                  0.634 FEMALE 2019      1773       333     1575          5
    ## 308                  0.602   MALE 2019      1773       333     1575          5
    ## 309                  0.708 FEMALE 2019       843       104     1579         12
    ## 310                  0.726   MALE 2019       843       104     1579         12
    ## 311                  0.681 FEMALE 2019       843       104     1579         12
    ## 312                  0.646   MALE 2019       843       104     1579         12
    ## 313                  0.719 FEMALE 2019        54        18      178          1
    ## 314                  0.656   MALE 2019        54        18      178          1
    ## 315                  0.655 FEMALE 2019        54        18      178          1
    ## 316                  0.611   MALE 2019        54        18      178          1
    ## 317                  0.548 FEMALE 2019        62      1511     4952         29
    ## 318                  0.522   MALE 2019        62      1511     4952         29
    ## 319                  0.552 FEMALE 2019        62      1511     4952         29
    ## 320                  0.545   MALE 2019        62      1511     4952         29
    ## 321                  0.718 FEMALE 2019       455       412      565         13
    ## 322                  0.711   MALE 2019       455       412      565         13
    ## 323                  0.626 FEMALE 2019       114       435     1624         26
    ## 324                  0.540   MALE 2019       114       435     1624         26
    ## 325                  0.549 FEMALE 2019       114       435     1624         26
    ## 326                  0.421   MALE 2019       114       435     1624         26
    ## 327                  0.717 FEMALE 2019        85       224      525          1
    ## 328                  0.698   MALE 2019        85       224      525          1
    ## 329                  0.739 FEMALE 2019        85       224      525          1
    ## 330                  0.737   MALE 2019        85       224      525          1
    ## 331                  0.675 FEMALE 2019       274        45      257          3
    ## 332                  0.689   MALE 2019       274        45      257          3
    ## 333                  0.606 FEMALE 2019       124       369      813          3
    ## 334                  0.551   MALE 2019       124       369      813          3
    ## 335                  0.496 FEMALE 2019       124       369      813          3
    ## 336                  0.655   MALE 2019       124       369      813          3
    ## 337                  0.752 FEMALE 2019        91        37      164          2
    ## 338                  0.670   MALE 2019        91        37      164          2
    ## 339                  0.583 FEMALE 2019       387       534      422          8
    ## 340                  0.651   MALE 2019       387       534      422          8
    ## 341                  0.782 FEMALE 2019       619       105      271          2
    ## 342                  0.708   MALE 2019       619       105      271          2
    ## 343                  0.737 FEMALE 2019       619       105      271          2
    ## 344                  0.721   MALE 2019       619       105      271          2
    ## 345                  0.734 FEMALE 2019      1057         9      264          5
    ## 346                  0.679   MALE 2019      1057         9      264          5
    ## 347                  0.772 FEMALE 2019      2959        70      618          2
    ## 348                  0.704   MALE 2019      2959        70      618          2
    ## 349                  0.773 FEMALE 2019      2959        70      618          2
    ## 350                  0.676   MALE 2019      2959        70      618          2
    ## 351                  0.712 FEMALE 2019       163         7      317          6
    ## 352                  0.711   MALE 2019       163         7      317          6
    ## 353                  0.797 FEMALE 2019       163         7      317          6
    ## 354                  0.750   MALE 2019       163         7      317          6
    ## 355                  0.736 FEMALE 2019      1268        10      302          1
    ## 356                  0.691   MALE 2019      1268        10      302          1
    ## 357                  0.691 FEMALE 2019      1268        10      302          1
    ## 358                  0.676   MALE 2019      1268        10      302          1
    ## 359                  0.632   MALE 2019        58        40      414          4
    ## 360                  0.615 FEMALE 2019        58        40      414          4
    ## 361                  0.667   MALE 2019        58        40      414          4
    ## 362                  0.846 FEMALE 2019      2015        71       95          3
    ## 363                  0.829   MALE 2019      2015        71       95          3
    ## 364                  0.760 FEMALE 2019      2015        71       95          3
    ## 365                  0.663   MALE 2019      2015        71       95          3
    ## 366                  0.632 FEMALE 2019      1941       128     1812          9
    ## 367                  0.594   MALE 2019      1941       128     1812          9
    ## 368                  0.619 FEMALE 2019      1941       128     1812          9
    ## 369                  0.524   MALE 2019      1941       128     1812          9
    ## 370                  0.783 FEMALE 2019       122        27      432         11
    ## 371                  0.743   MALE 2019       122        27      432         11
    ## 372                  0.722 FEMALE 2019       122        27      432         11
    ## 373                  0.638   MALE 2019       122        27      432         11
    ## 374                  0.738 FEMALE 2019        39        26       50          6
    ## 375                  0.689   MALE 2019        39        26       50          6
    ## 376                  0.710 FEMALE 2019        39        26       50          6
    ## 377                  0.612   MALE 2019        39        26       50          6
    ## 378                  0.662 FEMALE 2019        25       475      427         21
    ## 379                  0.656   MALE 2019        25       475      427         21
    ## 380                  0.640 FEMALE 2019        25       475      427         21
    ## 381                  0.635   MALE 2019        25       475      427         21
    ## 382                  0.735 FEMALE 2019        34        21       53          6
    ## 383                  0.675   MALE 2019        34        21       53          6
    ## 384                  0.644 FEMALE 2019        34        21       53          6
    ## 385                  0.638   MALE 2019        34        21       53          6
    ## 386                  0.587 FEMALE 2019         2         5       33         11
    ## 387                  0.635 FEMALE 2019         2         5       33         11
    ## 388                  0.609   MALE 2019         2         5       33         11
    ## 389                  0.574 FEMALE 2019        10        15       21          1
    ## 390                  0.733   MALE 2019        10        15       21          1
    ## 391                  0.557 FEMALE 2019         5         5       32          1
    ## 392                  0.646   MALE 2019         5         5       32          1
    ## 393                  0.536 FEMALE 2019         5         5       32          1
    ## 394                  0.600   MALE 2019         5         5       32          1
    ## 395                  0.612 FEMALE 2019        71       458      489         14
    ## 396                  0.611   MALE 2019        71       458      489         14
    ## 397                  0.528 FEMALE 2019        71       458      489         14
    ## 398                  0.577   MALE 2019        71       458      489         14
    ## 399                  0.669 FEMALE 2019        39        43      134          2
    ## 400                  0.663   MALE 2019        39        43      134          2
    ## 401                  0.754 FEMALE 2019        39        43      134          2
    ## 402                  0.599   MALE 2019        39        43      134          2
    ## 403                  0.672 FEMALE 2019       110       178       97         20
    ## 404                  0.657   MALE 2019       110       178       97         20
    ## 405                  0.679 FEMALE 2019       110       178       97         20
    ## 406                  0.669   MALE 2019       110       178       97         20
    ## 407                  0.724 FEMALE 2019       254       195      104          9
    ## 408                  0.685   MALE 2019       254       195      104          9
    ## 409                  0.695 FEMALE 2019       254       195      104          9
    ## 410                  0.693   MALE 2019       254       195      104          9
    ## 411                  0.590 FEMALE 2019         8         2       17          1
    ## 412                  0.710   MALE 2019         8         2       17          1
    ## 413                  0.675 FEMALE 2019         8         2       17          1
    ## 414                  0.535   MALE 2019         8         2       17          1
    ## 415                  0.627 FEMALE 2019        11       108      135         23
    ## 416                  0.645   MALE 2019        11       108      135         23
    ## 417                  0.513 FEMALE 2019        11       108      135         23
    ## 418                  0.646   MALE 2019        11       108      135         23
    ## 419                  0.683 FEMALE 2019        46       122      232         12
    ## 420                  0.671   MALE 2019        46       122      232         12
    ## 421                  0.637 FEMALE 2019        46       122      232         12
    ## 422                  0.662   MALE 2019        46       122      232         12
    ## 423                  0.734 FEMALE 2019       442       118      134         13
    ## 424                  0.751   MALE 2019       442       118      134         13
    ## 425                  0.741 FEMALE 2019       442       118      134         13
    ## 426                  0.692   MALE 2019       442       118      134         13
    ## 427                  0.608 FEMALE 2019         5        92       41         28
    ## 428                  0.559   MALE 2019         5        92       41         28
    ## 429                  0.492 FEMALE 2019         5        92       41         28
    ## 430                  0.644   MALE 2019         5        92       41         28
    ## 431                  0.592 FEMALE 2019      1529     10230     2991        215
    ## 432                  0.589   MALE 2019      1529     10230     2991        215
    ## 433                  0.479 FEMALE 2019      1529     10230     2991        215
    ## 434                  0.547   MALE 2019      1529     10230     2991        215
    ## 435                  0.689 FEMALE 2019         6         5       12          7
    ## 436                  0.792   MALE 2019         6         5       12          7
    ## 437                  0.432 FEMALE 2019         6         5       12          7
    ## 438                  0.565   MALE 2019         6         5       12          7
    ## 439                  0.594 FEMALE 2019        42       233      708          1
    ## 440                  0.607   MALE 2019        42       233      708          1
    ## 441                  0.517 FEMALE 2019        42       233      708          1
    ## 442                  0.556   MALE 2019        42       233      708          1
    ## 443                  0.591 FEMALE 2019         4         3       22          1
    ## 444                  0.627   MALE 2019         4         3       22          1
    ## 445                  0.427 FEMALE 2019         4         3       22          1
    ## 446                  0.578   MALE 2019         4         3       22          1
    ## 447                  0.550 FEMALE 2019         4         2       51          2
    ## 448                  0.646   MALE 2019         4         2       51          2
    ## 449                  0.565 FEMALE 2019         4         2       51          2
    ## 450                  0.537   MALE 2019         4         2       51          2
    ## 451                  0.727 FEMALE 2019       127       118      322         17
    ## 452                  0.718   MALE 2019       127       118      322         17
    ## 453                  0.801 FEMALE 2019       127       118      322         17
    ## 454                  0.645   MALE 2019       127       118      322         17
    ## 455                  0.699 FEMALE 2019        95       357      996         10
    ## 456                  0.704   MALE 2019        95       357      996         10
    ## 457                  0.641 FEMALE 2019        95       357      996         10
    ## 458                  0.586   MALE 2019        95       357      996         10
    ## 459                  0.679 FEMALE 2019       157       188      651          6
    ## 460                  0.693   MALE 2019       157       188      651          6
    ## 461                  0.667 FEMALE 2019       157       188      651          6
    ## 462                  0.581   MALE 2019       157       188      651          6
    ## 463                  0.738 FEMALE 2019       444       563     2202         10
    ## 464                  0.670   MALE 2019       444       563     2202         10
    ## 465                  0.613 FEMALE 2019       444       563     2202         10
    ## 466                  0.644   MALE 2019       444       563     2202         10
    ## 467                  0.544 FEMALE 2019       261      2517     6367         29
    ## 468                  0.490   MALE 2019       261      2517     6367         29
    ## 469                  0.490 FEMALE 2019       261      2517     6367         29
    ## 470                  0.534   MALE 2019       261      2517     6367         29
    ## 471                  0.742 FEMALE 2019        83       136      551          4
    ## 472                  0.740   MALE 2019        83       136      551          4
    ## 473                  0.695 FEMALE 2019        83       136      551          4
    ## 474                  0.644   MALE 2019        83       136      551          4
    ## 475                  0.605 FEMALE 2019         1         4       41          1
    ## 476                  0.562   MALE 2019         1         4       41          1
    ## 477                  0.556 FEMALE 2019         1         4       41          1
    ## 478                  0.635   MALE 2019         1         4       41          1
    ## 479                  0.612 FEMALE 2019         5        11       76          1
    ## 480                  0.633   MALE 2019         5        11       76          1
    ## 481                  0.487 FEMALE 2019         5        11       76          1
    ## 482                  0.440   MALE 2019         5        11       76          1
    ## 483                  0.561 FEMALE 2019         6         8       18          6
    ## 484                  0.560   MALE 2019         6         8       18          6
    ## 485                  0.427 FEMALE 2019         6         8       18          6
    ## 486                  0.515   MALE 2019         6         8       18          6
    ## 487                  0.618 FEMALE 2019        19        39      190          8
    ## 488                  0.621   MALE 2019        19        39      190          8
    ## 489                  0.606 FEMALE 2019        19        39      190          8
    ## 490                  0.528   MALE 2019        19        39      190          8
    ## 491                  0.530 FEMALE 2019         2         7       25          3
    ## 492                  0.566   MALE 2019         2         7       25          3
    ## 493                  0.510 FEMALE 2019         2         7       25          3
    ## 494                  0.528   MALE 2019         2         7       25          3
    ## 495                  0.593 FEMALE 2019         5         3       10          3
    ## 496                  0.598   MALE 2019         5         3       10          3
    ## 497                  0.500 FEMALE 2019         5         3       10          3
    ## 498                  0.462   MALE 2019         5         3       10          3
    ## 499                  0.603 FEMALE 2019         6         9       24          2
    ## 500                  0.744   MALE 2019         6         9       24          2
    ## 501                  0.548 FEMALE 2019         6         9       24          2
    ## 502                  0.575   MALE 2019         6         9       24          2
    ## 503                  0.631 FEMALE 2019        71       101      158          1
    ## 504                  0.722   MALE 2019        71       101      158          1
    ## 505                  0.709 FEMALE 2019        71       101      158          1
    ## 506                  0.640   MALE 2019        71       101      158          1
    ## 507                  0.737 FEMALE 2019        27        15       26          1
    ## 508                  0.795   MALE 2019        27        15       26          1
    ## 509                  0.744 FEMALE 2019        27        15       26          1
    ## 510                  0.662   MALE 2019        27        15       26          1
    ## 511                  0.596 FEMALE 2019        91       107     1170          7
    ## 512                  0.598   MALE 2019        91       107     1170          7
    ## 513                  0.551 FEMALE 2019        91       107     1170          7
    ## 514                  0.519   MALE 2019        91       107     1170          7
    ## 515                  0.814 FEMALE 2019        31        12       80          1
    ## 516                  0.841   MALE 2019        31        12       80          1
    ## 517                  0.666 FEMALE 2019        87        74     1256          4
    ## 518                  0.593   MALE 2019        87        74     1256          4
    ## 519                  0.527 FEMALE 2019        87        74     1256          4
    ## 520                  0.617   MALE 2019        87        74     1256          4
    ## 521                  0.647 FEMALE 2019       274       131      231          8
    ## 522                  0.673   MALE 2019       274       131      231          8
    ## 523                  0.600 FEMALE 2019       274       131      231          8
    ## 524                  0.630   MALE 2019       274       131      231          8
    ## 525                  0.658 FEMALE 2019        23        21       90          1
    ## 526                  0.685   MALE 2019        23        21       90          1
    ## 527                  0.641 FEMALE 2019        23        21       90          1
    ## 528                  0.687   MALE 2019        23        21       90          1
    ## 529                  0.516 FEMALE 2019        70      1221      774          9
    ## 530                  0.684   MALE 2019        70      1221      774          9
    ## 531                  0.628 FEMALE 2019        70      1221      774          9
    ## 532                  0.687   MALE 2019        70      1221      774          9
    ## 533                  0.665 FEMALE 2019       362       159      622          2
    ## 534                  0.687   MALE 2019       362       159      622          2
    ## 535                  0.652 FEMALE 2019       362       159      622          2
    ## 536                  0.674   MALE 2019       362       159      622          2
    ## 537                  0.607 FEMALE 2019       281      1090     4854         16
    ## 538                  0.584   MALE 2019       281      1090     4854         16
    ## 539                  0.596 FEMALE 2019       281      1090     4854         16
    ## 540                  0.570   MALE 2019       281      1090     4854         16
    ## 541                  0.683 FEMALE 2019       249        58      525          1
    ## 542                  0.727   MALE 2019       249        58      525          1
    ## 543                  0.671 FEMALE 2019       249        58      525          1
    ## 544                  0.576   MALE 2019       249        58      525          1
    ## 545                  0.751 FEMALE 2019       117        36      343          1
    ## 546                  0.703   MALE 2019       117        36      343          1
    ## 547                  0.754 FEMALE 2019       117        36      343          1
    ## 548                  0.630   MALE 2019       117        36      343          1
    ## 549                  0.639 FEMALE 2019         4         3        5          2
    ## 550                  0.566 FEMALE 2019         6         3       10          6
    ## 551                  0.541   MALE 2019         6         3       10          6
    ## 552                  0.400 FEMALE 2019         6         3       10          6
    ## 553                  0.481   MALE 2019         6         3       10          6
    ## 554                  0.645 FEMALE 2019         3         3       18          2
    ## 555                  0.661   MALE 2019         3         3       18          2
    ## 556                  0.583 FEMALE 2019         3         3       18          2
    ## 557                  0.641   MALE 2019         3         3       18          2
    ## 558                  0.621 FEMALE 2019        22        24      116          2
    ## 559                  0.650   MALE 2019        22        24      116          2
    ## 560                  0.544 FEMALE 2019        22        24      116          2
    ## 561                  0.623   MALE 2019        22        24      116          2
    ## 562                  0.743 FEMALE 2019       129       106      250          1
    ## 563                  0.722   MALE 2019       129       106      250          1
    ## 564                  0.685 FEMALE 2019       129       106      250          1
    ## 565                  0.704   MALE 2019       129       106      250          1
    ## 566                  0.769 FEMALE 2019       694       196      234         10
    ## 567                  0.713   MALE 2019       694       196      234         10
    ## 568                  0.743 FEMALE 2019       694       196      234         10
    ## 569                  0.698   MALE 2019       694       196      234         10
    ## 570                  0.608 FEMALE 2019        99       128      248          9
    ## 571                  0.652   MALE 2019        99       128      248          9
    ## 572                  0.508 FEMALE 2019        99       128      248          9
    ## 573                  0.493   MALE 2019        99       128      248          9
    ## 574                  0.767 FEMALE 2019         4         2       13          2
    ## 575                  0.738   MALE 2019         4         2       13          2
    ## 576                  0.531 FEMALE 2019        27        33       75         21
    ## 577                  0.617   MALE 2019        27        33       75         21
    ## 578                  0.647 FEMALE 2019        27        33       75         21
    ## 579                  0.460   MALE 2019        27        33       75         21
    ## 580                  0.601 FEMALE 2019         3        35       15          1
    ## 581                  0.585   MALE 2019         3        35       15          1
    ## 582                  0.581 FEMALE 2019         3        35       15          1
    ## 583                  0.514   MALE 2019         3        35       15          1
    ## 584                  0.616 FEMALE 2019        18        63       72          1
    ## 585                  0.627   MALE 2019        18        63       72          1
    ## 586                  0.571 FEMALE 2019        18        63       72          1
    ## 587                  0.549   MALE 2019        18        63       72          1
    ## 588                  0.643   MALE 2019         6         2       20          2
    ## 589                  0.556 FEMALE 2019         2         7        4          1
    ## 590                  0.522   MALE 2019         2         7        4          1
    ## 591                  0.548   MALE 2019         1         1        3          1
    ## 592                  0.657 FEMALE 2019         1         1        3          1
    ## 593                  0.763 FEMALE 2019        53        49      236          1
    ## 594                  0.753   MALE 2019        53        49      236          1
    ## 595                  0.592 FEMALE 2019        53        49      236          1
    ## 596                  0.623   MALE 2019        53        49      236          1
    ## 597                  0.605 FEMALE 2019       271      1085     1402         18
    ## 598                  0.618   MALE 2019       271      1085     1402         18
    ## 599                  0.505 FEMALE 2019       271      1085     1402         18
    ## 600                  0.553   MALE 2019       271      1085     1402         18
    ## 601                  0.542 FEMALE 2019        40      1097     1703         22
    ## 602                  0.574   MALE 2019        40      1097     1703         22
    ## 603                  0.505 FEMALE 2019        40      1097     1703         22
    ## 604                  0.560   MALE 2019        40      1097     1703         22
    ## 605                  0.615 FEMALE 2019       550       795     1086          4
    ## 606                  0.629   MALE 2019       550       795     1086          4
    ## 607                  0.589 FEMALE 2019       550       795     1086          4
    ## 608                  0.585   MALE 2019       550       795     1086          4
    ## 609                  0.621 FEMALE 2019       105       125     1177          9
    ## 610                  0.668   MALE 2019       105       125     1177          9
    ## 611                  0.602 FEMALE 2019       105       125     1177          9
    ## 612                  0.495   MALE 2019       105       125     1177          9
    ## 613                  0.639 FEMALE 2019       771       424     2221         28
    ## 614                  0.616   MALE 2019       771       424     2221         28
    ## 615                  0.625 FEMALE 2019       771       424     2221         28
    ## 616                  0.530   MALE 2019       771       424     2221         28
    ## 617                  0.815 FEMALE 2019        91         6      117          2
    ## 618                  0.771   MALE 2019        91         6      117          2
    ## 619                  0.697 FEMALE 2019        91         6      117          2
    ## 620                  0.663   MALE 2019        91         6      117          2
    ## 621                  0.715 FEMALE 2019       131        29      180          1
    ## 622                  0.697   MALE 2019       131        29      180          1
    ## 623                  0.692 FEMALE 2019       131        29      180          1
    ## 624                  0.674   MALE 2019       131        29      180          1
    ## 625                  0.662 FEMALE 2019       767       464     2391          3
    ## 626                  0.632   MALE 2019       767       464     2391          3
    ## 627                  0.605 FEMALE 2019       767       464     2391          3
    ## 628                  0.508   MALE 2019       767       464     2391          3
    ## 629                  0.634 FEMALE 2019       174       381     3393         12
    ## 630                  0.595   MALE 2019       174       381     3393         12
    ## 631                  0.520 FEMALE 2019       174       381     3393         12
    ## 632                  0.479   MALE 2019       174       381     3393         12
    ## 633                  0.572 FEMALE 2019       240      1336     3424         40
    ## 634                  0.518   MALE 2019       240      1336     3424         40
    ## 635                  0.517 FEMALE 2019       240      1336     3424         40
    ## 636                  0.539   MALE 2019       240      1336     3424         40
    ## 637                  0.700 FEMALE 2019        21        75      259         59
    ## 638                  0.617   MALE 2019        21        75      259         59
    ## 639                  0.601 FEMALE 2019        21        75      259         59
    ## 640                  0.509   MALE 2019        21        75      259         59
    ## 641                  0.660 FEMALE 2019        75       699     1689         16
    ## 642                  0.607   MALE 2019        75       699     1689         16
    ## 643                  0.559 FEMALE 2019        75       699     1689         16
    ## 644                  0.592   MALE 2019        75       699     1689         16
    ## 645                  0.739 FEMALE 2019       150        39      641          3
    ## 646                  0.647   MALE 2019       150        39      641          3
    ## 647                  0.694 FEMALE 2019       150        39      641          3
    ## 648                  0.647   MALE 2019       150        39      641          3
    ## 649                  0.708 FEMALE 2019      1365       953      998         20
    ## 650                  0.684   MALE 2019      1365       953      998         20
    ## 651                  0.692 FEMALE 2019      1365       953      998         20
    ## 652                  0.645   MALE 2019      1365       953      998         20
    ## 653                  0.745 FEMALE 2019       168       130      435          3
    ## 654                  0.732   MALE 2019       168       130      435          3
    ## 655                  0.754 FEMALE 2019       168       130      435          3
    ## 656                  0.686   MALE 2019       168       130      435          3
    ## 657                  0.610 FEMALE 2019       129       145      804          3
    ## 658                  0.678   MALE 2019       129       145      804          3
    ## 659                  0.747 FEMALE 2019       129       145      804          3
    ## 660                  0.702   MALE 2019       129       145      804          3
    ## 661                  0.682 FEMALE 2019       102        81      687          5
    ## 662                  0.577   MALE 2019       102        81      687          5
    ## 663                  0.686 FEMALE 2019       102        81      687          5
    ## 664                  0.565   MALE 2019       102        81      687          5
    ## 665                  0.500 FEMALE 2019       295       115      424          4
    ## 666                  0.462   MALE 2019       295       115      424          4
    ## 667                  0.652 FEMALE 2019       295       115      424          4
    ## 668                  0.699   MALE 2019       295       115      424          4
    ## 669                  0.683 FEMALE 2019        77        15      506          2
    ## 670                  0.643   MALE 2019        77        15      506          2
    ## 671                  0.635 FEMALE 2019        77        15      506          2
    ## 672                  0.647   MALE 2019        77        15      506          2
    ## 673                  0.582 FEMALE 2019        69       565     3341          8
    ## 674                  0.502   MALE 2019        69       565     3341          8
    ## 675                  0.623 FEMALE 2019        69       565     3341          8
    ## 676                  0.544   MALE 2019        69       565     3341          8
    ## 677                  0.701 FEMALE 2019       515       118      832         14
    ## 678                  0.686   MALE 2019       515       118      832         14
    ## 679                  0.672 FEMALE 2019       515       118      832         14
    ## 680                  0.656   MALE 2019       515       118      832         14
    ## 681                  0.615 FEMALE 2019         8         9      165          7
    ## 682                  0.250   MALE 2019         8         9      165          7
    ## 683                  0.642 FEMALE 2019        22       128      789          7
    ## 684                  0.577   MALE 2019        22       128      789          7
    ## 685                  0.607 FEMALE 2019        22       128      789          7
    ## 686                  0.559   MALE 2019        22       128      789          7
    ## 687                  0.742 FEMALE 2019         2         3       50          2
    ## 688                  0.750   MALE 2019         2         3       50          2
    ## 689                  0.510 FEMALE 2019         2         3       50          2
    ## 690                  0.611   MALE 2019         2         3       50          2
    ## 691                  0.644 FEMALE 2019        10         8      103          3
    ## 692                  0.733   MALE 2019        10         8      103          3
    ## 693                  0.514 FEMALE 2019        10         8      103          3
    ## 694                  0.409   MALE 2019        10         8      103          3
    ## 695                  0.577 FEMALE 2019        14        12       19          1
    ## 696                  0.677   MALE 2019        14        12       19          1
    ## 697                  0.510 FEMALE 2019        14        12       19          1
    ## 698                  0.540   MALE 2019        14        12       19          1
    ## 699                  0.638 FEMALE 2019         3         6       20          1
    ## 700                  0.693   MALE 2019         3         6       20          1
    ## 701                  0.687 FEMALE 2019         7         8       14          4
    ## 702                  0.648   MALE 2019         7         8       14          4
    ## 703                  0.673 FEMALE 2019         7         8       14          4
    ## 704                  0.412   MALE 2019         7         8       14          4
    ## 705                  0.679 FEMALE 2019         5        17       25          1
    ## 706                  0.577   MALE 2019         5        17       25          1
    ## 707                  0.633 FEMALE 2019         5        17       25          1
    ## 708                  0.667   MALE 2019         5        17       25          1
    ## 709                  0.728 FEMALE 2019       547       402      406         17
    ## 710                  0.735   MALE 2019       547       402      406         17
    ## 711                  0.728 FEMALE 2019       547       402      406         17
    ## 712                  0.716   MALE 2019       547       402      406         17
    ## 713                  0.819 FEMALE 2019        46        11       45          4
    ## 714                  0.680   MALE 2019        46        11       45          4
    ## 715                  0.644 FEMALE 2019        46        11       45          4
    ## 716                  0.611   MALE 2019        46        11       45          4
    ## 717                  0.622 FEMALE 2019       169       795     1611          6
    ## 718                  0.577   MALE 2019       169       795     1611          6
    ## 719                  0.493 FEMALE 2019       169       795     1611          6
    ## 720                  0.559   MALE 2019       169       795     1611          6
    ## 721                  0.673 FEMALE 2019        69       107      249          3
    ## 722                  0.652   MALE 2019        69       107      249          3
    ## 723                  0.644 FEMALE 2019        69       107      249          3
    ## 724                  0.553   MALE 2019        69       107      249          3
    ## 725                  0.691 FEMALE 2019        61        90      264          2
    ## 726                  0.628   MALE 2019        61        90      264          2
    ## 727                  0.743 FEMALE 2019        61        90      264          2
    ## 728                  0.627   MALE 2019        61        90      264          2
    ## 729                  0.633 FEMALE 2019        32       179      698          2
    ## 730                  0.631   MALE 2019        32       179      698          2
    ## 731                  0.556 FEMALE 2019        32       179      698          2
    ## 732                  0.527   MALE 2019        32       179      698          2
    ## 733                  0.671 FEMALE 2019        25        37      107          2
    ## 734                  0.591   MALE 2019        25        37      107          2
    ## 735                  0.614 FEMALE 2019        25        37      107          2
    ## 736                  0.664   MALE 2019        25        37      107          2
    ## 737                  0.630 FEMALE 2019        44        33       82          1
    ## 738                  0.654   MALE 2019        44        33       82          1
    ## 739                  0.661 FEMALE 2019        44        33       82          1
    ## 740                  0.631   MALE 2019        44        33       82          1
    ## 741                  0.580 FEMALE 2019         4        14       10          2
    ## 742                  0.464   MALE 2019         4        14       10          2
    ## 743                  0.741 FEMALE 2019         4        14       10          2
    ## 744                  0.531   MALE 2019         4        14       10          2
    ## 745                  0.580 FEMALE 2019        14        24       76          4
    ## 746                  0.593   MALE 2019        14        24       76          4
    ## 747                  0.517 FEMALE 2019        14        24       76          4
    ## 748                  0.432   MALE 2019        14        24       76          4
    ## 749                  0.686 FEMALE 2019         6        16       65          4
    ## 750                  0.662   MALE 2019         6        16       65          4
    ## 751                  0.714   MALE 2019         6        16       65          4
    ## 752                  0.711 FEMALE 2019         6         8       17          2
    ## 753                  0.720   MALE 2019         6         8       17          2
    ## 754                  0.449 FEMALE 2019         6         8       17          2
    ## 755                  0.482   MALE 2019         6         8       17          2
    ## 756                  0.738 FEMALE 2019       107        30      268          1
    ## 757                  0.753   MALE 2019       107        30      268          1
    ## 758                  0.750 FEMALE 2019       107        30      268          1
    ## 759                  0.696   MALE 2019       107        30      268          1
    ## 760                  0.661 FEMALE 2019        73       131      687          2
    ## 761                  0.628   MALE 2019        73       131      687          2
    ## 762                  0.663 FEMALE 2019        73       131      687          2
    ## 763                  0.632   MALE 2019        73       131      687          2
    ## 764                  0.789 FEMALE 2019       196        94      167          2
    ## 765                  0.754   MALE 2019       196        94      167          2
    ## 766                  0.786 FEMALE 2019       196        94      167          2
    ## 767                  0.718   MALE 2019       196        94      167          2
    ## 768                  0.704 FEMALE 2019       484        90      299          1
    ## 769                  0.756   MALE 2019       484        90      299          1
    ## 770                  0.726 FEMALE 2019       484        90      299          1
    ## 771                  0.661   MALE 2019       484        90      299          1
    ## 772                  0.662 FEMALE 2019       109       622      745          2
    ## 773                  0.611   MALE 2019       109       622      745          2
    ## 774                  0.509 FEMALE 2019       109       622      745          2
    ## 775                  0.471   MALE 2019       109       622      745          2
    ## 776                  0.688 FEMALE 2019       445        78      661          2
    ## 777                  0.716   MALE 2019       445        78      661          2
    ## 778                  0.685 FEMALE 2019       445        78      661          2
    ## 779                  0.683   MALE 2019       445        78      661          2
    ## 780                  0.717 FEMALE 2019        86         8      271          1
    ## 781                  0.727   MALE 2019        86         8      271          1
    ## 782                  0.715 FEMALE 2019        86         8      271          1
    ## 783                  0.667   MALE 2019        86         8      271          1
    ## 784                  0.664 FEMALE 2019        91       103      379          3
    ## 785                  0.750   MALE 2019        91       103      379          3
    ## 786                  0.626 FEMALE 2019        91       103      379          3
    ## 787                  0.596   MALE 2019        91       103      379          3
    ## 788                  0.760 FEMALE 2019        89        38      219          1
    ## 789                  0.713   MALE 2019        89        38      219          1
    ## 790                  0.780 FEMALE 2019        89        38      219          1
    ## 791                  0.703   MALE 2019        89        38      219          1
    ## 792                  0.603 FEMALE 2019       138      5307     1964         14
    ## 793                  0.597   MALE 2019       138      5307     1964         14
    ## 794                  0.728 FEMALE 2019       138      5307     1964         14
    ## 795                  0.725   MALE 2019       138      5307     1964         14
    ## 796                  0.626 FEMALE 2019       506      2144     5266         13
    ## 797                  0.636   MALE 2019       506      2144     5266         13
    ## 798                  0.614 FEMALE 2019       506      2144     5266         13
    ## 799                  0.576   MALE 2019       506      2144     5266         13
    ## 800                  0.670 FEMALE 2019        31        16      186          1
    ## 801                  0.757   MALE 2019        31        16      186          1
    ## 802                  0.719 FEMALE 2019        31        16      186          1
    ## 803                  0.611   MALE 2019        31        16      186          1
    ## 804                  0.809 FEMALE 2019       167       150      478          1
    ## 805                  0.816   MALE 2019       167       150      478          1
    ## 806                  0.821 FEMALE 2019       167       150      478          1
    ## 807                  0.696   MALE 2019       167       150      478          1
    ## 808                  0.750 FEMALE 2019       130        44      323          1
    ## 809                  0.840   MALE 2019       130        44      323          1
    ## 810                  0.840 FEMALE 2019       130        44      323          1
    ## 811                  0.756   MALE 2019       130        44      323          1
    ## 812                  0.760 FEMALE 2019      1024        52      334          3
    ## 813                  0.817   MALE 2019      1024        52      334          3
    ## 814                  0.787 FEMALE 2019      1024        52      334          3
    ## 815                  0.791   MALE 2019      1024        52      334          3
    ## 816                  0.712 FEMALE 2019       101        32      293          1
    ## 817                  0.686   MALE 2019       101        32      293          1
    ## 818                  0.776 FEMALE 2019       101        32      293          1
    ## 819                  0.704   MALE 2019       101        32      293          1
    ## 820                  0.662 FEMALE 2019         5         6       20          3
    ## 821                  0.725   MALE 2019         5         6       20          3
    ## 822                  0.593 FEMALE 2019         5         6       20          3
    ## 823                  0.569   MALE 2019         5         6       20          3
    ## 824                  0.639 FEMALE 2019         9         7       30          1
    ## 825                  0.678   MALE 2019         9         7       30          1
    ## 826                  0.651 FEMALE 2019         9         7       30          1
    ## 827                  0.490   MALE 2019         9         7       30          1
    ## 828                  0.567 FEMALE 2019         1         3       27          1
    ## 829                  0.575   MALE 2019         1         3       27          1
    ## 830                  0.674 FEMALE 2019         1         3       27          1
    ## 831                  0.532   MALE 2019         1         3       27          1
    ##     num_white num_female num_male num_lep num_multi num_swd num_ecdis
    ## 1        1504        909      934      13        98     320       786
    ## 2        1504        909      934      13        98     320       786
    ## 3        1504        909      934      13        98     320       786
    ## 4        1504        909      934      13        98     320       786
    ## 5        1429       1008     1007      25       164     338      1175
    ## 6        1429       1008     1007      25       164     338      1175
    ## 7        1429       1008     1007      25       164     338      1175
    ## 8        1429       1008     1007      25       164     338      1175
    ## 9        3105       2401     2418     182       390     717      1708
    ## 10       3105       2401     2418     182       390     717      1708
    ## 11       3105       2401     2418     182       390     717      1708
    ## 12       3105       2401     2418     182       390     717      1708
    ## 13       3703       2914     2973     313       331     792      1442
    ## 14       3703       2914     2973     313       331     792      1442
    ## 15       3703       2914     2973     313       331     792      1442
    ## 16       3703       2914     2973     313       331     792      1442
    ## 17       3568       2390     2451     227       177     587       999
    ## 18       3568       2390     2451     227       177     587       999
    ## 19       3568       2390     2451     227       177     587       999
    ## 20       3568       2390     2451     227       177     587       999
    ## 21       1059        569      549       1        13     164       562
    ## 22       1059        569      549       1        13     164       562
    ## 23       1059        569      549       1        13     164       562
    ## 24       1059        569      549       1        13     164       562
    ## 25       1301        648      756       2        49     158       564
    ## 26       1301        648      756       2        49     158       564
    ## 27       1301        648      756       2        49     158       564
    ## 28       1301        648      756       2        49     158       564
    ## 29       2206       2562     2717     197       642     961      3843
    ## 30       2206       2562     2717     197       642     961      3843
    ## 31       2206       2562     2717     197       642     961      3843
    ## 32       2206       2562     2717     197       642     961      3843
    ## 33        645        293      384       0        19     102       458
    ## 34        645        293      384       0        19     102       458
    ## 35        645        293      384       0        19     102       458
    ## 36        645        293      384       0        19     102       458
    ## 37       1256        656      739       4        48     181       695
    ## 38       1256        656      739       4        48     181       695
    ## 39       1256        656      739       4        48     181       695
    ## 40       1256        656      739       4        48     181       695
    ## 41       1536        850      872       8        91     244       728
    ## 42       1536        850      872       8        91     244       728
    ## 43       1536        850      872       8        91     244       728
    ## 44       1536        850      872       8        91     244       728
    ## 45       1062        554      602       2        43     131       485
    ## 46       1062        554      602       2        43     131       485
    ## 47       1062        554      602       2        43     131       485
    ## 48       1062        554      602       2        43     131       485
    ## 49        549        294      306       0        22      65       214
    ## 50        549        294      306       0        22      65       214
    ## 51        549        294      306       0        22      65       214
    ## 52        549        294      306       0        22      65       214
    ## 53        369        222      182       0        19      49       231
    ## 54        369        222      182       0        19      49       231
    ## 55       1684        976     1076       7       161     339      1128
    ## 56       1684        976     1076       7       161     339      1128
    ## 57       1684        976     1076       7       161     339      1128
    ## 58       1684        976     1076       7       161     339      1128
    ## 59        805        452      451       0        30     133       466
    ## 60        805        452      451       0        30     133       466
    ## 61        805        452      451       0        30     133       466
    ## 62        805        452      451       0        30     133       466
    ## 63        642        624      652       3       117     156       844
    ## 64        642        624      652       3       117     156       844
    ## 65        642        624      652       3       117     156       844
    ## 66        642        624      652       3       117     156       844
    ## 67        848        440      482       5        38     105       416
    ## 68        848        440      482       5        38     105       416
    ## 69        848        440      482       5        38     105       416
    ## 70        848        440      482       5        38     105       416
    ## 71        870        452      455       6        17     132       441
    ## 72        870        452      455       6        17     132       441
    ## 73        870        452      455       6        17     132       441
    ## 74        870        452      455       6        17     132       441
    ## 75        758        416      392       1        19     122       359
    ## 76        758        416      392       1        19     122       359
    ## 77        758        416      392       1        19     122       359
    ## 78        758        416      392       1        19     122       359
    ## 79        797        407      444       2        21     122       499
    ## 80        797        407      444       2        21     122       499
    ## 81        797        407      444       2        21     122       499
    ## 82        797        407      444       2        21     122       499
    ## 83        781       1068     1093     341        81     265      1397
    ## 84        781       1068     1093     341        81     265      1397
    ## 85        781       1068     1093     341        81     265      1397
    ## 86        781       1068     1093     341        81     265      1397
    ## 87        761        553      518      10        71     202       639
    ## 88        761        553      518      10        71     202       639
    ## 89        761        553      518      10        71     202       639
    ## 90        761        553      518      10        71     202       639
    ## 91        441        298      240      16        17      87       260
    ## 92        441        298      240      16        17      87       260
    ## 93        441        298      240      16        17      87       260
    ## 94        414        198      236       5         4      74       254
    ## 95        414        198      236       5         4      74       254
    ## 96        414        198      236       5         4      74       254
    ## 97        609        341      333       5         2     135       396
    ## 98        609        341      333       5         2     135       396
    ## 99        609        341      333       5         2     135       396
    ## 100       609        341      333       5         2     135       396
    ## 101      4207       2859     3025      17       668     813      3374
    ## 102      4207       2859     3025      17       668     813      3374
    ## 103      4207       2859     3025      17       668     813      3374
    ## 104      4207       2859     3025      17       668     813      3374
    ## 105       904        461      493       3         1     111       503
    ## 106       904        461      493       3         1     111       503
    ## 107       904        461      493       3         1     111       503
    ## 108       904        461      493       3         1     111       503
    ## 109       760        408      387       0        10     155       425
    ## 110       760        408      387       0        10     155       425
    ## 111       760        408      387       0        10     155       425
    ## 112       760        408      387       0        10     155       425
    ## 113      1605        869      895      12        32     265      1021
    ## 114      1605        869      895      12        32     265      1021
    ## 115      1605        869      895      12        32     265      1021
    ## 116      1605        869      895      12        32     265      1021
    ## 117      1125        582      585       0         6     158       670
    ## 118      1125        582      585       0         6     158       670
    ## 119      1125        582      585       0         6     158       670
    ## 120      1125        582      585       0         6     158       670
    ## 121      1918        968     1055       7         1     368       870
    ## 122      1918        968     1055       7         1     368       870
    ## 123      1918        968     1055       7         1     368       870
    ## 124      1918        968     1055       7         1     368       870
    ## 125      1748        895      962       8        43     361       871
    ## 126      1748        895      962       8        43     361       871
    ## 127      1748        895      962       8        43     361       871
    ## 128      1748        895      962       8        43     361       871
    ## 129      1004        612      633      33        73     147       438
    ## 130      1004        612      633      33        73     147       438
    ## 131      1004        612      633      33        73     147       438
    ## 132      1004        612      633      33        73     147       438
    ## 133       663        831      832     139       174     266      1167
    ## 134       663        831      832     139       174     266      1167
    ## 135       663        831      832     139       174     266      1167
    ## 136       663        831      832     139       174     266      1167
    ## 137      5269       3995     3968     196       347    1222      2255
    ## 138      5269       3995     3968     196       347    1222      2255
    ## 139      5269       3995     3968     196       347    1222      2255
    ## 140      5269       3995     3968     196       347    1222      2255
    ## 141       841        697      751      23        63     183       340
    ## 142       841        697      751      23        63     183       340
    ## 143       841        697      751      23        63     183       340
    ## 144       841        697      751      23        63     183       340
    ## 145      1490        860      928      35        22     232       485
    ## 146      1490        860      928      35        22     232       485
    ## 147      1490        860      928      35        22     232       485
    ## 148      1490        860      928      35        22     232       485
    ## 149       787        479      468      17        39     107       211
    ## 150       787        479      468      17        39     107       211
    ## 151       787        479      468      17        39     107       211
    ## 152       787        479      468      17        39     107       211
    ## 153      1565        826      807       5        20     192       499
    ## 154      1565        826      807       5        20     192       499
    ## 155      1565        826      807       5        20     192       499
    ## 156      1565        826      807       5        20     192       499
    ## 157      7406       4993     5121     388       453    1154      1688
    ## 158      7406       4993     5121     388       453    1154      1688
    ## 159      7406       4993     5121     388       453    1154      1688
    ## 160      7406       4993     5121     388       453    1154      1688
    ## 161      2115       1587     1797     135       164     338      1730
    ## 162      2115       1587     1797     135       164     338      1730
    ## 163      2115       1587     1797     135       164     338      1730
    ## 164      2115       1587     1797     135       164     338      1730
    ## 165       914       1105     1178       0       161     343      1508
    ## 166       914       1105     1178       0       161     343      1508
    ## 167       914       1105     1178       0       161     343      1508
    ## 168       914       1105     1178       0       161     343      1508
    ## 169      1613        833      873       5        27     207       606
    ## 170      1613        833      873       5        27     207       606
    ## 171      1613        833      873       5        27     207       606
    ## 172      1613        833      873       5        27     207       606
    ## 173      1736       1086     1156       1        71     436      1166
    ## 174      1736       1086     1156       1        71     436      1166
    ## 175      1736       1086     1156       1        71     436      1166
    ## 176      1736       1086     1156       1        71     436      1166
    ## 177      2466       1431     1481      46       143     364       711
    ## 178      2466       1431     1481      46       143     364       711
    ## 179      2466       1431     1481      46       143     364       711
    ## 180      2466       1431     1481      46       143     364       711
    ## 181      3225       1754     1721       8        87     566       855
    ## 182      3225       1754     1721       8        87     566       855
    ## 183      3225       1754     1721       8        87     566       855
    ## 184      3225       1754     1721       8        87     566       855
    ## 185      4199       2330     2423      39       181     783      1471
    ## 186      4199       2330     2423      39       181     783      1471
    ## 187      4199       2330     2423      39       181     783      1471
    ## 188      4199       2330     2423      39       181     783      1471
    ## 189      1279        956     1021     334        79     353      1542
    ## 190      1279        956     1021     334        79     353      1542
    ## 191      1279        956     1021     334        79     353      1542
    ## 192      1279        956     1021     334        79     353      1542
    ## 193      5270       2830     2940      47       139     941      1095
    ## 194      5270       2830     2940      47       139     941      1095
    ## 195      5270       2830     2940      47       139     941      1095
    ## 196      5270       2830     2940      47       139     941      1095
    ## 197      1140        691      695       6        52     163       582
    ## 198      1140        691      695       6        52     163       582
    ## 199      1140        691      695       6        52     163       582
    ## 200      1140        691      695       6        52     163       582
    ## 201      4398       2371     2379      26       116     710       679
    ## 202      4398       2371     2379      26       116     710       679
    ## 203      4398       2371     2379      26       116     710       679
    ## 204      4398       2371     2379      26       116     710       679
    ## 205      5088       3404     3490     207       418    1250      3397
    ## 206      5088       3404     3490     207       418    1250      3397
    ## 207      5088       3404     3490     207       418    1250      3397
    ## 208      5088       3404     3490     207       418    1250      3397
    ## 209       706        369      380       0        21     119       380
    ## 210       706        369      380       0        21     119       380
    ## 211       706        369      380       0        21     119       380
    ## 212       706        369      380       0        21     119       380
    ## 213       475        724      731       0        75     209      1089
    ## 214       475        724      731       0        75     209      1089
    ## 215       475        724      731       0        75     209      1089
    ## 216       475        724      731       0        75     209      1089
    ## 217      1424        729      821      11        33     183       783
    ## 218      1424        729      821      11        33     183       783
    ## 219      1424        729      821      11        33     183       783
    ## 220      1424        729      821      11        33     183       783
    ## 221      1672       1084     1222      47        75     282      1338
    ## 222      1672       1084     1222      47        75     282      1338
    ## 223      1672       1084     1222      47        75     282      1338
    ## 224      1672       1084     1222      47        75     282      1338
    ## 225       983        591      614      11        84     215       615
    ## 226       983        591      614      11        84     215       615
    ## 227       983        591      614      11        84     215       615
    ## 228       983        591      614      11        84     215       615
    ## 229      1059        568      683       9        51     154       475
    ## 230      1059        568      683       9        51     154       475
    ## 231      1059        568      683       9        51     154       475
    ## 232      1059        568      683       9        51     154       475
    ## 233      1787        946      948       4        44     333       944
    ## 234      1787        946      948       4        44     333       944
    ## 235      1787        946      948       4        44     333       944
    ## 236      1787        946      948       4        44     333       944
    ## 237      2441       1845     1872      96       279     533      1910
    ## 238      2441       1845     1872      96       279     533      1910
    ## 239      2441       1845     1872      96       279     533      1910
    ## 240      2441       1845     1872      96       279     533      1910
    ## 241       794        442      425       1        30     108       412
    ## 242       794        442      425       1        30     108       412
    ## 243       794        442      425       1        30     108       412
    ## 244       794        442      425       1        30     108       412
    ## 245      2374       1532     1659      40       242     542      1636
    ## 246      2374       1532     1659      40       242     542      1636
    ## 247      2374       1532     1659      40       242     542      1636
    ## 248      2374       1532     1659      40       242     542      1636
    ## 249       719        421      452      35        36     120       339
    ## 250       719        421      452      35        36     120       339
    ## 251       719        421      452      35        36     120       339
    ## 252       719        421      452      35        36     120       339
    ## 253      1164        641      652       7        60     188       682
    ## 254      1164        641      652       7        60     188       682
    ## 255      1164        641      652       7        60     188       682
    ## 256      1164        641      652       7        60     188       682
    ## 257      1745        933      971       3        70     295       755
    ## 258      1745        933      971       3        70     295       755
    ## 259      1745        933      971       3        70     295       755
    ## 260      1745        933      971       3        70     295       755
    ## 261      2395       1751     1767     151       267     405       687
    ## 262      2395       1751     1767     151       267     405       687
    ## 263      2395       1751     1767     151       267     405       687
    ## 264      2395       1751     1767     151       267     405       687
    ## 265      1367       1510     1534     121       220     314      1841
    ## 266      1367       1510     1534     121       220     314      1841
    ## 267      1367       1510     1534     121       220     314      1841
    ## 268      1367       1510     1534     121       220     314      1841
    ## 269      2537       1733     1883      79       168     384      1272
    ## 270      2537       1733     1883      79       168     384      1272
    ## 271      2537       1733     1883      79       168     384      1272
    ## 272      2537       1733     1883      79       168     384      1272
    ## 273      3885       2140     2270      24       124     487      1206
    ## 274      3885       2140     2270      24       124     487      1206
    ## 275      3885       2140     2270      24       124     487      1206
    ## 276      3885       2140     2270      24       124     487      1206
    ## 277       732        490      517      10        61     179       521
    ## 278       732        490      517      10        61     179       521
    ## 279       732        490      517      10        61     179       521
    ## 280       732        490      517      10        61     179       521
    ## 281      2886       1809     1944      56       215     389      1205
    ## 282      2886       1809     1944      56       215     389      1205
    ## 283      2886       1809     1944      56       215     389      1205
    ## 284      2886       1809     1944      56       215     389      1205
    ## 285      3220       2760     2843     354       347     540      2382
    ## 286      3220       2760     2843     354       347     540      2382
    ## 287      3220       2760     2843     354       347     540      2382
    ## 288      3220       2760     2843     354       347     540      2382
    ## 289      2635       1615     1693      41       182     482      1454
    ## 290      2635       1615     1693      41       182     482      1454
    ## 291      2635       1615     1693      41       182     482      1454
    ## 292      2635       1615     1693      41       182     482      1454
    ## 293      6880       4039     4283     165       291     766      1823
    ## 294      6880       4039     4283     165       291     766      1823
    ## 295      6880       4039     4283     165       291     766      1823
    ## 296      6880       4039     4283     165       291     766      1823
    ## 297      1571       1905     1965     237       174     585      2616
    ## 298      1571       1905     1965     237       174     585      2616
    ## 299      1571       1905     1965     237       174     585      2616
    ## 300      1571       1905     1965     237       174     585      2616
    ## 301       105       3466     3590    1349        46     766      4340
    ## 302       105       3466     3590    1349        46     766      4340
    ## 303       105       3466     3590    1349        46     766      4340
    ## 304       105       3466     3590    1349        46     766      4340
    ## 305      3499       3598     3642     237        55     845      1771
    ## 306      3499       3598     3642     237        55     845      1771
    ## 307      3499       3598     3642     237        55     845      1771
    ## 308      3499       3598     3642     237        55     845      1771
    ## 309      4598       3540     3701     188       105     835      1417
    ## 310      4598       3540     3701     188       105     835      1417
    ## 311      4598       3540     3701     188       105     835      1417
    ## 312      4598       3540     3701     188       105     835      1417
    ## 313      1970       1103     1146      26        28     326       272
    ## 314      1970       1103     1146      26        28     326       272
    ## 315      1970       1103     1146      26        28     326       272
    ## 316      1970       1103     1146      26        28     326       272
    ## 317       394       3344     3724    1491       120     940      4250
    ## 318       394       3344     3724    1491       120     940      4250
    ## 319       394       3344     3724    1491       120     940      4250
    ## 320       394       3344     3724    1491       120     940      4250
    ## 321       465        914     1050     128        54     265       717
    ## 322       465        914     1050     128        54     265       717
    ## 323       319       1200     1331     519        13     428      1913
    ## 324       319       1200     1331     519        13     428      1913
    ## 325       319       1200     1331     519        13     428      1913
    ## 326       319       1200     1331     519        13     428      1913
    ## 327      2632       1725     1856      45       114     478       514
    ## 328      2632       1725     1856      45       114     478       514
    ## 329      2632       1725     1856      45       114     478       514
    ## 330      2632       1725     1856      45       114     478       514
    ## 331       862        726      753      33        38     222       220
    ## 332       862        726      753      33        38     222       220
    ## 333       403        829      932     183        49     317       896
    ## 334       403        829      932     183        49     317       896
    ## 335       403        829      932     183        49     317       896
    ## 336       403        829      932     183        49     317       896
    ## 337       861        572      605      30        22     181       121
    ## 338       861        572      605      30        22     181       121
    ## 339        69        720      744     114        44     138       706
    ## 340        69        720      744     114        44     138       706
    ## 341      2169       1583     1633       0        50     317       460
    ## 342      2169       1583     1633       0        50     317       460
    ## 343      2169       1583     1633       0        50     317       460
    ## 344      2169       1583     1633       0        50     317       460
    ## 345       413        861      924     130        37     179       373
    ## 346       413        861      924     130        37     179       373
    ## 347      3010       3245     3591     351       177    1023      1349
    ## 348      3010       3245     3591     351       177    1023      1349
    ## 349      3010       3245     3591     351       177    1023      1349
    ## 350      3010       3245     3591     351       177    1023      1349
    ## 351      1960       1227     1321       0        95     356         1
    ## 352      1960       1227     1321       0        95     356         1
    ## 353      1960       1227     1321       0        95     356         1
    ## 354      1960       1227     1321       0        95     356         1
    ## 355      3287       2409     2547     124        88     698       549
    ## 356      3287       2409     2547     124        88     698       549
    ## 357      3287       2409     2547     124        88     698       549
    ## 358      3287       2409     2547     124        88     698       549
    ## 359       996        776      789     153        53     220       420
    ## 360       996        776      789     153        53     220       420
    ## 361       996        776      789     153        53     220       420
    ## 362       931       1465     1690     151        40     338       388
    ## 363       931       1465     1690     151        40     338       388
    ## 364       931       1465     1690     151        40     338       388
    ## 365       931       1465     1690     151        40     338       388
    ## 366      1392       2582     2810     526       110     668      1848
    ## 367      1392       2582     2810     526       110     668      1848
    ## 368      1392       2582     2810     526       110     668      1848
    ## 369      1392       2582     2810     526       110     668      1848
    ## 370      6004       3376     3337      28       117     827       593
    ## 371      6004       3376     3337      28       117     827       593
    ## 372      6004       3376     3337      28       117     827       593
    ## 373      6004       3376     3337      28       117     827       593
    ## 374      1809        948     1045      13        63     341       464
    ## 375      1809        948     1045      13        63     341       464
    ## 376      1809        948     1045      13        63     341       464
    ## 377      1809        948     1045      13        63     341       464
    ## 378      3186       2208     2294      42       368     794      2563
    ## 379      3186       2208     2294      42       368     794      2563
    ## 380      3186       2208     2294      42       368     794      2563
    ## 381      3186       2208     2294      42       368     794      2563
    ## 382      2749       1460     1486      30        83     328       529
    ## 383      2749       1460     1486      30        83     328       529
    ## 384      2749       1460     1486      30        83     328       529
    ## 385      2749       1460     1486      30        83     328       529
    ## 386       977        515      560      13        47     149       459
    ## 387       977        515      560      13        47     149       459
    ## 388       977        515      560      13        47     149       459
    ## 389       480        274      267       4        14      67       258
    ## 390       480        274      267       4        14      67       258
    ## 391       903        498      477       4        29     105       383
    ## 392       903        498      477       4        29     105       383
    ## 393       903        498      477       4        29     105       383
    ## 394       903        498      477       4        29     105       383
    ## 395      4443       2745     2868      67       138    1024      3625
    ## 396      4443       2745     2868      67       138    1024      3625
    ## 397      4443       2745     2868      67       138    1024      3625
    ## 398      4443       2745     2868      67       138    1024      3625
    ## 399      2826       1544     1599      37        99     471      1120
    ## 400      2826       1544     1599      37        99     471      1120
    ## 401      2826       1544     1599      37        99     471      1120
    ## 402      2826       1544     1599      37        99     471      1120
    ## 403      2900       1659     1752      71       106     586      1362
    ## 404      2900       1659     1752      71       106     586      1362
    ## 405      2900       1659     1752      71       106     586      1362
    ## 406      2900       1659     1752      71       106     586      1362
    ## 407      1977       1289     1405      72       155     277       684
    ## 408      1977       1289     1405      72       155     277       684
    ## 409      1977       1289     1405      72       155     277       684
    ## 410      1977       1289     1405      72       155     277       684
    ## 411       577        309      302       9         6      51       213
    ## 412       577        309      302       9         6      51       213
    ## 413       577        309      302       9         6      51       213
    ## 414       577        309      302       9         6      51       213
    ## 415      1045        720      733      59       131     229       962
    ## 416      1045        720      733      59       131     229       962
    ## 417      1045        720      733      59       131     229       962
    ## 418      1045        720      733      59       131     229       962
    ## 419      4720       2573     2816      29       257     758      1511
    ## 420      4720       2573     2816      29       257     758      1511
    ## 421      4720       2573     2816      29       257     758      1511
    ## 422      4720       2573     2816      29       257     758      1511
    ## 423      3370       2093     2110      35       126     391       622
    ## 424      3370       2093     2110      35       126     391       622
    ## 425      3370       2093     2110      35       126     391       622
    ## 426      3370       2093     2110      35       126     391       622
    ## 427       611        432      398      19        53     118       412
    ## 428       611        432      398      19        53     118       412
    ## 429       611        432      398      19        53     118       412
    ## 430       611        432      398      19        53     118       412
    ## 431      4566      10154    10903    3382      1526    4222     15638
    ## 432      4566      10154    10903    3382      1526    4222     15638
    ## 433      4566      10154    10903    3382      1526    4222     15638
    ## 434      4566      10154    10903    3382      1526    4222     15638
    ## 435       721        361      409       4        19      92       260
    ## 436       721        361      409       4        19      92       260
    ## 437       721        361      409       4        19      92       260
    ## 438       721        361      409       4        19      92       260
    ## 439       935       1097     1077     195       255     269      1299
    ## 440       935       1097     1077     195       255     269      1299
    ## 441       935       1097     1077     195       255     269      1299
    ## 442       935       1097     1077     195       255     269      1299
    ## 443      1133        559      611       2         7     181       642
    ## 444      1133        559      611       2         7     181       642
    ## 445      1133        559      611       2         7     181       642
    ## 446      1133        559      611       2         7     181       642
    ## 447       705        378      421       5        35      94       394
    ## 448       705        378      421       5        35      94       394
    ## 449       705        378      421       5        35      94       394
    ## 450       705        378      421       5        35      94       394
    ## 451      3656       2204     2218     104       182     496       953
    ## 452      3656       2204     2218     104       182     496       953
    ## 453      3656       2204     2218     104       182     496       953
    ## 454      3656       2204     2218     104       182     496       953
    ## 455      2355       1944     1973      56       104     489      1187
    ## 456      2355       1944     1973      56       104     489      1187
    ## 457      2355       1944     1973      56       104     489      1187
    ## 458      2355       1944     1973      56       104     489      1187
    ## 459      2001       1469     1633      47        99     378       622
    ## 460      2001       1469     1633      47        99     378       622
    ## 461      2001       1469     1633      47        99     378       622
    ## 462      2001       1469     1633      47        99     378       622
    ## 463      3077       3196     3270     351       170    1053      2118
    ## 464      3077       3196     3270     351       170    1053      2118
    ## 465      3077       3196     3270     351       170    1053      2118
    ## 466      3077       3196     3270     351       170    1053      2118
    ## 467      1921       5554     6005    1648       464    1713      7014
    ## 468      1921       5554     6005    1648       464    1713      7014
    ## 469      1921       5554     6005    1648       464    1713      7014
    ## 470      1921       5554     6005    1648       464    1713      7014
    ## 471      2669       1751     1818      55       126     482       726
    ## 472      2669       1751     1818      55       126     482       726
    ## 473      2669       1751     1818      55       126     482       726
    ## 474      2669       1751     1818      55       126     482       726
    ## 475       650        351      370       2        24     113       383
    ## 476       650        351      370       2        24     113       383
    ## 477       650        351      370       2        24     113       383
    ## 478       650        351      370       2        24     113       383
    ## 479       827        493      485       9        58     137       520
    ## 480       827        493      485       9        58     137       520
    ## 481       827        493      485       9        58     137       520
    ## 482       827        493      485       9        58     137       520
    ## 483      1063        569      562       0        30     231       746
    ## 484      1063        569      562       0        30     231       746
    ## 485      1063        569      562       0        30     231       746
    ## 486      1063        569      562       0        30     231       746
    ## 487      3016       1619     1742      37        89     591      2088
    ## 488      3016       1619     1742      37        89     591      2088
    ## 489      3016       1619     1742      37        89     591      2088
    ## 490      3016       1619     1742      37        89     591      2088
    ## 491      1261        621      689       7        12     200       946
    ## 492      1261        621      689       7        12     200       946
    ## 493      1261        621      689       7        12     200       946
    ## 494      1261        621      689       7        12     200       946
    ## 495       748        361      423       1        15     103       383
    ## 496       748        361      423       1        15     103       383
    ## 497       748        361      423       1        15     103       383
    ## 498       748        361      423       1        15     103       383
    ## 499      1615        838      866       1        48     172       885
    ## 500      1615        838      866       1        48     172       885
    ## 501      1615        838      866       1        48     172       885
    ## 502      1615        838      866       1        48     172       885
    ## 503      1438        881      949      18        61     328       434
    ## 504      1438        881      949      18        61     328       434
    ## 505      1438        881      949      18        61     328       434
    ## 506      1438        881      949      18        61     328       434
    ## 507       746        405      413       0         3      88       231
    ## 508       746        405      413       0         3      88       231
    ## 509       746        405      413       0         3      88       231
    ## 510       746        405      413       0         3      88       231
    ## 511      2478       1945     2028     158       120     608      1257
    ## 512      2478       1945     2028     158       120     608      1257
    ## 513      2478       1945     2028     158       120     608      1257
    ## 514      2478       1945     2028     158       120     608      1257
    ## 515       657        373      421      12        13      99       134
    ## 516       657        373      421      12        13      99       134
    ## 517      1561       1450     1616     361        84     411      1212
    ## 518      1561       1450     1616     361        84     411      1212
    ## 519      1561       1450     1616     361        84     411      1212
    ## 520      1561       1450     1616     361        84     411      1212
    ## 521      3225       2038     2050      56       219     587      1037
    ## 522      3225       2038     2050      56       219     587      1037
    ## 523      3225       2038     2050      56       219     587      1037
    ## 524      3225       2038     2050      56       219     587      1037
    ## 525      2434       1328     1337      13        96     448       658
    ## 526      2434       1328     1337      13        96     448       658
    ## 527      2434       1328     1337      13        96     448       658
    ## 528      2434       1328     1337      13        96     448       658
    ## 529      1624       2046     2092     116       440     744      3074
    ## 530      1624       2046     2092     116       440     744      3074
    ## 531      1624       2046     2092     116       440     744      3074
    ## 532      1624       2046     2092     116       440     744      3074
    ## 533      1063       1116     1146     151        54     270       603
    ## 534      1063       1116     1146     151        54     270       603
    ## 535      1063       1116     1146     151        54     270       603
    ## 536      1063       1116     1146     151        54     270       603
    ## 537      1849       4024     4156    1109        90    1183      4464
    ## 538      1849       4024     4156    1109        90    1183      4464
    ## 539      1849       4024     4156    1109        90    1183      4464
    ## 540      1849       4024     4156    1109        90    1183      4464
    ## 541      2047       1529     1482     146       131     333       456
    ## 542      2047       1529     1482     146       131     333       456
    ## 543      2047       1529     1482     146       131     333       456
    ## 544      2047       1529     1482     146       131     333       456
    ## 545      1832       1160     1221      83        52     276       338
    ## 546      1832       1160     1221      83        52     276       338
    ## 547      1832       1160     1221      83        52     276       338
    ## 548      1832       1160     1221      83        52     276       338
    ## 549       260        144      139       0         9      57       202
    ## 550       951        496      491       0        11     130       562
    ## 551       951        496      491       0        11     130       562
    ## 552       951        496      491       0        11     130       562
    ## 553       951        496      491       0        11     130       562
    ## 554      1088        554      589       1        29     121       552
    ## 555      1088        554      589       1        29     121       552
    ## 556      1088        554      589       1        29     121       552
    ## 557      1088        554      589       1        29     121       552
    ## 558      2778       1441     1586      10        85     404      1079
    ## 559      2778       1441     1586      10        85     404      1079
    ## 560      2778       1441     1586      10        85     404      1079
    ## 561      2778       1441     1586      10        85     404      1079
    ## 562      5623       3142     3227      67       260     682      1508
    ## 563      5623       3142     3227      67       260     682      1508
    ## 564      5623       3142     3227      67       260     682      1508
    ## 565      5623       3142     3227      67       260     682      1508
    ## 566      2946       2056     2230     118       206     459       675
    ## 567      2946       2056     2230     118       206     459       675
    ## 568      2946       2056     2230     118       206     459       675
    ## 569      2946       2056     2230     118       206     459       675
    ## 570      2130       1379     1420      34       185     382      1258
    ## 571      2130       1379     1420      34       185     382      1258
    ## 572      2130       1379     1420      34       185     382      1258
    ## 573      2130       1379     1420      34       185     382      1258
    ## 574       746        370      427       0        30     108       394
    ## 575       746        370      427       0        30     108       394
    ## 576      1016        595      634      12        57     155       622
    ## 577      1016        595      634      12        57     155       622
    ## 578      1016        595      634      12        57     155       622
    ## 579      1016        595      634      12        57     155       622
    ## 580      1485        763      784       2         8     203       842
    ## 581      1485        763      784       2         8     203       842
    ## 582      1485        763      784       2         8     203       842
    ## 583      1485        763      784       2         8     203       842
    ## 584      1372        817      801      33        92     290       837
    ## 585      1372        817      801      33        92     290       837
    ## 586      1372        817      801      33        92     290       837
    ## 587      1372        817      801      33        92     290       837
    ## 588       444        223      253       9         2      55       223
    ## 589       358        197      191       0        16      67       182
    ## 590       358        197      191       0        16      67       182
    ## 591       412        195      226       0         3      85       199
    ## 592       412        195      226       0         3      85       199
    ## 593      1179        768      786      38        36     197       230
    ## 594      1179        768      786      38        36     197       230
    ## 595      1179        768      786      38        36     197       230
    ## 596      1179        768      786      38        36     197       230
    ## 597      1636       2208     2341     302       137     663      2182
    ## 598      1636       2208     2341     302       137     663      2182
    ## 599      1636       2208     2341     302       137     663      2182
    ## 600      1636       2208     2341     302       137     663      2182
    ## 601       147       1459     1649     637        99     403      2277
    ## 602       147       1459     1649     637        99     403      2277
    ## 603       147       1459     1649     637        99     403      2277
    ## 604       147       1459     1649     637        99     403      2277
    ## 605      1454       1952     2063     262       126     609      1913
    ## 606      1454       1952     2063     262       126     609      1913
    ## 607      1454       1952     2063     262       126     609      1913
    ## 608      1454       1952     2063     262       126     609      1913
    ## 609      2227       1911     1859     264       127     591      1336
    ## 610      2227       1911     1859     264       127     591      1336
    ## 611      2227       1911     1859     264       127     591      1336
    ## 612      2227       1911     1859     264       127     591      1336
    ## 613      8826       6207     6395     343       332    2177      3445
    ## 614      8826       6207     6395     343       332    2177      3445
    ## 615      8826       6207     6395     343       332    2177      3445
    ## 616      8826       6207     6395     343       332    2177      3445
    ## 617       800        512      540      33        36     178        77
    ## 618       800        512      540      33        36     178        77
    ## 619       800        512      540      33        36     178        77
    ## 620       800        512      540      33        36     178        77
    ## 621      1796       1039     1128      15        30     299       223
    ## 622      1796       1039     1128      15        30     299       223
    ## 623      1796       1039     1128      15        30     299       223
    ## 624      1796       1039     1128      15        30     299       223
    ## 625      5665       4645     4911     596       266    1458      3705
    ## 626      5665       4645     4911     596       266    1458      3705
    ## 627      5665       4645     4911     596       266    1458      3705
    ## 628      5665       4645     4911     596       266    1458      3705
    ## 629      3368       3672     3905    1003       249    1142      4133
    ## 630      3368       3672     3905    1003       249    1142      4133
    ## 631      3368       3672     3905    1003       249    1142      4133
    ## 632      3368       3672     3905    1003       249    1142      4133
    ## 633      3702       4507     4710     611       475    1347      5593
    ## 634      3702       4507     4710     611       475    1347      5593
    ## 635      3702       4507     4710     611       475    1347      5593
    ## 636      3702       4507     4710     611       475    1347      5593
    ## 637      1149        750      814      87         1     298       363
    ## 638      1149        750      814      87         1     298       363
    ## 639      1149        750      814      87         1     298       363
    ## 640      1149        750      814      87         1     298       363
    ## 641      1557       2006     2185       0       155     599      2553
    ## 642      1557       2006     2185       0       155     599      2553
    ## 643      1557       2006     2185       0       155     599      2553
    ## 644      1557       2006     2185       0       155     599      2553
    ## 645      4110       2423     2652     167       132     795       797
    ## 646      4110       2423     2652     167       132     795       797
    ## 647      4110       2423     2652     167       132     795       797
    ## 648      4110       2423     2652     167       132     795       797
    ## 649      4120       3810     4019     221       373    1464      1670
    ## 650      4120       3810     4019     221       373    1464      1670
    ## 651      4120       3810     4019     221       373    1464      1670
    ## 652      4120       3810     4019     221       373    1464      1670
    ## 653      2173       1488     1540     105       119     458       488
    ## 654      2173       1488     1540     105       119     458       488
    ## 655      2173       1488     1540     105       119     458       488
    ## 656      2173       1488     1540     105       119     458       488
    ## 657      1553       1379     1347     149        92     394       903
    ## 658      1553       1379     1347     149        92     394       903
    ## 659      1553       1379     1347     149        92     394       903
    ## 660      1553       1379     1347     149        92     394       903
    ## 661      2655       1733     1895      77        98     579       872
    ## 662      2655       1733     1895      77        98     579       872
    ## 663      2655       1733     1895      77        98     579       872
    ## 664      2655       1733     1895      77        98     579       872
    ## 665      2367       1552     1728     109        75     467       565
    ## 666      2367       1552     1728     109        75     467       565
    ## 667      2367       1552     1728     109        75     467       565
    ## 668      2367       1552     1728     109        75     467       565
    ## 669      3395       1951     2154      26       110     638       652
    ## 670      3395       1951     2154      26       110     638       652
    ## 671      3395       1951     2154      26       110     638       652
    ## 672      3395       1951     2154      26       110     638       652
    ## 673      1983       2928     3143       0       105     841         0
    ## 674      1983       2928     3143       0       105     841         0
    ## 675      1983       2928     3143       0       105     841         0
    ## 676      1983       2928     3143       0       105     841         0
    ## 677      6747       4174     4220     134       168    1281       843
    ## 678      6747       4174     4220     134       168    1281       843
    ## 679      6747       4174     4220     134       168    1281       843
    ## 680      6747       4174     4220     134       168    1281       843
    ## 681        68        139      128       0        10      32         0
    ## 682        68        139      128       0        10      32         0
    ## 683       416        737      707     273        82     192       999
    ## 684       416        737      707     273        82     192       999
    ## 685       416        737      707     273        82     192       999
    ## 686       416        737      707     273        82     192       999
    ## 687       413        249      233       1        12      44       222
    ## 688       413        249      233       1        12      44       222
    ## 689       413        249      233       1        12      44       222
    ## 690       413        249      233       1        12      44       222
    ## 691       907        507      561      13        37     142       409
    ## 692       907        507      561      13        37     142       409
    ## 693       907        507      561      13        37     142       409
    ## 694       907        507      561      13        37     142       409
    ## 695      1429        758      743       1        26     211       739
    ## 696      1429        758      743       1        26     211       739
    ## 697      1429        758      743       1        26     211       739
    ## 698      1429        758      743       1        26     211       739
    ## 699       703        376      380       1        23     112       444
    ## 700       703        376      380       1        23     112       444
    ## 701       857        445      456       1        11     138       524
    ## 702       857        445      456       1        11     138       524
    ## 703       857        445      456       1        11     138       524
    ## 704       857        445      456       1        11     138       524
    ## 705       772        428      424       4        32     109       357
    ## 706       772        428      424       4        32     109       357
    ## 707       772        428      424       4        32     109       357
    ## 708       772        428      424       4        32     109       357
    ## 709      3365       2574     2693     210       530     715      1877
    ## 710      3365       2574     2693     210       530     715      1877
    ## 711      3365       2574     2693     210       530     715      1877
    ## 712      3365       2574     2693     210       530     715      1877
    ## 713       952        585      602      21       129     131       342
    ## 714       952        585      602      21       129     131       342
    ## 715       952        585      602      21       129     131       342
    ## 716       952        585      602      21       129     131       342
    ## 717      3422       3282     3314     506       593    1326      3635
    ## 718      3422       3282     3314     506       593    1326      3635
    ## 719      3422       3282     3314     506       593    1326      3635
    ## 720      3422       3282     3314     506       593    1326      3635
    ## 721      1163        827      840      14        76     226       665
    ## 722      1163        827      840      14        76     226       665
    ## 723      1163        827      840      14        76     226       665
    ## 724      1163        827      840      14        76     226       665
    ## 725      1532       1001     1034      48        86     313       557
    ## 726      1532       1001     1034      48        86     313       557
    ## 727      1532       1001     1034      48        86     313       557
    ## 728      1532       1001     1034      48        86     313       557
    ## 729      1847       1349     1482      64        73     387       991
    ## 730      1847       1349     1482      64        73     387       991
    ## 731      1847       1349     1482      64        73     387       991
    ## 732      1847       1349     1482      64        73     387       991
    ## 733      1777       1003     1070       3       125     368       959
    ## 734      1777       1003     1070       3       125     368       959
    ## 735      1777       1003     1070       3       125     368       959
    ## 736      1777       1003     1070       3       125     368       959
    ## 737      2916       1516     1620      14        60     490      1152
    ## 738      2916       1516     1620      14        60     490      1152
    ## 739      2916       1516     1620      14        60     490      1152
    ## 740      2916       1516     1620      14        60     490      1152
    ## 741      1014        502      570       0        28     187       483
    ## 742      1014        502      570       0        28     187       483
    ## 743      1014        502      570       0        28     187       483
    ## 744      1014        502      570       0        28     187       483
    ## 745      2146       1181     1202       6       119     488       999
    ## 746      2146       1181     1202       6       119     488       999
    ## 747      2146       1181     1202       6       119     488       999
    ## 748      2146       1181     1202       6       119     488       999
    ## 749      1726        899      969      11        51     231       819
    ## 750      1726        899      969      11        51     231       819
    ## 751      1726        899      969      11        51     231       819
    ## 752       784        378      446       5         7     150       423
    ## 753       784        378      446       5         7     150       423
    ## 754       784        378      446       5         7     150       423
    ## 755       784        378      446       5         7     150       423
    ## 756      1130        764      820      28        48     217       190
    ## 757      1130        764      820      28        48     217       190
    ## 758      1130        764      820      28        48     217       190
    ## 759      1130        764      820      28        48     217       190
    ## 760      1266       1122     1146      67       109     335       688
    ## 761      1266       1122     1146      67       109     335       688
    ## 762      1266       1122     1146      67       109     335       688
    ## 763      1266       1122     1146      67       109     335       688
    ## 764      1215        841      940      23       107     186       160
    ## 765      1215        841      940      23       107     186       160
    ## 766      1215        841      940      23       107     186       160
    ## 767      1215        841      940      23       107     186       160
    ## 768      1336       1068     1258      44       116     273       219
    ## 769      1336       1068     1258      44       116     273       219
    ## 770      1336       1068     1258      44       116     273       219
    ## 771      1336       1068     1258      44       116     273       219
    ## 772       212        838      880      90        28     281       962
    ## 773       212        838      880      90        28     281       962
    ## 774       212        838      880      90        28     281       962
    ## 775       212        838      880      90        28     281       962
    ## 776      2348       1821     1801     315        88     465       698
    ## 777      2348       1821     1801     315        88     465       698
    ## 778      2348       1821     1801     315        88     465       698
    ## 779      2348       1821     1801     315        88     465       698
    ## 780      1508        907      996      48        29     309       195
    ## 781      1508        907      996      48        29     309       195
    ## 782      1508        907      996      48        29     309       195
    ## 783      1508        907      996      48        29     309       195
    ## 784       749        681      706      55        62     168       235
    ## 785       749        681      706      55        62     168       235
    ## 786       749        681      706      55        62     168       235
    ## 787       749        681      706      55        62     168       235
    ## 788      1283        818      877      41        65     315       223
    ## 789      1283        818      877      41        65     315       223
    ## 790      1283        818      877      41        65     315       223
    ## 791      1283        818      877      41        65     315       223
    ## 792       364       3866     4015     577        94    1264      5474
    ## 793       364       3866     4015     577        94    1264      5474
    ## 794       364       3866     4015     577        94    1264      5474
    ## 795       364       3866     4015     577        94    1264      5474
    ## 796      2697       5136     5630    1279       140    1529      5870
    ## 797      2697       5136     5630    1279       140    1529      5870
    ## 798      2697       5136     5630    1279       140    1529      5870
    ## 799      2697       5136     5630    1279       140    1529      5870
    ## 800       775        523      523      28        37     161       195
    ## 801       775        523      523      28        37     161       195
    ## 802       775        523      523      28        37     161       195
    ## 803       775        523      523      28        37     161       195
    ## 804      1902       1398     1515      22       215     291       314
    ## 805      1902       1398     1515      22       215     291       314
    ## 806      1902       1398     1515      22       215     291       314
    ## 807      1902       1398     1515      22       215     291       314
    ## 808       983        763      809      80        91     151       219
    ## 809       983        763      809      80        91     151       219
    ## 810       983        763      809      80        91     151       219
    ## 811       983        763      809      80        91     151       219
    ## 812      3022       2297     2448      86       310     455         0
    ## 813      3022       2297     2448      86       310     455         0
    ## 814      3022       2297     2448      86       310     455         0
    ## 815      3022       2297     2448      86       310     455         0
    ## 816      2384       1400     1466      35        55     481       305
    ## 817      2384       1400     1466      35        55     481       305
    ## 818      2384       1400     1466      35        55     481       305
    ## 819      2384       1400     1466      35        55     481       305
    ## 820       899        455      481       3         3     112       422
    ## 821       899        455      481       3         3     112       422
    ## 822       899        455      481       3         3     112       422
    ## 823       899        455      481       3         3     112       422
    ## 824       799        457      415       8        26      87       415
    ## 825       799        457      415       8        26      87       415
    ## 826       799        457      415       8        26      87       415
    ## 827       799        457      415       8        26      87       415
    ## 828       621        317      359       0        23     140       384
    ## 829       621        317      359       0        23     140       384
    ## 830       621        317      359       0        23     140       384
    ## 831       621        317      359       0        23     140       384

``` r
result |>
  group_by()
```

    ## # A tibble: 831 × 25
    ##    district county year_reported number_overweight percent_overweight
    ##       <dbl> <chr>          <dbl>             <dbl>              <dbl>
    ##  1    10402 ALBANY          2019                28               11.4
    ##  2    10402 ALBANY          2019                35               13.9
    ##  3    10402 ALBANY          2019                21               23.6
    ##  4    10402 ALBANY          2019                 9                9.9
    ##  5    10500 ALBANY          2019                16               20  
    ##  6    10500 ALBANY          2019                36               20  
    ##  7    10500 ALBANY          2019                39               17.7
    ##  8    10500 ALBANY          2019                18               16.7
    ##  9    10601 ALBANY          2019                70               13.8
    ## 10    10601 ALBANY          2019                81               16.5
    ## # ℹ 821 more rows
    ## # ℹ 20 more variables: number_obese <dbl>, percent_obese <dbl>,
    ## #   number_overweight_or_obese <dbl>, percent_overweight_or_obese <dbl>,
    ## #   grade_level <chr>, number_healthy_weight <dbl>,
    ## #   percent_healthy_weight <dbl>, sex <chr>, year <dbl>, num_asian <dbl>,
    ## #   num_black <dbl>, num_hisp <dbl>, num_am_ind <dbl>, num_white <dbl>,
    ## #   num_female <dbl>, num_male <dbl>, num_lep <dbl>, num_multi <dbl>, …

See the unique grade_level:

``` r
unique(result$grade_level)
```

    ## [1] "ELEMENTARY"  "MIDDLE/HIGH"

``` r
#write.csv(result, file = "result.csv", row.names = FALSE)
```
