R Notebook
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.

## Library

``` r
options(scipen = 999)
library(tidymodels)
library(tidyverse)
library(janitor)
library(vip)
library(skimr)
library(reshape2)
```

\#import data

``` r
boston <- read_csv("boston_train.csv") %>% clean_names()
```

    ## Rows: 9959 Columns: 33
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (16): own_occ, structure_class, r_bldg_styl, r_roof_typ, r_ext_fin, r_bt...
    ## dbl (17): pid, zipcode, av_total, land_sf, yr_built, yr_remod, living_area, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
kaggle <- read_csv("boston_holdout.csv") %>% clean_names()
```

    ## Rows: 4266 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (16): own_occ, structure_class, r_bldg_styl, r_roof_typ, r_ext_fin, r_bt...
    ## dbl (16): pid, zipcode, land_sf, yr_built, yr_remod, living_area, num_floors...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(boston)
```

    ## # A tibble: 6 × 33
    ##     pid zipcode own_occ av_total land_sf yr_bu…¹ yr_re…² livin…³ num_f…⁴ struc…⁵
    ##   <dbl>   <dbl> <chr>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>  
    ## 1    10    2136 Y         321200   10288    1992       0    1681     1   R      
    ## 2    40    2124 Y         450500    3187    1900    2001    1868     2   R      
    ## 3    70    2130 Y         794300    6007    1962    2001    2146     2   R      
    ## 4    80    2136 Y         342100    5500    1949    2002    1357     1.5 R      
    ## 5    90    2124 Y         444700    3026    1900       0    2346     2.5 R      
    ## 6   100    2131 Y         518500    4950    1900    2012    2032     2   R      
    ## # … with 23 more variables: r_bldg_styl <chr>, r_roof_typ <chr>,
    ## #   r_ext_fin <chr>, r_total_rms <dbl>, r_bdrms <dbl>, r_full_bth <dbl>,
    ## #   r_half_bth <dbl>, r_bth_style <chr>, r_kitch <dbl>, r_kitch_style <chr>,
    ## #   r_heat_typ <chr>, r_ac <chr>, r_fplace <dbl>, r_ext_cnd <chr>,
    ## #   r_ovrall_cnd <chr>, r_int_cnd <chr>, r_int_fin <chr>, r_view <chr>,
    ## #   zip <chr>, population <dbl>, pop_density <dbl>, median_income <dbl>,
    ## #   city_state <chr>, and abbreviated variable names ¹​yr_built, ²​yr_remod, …
    ## # ℹ Use `colnames()` to see all variable names

``` r
boston %>% skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 9959       |
| Number of columns                                | 33         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 16         |
| numeric                                          | 17         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim_variable   | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:----------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| own_occ         |         0 |             1 |   1 |   1 |     0 |        2 |          0 |
| structure_class |         0 |             1 |   1 |   1 |     0 |        1 |          0 |
| r_bldg_styl     |         0 |             1 |   2 |   2 |     0 |       16 |          0 |
| r_roof_typ      |         0 |             1 |   1 |   1 |     0 |        7 |          0 |
| r_ext_fin       |         0 |             1 |   1 |   1 |     0 |       10 |          0 |
| r_bth_style     |         0 |             1 |   1 |   1 |     0 |        4 |          0 |
| r_kitch_style   |         0 |             1 |   1 |   1 |     0 |        4 |          0 |
| r_heat_typ      |         0 |             1 |   1 |   1 |     0 |        7 |          0 |
| r_ac            |         0 |             1 |   1 |   1 |     0 |        3 |          0 |
| r_ext_cnd       |         0 |             1 |   1 |   1 |     0 |        5 |          0 |
| r_ovrall_cnd    |         0 |             1 |   1 |   1 |     0 |        5 |          0 |
| r_int_cnd       |         0 |             1 |   1 |   1 |     0 |        5 |          0 |
| r_int_fin       |         0 |             1 |   1 |   1 |     0 |        2 |          0 |
| r_view          |         0 |             1 |   1 |   1 |     0 |        5 |          0 |
| zip             |         0 |             1 |   5 |   5 |     0 |        5 |          0 |
| city_state      |         0 |             1 |  13 |  21 |     0 |        5 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |        sd |     p0 |      p25 |    p50 |    p75 |    p100 | hist  |
|:--------------|----------:|--------------:|----------:|----------:|-------:|---------:|-------:|-------:|--------:|:------|
| pid           |         0 |          1.00 |  71093.60 |  41011.82 |     10 |  35780.0 |  71180 | 106575 |  142240 | ▇▇▇▇▇ |
| zipcode       |         0 |          1.00 |   2131.45 |      3.69 |   2124 |   2131.0 |   2132 |   2132 |    2136 | ▃▁▆▇▆ |
| av_total      |         0 |          1.00 | 448563.64 | 147761.17 | 134800 | 347100.0 | 418700 | 510150 | 1097100 | ▂▇▂▁▁ |
| land_sf       |         0 |          1.00 |   5936.18 |   2954.01 |    920 |   4319.0 |   5320 |   6756 |  107158 | ▇▁▁▁▁ |
| yr_built      |         0 |          1.00 |   1933.40 |     35.28 |      0 |   1910.0 |   1931 |   1955 |    2016 | ▁▁▁▁▇ |
| yr_remod      |       347 |          0.97 |    675.71 |    946.59 |      0 |      0.0 |      0 |   1997 |    2016 | ▇▁▁▁▅ |
| living_area   |         0 |          1.00 |   1659.00 |    545.95 |    332 |   1300.0 |   1554 |   1914 |    8623 | ▇▂▁▁▁ |
| num_floors    |         0 |          1.00 |      1.73 |      0.45 |      1 |      1.5 |      2 |      2 |       3 | ▃▂▇▁▁ |
| r_total_rms   |         0 |          1.00 |      7.11 |      1.55 |      3 |      6.0 |      7 |      8 |      17 | ▁▇▂▁▁ |
| r_bdrms       |         0 |          1.00 |      3.34 |      0.92 |      0 |      3.0 |      3 |      4 |       9 | ▁▇▃▁▁ |
| r_full_bth    |         0 |          1.00 |      1.35 |      0.56 |      1 |      1.0 |      1 |      2 |       6 | ▇▁▁▁▁ |
| r_half_bth    |         0 |          1.00 |      0.56 |      0.55 |      0 |      0.0 |      1 |      1 |      10 | ▇▁▁▁▁ |
| r_kitch       |         0 |          1.00 |      1.02 |      0.14 |      1 |      1.0 |      1 |      1 |       3 | ▇▁▁▁▁ |
| r_fplace      |         0 |          1.00 |      0.60 |      0.62 |      0 |      0.0 |      1 |      1 |       5 | ▇▁▁▁▁ |
| population    |         0 |          1.00 |  34871.56 |   6299.30 |  28488 |  29826.0 |  35401 |  36314 |   47783 | ▇▂▆▁▃ |
| pop_density   |         0 |          1.00 |  11368.99 |   3293.58 |   6207 |  10618.0 |  11505 |  13251 |   15913 | ▆▁▆▇▃ |
| median_income |         0 |          1.00 |  65984.07 |   9749.72 |  48841 |  58890.0 |  66735 |  75446 |   75730 | ▃▅▁▃▇ |

``` r
sprintf("rice rule bins=%d",floor((9959^(1/3))*2))
```

    ## [1] "rice rule bins=43"

``` r
ggplot(boston, aes(x = av_total)) + 
  geom_histogram(bins = 43, col= "white") +
  labs(title=" Histogram of assessed value for property",x="av_total",y="count")
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
boston %>%
  ggplot(aes(y=av_total))+
  geom_boxplot()+
  labs(title="box plot of assessed value for property",y="av_total")
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
ggplot(boston, aes(x = av_total)) + 
  geom_histogram(bins = 43, col= "white") +
  scale_x_log10() +
  labs(title="Histogram Log of assessed value for property",x="av_total",y="log of count")
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
boston<-boston %>%
  mutate(yr_remod = replace_na(yr_remod,0))%>%
  mutate(age=if_else(yr_remod>yr_built, 2022-yr_remod,2022-yr_built))

skim(boston)
```

|                                                  |        |
|:-------------------------------------------------|:-------|
| Name                                             | boston |
| Number of rows                                   | 9959   |
| Number of columns                                | 34     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |        |
| Column type frequency:                           |        |
| character                                        | 16     |
| numeric                                          | 18     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |        |
| Group variables                                  | None   |

Data summary

**Variable type: character**

| skim_variable   | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:----------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| own_occ         |         0 |             1 |   1 |   1 |     0 |        2 |          0 |
| structure_class |         0 |             1 |   1 |   1 |     0 |        1 |          0 |
| r_bldg_styl     |         0 |             1 |   2 |   2 |     0 |       16 |          0 |
| r_roof_typ      |         0 |             1 |   1 |   1 |     0 |        7 |          0 |
| r_ext_fin       |         0 |             1 |   1 |   1 |     0 |       10 |          0 |
| r_bth_style     |         0 |             1 |   1 |   1 |     0 |        4 |          0 |
| r_kitch_style   |         0 |             1 |   1 |   1 |     0 |        4 |          0 |
| r_heat_typ      |         0 |             1 |   1 |   1 |     0 |        7 |          0 |
| r_ac            |         0 |             1 |   1 |   1 |     0 |        3 |          0 |
| r_ext_cnd       |         0 |             1 |   1 |   1 |     0 |        5 |          0 |
| r_ovrall_cnd    |         0 |             1 |   1 |   1 |     0 |        5 |          0 |
| r_int_cnd       |         0 |             1 |   1 |   1 |     0 |        5 |          0 |
| r_int_fin       |         0 |             1 |   1 |   1 |     0 |        2 |          0 |
| r_view          |         0 |             1 |   1 |   1 |     0 |        5 |          0 |
| zip             |         0 |             1 |   5 |   5 |     0 |        5 |          0 |
| city_state      |         0 |             1 |  13 |  21 |     0 |        5 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |        sd |     p0 |      p25 |    p50 |    p75 |    p100 | hist  |
|:--------------|----------:|--------------:|----------:|----------:|-------:|---------:|-------:|-------:|--------:|:------|
| pid           |         0 |             1 |  71093.60 |  41011.82 |     10 |  35780.0 |  71180 | 106575 |  142240 | ▇▇▇▇▇ |
| zipcode       |         0 |             1 |   2131.45 |      3.69 |   2124 |   2131.0 |   2132 |   2132 |    2136 | ▃▁▆▇▆ |
| av_total      |         0 |             1 | 448563.64 | 147761.17 | 134800 | 347100.0 | 418700 | 510150 | 1097100 | ▂▇▂▁▁ |
| land_sf       |         0 |             1 |   5936.18 |   2954.01 |    920 |   4319.0 |   5320 |   6756 |  107158 | ▇▁▁▁▁ |
| yr_built      |         0 |             1 |   1933.40 |     35.28 |      0 |   1910.0 |   1931 |   1955 |    2016 | ▁▁▁▁▇ |
| yr_remod      |         0 |             1 |    652.17 |    938.17 |      0 |      0.0 |      0 |   1995 |    2016 | ▇▁▁▁▃ |
| living_area   |         0 |             1 |   1659.00 |    545.95 |    332 |   1300.0 |   1554 |   1914 |    8623 | ▇▂▁▁▁ |
| num_floors    |         0 |             1 |      1.73 |      0.45 |      1 |      1.5 |      2 |      2 |       3 | ▃▂▇▁▁ |
| r_total_rms   |         0 |             1 |      7.11 |      1.55 |      3 |      6.0 |      7 |      8 |      17 | ▁▇▂▁▁ |
| r_bdrms       |         0 |             1 |      3.34 |      0.92 |      0 |      3.0 |      3 |      4 |       9 | ▁▇▃▁▁ |
| r_full_bth    |         0 |             1 |      1.35 |      0.56 |      1 |      1.0 |      1 |      2 |       6 | ▇▁▁▁▁ |
| r_half_bth    |         0 |             1 |      0.56 |      0.55 |      0 |      0.0 |      1 |      1 |      10 | ▇▁▁▁▁ |
| r_kitch       |         0 |             1 |      1.02 |      0.14 |      1 |      1.0 |      1 |      1 |       3 | ▇▁▁▁▁ |
| r_fplace      |         0 |             1 |      0.60 |      0.62 |      0 |      0.0 |      1 |      1 |       5 | ▇▁▁▁▁ |
| population    |         0 |             1 |  34871.56 |   6299.30 |  28488 |  29826.0 |  35401 |  36314 |   47783 | ▇▂▆▁▃ |
| pop_density   |         0 |             1 |  11368.99 |   3293.58 |   6207 |  10618.0 |  11505 |  13251 |   15913 | ▆▁▆▇▃ |
| median_income |         0 |             1 |  65984.07 |   9749.72 |  48841 |  58890.0 |  66735 |  75446 |   75730 | ▃▅▁▃▇ |
| age           |         0 |             1 |     63.80 |     39.80 |      6 |     22.0 |     67 |     97 |     297 | ▇▇▁▁▁ |

``` r
kaggle<-kaggle%>%
  mutate(yr_remod = replace_na(yr_remod,0))%>%
  mutate(age=if_else(yr_remod>yr_built, 2022-yr_remod,2022-yr_built))
```

# explore category variables

``` r
boston_character<-select_if(boston, is.character)

for (col in colnames(boston_character)){
    print(boston%>%
    ggplot(aes(y=!!as.name(col),x=av_total))+
    geom_boxplot()+
    labs(title=paste("comparing",col,"with av_total"),x="av_total",y=col))

}
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-7.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-8.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-9.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-10.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-11.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-12.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-13.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-14.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-15.png)<!-- -->![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-5-16.png)<!-- -->

# explore numeric variables

``` r
boston%>%
  select_if(is.numeric) %>%
  select(-pid,-zipcode,-yr_built,-yr_remod) %>%
  cor() %>%
  melt() %>%
  filter(Var1 =="av_total") %>%
  ggplot(aes(Var1,Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446")+
  geom_text(aes(label=round(value,3)), color="white")+
  labs(title = "all variable's correlation")
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
boston%>%
  ggplot(aes(x=living_area,y=av_total))+
  geom_point()+
  geom_smooth(method=lm)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
boston%>%
  ggplot(aes(x=av_total , y=factor(median_income)))+
  geom_boxplot()
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
boston%>%
  ggplot(aes(x=age,y=av_total))+
  geom_point()+
  geom_smooth(method=lm)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
boston%>%
  filter(yr_built>0 & yr_remod==0)%>%
  ggplot(aes(x=yr_built,y=av_total,na.rm=TRUE))+ 
  geom_point()+
  geom_smooth(method=lm)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

``` r
  ggplot(boston[which(boston$yr_remod>0),],aes(x=yr_remod,y=av_total,na.rm=TRUE))+ 
  geom_point()+
    geom_smooth(method=lm)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->

``` r
model_built<-lm(av_total~ yr_built, data= boston[(boston$yr_built>0 & boston$yr_remod==0),])  
summary(model_built)
```

    ## 
    ## Call:
    ## lm(formula = av_total ~ yr_built, data = boston[(boston$yr_built > 
    ##     0 & boston$yr_remod == 0), ])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -297043  -89149  -25116   54843  681975 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept) 870490.98  104016.22   8.369 < 0.0000000000000002 ***
    ## yr_built      -230.87      53.69  -4.300            0.0000173 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 132400 on 6712 degrees of freedom
    ## Multiple R-squared:  0.002748,   Adjusted R-squared:  0.002599 
    ## F-statistic: 18.49 on 1 and 6712 DF,  p-value: 0.0000173

``` r
model_age<-lm(av_total~ age, data = boston)
summary(model_age)
```

    ## 
    ## Call:
    ## lm(formula = av_total ~ age, data = boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -303632  -98860  -27986   62225  683184 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value            Pr(>|t|)    
    ## (Intercept) 492271.03    2749.75  179.02 <0.0000000000000002 ***
    ## age           -685.12      36.57  -18.73 <0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 145200 on 9957 degrees of freedom
    ## Multiple R-squared:  0.03405,    Adjusted R-squared:  0.03395 
    ## F-statistic:   351 on 1 and 9957 DF,  p-value: < 0.00000000000000022

``` r
model_remod<-lm(av_total~ yr_remod, data= boston[which(boston$yr_remod>0),])  
summary(model_remod)
```

    ## 
    ## Call:
    ## lm(formula = av_total ~ yr_remod, data = boston[which(boston$yr_remod > 
    ##     0), ])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -314370 -110054  -33091   70091  657243 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value            Pr(>|t|)    
    ## (Intercept) -3410940.2   397633.5  -8.578 <0.0000000000000002 ***
    ## yr_remod        1954.5      198.7   9.838 <0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 160700 on 3243 degrees of freedom
    ## Multiple R-squared:  0.02898,    Adjusted R-squared:  0.02868 
    ## F-statistic: 96.79 on 1 and 3243 DF,  p-value: < 0.00000000000000022

``` r
set.seed(1)
# Save the split information for an 80/20 split of the data
bsplit <- initial_split(boston, prop = 0.8)
train <- training(bsplit) 
test  <-  testing(bsplit)

# Kfold cross validation
kfold_splits <- vfold_cv(train, v=5)
```

``` r
# write out the formula 
## Change variables here
boston_recipe <-
  recipe(av_total ~ land_sf + living_area + age + num_floors + own_occ+ city_state + r_ovrall_cnd +
           r_int_cnd +r_total_rms+median_income+r_full_bth+r_ext_cnd+r_kitch_style+r_bth_style+r_ext_fin+r_bldg_styl+r_half_bth+r_roof_typ
           , data = train) %>%
  step_impute_median(all_numeric_predictors()) %>% # missing values numeric 
  step_novel(all_nominal_predictors()) %>% # new factor levels 
  step_unknown(all_nominal_predictors()) %>% # missing values 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors())

## Check the recipe results m
bake(boston_recipe %>% prep(),train %>% sample_n(1000))
```

    ## # A tibble: 1,000 × 37
    ##    land_sf livin…¹   age num_f…² r_tot…³ media…⁴ r_ful…⁵ r_hal…⁶ av_to…⁷ own_o…⁸
    ##      <dbl>   <dbl> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <int>
    ##  1    5500    1116    55       1       6   75446       1       0  360200       1
    ##  2    6500    3264   112       2       9   75446       1       1  706800       0
    ##  3    4371    1142    77       1       7   58890       2       0  311400       0
    ##  4    5500    1244    72       1       6   48841       1       1  339400       0
    ##  5    6349    1500     7       2       8   75446       1       1  522100       0
    ##  6    5029     972    63       1       5   75730       1       0  473000       0
    ##  7    5998    1493    21       2       7   66735       1       1  404700       0
    ##  8    6596    1470    52       2       7   75446       1       1  419200       0
    ##  9    5378    1800     9       2       6   75446       2       1  543700       0
    ## 10    4072     936   102       1       5   75446       1       1  374400       0
    ## # … with 990 more rows, 27 more variables: own_occ_Y <int>,
    ## #   city_state_Cambridge..MA <int>, city_state_Dorchester.Center..MA <int>,
    ## #   city_state_Hyde.Park..MA <int>, city_state_Jamaica.Plain..MA <int>,
    ## #   city_state_Roslindale..MA <int>, r_ovrall_cnd_A <int>,
    ## #   r_ovrall_cnd_G <int>, r_int_cnd_A <int>, r_int_cnd_G <int>,
    ## #   r_ext_cnd_A <int>, r_ext_cnd_G <int>, r_kitch_style_M <int>,
    ## #   r_kitch_style_N <int>, r_kitch_style_S <int>, r_bth_style_M <int>, …
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
## every thing should be numeric, regression problems, don't make av_total a factor
```

``` r
lm_model <- linear_reg(mixture = 1, penalty = 0.01) %>%
  set_engine("glm") %>%  #other engine such as glm, lm dont have mixture and penalty, lm is the basic one
  set_mode("regression") 

lm_wflow <-workflow() %>%
  add_recipe(boston_recipe) %>%
  add_model(lm_model) %>%
  fit(train)

tidy(lm_wflow) %>%
  mutate_if(is.numeric,round,4)
```

    ## # A tibble: 37 × 5
    ##    term             estimate std.error statistic p.value
    ##    <chr>               <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)   -1033262.   30982.      -33.4     0    
    ##  2 land_sf              8.27     0.273    30.3     0    
    ##  3 living_area        102.       2.27     45.0     0    
    ##  4 age                 -6.19    22.5      -0.275   0.783
    ##  5 num_floors       -3130.    3226.       -0.970   0.332
    ##  6 r_total_rms       2827.     687.        4.12    0    
    ##  7 median_income       18.5      0.344    53.7     0    
    ##  8 r_full_bth       18760.    1628.       11.5     0    
    ##  9 r_half_bth       20543.    1502.       13.7     0    
    ## 10 own_occ_N       -10422.    2304.       -4.52    0    
    ## # … with 27 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

``` r
lm_wflow %>%
  pull_workflow_fit() %>%
  vi() %>%
  mutate(Importance = if_else(Sign=="NEG",-Importance,Importance))%>%
  ggplot(aes(x=reorder(Variable,Importance),y=Importance,fill=Sign))+
  geom_col()+
  coord_flip()+
  labs(title="linear regression model importance plot")
```

    ## Warning: `pull_workflow_fit()` was deprecated in workflows 0.2.3.
    ## ℹ Please use `extract_fit_parsnip()` instead.

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
#It's a regression model, do not have type of prob or class, only have type of numeric  
bind_cols(
  predict(lm_wflow,train,type="numeric"), train) %>% 
 mutate(part="train")->score_lm_train
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
bind_cols(
  predict(lm_wflow,test), test) %>% 
  mutate(part="test")->score_lm_test
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
bind_rows(score_lm_train,score_lm_test)%>%
  group_by(part) %>%
  metrics(av_total,.pred)%>%
  pivot_wider(id_cols = part,names_from = .metric,values_from = .estimate)
```

    ## # A tibble: 2 × 4
    ##   part    rmse   rsq    mae
    ##   <chr>  <dbl> <dbl>  <dbl>
    ## 1 test  60798. 0.821 44383.
    ## 2 train 65295. 0.807 46487.

# XG boost

``` r
xgb_model <- boost_tree(trees=tune(), 
                        learn_rate = tune(),
                        tree_depth = tune()) %>%
  set_engine("xgboost",
             importance="permutation") %>%
  set_mode("regression")


xgb_wflow <-workflow() %>%
  add_recipe(boston_recipe) %>%
  add_model(xgb_model)


xgb_search_res <- xgb_wflow %>% 
  tune_bayes(
    resamples = kfold_splits,
    # Generate five at semi-random to start
    initial = 5,
    iter = 10, 
    # How to measure performance?
    metrics = metric_set(rmse, rsq),
    control = control_bayes(no_improve = 10, verbose = TRUE)
  )# if no improvement after 10 iterations, stop grid
```

    ## 

    ## ❯  Generating a set of 5 initial parameter results

    ## ✓ Initialization complete

    ## 

    ## 

    ## ── Iteration 1 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=54750 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 5000 candidates

    ## i Predicted candidates

    ## i trees=640, tree_depth=2, learn_rate=0.00717

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=66130 (+/-816)

    ## 

    ## ── Iteration 2 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=54750 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 5000 candidates

    ## i Predicted candidates

    ## i trees=1122, tree_depth=11, learn_rate=0.24

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=59030 (+/-892)

    ## 

    ## ── Iteration 3 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=54750 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 5000 candidates

    ## i Predicted candidates

    ## i trees=2000, tree_depth=9, learn_rate=0.00698

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=55970 (+/-744)

    ## 

    ## ── Iteration 4 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=54750 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 5000 candidates

    ## i Predicted candidates

    ## i trees=510, tree_depth=15, learn_rate=0.00471

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=75420 (+/-857)

    ## 

    ## ── Iteration 5 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=54750 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 5000 candidates

    ## i Predicted candidates

    ## i trees=607, tree_depth=7, learn_rate=0.112

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=56440 (+/-974)

    ## 

    ## ── Iteration 6 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=54750 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 5000 candidates

    ## i Predicted candidates

    ## i trees=513, tree_depth=1, learn_rate=0.247

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=64050 (+/-819)

    ## 

    ## ── Iteration 7 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=54750 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 5000 candidates

    ## i Predicted candidates

    ## i trees=1827, tree_depth=4, learn_rate=0.00102

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=99500 (+/-782)

    ## 

    ## ── Iteration 8 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=54750 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 5000 candidates

    ## i Predicted candidates

    ## i trees=367, tree_depth=10, learn_rate=0.314

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=59160 (+/-1160)

    ## 

    ## ── Iteration 9 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=54750 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 5000 candidates

    ## i Predicted candidates

    ## i trees=96, tree_depth=15, learn_rate=0.00819

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=225000 (+/-587)

    ## 

    ## ── Iteration 10 ────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=54750 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 5000 candidates

    ## i Predicted candidates

    ## i trees=766, tree_depth=1, learn_rate=0.308

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=63930 (+/-885)

    ## ! No improvement for 10 iterations; returning current results.

``` r
xgb_search_res %>%
  collect_metrics()%>%
  filter(.metric == "rmse")
```

    ## # A tibble: 15 × 10
    ##    trees tree_depth learn_r…¹ .metric .esti…²   mean     n std_err .config .iter
    ##    <int>      <int>     <dbl> <chr>   <chr>    <dbl> <int>   <dbl> <chr>   <int>
    ##  1   897          3   0.0772  rmse    standa… 5.48e4     5   1000. Prepro…     0
    ##  2  1640          5   0.295   rmse    standa… 5.85e4     5    787. Prepro…     0
    ##  3   440          8   0.00932 rmse    standa… 5.71e4     5    642. Prepro…     0
    ##  4    40         10   0.0157  rmse    standa… 2.59e5     5    597. Prepro…     0
    ##  5  1530         15   0.00180 rmse    standa… 6.81e4     5    819. Prepro…     0
    ##  6   640          2   0.00717 rmse    standa… 6.61e4     5    816. Iter1       1
    ##  7  1122         11   0.240   rmse    standa… 5.90e4     5    892. Iter2       2
    ##  8  2000          9   0.00698 rmse    standa… 5.60e4     5    744. Iter3       3
    ##  9   510         15   0.00471 rmse    standa… 7.54e4     5    857. Iter4       4
    ## 10   607          7   0.112   rmse    standa… 5.64e4     5    974. Iter5       5
    ## 11   513          1   0.247   rmse    standa… 6.41e4     5    819. Iter6       6
    ## 12  1827          4   0.00102 rmse    standa… 9.95e4     5    782. Iter7       7
    ## 13   367         10   0.314   rmse    standa… 5.92e4     5   1159. Iter8       8
    ## 14    96         15   0.00819 rmse    standa… 2.25e5     5    587. Iter9       9
    ## 15   766          1   0.308   rmse    standa… 6.39e4     5    885. Iter10     10
    ## # … with abbreviated variable names ¹​learn_rate, ²​.estimator

``` r
# Graph of learning rate 
xgb_search_res %>%
  collect_metrics() %>%
  ggplot(aes(learn_rate, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# graph of tree depth 
xgb_search_res %>%
  collect_metrics() %>%
  ggplot(aes(tree_depth, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
# graph of number of trees 
xgb_search_res %>%
  collect_metrics() %>%
  ggplot(aes(trees, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
lowest_xgb_rmse <- xgb_search_res %>%
  select_best("rmse")
lowest_xgb_rmse
```

    ## # A tibble: 1 × 4
    ##   trees tree_depth learn_rate .config             
    ##   <int>      <int>      <dbl> <chr>               
    ## 1   897          3     0.0772 Preprocessor1_Model1

``` r
xgb_wflow <- finalize_workflow(
  xgb_wflow, lowest_xgb_rmse
) %>% 
  fit(train)
```

    ## [12:02:29] WARNING: amalgamation/../src/learner.cc:627: 
    ## Parameters: { "importance" } might not be used.
    ## 
    ##   This could be a false alarm, with some parameters getting used by language bindings but
    ##   then being mistakenly passed down to XGBoost core, or some parameter actually being used
    ##   but getting flagged wrongly here. Please open an issue if you find any such cases.

## VIP

What variables are important

``` r
xgb_wflow %>%
  extract_fit_parsnip() %>%
  vi()%>%
  ggplot(aes(x=reorder(Variable,Importance),y=Importance))+
  geom_col()+
  coord_flip()+
  labs(title="xgb model importance plot")
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# use these models replug to your linear regression formula

xgb_wflow %>%
  extract_fit_parsnip() %>%
  vip(10)+
  labs(title="xgb modeltop 10 importance plot")
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
bind_cols(
  predict(xgb_wflow,train), train) %>% 
  metrics(av_total,.pred)%>%
  mutate(part="train")
```

    ## # A tibble: 3 × 4
    ##   .metric .estimator .estimate part 
    ##   <chr>   <chr>          <dbl> <chr>
    ## 1 rmse    standard   42716.    train
    ## 2 rsq     standard       0.918 train
    ## 3 mae     standard   31877.    train

``` r
bind_cols(
  predict(xgb_wflow,test), test) %>% 
  metrics(av_total,.pred)%>%
  mutate(part="test")
```

    ## # A tibble: 3 × 4
    ##   .metric .estimator .estimate part 
    ##   <chr>   <chr>          <dbl> <chr>
    ## 1 rmse    standard   50485.    test 
    ## 2 rsq     standard       0.876 test 
    ## 3 mae     standard   37148.    test

## Best Worst Predicitons

You should have one best and two worst predictions

1.  the properties that you under-estimate the value of
2.  the properties that you over-estimate the value of
3.  the properties that are your best-estimate

``` r
bind_cols(predict(xgb_wflow,test),test) %>%
  mutate(error = av_total - .pred,
         abs_error = abs(error)) %>% 
  slice_min(order_by = abs_error,n=10) ->best_estimate

best_estimate
```

    ## # A tibble: 10 × 37
    ##     .pred    pid zipcode own_occ av_to…¹ land_sf yr_bu…² yr_re…³ livin…⁴ num_f…⁵
    ##     <dbl>  <dbl>   <dbl> <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 3.43e5  65330    2136 Y        342800    5000    1910    1990    1432     2  
    ##  2 4.08e5 132370    2131 Y        408300    3619    1880    2001    1303     2  
    ##  3 3.17e5  50130    2136 Y        316500    6272    1950       0    1208     1  
    ##  4 4.47e5 134230    2136 Y        447400    8875    1874       0    2724     3  
    ##  5 4.55e5  31650    2132 Y        455600    5350    1934    2003    1476     2  
    ##  6 4.16e5 122840    2132 N        415900    6221    1925       0    1512     1  
    ##  7 3.24e5  48270    2136 Y        324000    5000    1955       0    1224     1.5
    ##  8 3.72e5 124520    2131 Y        372500    5677    1904       0    1135     2  
    ##  9 3.09e5  84230    2136 Y        308500    6494    1961       0    1224     1.5
    ## 10 3.47e5  80980    2132 Y        347300    4000    1928       0    1121     1.5
    ## # … with 27 more variables: structure_class <chr>, r_bldg_styl <chr>,
    ## #   r_roof_typ <chr>, r_ext_fin <chr>, r_total_rms <dbl>, r_bdrms <dbl>,
    ## #   r_full_bth <dbl>, r_half_bth <dbl>, r_bth_style <chr>, r_kitch <dbl>,
    ## #   r_kitch_style <chr>, r_heat_typ <chr>, r_ac <chr>, r_fplace <dbl>,
    ## #   r_ext_cnd <chr>, r_ovrall_cnd <chr>, r_int_cnd <chr>, r_int_fin <chr>,
    ## #   r_view <chr>, zip <chr>, population <dbl>, pop_density <dbl>,
    ## #   median_income <dbl>, city_state <chr>, age <dbl>, error <dbl>, …
    ## # ℹ Use `colnames()` to see all variable names

``` r
# best estimate 
bind_cols(predict(xgb_wflow,test),test) %>%
  mutate(error = av_total - .pred,
         ) %>% 
  slice_max(order_by = error,n=10) ->under_estimate

under_estimate
```

    ## # A tibble: 10 × 36
    ##     .pred    pid zipcode own_occ av_to…¹ land_sf yr_bu…² yr_re…³ livin…⁴ num_f…⁵
    ##     <dbl>  <dbl>   <dbl> <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 7.81e5 118250    2130 Y       1052900    5004    1910       0    3397     2  
    ##  2 6.72e5  93320    2130 Y        909000    2074    1875    1990    2566     3  
    ##  3 7.08e5  55920    2124 Y        938600    9173    1996    2001    2987     2.5
    ##  4 5.06e5   6530    2130 Y        732300    4377    1950       0    1584     2  
    ##  5 6.37e5 122490    2131 Y        835500    4500    1900       0    2096     2  
    ##  6 5.05e5 116510    2132 Y        702800   10000    1945    1970    1988     1  
    ##  7 9.23e5  22560    2130 Y       1090500    5286    1898       0    3164     2  
    ##  8 6.02e5  40200    2130 Y        767700    5507    1965       0    1387     1.5
    ##  9 4.49e5  55260    2130 Y        609500    2668    1950       0    1210     1.5
    ## 10 5.18e5  31240    2132 Y        676500    8562    1920    1990    3436     2.5
    ## # … with 26 more variables: structure_class <chr>, r_bldg_styl <chr>,
    ## #   r_roof_typ <chr>, r_ext_fin <chr>, r_total_rms <dbl>, r_bdrms <dbl>,
    ## #   r_full_bth <dbl>, r_half_bth <dbl>, r_bth_style <chr>, r_kitch <dbl>,
    ## #   r_kitch_style <chr>, r_heat_typ <chr>, r_ac <chr>, r_fplace <dbl>,
    ## #   r_ext_cnd <chr>, r_ovrall_cnd <chr>, r_int_cnd <chr>, r_int_fin <chr>,
    ## #   r_view <chr>, zip <chr>, population <dbl>, pop_density <dbl>,
    ## #   median_income <dbl>, city_state <chr>, age <dbl>, error <dbl>, and …
    ## # ℹ Use `colnames()` to see all variable names

``` r
# overly simplistic evaluation 


# worst over-estimate 
bind_cols(predict(xgb_wflow,test),test)%>%
  mutate(error = .pred-av_total ) %>% 
  slice_max(order_by = error,n=10) -> overesimate

overesimate
```

    ## # A tibble: 10 × 36
    ##     .pred    pid zipcode own_occ av_to…¹ land_sf yr_bu…² yr_re…³ livin…⁴ num_f…⁵
    ##     <dbl>  <dbl>   <dbl> <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 8.29e5 106350    2130 N        549800    6945    1890       0    2492     2.5
    ##  2 9.16e5  38940    2130 Y        657900    9000    1915       0    2594     2  
    ##  3 7.25e5  42880    2131 Y        508664   12203    1900    2014    2225     2  
    ##  4 7.94e5  48890    2130 Y        583500    4257    1900    1998    2487     2  
    ##  5 5.13e5 128090    2124 Y        308900    7930    1885       0    2415     2  
    ##  6 7.69e5  56490    2130 Y        575600    7412    1895    2010    2305     2  
    ##  7 6.95e5 128570    2130 Y        505600    4303    1946    2005    2046     2  
    ##  8 6.87e5  43280    2130 N        507700    4740    1925    2001    1868     2  
    ##  9 6.35e5 125420    2130 N        470700    3150    1899    2004    1863     2  
    ## 10 5.15e5  92410    2131 Y        351800    7280    1910    2007    2150     2  
    ## # … with 26 more variables: structure_class <chr>, r_bldg_styl <chr>,
    ## #   r_roof_typ <chr>, r_ext_fin <chr>, r_total_rms <dbl>, r_bdrms <dbl>,
    ## #   r_full_bth <dbl>, r_half_bth <dbl>, r_bth_style <chr>, r_kitch <dbl>,
    ## #   r_kitch_style <chr>, r_heat_typ <chr>, r_ac <chr>, r_fplace <dbl>,
    ## #   r_ext_cnd <chr>, r_ovrall_cnd <chr>, r_int_cnd <chr>, r_int_fin <chr>,
    ## #   r_view <chr>, zip <chr>, population <dbl>, pop_density <dbl>,
    ## #   median_income <dbl>, city_state <chr>, age <dbl>, error <dbl>, and …
    ## # ℹ Use `colnames()` to see all variable names

``` r
# overly simplistic evaluation 

best_estimate %>% 
  summarize(
    mean(error),
    mean(av_total),
            mean(yr_built))
```

    ## # A tibble: 1 × 3
    ##   `mean(error)` `mean(av_total)` `mean(yr_built)`
    ##           <dbl>            <dbl>            <dbl>
    ## 1         -7.84           373880            1922.

``` r
overesimate %>% 
  summarize(
    mean(error),
    mean(av_total),
            mean(yr_built))
```

    ## # A tibble: 1 × 3
    ##   `mean(error)` `mean(av_total)` `mean(yr_built)`
    ##           <dbl>            <dbl>            <dbl>
    ## 1       205827.          502016.            1906.

``` r
under_estimate%>%
    summarize(
    mean(error),
    mean(av_total),
            mean(yr_built))
```

    ## # A tibble: 1 × 3
    ##   `mean(error)` `mean(av_total)` `mean(yr_built)`
    ##           <dbl>            <dbl>            <dbl>
    ## 1       201386.           831530            1931.

## KAGGLE

``` r
bind_cols(predict(xgb_wflow,kaggle),kaggle) %>%
  select(pid,av_total = .pred) %>% 
  write_csv("project3_kaggle_50484.csv")
```

\#Random Forest

``` r
rf_model <- rand_forest(trees=tune()) %>%
  set_engine("ranger",
             importance="permutation") %>%
  set_mode("regression")


rf_wflow <-workflow() %>%
  add_recipe(boston_recipe) %>%
  add_model(rf_model)


rf_search_res <- rf_wflow %>% 
  tune_bayes(
    resamples = kfold_splits,
    # Generate five at semi-random to start
    initial = 5,
    iter = 10, 
    # How to measure performance?
    metrics = metric_set(rmse, rsq),
    control = control_bayes(no_improve = 10, verbose = TRUE)
  )
```

    ## 

    ## ❯  Generating a set of 5 initial parameter results

    ## ✓ Initialization complete

    ## 

    ## 

    ## ── Iteration 1 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=57190 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 1995 candidates

    ## i Predicted candidates

    ## i trees=1334

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=57230 (+/-809)

    ## 

    ## ── Iteration 2 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=57190 (@iter 0)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 1994 candidates

    ## i Predicted candidates

    ## i trees=1492

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ♥ Newest results:    rmse=57180 (+/-803)

    ## 

    ## ── Iteration 3 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=57180 (@iter 2)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 1993 candidates

    ## i Predicted candidates

    ## i trees=1532

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=57200 (+/-790)

    ## 

    ## ── Iteration 4 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=57180 (@iter 2)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 1992 candidates

    ## i Predicted candidates

    ## i trees=1472

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ♥ Newest results:    rmse=57150 (+/-799)

    ## 

    ## ── Iteration 5 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=57150 (@iter 4)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 1991 candidates

    ## i Predicted candidates

    ## i trees=1596

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ♥ Newest results:    rmse=57130 (+/-786)

    ## 

    ## ── Iteration 6 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=57130 (@iter 5)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 1990 candidates

    ## i Predicted candidates

    ## i trees=1622

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=57210 (+/-785)

    ## 

    ## ── Iteration 7 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=57130 (@iter 5)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 1989 candidates

    ## i Predicted candidates

    ## i trees=1175

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=57210 (+/-784)

    ## 

    ## ── Iteration 8 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=57130 (@iter 5)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 1988 candidates

    ## i Predicted candidates

    ## i trees=401

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=57380 (+/-852)

    ## 

    ## ── Iteration 9 ─────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=57130 (@iter 5)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 1987 candidates

    ## i Predicted candidates

    ## i trees=78

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=57730 (+/-882)

    ## 

    ## ── Iteration 10 ────────────────────────────────────────────────────────────────

    ## 

    ## i Current best:      rmse=57130 (@iter 5)

    ## i Gaussian process model

    ## ✓ Gaussian process model

    ## i Generating 1986 candidates

    ## i Predicted candidates

    ## i trees=1253

    ## i Estimating performance

    ## i Fold1: preprocessor 1/1

    ## ✓ Fold1: preprocessor 1/1

    ## i Fold1: preprocessor 1/1, model 1/1

    ## ✓ Fold1: preprocessor 1/1, model 1/1

    ## i Fold1: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold2: preprocessor 1/1

    ## ✓ Fold2: preprocessor 1/1

    ## i Fold2: preprocessor 1/1, model 1/1

    ## ✓ Fold2: preprocessor 1/1, model 1/1

    ## i Fold2: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold3: preprocessor 1/1

    ## ✓ Fold3: preprocessor 1/1

    ## i Fold3: preprocessor 1/1, model 1/1

    ## ✓ Fold3: preprocessor 1/1, model 1/1

    ## i Fold3: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold4: preprocessor 1/1

    ## ✓ Fold4: preprocessor 1/1

    ## i Fold4: preprocessor 1/1, model 1/1

    ## ✓ Fold4: preprocessor 1/1, model 1/1

    ## i Fold4: preprocessor 1/1, model 1/1 (predictions)

    ## i Fold5: preprocessor 1/1

    ## ✓ Fold5: preprocessor 1/1

    ## i Fold5: preprocessor 1/1, model 1/1

    ## ✓ Fold5: preprocessor 1/1, model 1/1

    ## i Fold5: preprocessor 1/1, model 1/1 (predictions)

    ## ✓ Estimating performance

    ## ⓧ Newest results:    rmse=57280 (+/-769)

``` r
rf_search_res %>%
  collect_metrics()%>%
  filter(.metric == "rmse")
```

    ## # A tibble: 15 × 8
    ##    trees .metric .estimator   mean     n std_err .config              .iter
    ##    <int> <chr>   <chr>       <dbl> <int>   <dbl> <chr>                <int>
    ##  1   262 rmse    standard   57285.     5    832. Preprocessor1_Model1     0
    ##  2  1432 rmse    standard   57188.     5    784. Preprocessor1_Model2     0
    ##  3  1871 rmse    standard   57274.     5    794. Preprocessor1_Model3     0
    ##  4   595 rmse    standard   57321.     5    786. Preprocessor1_Model4     0
    ##  5   909 rmse    standard   57296.     5    785. Preprocessor1_Model5     0
    ##  6  1334 rmse    standard   57230.     5    809. Iter1                    1
    ##  7  1492 rmse    standard   57181.     5    803. Iter2                    2
    ##  8  1532 rmse    standard   57204.     5    790. Iter3                    3
    ##  9  1472 rmse    standard   57151.     5    799. Iter4                    4
    ## 10  1596 rmse    standard   57132.     5    786. Iter5                    5
    ## 11  1622 rmse    standard   57210.     5    785. Iter6                    6
    ## 12  1175 rmse    standard   57214.     5    784. Iter7                    7
    ## 13   401 rmse    standard   57383.     5    852. Iter8                    8
    ## 14    78 rmse    standard   57734.     5    882. Iter9                    9
    ## 15  1253 rmse    standard   57281.     5    769. Iter10                  10

``` r
lowest_rf_rmse <- rf_search_res %>%
  select_best("rmse")
lowest_rf_rmse
```

    ## # A tibble: 1 × 2
    ##   trees .config
    ##   <int> <chr>  
    ## 1  1596 Iter5

``` r
rf_wflow <- finalize_workflow(
  rf_wflow, lowest_rf_rmse
) %>% 
  fit(train)
```

## VIP

What variables are important

``` r
rf_wflow %>%
  extract_fit_parsnip() %>%
  vi()%>%
  ggplot(aes(x=reorder(Variable,Importance),y=Importance))+
  geom_col()+
  coord_flip()+
  labs(title="random forest model importance plot")
```

![](project3_Shi_Shi_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
# use these models replug to your linear regression formula
```

``` r
bind_cols(
  predict(rf_wflow,train), train) %>% 
  metrics(av_total,.pred)%>%
  mutate(part="train")
```

    ## # A tibble: 3 × 4
    ##   .metric .estimator .estimate part 
    ##   <chr>   <chr>          <dbl> <chr>
    ## 1 rmse    standard   30779.    train
    ## 2 rsq     standard       0.962 train
    ## 3 mae     standard   22548.    train

``` r
bind_cols(
  predict(rf_wflow,test), test) %>% 
  metrics(av_total,.pred)%>%
  mutate(part="test")
```

    ## # A tibble: 3 × 4
    ##   .metric .estimator .estimate part 
    ##   <chr>   <chr>          <dbl> <chr>
    ## 1 rmse    standard   52859.    test 
    ## 2 rsq     standard       0.869 test 
    ## 3 mae     standard   38426.    test

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or
by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output
will be saved alongside it (click the *Preview* button or press
*Cmd+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.
