knn_classification
================
Shi Shi
10/24/2022

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.

## Load Libraries

``` r
library(tidyverse)
library(tidymodels)
library(janitor)
library(skimr)
library(vip)
library(kknn)
```

## Import Data

``` r
Churn <- read_csv("Churn_training.csv") %>% clean_names()
```

    ## Rows: 90901 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (22): email_domain, phone_model, billing_city, billing_postal, billing_...
    ## dbl  (11): monthly_minutes, customer_service_calls, streaming_minutes, total...
    ## date  (1): customer_reg_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Churn %>% head()
```

    ## # A tibble: 6 × 34
    ##   monthly_m…¹ custo…² strea…³ total…⁴ prev_…⁵ late_…⁶ ip_ad…⁷ phone…⁸ customer…⁹
    ##         <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <date>    
    ## 1       22604       2   26525     285      47       5   16767     253 2020-04-15
    ## 2       17059       2   16887     201      45       5   53966     241 2020-04-12
    ## 3       25848       2   26783     264      44       6   19278     232 2020-02-22
    ## 4       22080       3   23649     274      49       6   16680     236 2020-08-05
    ## 5       23871       3    7705     236      61       5   51308     243 2019-11-16
    ## 6       28098       3   12062     307      58       6   48219     243 2020-01-24
    ## # … with 25 more variables: email_domain <chr>, phone_model <chr>,
    ## #   billing_city <chr>, billing_postal <chr>, billing_state <chr>,
    ## #   partner <chr>, phone_service <chr>, multiple_lines <chr>,
    ## #   streaming_plan <chr>, mobile_hotspot <chr>, wifi_calling_text <chr>,
    ## #   online_backup <chr>, device_protection <chr>, number_phones <dbl>,
    ## #   contract_code <chr>, currency_code <chr>, maling_code <chr>,
    ## #   paperless_billing <chr>, payment_method <chr>, customer_id <chr>, …
    ## # ℹ Use `colnames()` to see all variable names

``` r
Churn %>% skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 90901      |
| Number of columns                                | 34         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 22         |
| Date                                             | 1          |
| numeric                                          | 11         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim_variable     | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:------------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| email_domain      |        28 |             1 |   9 |  11 |     0 |        3 |          0 |
| phone_model       |        25 |             1 |   9 |  29 |     0 |       15 |          0 |
| billing_city      |        29 |             1 |   6 |  24 |     0 |     8140 |          0 |
| billing_postal    |        28 |             1 |   5 |   5 |     0 |     9956 |          0 |
| billing_state     |        26 |             1 |   4 |  14 |     0 |       48 |          0 |
| partner           |        25 |             1 |   2 |   3 |     0 |        2 |          0 |
| phone_service     |        25 |             1 |   2 |   3 |     0 |        2 |          0 |
| multiple_lines    |        24 |             1 |   2 |   3 |     0 |        2 |          0 |
| streaming_plan    |        28 |             1 |   3 |   8 |     0 |        4 |          0 |
| mobile_hotspot    |        36 |             1 |   2 |   3 |     0 |        2 |          0 |
| wifi_calling_text |        32 |             1 |   2 |   3 |     0 |        2 |          0 |
| online_backup     |        29 |             1 |   2 |  18 |     0 |        3 |          0 |
| device_protection |        29 |             1 |   1 |   1 |     0 |       26 |          0 |
| contract_code     |        26 |             1 |   1 |   1 |     0 |       26 |          0 |
| currency_code     |        29 |             1 |   3 |   3 |     0 |        3 |          0 |
| maling_code       |        31 |             1 |   1 |   1 |     0 |       26 |          0 |
| paperless_billing |        31 |             1 |   2 |   3 |     0 |        2 |          0 |
| payment_method    |        24 |             1 |  11 |  16 |     0 |        4 |          0 |
| customer_id       |         0 |             1 |   7 |  20 |     0 |    90901 |          0 |
| billing_address   |        20 |             1 |  10 |  38 |     0 |    90880 |          0 |
| gender            |        27 |             1 |   4 |   6 |     0 |        2 |          0 |
| network_speed     |        27 |             1 |   2 |   5 |     0 |        2 |          0 |

**Variable type: Date**

| skim_variable     | n_missing | complete_rate | min        | max        | median     | n_unique |
|:------------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| customer_reg_date |        27 |             1 | 2019-10-19 | 2020-10-18 | 2020-04-12 |      308 |

**Variable type: numeric**

| skim_variable          | n_missing | complete_rate |     mean |       sd |   p0 |   p25 |   p50 |   p75 |  p100 | hist  |
|:-----------------------|----------:|--------------:|---------:|---------:|-----:|------:|------:|------:|------:|:------|
| monthly_minutes        |        20 |             1 | 19851.97 |  5117.73 |    0 | 16244 | 19694 | 23337 | 43799 | ▁▅▇▂▁ |
| customer_service_calls |        22 |             1 |     1.65 |     0.66 |    0 |     1 |     2 |     2 |     4 | ▁▆▇▁▁ |
| streaming_minutes      |        22 |             1 | 20696.93 |  4988.01 |    0 | 17327 | 20671 | 24023 | 43799 | ▁▃▇▂▁ |
| total_billed           |        34 |             1 |   250.25 |    35.58 |  100 |   226 |   251 |   274 |   399 | ▁▂▇▂▁ |
| prev_balance           |        22 |             1 |    51.46 |    11.92 |    0 |    43 |    51 |    59 |    99 | ▁▂▇▃▁ |
| late_payments          |        20 |             1 |     4.80 |     1.32 |    0 |     4 |     5 |     6 |     9 | ▁▂▇▅▁ |
| ip_address_asn         |        17 |             1 | 34846.93 | 16862.57 | 2013 | 18773 | 26969 | 51472 | 65533 | ▂▇▁▆▃ |
| phone_area_code        |        28 |             1 |   247.56 |    10.66 |  200 |   240 |   248 |   255 |   289 | ▁▂▇▃▁ |
| number_phones          |        30 |             1 |     5.31 |     1.09 |    0 |     5 |     5 |     6 |    10 | ▁▂▇▂▁ |
| senior_citizen         |        35 |             1 |     0.50 |     0.50 |    0 |     0 |     0 |     1 |     1 | ▇▁▁▁▇ |
| churn                  |         0 |             1 |     0.05 |     0.23 |    0 |     0 |     0 |     0 |     1 | ▇▁▁▁▁ |

``` r
churn_kaggle<- read_csv("churn_holdout.csv") %>% clean_names()
```

    ## Rows: 10099 Columns: 33
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (22): email_domain, phone_model, billing_city, billing_postal, billing_...
    ## dbl  (10): monthly_minutes, customer_service_calls, streaming_minutes, total...
    ## date  (1): customer_reg_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
churn_kaggle<- churn_kaggle %>%
  mutate(senior_citizen= as.factor(senior_citizen),
         ip_address_asn=as.factor(ip_address_asn),
         phone_area_code=as.factor(phone_area_code))%>%
  mutate_if(is.character,factor)
```

## Analyze target

``` r
Churn_summary<- Churn %>%
  count(churn)%>%
  mutate(pct=n/sum(n))

Churn_summary %>%
  ggplot(aes(x=factor(churn),y=pct))+
  geom_col()+
  labs(title = "churn summary distribution", x= "churn", y="percentage")+
  geom_text(aes(label=paste(round(pct*100,1),"%")), vjust=1.5, color="white")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## explore data

``` r
Churn %>% skim_to_wide()
```

    ## Warning: 'skim_to_wide' is deprecated.
    ## Use 'skim()' instead.
    ## See help("Deprecated")

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 90901      |
| Number of columns                                | 34         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 22         |
| Date                                             | 1          |
| numeric                                          | 11         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim_variable     | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:------------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| email_domain      |        28 |             1 |   9 |  11 |     0 |        3 |          0 |
| phone_model       |        25 |             1 |   9 |  29 |     0 |       15 |          0 |
| billing_city      |        29 |             1 |   6 |  24 |     0 |     8140 |          0 |
| billing_postal    |        28 |             1 |   5 |   5 |     0 |     9956 |          0 |
| billing_state     |        26 |             1 |   4 |  14 |     0 |       48 |          0 |
| partner           |        25 |             1 |   2 |   3 |     0 |        2 |          0 |
| phone_service     |        25 |             1 |   2 |   3 |     0 |        2 |          0 |
| multiple_lines    |        24 |             1 |   2 |   3 |     0 |        2 |          0 |
| streaming_plan    |        28 |             1 |   3 |   8 |     0 |        4 |          0 |
| mobile_hotspot    |        36 |             1 |   2 |   3 |     0 |        2 |          0 |
| wifi_calling_text |        32 |             1 |   2 |   3 |     0 |        2 |          0 |
| online_backup     |        29 |             1 |   2 |  18 |     0 |        3 |          0 |
| device_protection |        29 |             1 |   1 |   1 |     0 |       26 |          0 |
| contract_code     |        26 |             1 |   1 |   1 |     0 |       26 |          0 |
| currency_code     |        29 |             1 |   3 |   3 |     0 |        3 |          0 |
| maling_code       |        31 |             1 |   1 |   1 |     0 |       26 |          0 |
| paperless_billing |        31 |             1 |   2 |   3 |     0 |        2 |          0 |
| payment_method    |        24 |             1 |  11 |  16 |     0 |        4 |          0 |
| customer_id       |         0 |             1 |   7 |  20 |     0 |    90901 |          0 |
| billing_address   |        20 |             1 |  10 |  38 |     0 |    90880 |          0 |
| gender            |        27 |             1 |   4 |   6 |     0 |        2 |          0 |
| network_speed     |        27 |             1 |   2 |   5 |     0 |        2 |          0 |

**Variable type: Date**

| skim_variable     | n_missing | complete_rate | min        | max        | median     | n_unique |
|:------------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| customer_reg_date |        27 |             1 | 2019-10-19 | 2020-10-18 | 2020-04-12 |      308 |

**Variable type: numeric**

| skim_variable          | n_missing | complete_rate |     mean |       sd |   p0 |   p25 |   p50 |   p75 |  p100 | hist  |
|:-----------------------|----------:|--------------:|---------:|---------:|-----:|------:|------:|------:|------:|:------|
| monthly_minutes        |        20 |             1 | 19851.97 |  5117.73 |    0 | 16244 | 19694 | 23337 | 43799 | ▁▅▇▂▁ |
| customer_service_calls |        22 |             1 |     1.65 |     0.66 |    0 |     1 |     2 |     2 |     4 | ▁▆▇▁▁ |
| streaming_minutes      |        22 |             1 | 20696.93 |  4988.01 |    0 | 17327 | 20671 | 24023 | 43799 | ▁▃▇▂▁ |
| total_billed           |        34 |             1 |   250.25 |    35.58 |  100 |   226 |   251 |   274 |   399 | ▁▂▇▂▁ |
| prev_balance           |        22 |             1 |    51.46 |    11.92 |    0 |    43 |    51 |    59 |    99 | ▁▂▇▃▁ |
| late_payments          |        20 |             1 |     4.80 |     1.32 |    0 |     4 |     5 |     6 |     9 | ▁▂▇▅▁ |
| ip_address_asn         |        17 |             1 | 34846.93 | 16862.57 | 2013 | 18773 | 26969 | 51472 | 65533 | ▂▇▁▆▃ |
| phone_area_code        |        28 |             1 |   247.56 |    10.66 |  200 |   240 |   248 |   255 |   289 | ▁▂▇▃▁ |
| number_phones          |        30 |             1 |     5.31 |     1.09 |    0 |     5 |     5 |     6 |    10 | ▁▂▇▂▁ |
| senior_citizen         |        35 |             1 |     0.50 |     0.50 |    0 |     0 |     0 |     1 |     1 | ▇▁▁▁▇ |
| churn                  |         0 |             1 |     0.05 |     0.23 |    0 |     0 |     0 |     0 |     1 | ▇▁▁▁▁ |

``` r
Churn %>%
  ggplot(aes(x=factor(network_speed),fill=factor(churn)))+
  geom_bar(position = "fill")+
  labs(title = "Churn with diff network speed", x="network speed", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(phone_model),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff phone_model",  x="phone model", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
Churn %>%
  group_by(paperless_billing,churn)%>%
  summarise(n=n())%>%
  mutate(pct=n/sum(n))%>%
  ggplot(aes(x=reorder(paperless_billing,pct),y=n, fill=factor(churn)))+
  geom_col(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w paperless billing or not",  x="paperless billing", y="churn")
```

    ## `summarise()` has grouped output by 'paperless_billing'. You can override using
    ## the `.groups` argument.

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
#####
Churn %>%
  ggplot(aes(x=factor(partner),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff partner",  x="partner", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(phone_service),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff phone_service",  x="phone_service", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(multiple_lines),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff multiple_lines",  x="multiple_lines", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(streaming_plan),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff streaming_plan",  x="streaming_plan", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(mobile_hotspot),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff mobile_hotspot",  x="mobile_hotspot", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-8.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(wifi_calling_text),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff wifi_calling_text",  x="wifi_calling_text", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-9.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(online_backup),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff online_backup",  x="online_backup", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-10.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(device_protection),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff device_protection",  x="device_protection", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-11.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(payment_method),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff payment_method",  x="payment_method", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-12.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(gender),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff gender",  x="gender", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-13.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(email_domain),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff email_domain",  x="email_domain", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-14.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(contract_code),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff contract_code",  x="contract_code", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-15.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(currency_code),fill=factor(churn)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="Churn w diff currency_code",  x="currency_code", y="churn")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-16.png)<!-- -->

``` r
Churn %>% 
  ggplot(aes(x=factor(churn),y=monthly_minutes))+
  geom_boxplot()+
  labs(title="Churn w diff monthly_minutes",  y="monthly_minutes", x="churn")
```

    ## Warning: Removed 20 rows containing non-finite values (stat_boxplot).

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-17.png)<!-- -->

``` r
Churn %>% 
  ggplot(aes(x=factor(churn),y=streaming_minutes))+
  geom_boxplot()+
  labs(title="Churn w diff streaming_minutes",  y="streaming_minutes", x="churn")
```

    ## Warning: Removed 22 rows containing non-finite values (stat_boxplot).

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-18.png)<!-- -->

``` r
Churn %>% 
  ggplot(aes(x=factor(churn),y=total_billed))+
  geom_boxplot()+
  labs(title="Churn w diff total_billed",  y="total_billed", x="churn")
```

    ## Warning: Removed 34 rows containing non-finite values (stat_boxplot).

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-19.png)<!-- -->

``` r
Churn %>% 
  ggplot(aes(x=factor(churn),y=prev_balance))+
  geom_boxplot()+
  labs(title="Churn w diff prev_balance",  y="prev_balance", x="churn")
```

    ## Warning: Removed 22 rows containing non-finite values (stat_boxplot).

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-20.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(churn),y=customer_service_calls))+
  geom_boxplot()+
  labs(title="Churn w diff customer_service_calls",  y="customer_service_calls", x="churn")
```

    ## Warning: Removed 22 rows containing non-finite values (stat_boxplot).

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-21.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(churn),y=late_payments))+
  geom_boxplot()+
  labs(title="Churn w diff late_payments",  y="late_payments", x="churn")
```

    ## Warning: Removed 20 rows containing non-finite values (stat_boxplot).

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-22.png)<!-- -->

``` r
Churn %>%
  ggplot(aes(x=factor(churn),y=number_phones))+
  geom_boxplot()+
  labs(title="Churn w diff number_phones",  y="number_phones", x="churn")
```

    ## Warning: Removed 30 rows containing non-finite values (stat_boxplot).

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-4-23.png)<!-- -->

## Prepare data

``` r
churn_prep<-Churn %>%
  mutate(churn=as.factor(churn),
         senior_citizen= as.factor(senior_citizen),
         ip_address_asn=as.factor(ip_address_asn),
         phone_area_code=as.factor(phone_area_code))%>%
  mutate_if(is.character,factor)

skim(churn_prep)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | churn_prep |
| Number of rows                                   | 90901      |
| Number of columns                                | 34         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| Date                                             | 1          |
| factor                                           | 26         |
| numeric                                          | 7          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: Date**

| skim_variable     | n_missing | complete_rate | min        | max        | median     | n_unique |
|:------------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| customer_reg_date |        27 |             1 | 2019-10-19 | 2020-10-18 | 2020-04-12 |      308 |

**Variable type: factor**

| skim_variable     | n_missing | complete_rate | ordered | n_unique | top_counts                                     |
|:------------------|----------:|--------------:|:--------|---------:|:-----------------------------------------------|
| ip_address_asn    |        17 |             1 | FALSE   |    10570 | 517: 32, 181: 31, 528: 31, 532: 30             |
| phone_area_code   |        28 |             1 | FALSE   |       85 | 247: 3383, 248: 3321, 250: 3261, 249: 3259     |
| email_domain      |        28 |             1 | FALSE   |        3 | gma: 73404, yah: 10756, hot: 6713              |
| phone_model       |        25 |             1 | FALSE   |       15 | Sam: 20715, iPh: 19609, One: 15923, iPh: 11554 |
| billing_city      |        29 |             1 | FALSE   |     8140 | Por: 40, Coo: 38, Eas: 38, Lak: 38             |
| billing_postal    |        28 |             1 | FALSE   |     9956 | 513: 35, 437: 33, 747: 33, 788: 32             |
| billing_state     |        26 |             1 | FALSE   |       48 | Tex: 6204, Mic: 6173, Col: 5990, Was: 5852     |
| partner           |        25 |             1 | FALSE   |        2 | No: 45659, Yes: 45217                          |
| phone_service     |        25 |             1 | FALSE   |        2 | Yes: 52077, No: 38799                          |
| multiple_lines    |        24 |             1 | FALSE   |        2 | Yes: 58569, No: 32308                          |
| streaming_plan    |        28 |             1 | FALSE   |        4 | 3GB: 46961, Unl: 40908, 6GB: 1708, 12G: 1296   |
| mobile_hotspot    |        36 |             1 | FALSE   |        2 | Yes: 49968, No: 40897                          |
| wifi_calling_text |        32 |             1 | FALSE   |        2 | Yes: 52830, No: 38039                          |
| online_backup     |        29 |             1 | FALSE   |        3 | Yes: 70953, No: 13665, No : 6254               |
| device_protection |        29 |             1 | FALSE   |       26 | C: 10754, R: 10326, W: 10083, V: 9423          |
| contract_code     |        26 |             1 | FALSE   |       26 | S: 12528, C: 12380, R: 11276, W: 10506         |
| currency_code     |        29 |             1 | FALSE   |        3 | cad: 75095, eur: 8582, usd: 7195               |
| maling_code       |        31 |             1 | FALSE   |       26 | X: 12949, W: 12825, B: 11717, C: 10852         |
| paperless_billing |        31 |             1 | FALSE   |        2 | No: 47804, Yes: 43066                          |
| payment_method    |        24 |             1 | FALSE   |        4 | Mai: 55636, Ele: 32124, Cre: 2545, Ban: 572    |
| customer_id       |         0 |             1 | FALSE   |    90901 | 0-0: 1, 0-0: 1, 0-0: 1, 0-0: 1                 |
| billing_address   |        20 |             1 | FALSE   |    90880 | 278: 2, 000: 1, 000: 1, 000: 1                 |
| gender            |        27 |             1 | FALSE   |        2 | Fem: 45507, Mal: 45367                         |
| network_speed     |        27 |             1 | FALSE   |        2 | 5G: 45493, 4Gl: 45381                          |
| senior_citizen    |        35 |             1 | FALSE   |        2 | 0: 45471, 1: 45395                             |
| churn             |         0 |             1 | FALSE   |        2 | 0: 85926, 1: 4975                              |

**Variable type: numeric**

| skim_variable          | n_missing | complete_rate |     mean |      sd |  p0 |   p25 |   p50 |   p75 |  p100 | hist  |
|:-----------------------|----------:|--------------:|---------:|--------:|----:|------:|------:|------:|------:|:------|
| monthly_minutes        |        20 |             1 | 19851.97 | 5117.73 |   0 | 16244 | 19694 | 23337 | 43799 | ▁▅▇▂▁ |
| customer_service_calls |        22 |             1 |     1.65 |    0.66 |   0 |     1 |     2 |     2 |     4 | ▁▆▇▁▁ |
| streaming_minutes      |        22 |             1 | 20696.93 | 4988.01 |   0 | 17327 | 20671 | 24023 | 43799 | ▁▃▇▂▁ |
| total_billed           |        34 |             1 |   250.25 |   35.58 | 100 |   226 |   251 |   274 |   399 | ▁▂▇▂▁ |
| prev_balance           |        22 |             1 |    51.46 |   11.92 |   0 |    43 |    51 |    59 |    99 | ▁▂▇▃▁ |
| late_payments          |        20 |             1 |     4.80 |    1.32 |   0 |     4 |     5 |     6 |     9 | ▁▂▇▅▁ |
| number_phones          |        30 |             1 |     5.31 |    1.09 |   0 |     5 |     5 |     6 |    10 | ▁▂▇▂▁ |

## split data

``` r
set.seed(15)
train_test_split <- initial_split(churn_prep,prop = 0.8)
train<- training(train_test_split)
test<- testing(train_test_split)
```

## Define Recipe

``` r
logistic_recipe_19<-recipe(churn ~ total_billed+payment_method+number_phones+streaming_minutes+streaming_plan+prev_balance+monthly_minutes+paperless_billing+partner+multiple_lines+customer_service_calls+mobile_hotspot+gender+late_payments+phone_model+device_protection+email_domain+contract_code+currency_code, data = train ) %>%
  step_impute_median(all_numeric_predictors())%>%
  step_unknown(all_nominal_predictors())%>%
  step_novel(all_nominal_predictors()) %>%
  step_scale(all_numeric_predictors())%>%
  step_dummy(all_nominal_predictors())

logistic_recipe_14<-recipe(churn ~ monthly_minutes+customer_service_calls+streaming_minutes+total_billed+prev_balance+late_payments+phone_model+partner+phone_service+multiple_lines+streaming_plan+mobile_hotspot+wifi_calling_text+number_phones+paperless_billing+payment_method+gender+network_speed, data = train ) %>%
  step_impute_median(all_numeric_predictors())%>%
  step_unknown(all_nominal_predictors())%>%
  step_novel(all_nominal_predictors()) %>%
  step_scale(all_numeric_predictors())%>%
  step_dummy(all_nominal_predictors())
```

## logistic_bake_12

``` r
logistic_model <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")
  
logistic_workflow_2 <- workflow() %>%
  add_recipe(logistic_recipe_19) %>%
  add_model(logistic_model) %>%
  fit(train)

scored_train_logit_2 <- predict(logistic_workflow_2, train, type = "prob") %>%
  bind_cols(predict(logistic_workflow_2,train,type = "class"))%>%
  bind_cols(.,train)
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
scored_test_logit_2 <- predict(logistic_workflow_2, test, type = "prob") %>%
  bind_cols(predict(logistic_workflow_2,test,type = "class"))%>%
  bind_cols(.,test)
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
options(yardstick.event_first = FALSE)
  # -- Metrics: Train and Test 
scored_train_logit_2 %>% 
  metrics(churn, .pred_1, estimate = .pred_class) %>%
  mutate(part="training") %>%
  bind_rows( scored_test_logit_2 %>% 
               metrics(churn, .pred_1, estimate = .pred_class) %>%
               mutate(part="testing") ) %>%
  filter(.metric %in% c('accuracy','roc_auc')) %>%
  pivot_wider(names_from = .metric, values_from=.estimate)
```

    ## Warning: The `yardstick.event_first` option has been deprecated as of yardstick 0.0.7 and will be completely ignored in a future version.
    ## Instead, set the following argument directly in the metric function:
    ## `options(yardstick.event_first = TRUE)`  -> `event_level = 'first'` (the default)
    ## `options(yardstick.event_first = FALSE)` -> `event_level = 'second'`
    ## This warning is displayed once per session.

    ## # A tibble: 2 × 4
    ##   .estimator part     accuracy roc_auc
    ##   <chr>      <chr>       <dbl>   <dbl>
    ## 1 binary     training    0.966   0.924
    ## 2 binary     testing     0.967   0.912

``` r
scored_train_logit_2 %>%
  yardstick::precision(churn,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test_logit_2 %>%
  yardstick::precision(churn,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric   .estimator .estimate part    
    ##   <chr>     <chr>          <dbl> <chr>   
    ## 1 precision binary         0.812 training
    ## 2 precision binary         0.846 testing

``` r
scored_train_logit_2 %>%
  yardstick::recall(churn,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test_logit_2 %>%
  yardstick::recall(churn,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric .estimator .estimate part    
    ##   <chr>   <chr>          <dbl> <chr>   
    ## 1 recall  binary         0.487 training
    ## 2 recall  binary         0.481 testing

``` r
scored_train_logit_2 %>%
  conf_mat(
  truth = churn,
  estimate = .pred_class,
  dnn = c("Prediction", "Truth")
) %>%
  autoplot(type = "heatmap") + 
  labs(title="Training Confusion Matrix")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
scored_test_logit_2 %>%
  conf_mat(
  truth = churn,
  estimate = .pred_class,
  dnn = c("Prediction", "Truth")
) %>%
  autoplot(type = "heatmap") + 
  labs(title="Testing Confusion Matrix")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
logistic_workflow_2 %>%
 pull_workflow_fit() %>%
  tidy() %>%
  mutate_if(is.numeric,round,2)
```

    ## Warning: `pull_workflow_fit()` was deprecated in workflows 0.2.3.
    ## ℹ Please use `extract_fit_parsnip()` instead.

    ## # A tibble: 111 × 5
    ##    term                            estimate std.error statistic p.value
    ##    <chr>                              <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)                         1.28   1079.        0          1
    ##  2 total_billed                       -1.45      0.03    -52.8        0
    ##  3 number_phones                      -0.85      0.02    -35.0        0
    ##  4 streaming_minutes                   0.48      0.02     20.9        0
    ##  5 prev_balance                       -0.47      0.03    -17.4        0
    ##  6 monthly_minutes                     0.54      0.03     19.5        0
    ##  7 customer_service_calls              0.12      0.02      5.07       0
    ##  8 late_payments                      -0.39      0.03    -14.2        0
    ##  9 payment_method_Credit.Card         -5.69      0.21    -27.7        0
    ## 10 payment_method_Electronic.Check    -3.18      0.15    -21.1        0
    ## # … with 101 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

``` r
logistic_workflow_2 %>%
  pull_workflow_fit() %>%
  vip()
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
######
logistic_model <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")
  
logistic_workflow_3 <- workflow() %>%
  add_recipe(logistic_recipe_14) %>%
  add_model(logistic_model) %>%
  fit(train)

scored_train_logit_3 <- predict(logistic_workflow_3, train, type = "prob") %>%
  bind_cols(predict(logistic_workflow_3,train,type = "class"))%>%
  bind_cols(.,train)
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
scored_test_logit_3 <- predict(logistic_workflow_3, test, type = "prob") %>%
  bind_cols(predict(logistic_workflow_3,test,type = "class"))%>%
  bind_cols(.,test)
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
options(yardstick.event_first = FALSE)
  # -- Metrics: Train and Test 
scored_train_logit_3 %>% 
  metrics(churn, .pred_1, estimate = .pred_class) %>%
  mutate(part="training") %>%
  bind_rows( scored_test_logit_3 %>% 
               metrics(churn, .pred_1, estimate = .pred_class) %>%
               mutate(part="testing") ) %>%
  filter(.metric %in% c('accuracy','roc_auc')) %>%
  pivot_wider(names_from = .metric, values_from=.estimate)
```

    ## # A tibble: 2 × 4
    ##   .estimator part     accuracy roc_auc
    ##   <chr>      <chr>       <dbl>   <dbl>
    ## 1 binary     training    0.964   0.914
    ## 2 binary     testing     0.965   0.906

``` r
logistic_workflow_3 %>%
 pull_workflow_fit() %>%
  tidy() %>%
  mutate_if(is.numeric,round,2)
```

    ## # A tibble: 58 × 5
    ##    term                        estimate std.error statistic p.value
    ##    <chr>                          <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 (Intercept)                     2.22    616.        0       1   
    ##  2 monthly_minutes                 0.63      0.03     23.6     0   
    ##  3 customer_service_calls          0.07      0.02      2.9     0   
    ##  4 streaming_minutes               0.51      0.02     23.6     0   
    ##  5 total_billed                   -1.52      0.03    -57.8     0   
    ##  6 prev_balance                   -0.36      0.02    -14.3     0   
    ##  7 late_payments                  -0.46      0.03    -17.5     0   
    ##  8 number_phones                  -0.81      0.02    -34.4     0   
    ##  9 phone_model_Google.Pixel.4a    12.8     616.        0.02    0.98
    ## 10 phone_model_iPhone.11          14.0     616.        0.02    0.98
    ## # … with 48 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

``` r
logistic_workflow_3 %>%
  pull_workflow_fit() %>%
  vip()
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
scored_train_logit_3 %>%
  yardstick::precision(churn,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test_logit_3 %>%
  yardstick::precision(churn,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric   .estimator .estimate part    
    ##   <chr>     <chr>          <dbl> <chr>   
    ## 1 precision binary         0.817 training
    ## 2 precision binary         0.852 testing

``` r
scored_train_logit_3 %>%
  yardstick::recall(churn,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test_logit_3 %>%
  yardstick::recall(churn,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric .estimator .estimate part    
    ##   <chr>   <chr>          <dbl> <chr>   
    ## 1 recall  binary         0.447 training
    ## 2 recall  binary         0.452 testing

``` r
scored_train_logit_3 %>%
  conf_mat(
  truth = churn,
  estimate = .pred_class,
  dnn = c("Prediction", "Truth")
) %>%
  autoplot(type = "heatmap") + 
  labs(title="Training Confusion Matrix")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
scored_test_logit_3 %>%
  conf_mat(
  truth = churn,
  estimate = .pred_class,
  dnn = c("Prediction", "Truth")
) %>%
  autoplot(type = "heatmap") + 
  labs(title="Testing Confusion Matrix")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
scored_kaggle<- predict(logistic_workflow_3, churn_kaggle,type="class") %>%
  bind_cols(.,churn_kaggle)%>%
  select(customer_id , churn=.pred_class)
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
scored_kaggle %>%
  write_csv("logistic_0.9676.csv")
```

``` r
knn_recipe<-recipe(churn ~ monthly_minutes+customer_service_calls+streaming_minutes+total_billed+prev_balance+late_payments+phone_model+partner+phone_service+multiple_lines+streaming_plan+mobile_hotspot+wifi_calling_text+number_phones+paperless_billing+payment_method+gender+network_speed, data = train ) %>%
  step_impute_median(all_numeric_predictors())%>%
  step_unknown(all_nominal_predictors())%>%
  step_novel(all_nominal_predictors()) %>%
  step_scale(all_numeric_predictors())%>%
  step_dummy(all_nominal_predictors())
```

## Bake

``` r
knn_bake<-bake(knn_recipe %>% prep(), train, composition = "tibble")
knn_bake
```

    ## # A tibble: 72,720 × 58
    ##    month…¹ custo…² strea…³ total…⁴ prev_…⁵ late_…⁶ numbe…⁷ churn phone…⁸ phone…⁹
    ##      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <fct>   <int>   <int>
    ##  1    3.55    1.51    2.42    5.18    2.10    3.02    2.74 0           0       0
    ##  2    5.14    3.02    2.92    6.93    4.19    4.52    6.40 0           0       0
    ##  3    3.47    3.02    3.45    7.74    3.02    2.26    4.57 0           0       0
    ##  4    3.37    1.51    3.15    7.12    4.70    3.02    4.57 0           0       0
    ##  5    3.05    3.02    2.91    6.36    3.19    2.26    4.57 0           0       0
    ##  6    3.09    1.51    5.87    6.64    4.19    3.77    5.49 0           0       0
    ##  7    3.59    3.02    6.15    7.52    2.93    4.52    4.57 0           0       0
    ##  8    5.11    3.02    3.11    7.60    4.61    3.77    5.49 0           0       0
    ##  9    5.19    1.51    4.31    7.66    3.27    3.77    4.57 0           0       0
    ## 10    6.58    3.02    5.20    7.77    3.27    5.28    2.74 0           0       0
    ## # … with 72,710 more rows, 48 more variables:
    ## #   phone_model_iPhone.11.Pro...Pro.Max <int>,
    ## #   phone_model_iPhone.SE.2020 <int>, phone_model_iPhone.XR <int>,
    ## #   phone_model_iPhone.XS <int>, phone_model_Moto.G8.Power <int>,
    ## #   phone_model_OnePlus.7.Pro <int>, phone_model_OnePlus.8.Pro <int>,
    ## #   phone_model_Samsung.Galaxy.Note.20.Ultra <int>,
    ## #   phone_model_Samsung.Galaxy.S10...S10.Plus <int>, …
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

## Define Model

``` r
knn_model <- nearest_neighbor(neighbors = 10 ) %>%
  set_mode("classification")%>%
  set_engine("kknn")

knn_model
```

    ## K-Nearest Neighbor Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   neighbors = 10
    ## 
    ## Computational engine: kknn

``` r
knn_workflow_churn <- workflow()%>%
  add_recipe(knn_recipe) %>%
  add_model(knn_model)

knn_fit_churn <- knn_workflow_churn %>%
  fit(data=train)

knn_fit_churn
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: nearest_neighbor()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 5 Recipe Steps
    ## 
    ## • step_impute_median()
    ## • step_unknown()
    ## • step_novel()
    ## • step_scale()
    ## • step_dummy()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## 
    ## Call:
    ## kknn::train.kknn(formula = ..y ~ ., data = data, ks = min_rows(10,     data, 5))
    ## 
    ## Type of response variable: nominal
    ## Minimal misclassification: 0.02874037
    ## Best kernel: optimal
    ## Best k: 10

## Scoreing train & test

``` r
scored_train <- predict(knn_fit_churn,train, type = "prob") %>%
  bind_cols(predict(knn_fit_churn,train,type = "class"))%>%
  bind_cols(.,train)

scored_test <- predict(knn_fit_churn, test, type = "prob") %>%
  bind_cols(predict(knn_fit_churn,test,type = "class"))%>%
  bind_cols(.,test)
```

### Metrics

``` r
options(yardstick.event_first = FALSE)
  # -- Metrics: Train and Test 
scored_train %>% 
  metrics(churn, .pred_1, estimate = .pred_class) %>%
  mutate(part="training") %>%
  bind_rows( scored_test %>% 
               metrics(churn, .pred_1, estimate = .pred_class) %>%
               mutate(part="testing") ) %>%
  filter(.metric %in% c('accuracy','roc_auc')) %>%
  pivot_wider(names_from = .metric, values_from=.estimate)
```

    ## # A tibble: 2 × 4
    ##   .estimator part     accuracy roc_auc
    ##   <chr>      <chr>       <dbl>   <dbl>
    ## 1 binary     training    0.982   0.999
    ## 2 binary     testing     0.972   0.899

``` r
scored_train %>%
  yardstick::precision(churn,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test %>%
  yardstick::precision(churn,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric   .estimator .estimate part    
    ##   <chr>     <chr>          <dbl> <chr>   
    ## 1 precision binary         0.989 training
    ## 2 precision binary         0.945 testing

``` r
scored_train %>%
  yardstick::recall(churn,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test %>%
  yardstick::recall(churn,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric .estimator .estimate part    
    ##   <chr>   <chr>          <dbl> <chr>   
    ## 1 recall  binary         0.674 training
    ## 2 recall  binary         0.517 testing

``` r
scored_train %>%
  conf_mat(
  truth = churn,
  estimate = .pred_class,
  dnn = c("Prediction", "Truth")
) %>%
  autoplot(type = "heatmap") + 
  labs(title="Training Confusion Matrix")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
scored_test %>%
  conf_mat(
  truth = churn,
  estimate = .pred_class,
  dnn = c("Prediction", "Truth")
) %>%
  autoplot(type = "heatmap") + 
  labs(title="Testing Confusion Matrix")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
scored_kaggle<- predict(knn_fit_churn, churn_kaggle,type="class") %>%
  bind_cols(.,churn_kaggle)%>%
  select(customer_id , churn=.pred_class)

scored_kaggle %>%
  write_csv("logistic_0.973.csv")
```

\###logistic regression lasso

``` r
lasso_spec <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
```

``` r
lasso_workflow <- workflow() %>%
  add_recipe(logistic_recipe_19) %>%
  add_model(lasso_spec) %>%
  fit(train)
```

``` r
scored_train_lasso <- predict(lasso_workflow, train, type = "prob") %>%
  bind_cols(predict(lasso_workflow,train,type = "class"))%>%
  bind_cols(.,train)

scored_test_lasso <- predict(lasso_workflow, test, type = "prob") %>%
  bind_cols(predict(lasso_workflow,test,type = "class"))%>%
  bind_cols(.,test)
```

``` r
options(yardstick.event_first = FALSE)
  # -- Metrics: Train and Test 
scored_train_lasso %>% 
  metrics(churn, .pred_1, estimate = .pred_class) %>%
  mutate(part="training") %>%
  bind_rows( scored_test_lasso %>% 
               metrics(churn, .pred_1, estimate = .pred_class) %>%
               mutate(part="testing") ) %>%
  filter(.metric %in% c('accuracy','roc_auc')) %>%
  pivot_wider(names_from = .metric, values_from=.estimate)
```

    ## # A tibble: 2 × 4
    ##   .estimator part     accuracy roc_auc
    ##   <chr>      <chr>       <dbl>   <dbl>
    ## 1 binary     training    0.953   0.902
    ## 2 binary     testing     0.953   0.896

``` r
lasso_workflow %>%
 pull_workflow_fit() %>%
  tidy() %>%
  mutate_if(is.numeric,round,2)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Loaded glmnet 4.1-4

    ## # A tibble: 111 × 3
    ##    term                            estimate penalty
    ##    <chr>                              <dbl>   <dbl>
    ##  1 (Intercept)                         4.5     0.01
    ##  2 total_billed                       -1.02    0.01
    ##  3 number_phones                      -0.48    0.01
    ##  4 streaming_minutes                   0.3     0.01
    ##  5 prev_balance                       -0.02    0.01
    ##  6 monthly_minutes                     0.25    0.01
    ##  7 customer_service_calls              0       0.01
    ##  8 late_payments                       0       0.01
    ##  9 payment_method_Credit.Card         -0.04    0.01
    ## 10 payment_method_Electronic.Check     0       0.01
    ## # … with 101 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

``` r
lasso_workflow %>%
  pull_workflow_fit() %>%
  vip()
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
scored_train_lasso %>%
  yardstick::precision(churn,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test_lasso %>%
  yardstick::precision(churn,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric   .estimator .estimate part    
    ##   <chr>     <chr>          <dbl> <chr>   
    ## 1 precision binary         0.916 training
    ## 2 precision binary         0.956 testing

``` r
scored_train_lasso %>%
  yardstick::recall(churn,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test_lasso %>%
  yardstick::recall(churn,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric .estimator .estimate part    
    ##   <chr>   <chr>          <dbl> <chr>   
    ## 1 recall  binary         0.148 training
    ## 2 recall  binary         0.151 testing

``` r
scored_train_lasso %>%
  conf_mat(
  truth = churn,
  estimate = .pred_class,
  dnn = c("Prediction", "Truth")
) %>%
  autoplot(type = "heatmap") + 
  labs(title="Training Confusion Matrix")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->

``` r
scored_test_lasso %>%
  conf_mat(
  truth = churn,
  estimate = .pred_class,
  dnn = c("Prediction", "Truth")
) %>%
  autoplot(type = "heatmap") + 
  labs(title="Testing Confusion Matrix")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-21-3.png)<!-- -->

### decision tree

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:kknn':
    ## 
    ##     contr.dummy

    ## The following objects are masked from 'package:yardstick':
    ## 
    ##     precision, recall, sensitivity, specificity

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
set.seed(44)
treemod2<-train(churn~.,data=knn_bake,
                method="rpart",
                trControl=trainControl("cv",number=10),
                tuneLength=10,
                na.action=na.omit)
plot(treemod2)
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
treemod2$bestTune
```

    ##            cp
    ## 1 0.003649635

``` r
tree<- decision_tree(mode = "classification", cost_complexity = 0.003, tree_depth = 10 , min_n =3)%>% #based on kcv
  set_engine("rpart")%>%
  fit(churn~.,data=knn_bake)

tree$fit
```

    ## n= 72720 
    ## 
    ## node), split, n, loss, yval, (yprob)
    ##       * denotes terminal node
    ## 
    ##   1) root 72720 3973 0 (0.94536579 0.05463421)  
    ##     2) total_billed>=5.869733 63917 2123 0 (0.96678505 0.03321495)  
    ##       4) total_billed>=6.573538 49610  935 0 (0.98115299 0.01884701) *
    ##       5) total_billed< 6.573538 14307 1188 0 (0.91696372 0.08303628)  
    ##        10) number_phones>=3.201467 13846  979 0 (0.92929366 0.07070634)  
    ##          20) streaming_minutes< 5.441115 12762  708 0 (0.94452280 0.05547720) *
    ##          21) streaming_minutes>=5.441115 1084  271 0 (0.75000000 0.25000000)  
    ##            42) payment_method_Mailed.Check>=0.5 653   59 0 (0.90964778 0.09035222) *
    ##            43) payment_method_Mailed.Check< 0.5 431  212 0 (0.50812065 0.49187935)  
    ##              86) streaming_plan_X3GB>=0.5 144   19 0 (0.86805556 0.13194444) *
    ##              87) streaming_plan_X3GB< 0.5 287   94 1 (0.32752613 0.67247387)  
    ##               174) payment_method_Credit.Card>=0.5 36    1 0 (0.97222222 0.02777778) *
    ##               175) payment_method_Credit.Card< 0.5 251   59 1 (0.23505976 0.76494024)  
    ##                 350) prev_balance>=4.569603 40   14 0 (0.65000000 0.35000000) *
    ##                 351) prev_balance< 4.569603 211   33 1 (0.15639810 0.84360190) *
    ##        11) number_phones< 3.201467 461  209 0 (0.54663774 0.45336226)  
    ##          22) paperless_billing_Yes>=0.5 206   31 0 (0.84951456 0.15048544) *
    ##          23) paperless_billing_Yes< 0.5 255   77 1 (0.30196078 0.69803922)  
    ##            46) customer_service_calls< 2.267953 91   31 0 (0.65934066 0.34065934)  
    ##              92) monthly_minutes< 4.419836 57    6 0 (0.89473684 0.10526316) *
    ##              93) monthly_minutes>=4.419836 34    9 1 (0.26470588 0.73529412) *
    ##            47) customer_service_calls>=2.267953 164   17 1 (0.10365854 0.89634146) *
    ##     3) total_billed< 5.869733 8803 1850 0 (0.78984437 0.21015563)  
    ##       6) number_phones>=4.116171 7309 1066 0 (0.85415241 0.14584759)  
    ##        12) streaming_minutes< 5.354826 6469  604 0 (0.90663163 0.09336837)  
    ##          24) monthly_minutes< 4.525019 4966  294 0 (0.94079742 0.05920258) *
    ##          25) monthly_minutes>=4.525019 1503  310 0 (0.79374584 0.20625416)  
    ##            50) multiple_lines_Yes>=0.5 1279  176 0 (0.86239249 0.13760751) *
    ##            51) multiple_lines_Yes< 0.5 224   90 1 (0.40178571 0.59821429)  
    ##             102) streaming_plan_X3GB>=0.5 80   19 0 (0.76250000 0.23750000) *
    ##             103) streaming_plan_X3GB< 0.5 144   29 1 (0.20138889 0.79861111) *
    ##        13) streaming_minutes>=5.354826 840  378 1 (0.45000000 0.55000000)  
    ##          26) payment_method_Electronic.Check< 0.5 488  160 0 (0.67213115 0.32786885)  
    ##            52) streaming_plan_X3GB>=0.5 206   22 0 (0.89320388 0.10679612) *
    ##            53) streaming_plan_X3GB< 0.5 282  138 0 (0.51063830 0.48936170)  
    ##             106) paperless_billing_Yes>=0.5 65    8 0 (0.87692308 0.12307692) *
    ##             107) paperless_billing_Yes< 0.5 217   87 1 (0.40092166 0.59907834)  
    ##               214) late_payments< 4.146244 66   20 0 (0.69696970 0.30303030) *
    ##               215) late_payments>=4.146244 151   41 1 (0.27152318 0.72847682) *
    ##          27) payment_method_Electronic.Check>=0.5 352   50 1 (0.14204545 0.85795455)  
    ##            54) streaming_plan_X3GB>=0.5 61   24 0 (0.60655738 0.39344262) *
    ##            55) streaming_plan_X3GB< 0.5 291   13 1 (0.04467354 0.95532646) *
    ##       7) number_phones< 4.116171 1494  710 1 (0.47523427 0.52476573)  
    ##        14) prev_balance< 3.060376 393   80 0 (0.79643766 0.20356234)  
    ##          28) paperless_billing_Yes>=0.5 273   21 0 (0.92307692 0.07692308) *
    ##          29) paperless_billing_Yes< 0.5 120   59 0 (0.50833333 0.49166667)  
    ##            58) mobile_hotspot_Yes< 0.5 35    2 0 (0.94285714 0.05714286) *
    ##            59) mobile_hotspot_Yes>=0.5 85   28 1 (0.32941176 0.67058824) *
    ##        15) prev_balance>=3.060376 1101  397 1 (0.36058129 0.63941871)  
    ##          30) monthly_minutes< 3.93113 349  116 0 (0.66762178 0.33237822)  
    ##            60) partner_Yes< 0.5 244   44 0 (0.81967213 0.18032787) *
    ##            61) partner_Yes>=0.5 105   33 1 (0.31428571 0.68571429) *
    ##          31) monthly_minutes>=3.93113 752  164 1 (0.21808511 0.78191489)  
    ##            62) paperless_billing_Yes>=0.5 264  114 1 (0.43181818 0.56818182)  
    ##             124) mobile_hotspot_Yes< 0.5 67    8 0 (0.88059701 0.11940299) *
    ##             125) mobile_hotspot_Yes>=0.5 197   55 1 (0.27918782 0.72081218) *
    ##            63) paperless_billing_Yes< 0.5 488   50 1 (0.10245902 0.89754098) *

``` r
options(scipen=0)
```

``` r
# -- training 
predict(tree, knn_bake, type = "prob") %>%
  bind_cols(.,predict(tree, knn_bake)) %>%
  bind_cols(.,knn_bake) -> scored_train_tree

head(scored_train_tree)
```

    ## # A tibble: 6 × 61
    ##   .pred_0 .pred_1 .pred_class monthly_…¹ custo…² strea…³ total…⁴ prev_…⁵ late_…⁶
    ##     <dbl>   <dbl> <fct>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1   0.329  0.671  1                 3.55    1.51    2.42    5.18    2.10    3.02
    ## 2   0.981  0.0188 0                 5.14    3.02    2.92    6.93    4.19    4.52
    ## 3   0.981  0.0188 0                 3.47    3.02    3.45    7.74    3.02    2.26
    ## 4   0.981  0.0188 0                 3.37    1.51    3.15    7.12    4.70    3.02
    ## 5   0.945  0.0555 0                 3.05    3.02    2.91    6.36    3.19    2.26
    ## 6   0.981  0.0188 0                 3.09    1.51    5.87    6.64    4.19    3.77
    ## # … with 52 more variables: number_phones <dbl>, churn <fct>,
    ## #   phone_model_Google.Pixel.4a <int>, phone_model_iPhone.11 <int>,
    ## #   phone_model_iPhone.11.Pro...Pro.Max <int>,
    ## #   phone_model_iPhone.SE.2020 <int>, phone_model_iPhone.XR <int>,
    ## #   phone_model_iPhone.XS <int>, phone_model_Moto.G8.Power <int>,
    ## #   phone_model_OnePlus.7.Pro <int>, phone_model_OnePlus.8.Pro <int>,
    ## #   phone_model_Samsung.Galaxy.Note.20.Ultra <int>, …
    ## # ℹ Use `colnames()` to see all variable names

``` r
bake_test<-bake(knn_recipe %>% prep(), test, composition = "tibble")

# -- testing 
predict(tree, bake_test, type = "prob") %>%
  bind_cols(.,predict(tree, bake_test)) %>%
  bind_cols(.,bake_test) -> scored_test_tree

head(scored_test_tree)
```

    ## # A tibble: 6 × 61
    ##   .pred_0 .pred_1 .pred_class monthly_…¹ custo…² strea…³ total…⁴ prev_…⁵ late_…⁶
    ##     <dbl>   <dbl> <fct>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1   0.981  0.0188 0                 4.42    3.02    5.33    8.02    3.94    3.77
    ## 2   0.272  0.728  1                 3.83    3.02    5.87    4.79    3.27    5.28
    ## 3   0.820  0.180  0                 3.57    3.02    4.26    5.46    4.11    3.77
    ## 4   0.981  0.0188 0                 3.93    1.51    5.77    9.54    6.79    3.02
    ## 5   0.981  0.0188 0                 3.55    3.02    2.24    7.88    5.03    4.52
    ## 6   0.981  0.0188 0                 2.55    1.51    4.15    7.26    4.78    3.77
    ## # … with 52 more variables: number_phones <dbl>, churn <fct>,
    ## #   phone_model_Google.Pixel.4a <int>, phone_model_iPhone.11 <int>,
    ## #   phone_model_iPhone.11.Pro...Pro.Max <int>,
    ## #   phone_model_iPhone.SE.2020 <int>, phone_model_iPhone.XR <int>,
    ## #   phone_model_iPhone.XS <int>, phone_model_Moto.G8.Power <int>,
    ## #   phone_model_OnePlus.7.Pro <int>, phone_model_OnePlus.8.Pro <int>,
    ## #   phone_model_Samsung.Galaxy.Note.20.Ultra <int>, …
    ## # ℹ Use `colnames()` to see all variable names

``` r
# -- AUC: Train and Test 
scored_train_tree %>% 
  metrics(churn, .pred_1, estimate = .pred_class) %>%
  mutate(part="training") %>%
  bind_rows( scored_test_tree %>% 
               metrics(churn, .pred_1, estimate = .pred_class) %>%
               mutate(part="testing") 
  ) 
```

    ## # A tibble: 8 × 4
    ##   .metric     .estimator .estimate part    
    ##   <chr>       <chr>          <dbl> <chr>   
    ## 1 accuracy    binary         0.963 training
    ## 2 kap         binary         0.518 training
    ## 3 mn_log_loss binary         0.146 training
    ## 4 roc_auc     binary         0.802 training
    ## 5 accuracy    binary         0.961 testing 
    ## 6 kap         binary         0.493 testing 
    ## 7 mn_log_loss binary         0.149 testing 
    ## 8 roc_auc     binary         0.801 testing

``` r
scored_train_tree %>%
  yardstick::precision(churn,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test_tree %>%
  yardstick::precision(churn,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric   .estimator .estimate part    
    ##   <chr>     <chr>          <dbl> <chr>   
    ## 1 precision binary         0.835 training
    ## 2 precision binary         0.836 testing

``` r
scored_train_tree %>%
  yardstick::recall(churn,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test_tree %>%
  yardstick::recall(churn,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric .estimator .estimate part    
    ##   <chr>   <chr>          <dbl> <chr>   
    ## 1 recall  binary         0.393 training
    ## 2 recall  binary         0.367 testing

``` r
# -- Variable Importance top 10 features  
tree %>%
  vip(num_features = 5)
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
# -- ROC Charts 
scored_train_tree %>%
  mutate(model = "train") %>%
  bind_rows(scored_test_tree %>%
              mutate(model="test")) %>%
  group_by(model) %>%
  roc_curve(churn, .pred_1) %>%
  autoplot()
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->

``` r
# -- Confustion Matricies  
scored_train_tree %>%
  conf_mat(churn, .pred_class) %>%
  autoplot( type = "heatmap") +
  labs(title="Train Confusion Matrix")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-25-3.png)<!-- -->

``` r
scored_test_tree %>%
  conf_mat(churn, .pred_class) %>%
  autoplot( type = "heatmap") +
  labs(title="Test Confusion Matrix")
```

![](churn_Shi_Shi_files/figure-gfm/unnamed-chunk-25-4.png)<!-- -->

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or
by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output
will be saved alongside it (click the *Preview* button or press
*Cmd+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.
