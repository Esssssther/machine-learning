Fraud Detection_random_forest
================
Esther
10/31/2022

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.

``` r
library(tidyverse)   # tidyverse 
library(tidymodels)  # modeling interface 
library(janitor)     # clean_names() 
library(skimr)       # profiling 
library(vip)         # variable importance 
```

``` r
fraud<- read_csv("project_2_training.csv",na=c("null","nan","","NA")) %>% clean_names()
```

    ## Rows: 125000 Columns: 27
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): ip_address, user_agent, email_domain, phone_number, billing_city,...
    ## dbl   (9): EVENT_ID, account_age_days, transaction_amt, transaction_adj_amt,...
    ## dttm  (1): EVENT_TIMESTAMP
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fraud%>% skim_to_wide()
```

    ## Warning: 'skim_to_wide' is deprecated.
    ## Use 'skim()' instead.
    ## See help("Deprecated")

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 125000     |
| Number of columns                                | 27         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 17         |
| numeric                                          | 9          |
| POSIXct                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim_variable       | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| ip_address          |       104 |             1 |   8 |  15 |     0 |    13313 |          0 |
| user_agent          |        95 |             1 |  59 | 147 |     0 |     8570 |          0 |
| email_domain        |        76 |             1 |   6 |  25 |     0 |     6991 |          0 |
| phone_number        |       102 |             1 |  10 |  22 |     0 |    11927 |          0 |
| billing_city        |        94 |             1 |   6 |  24 |     0 |     8979 |          0 |
| billing_state       |        94 |             1 |   4 |  14 |     0 |       50 |          0 |
| currency            |        94 |             1 |   3 |   3 |     0 |        3 |          0 |
| cvv                 |       102 |             1 |   1 |   1 |     0 |       25 |          0 |
| signature_image     |        92 |             1 |   1 |   1 |     0 |       26 |          0 |
| transaction_type    |        98 |             1 |   1 |   1 |     0 |       26 |          0 |
| transaction_env     |       104 |             1 |   1 |   1 |     0 |       26 |          0 |
| applicant_name      |       124 |             1 |   6 |  27 |     0 |    84958 |          0 |
| billing_address     |       111 |             1 |  11 |  38 |     0 |   124884 |          0 |
| merchant_id         |        89 |             1 |  11 |  11 |     0 |   124904 |          0 |
| locale              |       115 |             1 |   5 |   6 |     0 |      293 |          0 |
| tranaction_initiate |       100 |             1 |   1 |   1 |     0 |       26 |          0 |
| event_label         |         0 |             1 |   5 |   5 |     0 |        2 |          0 |

**Variable type: numeric**

| skim_variable         | n_missing | complete_rate |       mean |        sd |   p0 |    p25 |     p50 |        p75 |    p100 | hist  |
|:----------------------|----------:|--------------:|-----------:|----------:|-----:|-------:|--------:|-----------:|--------:|:------|
| event_id              |         0 |             1 | 1500443.74 | 866356.54 |   20 | 750535 | 1500570 | 2251825.00 | 2999960 | ▇▇▇▇▇ |
| account_age_days      |         0 |             1 |    4642.45 |   1160.92 |   -1 |   3822 |    4668 |    5472.00 |    9119 | ▁▃▇▃▁ |
| transaction_amt       |         0 |             1 |    2519.55 |    609.30 |   -1 |   2102 |    2543 |    2952.00 |    4880 | ▁▂▇▃▁ |
| transaction_adj_amt   |         0 |             1 |      54.14 |     10.17 |   -1 |     48 |      55 |      61.00 |      99 | ▁▁▇▃▁ |
| historic_velocity     |         0 |             1 |    4699.90 |   1194.36 |   -1 |   3871 |    4731 |    5549.00 |    8875 | ▁▂▇▅▁ |
| billing_postal        |        98 |             1 |   50210.79 |  28405.88 |  503 |  25298 |   50124 |   74457.00 |   99950 | ▇▇▇▇▇ |
| card_bin              |       110 |             1 |   41813.29 |  10084.07 | 6040 |  35378 |   42061 |   47330.75 |   67639 | ▁▃▇▇▂ |
| days_since_last_logon |       113 |             1 |      49.81 |     29.22 |    0 |     24 |      50 |      75.00 |     100 | ▇▇▇▇▇ |
| inital_amount         |       109 |             1 |    7999.64 |   4050.18 | 1000 |   4486 |    8007 |   11498.00 |   15000 | ▇▇▇▇▇ |

**Variable type: POSIXct**

| skim_variable   | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:----------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| event_timestamp |        90 |             1 | 2020-10-25 08:44:38 | 2021-10-25 14:27:09 | 2021-04-25 23:50:23 |   124685 |

``` r
fraud_kaggle<-read_csv("project_2_holdout.csv",na=c("null","nan","","NA")) %>% clean_names()
```

    ## Rows: 25000 Columns: 26
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (17): ip_address, user_agent, email_domain, phone_number, billing_city, ...
    ## dbl  (9): EVENT_ID, account_age_days, transaction_amt, transaction_adj_amt, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fraud %>%
  count(event_label)%>%
  mutate(pct=n/sum(n))
```

    ## # A tibble: 2 × 3
    ##   event_label      n    pct
    ##   <chr>        <int>  <dbl>
    ## 1 fraud         6785 0.0543
    ## 2 legit       118215 0.946

``` r
fraud %>%
  count(event_label)%>%
  mutate(pct=n/sum(n))%>%
  ggplot(aes(x=factor(event_label),y=pct))+
  geom_col()+
  labs(title = "fraud vs legit summary", x="event label", y="percentage")+
  geom_text(aes(label=paste(round(pct*100,1),"%")),vjust=1.5,color="white")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
fraud_email <-fraud%>%
  count(event_label,email_domain)%>%
  pivot_wider(id_cols= email_domain, names_from= event_label, values_from=n, values_fill=0)%>%
  mutate(total=fraud+legit,
         pct_email=fraud/total) %>%
  filter(total>5 & pct_email>0.1)%>%
  arrange(desc(pct_email))

fraud_email
```

    ## # A tibble: 808 × 5
    ##    email_domain         fraud legit total pct_email
    ##    <chr>                <int> <int> <int>     <dbl>
    ##  1 davis.com                4     2     6     0.667
    ##  2 baker-clark.org          3     3     6     0.5  
    ##  3 cochran.com              3     3     6     0.5  
    ##  4 sutton.org               3     3     6     0.5  
    ##  5 glass.com                5     6    11     0.455
    ##  6 crawford-jenkins.org     3     4     7     0.429
    ##  7 marshall.net             3     4     7     0.429
    ##  8 keller-woodward.com      5     7    12     0.417
    ##  9 barron.com               3     6     9     0.333
    ## 10 baxter.net               2     4     6     0.333
    ## # … with 798 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

``` r
#fraud_email %>%
# ggplot(aes(x=factor(email_domain),y=pct))+
# geom_col()+
#  labs(title = "fraud percentage of different email domain")
```

``` r
fraud_prep<-fraud %>%
  mutate(card_bin=as.factor(card_bin),
         billing_postal=as.factor(billing_postal)
      )%>%
#  mutate(event_label=if_else(event_label=="fraud",1,0),
#         event_label=as.factor(event_label))%>%
  mutate_if(is.character,factor)

skim(fraud_prep)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | fraud_prep |
| Number of rows                                   | 125000     |
| Number of columns                                | 27         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| factor                                           | 19         |
| numeric                                          | 7          |
| POSIXct                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: factor**

| skim_variable       | n_missing | complete_rate | ordered | n_unique | top_counts                                 |
|:--------------------|----------:|--------------:|:--------|---------:|:-------------------------------------------|
| ip_address          |       104 |             1 | FALSE   |    13313 | 112: 32, 221: 32, 114: 30, 161: 30         |
| user_agent          |        95 |             1 | FALSE   |     8570 | Moz: 60, Moz: 57, Moz: 56, Moz: 53         |
| email_domain        |        76 |             1 | FALSE   |     6991 | fre: 61, cal: 59, sol: 59, boy: 58         |
| phone_number        |       102 |             1 | FALSE   |    11927 | 292: 43, 349: 39, 414: 37, (22: 35         |
| billing_city        |        94 |             1 | FALSE   |     8979 | Lak: 46, Ric: 46, Dav: 45, Pow: 44         |
| billing_postal      |        98 |             1 | FALSE   |    11064 | 157: 38, 854: 38, 957: 38, 602: 37         |
| billing_state       |        94 |             1 | FALSE   |       50 | Ind: 7354, Mic: 7277, Lou: 7229, Rho: 7162 |
| card_bin            |       110 |             1 | FALSE   |     6321 | 653: 73, 655: 73, 409: 71, 357: 70         |
| currency            |        94 |             1 | FALSE   |        3 | cad: 94846, usd: 26414, eur: 3646          |
| cvv                 |       102 |             1 | FALSE   |       25 | D: 19323, W: 19194, G: 16937, X: 16331     |
| signature_image     |        92 |             1 | FALSE   |       26 | H: 14412, F: 13988, U: 13564, I: 12714     |
| transaction_type    |        98 |             1 | FALSE   |       26 | I: 13530, G: 13226, F: 12965, D: 12909     |
| transaction_env     |       104 |             1 | FALSE   |       26 | D: 18876, W: 17826, G: 17258, X: 14297     |
| applicant_name      |       124 |             1 | FALSE   |    84958 | Mic: 57, Joh: 49, Rob: 41, Dav: 40         |
| billing_address     |       111 |             1 | FALSE   |   124884 | 057: 2, 181: 2, 209: 2, 546: 2             |
| merchant_id         |        89 |             1 | FALSE   |   124904 | 041: 2, 186: 2, 238: 2, 353: 2             |
| locale              |       115 |             1 | FALSE   |      293 | mai: 752, mr\_: 750, csb: 745, km\_: 732   |
| tranaction_initiate |       100 |             1 | FALSE   |       26 | N: 4959, G: 4896, D: 4880, P: 4868         |
| event_label         |         0 |             1 | FALSE   |        2 | leg: 118215, fra: 6785                     |

**Variable type: numeric**

| skim_variable         | n_missing | complete_rate |       mean |        sd |   p0 |    p25 |     p50 |     p75 |    p100 | hist  |
|:----------------------|----------:|--------------:|-----------:|----------:|-----:|-------:|--------:|--------:|--------:|:------|
| event_id              |         0 |             1 | 1500443.74 | 866356.54 |   20 | 750535 | 1500570 | 2251825 | 2999960 | ▇▇▇▇▇ |
| account_age_days      |         0 |             1 |    4642.45 |   1160.92 |   -1 |   3822 |    4668 |    5472 |    9119 | ▁▃▇▃▁ |
| transaction_amt       |         0 |             1 |    2519.55 |    609.30 |   -1 |   2102 |    2543 |    2952 |    4880 | ▁▂▇▃▁ |
| transaction_adj_amt   |         0 |             1 |      54.14 |     10.17 |   -1 |     48 |      55 |      61 |      99 | ▁▁▇▃▁ |
| historic_velocity     |         0 |             1 |    4699.90 |   1194.36 |   -1 |   3871 |    4731 |    5549 |    8875 | ▁▂▇▅▁ |
| days_since_last_logon |       113 |             1 |      49.81 |     29.22 |    0 |     24 |      50 |      75 |     100 | ▇▇▇▇▇ |
| inital_amount         |       109 |             1 |    7999.64 |   4050.18 | 1000 |   4486 |    8007 |   11498 |   15000 | ▇▇▇▇▇ |

**Variable type: POSIXct**

| skim_variable   | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:----------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| event_timestamp |        90 |             1 | 2020-10-25 08:44:38 | 2021-10-25 14:27:09 | 2021-04-25 23:50:23 |   124685 |

``` r
fraud_billing_postal <-fraud_prep%>%
  count(event_label,billing_postal)%>%
  pivot_wider(id_cols= billing_postal, names_from= event_label, values_from=n, values_fill=0)%>%
  mutate(total=fraud+legit,
         pct_billpos=fraud/total) %>%
  filter(total>5 & pct_billpos>0.1)%>%
  arrange(desc(pct_billpos))

fraud_billing_postal
```

    ## # A tibble: 1,481 × 5
    ##    billing_postal fraud legit total pct_billpos
    ##    <fct>          <int> <int> <int>       <dbl>
    ##  1 74435              3     3     6       0.5  
    ##  2 3524               5     8    13       0.385
    ##  3 32676              5     8    13       0.385
    ##  4 61756              3     5     8       0.375
    ##  5 83729              3     5     8       0.375
    ##  6 92094              3     5     8       0.375
    ##  7 38992              4     7    11       0.364
    ##  8 2081               2     4     6       0.333
    ##  9 8454               2     4     6       0.333
    ## 10 13586              2     4     6       0.333
    ## # … with 1,471 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

# box plot

``` r
fraud_prep %>% 
  ggplot(aes(x=factor(event_label),y=account_age_days))+
  geom_boxplot()+
  labs(title="account_age_days w diff transaction label ",  y="account_age_days", x="event_label")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
fraud_prep %>% 
  ggplot(aes(x=factor(event_label),y=transaction_amt))+
  geom_boxplot()+
  labs(title="transaction_amt w diff transaction label ",  y="transaction_amt", x="event_label")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
fraud_prep %>% 
  ggplot(aes(x=factor(event_label),y=transaction_adj_amt))+
  geom_boxplot()+
  labs(title="transaction_adj_amt w diff transaction label",  y="transaction_adj_amt", x="event_label")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
fraud_prep %>% 
  ggplot(aes(x=factor(event_label),y=historic_velocity))+
  geom_boxplot()+
  labs(title="historic_velocity w diff transaction label",  y="historic_velocity", x="event_label")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

``` r
fraud_prep %>% 
  ggplot(aes(x=factor(event_label),y=days_since_last_logon))+
  geom_boxplot()+
  labs(title="days_since_last_logon w diff transaction label",  y="days_since_last_logon", x="event_label")
```

    ## Warning: Removed 113 rows containing non-finite values (stat_boxplot).

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->

``` r
fraud_prep %>% 
  ggplot(aes(x=factor(event_label),y=inital_amount))+
  geom_boxplot()+
  labs(title="initial_amount w diff transaction label",  y="initial_amount", x="event_label")
```

    ## Warning: Removed 109 rows containing non-finite values (stat_boxplot).

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->

\#bar chart

``` r
fraud_prep %>%
  ggplot(aes(x=factor(cvv),fill=factor(event_label)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="transaction label w diff cvv",  x="cvv", y="event_label")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
fraud_prep %>%
  ggplot(aes(x=factor(currency),fill=factor(event_label)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="transaction label w diff currency",  x="currency", y="event_label")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
fraud_prep %>%
  ggplot(aes(x=factor(signature_image),fill=factor(event_label)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="transaction label w diff signature_image",  x="signature_image", y="event_label")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
fraud_prep %>%
  ggplot(aes(x=factor(transaction_type),fill=factor(event_label)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="transaction label w diff transaction_type",  x="transaction_type", y="event_label")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
fraud_prep %>%
  ggplot(aes(x=factor(transaction_env),fill=factor(event_label)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="transaction label w diff transaction_env",  x="transaction_env", y="event_label")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

``` r
fraud_prep %>%
  ggplot(aes(x=factor(tranaction_initiate),fill=factor(event_label)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  labs(title="transaction label w diff tranaction_initiate",  x="tranaction_initiate", y="event_label")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-8-6.png)<!-- --> \#
histogram

``` r
histogram_fill <- function(col){
  fraud %>%
  na.omit() %>%
  ggplot(aes(x=!!as.name(col), fill=as.factor(event_label)))+
    geom_histogram(position = 'fill')+
    labs(title = as.character(col),y='fraud or legit')+
    theme(legend.title = element_blank())
}

numerics <- c('account_age_days',"transaction_amt","transaction_adj_amt","historic_velocity","days_since_last_logon","inital_amount")

for(col in numerics){
  print(histogram_fill(col))
}
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-9-5.png)<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-9-6.png)<!-- -->

``` r
fraud_kaggle <- fraud_kaggle %>%
  mutate(card_bin=as.factor(card_bin),
         billing_postal=as.factor(billing_postal)
      )%>%
  mutate_if(is.character,factor)

set.seed(43)

split<- initial_split(fraud_prep, prop=0.7)

train<-training(split)
test<-testing(split)

sprintf("train pct：%1.2f%%",nrow(train)/nrow(fraud_prep)*100)
```

    ## [1] "train pct：70.00%"

``` r
sprintf("test pct：%1.2f%%",nrow(test)/nrow(fraud_prep)*100)
```

    ## [1] "test pct：30.00%"

\#model1

``` r
model_recipe <- recipe(event_label ~ account_age_days + 
                         transaction_amt + 
                         transaction_adj_amt + 
                         historic_velocity + signature_image+ billing_state+
                         currency+cvv+transaction_type+transaction_env,data = train) %>% 
  step_impute_median(all_numeric_predictors()) %>% # replace numeric missing values 
# step_novel(all_nominal_predictors()) %>%         # handle new levels 
  themis::step_downsample(event_label, under_ratio = 10) %>% 
  step_unknown(all_nominal_predictors()) %>%       # replace category missing values 
#  step_other(all_nominal_predictors(),threshold = 0.1) %>%  # pool rarely occuring levels 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) # one-hot encode 

bake(model_recipe %>% prep(), train %>% sample_n(1000))
```

    ## # A tibble: 1,000 × 167
    ##    account_age…¹ trans…² trans…³ histo…⁴ event…⁵ signa…⁶ signa…⁷ signa…⁸ signa…⁹
    ##            <dbl>   <dbl>   <dbl>   <dbl> <fct>     <int>   <int>   <int>   <int>
    ##  1          1741    2256      48    3373 legit         0       0       0       0
    ##  2          4657    2518      59    3992 legit         0       0       0       0
    ##  3          5793    2151      47    5639 legit         0       0       0       0
    ##  4          3716    2756      46    2493 legit         0       0       0       0
    ##  5          3955    3127      42    4593 legit         0       0       0       0
    ##  6          5601    2945      66    5262 legit         0       0       0       0
    ##  7          4672    2112      60    2967 legit         0       0       0       1
    ##  8          3786    3534      51    5163 legit         0       0       0       0
    ##  9          4753    2614      51    3906 legit         0       0       0       0
    ## 10          3333    1783      47    4128 legit         0       0       0       1
    ## # … with 990 more rows, 158 more variables: signature_image_E <int>,
    ## #   signature_image_F <int>, signature_image_G <int>, signature_image_H <int>,
    ## #   signature_image_I <int>, signature_image_J <int>, signature_image_K <int>,
    ## #   signature_image_L <int>, signature_image_M <int>, signature_image_N <int>,
    ## #   signature_image_O <int>, signature_image_P <int>, signature_image_Q <int>,
    ## #   signature_image_R <int>, signature_image_S <int>, signature_image_T <int>,
    ## #   signature_image_U <int>, signature_image_V <int>, …
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
bake_train<-bake(model_recipe %>% prep(), train)
```

``` r
rf_model_new <- rand_forest(trees = 600, mtry=13,min_n = 10) %>%
   set_mode("classification") %>%
   set_engine("ranger", importance="permutation")
#num.threads = 8 , max.depth = 10
rf_workflow <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(rf_model_new) %>%
  fit(train)

rf_workflow
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: rand_forest()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 4 Recipe Steps
    ## 
    ## • step_impute_median()
    ## • step_downsample()
    ## • step_unknown()
    ## • step_dummy()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Ranger result
    ## 
    ## Call:
    ##  ranger::ranger(x = maybe_data_frame(x), y = y, mtry = min_cols(~13,      x), num.trees = ~600, min.node.size = min_rows(~10, x), importance = ~"permutation",      num.threads = 1, verbose = FALSE, seed = sample.int(10^5,          1), probability = TRUE) 
    ## 
    ## Type:                             Probability estimation 
    ## Number of trees:                  600 
    ## Sample size:                      52646 
    ## Number of independent variables:  166 
    ## Mtry:                             13 
    ## Target node size:                 10 
    ## Variable importance mode:         permutation 
    ## Splitrule:                        gini 
    ## OOB prediction error (Brier s.):  0.02862928

``` r
#rf_model <- rand_forest(trees = 300,mtry=53, min_n = 10) %>%
#   set_mode("classification") %>%
#   set_engine("ranger", importance="permutation")
#num.threads = 8 , max.depth = 10
#rf_workflow <- workflow() %>%
#  add_recipe(model_recipe) %>%
#  add_model(rf_model) %>%
#  fit(train)

#rf_workflow
```

``` r
options(yardstick.event_first = TRUE)
# score training
predict(rf_workflow, train, type = "prob") %>%
  bind_cols(predict(rf_workflow, train, type = "class")) %>%
  mutate(part = "train") %>%
  bind_cols(., train) -> scored_train

# -- score testing
predict(rf_workflow, test, type = "prob") %>%
  bind_cols(predict(rf_workflow,  test, type = "class")) %>%
  mutate(part = "testing") %>%
  bind_cols(., test) -> scored_test

## Metrics (AUC / Accuracy / Log Loss)
bind_rows (scored_train, scored_test)  %>%
  group_by(part) %>%
  metrics(event_label, .pred_fraud, estimate = .pred_class) %>%
  filter(.metric %in% c('accuracy', 'roc_auc', 'mn_log_loss')) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)
```

    ## Warning: The `yardstick.event_first` option has been deprecated as of yardstick 0.0.7 and will be completely ignored in a future version.
    ## Instead, set the following argument directly in the metric function:
    ## `options(yardstick.event_first = TRUE)`  -> `event_level = 'first'` (the default)
    ## `options(yardstick.event_first = FALSE)` -> `event_level = 'second'`
    ## This warning is displayed once per session.

    ## # A tibble: 2 × 5
    ##   part    .estimator accuracy mn_log_loss roc_auc
    ##   <chr>   <chr>         <dbl>       <dbl>   <dbl>
    ## 1 testing binary        0.980      0.0867   0.951
    ## 2 train   binary        0.990      0.0537   0.998

``` r
# precision @0.5
bind_rows(scored_train, scored_test) %>%
  group_by(part) %>%
  precision(event_label, .pred_class)
```

    ## # A tibble: 2 × 4
    ##   part    .metric   .estimator .estimate
    ##   <chr>   <chr>     <chr>          <dbl>
    ## 1 testing precision binary         0.892
    ## 2 train   precision binary         0.959

``` r
# recall @0.5
bind_rows(scored_train, scored_test) %>%
  group_by(part) %>%
  recall(event_label, .pred_class)
```

    ## # A tibble: 2 × 4
    ##   part    .metric .estimator .estimate
    ##   <chr>   <chr>   <chr>          <dbl>
    ## 1 testing recall  binary         0.706
    ## 2 train   recall  binary         0.845

``` r
#fpr
bind_rows(scored_train, scored_test) %>%
  group_by(part) %>%
  spec(event_label, .pred_class)%>%
  mutate(fpr=1-.estimate)
```

    ## # A tibble: 2 × 5
    ##   part    .metric .estimator .estimate     fpr
    ##   <chr>   <chr>   <chr>          <dbl>   <dbl>
    ## 1 testing spec    binary         0.995 0.00482
    ## 2 train   spec    binary         0.998 0.00209

``` r
# ROC Curve  
bind_rows(scored_train, scored_test) %>%
  group_by(part) %>%
  roc_curve(event_label, .pred_fraud) %>%
  autoplot() +
  geom_vline(xintercept = 0.004112560, # 0.12% FPR 
             color = "red",
             linetype = "longdash") +
  geom_vline(xintercept = 0.05, # 5% FPR 
             color = "purple",
             linetype = "longdash") +
  geom_vline(xintercept = 0.25,   # 25% FPR 
             color = "blue",
             linetype = "longdash") +
  geom_vline(xintercept = 0.75,   # 75% FPR 
             color = "green",
             linetype = "longdash") +
  labs(title = "RF1 ROC Curve" , x = "FPR(1 - specificity)", y = "TPR(recall)") 
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# histogram of probablyt of fraud 
scored_test %>%
  ggplot(aes(.pred_fraud, fill = event_label)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = 0.5, color = "red") +
  labs(
    title = paste("Distribution of the Probabilty of FRAUD:", "RF Model") ,
    x = ".pred_fraud",
    y = "count"
  ) 
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
# operating range 0 - 10% 
operating_range <- scored_test %>%
  roc_curve(event_label, .pred_fraud)  %>%
  mutate(
    fpr = round((1 - specificity), 2),
    tpr = round(sensitivity, 3),
    score_threshold =  round(.threshold, 3)
  ) %>%
  group_by(fpr) %>%
  summarise(threshold = round(mean(score_threshold),3),
            tpr = mean(tpr)) %>%
  filter(fpr <= 0.1)
# operating range table 
operating_range
```

    ## # A tibble: 11 × 3
    ##      fpr threshold   tpr
    ##    <dbl>     <dbl> <dbl>
    ##  1  0      Inf     0.379
    ##  2  0.01     0.41  0.763
    ##  3  0.02     0.29  0.822
    ##  4  0.03     0.228 0.850
    ##  5  0.04     0.188 0.867
    ##  6  0.05     0.16  0.882
    ##  7  0.06     0.138 0.891
    ##  8  0.07     0.124 0.898
    ##  9  0.08     0.111 0.903
    ## 10  0.09     0.1   0.908
    ## 11  0.1      0.091 0.913

``` r
## What is the precision at 5% false positive rate? 
scored_test %>%
  mutate(fpr_5_pct = as.factor(if_else(.pred_fraud >= 0.165,"fraud","legit"))) %>% 
  precision(event_label, fpr_5_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric   .estimator .estimate
    ##   <chr>     <chr>          <dbl>
    ## 1 precision binary         0.509

``` r
scored_test %>%
  mutate(fpr_5_pct = as.factor(if_else(.pred_fraud >= 0.165,"fraud","legit"))) %>% 
  recall(event_label, fpr_5_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 recall  binary         0.880

``` r
# function to find precision at threshold
#precision_funk <- function(threshold){
#  scored_test %>%
#  mutate(fpr_5_pct = as.factor(if_else(.pred_fraud >= threshold,"fraud","legit"))) %>% 
#  precision(event_label, fpr_5_pct) %>% print()}
# precision at given threshold
#precision_funk(threshold = 0.424)
# Variable Importandce 
rf_workflow %>%
  extract_fit_parsnip() %>%
  vip()+
  labs(title="RF model 1 variable importance plot")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
scored_kaggle<-predict(rf_workflow, fraud_kaggle, type = "prob")  %>%
  bind_cols(fraud_kaggle) %>%
  select(event_id,event_label = .pred_fraud)
```

    ## Warning: Novel levels found in column 'cvv': 'Y'. The levels have been removed,
    ## and values have been coerced to 'NA'.

``` r
scored_kaggle %>%
  write_csv("rf_my53_1_0.9793067.csv")
```

# add email domain, model 2

``` r
train_w_feature <- train %>% left_join(fraud_email %>% 
                                         select(email_domain, pct_email)) %>% 
  mutate(pct= replace_na(pct_email,0)) %>%
  left_join(fraud_billing_postal%>% select(billing_postal,pct_billpos))%>%
  mutate(pct_billpos=replace_na(pct_billpos,0))
```

    ## Joining, by = "email_domain"
    ## Joining, by = "billing_postal"

``` r
train_w_feature
```

    ## # A tibble: 87,500 × 30
    ##    event_id account_ag…¹ trans…² trans…³ histo…⁴ ip_ad…⁵ user_…⁶ email…⁷ phone…⁸
    ##       <dbl>        <dbl>   <dbl>   <dbl>   <dbl> <fct>   <fct>   <chr>   <fct>  
    ##  1  2471480         5280    2033      53    5019 94.32.… Mozill… johnso… +1-460…
    ##  2  1516220         4120    2501      47    3178 111.24… Opera/… smith-… 635-95…
    ##  3   214040         4796    1955      50    4073 25.195… Mozill… robert… +1-716…
    ##  4  2968260         3151    1998      57    6088 195.23… Mozill… bailey… (794)2…
    ##  5  1040220         6061    2811      62    6741 201.21… Mozill… nash-c… (829)2…
    ##  6  1275480         6368    2508      62    6148 190.12… Mozill… andrew… (059)7…
    ##  7   736100         3409    2161      58    2542 206.77… Opera/… jackso… (598)4…
    ##  8   570220         4383    2649      61    5821 139.59… Mozill… oconno… 033-06…
    ##  9  1688480         6073    2637      51    5838 125.15… Mozill… fernan… 689.79…
    ## 10   819580         5570    2874      48    7331 188.65… Mozill… meza.n… 431-54…
    ## # … with 87,490 more rows, 21 more variables: billing_city <fct>,
    ## #   billing_postal <fct>, billing_state <fct>, card_bin <fct>, currency <fct>,
    ## #   cvv <fct>, signature_image <fct>, transaction_type <fct>,
    ## #   transaction_env <fct>, event_timestamp <dttm>, applicant_name <fct>,
    ## #   billing_address <fct>, merchant_id <fct>, locale <fct>,
    ## #   tranaction_initiate <fct>, days_since_last_logon <dbl>,
    ## #   inital_amount <dbl>, event_label <fct>, pct_email <dbl>, pct <dbl>, …
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
test_w_feature <- test %>% left_join(fraud_email %>% 
                                         select(email_domain,  pct_email)) %>% 
  mutate(pct = replace_na(pct_email,0))%>%
  left_join(fraud_billing_postal%>% select(billing_postal,pct_billpos))%>%
  mutate(pct_billpos=replace_na(pct_billpos,0))
```

    ## Joining, by = "email_domain"
    ## Joining, by = "billing_postal"

``` r
test_w_feature
```

    ## # A tibble: 37,500 × 30
    ##    event_id account_ag…¹ trans…² trans…³ histo…⁴ ip_ad…⁵ user_…⁶ email…⁷ phone…⁸
    ##       <dbl>        <dbl>   <dbl>   <dbl>   <dbl> <fct>   <fct>   <chr>   <fct>  
    ##  1   294600         6570    2887      56    5602 109.15… Mozill… walsh-… 366.18…
    ##  2   694280         5126    2368      76    5444 194.13… Opera/… smith-… 001-64…
    ##  3   127560         4793    2721      48    6098 35.187… Mozill… gutier… 001-44…
    ##  4   586160         6418    3334      65    5358 7.88.2… Mozill… dunlap… 188.62…
    ##  5   946720         4195    2320      62    4281 37.8.3… Mozill… parks.… 001-05…
    ##  6  2991660         3705    2020      61    2873 215.55… Mozill… alvara… (525)4…
    ##  7   430660         3335    2281      62    3258 46.16.… Mozill… estrad… 252954…
    ##  8  2471680         6298    2688      51    4176 110.21… Mozill… perez-… 527414…
    ##  9     4620         4511    3504      39    7830 96.31.… Mozill… zimmer… 001-50…
    ## 10   346640         6037    2395      39    5283 205.10… Opera/… oneill… (336)9…
    ## # … with 37,490 more rows, 21 more variables: billing_city <fct>,
    ## #   billing_postal <fct>, billing_state <fct>, card_bin <fct>, currency <fct>,
    ## #   cvv <fct>, signature_image <fct>, transaction_type <fct>,
    ## #   transaction_env <fct>, event_timestamp <dttm>, applicant_name <fct>,
    ## #   billing_address <fct>, merchant_id <fct>, locale <fct>,
    ## #   tranaction_initiate <fct>, days_since_last_logon <dbl>,
    ## #   inital_amount <dbl>, event_label <fct>, pct_email <dbl>, pct <dbl>, …
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
kaggle_w_feature <- fraud_kaggle %>% left_join(fraud_email %>% 
                                         select(email_domain,  pct_email)) %>% 
  mutate(pct= replace_na(pct_email,0))%>%
  left_join(fraud_billing_postal%>% select(billing_postal,pct_billpos))%>%
  mutate(pct_billpos=replace_na(pct_billpos,0))
```

    ## Joining, by = "email_domain"
    ## Joining, by = "billing_postal"

``` r
kaggle_w_feature
```

    ## # A tibble: 25,000 × 29
    ##    event_id account_ag…¹ trans…² trans…³ histo…⁴ ip_ad…⁵ user_…⁶ email…⁷ phone…⁸
    ##       <dbl>        <dbl>   <dbl>   <dbl>   <dbl> <fct>   <fct>   <chr>   <fct>  
    ##  1   109420         4462    3648      54    6325 197.10… Opera/… perez-… +1-776…
    ##  2  1049060         3360    2180      54    4030 178.16… Opera/… morris… 001-42…
    ##  3  2805320         4725    2063      60    4372 163.48… Mozill… campos… 073.15…
    ##  4   450840         5336    2319      72    4385 55.233… Mozill… hanna-… 835-29…
    ##  5   423160         6115    3413      61    5942 119.22… Opera/… gregor… 424.20…
    ##  6   278980         3664    2241      52    3897 161.16… Mozill… carter… 001-68…
    ##  7  2282820         3569    3064      57    4892 15.142… Mozill… carter… (732)2…
    ##  8  1192680         4327    1168      57    3426 3.73.2… Mozill… may-hi… 396.78…
    ##  9  1319780         2797    1721      51    6066 204.15… Mozill… morgan… (985)8…
    ## 10  1373260         4615    2892      53    6418 171.15… Mozill… evans-… 001-44…
    ## # … with 24,990 more rows, 20 more variables: billing_city <fct>,
    ## #   billing_postal <fct>, billing_state <fct>, card_bin <fct>, currency <fct>,
    ## #   cvv <fct>, signature_image <fct>, transaction_type <fct>,
    ## #   transaction_env <fct>, event_timestamp <fct>, applicant_name <fct>,
    ## #   billing_address <fct>, merchant_id <fct>, locale <fct>,
    ## #   tranaction_initiate <fct>, days_since_last_logon <dbl>,
    ## #   inital_amount <dbl>, pct_email <dbl>, pct <dbl>, pct_billpos <dbl>, and …
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
model_recipe2 <- recipe(event_label ~ account_age_days + 
                         transaction_amt + 
                         transaction_adj_amt + 
                         historic_velocity + pct_email + pct_billpos+
                         currency+transaction_type++cvv+transaction_env,data = train_w_feature) %>% 
  step_impute_median(all_numeric_predictors()) %>% # replace numeric missing values 
  step_novel(all_nominal_predictors()) %>%         # handle new levels 
  themis::step_downsample(event_label, under_ratio = 10) %>% 
  step_unknown(all_nominal_predictors()) %>%       # replace category missing values 
  step_other(all_nominal_predictors(),threshold = 0.1) %>%  # pool rarely occuring levels 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) # onehot encode 

bake(model_recipe2 %>% prep(), train_w_feature %>% sample_n(1000))
```

    ## # A tibble: 1,000 × 26
    ##    account_age…¹ trans…² trans…³ histo…⁴ pct_e…⁵ pct_b…⁶ event…⁷ curre…⁸ curre…⁹
    ##            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <fct>     <int>   <int>
    ##  1          4103    2872      48    4633   0.129   0     legit         1       0
    ##  2          3308    2215      54    2901   0.129   0     legit         1       0
    ##  3          4822    2796      63    5691   0.129   0     legit         0       1
    ##  4          3950    1260      48    4566   0.107   0     legit         1       0
    ##  5          5627    4169      57    6787   0.129   0     fraud         0       1
    ##  6          6682    2393      63    5577   0.129   0     legit         0       1
    ##  7          3431    2062      35    3167   0.118   0.176 legit         1       0
    ##  8          2909    1726      66    4275   0.129   0     legit         0       0
    ##  9          5338    2389      63    4789   0.129   0     legit         0       1
    ## 10          5932    3515      55    5702   0.129   0     legit         1       0
    ## # … with 990 more rows, 17 more variables: currency_other <int>,
    ## #   transaction_type_D <int>, transaction_type_F <int>,
    ## #   transaction_type_G <int>, transaction_type_I <int>,
    ## #   transaction_type_other <int>, cvv_D <int>, cvv_G <int>, cvv_W <int>,
    ## #   cvv_X <int>, cvv_other <int>, transaction_env_D <int>,
    ## #   transaction_env_G <int>, transaction_env_I <int>, transaction_env_W <int>,
    ## #   transaction_env_X <int>, transaction_env_other <int>, and abbreviated …
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
bake_train2<- bake(model_recipe2 %>% prep(), train_w_feature)
#min_n = 10
rf_model2 <- rand_forest(trees = 500, mtry=6,min_n = 10) %>%
   set_mode("classification") %>%
   set_engine("ranger", num.threads = 5, max.depth = 10, importance="permutation")

rf_workflow2 <- workflow() %>%
  add_recipe(model_recipe2) %>%
  add_model(rf_model2) %>%
  fit(train_w_feature)

rf_workflow2
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: rand_forest()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 6 Recipe Steps
    ## 
    ## • step_impute_median()
    ## • step_novel()
    ## • step_downsample()
    ## • step_unknown()
    ## • step_other()
    ## • step_dummy()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Ranger result
    ## 
    ## Call:
    ##  ranger::ranger(x = maybe_data_frame(x), y = y, mtry = min_cols(~6,      x), num.trees = ~500, min.node.size = min_rows(~10, x), num.threads = ~5,      max.depth = ~10, importance = ~"permutation", verbose = FALSE,      seed = sample.int(10^5, 1), probability = TRUE) 
    ## 
    ## Type:                             Probability estimation 
    ## Number of trees:                  500 
    ## Sample size:                      52646 
    ## Number of independent variables:  25 
    ## Mtry:                             6 
    ## Target node size:                 10 
    ## Variable importance mode:         permutation 
    ## Splitrule:                        gini 
    ## OOB prediction error (Brier s.):  0.0341118

``` r
rf_workflow2 %>%
  pull_workflow_fit() %>%
  vip(10)+
  labs(title="RF model 2 variable importance plot")
```

    ## Warning: `pull_workflow_fit()` was deprecated in workflows 0.2.3.
    ## ℹ Please use `extract_fit_parsnip()` instead.

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
options(yardstick.event_first = TRUE)
# score training
predict(rf_workflow2, train_w_feature, type = "prob") %>%
  bind_cols(predict(rf_workflow2, train_w_feature, type = "class")) %>%
  mutate(part = "training") %>%
  bind_cols(., train_w_feature) -> scored_train2

# -- score testing
predict(rf_workflow2, test_w_feature, type = "prob") %>%
  bind_cols(predict(rf_workflow2,  test_w_feature, type = "class")) %>%
  mutate(part = "testing") %>%
  bind_cols(., test_w_feature) -> scored_test2

## Metrics (AUC / Accuracy / Log Loss)
bind_rows (scored_train2, scored_test2)  %>%
  group_by(part) %>%
  metrics(event_label, .pred_fraud, estimate = .pred_class) %>%
  filter(.metric %in% c('accuracy', 'roc_auc', 'mn_log_loss')) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)
```

    ## # A tibble: 2 × 5
    ##   part     .estimator accuracy mn_log_loss roc_auc
    ##   <chr>    <chr>         <dbl>       <dbl>   <dbl>
    ## 1 testing  binary        0.973      0.0959   0.954
    ## 2 training binary        0.976      0.0876   0.971

``` r
# precision @0.5
bind_rows(scored_train2, scored_test2) %>%
  group_by(part) %>%
  precision(event_label, .pred_class)
```

    ## # A tibble: 2 × 4
    ##   part     .metric   .estimator .estimate
    ##   <chr>    <chr>     <chr>          <dbl>
    ## 1 testing  precision binary         0.846
    ## 2 training precision binary         0.889

``` r
# recall @0.5
bind_rows(scored_train2, scored_test2) %>%
  group_by(part) %>%
  recall(event_label, .pred_class)
```

    ## # A tibble: 2 × 4
    ##   part     .metric .estimator .estimate
    ##   <chr>    <chr>   <chr>          <dbl>
    ## 1 testing  recall  binary         0.608
    ## 2 training recall  binary         0.648

``` r
#fpr
bind_rows(scored_train2, scored_test2) %>%
  group_by(part) %>%
  spec(event_label, .pred_class)%>%
  mutate(fpr=1-.estimate)
```

    ## # A tibble: 2 × 5
    ##   part     .metric .estimator .estimate     fpr
    ##   <chr>    <chr>   <chr>          <dbl>   <dbl>
    ## 1 testing  spec    binary         0.994 0.00625
    ## 2 training spec    binary         0.995 0.00465

``` r
# ROC Curve  
bind_rows(scored_train2, scored_test2) %>%
  group_by(part) %>%
  roc_curve(event_label, .pred_fraud) %>%
  autoplot() +
  geom_vline(xintercept = 0.0064, # 0.43% FPR 
             color = "red",
             linetype = "longdash") +
  geom_vline(xintercept = 0.05, # 5% FPR 
             color = "purple",
             linetype = "longdash") +
  geom_vline(xintercept = 0.25,   # 25% FPR 
             color = "blue",
             linetype = "longdash") +
  geom_vline(xintercept = 0.75,   # 75% FPR 
             color = "green",
             linetype = "longdash") +
  labs(title = "RF2 ROC Curve" , x = "FPR(1 - specificity)", y = "TPR(recall)") 
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
# histogram of probablyt of fraud 
scored_test2 %>%
  ggplot(aes(.pred_fraud, fill = event_label)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = 0.5, color = "red") +
  labs(
    title = paste("Distribution of the Probabilty of FRAUD:", "RF Model") ,
    x = ".pred_fraud",
    y = "count"
  ) 
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
# operating range 0 - 10% 
operating_range2 <- scored_test2 %>%
  roc_curve(event_label, .pred_fraud)  %>%
  mutate(
    fpr = round((1 - specificity), 2),
    tpr = round(sensitivity, 3),
    score_threshold =  round(.threshold, 3)
  ) %>%
  group_by(fpr) %>%
  summarise(threshold = round(mean(score_threshold),3),
            tpr = mean(tpr)) %>%
  filter(fpr <= 0.1)
# operating range table 
operating_range2
```

    ## # A tibble: 11 × 3
    ##      fpr threshold   tpr
    ##    <dbl>     <dbl> <dbl>
    ##  1  0      Inf     0.313
    ##  2  0.01     0.435 0.656
    ##  3  0.02     0.303 0.743
    ##  4  0.03     0.239 0.789
    ##  5  0.04     0.196 0.817
    ##  6  0.05     0.169 0.838
    ##  7  0.06     0.149 0.851
    ##  8  0.07     0.133 0.861
    ##  9  0.08     0.12  0.872
    ## 10  0.09     0.108 0.880
    ## 11  0.1      0.099 0.887

``` r
## What is the precision at 5% false positive rate? 
scored_test %>%
  mutate(fpr_5_pct = as.factor(if_else(.pred_fraud >= 0.167,"fraud","legit"))) %>% 
  precision(event_label, fpr_5_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric   .estimator .estimate
    ##   <chr>     <chr>          <dbl>
    ## 1 precision binary         0.512

``` r
scored_test %>%
  mutate(fpr_5_pct = as.factor(if_else(.pred_fraud >= 0.167,"fraud","legit"))) %>% 
  recall(event_label, fpr_5_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 recall  binary         0.878

``` r
predict(rf_workflow2, test_w_feature, type = "prob") %>%
  mutate(.pred_class=as.factor(if_else(.pred_fraud>0.167,"fraud","legit")))%>%
  bind_cols(., test_w_feature)%>%
  ggplot(aes(.pred_fraud, fill = event_label)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = 0.167, color = "red") +
  labs(
    title = paste("Distribution using 0.167 threshold", "RF Model") ,
    x = ".pred_fraud",
    y = "count"
  ) 
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->

``` r
predict(rf_workflow2, kaggle_w_feature, type = "prob")  %>%
  bind_cols(kaggle_w_feature) %>%
  select(event_id,event_label = .pred_fraud) %>%
  write_csv("rf_mtry_6.csv")
```

    ## Warning: Novel levels found in column 'cvv': 'Y'. The levels have been removed,
    ## and values have been coerced to 'NA'.

# which emails are important

``` r
email_recipe <- recipe(event_label ~ email_domain,data = train) %>% 
  #step_impute_median(all_numeric_predictors()) %>% # replace numeric missing values 
  #step_novel(all_nominal_predictors()) %>%         # handle new levels 
  themis::step_downsample(event_label, under_ratio = 3) %>% 
  step_unknown(all_nominal_predictors()) %>%       # replace category missing values 
  step_other(all_nominal_predictors(),threshold = 10) %>%  # pool rarely occuring levels 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) # onehot encode 

bake(email_recipe %>% prep(), train %>% sample_n(1000))
```

    ## # A tibble: 1,000 × 156
    ##    event_label email_d…¹ email…² email…³ email…⁴ email…⁵ email…⁶ email…⁷ email…⁸
    ##    <fct>           <int>   <int>   <int>   <int>   <int>   <int>   <int>   <int>
    ##  1 legit               0       0       0       0       0       0       0       0
    ##  2 fraud               0       0       0       0       0       0       0       0
    ##  3 legit               0       0       0       0       0       0       0       0
    ##  4 legit               0       0       0       0       0       0       0       0
    ##  5 legit               0       0       0       0       0       0       0       0
    ##  6 legit               0       0       0       0       0       0       0       0
    ##  7 legit               0       0       0       0       0       0       0       0
    ##  8 fraud               0       0       0       0       0       0       0       0
    ##  9 legit               0       0       0       0       0       0       0       0
    ## 10 legit               0       0       0       0       0       0       0       0
    ## # … with 990 more rows, 147 more variables: email_domain_barrett.net <int>,
    ## #   email_domain_bauer.arnold.com <int>, email_domain_beck.jackson.com <int>,
    ## #   email_domain_beck.biz <int>, email_domain_bender.info <int>,
    ## #   email_domain_benitez.bailey.info <int>,
    ## #   email_domain_benjamin.watson.org <int>, email_domain_black.com <int>,
    ## #   email_domain_boone.gonzales.com <int>, email_domain_booth.info <int>,
    ## #   email_domain_bradford.rivera.biz <int>, …
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
email_model <- logistic_reg(penalty = 0.01, mixture = 1) %>%
   set_mode("classification") %>%
   set_engine("glm")

email_workflow <- workflow() %>%
  add_recipe(email_recipe) %>%
  add_model(email_model) %>%
  fit(train)

tidy(email_workflow) %>%
  mutate_if(is.numeric,round,3) %>%
  filter(p.value < 0.05)
```

    ## # A tibble: 6 × 5
    ##   term                              estimate std.error statistic p.value
    ##   <chr>                                <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)                           1.12     0.018     63.7    0    
    ## 2 email_domain_beck.biz                -1.30     0.606     -2.15   0.032
    ## 3 email_domain_cabrera.com             -1.53     0.646     -2.36   0.018
    ## 4 email_domain_frazier.woods.info      -1.97     0.69      -2.85   0.004
    ## 5 email_domain_hoffman.ferguson.com    -1.53     0.646     -2.36   0.018
    ## 6 email_domain_small.medina.com        -1.53     0.646     -2.36   0.018

# which billing postals are important

``` r
billingpostal_recipe <- recipe(event_label ~ billing_postal ,data = train) %>% 
  #step_impute_median(all_numeric_predictors()) %>% # replace numeric missing values 
  #step_novel(all_nominal_predictors()) %>%         # handle new levels 
  themis::step_downsample(event_label, under_ratio = 5) %>% 
  step_unknown(all_nominal_predictors()) %>%       # replace category missing values 
  step_other(all_nominal_predictors(),threshold = 10) %>%  # pool rarely occuring levels 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) # onehot encode 

bake(billingpostal_recipe %>% prep(), train %>% sample_n(1000))
```

    ## # A tibble: 1,000 × 90
    ##    event_label billing…¹ billi…² billi…³ billi…⁴ billi…⁵ billi…⁶ billi…⁷ billi…⁸
    ##    <fct>           <int>   <int>   <int>   <int>   <int>   <int>   <int>   <int>
    ##  1 legit               0       0       0       0       0       0       0       0
    ##  2 legit               0       0       0       0       0       0       0       0
    ##  3 legit               0       0       0       0       0       0       0       0
    ##  4 legit               0       0       0       0       0       0       0       0
    ##  5 legit               0       0       0       0       0       0       0       0
    ##  6 legit               0       0       0       0       0       0       0       0
    ##  7 legit               0       0       0       0       0       0       0       0
    ##  8 legit               0       0       0       0       0       0       0       0
    ##  9 legit               0       0       0       0       0       0       0       0
    ## 10 legit               0       0       0       0       0       0       0       0
    ## # … with 990 more rows, 81 more variables: billing_postal_X9311 <int>,
    ## #   billing_postal_X9879 <int>, billing_postal_X10024 <int>,
    ## #   billing_postal_X11455 <int>, billing_postal_X11554 <int>,
    ## #   billing_postal_X12133 <int>, billing_postal_X12742 <int>,
    ## #   billing_postal_X13368 <int>, billing_postal_X13515 <int>,
    ## #   billing_postal_X14721 <int>, billing_postal_X14908 <int>,
    ## #   billing_postal_X15732 <int>, billing_postal_X17039 <int>, …
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
billing_model <- logistic_reg() %>%
   set_mode("classification") %>%
   set_engine("glm")

billpostal_workflow <- workflow() %>%
  add_recipe(billingpostal_recipe) %>%
  add_model(billing_model) %>%
  fit(train)

tidy(billpostal_workflow) %>%
  mutate_if(is.numeric,round,3)%>%
  filter(p.value<0.05)
```

    ## # A tibble: 2 × 5
    ##   term                  estimate std.error statistic p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)               1.61     0.016     99.9     0   
    ## 2 billing_postal_X47476    -1.27     0.586     -2.17    0.03

\#model 3

``` r
rf_model3 <- logistic_reg() %>%
   set_mode("classification") %>%
   set_engine("glm")

rf_workflow3 <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(rf_model3) %>%
  fit(train_w_feature)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
rf_workflow3
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: logistic_reg()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 4 Recipe Steps
    ## 
    ## • step_impute_median()
    ## • step_downsample()
    ## • step_unknown()
    ## • step_dummy()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## 
    ## Call:  stats::glm(formula = ..y ~ ., family = stats::binomial, data = data)
    ## 
    ## Coefficients:
    ##                  (Intercept)              account_age_days  
    ##                    2.173e+00                    -1.342e-03  
    ##              transaction_amt           transaction_adj_amt  
    ##                   -2.182e-04                     1.748e-01  
    ##            historic_velocity             signature_image_A  
    ##                   -1.449e-05                    -3.659e-02  
    ##            signature_image_B             signature_image_C  
    ##                   -2.100e-01                     1.540e+00  
    ##            signature_image_D             signature_image_E  
    ##                    1.343e+00                     1.359e+01  
    ##            signature_image_F             signature_image_G  
    ##                    7.769e-01                     1.150e+00  
    ##            signature_image_H             signature_image_I  
    ##                    6.225e-01                     8.771e-01  
    ##            signature_image_J             signature_image_K  
    ##                   -1.607e+00                     1.433e+01  
    ##            signature_image_L             signature_image_M  
    ##                           NA                     1.161e+01  
    ##            signature_image_N             signature_image_O  
    ##                   -3.974e-02                     2.039e+00  
    ##            signature_image_P             signature_image_Q  
    ##                   -2.592e-01                    -3.328e-01  
    ##            signature_image_R             signature_image_S  
    ##                    1.871e+00                     3.777e-01  
    ##            signature_image_T             signature_image_U  
    ##                    2.199e+01                     5.390e-01  
    ##            signature_image_V             signature_image_W  
    ##                    3.457e-01                     1.474e+00  
    ##            signature_image_X             signature_image_Y  
    ##                    1.484e+00                    -1.372e+01  
    ##            signature_image_Z       signature_image_unknown  
    ##                    1.351e+00                            NA  
    ##        billing_state_Alabama          billing_state_Alaska  
    ##                   -7.548e-01                    -5.851e-02  
    ##        billing_state_Arizona        billing_state_Arkansas  
    ##                    1.915e+00                    -6.016e-02  
    ##     billing_state_California        billing_state_Colorado  
    ##                   -7.218e-01                     1.374e-01  
    ##    billing_state_Connecticut        billing_state_Delaware  
    ##                    5.392e-01                     2.500e-02  
    ##        billing_state_Florida         billing_state_Georgia  
    ##                   -1.095e+00                    -5.698e-01  
    ##         billing_state_Hawaii           billing_state_Idaho  
    ##                    1.453e+01                    -1.047e-01  
    ##       billing_state_Illinois         billing_state_Indiana  
    ##                    3.610e-01                    -7.439e-02  
    ## 
    ## ...
    ## and 126 more lines.

``` r
rf_workflow3 %>%
  pull_workflow_fit() %>%
  vip(10)+
  labs(title="logistic model variable importance plot")
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
tidy(rf_workflow3) %>%
  mutate_if(is.numeric,round,3) %>%
  filter(p.value < 0.05)
```

    ## # A tibble: 32 × 5
    ##    term                estimate std.error statistic p.value
    ##    <chr>                  <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 account_age_days      -0.001     0        -42.0    0    
    ##  2 transaction_amt        0         0         -4.00   0    
    ##  3 transaction_adj_amt    0.175     0.003     61.0    0    
    ##  4 signature_image_C      1.54      0.7        2.2    0.028
    ##  5 signature_image_D      1.34      0.675      1.99   0.047
    ##  6 signature_image_J     -1.61      0.752     -2.14   0.033
    ##  7 signature_image_O      2.04      1.00       2.04   0.042
    ##  8 signature_image_R      1.87      0.815      2.30   0.022
    ##  9 signature_image_W      1.47      0.68       2.17   0.03 
    ## 10 signature_image_X      1.48      0.687      2.16   0.031
    ## # … with 22 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

``` r
scored_train_logit<- predict(rf_workflow3, train_w_feature, type = "prob") %>%
  bind_cols(predict(rf_workflow3,train_w_feature,type = "class"))%>%
  mutate(part="training")%>%
  bind_cols(.,train_w_feature)
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
scored_test_logit<- predict(rf_workflow3, test_w_feature, type = "prob") %>%
  bind_cols(predict(rf_workflow3,test_w_feature,type = "class"))%>%
  mutate(part="testing")%>%
  bind_cols(.,test_w_feature)
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
options(yardstick.event_first = TRUE)
  # -- Metrics: Train and Test 
scored_train_logit %>% 
  metrics(event_label, .pred_fraud, estimate = .pred_class) %>%
  mutate(part="training") %>%
  bind_rows( scored_test_logit %>% 
               metrics(event_label, .pred_fraud, estimate = .pred_class) %>%
               mutate(part="testing") ) %>%
  filter(.metric %in% c('accuracy','roc_auc','mn_log_loss')) %>%
  pivot_wider(names_from = .metric, values_from=.estimate)
```

    ## # A tibble: 2 × 5
    ##   .estimator part     accuracy mn_log_loss roc_auc
    ##   <chr>      <chr>       <dbl>       <dbl>   <dbl>
    ## 1 binary     training    0.976      0.0900   0.945
    ## 2 binary     testing     0.975      0.0937   0.942

``` r
scored_train_logit %>%
  yardstick::precision(event_label,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test_logit %>%
  yardstick::precision(event_label,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric   .estimator .estimate part    
    ##   <chr>     <chr>          <dbl> <chr>   
    ## 1 precision binary         0.828 training
    ## 2 precision binary         0.803 testing

``` r
scored_train_logit %>%
  yardstick::recall(event_label,.pred_class) %>%
  mutate(part="training") %>%
  bind_rows(
  scored_test_logit %>%
  yardstick::recall(event_label,.pred_class) %>%
    mutate(part="testing") 
  )
```

    ## # A tibble: 2 × 4
    ##   .metric .estimator .estimate part    
    ##   <chr>   <chr>          <dbl> <chr>   
    ## 1 recall  binary         0.702 training
    ## 2 recall  binary         0.704 testing

``` r
#fpr
 scored_test_logit%>%
  spec(event_label,.pred_class) %>%
  mutate(part="testing") %>%
  mutate(fpr=1-.estimate)%>%
  bind_rows(
  scored_train_logit%>%
  spec(event_label,.pred_class) %>%
    mutate(part="training") %>%
  mutate(fpr=1-.estimate)
  )
```

    ## # A tibble: 2 × 5
    ##   .metric .estimator .estimate part         fpr
    ##   <chr>   <chr>          <dbl> <chr>      <dbl>
    ## 1 spec    binary         0.990 testing  0.00972
    ## 2 spec    binary         0.992 training 0.00843

``` r
# ROC Curve  
bind_rows(scored_train_logit, scored_test_logit) %>%
  group_by(part) %>%
  roc_curve(event_label, .pred_fraud) %>%
  autoplot() +
  geom_vline(xintercept = 0.010506746, # 0.43% FPR 
             color = "red",
             linetype = "longdash") +
  geom_vline(xintercept = 0.05, # 5% FPR 
             color = "purple",
             linetype = "longdash") +
  geom_vline(xintercept = 0.25,   # 25% FPR 
             color = "blue",
             linetype = "longdash") +
  geom_vline(xintercept = 0.75,   # 75% FPR 
             color = "green",
             linetype = "longdash") +
  labs(title = "Logisctic ROC Curve" , x = "FPR(1 - specificity)", y = "TPR(recall)") 
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->

``` r
# histogram of probablyt of fraud 
scored_train_logit %>%
  ggplot(aes(.pred_fraud, fill = event_label)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = 0.5, color = "red") +
  labs(
    title = paste("Distribution of the Probabilty of FRAUD:", "logistic Model") ,
    x = ".pred_fraud",
    y = "count"
  ) 
```

![](fraud_Shi_Shi_files/figure-gfm/unnamed-chunk-22-3.png)<!-- -->

``` r
# operating range 0 - 10% 
operating_range3 <- scored_test_logit %>%
  roc_curve(event_label, .pred_fraud)  %>%
  mutate(
    fpr = round((1 - specificity), 2),
    tpr = round(sensitivity, 3),
    score_threshold =  round(.threshold, 3)
  ) %>%
  group_by(fpr) %>%
  summarise(threshold = round(mean(score_threshold),3),
            tpr = mean(tpr)) %>%
  filter(fpr <= 0.1)
# operating range table 
operating_range3
```

    ## # A tibble: 11 × 3
    ##      fpr threshold   tpr
    ##    <dbl>     <dbl> <dbl>
    ##  1  0      Inf     0.323
    ##  2  0.01     0.517 0.694
    ##  3  0.02     0.333 0.778
    ##  4  0.03     0.25  0.812
    ##  5  0.04     0.199 0.829
    ##  6  0.05     0.165 0.841
    ##  7  0.06     0.139 0.851
    ##  8  0.07     0.12  0.860
    ##  9  0.08     0.105 0.869
    ## 10  0.09     0.092 0.875
    ## 11  0.1      0.082 0.881

``` r
## What is the precision at 5% false positive rate? 
scored_test_logit %>%
  mutate(fpr_5_pct = as.factor(if_else(.pred_fraud >= 0.174,"fraud","legit"))) %>% 
  precision(event_label, fpr_5_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric   .estimator .estimate
    ##   <chr>     <chr>          <dbl>
    ## 1 precision binary         0.501

``` r
scored_test_logit %>%
  mutate(fpr_5_pct = as.factor(if_else(.pred_fraud >= 0.174,"fraud","legit"))) %>% 
  recall(event_label, fpr_5_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 recall  binary         0.837

``` r
predict(rf_workflow3, kaggle_w_feature, type = "prob")  %>%
  bind_cols(kaggle_w_feature) %>%
  select(event_id,event_label = .pred_fraud) %>%
  write_csv("logistic_2.csv")
```

    ## Warning: Novel levels found in column 'cvv': 'Y'. The levels have been removed,
    ## and values have been coerced to 'NA'.

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

\#tune

``` r
rf_model4 <- rand_forest(trees = 100,mtry=tune(), min_n = 10) %>%
   set_mode("classification") %>%
   set_engine("ranger", importance="permutation")

set.seed(1)
boots_1<- bootstraps(bake_train,times = 6)
ctrl<- control_grid(verbose = FALSE,save_pred = TRUE)

roc_vals<- metric_set(roc_auc)

formula_res<-
  rf_model4 %>%
  tune_grid(event_label~.,
            resamples=boots_1,
            grid=3,
            metrics=roc_vals,
            control=ctrl)
```

    ## i Creating pre-processing data to finalize unknown parameter: mtry

``` r
estimates<-collect_metrics(formula_res)

estimates
```

    ## # A tibble: 3 × 7
    ##    mtry .metric .estimator  mean     n std_err .config             
    ##   <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
    ## 1     4 roc_auc binary     0.940     6 0.00233 Preprocessor1_Model1
    ## 2   164 roc_auc binary     0.936     6 0.00185 Preprocessor1_Model2
    ## 3   100 roc_auc binary     0.939     6 0.00159 Preprocessor1_Model3

``` r
show_best(formula_res,metric="roc_auc")
```

    ## # A tibble: 3 × 7
    ##    mtry .metric .estimator  mean     n std_err .config             
    ##   <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
    ## 1     4 roc_auc binary     0.940     6 0.00233 Preprocessor1_Model1
    ## 2   100 roc_auc binary     0.939     6 0.00159 Preprocessor1_Model3
    ## 3   164 roc_auc binary     0.936     6 0.00185 Preprocessor1_Model2

``` r
rf_model5 <- rand_forest(trees = 100,mtry=tune(),min_n = 10 ) %>%
   set_mode("classification") %>%
   set_engine("ranger", importance="permutation")

set.seed(2)
boots_2<- bootstraps(bake_train2,times = 6)
ctrl<- control_grid(verbose = FALSE,save_pred = TRUE)

roc_vals2<- metric_set(roc_auc)

formula_res2<-
  rf_model4 %>%
  tune_grid(event_label~.,
            resamples=boots_2,
            grid=3,
            metrics=roc_vals2,
            control=ctrl)
```

    ## i Creating pre-processing data to finalize unknown parameter: mtry

``` r
estimates2<-collect_metrics(formula_res2)

estimates2
```

    ## # A tibble: 3 × 7
    ##    mtry .metric .estimator  mean     n  std_err .config             
    ##   <int> <chr>   <chr>      <dbl> <int>    <dbl> <chr>               
    ## 1     6 roc_auc binary     0.943     6 0.000940 Preprocessor1_Model1
    ## 2    23 roc_auc binary     0.934     6 0.00199  Preprocessor1_Model2
    ## 3    10 roc_auc binary     0.941     6 0.00121  Preprocessor1_Model3

``` r
show_best(formula_res2,metric="roc_auc")
```

    ## # A tibble: 3 × 7
    ##    mtry .metric .estimator  mean     n  std_err .config             
    ##   <int> <chr>   <chr>      <dbl> <int>    <dbl> <chr>               
    ## 1     6 roc_auc binary     0.943     6 0.000940 Preprocessor1_Model1
    ## 2    10 roc_auc binary     0.941     6 0.00121  Preprocessor1_Model3
    ## 3    23 roc_auc binary     0.934     6 0.00199  Preprocessor1_Model2

``` r
rf_model6 <- rand_forest(trees = 100,mtry=tune(), min_n = 10) %>%
   set_mode("classification") %>%
   set_engine("ranger", importance="permutation")

set.seed(1)
boots_1<- bootstraps(bake_train,times = 6)
ctrl<- control_grid(verbose = FALSE,save_pred = TRUE)

roc_vals<- metric_set(roc_auc)

formula_res<-
  rf_model6 %>%
  tune_grid(event_label~.,
            resamples=boots_1,
            grid=3,
            metrics=roc_vals,
            control=ctrl)
```

    ## i Creating pre-processing data to finalize unknown parameter: mtry

``` r
estimates<-collect_metrics(formula_res)

estimates
```

    ## # A tibble: 3 × 7
    ##    mtry .metric .estimator  mean     n std_err .config             
    ##   <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
    ## 1     4 roc_auc binary     0.940     6 0.00233 Preprocessor1_Model1
    ## 2   164 roc_auc binary     0.936     6 0.00185 Preprocessor1_Model2
    ## 3   100 roc_auc binary     0.939     6 0.00159 Preprocessor1_Model3

``` r
show_best(formula_res,metric="roc_auc")
```

    ## # A tibble: 3 × 7
    ##    mtry .metric .estimator  mean     n std_err .config             
    ##   <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
    ## 1     4 roc_auc binary     0.940     6 0.00233 Preprocessor1_Model1
    ## 2   100 roc_auc binary     0.939     6 0.00159 Preprocessor1_Model3
    ## 3   164 roc_auc binary     0.936     6 0.00185 Preprocessor1_Model2

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or
by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output
will be saved alongside it (click the *Preview* button or press
*Cmd+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.
