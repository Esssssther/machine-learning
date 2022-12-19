Surrogate model with breakdown explaination
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.

``` r
options(scipen = 999)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(tidymodels)
```

    ## ── Attaching packages ────────────────────────────────────── tidymodels 1.0.0 ──
    ## ✔ broom        1.0.0     ✔ rsample      1.1.0
    ## ✔ dials        1.0.0     ✔ tune         1.0.1
    ## ✔ infer        1.0.3     ✔ workflows    1.1.0
    ## ✔ modeldata    1.0.1     ✔ workflowsets 1.0.0
    ## ✔ parsnip      1.0.2     ✔ yardstick    1.1.0
    ## ✔ recipes      1.0.2     
    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## ✖ scales::discard() masks purrr::discard()
    ## ✖ dplyr::filter()   masks stats::filter()
    ## ✖ recipes::fixed()  masks stringr::fixed()
    ## ✖ dplyr::lag()      masks stats::lag()
    ## ✖ yardstick::spec() masks readr::spec()
    ## ✖ recipes::step()   masks stats::step()
    ## • Use tidymodels_prefer() to resolve common conflicts.

``` r
library(solitude) # -- new package 
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(ggpubr)
library(skimr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(vip)
```

    ## 
    ## Attaching package: 'vip'
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     vi

``` r
library(NeuralNetTools)
library(DALEX)    # new 
```

    ## Welcome to DALEX (version: 2.4.2).
    ## Find examples and detailed introduction at: http://ema.drwhy.ai/
    ## 
    ## 
    ## Attaching package: 'DALEX'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     explain

``` r
library(DALEXtra) # new 
```

``` r
labels<-read_csv("smilegate_1M_labels.csv",na=c("null","nan","","NA","n/a")) %>% clean_names()
```

    ## Rows: 1000000 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): transaction_id, EVENT_LABEL
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
skim(labels)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | labels  |
| Number of rows                                   | 1000000 |
| Number of columns                                | 2       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 2       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: character**

| skim_variable  | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:---------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| transaction_id |         0 |             1 |  12 |  18 |     0 |  1000000 |          0 |
| event_label    |         0 |             1 |   5 |   5 |     0 |        2 |          0 |

``` r
transaction<-read_csv("smilegate_1M_transactions.csv",na=c("null","nan","","NA","n/a")) %>% clean_names()
```

    ## Rows: 1000000 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): ip_address, email_address, transaction_id
    ## dbl  (9): registration_deposit, mean_deposit, mean_txn, monetary_returns_5da...
    ## dttm (1): EVENT_TIMESTAMP
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
skim(transaction)
```

|                                                  |             |
|:-------------------------------------------------|:------------|
| Name                                             | transaction |
| Number of rows                                   | 1000000     |
| Number of columns                                | 13          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| character                                        | 3           |
| numeric                                          | 9           |
| POSIXct                                          | 1           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

Data summary

**Variable type: character**

| skim_variable  | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:---------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| ip_address     |         0 |             1 |   9 |  15 |     0 |     3531 |          0 |
| email_address  |         0 |             1 |  15 |  32 |     0 |     3913 |          0 |
| transaction_id |         0 |             1 |  12 |  18 |     0 |  1000000 |          0 |

**Variable type: numeric**

| skim_variable          | n_missing | complete_rate |     mean |       sd |    p0 |   p25 |   p50 |   p75 |  p100 | hist  |
|:-----------------------|----------:|--------------:|---------:|---------:|------:|------:|------:|------:|------:|:------|
| registration_deposit   |         0 |             1 |  4818.73 |  1322.51 |    10 |  3790 |  4785 |  5840 |  9999 | ▁▅▇▃▁ |
| mean_deposit           |         0 |             1 |   499.24 |   124.59 |    10 |   409 |   502 |   591 |   999 | ▁▃▇▃▁ |
| mean_txn               |         0 |             1 |   511.51 |    95.11 |    10 |   448 |   512 |   575 |   999 | ▁▂▇▂▁ |
| monetary_returns_5day  |         0 |             1 |   109.92 |    26.32 |     0 |    91 |   113 |   130 |   199 | ▁▂▇▇▁ |
| monetary_returns_15day |         0 |             1 |   101.47 |    24.78 |     0 |    83 |   101 |   120 |   199 | ▁▃▇▃▁ |
| monetary_returns_30day |         0 |             1 |    87.09 |    18.31 |     0 |    75 |    87 |    99 |   199 | ▁▅▇▁▁ |
| game_cash_count_3day   |         0 |             1 |    49.57 |    12.79 |     1 |    40 |    49 |    59 |    99 | ▁▅▇▃▁ |
| distinct_account_3day  |         0 |             1 |    50.16 |     8.13 |     1 |    45 |    50 |    55 |    99 | ▁▁▇▁▁ |
| account_id             |         0 |             1 | 54976.41 | 25979.38 | 10000 | 32488 | 54938 | 77502 | 99999 | ▇▇▇▇▇ |

**Variable type: POSIXct**

| skim_variable   | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:----------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| event_timestamp |         0 |             1 | 2021-12-03 02:40:53 | 2022-12-03 08:25:20 | 2022-06-03 19:56:53 |   984283 |

``` r
head(transaction)
```

    ## # A tibble: 6 × 13
    ##   registration…¹ mean_…² mean_…³ monet…⁴ monet…⁵ monet…⁶ game_…⁷ disti…⁸ ip_ad…⁹
    ##            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>  
    ## 1           5754     504     453      83      58     102      39      49 43.49.…
    ## 2           6496     406     625     109     125     104      54      46 85.27.…
    ## 3           4786     549     459     148     149      76      65      46 54.225…
    ## 4           3677     639     493     109      98     112      40      64 129.98…
    ## 5           5200     435     659      61      98      92      56      52 74.230…
    ## 6           4498     376     632     122      78      89      41      62 12.219…
    ## # … with 4 more variables: email_address <chr>, event_timestamp <dttm>,
    ## #   transaction_id <chr>, account_id <dbl>, and abbreviated variable names
    ## #   ¹​registration_deposit, ²​mean_deposit, ³​mean_txn, ⁴​monetary_returns_5day,
    ## #   ⁵​monetary_returns_15day, ⁶​monetary_returns_30day, ⁷​game_cash_count_3day,
    ## #   ⁸​distinct_account_3day, ⁹​ip_address
    ## # ℹ Use `colnames()` to see all variable names

``` r
set.seed(12)

dtsplit<- initial_split(transaction,prop = 0.7)
isotrain<- training(dtsplit)
isotest<- testing(dtsplit)
isotrain1<-isotrain %>% dplyr::select_if(is.numeric)
isotest1<- isotest %>% dplyr::select_if(is.numeric)
so_recipe <- recipe( ~  registration_deposit+mean_deposit+mean_txn+monetary_returns_5day+
                       monetary_returns_15day+monetary_returns_30day+game_cash_count_3day+distinct_account_3day, isotrain1) %>% 
  step_impute_median(all_numeric_predictors()) %>%
#  step_normalize(all_numeric_predictors())  %>%
  prep()


bake_iso<-bake(so_recipe,isotrain1)
```

``` r
iso_forest <- isolationForest$new(
  sample_size = 2048,
  num_trees = 100,
  max_depth = 12)


iso_forest$fit(bake_iso)
```

    ## INFO  [22:18:56.374] Building Isolation Forest ...
    ## INFO  [22:19:01.578] done
    ## INFO  [22:19:01.584] Computing depth of terminal nodes ...
    ## INFO  [22:19:01.780] done
    ## INFO  [22:19:13.493] Completed growing isolation forest

``` r
pred_train <- iso_forest$predict(bake_iso)

pred_train %>%
  ggplot(aes(average_depth)) +
  geom_histogram(bins=20) + 
  geom_vline(xintercept = 9.9, linetype="dotted", 
                color = "blue", size=1.5) + 
  labs(title="Isolation Forest Average Tree Depth")
```

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
pred_train %>%
  ggplot(aes(anomaly_score)) +
  geom_histogram(bins=20) + 
  geom_vline(xintercept = 0.62, linetype="dotted", 
                color = "blue", size=1.5) + 
  labs(title="Isolation Forest Anomaly Score Above 0.62")
```

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
bake_test<-bake(so_recipe,isotest1)
pred_test <- iso_forest$predict(bake_test)

pred_test %>%
  ggplot(aes(average_depth)) +
  geom_histogram(bins=20) + 
  geom_vline(xintercept = 9.9, linetype="dotted", 
                color = "blue", size=1.5) + 
  labs(title="Isolation Forest Average Tree Depth")
```

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
pred_test %>%
  ggplot(aes(anomaly_score)) +
  geom_histogram(bins=20) + 
  geom_vline(xintercept = 0.62, linetype="dotted", 
                color = "blue", size=1.5) + 
  labs(title="Isolation Forest Anomaly Score Above 0.62")
```

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
train_pred <- bind_cols(pred_train,bake_iso) %>%
  mutate(anomaly = as.factor(if_else(average_depth <=9.9, "Anomaly","Normal")))

train_pred %>%
  arrange(average_depth) %>%
  count(anomaly)
```

    ##    anomaly      n
    ## 1: Anomaly   4169
    ## 2:  Normal 695831

``` r
train_pred_score <- bind_cols(pred_train,bake_iso) %>%
  mutate(anomaly = as.factor(if_else(anomaly_score >=0.62, "Anomaly","Normal")))

train_pred_score %>%
  arrange(anomaly_score) %>%
  count(anomaly)
```

    ##    anomaly      n
    ## 1: Anomaly   4395
    ## 2:  Normal 695605

``` r
synth_train <- bind_cols(pred_train, isotrain) %>%
  mutate(synthetic_target = as.factor(
      if_else(average_depth <= 9.9,"fraud","legit")),
         synthetic_target2 = as.factor(
           if_else(anomaly_score >= 0.62,"fraud","legit"))
         )
synth_train%>%
count(synthetic_target,synthetic_target2)
```

    ##    synthetic_target synthetic_target2      n
    ## 1:            fraud             fraud   4169
    ## 2:            legit             fraud    226
    ## 3:            legit             legit 695605

``` r
synth_test <- bind_cols(pred_test, isotest) %>%
  mutate(synthetic_target = as.factor(
      if_else(average_depth <= 9.9,"fraud","legit")),
         synthetic_target2 = as.factor(
           if_else(anomaly_score >= 0.62,"fraud","legit"))
         )
synth_test%>%
count(synthetic_target,synthetic_target2)
```

    ##    synthetic_target synthetic_target2      n
    ## 1:            fraud             fraud   1879
    ## 2:            legit             fraud    108
    ## 3:            legit             legit 298013

``` r
train_w_label<-synth_train %>%
  inner_join(labels,by="transaction_id")%>%
  mutate(event_label=factor(event_label))

test_w_label<-synth_test %>%
  inner_join(labels,by="transaction_id")%>%
  mutate(event_label=factor(event_label))
```

``` r
# precision,recall
train_w_label %>%
  yardstick::precision(event_label, synthetic_target)%>%
mutate(part="train")%>%
  bind_rows(train_w_label %>%
    yardstick::recall(event_label, synthetic_target)%>%
    mutate(part="train"))%>%
  bind_rows(test_w_label %>%
  yardstick::precision(event_label, synthetic_target)%>%
    mutate(part="test")%>%
  bind_rows(test_w_label %>%
  yardstick::recall(event_label, synthetic_target)%>%mutate(part="test")))
```

    ## # A tibble: 4 × 4
    ##   .metric   .estimator .estimate part 
    ##   <chr>     <chr>          <dbl> <chr>
    ## 1 precision binary         0.260 train
    ## 2 recall    binary         0.105 train
    ## 3 precision binary         0.261 test 
    ## 4 recall    binary         0.111 test

``` r
# precision,recall
train_w_label %>%
  yardstick::precision(event_label, synthetic_target2)%>%
mutate(part="train")%>%
  bind_rows(train_w_label %>%
    yardstick::recall(event_label, synthetic_target2)%>%
    mutate(part="train"))%>%
  bind_rows(test_w_label %>%
  yardstick::precision(event_label, synthetic_target2)%>%
    mutate(part="test")%>%
  bind_rows(test_w_label %>%
  yardstick::recall(event_label, synthetic_target2)%>%mutate(part="test")))
```

    ## # A tibble: 4 × 4
    ##   .metric   .estimator .estimate part 
    ##   <chr>     <chr>          <dbl> <chr>
    ## 1 precision binary         0.254 train
    ## 2 recall    binary         0.108 train
    ## 3 precision binary         0.256 test 
    ## 4 recall    binary         0.115 test

``` r
train_w_label$synthetic_target2<-as.factor(train_w_label$synthetic_target2)
model_recipe <- recipe(synthetic_target2 ~  registration_deposit+mean_deposit+mean_txn+monetary_returns_5day+
                       monetary_returns_15day+monetary_returns_30day+game_cash_count_3day+distinct_account_3day,data = train_w_label) %>% 
prep()


bake_train<-bake(model_recipe, train_w_label)
skim(bake_train)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | bake_train |
| Number of rows                                   | 700000     |
| Number of columns                                | 9          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| factor                                           | 1          |
| numeric                                          | 8          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: factor**

| skim_variable     | n_missing | complete_rate | ordered | n_unique | top_counts             |
|:------------------|----------:|--------------:|:--------|---------:|:-----------------------|
| synthetic_target2 |         0 |             1 | FALSE   |        2 | leg: 695605, fra: 4395 |

**Variable type: numeric**

| skim_variable          | n_missing | complete_rate |    mean |      sd |  p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:-----------------------|----------:|--------------:|--------:|--------:|----:|-----:|-----:|-----:|-----:|:------|
| registration_deposit   |         0 |             1 | 4819.74 | 1321.90 |  10 | 3791 | 4787 | 5841 | 9999 | ▁▅▇▃▁ |
| mean_deposit           |         0 |             1 |  499.17 |  124.59 |  10 |  409 |  502 |  591 |  999 | ▁▃▇▃▁ |
| mean_txn               |         0 |             1 |  511.65 |   95.07 |  10 |  448 |  512 |  576 |  999 | ▁▂▇▂▁ |
| monetary_returns_5day  |         0 |             1 |  109.92 |   26.32 |   0 |   91 |  113 |  130 |  199 | ▁▂▇▇▁ |
| monetary_returns_15day |         0 |             1 |  101.45 |   24.78 |   0 |   83 |  101 |  120 |  196 | ▁▃▇▅▁ |
| monetary_returns_30day |         0 |             1 |   87.12 |   18.30 |   0 |   75 |   87 |   99 |  199 | ▁▅▇▁▁ |
| game_cash_count_3day   |         0 |             1 |   49.57 |   12.79 |   1 |   40 |   49 |   59 |   99 | ▁▅▇▃▁ |
| distinct_account_3day  |         0 |             1 |   50.17 |    8.13 |   1 |   45 |   50 |   55 |   99 | ▁▁▇▁▁ |

``` r
rf_model <- rand_forest(trees = 100, mtry=13,min_n = 10) %>%
   set_mode("classification") %>%
   set_engine("ranger", importance="permutation")
#num.threads = 8 , max.depth = 10
rf_workflow <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(rf_model) %>%
    fit(train_w_label)
```

    ## Warning: 13 columns were requested but there were 8 predictors in the data. 8
    ## will be used.

``` r
rf_workflow
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: rand_forest()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 0 Recipe Steps
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Ranger result
    ## 
    ## Call:
    ##  ranger::ranger(x = maybe_data_frame(x), y = y, mtry = min_cols(~13,      x), num.trees = ~100, min.node.size = min_rows(~10, x), importance = ~"permutation",      num.threads = 1, verbose = FALSE, seed = sample.int(10^5,          1), probability = TRUE) 
    ## 
    ## Type:                             Probability estimation 
    ## Number of trees:                  100 
    ## Sample size:                      700000 
    ## Number of independent variables:  8 
    ## Mtry:                             8 
    ## Target node size:                 10 
    ## Variable importance mode:         permutation 
    ## Splitrule:                        gini 
    ## OOB prediction error (Brier s.):  0.001947785

``` r
options(yardstick.event_first = TRUE)
# score training
predict(rf_workflow, train_w_label, type = "prob") %>%
  bind_cols(predict(rf_workflow, train_w_label, type = "class")) %>%
  mutate(part = "train_w_label") %>%
  bind_cols(., train_w_label) -> scored_train

predict(rf_workflow, test_w_label, type = "prob") %>%
  bind_cols(predict(rf_workflow,  test_w_label, type = "class")) %>%
  mutate(part = "test_w_label") %>%
  bind_cols(., test_w_label) -> scored_test
```

``` r
# precision,recall
scored_train %>%
  yardstick::precision(event_label, .pred_class)%>%
mutate(part="train")%>%
    bind_rows(scored_test %>%
  yardstick::precision(event_label, .pred_class)%>%
    mutate(part="test")%>%
  bind_rows(scored_train %>%
    yardstick::recall(event_label, .pred_class)%>%
    mutate(part="train"))%>%
  bind_rows(scored_test %>%
  yardstick::recall(event_label, .pred_class)%>%mutate(part="test")))
```

    ## Warning: The `yardstick.event_first` option has been deprecated as of yardstick 0.0.7 and will be completely ignored in a future version.
    ## Instead, set the following argument directly in the metric function:
    ## `options(yardstick.event_first = TRUE)`  -> `event_level = 'first'` (the default)
    ## `options(yardstick.event_first = FALSE)` -> `event_level = 'second'`
    ## This warning is displayed once per session.

    ## # A tibble: 4 × 4
    ##   .metric   .estimator .estimate part 
    ##   <chr>     <chr>          <dbl> <chr>
    ## 1 precision binary         0.258 train
    ## 2 precision binary         0.286 test 
    ## 3 recall    binary         0.107 train
    ## 4 recall    binary         0.102 test

``` r
## Metrics (AUC / Accuracy / Log Loss)
bind_rows (scored_train, scored_test)  %>%
  group_by(part) %>%
  metrics(event_label, .pred_fraud, estimate = .pred_class) %>%
  filter(.metric %in% c('accuracy', 'roc_auc', 'mn_log_loss')) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)
```

    ## # A tibble: 2 × 5
    ##   part          .estimator accuracy mn_log_loss roc_auc
    ##   <chr>         <chr>         <dbl>       <dbl>   <dbl>
    ## 1 test_w_label  binary        0.983       0.427   0.603
    ## 2 train_w_label binary        0.982       0.437   0.597

``` r
model_recipe_original <- recipe(event_label ~   registration_deposit+mean_deposit+mean_txn+monetary_returns_5day+
                       monetary_returns_15day+monetary_returns_30day+game_cash_count_3day+distinct_account_3day,data = train_w_label) %>% 
prep()


bake_train<-bake(model_recipe_original, train_w_label)
skim(bake_train)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | bake_train |
| Number of rows                                   | 700000     |
| Number of columns                                | 9          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| factor                                           | 1          |
| numeric                                          | 8          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts              |
|:--------------|----------:|--------------:|:--------|---------:|:------------------------|
| event_label   |         0 |             1 | FALSE   |        2 | leg: 689649, fra: 10351 |

**Variable type: numeric**

| skim_variable          | n_missing | complete_rate |    mean |      sd |  p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:-----------------------|----------:|--------------:|--------:|--------:|----:|-----:|-----:|-----:|-----:|:------|
| registration_deposit   |         0 |             1 | 4819.74 | 1321.90 |  10 | 3791 | 4787 | 5841 | 9999 | ▁▅▇▃▁ |
| mean_deposit           |         0 |             1 |  499.17 |  124.59 |  10 |  409 |  502 |  591 |  999 | ▁▃▇▃▁ |
| mean_txn               |         0 |             1 |  511.65 |   95.07 |  10 |  448 |  512 |  576 |  999 | ▁▂▇▂▁ |
| monetary_returns_5day  |         0 |             1 |  109.92 |   26.32 |   0 |   91 |  113 |  130 |  199 | ▁▂▇▇▁ |
| monetary_returns_15day |         0 |             1 |  101.45 |   24.78 |   0 |   83 |  101 |  120 |  196 | ▁▃▇▅▁ |
| monetary_returns_30day |         0 |             1 |   87.12 |   18.30 |   0 |   75 |   87 |   99 |  199 | ▁▅▇▁▁ |
| game_cash_count_3day   |         0 |             1 |   49.57 |   12.79 |   1 |   40 |   49 |   59 |   99 | ▁▅▇▃▁ |
| distinct_account_3day  |         0 |             1 |   50.17 |    8.13 |   1 |   45 |   50 |   55 |   99 | ▁▁▇▁▁ |

``` r
#num.threads = 8 , max.depth = 10
rf_workflow_original <- workflow() %>%
  add_recipe(model_recipe_original) %>%
  add_model(rf_model) %>%
    fit(train_w_label)
```

    ## Warning: 13 columns were requested but there were 8 predictors in the data. 8
    ## will be used.

``` r
rf_workflow_original
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: rand_forest()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 0 Recipe Steps
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Ranger result
    ## 
    ## Call:
    ##  ranger::ranger(x = maybe_data_frame(x), y = y, mtry = min_cols(~13,      x), num.trees = ~100, min.node.size = min_rows(~10, x), importance = ~"permutation",      num.threads = 1, verbose = FALSE, seed = sample.int(10^5,          1), probability = TRUE) 
    ## 
    ## Type:                             Probability estimation 
    ## Number of trees:                  100 
    ## Sample size:                      700000 
    ## Number of independent variables:  8 
    ## Mtry:                             8 
    ## Target node size:                 10 
    ## Variable importance mode:         permutation 
    ## Splitrule:                        gini 
    ## OOB prediction error (Brier s.):  0.006761302

``` r
options(yardstick.event_first = TRUE)
# score training
predict(rf_workflow_original, train_w_label, type = "prob") %>%
  bind_cols(predict(rf_workflow_original, train_w_label, type = "class")) %>%
  mutate(part = "train") %>%
  bind_cols(., train_w_label) -> scored_train_original

predict(rf_workflow_original, test_w_label, type = "prob") %>%
  bind_cols(predict(rf_workflow_original,  test_w_label, type = "class")) %>%
  mutate(part = "test") %>%
  bind_cols(., test_w_label) -> scored_test_original
```

``` r
# precision,recall
scored_train_original %>%
  yardstick::precision(event_label, .pred_class)%>%
mutate(part="train")%>%
  bind_rows(scored_test_original %>%
  yardstick::precision(event_label, .pred_class)%>%
    mutate(part="test")%>%
  bind_rows(scored_train_original %>%
    yardstick::recall(event_label, .pred_class)%>%
    mutate(part="train"))%>%
  bind_rows(scored_test_original %>%
  yardstick::recall(event_label, .pred_class)%>%mutate(part="test")))
```

    ## # A tibble: 4 × 4
    ##   .metric   .estimator .estimate part 
    ##   <chr>     <chr>          <dbl> <chr>
    ## 1 precision binary         0.997 train
    ## 2 precision binary         0.982 test 
    ## 3 recall    binary         0.638 train
    ## 4 recall    binary         0.552 test

``` r
## Metrics (AUC / Accuracy / Log Loss)
bind_rows (scored_train_original, scored_test_original)  %>%
  group_by(part) %>%
  metrics(event_label, .pred_fraud, estimate = .pred_class) %>%
  filter(.metric %in% c('accuracy', 'roc_auc', 'mn_log_loss')) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)
```

    ## # A tibble: 2 × 5
    ##   part  .estimator accuracy mn_log_loss roc_auc
    ##   <chr> <chr>         <dbl>       <dbl>   <dbl>
    ## 1 test  binary        0.993      0.0968   0.829
    ## 2 train binary        0.995      0.0111   1.00

``` r
train_w_label_sample <- train_w_label %>% sample_n(1000)




rf_explainer_surrogate <- 
  explain_tidymodels(
    rf_workflow,   # fitted workflow object 
    data = train_w_label_sample,    # original training data
    y = train_w_label_sample$event_label, # predicted outcome 
    label = "rf_explainer_surrogate",
    verbose = FALSE
  )
```

    ## Warning in Ops.factor(y, predict_function(model, data)): '-' not meaningful for
    ## factors

``` r
explain_prediction <- function(single_record){
 
# step 3. run the explainer 
rf_breakdown_surrogate <- predict_parts(explainer = rf_explainer_surrogate, 
                               new_observation = single_record,
                               type="break_down"
                               )

# step 4. plot it. 
# you notice you don't get categorical values ...  
rf_breakdown_surrogate %>% plot()%>%print()

# --- more involved explanations with categories. ---- 

# step 4a.. convert breakdown to a tibble so we can join it
rf_breakdown_surrogate %>%
  as_tibble() -> breakdown_data_surrogate 

# step 4b. transpose your single record prediction 
single_record %>% 
 gather(key="variable_name",value="value") -> prediction_data_surrogate 

# step 4c. get a predicted probability for plot 
prediction_prob_surrogate <- single_record[,".pred_fraud"] %>% pull()

# step 5. plot it.
print(breakdown_data_surrogate %>% 
  inner_join(prediction_data_surrogate) %>%
  mutate(contribution = round(contribution,3),) %>%
  filter(variable_name != "intercept") %>%
  mutate(variable = paste(variable_name,value,sep = ": ")) %>% 
  ggplot(aes(y=reorder(variable, contribution), x= contribution, fill=sign)) +
  geom_col() + 
  geom_text(aes(label=contribution), 
          size=4,
            position=position_dodge(width=0.7),
            vjust=0.5,
            )+
  labs(
    title = "DALEX explainations",
    subtitle = paste("predicted:",as.character(round(prediction_prob_surrogate,3))),
                    x="contribution",
                    y="features")
)
}

top_10_surrogate <- scored_test %>%
  filter(.pred_class == event_label) %>%
  filter(event_label == "fraud")%>%
  slice_max(.pred_fraud,n=10)%>%
  head(10)


for (row in 1:nrow(top_10_surrogate)) {
    s_record_surrogate <- top_10_surrogate[row,]
    explain_prediction(s_record_surrogate)
} 
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-3.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-4.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-5.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-6.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-7.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-8.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-9.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-10.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-11.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-12.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-13.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-14.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-15.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-16.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-17.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-18.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-19.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-24-20.png)<!-- -->

``` r
rf_explainer_original <- 
  explain_tidymodels(
    rf_workflow_original,   # fitted workflow object 
    data = train_w_label_sample,    # original training data
    y = train_w_label_sample$event_label, # predicted outcome 
    label = "rf_explainer_original",
    verbose = FALSE
  )
```

    ## Warning in Ops.factor(y, predict_function(model, data)): '-' not meaningful for
    ## factors

``` r
explain_prediction2 <- function(single_record){
# step 3. run the explainer 
rf_breakdown_original <- predict_parts(explainer = rf_explainer_original, 
                               new_observation = single_record,
                               type="break_down"
                               )

# step 4. plot it. 
# you notice you don't get categorical values ...  
rf_breakdown_original %>% plot()%>%print()

# --- more involved explanations with categories. ---- 

# step 4a.. convert breakdown to a tibble so we can join it
rf_breakdown_original %>%
  as_tibble() -> breakdown_data_origianl 

# step 4b. transpose your single record prediction 
single_record %>% 
 gather(key="variable_name",value="value") -> prediction_data_original

# step 4c. get a predicted probability for plot 
prediction_prob_original <- single_record[,".pred_fraud"] %>% pull()

# step 5. plot it.
print(breakdown_data_origianl %>% 
  inner_join(prediction_data_original) %>%
  mutate(contribution = round(contribution,3),) %>%
  filter(variable_name != "intercept") %>%
  mutate(variable = paste(variable_name,value,sep = ": ")) %>% 
  ggplot(aes(y=reorder(variable, contribution), x= contribution, fill=sign)) +
  geom_col() + 
  geom_text(aes(label=contribution), 
          size=4,
            position=position_dodge(width=0.7),
            vjust=0.5,
            )+
  labs(
    title = "DALEX explainations",
    subtitle = paste("predicted:",as.character(round(prediction_prob_original,3))),
                    x="contribution",
                    y="features")
)
  }

top_10_original <- scored_test_original %>%
  filter(.pred_class == event_label) %>%
  filter(event_label=="fraud")%>%
  slice_max(.pred_fraud,n=10)%>%
  head(10)


for (row in 1:nrow(top_10_original)) {
    s_record_original <- top_10_original[row,]
  explain_prediction2(s_record_original)
} 
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-3.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-4.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-5.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-6.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-7.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-8.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-9.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-10.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-11.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-12.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-13.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-14.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-15.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-16.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-17.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-18.png)<!-- -->

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## Joining, by = "variable_name"

![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-19.png)<!-- -->![](Surrogate_Model_Breakdown_files/figure-gfm/unnamed-chunk-25-20.png)<!-- -->

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or
by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output
will be saved alongside it (click the *Preview* button or press
*Cmd+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.
