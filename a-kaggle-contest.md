---
title: "A simple kaggle contest"
author: "Prasanna Bhogale"
date: "2019-07-23"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{A simple kaggle contest}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



This vignette will use data from the [house prices kaggle contest](https://www.kaggle.com/c/house-prices-advanced-regression-techniques/overview). This vignette will focus on thge simplest possible data cleaning and rely on the sensible defaults in `simplexgb` to construct a submission and evaluate the results on kaggle. For a more comprehensive data exploration of this data set check [this kernel](https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda)

**Get the data**

If you have the kaggle api, use `kaggle competitions download -c house-prices-advanced-regression-techniques` to obtain the data. See [this blog](https://adityashrm21.github.io/Setting-Up-Kaggle/) to see how to setup the kaggle api, if you want to.


```r
train <- read_csv("../data/kaggle-house-prices/train.csv") %>% select(-Id) %>% clean_names()
#> Parsed with column specification:
#> cols(
#>   .default = col_character(),
#>   Id = col_double(),
#>   MSSubClass = col_double(),
#>   LotFrontage = col_double(),
#>   LotArea = col_double(),
#>   OverallQual = col_double(),
#>   OverallCond = col_double(),
#>   YearBuilt = col_double(),
#>   YearRemodAdd = col_double(),
#>   MasVnrArea = col_double(),
#>   BsmtFinSF1 = col_double(),
#>   BsmtFinSF2 = col_double(),
#>   BsmtUnfSF = col_double(),
#>   TotalBsmtSF = col_double(),
#>   `1stFlrSF` = col_double(),
#>   `2ndFlrSF` = col_double(),
#>   LowQualFinSF = col_double(),
#>   GrLivArea = col_double(),
#>   BsmtFullBath = col_double(),
#>   BsmtHalfBath = col_double(),
#>   FullBath = col_double()
#>   # ... with 18 more columns
#> )
#> See spec(...) for full column specifications.
test <- read_csv("../data/kaggle-house-prices/test.csv") %>% clean_names()
#> Parsed with column specification:
#> cols(
#>   .default = col_character(),
#>   Id = col_double(),
#>   MSSubClass = col_double(),
#>   LotFrontage = col_double(),
#>   LotArea = col_double(),
#>   OverallQual = col_double(),
#>   OverallCond = col_double(),
#>   YearBuilt = col_double(),
#>   YearRemodAdd = col_double(),
#>   MasVnrArea = col_double(),
#>   BsmtFinSF1 = col_double(),
#>   BsmtFinSF2 = col_double(),
#>   BsmtUnfSF = col_double(),
#>   TotalBsmtSF = col_double(),
#>   `1stFlrSF` = col_double(),
#>   `2ndFlrSF` = col_double(),
#>   LowQualFinSF = col_double(),
#>   GrLivArea = col_double(),
#>   BsmtFullBath = col_double(),
#>   BsmtHalfBath = col_double(),
#>   FullBath = col_double()
#>   # ... with 17 more columns
#> )
#> See spec(...) for full column specifications.
test_id <- test$id
test <- test %>% select(-id)
sample_sub <- read_csv("../data/kaggle-house-prices/sample_submission.csv")
#> Parsed with column specification:
#> cols(
#>   Id = col_double(),
#>   SalePrice = col_double()
#> )
sample_sub %>% head()
#> # A tibble: 6 x 2
#>      Id SalePrice
#>   <dbl>     <dbl>
#> 1  1461   169277.
#> 2  1462   187758.
#> 3  1463   183584.
#> 4  1464   179317.
#> 5  1465   150730.
#> 6  1466   177151.
```

#### Dealing with missing values

This data set has a lot of missing values. If we removed all rows which had a missing value of them, we would lost a significant amount of data. If the user does not want to deal with missing values (see the detailed analysis done for this data set [here](https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda)) `simplexgb` use a simple heuristic for dealing with missing values. For character variables, it use "not available" as a new level, while for numeric variables it generates random numbers from a normalized distribution of the available values of that variable. Clearly, there are better ways to impute missing values that include dependencies and correlations.. but this will suffice for now.

#### Preparing the training structure


```r
train$sale_price <- 1/sqrt(train$sale_price)
train_struct <- prepare_training_set(df = train, target_variable = "sale_price")
#> Warning in if (!is.na(facs_df)) {: the condition has length > 1 and only
#> the first element will be used
#> 
#>  iter imp variable
#>   1   1  lot_frontage*  alley*  mas_vnr_type  mas_vnr_area*  bsmt_qual  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_type2  electrical*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*
#>   2   1  lot_frontage*  alley*  mas_vnr_type*  mas_vnr_area  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_type2*  electrical  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*
#>   3   1  lot_frontage*  alley*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_type2*  electrical*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*
#>   4   1  lot_frontage*  alley*  mas_vnr_type*  mas_vnr_area  bsmt_qual*  bsmt_cond  bsmt_exposure*  bsmt_fin_type1  bsmt_fin_type2*  electrical*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*
#>   5   1  lot_frontage*  alley*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_type2  electrical*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*
#>   6   1  lot_frontage*  alley*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_type2*  electrical  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*
#>   7   1  lot_frontage*  alley*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_type2*  electrical*  fireplace_qu  garage_type*  garage_yr_blt*  garage_finish*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*
#>   8   1  lot_frontage*  alley*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_type2  electrical*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_qual  garage_cond*  pool_qc*  fence*  misc_feature*
#>   9   1  lot_frontage*  alley*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_type2*  electrical*  fireplace_qu  garage_type*  garage_yr_blt*  garage_finish  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*
#>   10   1  lot_frontage*  alley*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_type2*  electrical*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*
#>  * Please inspect the loggedEvents
#> Warning: Number of logged events: 368
```


#### Cross validation

**Guessing the hyperparameters**


```r
hyp <- guess_hyperparameters(train_structure = train_struct)
hyp %>% print()
#> $depth
#> [1] 8
#> 
#> $n_estimators
#> [1] 8000
#> 
#> $learning_rate
#> [1] 0.09036168
#> 
#> $objective_function
#> [1] "reg:linear"
#> 
#> $eval_metric
#> [1] "mae"
#> 
#> $nrounds
#> [1] 715
```

**Cross validation**


```r
cv_results <- cross_validate(train_structure = train_struct, hyperparameters = hyp, nfold = 5)
print(cv_results$metric)
#> [1] 0.000134
```

It is not really clear what that means, but let us see how we perform if we make a submission.

**Training a model**


```r
hyp <- guess_hyperparameters(train_structure = train_struct, depth = 3, n_estimators = 1e3)
print(hyp)
#> $depth
#> [1] 8
#> 
#> $n_estimators
#> [1] 8000
#> 
#> $learning_rate
#> [1] 0.09036168
#> 
#> $objective_function
#> [1] "reg:linear"
#> 
#> $eval_metric
#> [1] "mae"
#> 
#> $nrounds
#> [1] 715
xgbmodel <- train_model(train_structure = train_struct, hyperparameters = hyp)
```

**Predicting on the test set**


```r
pred_df <- get_predictions(xgbmodel, test_df = test)
#> Warning in if (!is.na(facs_df)) {: the condition has length > 1 and only
#> the first element will be used
#> 
#>  iter imp variable
#>   1   1  ms_zoning*  lot_frontage*  alley*  utilities*  exterior1st*  exterior2nd*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_sf1*  bsmt_fin_type2*  bsmt_fin_sf2*  bsmt_unf_sf*  total_bsmt_sf*  bsmt_full_bath*  bsmt_half_bath*  kitchen_qual*  functional*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_cars*  garage_area*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*  sale_type*
#>   2   1  ms_zoning*  lot_frontage*  alley*  utilities*  exterior1st*  exterior2nd*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_sf1*  bsmt_fin_type2*  bsmt_fin_sf2*  bsmt_unf_sf*  total_bsmt_sf*  bsmt_full_bath*  bsmt_half_bath*  kitchen_qual*  functional*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_cars*  garage_area*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*  sale_type*
#>   3   1  ms_zoning*  lot_frontage*  alley*  utilities*  exterior1st*  exterior2nd*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_sf1*  bsmt_fin_type2*  bsmt_fin_sf2*  bsmt_unf_sf*  total_bsmt_sf*  bsmt_full_bath*  bsmt_half_bath*  kitchen_qual*  functional*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_cars*  garage_area*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*  sale_type*
#>   4   1  ms_zoning*  lot_frontage*  alley*  utilities*  exterior1st*  exterior2nd*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_sf1*  bsmt_fin_type2*  bsmt_fin_sf2*  bsmt_unf_sf*  total_bsmt_sf*  bsmt_full_bath*  bsmt_half_bath*  kitchen_qual*  functional*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_cars*  garage_area*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*  sale_type*
#>   5   1  ms_zoning*  lot_frontage*  alley*  utilities*  exterior1st*  exterior2nd*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_sf1*  bsmt_fin_type2*  bsmt_fin_sf2*  bsmt_unf_sf*  total_bsmt_sf*  bsmt_full_bath*  bsmt_half_bath*  kitchen_qual*  functional*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_cars*  garage_area*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*  sale_type*
#>   6   1  ms_zoning*  lot_frontage*  alley*  utilities*  exterior1st*  exterior2nd*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_sf1*  bsmt_fin_type2*  bsmt_fin_sf2*  bsmt_unf_sf*  total_bsmt_sf*  bsmt_full_bath*  bsmt_half_bath*  kitchen_qual*  functional*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_cars*  garage_area*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*  sale_type*
#>   7   1  ms_zoning*  lot_frontage*  alley*  utilities*  exterior1st*  exterior2nd*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_sf1*  bsmt_fin_type2*  bsmt_fin_sf2*  bsmt_unf_sf*  total_bsmt_sf*  bsmt_full_bath*  bsmt_half_bath*  kitchen_qual*  functional*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_cars*  garage_area*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*  sale_type*
#>   8   1  ms_zoning*  lot_frontage*  alley*  utilities*  exterior1st*  exterior2nd*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_sf1*  bsmt_fin_type2*  bsmt_fin_sf2*  bsmt_unf_sf*  total_bsmt_sf*  bsmt_full_bath*  bsmt_half_bath*  kitchen_qual*  functional*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_cars*  garage_area*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*  sale_type*
#>   9   1  ms_zoning*  lot_frontage*  alley*  utilities*  exterior1st*  exterior2nd*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_sf1*  bsmt_fin_type2*  bsmt_fin_sf2*  bsmt_unf_sf*  total_bsmt_sf*  bsmt_full_bath*  bsmt_half_bath*  kitchen_qual*  functional*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_cars*  garage_area*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*  sale_type*
#>   10   1  ms_zoning*  lot_frontage*  alley*  utilities*  exterior1st*  exterior2nd*  mas_vnr_type*  mas_vnr_area*  bsmt_qual*  bsmt_cond*  bsmt_exposure*  bsmt_fin_type1*  bsmt_fin_sf1*  bsmt_fin_type2*  bsmt_fin_sf2*  bsmt_unf_sf*  total_bsmt_sf*  bsmt_full_bath*  bsmt_half_bath*  kitchen_qual*  functional*  fireplace_qu*  garage_type*  garage_yr_blt*  garage_finish*  garage_cars*  garage_area*  garage_qual*  garage_cond*  pool_qc*  fence*  misc_feature*  sale_type*
#>  * Please inspect the loggedEvents
#> Warning: Number of logged events: 663
pred_df %>% head()
#> # A tibble: 6 x 1
#>   sale_price
#>        <dbl>
#> 1    0.00297
#> 2    0.00251
#> 3    0.00270
#> 4    0.00277
#> 5    0.00260
#> 6    0.00265
```

**Constructing the submission**


```r
submission <- sample_sub
submission$Id <- test_id
submission$SalePrice <- 1/((pred_df$sale_price)^2)
submission %>% write_csv("../data/kaggle-house-prices/submission_basic.csv")
```

We see that while `simplexgb` will get you from start to submission in no time at all, there is no short cut to understanding the data and careful cleaning and feature engineering. See [this kernel](https://www.kaggle.com/raimohaikari/house-prices-pls) for another example. 
