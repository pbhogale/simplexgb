# simplexgb : helper functions to deploy ML models

This package is a wrapper with helper functions to help deploy xgboost, glmnet and random forest models in R. it takes care of tracking categorical levels,
transforming numerical features, getting predictions and so on in a consistent way across the different packages (mainly xgboost and glmnet).
It uses the `tibble` as a fundamental data structure, and uses lists of tibbles internally. 
