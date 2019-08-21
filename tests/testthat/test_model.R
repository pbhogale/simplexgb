context("test_model.R")
data(iris)

tdf_c <- prepare_training_set(iris,"Species")
tdf_r <- prepare_training_set(iris,"Sepal.Length")
hyp_c <- guess_hyperparameters(tdf_c)
hyp_r <- guess_hyperparameters(tdf_r)
tmo_c <- train_model_xgb(tdf_c, hyp_c)
tmo_r <- train_model_xgb(tdf_r, hyp_r)
test_that("the hyperparameters array makes sense", {
  expect_equal(class(hyp_c), "list")
  expect_gt(hyp_c[["nrounds"]], 1)
  expect_equal(hyp_r[["objective_function"]], "reg:linear")
})

test_that("the cv function works as intended", {
  expect_match(cross_validate_xgb(train_structure = tdf_c,
                              hyperparameters = hyp_c)$cv_model$params$eval_metric, "mlogloss")
})

test_that("the predictions from the model are alright", {
  expect_gt(get_predictions_xgb(tmo_r, iris[1:1,])[["Sepal.Length"]][1], 2.0)
  expect_match(get_predictions_xgb(train_model_xgb(tdf_c, hyp_c), iris[1:1,])[["category"]][1], "setosa")
})

iris_mod <- iris
iris_mod[["Species"]][1] <- "hellohello"
test_that("a test set with new levels is handled properly",{
  expect_gt(get_predictions_xgb(model_structure = tmo_r, test_df = iris_mod)[["Sepal.Length"]][1], 2.0)
})
