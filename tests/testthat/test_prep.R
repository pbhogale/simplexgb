context("test_prep.R")
data(iris)
test_normalize_df <- function(){
  test_df <- simplexgb::normalize_df(iris, "Species", facs_df = simplexgb::get_normalizing_factors(iris))
  return(mean(test_df[["Sepal.Length"]]))
}

test_that("normalizing gives means close to 0", {
  expect_equal(test_normalize_df(), 0.0)
})

test_normalizing_factors <- function(){
  test_facs <- get_normalizing_factors(iris)
  test_df <- normalize_df(iris, "Species", facs_df = test_facs)
  colns <- colnames(test_facs)
  for(i in 1:length(colns)){
    test_df[[colns[i]]] <- test_df[[colns[i]]] * test_facs[[colns[i]]][2]
    test_df[[colns[i]]] <- test_df[[colns[i]]] + test_facs[[colns[i]]][1]
  }
  return(test_df)
}

test_const <- iris$Sepal.Length[1]
test_df <- test_normalizing_factors()
test_that("normalizing factors can be used to recreate original df", {
  expect_equal(test_df[["Sepal.Length"]][1], test_const)
  expect_equal(colnames(test_normalizing_factors()), colnames(iris))
})


iris_test <- iris
iris_test[["bad_column"]] <- rep(0, nrow(iris))

test_that("uninformative columns are removed",{
  expect_equal(colnames(remove_uninformative(iris_test)), colnames(iris))
})

test_that("the train levels returned are correct",{
  expect_equal(get_train_levels(iris)[["Species"]], unique(iris$Species))
})

test_that("making sure that the categorical target variable map is okay", {
  expect_match(as.character(transform_target_variable(iris, "Species")[["target_reference"]][["original_Species"]][1]), "setosa")
})

test_that("the prepared training data can be used",{
  expect_equal(prepare_training_set(iris, "Species")$data[["Sepal.Length"]][1],-0.89767388, tolerance=1e-3)
  expect_equal(prepare_training_set(iris, "Species")$normalize_by[["Sepal.Length"]][1],5.8433, tolerance=1e-3)
})

iris_mod <- iris
iris_mod[["Sepal.Length"]][c(1,2,3,4)] <- NA
iris_mod$Species <- as.character(iris_mod$Species)
iris_mod[["Species"]][c(1,2,3,4)] <- NA
iris_mod$Species <- as.factor(iris_mod$Species)
iris_mod_handle <- handle_missing_values(iris_mod, train_facs = get_normalizing_factors(iris))
test_that("the NA handling works", {
  expect_match(as.character(iris_mod_handle$Species[1]), "not_available")
  expect_false(is.na(iris_mod_handle$Sepal.Length[1]))
})
