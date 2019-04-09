#' this function returns the factors by which a df was normalized,
#' so that the test dataset can be processed similarly
#' @inheritParams normalize_df
#' @return data frame with the normalizing factors
#' @export
get_normalizing_factors <- function(df, target_variable=NA){
  colns <- colnames(df)
  coln_class <- df %>%
    dplyr::summarise_all(class)
  selected_colns <- colns
  for(i in 1:length(colns)){
    if(coln_class[[colns[i]]][1]!="numeric"){
      selected_colns <- selected_colns[selected_colns!=colns[i]]
    }
  }
  if(!is.na(target_variable)){
    selected_colns <- selected_colns[selected_colns!=target_variable]
  }
  facs <- list()
  for(i in 1:length(selected_colns)){
    temp <- df[[selected_colns[i]]] - mean(df[[selected_colns[i]]], na.rm = T)
    facs[[selected_colns[i]]] <- c(mean(df[[selected_colns[i]]], na.rm = T), sd(temp, na.rm = T))
  }

  facs_df <- facs %>% dplyr::as_tibble() %>% tibble::rownames_to_column() %>% dplyr::select(-rowname)
  return(facs_df)
}


#' This function normalizes the n umeric columns in a data frame by
#' subtracting the mean and dividing by the standard deviation
#' given by the df from get_normalizing_factors
#' @param df a data frame
#' @param target_variable a string name for the target variable
#' @param facs_df the factors to normalize by
#' @return a data frame with numerical columns normalized
normalize_df <- function(df, target_variable=NA, facs_df){
  norm_colns <- colnames(facs_df)
  for(i in 1:length(norm_colns)){
    df[[norm_colns[i]]] <- (df[[norm_colns[i]]] - facs_df[[norm_colns[i]]][1])
    df[[norm_colns[i]]] <- (df[[norm_colns[i]]] / facs_df[[norm_colns[i]]][2])
  }
  return(df)
}


#' remove columns from the df that contain no information,
#' like factors with only one level
#' @param df a data frame
#' @return data frame with uninformative columns removed
remove_uninformative <- function(df){
  colns <- colnames(df)
  for(i in 1:length(colns)){
    if(length(unique(df[[colns[i]]]))<2){
      df[[colns[i]]] <- NULL
    }
  }
  return(df)
}



#' This function returns a data frame of all the various categories in the
#' training set, so that these are available while pre-processing the test set
#' @param df the training set data frame
#' @return a data frame with all the columns and all levels in the categorical columns
get_train_levels <- function(df){
  colns <- colnames(df)
  coln_class <- df %>%
    dplyr::summarise_all(class)
  max_levels <- 1
  levels_store <- list()
  for(i in 1:length(colns)){
    if(coln_class[[colns[i]]][1]!="numeric"){
      levels_store[[colns[i]]] <- df[[colns[i]]] %>% forcats::fct_unique()
      if(length(levels_store[[colns[i]]])>max_levels){
        max_levels <- length(levels_store[[colns[i]]])
      }
    }
    else {
      levels_store[[colns[i]]] <- c(0)
    }
  }

  for(i in 1:length(colns)){
    levels_store[[colns[i]]] <- c(levels_store[[colns[i]]], rep(levels_store[[colns[i]]][1],max_levels - length(levels_store[[colns[i]]])))
  }

  levels_df <- levels_store %>% dplyr::as_tibble() %>% tibble::rownames_to_column() %>% dplyr::select(-rowname)
  return(levels_df)
}


#' This function converts the target variable to integer if it is a
#' categorical variable. and builds a map of the transformation.
#' @inheritParams normalize_df
#' @return list
transform_target_variable <- function(df, target_variable){
  return_structure <-  list()
  return_df <- list()
  if(class(df[[target_variable]]) != "numeric"){
    return_df[[target_variable]] <- as.integer(df[[target_variable]])-1
    return_df[[paste("original",target_variable,sep = "_")]] <- df[[target_variable]]
    return_df <- tibble::as_tibble(return_df)
    reference_df <- return_df %>%
      dplyr::group_by_at(paste("original",target_variable,sep = "_")) %>%
      dplyr::summarise_at(.vars = target_variable, .funs = min) %>%
      dplyr::mutate_at(.vars = target_variable, .funs = as.integer)
    return_df[[paste("original",target_variable,sep = "_")]] <- NULL
    return_structure[["new_target"]] <- return_df
    return_structure[["target_reference"]] <- reference_df
  } else {
    return_structure[["new_target"]] <- NA
    return_structure[["target_reference"]] <- NA
  }
  return(return_structure)
}


#' this function takes a data frame and the target variable and
#' returns a data structure with everything needed to train and
#' predict with the model.
#' @inheritParams normalize_df
#' @return list of data frames
#' @export
prepare_training_set <- function(df, target_variable = "y"){
  train_facs <- get_normalizing_factors(df, target_variable)
  train_data <- normalize_df(df, target_variable, train_facs)
  target_reference <- transform_target_variable(df, target_variable)
  train_levels <- get_train_levels(df)
  train_structure <- list()
  if(class(df[[target_variable]]) != "numeric"){
    train_data[[target_variable]] <- target_reference[["new_target"]][[target_variable]]
    train_structure[["target_reference"]] <- target_reference[["target_reference"]]
  }
  train_structure[["data"]] <- train_data
  train_structure[["normalize_by"]] <- train_facs
  train_structure[["levels"]] <- train_levels
  train_structure[["target_variable"]] <- target_variable
  return(train_structure)
}
