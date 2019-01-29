library(data.table)
library(dplyr)
library(feather)


#' Apply is.finite() to data.frame objects
#'
#' @param obj data.frame; Target data frame
#' @return is.finite() response
#' @export
is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}


#' Prepare general trends data
#'
#' @param est_df_file Full path with file name and extension to est_df file.
#' @param general_trend_file Full path with file name and extension to general trends file.
#' @return Tibble, [originFID, gt]; predicted values associated with each feature id
#' @export
calc_general_trends <- function(est_df_file, general_trend_file) {
  
  # read training file feather
  est_df <- feather::read_feather(est_df_file)

  # load general trend data
  gt <- base::readRDS(general_trend_file)
  
  # predicted values obtained by evaluating the regression function
  est_df$GT <- stats::predict(gt, est_df)
  
  # return the predicted values associated with each feature id
  return(base::subset(est_df, select = c(-x)))
}


#' Extract covariate data and save original file with feature id
#'
#' @param est_df Tibble; <need description>
#' @param bu_covars_file Full path with file name and extension to bu_covars file.
#' @param bu_covars_file_old Full path with file name and extension to save formatted bu_covars file.
#' @return Tibble; predicted values associated with each component
#' @export
format_covars <- function(est_df, bu_covars_file, bu_covars_file_old) {
  
  # read bu_covars feather extracted from training grids
  bu_covars <- feather::read_feather(bu_covars_file)
  
  # append fields
  est_df$rT1 <- bu_covars$r00
  est_df$dT0T1 <- bu_covars$d9000
  
  # append feature id from est_df and set column name
  bu_covars <- cbind(est_df$originFID, bu_covars)
  colnames(bu_covars)[1] <- "originFID"
  
  # save to file
  feather::write_feather(bu_covars, bu_covars_file_old)
  
  # load principle components data and run prediction
  return(est_df)
}


#' Principle Components Analysis
#'
#' @param bu_covars_file Full path with file name and extension to bu_covars file.
#' @param pca_file Full path with file name and extension to PCA file.
#' @return Tibble; predicted values associated with each component
#' @export
calc_pca <- function(bu_covars_file, pca_file) {
  
  # read bu_covars feather extracted from training grids
  bu_covars <- feather::read_feather(bu_covars_file)

  # load principle components data and run prediction
  pca <- base::readRDS(pca_file) %>%
          stats::predict(newdata = bu_covars)
  
  return(pca[,1:15])
}


#' Append fields from training data to est_df
#'
#' @param est_df Tibble; <NEED DESCRIPTION>.
#' @param grump_land_ar Field; <NEED DESCRIPTION>.
#' @param iso Field; <NEED DESCRIPTION>.
#' @param tpid Field; <NEED DESCRIPTION>.
#' @return Tibble; predicted values associated with each component
#' @export
append_fields <- function(est_df, grump_land_ar, iso, tpid) {
  
  # append fields
  est_df$GrumpLndAr <- grump_land_ar
  est_df$ISO <- iso
  est_df$TPID <- tpid
  
  return(est_df)
}


#' Append mask grids to data
#'
#' @param est_df Tibble; <NEED DESCRIPTION>.
#' @param mask_file Full path to file with file name and extension.
#' @return Tibble; predicted values associated with each component
#' @export
apply_mask <- function(est_df, mask_file) {
  
  # read in mask data
  mask_grids <- data.table::fread(mask_file)
  
  # append field
  est_df$mask <- mask_grids$FinalMask
  
  return(est_df)
}


#' Calculate population data
#'
#' @param est_df Tibble; <NEED DESCRIPTION>.
#' @param population_grids_base_file Full path to file with file name and extension to population base year file.
#' @param population_grids_ssp_file Full path to file with file name and extension to target SSP population file.
#' @param beginning_training_file Full path to file with file name and extension to first part of training data.
#' @return data.frame; Training data with updated population estimates.
#' @export
calc_population <- function(population_grids_base_file, population_grids_ssp_file, beginning_training_file) {
  
  # read in population and training data
  pop_base <- data.table::fread(population_grids_base_file)
  pop_ssp <- data.table::fread(population_grids_ssp_file)
  train_data <- feather::read_feather(beginning_training_file)

  # calculate population fraction and replace NaN with 0
  train_data$ppCnt10 <- pop_ssp[,2]
  train_data$DppC00_10 <- train_data$ppCnt10 - pop_base[,2]
  train_data$CRppC00_10 <- train_data$DppC00_10 / pop_base[,2]
  train_data$CRppC00_10 <- base::replace(train_data$CRppC00_10, !is.finite.data.frame(train_data$CRppC00_10), 0)
  
  return(train_data)
}


#' Create selected features data frame
#'
#' @param beginning_training_data data.frame; First nine fields in training data.
#' @param pca_data data.frame; PCA data.
#' @param ending_training_data data.frame; Last three fields in training data.
#' @param select_features_file Full path to file with file name and extension to save to.
#' @return data.frame; Bound selected features
#' @export
build_selected_features <- function(beginning_training_data, pca_data, ending_training_data, selected_features_file) {
  
  selected_features <- cbind(beginning_training_data, pca_data, ending_training_data)
  
  # save output
  #feather::write_feather(selected_features, selected_features_file)
  
  return(selected_features)
}


















