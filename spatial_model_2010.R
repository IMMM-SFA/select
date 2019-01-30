library(data.table)
library(dplyr)
library(feather)
library(tools)

rdata_to_rds <- function(rdata_file, rds_file) {
  
  # get rdata object name as a string and load data
  rdata_obj_str <- base::load(rdata_file)
  
  # create variable from string
  rdata_obj <- get(rdata_obj_str)
  
  # save as RDS file
  base::saveRDS(rdata_obj, rds_file)
}


rdata_to_feather <- function(rdata_file, feather_file) {
  
  # get rdata object name as a string and load data
  rdata_obj_str <- base::load(rdata_file)
  
  # create variable from string
  rdata_obj <- get(rdata_obj_str)
  
  # save as RDS file
  feather::write_feather(rdata_obj, rds_file)
}



#' Import training data and extract parts.
#'
#' @param training_data_file data.frame; First nine fields in training data.
#' @return list; extracted components from training data => list(est_df,  bu_covars, beginning_training_data, 
#'         ending_training_data, grump_land_ar, iso, tpid)
#' @export
read_training_data <- function(training_data_file) {
  
  training_data <- data.table::fread(training_data_file)
  
  # convert character fields imported as character to factor
  training_data$TPID <- as.factor(training_data$TPID)
  training_data$ISO <- as.factor(training_data$ISO)

  # extract est_df
  est_df <- training_data[, c(1, 46)]
  colnames(est_df) <- c("originFID", "x")
  
  # extract bu_covars
  bu_covars <- training_data[, 10:117]
  colnames(bu_covars) <- c("r80", "r80_slope", "r80_mean3", "r80_mean5", "r80_mean7", "r80_mean9", "r80_slrg3", "r80_slrg5", "r80_slrg7",
                           "r80_slrg9", "r80_std3", "r80_std5", "r80_std7", "r80_std9", "r80_stps3", "r80_stps5", "r80_stps7", "r80_stps9",
                           "r90", "r90_slope", "r90_mean3", "r90_mean5", "r90_mean7", "r90_mean9", "r90_slrg3", "r90_slrg5", "r90_slrg7",
                           "r90_slrg9", "r90_std3", "r90_std5", "r90_std7", "r90_std9", "r90_stps3", "r90_stps5", "r90_stps7", "r90_stps9",
                           "r00", "r00_slope", "r00_mean3", "r00_mean5", "r00_mean7", "r00_mean9", "r00_slrg3", "r00_slrg5", "r00_slrg7",
                           "r00_slrg9", "r00_std3", "r00_std5", "r00_std7", "r00_std9", "r00_stps3", "r00_stps5", "r00_stps7", "r00_stps9",
                           "d8090", "d8090slope", "d8090mean3", "d8090mean5", "d8090mean7", "d8090mean9", "d8090slrg3", "d8090slrg5",
                           "d8090slrg7", "d8090slrg9", "d8090std3", "d8090std5", "d8090std7", "d8090std9", "d8090stps3", "d8090stps5",
                           "d8090stps7", "d8090stps9", "d9000", "d9000slope", "d9000mean3", "d9000mean5", "d9000mean7", "d9000mean9",
                           "d9000slrg3", "d9000slrg5", "d9000slrg7", "d9000slrg9", "d9000std3", "d9000std5", "d9000std7", "d9000std9",
                           "d9000stps3", "d9000stps5", "d9000stps7", "d9000stps9", "a8000", "a8000slope", "a8000mean3", "a8000mean5",
                           "a8000mean7", "a8000mean9", "a8000slrg3", "a8000slrg5", "a8000slrg7", "a8000slrg9", "a8000std3", "a8000std5",
                           "a8000std7", "a8000std9", "a8000stps3", "a8000stps5", "a8000stps7", "a8000stps9")
  
  # extract beginning fields
  beginning_training_data <- training_data[,1:9]
  
  # extract end fields
  ending_training_data <- training_data[,119:121]
  
  # extract target fields
  grump_land_ar <- training_data$GrumpLndAr
  iso <- training_data$ISO
  tpid <- training_data$TPID
  
  return(list("est_df" = est_df, 
              "bu_covars" = bu_covars, 
              "beginning_training_data" = beginning_training_data, 
              "ending_training_data" = ending_training_data,
              "grump_land_ar" = grump_land_ar,
              "iso" = iso,
              "tpid" = tpid))
}


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
#' @param training_data list of data.table; Training data.
#' @param general_trend_file Full path with file name and extension to general trends file.
#' @return data.table, [originFID, GT]; predicted values associated with each feature id
#' @export
calc_general_trends <- function(training_data, general_trend_file) {
  
  # load general trend data
  gt <- base::readRDS(general_trend_file)
  
  # predicted values obtained by evaluating the regression function
  training_data$est_df$GT <- stats::predict(gt, training_data$est_df)
  
  # return the predicted values associated with each feature id
  training_data$est_df <- base::subset(training_data$est_df, select = c(-x))
                                       
  return(training_data)
}


#' Extract covariate data and save original file with feature id
#'
#' @param training_data list of data.table; Training data
#' @param bu_covars_file_store Full path with file name and extension to save formatted bu_covars file.
#' @return list of data.table; Predicted values associated with each component
#' @export
format_covars <- function(training_data, bu_covars_file_store, fid_field_name = "originFID") {
  
  # append fields
  training_data$est_df$rT1 <- training_data$bu_covars$r00
  training_data$est_df$dT0T1 <- training_data$bu_covars$d9000
  
  # append feature id from est_df and set column name
  training_data$bu_covars <- cbind(training_data$est_df$originFID, training_data$bu_covars)
  colnames(training_data$bu_covars)[1] <- fid_field_name
  
  # save to file
  feather::write_feather(training_data$bu_covars, bu_covars_file_store)
  
  # load principle components data and run prediction
  return(training_data)
}


#' Principle Components Analysis
#'
#' @param bu_covars data.table; Extracted from training data
#' @param pca_file Full path with file name and extension to PCA file.
#' @return Tibble; predicted values associated with each component
#' @export
calc_pca <- function(bu_covars, pca_file) {
  
  # load principle components data and run prediction
  pca <- base::readRDS(pca_file) %>%
          stats::predict(newdata = bu_covars)
  
  return(pca[,1:15])
}


#' Calculate population data
#'
#' @param est_df Tibble; <NEED DESCRIPTION>.
#' @param population_grids_base_file Full path to file with file name and extension to population base year file.
#' @param population_grids_ssp_file Full path to file with file name and extension to target SSP population file.
#' @param beginning_training_file Full path to file with file name and extension to first part of training data.
#' @return data.frame; Training data with updated population estimates.
#' @export
calc_population <- function(training_data, population_grids_base_file, population_grids_ssp_file) {
  
  # read in population and training data
  pop_base <- data.table::fread(population_grids_base_file)
  pop_ssp <- data.table::fread(population_grids_ssp_file)

  # calculate population fraction and replace NaN with 0
  training_data$beginning_training_data$ppCnt10 <- pop_ssp[,2]
  training_data$beginning_training_data$DppC00_10 <- training_data$beginning_training_data$ppCnt10 - pop_base[,2]
  training_data$beginning_training_data$CRppC00_10 <- training_data$beginning_training_data$DppC00_10 / pop_base[,2]
  training_data$beginning_training_data$CRppC00_10 <- base::replace(training_data$beginning_training_data$CRppC00_10, 
                                                                    !is.finite.data.frame(training_data$beginning_training_data$CRppC00_10),
                                                                    0)
  
  return(training_data)
}


#' Create selected features data frame
#'
#' @param beginning_training_data data.frame; First nine fields in training data.
#' @param pca_data data.frame; PCA data.
#' @param ending_training_data data.frame; Last three fields in training data.
#' @param select_features_file Full path to file with file name and extension to save to.
#' @return data.frame; Bound selected features
#' @export
build_selected_features <- function(training_data, pca_data, selected_features_file) {
  
  selected_features <- cbind(training_data$beginning_training_data, pca_data, training_data$ending_training_data)
  
  # save output
  #feather::write_feather(selected_features, selected_features_file)
  
  return(selected_features)
}


#' Append mask and population fields
#'
#' @param training_data list of data.table; <NEED DESCRIPTION>.
#' @param mask_file Full path to file with file name and extension.
#' @param select_feature_field 
#' @return Tibble; predicted values associated with each component
#' @export
append_fields <- function(training_data, mask_file, select_feature_field) {
  
  # read in mask data
  mask_grids <- data.table::fread(mask_file)
  
  # append fields
  training_data$est_df$GrumpLndAr <- training_data$grump_land_ar
  training_data$est_df$mask <- mask_grids$FinalMask
  training_data$est_df$ISO <- training_data$iso
  training_data$est_df$TPID <- training_data$tpid
  training_data$est_df$ppCntT2 <- select_feature_field
  
  return(training_data$est_df)
}



#' Generate PCA and linear model to training data.
#'
#' @param training_data_file data.frame; First nine fields in training data.
#' @param pca_file data.frame; First nine fields in training data.
#' @param population_grids_base_file data.frame; First nine fields in training data.
#' @param population_grids_ssp_file data.frame; First nine fields in training data.
#' @param selected_features_file data.frame; First nine fields in training data.
#' @param general_trend_file data.frame; First nine fields in training data.
#' @param mask_file data.frame; First nine fields in training data.
#' @return list; (selected_features, est_df)
#' @export
apply_training <- function(training_data_file, 
                           pca_file, 
                           population_grids_base_file, 
                           population_grids_ssp_file,
                           selected_features_file,
                           general_trend_file,
                           mask_file) {
  
  training_data <- read_training_data(training_data_file)
  
  
  # calculate PCA
  pca_data <- calc_pca(training_data$bu_covars, pca_file)
  
  # update population data in training data and bind selected features and save output
  selected_features <- calc_population(training_data, population_grids_base_file, population_grids_ssp_file) %>%
    build_selected_features(pca_data, selected_features_file)
  
  # calculate general trends, format covariate file, apply mask, append fields
  est_df <- calc_general_trends(training_data, general_trend_file) %>%
    format_covars(bu_covars_file_store) %>%
    append_fields(mask_file, selected_features$ppCnt10)
  
  return(list("selected_features" = selected_features,
              "est_df" = est_df))
}



#' Process TPIDs for each GAM
#'
#' @param ld_df data.frame; Stores GAM outputs
#' @param iter_file Full path with file name and extension of the file containing iteration values.
#' @param gam_dir Full path the the GAM file directory.
#' @param selected_features data.frame; First nine fields in training data.
#' @param predict_status int; 0 == use GAM predict; 1 == use MGCV predict; else, do not load
#' @return data.frame Store of GAM outputs
#' @export
process_iso_gam <- function(ld_df, iter_file, gam_dir, selected_features, predict_status = 0) {
  
  # load target stats library
  if (predict_status == 0) {
    library(gam)
  }
  else if (predict_status == 1) {
    library(mgcv)
  }
  
  iter_list <- data.table::fread(iter_file) %>%
    names() %>%
    as.vector()
  
  for (ctry in iter_list) {
    
    # load GAM file
    gam_data <- file.path(gam_dir, paste0('GAMModels_LD_', ctry, '.rds')) %>%
                  base::readRDS()
    
    # get records from selected features where country abbreviation in TPID
    iso_in_gam <- selected_features[grepl(paste('^', ctry, sep=''), TPID)]

    # get a vector of unique TPID codes for the target country
    tpid_list <- as.vector(unique(iso_in_gam$TPID))
    
    for (target_tpid in tpid_list) {
      
      # get all records where TPID is target TPID
      in_gam <- iso_in_gam[grepl(paste('^', target_tpid, sep=''), TPID)]
      
      curr_df <- as.data.frame(in_gam$originFID)
      
      curr_df$LD <- predict(gam_data[[target_tpid]], in_gam)
      
      ld_df <- rbind(ld_df, curr_df)
    }
  }
  
  # remove target stats library
  if (predict_status == 0) {
    detach("package:gam", unload=TRUE)
    detach("package:splines", unload=TRUE)
    detach("package:foreach", unload=TRUE)
  }
  else if (predict_status == 1) {
    detach("package:mgcv", unload=TRUE)
    detach("package:nlme", unload=TRUE)
  }

  return(ld_df)
}



# ------------------------------
# SETUP PROJECT
# ------------------------------

root_dir <- '/Users/d3y010/projects/jing'
data_dir <- 'data'
gam_dir <- 'gam'
feather_dir <- 'feather'
training_file <- 'tbl_attr_1-8-dgr_training.csv'
ssp = 'SSP5'

# training data file 
training_data_file <- file.path(root_dir, data_dir, training_file)

bu_covars_file_store <- file.path(root_dir, data_dir, feather_dir, 'temp_tbl_attr_old.feather')
selected_features_file <- file.path(root_dir, data_dir, feather_dir, 'temp_SelectedFeatures_projection.feather')

general_trend_file_rdata <- file.path(root_dir, data_dir, 'Model_GeneralTrend.Rdata')
general_trend_file <- file.path(root_dir, data_dir, 'Model_GeneralTrend.rds')
#rdata_to_rds(general_trend_file_rdata, general_trend_file)

pca_file_rdata <- file.path(root_dir, data_dir, 'Model_BuPca.RData')
pca_file <- file.path(root_dir, data_dir, 'Model_BuPca.rds')
#rdata_to_rds(pca_file_rdata, pca_file)

mask_file <- file.path(root_dir, data_dir, 'data_FinalMask.csv')

population_grids_base_file <- file.path(root_dir, data_dir, 'data_2000TotalPop_SSPBaseYr.csv')
population_grids_ssp_file <- file.path(root_dir, data_dir, paste0('data_', ssp, 'TotalPopSeries.csv'))

iso3_except_file <- file.path(root_dir, data_dir,'ISO3s_exceptions.csv')
iso3_include_file <- file.path(root_dir, data_dir,'ISO3s.csv')



gam_dir = file.path(root_dir, data_dir, gam_dir)




# ------------------------------
# END PROJECT SETUP
# ------------------------------



# ------------------------------
# MAIN
# ------------------------------

# outputs trained_output$selected_features, trained_output$est_df
trained_output <- apply_training(training_data_file,
                                 pca_file,
                                 population_grids_base_file,
                                 population_grids_ssp_file,
                                 selected_features_file,
                                 general_trend_file,
                                 mask_file)



# generalized additive model
ld_df <- data.frame()

# START HERE Figure out Error in 1:object$nsdf error - this ran after running original version first

# TODO:  is `gam` and `mgcv` package being used? Is is supposed to be using the nested `predict` methods?
# process ISO exceptions and inclusions
ld_df <- process_iso_gam(ld_df, iso3_except_file, gam_dir, trained_output$selected_features, predict_status = 0) %>% 
            process_iso_gam(iso3_include_file, gam_dir, trained_output$selected_features, predict_status = 1)

colnames(ld_df) <- c("originFID", "LD")

# sort data frame by feature id
ld_df <- ld_df[order(ld_df$originFID),]








# tests
all.equal(current=data.table::setDT(estDF), target=trained_output$est_df)
all.equal(current=data.table::setDT(selectedFeatures), target=trained_output$selected_features)
