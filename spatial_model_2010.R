#' @description Convert RData file to RDS file
#' @param rdata_file #TODO what is this?
#' @param rds_file #TODO what is this?
#' @return RDS
#' @export
rdata_to_rds <- function(rdata_file, rds_file) {

  # get rdata object name as a string and load data
  rdata_obj_str <- load(rdata_file)

  # create variable from string
  rdata_obj <- get(rdata_obj_str)

  # save as RDS file
  rds <- saveRDS(rdata_obj, rds_file)
  return(rds)
}

#' @description Convert RData file to feather file
#' @param rdata_file #TODO what is this?
#' @param feather_file #TODO what is this?
#' @importFrom feather write_feather
#' @return rds_feather
#' @export
rdata_to_feather <- function(rdata_file, feather_file) {

  # get rdata object name as a string and load data
  rdata_obj_str <- load(rdata_file)

  # create variable from string
  rdata_obj <- get(rdata_obj_str)

  # save as RDS file
  rds_feather <- write_feather(rdata_obj, rds_file)
  return(rds_feather)
}


#' @description Import training data and extract parts.
#' @param training_data_file data.frame; First nine fields in training data.
#' @importFrom data.table fread
#' @return list; extracted components from training data => list(est_df,  bu_covars, beginning_training_data,
#'         ending_training_data, grump_land_ar, iso, tpid)
#' @export
read_training_data <- function(training_data_file) {

  training_data <- fread(training_data_file)

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

#' @description Apply is.finite() to data.frame objects
#' @param obj data.frame; Target data frame
#' @return is.finite() response
#' @export
is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}

#' @description Prepare general trends data
#' @param training_data list of data.table; Training data.
#' @param general_trend_file Full path with file name and extension to general trends file.
#' @importFrom stats predict
#' @return training_data predicted values associated with each feature id
#' @export
calc_general_trends <- function(training_data, general_trend_file) {

  # load general trend data
  gt <- readRDS(general_trend_file)

  # predicted values obtained by evaluating the regression function
  training_data$est_df$GT <- predict(gt, training_data$est_df)

  # return the predicted values associated with each feature id
  training_data$est_df <- base::subset(training_data$est_df, select = c(-x))

  return(training_data)
}


#' @description Extract covariate data and save original file with feature id
#' @param training_data list of data.table; Training data
#' @param bu_covars_file_store Full path with file name and extension to save formatted bu_covars file.
#' @importFrom feather write_feather
#' @return training_data
#' @export
format_covars <- function(training_data, bu_covars_file_store, fid_field_name = "originFID") {

  # append fields
  training_data$est_df$rT1 <- training_data$bu_covars$r00
  training_data$est_df$dT0T1 <- training_data$bu_covars$d9000

  # append feature id from est_df and set column name
  training_data$bu_covars <- cbind(training_data$est_df$originFID, training_data$bu_covars)
  colnames(training_data$bu_covars)[1] <- fid_field_name

  # save to file
  write_feather(training_data$bu_covars, bu_covars_file_store)

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

#' @description Create selected features data frame
#' @param beginning_training_data data.frame; First nine fields in training data.
#' @param pca_data data.frame; PCA data.
#' @param ending_training_data data.frame; Last three fields in training data.
#' @param select_features_file Full path to file with file name and extension to save to.
#' @return selected_features data.frame; Bound selected features
#' @export
build_selected_features <- function(training_data, pca_data, selected_features_file) {

  selected_features <- cbind(training_data$beginning_training_data, pca_data, training_data$ending_training_data)

  return(selected_features)
}


#' @description Append mask and population fields
#' @param training_data list of data.table; <NEED DESCRIPTION>.
#' @param mask_file Full path to file with file name and extension.
#' @param select_feature_field
#' @importFrom data.table fread
#' @return training_data$est_df Tibble; predicted values associated with each component
#' @export
append_fields <- function(training_data, mask_file, select_feature_field) {

  # read in mask data
  mask_grids <- fread(mask_file)

  # append fields
  training_data$est_df$GrumpLndAr <- training_data$grump_land_ar
  training_data$est_df$mask <- mask_grids$FinalMask
  training_data$est_df$ISO <- training_data$iso
  training_data$est_df$TPID <- training_data$tpid
  training_data$est_df$ppCntT2 <- select_feature_field

  return(training_data$est_df)
}


#' @description Generate PCA and linear model to training data.
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


#' @description Process TPIDs for each GAM
#' @param ld_df data.frame; Stores GAM outputs
#' @param iter_file Full path with file name and extension of the file containing iteration values.
#' @param gam_dir Full path the the GAM file directory.
#' @param selected_features data.frame; First nine fields in training data.
#' @param predict_status int; 0 == use GAM predict; 1 == use MGCV predict; else, do not load
#' @importFrom data.table fread
#' @importFrom stats predict
#' @return ld_df data.frame Store of GAM outputs
#' @export
process_iso_gam <- function(ld_df, iter_file, gam_dir, selected_features, predict_status = 0) {

  # load target stats library
  if (predict_status == 0) {
  }
  else if (predict_status == 1) {
  }

  iter_list <- fread(iter_file) %>%
    names() %>%
    as.vector()

  for (ctry in iter_list) {

    # load GAM file
    gam_data <- file.path(gam_dir, paste0('GAMModels_LD_', ctry, '.rds')) %>%
                  readRDS()

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
  }
  else if (predict_status == 1) {
  }
  return(ld_df)
}


#' @description Sort data frame by column values
#' @param df data.frame; In data frame
#' @param col Name of column to sort by.
#' @return data.frame
#' @export
sort_df_by_column <- function(df, col) {
  return(df[order(df[,col]),])
}

#' @description Rename all columns in a data frame
#' @param df data.frame; In data frame
#' @param col_list list; List of column names (e.g.: `c("col_1", "col_2")`)
#' @return data.frame
#' @export
rename_columns <- function(df, col_list) {
  return(colnames(df) <- col_list)
}

#' @description Load GAM data frame and aggregate with general trends
#' @param trained_output list of data.frame; Trained data output.
#' @param gam_dir Full path to the directory containing the GAM data.
#' @param iso3_except_file Full path with file name and extension to the ISO exeption file.
#' @param iso3_include_file Full path with file name and extension to the ISO inclusion file.
#' @return list of data.frame; Trained output with modified columns.
#' @export
aggregate_gt_with_gam <- function(trained_output, gam_dir, iso3_except_file, iso3_include_file) {

  # generalized additive model
  ld_df <- data.frame()

  # TODO:  is `gam` and `mgcv` package being used? Is it supposed to be using the nested `predict` methods?
  # process ISO exceptions and inclusions
  ld_df <- process_iso_gam(ld_df, iso3_except_file, gam_dir, trained_output$selected_features, predict_status = 0) %>%
    process_iso_gam(iso3_include_file, gam_dir, trained_output$selected_features, predict_status = 1) %>%
    rename_columns(c("originFID", "LD")) %>%
    sort_df_by_column("originFID")

  # build aggregated field
  trained_output$est_df$newD <- trained_output$est_df$GT + ld_df$LD

  return(trained_output)
}


#' @description Calculate parallel maxima and minima
#' @param trained_output list of data.frame; Trained data output.
#' @return list of data.frame; Trained output with modified columns.
#' @export
calc_parallel_max_min <- function(trained_output) {

  trained_output$est_df$newR <- trained_output$est_df$rT1 + trained_output$est_df$newD
  trained_output$est_df$newR <- pmax(trained_output$est_df$newR, trained_output$est_df$rT1, na.rm = TRUE)
  trained_output$est_df$newR <- pmin(trained_output$est_df$newR, trained_output$est_df$mask, na.rm = TRUE)
  trained_output$est_df$newD <- trained_output$est_df$newR - trained_output$est_df$rT1

  # in case of rounding error
  trained_output$est_df$newD <- ifelse(trained_output$est_df$newD < 0, 0, trained_output$est_df$newD)

  trained_output$est_df$amtD <- trained_output$est_df$GrumpLndAr * trained_output$est_df$newD
  trained_output$est_df$availLnd <- trained_output$est_df$GrumpLndAr * (trained_output$est_df$mask - trained_output$est_df$rT1)

  # in case of rounding error
  trained_output$est_df$availLnd <- ifelse(trained_output$est_df$availLnd < 0, 0, trained_output$est_df$availLnd)

  return(trainind_output)
}



# ------------------------------
# SETUP PROJECT
# ------------------------------

data_dir <- '/Users/d3y010/projects/jing/data'
gam_dir <- 'gam'
feather_dir <- 'feather'
training_file <- 'tbl_attr_1-8-dgr_training.csv'
ssp = 'SSP5'

# training data file
training_data_file <- file.path(data_dir, training_file)

bu_covars_file_store <- file.path(data_dir, feather_dir, 'temp_tbl_attr_old.feather')
selected_features_file <- file.path(data_dir, feather_dir, 'temp_SelectedFeatures_projection.feather')

general_trend_file_rdata <- file.path(data_dir, 'Model_GeneralTrend.Rdata')
general_trend_file <- file.path(data_dir, 'Model_GeneralTrend.rds')
#rdata_to_rds(general_trend_file_rdata, general_trend_file)

pca_file_rdata <- file.path(data_dir, 'Model_BuPca.RData')
pca_file <- file.path(data_dir, 'Model_BuPca.rds')
#rdata_to_rds(pca_file_rdata, pca_file)

mask_file <- file.path(data_dir, 'data_FinalMask.csv')

population_grids_base_file <- file.path(data_dir, 'data_2000TotalPop_SSPBaseYr.csv')
population_grids_ssp_file <- file.path(data_dir, paste0('data_', ssp, 'TotalPopSeries.csv'))

iso3_except_file <- file.path(data_dir,'ISO3s_exceptions.csv')
iso3_include_file <- file.path(data_dir,'ISO3s.csv')

tp_pp_bu_file <- file.path(data_dir, "data_2000TPppBU.csv")



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
                                 mask_file) %>%
                  aggregate_gt_with_gam(gam_dir,
                                        iso3_except_file,
                                        iso3_include_file) %>%
                  calc_parallel_max_min()


# tests
all.equal(current=data.table::setDT(estDF), target=trained_output$est_df)
all.equal(current=data.table::setDT(selectedFeatures), target=trained_output$selected_features)


run_part <- function(training_outputs, data_dir, tp_pp_bu_file, ssp) {

  national_bu_amounts_list <- data.table.fread(file.path(data_dir, paste0("data_NationalBuAmts_", ssp, ".csv")))
  national_bu_amounts_list$BUAmtChg <- national_bu_amounts_list[,3] - national_bu_amounts_list[,2]

  tp_pp_bu_list <- data.table.fread(tp_pp_bu_file)

  # "NATIONAL" total amount control: update newR, newD
  ctry_list <- as.vector(unique(training_outputs$est_df$ISO))
  allo_df <- data.frame()


  for (ctry in ctry_list) {

    ctry_est_df <- training_outputs$est_df[grepl(paste("^", ctry, sep=""), ISO)]
    sum_avail_lnd <- sum(ctry_est_df$availLnd)

    ctry_bu_chg <- national_bu_amounts_list[grepl(paste("^", ctry, "$", sep=""), BUAmtChg)]
    sum_amt_d <- sum(ctry_est_df$amtD)

    if (ctry_bu_chg == 0) {
      curr_allo_df <- as.data.frame()
      currAlloDF <- as.data.frame(ctryEstDF$originFID)
      colnames(currAlloDF) <- "originFID"
      currAlloDF$newR <- ctryEstDF$rT1
      currAlloDF$newD <- 0
      alloDF <- rbind(alloDF, currAlloDF)
      print(paste(CTRY, ": mode 0 (no change)", sep=""))

    }

}

for (CTRY in CtryList) {
  ctryEstDF <- subset(estDF, grepl(paste("^", CTRY, "$", sep=""), estDF$ISO))

  sumAvailLnd <- sum(ctryEstDF$availLnd)
  ctryBUChg <- subset(NatDecBUAmtList, grepl(paste("^", CTRY, "$", sep=""), NatDecBUAmtList$ISO3v10))$BUAmtChg
  sumAmtD <- sum(ctryEstDF$amtD)

  if (ctryBUChg == 0) {
    currAlloDF <- as.data.frame(ctryEstDF$originFID)
    colnames(currAlloDF) <- "originFID"
    currAlloDF$newR <- ctryEstDF$rT1
    currAlloDF$newD <- 0
    alloDF <- rbind(alloDF, currAlloDF)
    print(paste(CTRY, ": mode 0 (no change)", sep=""))

  } else if (sumAvailLnd <= ctryBUChg) {
    currAlloDF <- as.data.frame(ctryEstDF$originFID)
    colnames(currAlloDF) <- "originFID"
    currAlloDF$newR <- ctryEstDF$mask
    currAlloDF$newD <- currAlloDF$newR - ctryEstDF$rT1
    currAlloDF$newD <- ifelse(currAlloDF$newD < 0, 0, currAlloDF$newD) # in case of rounding error
    alloDF <- rbind(alloDF, currAlloDF)
    print(paste(CTRY, ": mode 1 (total overflows avail land)", sep=""))

  } else { # sumAvailLnd > ctryBUChg
    tpidList <- as.vector(unique(ctryEstDF$TPID))
    tpidDistrDF <- as.data.frame(tapply(ctryEstDF$ppCntT2, ctryEstDF$TPID, sum))
    colnames(tpidDistrDF) <- "ppCntT2"
    tpidDistrDF$availLnd <- as.vector(tapply(ctryEstDF$availLnd, ctryEstDF$TPID, sum))
    tpidDistrDF <- subset(tpidDistrDF, !is.na(tpidDistrDF$ppCntT2))
    tpidDistrDF$TPID <- row.names(tpidDistrDF)
    library("dplyr")
    tpidDistrDF <- left_join(tpidDistrDF, TPppBUList, by = "TPID")
    detach("package:dplyr", unload=TRUE)
    tpidDistrDF$ppCntT2 <- tpidDistrDF$ppCntT2 / 1000000 * tpidDistrDF$ppBU00_m.2

    sumT2Pop <- sum(tpidDistrDF$ppCntT2)
    tpidDistrDF$tpidBUChg <- ctryBUChg * tpidDistrDF$ppCntT2 / sumT2Pop
    tpidDistrDF$overflow <- tpidDistrDF$tpidBUChg - tpidDistrDF$availLnd
    sumTpidOverflow <- sum(tpidDistrDF$overflow[tpidDistrDF$overflow > 0])

    while (sumTpidOverflow > 0) {
      sumT2Pop <- sum(tpidDistrDF$ppCntT2[tpidDistrDF$overflow < 0])
      tpidDistrDF$tpidBUChg <- ifelse(tpidDistrDF$overflow < 0,
                                      (sumTpidOverflow * tpidDistrDF$ppCntT2 / sumT2Pop + tpidDistrDF$tpidBUChg),
                                      tpidDistrDF$availLnd)
      tpidDistrDF$overflow <- tpidDistrDF$tpidBUChg - tpidDistrDF$availLnd
      sumTpidOverflow <- sum(tpidDistrDF$overflow[tpidDistrDF$overflow > 0])
    }
    rm(sumT2Pop, sumTpidOverflow)

    for (currTPID in tpidList) {
      tpidEstDF <- subset(ctryEstDF, grepl(paste("^", currTPID, "$", sep=""), ctryEstDF$TPID))

      sumAvailLnd <- sum(tpidEstDF$availLnd)
      tpidBUChg <- subset(tpidDistrDF, grepl(paste("^", currTPID, "$", sep=""), tpidDistrDF$TPID))$tpidBUChg
      sumAmtD <- sum(tpidEstDF$amtD)

      if (sumAmtD == 0) {
        sumRT1 <- sum(tpidEstDF$rT1)
        scaler <- tpidBUChg / sumRT1
        tpidEstDF$amtD <- tpidEstDF$rT1 * scaler

        tpidEstDF$overflow <- tpidEstDF$amtD - tpidEstDF$availLnd
        sumOverflow <- sum(tpidEstDF[tpidEstDF$overflow > 0, ]$overflow)
        tpidEstDF$amtD <- ifelse(tpidEstDF$overflow < 0, tpidEstDF$amtD, tpidEstDF$availLnd)

        while (sumOverflow > 0) {
          sumRT1 <- sum(tpidEstDF[tpidEstDF$overflow < 0, ]$rT1)
          scaler <- sumOverflow / sumRT1
          tpidEstDF$amtD <- ifelse(tpidEstDF$overflow < 0, (tpidEstDF$rT1 * scaler + tpidEstDF$amtD), tpidEstDF$amtD)
          tpidEstDF$overflow <- tpidEstDF$amtD - tpidEstDF$availLnd
          sumOverflow <- sum(tpidEstDF[tpidEstDF$overflow > 0, ]$overflow)
          tpidEstDF$amtD <- ifelse(tpidEstDF$overflow < 0, tpidEstDF$amtD, tpidEstDF$availLnd)
        }

        rm(scaler, sumOverflow)
        tpidEstDF$newD <- tpidEstDF$amtD / tpidEstDF$GrumpLndAr
        tpidEstDF$newR <- tpidEstDF$rT1 + tpidEstDF$newD
        tpidEstDF$newR <- pmax(tpidEstDF$newR, tpidEstDF$rT1, na.rm = TRUE)
        tpidEstDF$newR <- pmin(tpidEstDF$newR, tpidEstDF$mask, na.rm = TRUE)
        tpidEstDF$newD <- tpidEstDF$newR - tpidEstDF$rT1
        tpidEstDF$newD <- ifelse(tpidEstDF$newD < 0, 0, tpidEstDF$newD) # in case of rounding error

        currAlloDF <- as.data.frame(tpidEstDF$originFID)
        colnames(currAlloDF) <- "originFID"
        currAlloDF$newR <- tpidEstDF$newR
        currAlloDF$newD <- tpidEstDF$newD
        alloDF <- rbind(alloDF, currAlloDF)
        print(paste(currTPID, ": mode 2 (potential = 0, iteratively fill according to rT1)", sep=""))

      } else if (sumAmtD >= tpidBUChg) {
        scaler <- tpidBUChg / sumAmtD
        tpidEstDF$newD <- tpidEstDF$newD * scaler
        rm(scaler)
        currAlloDF <- as.data.frame(tpidEstDF$originFID)
        colnames(currAlloDF) <- "originFID"
        currAlloDF$newR <- tpidEstDF$rT1 + tpidEstDF$newD
        currAlloDF$newR <- pmin(currAlloDF$newR, tpidEstDF$mask, na.rm = TRUE) # in case of rounding error
        currAlloDF$newD <- tpidEstDF$newD
        alloDF <- rbind(alloDF, currAlloDF)
        print(paste(currTPID, ": mode 3 (potential >= total, proportionally scale down)", sep=""))

      } else { # sumAmtD < tpidBUChg
        scaler <- tpidBUChg / sumAmtD
        tpidEstDF$amtD <- tpidEstDF$amtD * scaler

        tpidEstDF$overflow <- tpidEstDF$amtD - tpidEstDF$availLnd
        sumOverflow <- sum(tpidEstDF[tpidEstDF$overflow > 0, ]$overflow)
        tpidEstDF$amtD <- ifelse(tpidEstDF$overflow < 0, tpidEstDF$amtD, tpidEstDF$availLnd)

        while (sumOverflow > 0) {
          sumAmtD <- sum(tpidEstDF[tpidEstDF$overflow < 0, ]$amtD)
          if (sumAmtD > 0) {
            scaler <- sumOverflow / sumAmtD
            tpidEstDF$amtD <- ifelse(tpidEstDF$overflow < 0, (tpidEstDF$amtD * scaler + tpidEstDF$amtD), tpidEstDF$amtD)
          } else {
            sumRT1 <- sum(tpidEstDF[tpidEstDF$overflow < 0, ]$rT1)
            scaler <- sumOverflow / sumRT1
            tpidEstDF$amtD <- ifelse(tpidEstDF$overflow < 0, (tpidEstDF$rT1 * scaler + tpidEstDF$amtD), tpidEstDF$amtD)
          }
          tpidEstDF$overflow <- tpidEstDF$amtD - tpidEstDF$availLnd
          sumOverflow <- sum(tpidEstDF[tpidEstDF$overflow > 0, ]$overflow)
          tpidEstDF$amtD <- ifelse(tpidEstDF$overflow < 0, tpidEstDF$amtD, tpidEstDF$availLnd)
        }

        rm(scaler, sumOverflow)
        tpidEstDF$newD <- tpidEstDF$amtD / tpidEstDF$GrumpLndAr
        tpidEstDF$newR <- tpidEstDF$rT1 + tpidEstDF$newD
        tpidEstDF$newR <- pmax(tpidEstDF$newR, tpidEstDF$rT1, na.rm = TRUE)
        tpidEstDF$newR <- pmin(tpidEstDF$newR, tpidEstDF$mask, na.rm = TRUE)
        tpidEstDF$newD <- tpidEstDF$newR - tpidEstDF$rT1
        tpidEstDF$newD <- ifelse(tpidEstDF$newD < 0, 0, tpidEstDF$newD) # in case of rounding error

        currAlloDF <- as.data.frame(tpidEstDF$originFID)
        colnames(currAlloDF) <- "originFID"
        currAlloDF$newR <- tpidEstDF$newR
        currAlloDF$newD <- tpidEstDF$newD
        alloDF <- rbind(alloDF, currAlloDF)
        print(paste(currTPID, ": mode 4 (potential < total, iteratively fill)", sep=""))
      }
    }
  }
}
rm(CTRY, CtryList, ctryBUChg, sumAmtD, sumAvailLnd, ctryEstDF, currAlloDF, NatDecBUAmtList, sumRT1,
   currTPID, tpidBUChg, tpidList, tpidDistrDF, TPppBUList, tpidEstDF)
# colnames(alloDF) <- c("originFID", "newR", "newD")

estDF <- estDF[order(estDF$originFID),]
alloDF <- alloDF[order(alloDF$originFID),]

alloDF$newA <- alloDF$newD - estDF$dT0T1
write.csv(alloDF, file = paste0("tbl_inputToArcGIS_", currSSP, "_2010.csv"))
rm(estDF, alloDF, currSSP)









