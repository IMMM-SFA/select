###### for 2020 and onward ######
## NOTE of CAUTION:
## be careful with .csv files, where Excell may interpret TPID "MAR*" as dates
## work-around: 1) copy column from source data (e.g. dbf file), 2) specify column type (in Excel), 3) save
##              (this may need to be done everytime the file needs to be saved in Excel)

######################
####SET PARAMETERS####
######################

setwd("C:\\Users\\mcgr323\\OneDrive - PNNL\\Documents\\GitHub\\select")
workspacePath <- "C:\\Users\\mcgr323\\OneDrive - PNNL\\Documents\\GitHub\\select"
# 3 for 2020, +1 per later decade, 11 for 2100;
# differently, for National BUAmts, this is the column ID for the beginning of the decade, i.e. 3 for 2010
endPopColID <- 6
currSSP <- "SSP1"
# make sure temp_tbl_attr_updates.csv is sorted on originFID (low to high)
updateGrids <-read.csv("temp_tbl_attr_updates.csv")

######################
### General Trends ###
######################
#' @description Creates a data frame of general trends
#' @param updateGrids
#' @param general_trend_model_file
#' @return estDF
#' @export
general_trends <- function(updateGrids,
                           general_trend_model_file= "Model_GeneralTrend.RData") {
  # use selected columns and rename them
  estDF <- updateGrids[,1:2]
  colnames(estDF) <- c("originFID", "x")
  # load GeneralTrendModel
  load(general_trend_model_file)
  # predict values based from the GeneralTrendModel for estDF
  estDF$GT <- predict(GeneralTrendModel, estDF)
  # remove x from the estDF dataframe
  estDF <- subset(estDF, select=c(-x))
  return(estDF)
}

#TEST THE FUNCTION
estDF_package_20 <- general_trends(updateGrids, general_trend_model_file= "Model_GeneralTrend.RData")
all.equal(estDF_package_20,estDF_20) #TRUE

######################
##Data Updates & PCA##
######################
#' @description Updates the data and create PCAs
#' @param updateGrids
#' @param estDF #dataframe created by general_trends()
#' @param BUCovars #dataframe of focal statistics for 3 variables with moving window (3,5,7,9)
#' @param pcaModel #PCA model of BU data
#' @param selectedFeatures selected features of model outputs
#' @importFrom car predict
#' @return list; extracted components from update Grids data => list(bu_covars, selectedFeatures)
#' @export
update_pca <- function(updateGrids,
                       estDF,
                       BUCovars = "temp_tbl_attr_old.RData",
                       pcaModel ="Model_BuPca.RData",
                       selectedFeatures = "temp_SelectedFeatures_projection.RData") {
  #create dataframe of focal statistics
  load(BUCovars)
  oldGrids <- BUCovars
  BUCovars <- cbind(oldGrids[,20:55], updateGrids[,2:19], oldGrids[,74:91], updateGrids[,20:55])
  colnames(BUCovars) <- c("r80", "r80_slope", "r80_mean3", "r80_mean5", "r80_mean7", "r80_mean9", "r80_slrg3", "r80_slrg5", "r80_slrg7",
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
  #predicted values based from the pcaModel for BUCovars
  load(pcaModel)
  PCs <- predict(pcaModel, newdata = BUCovars)
  #assign values to columns in estDF dataframe
  estDF$rT1 <- BUCovars$r00
  estDF$dT0T1 <- BUCovars$d9000

  #bind orginFID to BUcovars dataframe
  BUCovars <- cbind(estDF$originFID, BUCovars)
  colnames(BUCovars)[1] <- "originFID"
  #save the new BUCovars as RData output
  save(BUCovars, file = "temp_tbl_attr_old.RData")
  #load the popGrids file for the current SSP
  popGrids <- read.csv(paste0("data_", currSSP, "TotalPopSeries.csv"))

  #add new columns to estDF from selectedFeatures
  load(selectedFeatures)
  estDF$GrumpLndAr <- selectedFeatures$GrumpLndAr
  estDF$mask <- selectedFeatures$mask
  estDF$ISO <- selectedFeatures$ISO
  estDF$TPID <- selectedFeatures$TPID

  #TODO: what is this calculating?
  selectedFeatures$ppCnt10 <- popGrids[,endPopColID]
  selectedFeatures$DppC00_10 <- popGrids[,endPopColID] - popGrids[,(endPopColID-1)]
  selectedFeatures$CRppC00_10 <- selectedFeatures$DppC00_10 / popGrids[,(endPopColID-1)]
  selectedFeatures$CRppC00_10 <- replace(selectedFeatures$CRppC00_10, !is.finite(selectedFeatures$CRppC00_10), 0)
  estDF$ppCntT2 <- selectedFeatures$ppCnt10

  #create new selectedFeatures dataframe and save
  selectedFeatures <- cbind(selectedFeatures[,1:9], PCs[,1:15], selectedFeatures[,25:27])
  save(selectedFeatures, file = "temp_SelectedFeatures_projection.RData")
  return(list("bu_covars" = BUCovars,
              "selectedFeatures" = selectedFeatures))
}

#TEST THE FUNCTION
list_2_20 <- update_pca (updateGrids,estDF,BUCovars = "temp_tbl_attr_old.RData",
                     pcaModel ="Model_BuPca.RData",selectedFeatures = "temp_SelectedFeatures_projection.RData")

all.equal(list_2_20$bu_covars, BUCovars) #TRUE
all.equal(list_2_20$selectedFeatures, selectedFeatures) #TRUE

######################
######## GAMs ########
######################
#' @description Create dataframe from GAM models
#' @param ISO3ExceptList
#' @param selectedFeatures
#' @param ISO3List
#' @param estDF
#' @param TPppBUList
#' @return estDF
#' @importFrom gam gam
#' @importFrom dplyr left_join
#' @importFrom mgcv gam
#' @return list; extracted components from update Grids data => list(estDF, alloDF)
#' @export
gam_dataframe <- function(ISO3ExceptList = "ISO3s_exceptions.csv",
                selectedFeatures= "temp_SelectedFeatures_projection.RData",
                ISO3List ="ISO3s.csv",
                estDF,
                TPppBUList = "data_2000TPppBU.csv") {
  #create an empty dataframe
  LD_DF <- data.frame()
  #load the ISO3ExceptList and convert to vector
  ISO3ExceptList <- read.csv(ISO3ExceptList)
  ISO3ExceptList <- as.vector(names(ISO3ExceptList))

  #for each CTRY in ISO3Expect list load the GAM model
  for (CTRY in ISO3ExceptList) {
    load(paste(workspacePath, "Model_GAMs/GAMModels_LD_", CTRY, ".RData", sep=""))
    #create dataframe of selectedFeatures TPID subset by CTRY
    isoInGAM <- subset(selectedFeatures, grepl(paste("^", CTRY, sep=""), selectedFeatures$TPID))
    #create vector of unique TPID in isoInGAM
    tpidList <- as.vector(unique(isoInGAM$TPID))

    for (currTpid in tpidList) {
      inGAM <- subset(isoInGAM, grepl(paste("^", currTpid, "$", sep=""), isoInGAM$TPID))
      #create dataframe of originFID
      currDF <- as.data.frame(inGAM$originFID)
      #add model predictions for list of models
      currDF$LD <- predict(modelList[[currTpid]], inGAM)
      #create new dataframe
      LD_DF <- rbind(LD_DF, currDF)
      print(paste(currTpid, ": ", dim(LD_DF), sep=""))
    }
  }

  #load the ISO3 list and convert to vector
  ISO3List <- read.csv(ISO3List)
  ISO3List <- as.vector(names(ISO3List))

  #for each CTRY in ISO3Expect list load the GAM model
  for (CTRY in ISO3List) {
    load(paste(workspacePath, "Model_GAMs/GAMModels_LD_", CTRY, ".RData", sep=""))
    #create dataframe of selectedFeatures TPID subset by CTRY
    isoInGAM <- subset(selectedFeatures, grepl(paste("^", CTRY, sep=""), selectedFeatures$TPID))
    #create vector of unique TPID in isoInGAM
    tpidList <- as.vector(unique(isoInGAM$TPID))
    for (currTpid in tpidList) {
      inGAM <- subset(isoInGAM, grepl(paste("^", currTpid, "$", sep=""), isoInGAM$TPID))
      #create dataframe of originFID
      currDF <- as.data.frame(inGAM$originFID)
      #add model predictions for list of models
      currDF$LD <- predict(modelList[[currTpid]], inGAM)
      #create new dataframe
      LD_DF <- rbind(LD_DF, currDF)
      print(paste(currTpid, ": ", dim(LD_DF), sep=""))
    }
  }
  #change the column names of the LD_DF dataframe
  colnames(LD_DF) <- c("originFID", "LD")
  #order the dataframe based on originFID
  LD_DF <- LD_DF[order(LD_DF$originFID),]
  #TODO create new columns (specfically?)
  estDF$newD <- estDF$GT + LD_DF$LD
  estDF$newR <- estDF$rT1 + estDF$newD
  #returns the parallel maxima
  estDF$newR <- pmax(estDF$newR, estDF$rT1, na.rm = TRUE)
  #returns the parallel minima
  estDF$newR <- pmin(estDF$newR, estDF$mask, na.rm = TRUE)
  #TODO create new columns (specfically?)
  estDF$newD <- estDF$newR - estDF$rT1
  #if value is less than 0 set to 0
  estDF$newD <- ifelse(estDF$newD < 0, 0, estDF$newD) # in case of rounding error
  #TODO create new columns (specfically?)
  estDF$amtD <- estDF$GrumpLndAr * estDF$newD
  estDF$availLnd <- estDF$GrumpLndAr * (estDF$mask - estDF$rT1)
  estDF$availLnd <- ifelse(estDF$availLnd < 0, 0, estDF$availLnd) # in case of rounding error

  #save estDF dataframe
  save(estDF, file = "temp_estDF.RData")

  #load file for current SSP
  NatDecBUAmtList <- read.csv(paste0("data_NationalBUAmts_", currSSP, ".csv"))
  #TODO create new column (specfically?)
  NatDecBUAmtList$BUAmtChg <- NatDecBUAmtList[,(endPopColID+1)] - NatDecBUAmtList[,endPopColID]

  #create vector of unique countries
  CtryList <- as.vector(unique(estDF$ISO))
  #create empty dataframe
  alloDF <- data.frame()
  #load dataframe
  TPppBUList <- read.csv(TPppBUList)
  #for each country in country list subset estDF
  for (CTRY in CtryList) {
    ctryEstDF <- subset(estDF, grepl(paste("^", CTRY, "$", sep=""), estDF$ISO))
    #sum the availLnd
    sumAvailLnd <- sum(ctryEstDF$availLnd)
    #subset the NatDecBUAmtList
    ctryBUChg <- subset(NatDecBUAmtList, grepl(paste("^", CTRY, "$", sep=""), NatDecBUAmtList$ISO3v10))$BUAmtChg
    #sum the amtD
    sumAmtD <- sum(ctryEstDF$amtD)
    #create dataframes based off available land vs. overflow values
    #if ctryBUChg has no change create dataframe
    if (ctryBUChg == 0) {
      currAlloDF <- as.data.frame(ctryEstDF$originFID)
      colnames(currAlloDF) <- "originFID"
      currAlloDF$newR <- ctryEstDF$rT1
      currAlloDF$newD <- 0
      alloDF <- rbind(alloDF, currAlloDF)
      print(paste(CTRY, ": mode 0 (no change)", sep=""))

      #else if available land is less than/ equal to overflow create dataframe
    } else if (sumAvailLnd <= ctryBUChg) {
      currAlloDF <- as.data.frame(ctryEstDF$originFID)
      colnames(currAlloDF) <- "originFID"
      currAlloDF$newR <- ctryEstDF$mask
      currAlloDF$newD <- currAlloDF$newR - ctryEstDF$rT1
      currAlloDF$newD <- ifelse(currAlloDF$newD < 0, 0, currAlloDF$newD) # in case of rounding error
      alloDF <- rbind(alloDF, currAlloDF)
      print(paste(CTRY, ": mode 1 (total overflows avail land)", sep=""))

      #else if available land is more than overflow create dataframe
    } else { # sumAvailLnd > ctryBUChg
      tpidList <- as.vector(unique(ctryEstDF$TPID))
      tpidDistrDF <- as.data.frame(tapply(ctryEstDF$ppCntT2, ctryEstDF$TPID, sum))
      colnames(tpidDistrDF) <- "ppCntT2"
      tpidDistrDF$availLnd <- as.vector(tapply(ctryEstDF$availLnd, ctryEstDF$TPID, sum))
      tpidDistrDF <- subset(tpidDistrDF, !is.na(tpidDistrDF$ppCntT2))
      tpidDistrDF$TPID <- row.names(tpidDistrDF)
      tpidDistrDF <- left_join(tpidDistrDF, TPppBUList, by = "TPID")
      tpidDistrDF$ppCntT2 <- tpidDistrDF$ppCntT2 / 1000000 * tpidDistrDF$ppBU00_m.2
      #TODO: what is being calculated?
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

          #else if available land is more than/ equal to overflow create dataframe
        } else if (sumAmtD >= tpidBUChg) {
          scaler <- tpidBUChg / sumAmtD
          tpidEstDF$newD <- tpidEstDF$newD * scaler
          currAlloDF <- as.data.frame(tpidEstDF$originFID)
          colnames(currAlloDF) <- "originFID"
          currAlloDF$newR <- tpidEstDF$rT1 + tpidEstDF$newD
          currAlloDF$newR <- pmin(currAlloDF$newR, tpidEstDF$mask, na.rm = TRUE) # in case of rounding error
          currAlloDF$newD <- tpidEstDF$newD
          alloDF <- rbind(alloDF, currAlloDF)
          print(paste(currTPID, ": mode 3 (potential >= total, proportionally scale down)", sep=""))

          #else if available land is less than overflow create dataframe
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

  #order estDF by originFID
  estDF <- estDF[order(estDF$originFID),]

  #order alloDF by originFID
  alloDF <- alloDF[order(alloDF$originFID),]

  #TODO: what is being calculated
  alloDF$newA <- alloDF$newD - estDF$dT0T1
  year <- 2000 + (endPopColID - 1) * 10
  #write alloDF csv
  write.csv(alloDF, file = paste0("tbl_inputToArcGIS_", currSSP, "_", year, ".csv"))

  return(list("estDF" = estDF,
              "alloDF" = alloDF))
}
#TEST THE FUNCTION
list_3 <- gam_dataframe(ISO3ExceptList = "ISO3s_exceptions.csv",
              selectedFeatures= "temp_SelectedFeatures_projection.RData",
              ISO3List ="ISO3s.csv",estDF,TPppBUList = "data_2000TPppBU.csv")
