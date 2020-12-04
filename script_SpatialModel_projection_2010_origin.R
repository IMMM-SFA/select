
###### for 2010 ######

## NOTE of CAUTION:
## be careful with .csv files, where Excell may interpret TPID "MAR*" as dates
## work-around: 1) copy column from source data (e.g. dbf file), 2) specify column type (in Excel), 3) save
##              (this may need to be done everytime the file needs to be saved in Excel)

######################
######################
currSSP <- "SSP5"
workspacePath <- "C:\\Users\\mcgr323\\OneDrive - PNNL\\Documents\\GitHub\\select"
trainingGrids <- read.csv("tbl_attr_1-8-dgr_training.csv"))
######################
######################

######################
### General Trends ###
######################
#' @description Creates a data frame of general trends
#' @param trainingGrids
#' @param general_trend_model_file
#' @return estDF
#' @export
general_trends <- function(trainingGrids,
                           general_trend_model_file= "Model_GeneralTrend.RData") {
  # use selected columns and rename them
  estDF <- trainingGrids[,c(1, 46)]
  colnames(estDF) <- c("originFID", "x")
  # load GeneralTrendModel
  load("Model_GeneralTrend.RData")
  # predict values based from the GeneralTrendModel for estDF
  estDF$GT <- predict(GeneralTrendModel, estDF)
  # remove x from the estDF dataframe
  estDF <- subset(estDF, select=c(-x))
  return(estDF)
}

#TEST THE FUNCTION
estDF <- general_trends(trainingGrids, general_trend_model_file= "Model_GeneralTrend.RData")


######################
# Data Updates & PCA #
######################
#' @description Updates the data and create PCAs
#' @param updateGrids
#' @param estDF #dataframe created by general_trends()
#' @param BUCovars #dataframe of focal statistics for 3 variables with moving window (3,5,7,9)
#' @param pcaModel #PCA model of BU data
#' @param selectedFeatures
#' @return estDF
#' @export
update_pca <- function(trainingGrids,
                       estDF,
                       pcaModel ="Model_BuPca.RData",
                       selectedFeatures = "temp_SelectedFeatures_projection.RData",
                       maskGrids = "data_FinalMask.csv") {
    #create dataframe of focal statistics
  BUCovars <- trainingGrids[,10:117]
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
  #rename the first column
  colnames(BUCovars)[1] <- "originFID"
  #save the file
  save(BUCovars, file = "temp_tbl_attr_old.RData")

  estDF$GrumpLndAr <- trainingGrids$GrumpLndAr
  maskGrids <- read.csv(maskGrids)
  estDF$mask <- maskGrids$FinalMask
  estDF$ISO <- trainingGrids$ISO
  estDF$TPID <- trainingGrids$TPID

  popGrids2000 <- read.csv( "data_2000TotalPop_SSPBaseYr.csv")
  popGridsSSP <- read.csv(paste0("data_", currSSP, "TotalPopSeries.csv"))

  print(class(popGridsSSP))
  print(class(trainingGrids))
  print(class(popGridsSSP[,2]))

  trainingGrids$ppCnt10 <- popGridsSSP[,2]
  trainingGrids$DppC00_10 <- popGridsSSP[,2] - popGrids2000[,2]
  trainingGrids$CRppC00_10 <- trainingGrids$DppC00_10 / popGrids2000[,2]
  trainingGrids$CRppC00_10 <- replace(trainingGrids$CRppC00_10, !is.finite(trainingGrids$CRppC00_10), 0)
  estDF$ppCntT2 <- trainingGrids$ppCnt10

  selectedFeatures <- cbind(trainingGrids[,1:9], PCs[,1:15], trainingGrids[,119:121])
  save(selectedFeatures, file = "temp_SelectedFeatures_projection.RData")

######################
######## GAMs ########
######################

LD_DF <- data.frame()

ISO3ExceptList <- read.csv(file.path(workspacePath,"ISO3s_exceptions.csv"))
ISO3ExceptList <- as.vector(names(ISO3ExceptList))


library(gam)

for (CTRY in ISO3ExceptList) {
  load(paste(workspacePath, "/Model_GAMs/GAMModels_LD_", CTRY, ".RData", sep=""))
  isoInGAM <- subset(selectedFeatures, grepl(paste("^", CTRY, sep=""), selectedFeatures$TPID))
  tpidList <- as.vector(unique(isoInGAM$TPID))



  # for (currTpid in tpidList) {
  #   inGAM <- subset(isoInGAM, grepl(paste("^", currTpid, "$", sep=""), isoInGAM$TPID))
  #   currDF <- as.data.frame(inGAM$originFID)
  #   currDF$LD <- predict(modelList[[currTpid]], inGAM)
  #   LD_DF <- rbind(LD_DF, currDF)
  #   print(paste(currTpid, ": ", dim(LD_DF), sep=""))
  # }
}




detach("package:gam", unload=TRUE)
detach("package:splines", unload=TRUE)
detach("package:foreach", unload=TRUE)
rm(ISO3ExceptList)

ISO3List <- read.csv(file.path(workspacePath,"ISO3s.csv"))
ISO3List <- as.vector(names(ISO3List))

library(mgcv)
for (CTRY in ISO3List) {
  load(paste(workspacePath, "/Model_GAMs/GAMModels_LD_", CTRY, ".RData", sep=""))
  isoInGAM <- subset(selectedFeatures, grepl(paste("^", CTRY, sep=""), selectedFeatures$TPID))
  tpidList <- as.vector(unique(isoInGAM$TPID))
  for (currTpid in tpidList) {
    inGAM <- subset(isoInGAM, grepl(paste("^", currTpid, "$", sep=""), isoInGAM$TPID))
    currDF <- as.data.frame(inGAM$originFID)
    currDF$LD <- predict(modelList[[currTpid]], inGAM)
    LD_DF <- rbind(LD_DF, currDF)
    print(paste(currTpid, ": ", dim(LD_DF), sep=""))
  }
}
detach("package:mgcv", unload=TRUE)
detach("package:nlme", unload=TRUE)
rm(ISO3List, CTRY, tpidList, currTpid, isoInGAM, inGAM, currDF, modelList, selectedFeatures, workspacePath)

colnames(LD_DF) <- c("originFID", "LD")
LD_DF <- LD_DF[order(LD_DF$originFID),]
estDF$newD <- estDF$GT + LD_DF$LD
rm(LD_DF)

# DELETE THIS - for testing only
test_LD_DF <- data.frame(LD_DF)


estDF$newR <- estDF$rT1 + estDF$newD
estDF$newR <- pmax(estDF$newR, estDF$rT1, na.rm = TRUE)
estDF$newR <- pmin(estDF$newR, estDF$mask, na.rm = TRUE)
estDF$newD <- estDF$newR - estDF$rT1
estDF$newD <- ifelse(estDF$newD < 0, 0, estDF$newD) # in case of rounding error

estDF$amtD <- estDF$GrumpLndAr * estDF$newD
estDF$availLnd <- estDF$GrumpLndAr * (estDF$mask - estDF$rT1)
estDF$availLnd <- ifelse(estDF$availLnd < 0, 0, estDF$availLnd) # in case of rounding error

save(estDF, file = "temp_estDF.RData")
# load("temp_estDF.RData")

NatDecBUAmtList <- read.csv(paste0("data_NationalBUAmts_", currSSP, ".csv"))
NatDecBUAmtList$BUAmtChg <- NatDecBUAmtList[,3] - NatDecBUAmtList[,2]

# "NATIONAL" total amount control: update newR, newD
CtryList <- as.vector(unique(estDF$ISO))
alloDF <- data.frame()
TPppBUList <- read.csv("data_2000TPppBU.csv")
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
