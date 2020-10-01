library(rio)

setwd("C:\\Users\\mcgr323\\projects\\select")

dbfA <- import("_tempCentroidnewA.dbf")
dbfD <- import("_tempCentroidnewD.dbf")
dbfR <- import("_tempCentroidnewR.dbf")

dbf <- cbind(dbfA, dbfD, dbfR)

library(arsenal)
comparedf(dbf,focal_data)