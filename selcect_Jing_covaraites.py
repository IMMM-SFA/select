flds = [i.name for i in arcpy.ListFields("gridCntrLayer")]
print(flds)

# convert attributes to rasters, using iterations (~2 min per raster)
for outName in fileNameArr:
    arcpy.AddMessage("start raster conversion: "+outName)
    arcpy.PointToRaster_conversion(gridCntrLyr, "tbl_inputToArcGIS_SSP5_2010.csv."+outName, outName, "MEAN")
    arcpy.AddMessage("complete raster conversion: "+outName)

# load necesary tools
arcpy.CheckOutExtension("spatial")
from arcpy.sa import *

# derive focal means & STDs, iterating through the rasters
arcpy.AddMessage("start mean & std")
for i in range(0, len(fileNameArr), 1):
    inRas = arcpy.Raster(fileNameArr[i])
    for size in sizeArr:
        meanRas = FocalStatistics(inRas, NbrRectangle(size,size,"CELL"), "MEAN", "DATA")
        meanRas.save(fileNameArr[i]+"mean"+str(size))
        arcpy.AddMessage("complete mean: "+str(size))
        stdRas = FocalStatistics(inRas, NbrRectangle(size,size,"CELL"), "STD", "DATA")
        stdRas.save(fileNameArr[i]+"std"+str(size))
        arcpy.AddMessage("complete std: "+str(size))
arcpy.AddMessage("complete mean & std")

# derive standardized positions, iterating through the rasters
arcpy.AddMessage("start std position")
for i in range(0, len(fileNameArr), 1):
    inRas = arcpy.Raster(fileNameArr[i])
    for size in sizeArr:
        meanRas = arcpy.Raster(fileNameArr[i]+"mean"+str(size))
        stdRas = arcpy.Raster(fileNameArr[i]+"std"+str(size))
        outFile = fileNameArr[i]+"stps"+str(size)
        outRas = Con(IsNull(inRas), inRas, ((inRas-meanRas)/stdRas))
        outRas = Con(IsNull(outRas), 0, outRas)
        outRas.save(outFile)
arcpy.AddMessage("complete std position")

# derive slope range, iterating through the rasters
arcpy.AddMessage("start slope & slope range")
for i in range(0, len(fileNameArr), 1):
    inRas = arcpy.Raster(fileNameArr[i])
    slopeRas = Slope(inRas, "PERCENT_RISE")
    slopeRas = Con(IsNull(slopeRas), 0, slopeRas)
    slopeRas.save(fileNameArr[i]+"slope")
    for size in sizeArr:
        outRas = FocalStatistics(slopeRas, NbrRectangle(size,size,"CELL"), "RANGE", "DATA")
        outRas.save(fileNameArr[i]+"slrg"+str(size))
arcpy.AddMessage("complete slope & slope range")

# convert 1/8-dgr fishnet to centroids
arcpy.AddMessage("start centroid conversion")
fishnetFile =  "C:\\Users\\mcgr323\\OneDrive - PNNL\\Documents\\GitHub\\select\\LandMask_1-8-degree_DATA\\LandMask_1-8-degree_fishnet_centroids.shp"
centroidFile = "_tempCentroid"+fileNameArr[0]+".shp"
arcpy.FeatureToPoint_management(fishnetFile, centroidFile, "CENTROID")
arcpy.AddMessage("complete centroid conversion")
for i in range(1, len(fileNameArr), 1):
    arcpy.Copy_management(centroidFile, ("_tempCentroid"+fileNameArr[i]+".shp"))
arcpy.AddMessage("complete centroid copies")

# extract raster values to centroids
arcpy.AddMessage("start raster value extraction")
statStrArr = ["mean", "slrg", "std", "stps"] # "slope" - one per var
sizeStrArr = ["3", "5", "7", "9"]
for var in fileNameArr:
    centroidFile = "_tempCentroid"+var+".shp"
    rasArr = []
    rasArr.append([var,var])
    rasArr.append([(var+"slope"),(var+"_slope")])
    for stat in statStrArr:
        for size in sizeStrArr:
            rasArr.append([(var+stat+size),(var+"_"+stat+size)])
    arcpy.sa.ExtractMultiValuesToPoints(centroidFile, rasArr, "NONE")
    arcpy.AddMessage("complete raster value extraction: " + var)
arcpy.AddMessage("complete raster value extraction")

####################
# In ArcMap, visualize all raster files as a sanity check
# MANUALLY, merge the three _tempCentroid .dbf files, "copy" and "paste values only" to the updates.csv for R
#     a final update file is usually around 500MB
####################