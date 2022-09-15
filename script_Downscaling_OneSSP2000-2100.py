import sys
import arcpy
arcpy.CheckOutExtension("spatial")

currSSP = sys.argv[1]

output_path = './output'
if len(sys.argv) > 2:
    output_path = sys.argv[2]

start_decade = 1
if len(sys.argv) > 3:
    start_decade = int(sys.argv[3])

#### set parameters ####
# CONSTANT: 1/8-dgr input path
inPath = f'{output_path}/{currSSP}'
# CONSTANT: 1-km output path
outPath = inPath
## CONSTANT VARIABLES ##
# Envir Var - scratch space, now renamed as ./******
arcpy.env.workspace = "./downscale_inputs"
# Envir Var - overwrite flag
arcpy.env.overwriteOutput = True
# constant map layers
fineLandAreaR = arcpy.Raster("1km_land_area_km2.tif")
coarseLandAreaR = arcpy.Raster("18dgrTrue_land_area_km2.tif")
supScalerR = arcpy.Raster("SupScaler_forNoData.tif")
# More Envir Vars
arcpy.env.extent = fineLandAreaR
arcpy.env.snapRaster = fineLandAreaR
arcpy.env.cellSize = fineLandAreaR
arcpy.env.compression = 'LZW'

for endYr in range(2000 + (start_decade * 10), 2101, 10):
    beginYr = endYr-10
    coarseZoneR = arcpy.Raster("Fishnet_of18dgr_in1km_forZonalStat.tif")
    flagR = arcpy.Raster("1km_flag_allZero.tif")
    
    if endYr == 2010:
        fineFracR = arcpy.Raster("urb_frac_2000.tif")
    else:  # i.e. endYr = 2020~2100
        fineFracR = arcpy.Raster(f'{outPath}/urban_fraction_1km_{currSSP}_{beginYr}.tif')
    fineAmtR = fineFracR * fineLandAreaR
    fineAmtR.save("temp_fineAmt.tif")
    del fineFracR

    coarseAmtR = arcpy.sa.ZonalStatistics(coarseZoneR, "VALUE", fineAmtR, "Sum")
    coarseAmtR.save("temp_coarseAmt_beginYr_forScaler.tif")
    scalerRawR = fineAmtR / coarseAmtR
    scalerRawR.save("temp_scalerWithNoData.tif")
    scalerR = arcpy.sa.Con(arcpy.sa.IsNull(scalerRawR), supScalerR, scalerRawR)
    scalerR.save("temp_scaler.tif")
    del scalerRawR

    # from here to RESAMPLE, all rasters are in 1/8-dgr
    # (for raster calculator to function accurately, all input rasters must be of the same spatial resolution and align)
    coarseEndYrAmtR = arcpy.Raster(f'{inPath}/urban_fraction_{currSSP}_{endYr}.tif') * coarseLandAreaR
    arcpy.management.Resample(coarseEndYrAmtR, "temp_coarseEndYrAmt_1km.tif", "0.008333333333", "NEAREST")
    coarseEndYrAmtR = arcpy.Raster("temp_coarseEndYrAmt_1km.tif")
    coarseChgAmtR = coarseEndYrAmtR - coarseAmtR
    coarseChgAmtR = arcpy.sa.Con(coarseChgAmtR > 0, coarseChgAmtR, 0)
    coarseChgAmtR.save("temp_coarseChgAmt.tif")
    del coarseEndYrAmtR, coarseAmtR

    LoopAllocate = True
    i = 0

    while LoopAllocate:
        i = i+1
        
        fineChgAmtR = coarseChgAmtR * scalerR
        fineChgAmtR.save("temp_fineChgAmt_"+str(i)+".tif")
        
        fineAmtR = fineAmtR + fineChgAmtR
        fineAmtR.save("temp_fineAmt_"+str(i)+".tif")
        del fineChgAmtR
        
        fineOverflowPreR = fineAmtR - fineLandAreaR
        fineOverflowR = arcpy.sa.Con(fineOverflowPreR > 0, fineOverflowPreR, 0)
        fineOverflowR.save("temp_fineOverflow_"+str(i)+"_0.tif")
        del fineOverflowPreR
        
        fineAmtR = arcpy.sa.Con(fineOverflowR > 0, fineLandAreaR, fineAmtR)
        fineAmtR.save("temp_fineAmt_loop_"+str(i)+".tif")

        if fineOverflowR.maximum > 0.000005:
            coarseChgAmtR = arcpy.sa.ZonalStatistics(coarseZoneR, "VALUE", fineOverflowR, "Sum")
            coarseChgAmtR = arcpy.sa.SetNull(coarseChgAmtR, coarseChgAmtR, "VALUE<=0")
            coarseChgAmtR.save("temp_coarseChgAmt_"+str(i)+".tif")
            
            coarseZoneR = arcpy.sa.Con(arcpy.sa.IsNull(coarseChgAmtR), coarseChgAmtR, coarseZoneR)
            coarseZoneR = arcpy.sa.Int(coarseZoneR)
            coarseZoneR.save("temp_coarseZone_"+str(i)+".tif")
            fineOverflowR = arcpy.sa.Con(arcpy.sa.IsNull(coarseChgAmtR), coarseChgAmtR, fineOverflowR)
            fineOverflowR.save("temp_fineOverflow_"+str(i)+"_1.tif")
            
            flagR = arcpy.sa.Con(arcpy.sa.IsNull(coarseChgAmtR), coarseChgAmtR, flagR)
            flagR = arcpy.sa.Con((flagR==0) & (fineOverflowR>0), 1, flagR)
            flagR.save("temp_flag_"+str(i)+".tif")
            
            scalerR = arcpy.sa.Con(arcpy.sa.IsNull(coarseChgAmtR), coarseChgAmtR, scalerR)
            scalerR = arcpy.sa.Con(flagR > 0, 0, scalerR)
            scalerR.save("temp_scaler_"+str(i)+"_0.tif")
            coarseScalerSumR = arcpy.sa.ZonalStatistics(coarseZoneR, "VALUE", scalerR, "Sum")
            coarseScalerSumR.save("temp_coarseScalerSum_"+str(i)+"_0.tif")
            scalerR = arcpy.sa.Con(coarseScalerSumR == 0, supScalerR, scalerR)
            scalerR = arcpy.sa.Con(flagR > 0, 0, scalerR)
            scalerR.save("temp_scaler_"+str(i)+"_1.tif")
            coarseScalerSumR = arcpy.sa.ZonalStatistics(coarseZoneR, "VALUE", scalerR, "Sum")
            coarseScalerSumR.save("temp_coarseScalerSum_"+str(i)+"_1.tif")

            scalerR = scalerR / coarseScalerSumR
            scalerR.save("temp_scaler_"+str(i)+".tif")

            del coarseScalerSumR, fineOverflowR
            
        else:
            LoopAllocate = False
            del coarseZoneR, scalerR, fineAmtR, flagR, coarseChgAmtR, fineOverflowR
            break

    rasterList = []
    for iterID in range(1, i+1, 1):
        rasterList.append("temp_fineAmt_loop_"+str(iterID)+".tif")

    fineAmtR = arcpy.sa.CellStatistics(rasterList, "MAXIMUM", "DATA")
    # arcpy.management.CopyRaster(fineAmtR, f'_1km_amt_{currSSP}_{endYr}.tif')

    fineFracR = fineAmtR / fineLandAreaR
    arcpy.management.CopyRaster(fineFracR, f'{outPath}/urban_fraction_1km_{currSSP}_{endYr}.tif')

    del rasterList, fineAmtR, fineFracR
