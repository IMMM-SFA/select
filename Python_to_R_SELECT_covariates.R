#' Analysis tools for spatial model projection Covariates
setwd("C:\\Users\\mcgr323\\projects\\select\\LandMask_1-8-degree_DATA")
workspace_path = "C:\\Users\\mcgr323\\projects\\select\\LandMask_1-8-degree_DATA"
#load files
library(sf)
library(raster)
library(sp)
library(rgdal)
library(rio)
attribute_tble = import("tbl_inputToArcGIS_SSP5_2010.csv")
centroid_shp = st_read(file.path(workspace_path,"LandMask_1-8-degree_fishnet_centroids.shp"))
raster_benchmark = raster(file.path(workspace_path,"LandMask_1-8-degree_raster.img"))

variable_arr <- c('newR', 'newD', 'newA')
size_arr <- c(3,5,7,9)


#' join the grid center layer to attribute feature file
#' @param centroid_shp
#' @param attribute_tble
#' @return  centroid_lyr
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
spatial_join <- function(centroid_shp,attribute_tble) {
  centroid_lyr <- merge(centroid_shp, attribute_tble)
  return(centroid_lyr)
}

#' create dataframe of focal sd from variable arrangement in attribute table 
#' @param attribute_tble
#' @param variable_arr
#' @param size
#' @return focal_data
#' @importFrom zoo rollapply
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export 
focal_stats = function(attribute_tble, variable_arr, size) {
  #create empty dataframes 
  output_mean = data.frame(matrix(NA, nrow = nrow(attribute_tble), ncol = length(variable_arr)))
  output_sd = data.frame(matrix(NA, nrow = nrow(attribute_tble), ncol = length(variable_arr)))
  output_std_pos = data.frame(matrix(NA, nrow = nrow(attribute_tble), ncol = length(variable_arr)))
  output_rnge = data.frame(matrix(NA, nrow = nrow(attribute_tble), ncol = length(variable_arr)))
  #populate dataframes with mean, sd, std pos and range for each variable in attribute table  
  for (f in variable_arr){
      output_mean[f] =     rollapply(attribute_tble[,f],size, function(x) mean(x),fill = NA)
      output_sd[f] =       rollapply(attribute_tble[,f],size, function(x) sd(x),fill = NA)
      output_std_pos[f] =  (attribute_tble[,f] - output_mean[f])/output_sd[f]
      output_rnge[f] =     ((rollapply(attribute_tble[,f],size, function(x) max(x),fill = NA))
                           -(rollapply(attribute_tble[,f],size, function(x) min(x),fill = NA)))
  }
  #bind the focal stat data together and nme columns
  focal_data = bind_cols(output_mean[,4:6],output_sd[,4:6],
                        output_std_pos[,4:6],output_rnge[,4:6])
  names(focal_data) = c(
    "mean_newR", "mean_newD", "mean_newA",
    "sd_newR", "sd_newD", "sd_newA",
    "std_pos_newR", "std_pos_newD", "std_pos_newA",
    "rnge_newR", "rnge_newD", "rnge_newA")
  return(focal_data)
}

# create rasters from the centroid layer
#' @param raster_benchmark
#' @param centroid_lyr
#' @param variable_arr
#' @return raster_lyr
#' @importFrom raster rasterize
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
combine_df <- function(cent_lyr, focal_3, focal_5, focal_7, focal_9){
 names(focal_3) = c(
   "newR_mean3", "newD_mean3", "newA_mean3",
   "newR_std3", "newD_std3", "newA_std3",
   "newR_stps3", "newD_stps3", "newA_stps3",
   "newR_slrg3", "newD_slrg3", "newA_slrg3")
 names(focal_5) = c(
   "newR_mean5", "newD_mean5", "newA_mean5",
   "newR_std5", "newD_std5", "newA_std5",
   "newR_stps5", "newD_stps5", "newA_stps5",
   "newR_slrg5", "newD_slrg5", "newA_slrg5")
 names(focal_7) = c(
   "newR_mean7", "newD_mean7", "newA_mean7",
   "newR_std7", "newD_std7", "newA_std7",
   "newR_stps7", "newD_stps7", "newA_stps7",
   "newR_slrg7", "newD_slrg7", "newA_slrg7")
 names(focal_9) = c(
   "newR_mean9", "newD_mean9", "newA_mean9",
   "newR_std9", "newD_std9", "newA_std9",
   "newR_stps9", "newD_stps9", "newA_stps9",
   "newR_slrg9", "newD_slrg9", "newA_slrg9")
 focal_widths = cbind(focal_3, focal_5, focal_7, focal_9)
 focal_data = cbind(cent_lyr,focal_widths)
  return( raster_lyr)
}

#' # create rasters from the centroid layer for slope
#' #' @param raster_benchmark
#' #' @param centroid_lyr
#' #' @param variable_arr
#' #' @return raster_lyr
#' #' @importFrom raster rasterize
#' #' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' #' @export
#' raster <- function(centroid_lyr){
#'   raster_lyr=list()
#'   for (i in names(centroid_lyr)){
#'     raster_lyr[i] = raster(centroid_lyr[i])
#'   }
#'   return( raster_lyr)
#' }



