#' Analysis tools for spatial model projection Covariates

#set working directory
workspace_path <- "C:\\Users\\mcgr323\\OneDrive - PNNL\\Documents\\GitHub\\select\\LandMask_1-8-degree_DATA\\"

#load files
library(sf)
library(raster)
library(sp)
library(rgdal)
attribute_tble <- read.csv(file.path(workspace_path,"tbl_inputToArcGIS_SSP5_2010.csv"))
centroid_shp <- st_read(file.path(workspace_path,"LandMask_1-8-degree_fishnet_centroids.shp"))
raster_benchmark <- raster(file.path(workspace_path,"LandMask_1-8-degree_raster.img"))

variable_arr <- c("newR", "newD", "newA")
size_arr <- c(3, 5, 7, 9)

#' join the grid center layer to attriute feature file
#' @param centroid_shp
#' @param attribute_tble
#' @return  centroid_attr
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
spatial_join <- function(centroid_shp, attribute_tble) {
  centroid_lyr <- merge(centroid_shp,  attribute_tble)
  return(centroid_lyr)
}

#' Convert attributes to rasters, using iterations
#'
#' @param centroid_attr
#' @param raster_benchmark
#' @param variable_arr
#' @return raster_output
#' @importFrom raster rasterize writeRaster
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
lyr_to_raser <- function(centroid_lyr, raster_benchmark, variable_arr) {
  for (f in seq_along(variable_arr)){
      output_raster <- rasterize(centroid_lyr[variable_arr[f]], raster_benchmark, fun=mean,
                                 filename = paste(variable_arr[f],"_output_raster",sep=""))
      return(output_rast)
  }
}

#' derive focal means & STDs, iterating through the rasters
#' @param variable_arr
#' @param size_arr
#' @return
#' @importFrom raster rasterize focal
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
mean_std_Ras <- function(size_arr, output_raster, variable_arr){
  for (f in seq_along(variable_arr)){
    for (size in size_arr){
      meanRas <- focal(output_raster[f], w= matrix(1/(size^2),nrow=size, ncol=size), fun =mean,
                       filename = paste("new_raster_mean"+str(size)+variable_arr[f],sep=""))
      stdRas <- focal(output_raster[f], w= matrix(1/(size^2),nrow=size, ncol=size), fun =st,
                      filename = paste("new_raster_std"+str(size)+variable_arr[f],sep=""))
    }
  }
}

#' derive standardized positions, iterating through the rasters
#' @param variable_arr
#' @param size_arr
#' @param centroid_lyr
#' @return outRas
#' @importFrom
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
standarize_pos <- function(variable_arr, size_arr,centroid_lyr){
  for (f in seq_along(variable_arr)){
    inRas = rasterize(centroid_lyr[f])
    for (size in size_arr){
      meanRas <- paste("new_raster_mean"+str(size)+variable_arr[f],sep="")
      stdRas <-  paste("new_raster_std"+str(size)+variable_arr[f],sep="")
      outRas <- (variable_arr[i]+"stps"+str(size))
      if (IsNull(inRas)){
        ((inRas-meanRas)/stdRas)
      outRas.save(outFile)
      }
    }
  }
}

#' derive slope range, iterating through the rasters
#' @param variable_arr
#' @param size_arr
#' @param centroid_lyr
#' @return outRas
#' @importFrom raster rasterize
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
raster_slope <- function(variable_arr, size_arr, centroid_lyr){
  for (f in seq_along(variable_arr)){
    inRas = rasterize(centroid_lyr[f])
      slopeRas <- slope(inRas)
  for (size in size_arr){
        outRas <- focal(inRas, w= matrix(1/(size^2),nrow=size, ncol=size), fun =range,
                         filename = (centroid_lyr[f]+"outRas"+str(size)))
        }
  }
}

#' convert 1/8-dgr fishnet to centroids
#' @param centriod_shp
#' @param variable_arr
#' @return centriods
#' @importFrom sf st_centroid
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
compute_centriod <- function(centriod_shp, variable_arr){
  for (f in seq_along(variable_arr)){
    centriods <- st_centriod(centriod_shp[f])
  }
  return(centriods)
}

# extract raster values to centroids
#' @param raster_benchmark
#' @return centriods
#' @importFrom raster exract
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
#' 
 centroids$chmMaxShape <- raster::extract(chm, centShape, weights=FALSE, fun=max)

