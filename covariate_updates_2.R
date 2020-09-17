#' Analysis tools for spatial model projection Covariates
setwd("C:\\Users\\mcgr323\\OneDrive - PNNL\\Documents\\GitHub\\select")
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

variable_arr <- c('newR', 'newD', 'newA')
size_arr <- c(3,5,7,9)

#' create dataframe of focal sd from variable arrangement in attribute table 
#' @param attribute_tble
#' @param variable_arr
#' @param size
#' @return output_mean
#' @importFrom zoo rollapply
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export 
focal_mean <- function(attribute_tble, variable_arr, size) {
  output_mean <- data.frame(matrix(NA, nrow = nrow(attribute_tble), ncol = length(variable_arr)))
  for (f in variable_arr){
    output_mean[f] <-rollapply(attribute_tble[,f],size, function(x) mean(x),na.pad = TRUE)
    colnames(output_mean[f]) <- paste0('mean',f,sep = "_")
  }
  return(output_mean)
}

#' create dataframe of focal sd from variable arrangement in attribute table 
#' @param attribute_tble
#' @param variable_arr
#' @param size
#' @return output_sd
#' @importFrom zoo rollapply
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export 
focal_sd <- function(attribute_tble, variable_arr, size) {
  output_sd <- data.frame(matrix(NA, nrow = nrow(attribute_tble), ncol = length(variable_arr)))
  for (f in variable_arr){
    output_sd[f] <-rollapply(attribute_tble[,f],size, function(x) sd(x),na.pad = TRUE)
    colnames(output_mean[f]) <- paste("sd",f,sep = "_")
  }
  return(output_sd)
}


#' derive standardized positions, iterating variable arrangement 
#' @param attribute_tble
#' @param output_mean
#' @param output_sd
#' @return output_std_pos
#' @importFrom
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
standarize_pos <- function(attribute_tble,output_mean,output_sd){
  output_std_pos <- data.frame(matrix(NA, nrow = nrow(attribute_tble), ncol = length(variable_arr)))
    for (f in variable_arr){
      output_std_pos[f] <- (attribute_tble[,f] - output_mean[f])/output_sd[f]
      names(output_std_pos[,f]) <- paste0("std_pos",f,sep = "_")
    }
  return(output_std_pos)
}

#' create dataframe of range from variable arrangement in attribute table 
#' @param attribute_tble
#' @param variable_arr
#' @param size
#' @return output_min
#' @importFrom zoo rollapply
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export 
focal_min <- function(attribute_tble, variable_arr, size) {
  output_min <- data.frame(matrix(NA, nrow = nrow(attribute_tble), ncol = length(variable_arr)))
  for (f in variable_arr){
    output_min[f] <-rollapply(attribute_tble[,f],size, function(x) min(x),na.pad = TRUE)
    colnames(output_min[f]) <- paste("min",f,sep = "_")
  }
  return(output_min)
}

#' create dataframe of range from variable arrangement in attribute table 
#' @param attribute_tble
#' @param variable_arr
#' @param size
#' @return output_max
#' @importFrom zoo rollapply
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export 
focal_max <- function(attribute_tble, variable_arr, size) {
  output_max <- data.frame(matrix(NA, nrow = nrow(attribute_tble), ncol = length(variable_arr)))
  for (f in variable_arr){
    output_max[f] <-rollapply(attribute_tble[,f],size, function(x) max(x),na.pad = TRUE)
    colnames(output_max[f]) <- paste("max",f,sep = "_")
  }
  return(output_max)
  
  #' derive slope range, iterating variable arrangement 
  #' @param attribute_tble
  #' @param output_mean
  #' @param output_sd
  #' @return output_std_pos
  #' @importFrom
  #' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
  #' @export
  slope_range <- function(attribute_tble,variable_arr,output_max,output_min){
    output_slp_rnge <- data.frame(matrix(NA, nrow = nrow(attribute_tble), ncol = length(variable_arr)))
    for (f in variable_arr){
      output_slp_rnge[f] <- (output_max[f])- output_min[f]
      names(output_slp_rnge[,f]) <- paste0("slp_rnge",f,sep = "_")
    }
    return(output_slp_rnge)
  }
  
}

#' combine attribute table, mean output, sd output and std pos output in one dataframe
#' @param attribute_tble
#' @param output_mean
#' @param output_sd
#' @param output_std_pos
#' @return focal_attribute_tble
#' @importFrom dplyr bind_cols
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export 
create_focal_tble <- function(attribute_tble, output_mean, output_sd, output_std_pos, output_min, output_max) {
  attr_tble <- attribute_tble[,1:5]
  mean_out  <- output_mean[,4:6]
  sd_out  <- output_sd[,4:6]
  std_pos_out  <- output_std_pos[,4:6]
  slp_rnge_out  <- output_slp_rnge[,4:6]
  focal_attribute_tble <- bind_cols(attr_tble, mean_out,sd_out,std_pos_out, slp_rnge_out)
   names(focal_attribute_tble)[3:length(focal_attribute_tble)] <- c(
                                          "new_R","new_D","new_A",
                                          "mean_newR", "mean_newD", "mean_newA",
                                          "sd_newR", "sd_newD", "sd_newA",
                                          "std_pos_newR", "std_pos_newD", "std_pos_newA",
                                          "slp_rnge_newR", "slp_rnge_newD", "slp_rnge_newA")
  return(focal_attribute_tble)
}

#' join the grid center layer to attribute feature file
#' @param centroid_shp
#' @param attribute_tble
#' @return  centroid_lyr
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
spatial_join <- function(centroid_shp, focal_attribute_tble) {
  centroid_lyr <- merge(centroid_shp,  focal_attribute_tble)
  return(centroid_lyr)
}

#' convert 1/8-dgr fishnet to centroids
#' @param centroid_lyr
#' @return centroids
#' @importFrom sf st_centroid
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
compute_centriod <- function(centroid_lyr){
    centroids <- st_centroid(centroid_lyr)
  return(centroids)
}

# extract raster values to centroids
#' @param raster_benchmark
#' @param centroid_lyr
#' @param variable_arr
#' @return raster_lyr
#' @importFrom raster rasterize
#' @author Casey R. McGrath (casey.mcgrath@pnnl.gov)
#' @export
extract_raster <- function(raster_benchmark, centroid_lyr){
    raster_lyr <- rasterize(centroid_lyr, raster_benchmark)
  return( raster_lyr)
}


