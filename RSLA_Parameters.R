library(tidyverse)
library(arrow)
library(lutz)
library(data.table)
library(terra)
library(exactextractr)
library(raster)
library(sf)
library(matrixStats)
library(this.path)



###############################################
# run this before any processing - loads the lai raster stack, then converts into rast format
lai_data <- get(load(here("data/MODIS/LAI_4Days_500m_v61/Time_Series/RData/Terra/Lai/MCD15A3H_Lai_1_2019_1_2020_RData.RData")))
lai_conv <- terra::rast(lai_data[[1:92]])
# run this before any processing - loads the dswrf netcdf file, then rotates it
dswrf_2019 <- rotate(rast(here("data/year_2019_dswrf.nc")))
# run this before any processing - loads the chm raster
chm_rast <- rast(here("data/CONUS_CHM.tif"))
# run this before any processing - gets list of features in geodatabase



###############################################
# Preparing the preprocessed NHDPlus polylines. Read in the NHD shapefile and create a data.table using only the ID and shape columns.
# From the centroid coordinates, use the lutz package to assign a timezone. To account for any rivers that had been assigned a width of zero, we estimate 
# the median river width within the Strahler stream order for the HUC8 and apply that value to the river widths with a value of zero. We then create
# a buffered polygon using the river width plus an additional 20 meters to capture the riparian zone. 
###############################################

gdb_shapefile <- st_read(here("data/NHD_Lines.gdb/"), layer = "ORIG_NHD_LINES")
df_shapes <- data.table(dplyr::select(gdb_shapefile, c("ID", "Shape")))
gdb_shapefile$timezone <- tz_lookup_coords(as.numeric(st_drop_geometry(gdb_shapefile$Lat)), as.numeric(st_drop_geometry(gdb_shapefile$Lon)))
gdb_shapefile$Shape <- st_zm(gdb_shapefile$Shape)
gdb_shapefile <- data.table(st_drop_geometry(gdb_shapefile))
gdb_shapefile$REACHCODE <- str_sub(gdb_shapefile$REACHCODE, end = -7)
setnames(gdb_shapefile, "REACHCODE", "HUC8")
gdb_shapefile[,huc8_width := median(BF_WIDTH), by = c("StreamOrd", "HUC8")][, .SD, .SDcols = c("ID", "HUC8", "StreamOrd", "huc8_width")]
gdb_shapefile[,huc8_width := median(BF_WIDTH), by = c("StreamOrd", "HUC8")]
gdb_shapefile[BF_WIDTH == 0, BF_WIDTH := huc8_width]
df_shapes <- df_shapes[gdb_shapefile[, .SD, .SDcols = c("ID","BF_WIDTH")], on = .(ID)]
df_shapes[,BuffDist := (BF_WIDTH/2) + 20]

###############################################
# From the buffered polygon, the exactextractr package is used to apply zonal statistics from the raster data (LAI, DSWRF, CHM). If NA is calculated when
# applying the zonal statistic, a value of zero is assigned to the pixel. The table is saved in a parquet format to reduce the file size.
###############################################

df_shapes <- st_as_sf(df_shapes)
buffer_shp <- st_zm(df_shapes) %>% st_geometry() %>% st_transform("EPSG:3857") %>% st_buffer(df_shapes$BuffDist, endCapStyle = "FLAT")
lai <- exact_extract(lai_conv, buffer_shp, 'mean')
lai[is.na(lai)] <- 0.00001
lai_df <- data.frame(January = rowMedians(as.matrix(lai[, 1:8])),
                  February = rowMedians(as.matrix(lai[, 9:15])),
                  March = rowMedians(as.matrix(lai[, 16:23])),
                  April = rowMedians(as.matrix(lai[, 24:30])),
                  May = rowMedians(as.matrix(lai[, 31:38])),
                  June = rowMedians(as.matrix(lai[, 39:46])),
                  July = rowMedians(as.matrix(lai[, 47:53])),
                  August = rowMedians(as.matrix(lai[, 54:61])),
                  September = rowMedians(as.matrix(lai[, 62:69])),
                  October = rowMedians(as.matrix(lai[, 70:76])),
                  November = rowMedians(as.matrix(lai[, 77:84])),
                  December = rowMedians(as.matrix(lai[, 85:92])))
write_parquet(lai_df, here("data/lai.parquet"))
dswrf <- exact_extract(dswrf_2019, st_transform(buffer_shp, "EPSG:4326"), 'mean')
dswrf[is.na(dswrf)] <- 0
write_parquet(dswrf, here("data/dswrf.parquet"))
chm <- exact_extract(chm_rast, buffer_shp, 'median')
chm[is.na(chm)] <- 0
rm(buffer_shp)
chm_dt <- data.table(chm)
write_parquet(chm_dt, here("data/chm.parquet"))

#lai <- read_parquet(here("data/lai.parquet"))
#dswrf <- read_parquet(here("data/dswrf.parquet"))
#chm <- read_parquet(here("data/chm.parquet"))

###############################################
# Create a table of parameters for all the features of the preprocessed NHD Plus shapefile. In addition, include the tree height from the CHM table.
# The parameters are the static values which remain constant throughout all timesteps and assumes rivers are constantly at bankfull. 
# Bank height (BH), bank slope (BS), overhanging vegetation (overhang), and height of overhanging vegetation (overhang_height) used the suggested 
# values by the developer of StreamLight.
###############################################


conus_param <- data.table(
  ID = gdb_shapefile$ID,
  Lat = as.numeric(gdb_shapefile$Lat), 
  Lon = as.numeric(gdb_shapefile$Lon),
  channel_azimuth = as.numeric(gdb_shapefile$Azimuth), 
  bottom_width = as.numeric(gdb_shapefile$BF_WIDTH), 
  BH = 0.1,
  BS = 100, 
  WL = as.numeric(gdb_shapefile$BF_DEPTH), 
  TH = as.numeric(unlist(chm)), 
  overhang = as.numeric(as.numeric(unlist(chm)) * .1),
  overhang_height = as.numeric(as.numeric(unlist(chm)) * .75), 
  x_LAD = 1
)
write_parquet(conus_param, here("data/conus_param.parquet"))

###############################################

lai_dt <- as.data.table(rowMedians(as.matrix(lai[, 9:15])))
names(lai_dt) <- "LAI"
lai_dt$ID <- gdb_shapefile$ID
lai_dt$timezone <- gdb_shapefile$timezone
###############################################

for(i in 1:12){


lai_dt <- as.data.table(dplyr::select(lai_df, month.name[i]))


names(lai_dt) <- "LAI"
lai_dt$ID <- gdb_shapefile$ID
lai_dt$timezone <- gdb_shapefile$timezone
lai_dt[, LAI := as.numeric((LAI*0.1) + 0.01)]


time_field <- paste("2019-", sprintf("%02d", i), "-15", sprintf(" %02d", 0:23), ":00:00", sep = "")


tz_lookup <- data.frame(t(as.data.frame(sapply(unique(lai_dt$timezone), tz_offset, dt = time_field[1]))))
tz_lookup <- dplyr::select(tz_lookup, tz_name, utc_offset_h)
tz_lookup <- data.frame(matrix(unlist(tz_lookup), ncol = 2)) %>% rename(timezone = X1, offset = X2)

lai_dt <- lai_dt[tz_lookup, on = .(timezone)]


dswrf_dt <- as.data.table(dswrf[, 24*(i-1)+c(1:24)])


names(dswrf_dt) <- time_field
dswrf_dt$ID <- gdb_shapefile$ID
dswrf_dt <- melt(dswrf_dt, measure.vars = time_field, variable.name = "time", value.name = "SW_inc")
dswrf_dt[, SW_inc := SW_inc/3600] 
driver <- dswrf_dt[lai_dt, on = .(ID)]
rm(lai_dt)
rm(dswrf_dt)
gc()


driver[,c('jday[,1]') := as.numeric(paste("2019", strftime(as.POSIXct(paste("2019-", sprintf("%02d", i), "-15", sep = "")), format = "%j"), sep = ""))]
driver[,c('DOY') := as.numeric(strftime(as.POSIXct(paste("2019-", sprintf("%02d", i), "-15", sep = "")), format = "%j"), sep = "")]


driver[,c('Year') := as.numeric(2019)]
driver[, UTC_time:=time_field, by = ID]
driver[, Hour:=c(rep((24+as.numeric(.SD$offset)[1]):24), rep(1:(24+as.numeric(.SD$offset)[1]-1))), by=ID]


driver$local_time <- as.POSIXct(paste("2019-", sprintf("%02d", i), "-15 ", driver$Hour, ":00:00", sep = ""), format="%Y-%m-%d %H")


driver <- setcolorder(driver, c("ID", "local_time", "offset", 'jday[,1]', "DOY", "Year", "Hour", "SW_inc", "LAI"))
driver[, time := NULL][, timezone := NULL]


write_parquet(driver, here(paste("drivers/driver_", sprintf("%02d", i), ".parquet", sep = "")))


}

###############################################
#
###############################################
