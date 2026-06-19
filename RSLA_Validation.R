library(tidyverse)
library(lubridate)
library(neonUtilities)
library(neonOS)
library(Metrics)
library(sf)
library(arrow)
library(data.table)
library(this.path)

# Grabs data from NEON. The dpID points to the "PAR at water surface" and goes from start to end date.

zipsByProduct(dpID = "DP1.20042.001",
              startdate = "2019-01",
              enddate = "2020-01",
              check.size = FALSE,
              savepath = this.path::here("data/NEON/")
)
stackByTable(filepath = this.path::here("data/NEON/filesToStack20042"))

# Filter the NEON data to only select streams, within the year 2019, and within CONUS. Additional sites removed after manually inspecting.
pos_df <- data.frame(read.csv(here("data/NEON/filesToStack20042/stackedFiles/sensor_positions_20042.csv")))
pos_df <- pos_df %>% filter(!grepl('Lake|Pothole|Buoy', sensorLocationDescription))
pos_df <- pos_df %>% filter(ymd_hms(positionStartDateTime) < ymd("2019-02-01")) %>% filter(ymd_hms(positionEndDateTime) > ymd("2019-11-30") | positionEndDateTime == "")
pos_df <- pos_df %>% filter(locationReferenceLatitude > 25 & locationReferenceLatitude < 50)
pos_df <- pos_df %>% filter(!siteID %in% c("REDB", "POSE", "KING", "TECR"))

# Filter PAR data for quality flag, then aggregate into hour-per-month.
par_df <- data.frame(read.csv(here("data/NEON/filesToStack20042/stackedFiles/PARWS_30min.csv")))
par_df <- par_df %>% filter(PARFinalQF == 0)
par_df <- par_df %>% filter(str_detect(startDateTime, "2019")) %>% filter(siteID %in% unique(pos_df$siteID))
par_df <- par_df %>% group_by(siteID, startDateTime) %>% summarise(PARMean = mean(PARMean)) 
par_df <- par_df %>% mutate(month = month(startDateTime), hour = hour(ymd_hms(startDateTime)))
par_df <- par_df %>% group_by(siteID, month, hour) %>% summarise(PARMean = mean(PARMean, na.rm = TRUE))

# Shapefile produced in ArcGIS Pro by applying applying a spatial join from NHD points to our processed NHD drainage network.
neon_nhd <- data.table(st_read(here("data/NEON/NEON_NHD_VALIDATION.shp"), layer = "NHD_NEON_VAL", query = "SELECT ID, siteID FROM NEON_NHD_VALIDATION")) %>% st_drop_geometry()

# Read in the estimated PAR dataset and filter using the ID.
par_dataset <- open_dataset(here("data/pred_par/"), partitioning = "month")
pred_neon <- par_dataset %>% dplyr::filter(ID %in% neon_nhd$ID) %>% collect()
pred_neon <- neon_nhd[,.SD, .SDcols = c("ID", "siteID")][pred_neon, on = .(ID)]
pred_neon <- pred_neon[, c("ID", "siteID", "UTC_time", "PAR_surface")]
pred_neon[, hour := hour(UTC_time)]
pred_neon[, month := month(UTC_time)]

# Join the observed and estimated PAR datasets and rename columns.
neon_val <- pred_neon[par_df, on = .(siteID, month, hour)]
neon_val <- na.omit(neon_val)
neon_val <- neon_val %>% rename(PAR_obs = PARMean, PAR_pred = PAR_surface)


neon_val[,round(cor(PAR_pred, PAR_obs),2), by = "siteID"]
neon_val[,round(rmse(PAR_pred, PAR_obs)/mean(PAR_obs),2), by = "siteID"]
neon_val[,round(bias(PAR_pred, PAR_obs),2), by = "siteID"]

neon_val[,round(cor(PAR_pred, PAR_obs),2)]
neon_val[,round(rmse(PAR_pred, PAR_obs)/mean(PAR_obs),2)]
neon_val[,round(bias(PAR_pred, PAR_obs),2)]
