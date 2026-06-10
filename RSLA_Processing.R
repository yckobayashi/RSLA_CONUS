library(StreamLight)
library(arrow)
library(data.table)
library(this.path)

###############################################
#Point to directory which contains driver files
###############################################

drivers_dir <- here("drivers/")
drivers_list <- list.files(path = drivers_dir, pattern = "\\.parquet$", full.names = TRUE)

###############################################
# Create a function which reads in the driver file, joins the parameters for each river segment ID, then saves the output as a parquet.
###############################################

par_streamlight <- function(driver){
  sl_driver <- data.table(read_parquet(driver))
  sl_param <- data.table(read_parquet("data/conus_param.parquet"))
  sl_pred <- sl_driver[sl_param, on = .(ID)][, stream_light(.SD, overhang = overhang[1],
                                                            overhang_height = overhang_height[1],
                                                            Lat = Lat[1],
                                                            Lon = Lon[1],
                                                            channel_azimuth = channel_azimuth[1],
                                                            bottom_width = bottom_width[1],
                                                            BS = 100,
                                                            BH = 0.1,
                                                            TH = TH[1],
                                                            x_LAD = 1,
                                                            WL = WL[1]), by = ID]
  write_parquet(sl_pred, paste("results/predicted/",gsub("driver", "predicted", basename(driver)), sep = ""), )
  rm(list = c("sl_driver", "sl_param", "sl_pred"))
  gc()
}

for (file in drivers_list) {
  par_streamlight(file)
}
