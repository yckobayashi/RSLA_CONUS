library(sf)
library(ggmap)
library(tidyverse)
library(ggspatial)
library(data.table)
library(arrow)
library(relaimpo)
library(this.path)
library(ggnewscale)

theme_set(theme_classic())
par_dataset <- open_dataset(here("data/pred_par/", partitioning = "month"))
par_param <- data.table(read_parquet(here("data/conus_param.parquet")))
par_char <- data.table(st_read(here("data/geodatabase/NHD_Lines.gdb/"), layer = "ORIG_NHD_LINES", 
                               query = "SELECT ID, StreamOrd, BF_WIDTH, BF_DEPTH, 
                                        Azimuth, REACHCODE, PROVINCE, Shape FROM ORIG_NHD_LINES"))
df_shapes <- data.table(dplyr::select(par_char, c("ID", "Shape")))
par_char$Shape <- st_zm(par_char$Shape)

## Monthly Mean PAR

par_mean <- par_dataset %>% dplyr::select(StreamOrd, ID, month, PAR_surface) %>% group_by(ID, month) %>% summarise(mean_par = mean(PAR_surface, na.rm = TRUE)) %>% collect() %>% data.table()
par_mean <- par_mean[par_char, on = .(ID)]
par_mean <- par_mean[, stream_class := fifelse(StreamOrd == 1 | StreamOrd == 2 | StreamOrd == 3, "small",
                                               fifelse(StreamOrd == 4 | StreamOrd == 5 | StreamOrd == 6, "medium",
                                                       fifelse(StreamOrd == 7 | StreamOrd == 8 | StreamOrd == 9 | StreamOrd == 10, "large",
                                                               "NA")))]
par_mean <- par_mean[, season := fifelse(month == 12 | month == 1 | month == 2, "Winter",
                                         fifelse(month == 3 | month == 4 | month == 5, "Spring",
                                                 fifelse(month == 6 | month == 7 | month == 8, "Summer",                                              
                                                         fifelse(month == 9 | month == 10 | month == 11, "Fall",
                                                                 "NA"))))]
par_mean_summary <-par_mean[, as.list(summary(mean_par)), by = list(month, stream_class)][order(month)]
par_mean_summary[, variability := `3rd Qu.` - `1st Qu.`]
par_mean_summary[, norm_variability := variability/Median]

## Figure 5 - Plotting monthly mean PAR

ggplot() +
  geom_boxplot(data = par_mean, aes(x = factor(month), y = mean_par, fill = factor(stream_class, level = c("small", "medium", "large")), color = factor(stream_class, level = c("small", "medium", "large"))), width=.85, position = position_dodge(width = 0.85), outlier.shape = NA, coef = 0, linewidth = 0.3, color = "white") +
  scale_fill_manual(values = c("#E69F00","#0072B2", "#CC79A7"), labels = c(small = "Small Rivers", medium = "Medium Rivers", large = "Large Rivers"), name = "") +
  scale_x_discrete(
    breaks = seq_along(substr(month.abb, 1, 1)), 
    labels = substr(month.abb, 1, 1)) +
  new_scale_color() +
  geom_errorbar(data = par_mean_summary, linewidth = 0.75, width = 0, position=position_dodge(0.85), aes(x = factor(month), ymin = `3rd Qu.` - ((`3rd Qu.`-Median)/2), ymax = `Max.`, color = factor(stream_class, level = c("small", "medium", "large"))), show.legend = FALSE) +
  geom_errorbar(data = par_mean_summary, linewidth = 0.75, width = 0, position = position_dodge(0.85), aes(x = factor(month), ymin = Min., ymax = `1st Qu.` + ((Median - `1st Qu.`)/2), color = factor(stream_class, level = c("small", "medium", "large"))), show.legend = FALSE) +
  scale_color_manual(values = c("#E69F00E1", "#0072B2E1", "#CC79A7E1"), labels = c(small = "Small Rivers", medium = "Medium Rivers", large = "Large Rivers"), name = "") +
  labs(
    title = "",
    x = "",
    y = expression("PAR (" * mu * "mol" ~ m^-2 ~ s^-1 * ")"),
  ) +
  ylim(0,800) +
  theme_classic(base_size = 17) +
  theme(legend.position = c(0.85, 0.9), legend.text=element_text(size=10))

## Hourly Mean PAR

hourly_summary <- list()
time_list <- sprintf("%02d", 0:23)
for(i in 1:24){
  hour_summary <- par_dataset %>% dplyr::select(StreamOrd, ID, month, UTC_hour, PAR_surface, season, offset) %>% dplyr::filter(UTC_hour == paste(" ", time_list[i], sep = "")) %>% dplyr::group_by(season, UTC_hour, ID) %>% dplyr::summarise(mean_par = mean(PAR_surface, na.rm = TRUE)) %>% collect() %>% data.table()
  
  hour_summary <- hour_summary[par_char, on = .(ID)]
  
  hour_summary[, stream_class := fifelse(StreamOrd == 1 | StreamOrd == 2 | StreamOrd == 3, "small",
                                         fifelse(StreamOrd == 4 | StreamOrd == 5 | StreamOrd == 6, "medium",
                                                 fifelse(StreamOrd == 7 | StreamOrd == 8 | StreamOrd == 9 | StreamOrd == 10, "large",
                                                         "NA")))]
  
  hourly_par_mean_summary <- hour_summary[, as.list(summary(mean_par)), by = list(season, stream_class, UTC_hour)][order(season)]
  
  hourly_summary[[i]] <- hourly_par_mean_summary
}

hourly_mean_par <- rbindlist(hourly_summary)
hourly_mean_par$stream_class <- factor(hourly_mean_par$stream_class, levels = c("small", "medium", "large"))
hourly_mean_par$season <- factor(hourly_mean_par$season, levels = c("spring", "summer", "fall", "winter"))
hourly_mean_par[, variability := `3rd Qu.` - `1st Qu.`]

small_hourly_mean_par <- hourly_mean_par[stream_class == "small"]
small_hourly_mean_par[season != "winter", CST_time := as.numeric(UTC_hour) - 5]
small_hourly_mean_par[season == "winter", CST_time := as.numeric(UTC_hour) - 6]
small_hourly_mean_par[CST_time < 1, CST_time := CST_time + 24]
setnames(small_hourly_mean_par, c("Min.", "Max.", "Median", "1st Qu.", "3rd Qu."), c("min", "max", "median", "lowIQR", "highIQR"))
small_hourly_mean_par$season <- factor(small_hourly_mean_par$season, levels = c("spring", "summer", "fall", "winter"))

medium_hourly_mean_par <- hourly_mean_par[stream_class == "medium"]
medium_hourly_mean_par[season != "winter", CST_time := as.numeric(UTC_hour) - 5]
medium_hourly_mean_par[season == "winter", CST_time := as.numeric(UTC_hour) - 6]
medium_hourly_mean_par[CST_time < 1, CST_time := CST_time + 24]
setnames(medium_hourly_mean_par, c("Min.", "Max.", "Median", "1st Qu.", "3rd Qu."), c("min", "max", "median", "lowIQR", "highIQR"))
medium_hourly_mean_par$season <- factor(medium_hourly_mean_par$season, levels = c("spring", "summer", "fall", "winter"))

large_hourly_mean_par <- hourly_mean_par[stream_class == "large"]
large_hourly_mean_par[season != "winter", CST_time := as.numeric(UTC_hour) - 5]
large_hourly_mean_par[season == "winter", CST_time := as.numeric(UTC_hour) - 6]
large_hourly_mean_par[CST_time < 1, CST_time := CST_time + 24]
setnames(large_hourly_mean_par, c("Min.", "Max.", "Median", "1st Qu.", "3rd Qu."), c("min", "max", "median", "lowIQR", "highIQR"))
large_hourly_mean_par$season <- factor(large_hourly_mean_par$season, levels = c("spring", "summer", "fall", "winter"))

## Figure 6 - Plotting the hourly mean PAR for small, medium, and large rivers

# Boxplot for Small Rivers
ggplot(data = small_hourly_mean_par, aes(x = CST_time, y = median, group = interaction(CST_time, season), fill = season)) +
  geom_boxplot(aes(
    lower = lowIQR,
    upper = highIQR,
    middle = median,
    ymin = min,
    ymax = max
  ), stat = "identity", width=.85, position = position_dodge(width = 0.85), outlier.shape = NA, linewidth = 0.1, color = "white")  +
  scale_fill_manual(values = c("darkgreen", "#E69F00", "#915e04", "#0072F9"), labels = c(spring = "Spring", summer = "Summer", fall = "Fall", winter = "Winter"), name = element_blank()) +
  scale_x_continuous(
    breaks = c(6,12,18,24),
    labels = c("6","12","18","24"),
    minor_breaks = seq(4, 24, 1),
    limits = c(4, 23),
    expand = c(0,0)) +
  guides(
    x = guide_axis(minor.ticks = TRUE)
  ) +
  new_scale_color() +
  geom_errorbar(data = small_hourly_mean_par, linewidth = 0.25, width = 0, position=position_dodge(0.85), aes(ymin = highIQR - ((highIQR - median)/2), ymax = max, color = season), show.legend = FALSE) +
  geom_errorbar(data = small_hourly_mean_par, linewidth = 0.25, width = 0, position = position_dodge(0.85), aes(ymin = min, ymax = lowIQR + ((median  - lowIQR)/2), color = season), show.legend = FALSE) +
  scale_color_manual(values = c("darkgreen", "#E69F00", "#915e04", "#0072F9"), labels = c(spring = "Spring", summer = "Summer", fall = "Fall", winter = "Winter"), name = element_blank()) +
  scale_y_continuous(
    breaks = c(0, 500, 1000, 1500, 2000),
    limits = c(0, 2200)
  ) +
  theme_classic(base_size = 18) +
  theme(legend.position = c(0.85, 0.9), 
        legend.text=element_text(size=10), 
        aspect.ratio = 5.5/10, 
        axis.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.margin=unit(c(0,0,0,0), "pt"))

# Boxplot for Medium Rivers
ggplot(data = medium_hourly_mean_par, aes(x = CST_time, y = median, group = interaction(CST_time, season), fill = season)) +
  geom_boxplot(aes(
    lower = lowIQR,
    upper = highIQR,
    middle = median,
    ymin = min,
    ymax = max
  ), stat = "identity", width=.85, position = position_dodge(width = 0.85), outlier.shape = NA, linewidth = 0.1, color = "white")  +
  scale_fill_manual(values = c("darkgreen", "#E69F00", "#915e04", "#0072F9"), labels = c(spring = "Spring", summer = "Summer", fall = "Fall", winter = "Winter"), name = element_blank()) +
  scale_x_continuous(
    breaks = c(6,12,18,24),
    labels = c("6","12","18","24"),
    minor_breaks = seq(4, 24, 1),
    limits = c(4, 23),
    expand = c(0,0)) +
  guides(
    x = guide_axis(minor.ticks = TRUE)
  ) +
  new_scale_color() +
  geom_errorbar(data = medium_hourly_mean_par, linewidth = 0.25, width = 0, position=position_dodge(0.85), aes(ymin = highIQR - ((highIQR - median)/2), ymax = max, color = season), show.legend = FALSE) +
  geom_errorbar(data = medium_hourly_mean_par, linewidth = 0.25, width = 0, position = position_dodge(0.85), aes(ymin = min, ymax = lowIQR + ((median  - lowIQR)/2), color = season), show.legend = FALSE) +
  scale_color_manual(values = c("darkgreen", "#E69F00", "#915e04", "#0072F9"), labels = c(spring = "Spring", summer = "Summer", fall = "Fall", winter = "Winter"), name = element_blank()) +
  scale_y_continuous(
    breaks = c(0, 500, 1000, 1500, 2000),
    limits = c(0, 2200)
  ) +
  theme_classic(base_size = 18) +
  theme(legend.position = c(0.85, 0.9), 
        legend.text=element_text(size=10), 
        aspect.ratio = 5.5/10, 
        axis.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.margin=unit(c(0,0,0,0), "pt"))

# Boxplot for Large Rivers
ggplot(data = large_hourly_mean_par, aes(x = CST_time, y = median, group = interaction(CST_time, season), fill = season)) +
  geom_boxplot(aes(
    lower = lowIQR,
    upper = highIQR,
    middle = median,
    ymin = min,
    ymax = max
  ), stat = "identity", width=.85, position = position_dodge(width = 0.85), outlier.shape = NA, linewidth = 0.1, color = "white")  +
  scale_fill_manual(values = c("darkgreen", "#E69F00", "#915e04", "#0072F9"), labels = c(spring = "Spring", summer = "Summer", fall = "Fall", winter = "Winter"), name = element_blank()) +
  scale_x_continuous(
    breaks = c(6,12,18,24),
    labels = c("6","12","18","24"),
    minor_breaks = seq(4, 24, 1),
    limits = c(4, 23),
    expand = c(0,0)) +
  guides(
    x = guide_axis(minor.ticks = TRUE)
  ) +
  new_scale_color() +
  geom_errorbar(data = large_hourly_mean_par, linewidth = 0.25, width = 0, position=position_dodge(0.85), aes(ymin = highIQR - ((highIQR - median)/2), ymax = max, color = season), show.legend = FALSE) +
  geom_errorbar(data = large_hourly_mean_par, linewidth = 0.25, width = 0, position = position_dodge(0.85), aes(ymin = min, ymax = lowIQR + ((median  - lowIQR)/2), color = season), show.legend = FALSE) +
  scale_color_manual(values = c("darkgreen", "#E69F00", "#915e04", "#0072F9"), labels = c(spring = "Spring", summer = "Summer", fall = "Fall", winter = "Winter"), name = element_blank()) +
  scale_y_continuous(
    breaks = c(0, 500, 1000, 1500, 2000),
    limits = c(0, 2200)
  ) +
  theme_classic(base_size = 18) +
  theme(legend.position = c(0.85, 0.9), 
        legend.text=element_text(size=10), 
        aspect.ratio = 5.5/10, 
        axis.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.margin=unit(c(0,0,0,0), "pt"))

## Land Cover Analysis
## Using the NLCD 2019 raster, reclassify the pixels for Pasture and Cultivated Crops (81, 82) to Cultivated (80),
## keep deciduous (41), evergreen (42), shrub (52), and grassland (71) and reclassify other classes as misc (99)

## From the reclassified raster, apply a zonal extraction
nlcd_raster <- rast(here("data/nlcd_raster.tif"))
nlcd_classes <- data.frame(class = c(41,42,52,71,80,99), landcov = c("deciduous", "evergreen", "shrub", "grassland", "cultivated", "misc"))
levels(nlcd_raster) <- list(nlcd_classes)

nlcd_shp_df <- exact_extract(
  nlcd_raster, 
  st_as_sf(df_shapes), 
  'mode',
  append_cols = 'ID')
names(nlcd_shp_df)[1] <- "ID"
names(nlcd_shp_df)[2] <- "class"

nlcd <- merge(nlcd_shp_df, nlcd_classes, by = "class", all = TRUE)

## Join the NLCD parameters with the averaged PAR dataset.

nlcd[is.na(landcov), landcov := "misc"] 
par_mean <- par_dataset %>% dplyr::select(StreamOrd, ID, month, PAR_surface) %>% group_by(ID, month) %>% summarise(mean_par = mean(PAR_surface, na.rm = TRUE)) %>% collect() %>% data.table()
par_mean <- par_mean[par_char, on = .(ID)]
par_mean <- par_mean[, stream_class := fifelse(StreamOrd == 1 | StreamOrd == 2 | StreamOrd == 3, "small",
                                               fifelse(StreamOrd == 4 | StreamOrd == 5 | StreamOrd == 6, "medium",
                                                       fifelse(StreamOrd == 7 | StreamOrd == 8 | StreamOrd == 9 | StreamOrd == 10, "large",
                                                               "NA")))]
par_mean <- par_mean[, season := fifelse(month == 12 | month == 1 | month == 2, "Winter",
                                         fifelse(month == 3 | month == 4 | month == 5, "Spring",
                                                 fifelse(month == 6 | month == 7 | month == 8, "Summer",                                              
                                                         fifelse(month == 9 | month == 10 | month == 11, "Fall",
                                                                 "NA"))))]
par_mean <- par_mean[nlcd, on = .(ID)]
par_mean_split <- split(par_mean, par_mean$landcov)
par_mean_nlcd_summary <- par_mean[, as.list(summary(mean_par)), by = list(landcov, month, stream_class)][order(landcov, month)]
par_mean_nlcd_summary_subset <- par_mean_nlcd_summary[landcov %in% c("shrub", "grassland", "cultivated", "deciduous", "evergreen", "misc")]

## Figure 7 - Plotting the monthly mean PAR for each land cover class.

lapply(names(par_mean_split), function(i){
  nlcd_par_mean <- data.table(par_mean_split[[i]])
  par_mean_summary <- nlcd_par_mean[, as.list(summary(mean_par)), by = list(month, stream_class)][order(month)]
  mm_par_plot <- ggplot() +
    geom_boxplot(data = par_mean_split[[i]], aes(x = factor(month), y = mean_par, fill = factor(stream_class, level = c("small", "medium", "large")), color = factor(stream_class, level = c("small", "medium", "large"))), width=.85, position = position_dodge(width = 0.85), outlier.shape = NA, coef = 0, linewidth = 0.3, color = "white") +
    scale_fill_manual(values = c("#E69F00","#0072B2", "#CC79A7"), labels = c(small = "Small Rivers", medium = "Medium Rivers", large = "Large Rivers"), name = "") +
    scale_x_discrete(
      breaks = seq_along(substr(month.abb, 1, 1)), 
      labels = substr(month.abb, 1, 1)) +
    new_scale_color() +
    geom_errorbar(data = par_mean_summary, linewidth = 0.75, width = 0, position=position_dodge(0.85), aes(x = factor(month), ymin = `3rd Qu.` - ((`3rd Qu.`-Median)/2), ymax = `Max.`, color = factor(stream_class, level = c("small", "medium", "large"))), show.legend = FALSE) +
    geom_errorbar(data = par_mean_summary, linewidth = 0.75, width = 0, position = position_dodge(0.85), aes(x = factor(month), ymin = Min., ymax = `1st Qu.` + ((Median - `1st Qu.`)/2), color = factor(stream_class, level = c("small", "medium", "large"))), show.legend = FALSE) +
    scale_color_manual(values = c("#E69F00E1", "#0072B2E1", "#CC79A7E1"), labels = c(small = "Small Rivers", medium = "Medium Rivers", large = "Large Rivers"), name = "") +
    labs(
      title = "",
      x = "",
      y = expression("PAR (" * mu * "mol" ~ m^-2 ~ s^-1 * ")"),
    ) +
    ylim(0, 800) +
    theme_classic(base_size = 17) +
    theme(legend.position = c(0.85, 0.9), legend.text=element_text(size=10))
})

## Relative Importance analysis
## Prepare the data by filtering and adjusting the azimuth.

rivers_list <- as.data.frame(list(1:7712125 ))
names(rivers_list)[names(rivers_list) == 'X1.7712125'] <- 'ID'
set.seed(12345)
sampled_rivers <- rivers_list %>% sample_n(800000)

spring_par_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "spring", SW_inc > 0) %>% group_by(ID) %>% summarise(med_PAR = median(PAR_surface, na.rm = TRUE)) %>% collect() %>% data.table()
spring_lai_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "spring", SW_inc > 0) %>% group_by(ID) %>% summarise(med_lai = median(LAI, na.rm = TRUE)) %>% collect() %>% data.table()
spring_sw_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "spring", SW_inc > 0) %>% group_by(ID) %>% summarise(med_sw = median(SW_inc, na.rm = TRUE)) %>% collect() %>% data.table()

summer_par_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "summer", SW_inc > 0) %>% group_by(ID) %>% summarise(med_PAR = median(PAR_surface, na.rm = TRUE)) %>% collect() %>% data.table()
summer_lai_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "summer", SW_inc > 0) %>% group_by(ID) %>% summarise(med_lai = median(LAI, na.rm = TRUE)) %>% collect() %>% data.table()
summer_sw_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "summer", SW_inc > 0) %>% group_by(ID) %>% summarise(med_sw = median(SW_inc, na.rm = TRUE)) %>% collect() %>% data.table()

fall_par_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "fall", SW_inc > 0) %>% group_by(ID) %>% summarise(med_PAR = median(PAR_surface, na.rm = TRUE)) %>% collect() %>% data.table()
fall_lai_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "fall", SW_inc > 0) %>% group_by(ID) %>% summarise(med_lai = median(LAI, na.rm = TRUE)) %>% collect() %>% data.table()
fall_sw_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "fall", SW_inc > 0) %>% group_by(ID) %>% summarise(med_sw = median(SW_inc, na.rm = TRUE)) %>% collect() %>% data.table()

winter_par_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "winter", SW_inc > 0) %>% group_by(ID) %>% summarise(med_PAR = median(PAR_surface, na.rm = TRUE)) %>% collect() %>% data.table()
winter_lai_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "winter", SW_inc > 0) %>% group_by(ID) %>% summarise(med_lai = median(LAI, na.rm = TRUE)) %>% collect() %>% data.table()
winter_sw_med <- par_dataset %>% filter(ID %in% sampled_rivers$ID, season == "winter", SW_inc > 0) %>% group_by(ID) %>% summarise(med_sw = median(SW_inc, na.rm = TRUE)) %>% collect() %>% data.table()

spring_par_med <- spring_par_med[spring_lai_med, on = .(ID)][spring_sw_med, on = .(ID)]
summer_par_med <- summer_par_med[summer_lai_med, on = .(ID)][summer_sw_med, on = .(ID)]
fall_par_med <- fall_par_med[fall_lai_med, on = .(ID)][fall_sw_med, on = .(ID)]
winter_par_med <- winter_par_med[winter_lai_med, on = .(ID)][winter_sw_med, on = .(ID)]

spring_med <- spring_par_med[par_param %>% dplyr::filter(ID %in% sampled_rivers$ID), on = .(ID)][par_char, on = .(ID)]
summer_med <- summer_par_med[par_param %>% dplyr::filter(ID %in% sampled_rivers$ID), on = .(ID)][par_char, on = .(ID)]
fall_med <- fall_par_med[par_param %>% dplyr::filter(ID %in% sampled_rivers$ID), on = .(ID)][par_char, on = .(ID)]
winter_med <- winter_par_med[par_param %>% dplyr::filter(ID %in% sampled_rivers$ID), on = .(ID)][par_char, on = .(ID)]

spring_med[channel_azimuth > 180, channel_azimuth := channel_azimuth - 180]
summer_med[channel_azimuth > 180, channel_azimuth := channel_azimuth - 180]
fall_med[channel_azimuth > 180, channel_azimuth := channel_azimuth - 180]
winter_med[channel_azimuth > 180, channel_azimuth := channel_azimuth - 180]

spring_med[channel_azimuth > 90, channel_azimuth := 90 - (channel_azimuth - 90)]
summer_med[channel_azimuth > 90, channel_azimuth := 90 - (channel_azimuth - 90)]
fall_med[channel_azimuth > 90, channel_azimuth := 90 - (channel_azimuth - 90)]
winter_med[channel_azimuth > 90, channel_azimuth := 90 - (channel_azimuth - 90)]

spring_med[, stream_class := fifelse(StreamOrd == 1 | StreamOrd == 2 | StreamOrd == 3, "small",
                                   fifelse(StreamOrd == 4 | StreamOrd == 5 | StreamOrd == 6, "medium",
                                           fifelse(StreamOrd == 7 | StreamOrd == 8 | StreamOrd == 9 | StreamOrd == 10, "large",
                                                   "NA")))]
summer_med[, stream_class := fifelse(StreamOrd == 1 | StreamOrd == 2 | StreamOrd == 3, "small",
                                   fifelse(StreamOrd == 4 | StreamOrd == 5 | StreamOrd == 6, "medium",
                                           fifelse(StreamOrd == 7 | StreamOrd == 8 | StreamOrd == 9 | StreamOrd == 10, "large",
                                                   "NA")))]
fall_med[, stream_class := fifelse(StreamOrd == 1 | StreamOrd == 2 | StreamOrd == 3, "small",
                                   fifelse(StreamOrd == 4 | StreamOrd == 5 | StreamOrd == 6, "medium",
                                           fifelse(StreamOrd == 7 | StreamOrd == 8 | StreamOrd == 9 | StreamOrd == 10, "large",
                                                   "NA")))]
winter_med[, stream_class := fifelse(StreamOrd == 1 | StreamOrd == 2 | StreamOrd == 3, "small",
                                   fifelse(StreamOrd == 4 | StreamOrd == 5 | StreamOrd == 6, "medium",
                                           fifelse(StreamOrd == 7 | StreamOrd == 8 | StreamOrd == 9 | StreamOrd == 10, "large",
                                                   "NA")))]

## Apply a multiple linear regression before using as input in averaging over orderings.

s_spring_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(spring_med, stream_class == "small"))
s_summer_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(summer_med, stream_class == "small"))
s_fall_driver <-lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(fall_med, stream_class == "small"))
s_winter_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(winter_med, stream_class == "small"))

m_spring_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(spring_med, stream_class == "medium"))
m_summer_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(summer_med, stream_class == "medium"))
m_fall_driver <-lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(fall_med, stream_class == "medium"))
m_winter_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(winter_med, stream_class == "medium"))

l_spring_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(spring_med, stream_class == "large"))
l_summer_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(summer_med, stream_class == "large"))
l_fall_driver <-lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(fall_med, stream_class == "large"))
l_winter_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = filter(winter_med, stream_class == "large"))

spring_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = spring_med)
summer_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = summer_med)
fall_driver <-lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = fall_med)
winter_driver <- lm(med_PAR~med_lai+med_sw+Lat+Lon+channel_azimuth+bottom_width+TH, data = winter_med)

s_spring_aoo <- boot.relimp(s_spring_driver, b = 1000, rela = TRUE)
s_summer_aoo <- boot.relimp(s_summer_driver, b = 1000, rela = TRUE)
s_fall_aoo <- boot.relimp(s_fall_driver, b = 1000, rela = TRUE)
s_winter_aoo <- boot.relimp(s_winter_driver, b = 1000, rela = TRUE)

m_spring_aoo <- boot.relimp(m_spring_driver, b = 1000, rela = TRUE)
m_summer_aoo <- boot.relimp(m_summer_driver, b = 1000, rela = TRUE)
m_fall_aoo <- boot.relimp(m_fall_driver, b = 1000, rela = TRUE)
m_winter_aoo <- boot.relimp(m_winter_driver, b = 1000, rela = TRUE)

l_spring_aoo <- boot.relimp(l_spring_driver, b = 1000, rela = TRUE)
l_summer_aoo <- boot.relimp(l_summer_driver, b = 1000, rela = TRUE)
l_fall_aoo <- boot.relimp(l_fall_driver, b = 1000, rela = TRUE)
l_winter_aoo <- boot.relimp(l_winter_driver, b = 1000, rela = TRUE)

spring_aoo <- boot.relimp(spring_driver, b = 1000, rela = TRUE)
summer_aoo <- boot.relimp(summer_driver, b = 1000, rela = TRUE)
fall_aoo <- boot.relimp(fall_driver, b = 1000, rela = TRUE)
winter_aoo <- boot.relimp(winter_driver, b = 1000, rela = TRUE)

s_spring_aoo_res <- booteval.relimp(s_spring_aoo, norank = T)
s_summer_aoo_res <- booteval.relimp(s_summer_aoo, norank = T)
s_fall_aoo_res <- booteval.relimp(s_fall_aoo, norank = T)
s_winter_aoo_res <- booteval.relimp(s_winter_aoo, norank = T)

m_spring_aoo_res <- booteval.relimp(m_spring_aoo, norank = T)
m_summer_aoo_res <- booteval.relimp(m_summer_aoo, norank = T)
m_fall_aoo_res <- booteval.relimp(m_fall_aoo, norank = T)
m_winter_aoo_res <- booteval.relimp(m_winter_aoo, norank = T)

l_spring_aoo_res <- booteval.relimp(l_spring_aoo, norank = T)
l_summer_aoo_res <- booteval.relimp(l_summer_aoo, norank = T)
l_fall_aoo_res <- booteval.relimp(l_fall_aoo, norank = T)
l_winter_aoo_res <- booteval.relimp(l_winter_aoo, norank = T)

spring_aoo_res <- booteval.relimp(spring_aoo, norank = T)
summer_aoo_res <- booteval.relimp(summer_aoo, norank = T)
fall_aoo_res <- booteval.relimp(fall_aoo, norank = T)
winter_aoo_res <- booteval.relimp(winter_aoo, norank = T)

## Formatting results of averaging over ordering

s_spring <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = s_spring_aoo_res$lmg)
s_summer <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = s_summer_aoo_res$lmg)
s_fall <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = s_fall_aoo_res$lmg)
s_winter <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = s_winter_aoo_res$lmg)
s_season <- bind_rows(s_spring, s_summer, s_fall, s_winter, .id = "id") %>% rename(season = id)
s_season$season <-  replace(s_season$season, s_season$season == 1, "spring")
s_season$season <-  replace(s_season$season, s_season$season == 2, "summer")
s_season$season <-  replace(s_season$season, s_season$season == 3, "fall")
s_season$season <-  replace(s_season$season, s_season$season == 4, "winter")

m_spring <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = m_spring_aoo_res$lmg)
m_summer <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = m_summer_aoo_res$lmg)
m_fall <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = m_fall_aoo_res$lmg)
m_winter <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = m_winter_aoo_res$lmg)
m_season <- bind_rows(m_spring, m_summer, m_fall, m_winter, .id = "id") %>% rename(season = id)
m_season$season <-  replace(m_season$season, m_season$season == 1, "spring")
m_season$season <-  replace(m_season$season, m_season$season == 2, "summer")
m_season$season <-  replace(m_season$season, m_season$season == 3, "fall")
m_season$season <-  replace(m_season$season, m_season$season == 4, "winter")

l_spring <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = l_spring_aoo_res$lmg)
l_summer <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = l_summer_aoo_res$lmg)
l_fall <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = l_fall_aoo_res$lmg)
l_winter <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = l_winter_aoo_res$lmg)
l_season <- bind_rows(l_spring, l_summer, l_fall, l_winter, .id = "id") %>% rename(season = id)
l_season$season <-  replace(l_season$season, l_season$season == 1, "spring")
l_season$season <-  replace(l_season$season, l_season$season == 2, "summer")
l_season$season <-  replace(l_season$season, l_season$season == 3, "fall")
l_season$season <-  replace(l_season$season, l_season$season == 4, "winter")

spring <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = spring_aoo_res$lmg)
summer <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = summer_aoo_res$lmg)
fall <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = fall_aoo_res$lmg)
winter <- data.frame(variable = c("Leaf Area Index", "Downward Shortwave Radiation Flux", "Latitude", "Longitude", "River Azimuth", "River Width", "Tree Height"), metric = winter_aoo_res$lmg)
season <- bind_rows(spring, summer, fall, winter, .id = "id") %>% rename(season = id)
season$season <-  replace(season$season, season$season == 1, "spring")
season$season <-  replace(season$season, season$season == 2, "summer")
season$season <-  replace(season$season, season$season == 3, "fall")
season$season <-  replace(season$season, season$season == 4, "winter")

## Figure 8 - Plotting results of averaging over ordering to show the relative importance of drivers and parameters for PAR

s_season$season <- factor(s_season$season, levels = c("spring", "summer", "fall", "winter"))
s_season$variable <- factor(s_season$variable, levels = c("Latitude", "Longitude", "Downward Shortwave Radiation Flux", "Leaf Area Index", "Tree Height", "River Azimuth", "River Width"))
ggplot(data = s_season, aes(fill = variable, y = metric, x = season)) +
  geom_bar(position = "fill", stat = "identity", width = 0.8) +
  theme(aspect.ratio = 2/1) +
  scale_fill_manual(values = c( "darkred", "pink" , "yellow", "#1AC42C", "#A76819","#AB84F0","#0D3CEC"))

m_season$season <- factor(m_season$season, levels = c("spring", "summer", "fall", "winter"))
m_season$variable <- factor(m_season$variable, levels = c("Latitude", "Longitude", "Downward Shortwave Radiation Flux", "Leaf Area Index", "Tree Height", "River Azimuth", "River Width"))
ggplot(data = m_season, aes(fill = variable, y = metric, x = season)) +
  geom_bar(position = "fill", stat = "identity", width = 0.8) +
  theme(aspect.ratio = 2/1) +
  scale_fill_manual(values = c( "darkred", "pink" , "yellow", "#1AC42C", "#A76819","#AB84F0","#0D3CEC"))

l_season$season <- factor(l_season$season, levels = c("spring", "summer", "fall", "winter"))
l_season$variable <- factor(l_season$variable, levels = c("Latitude", "Longitude", "Downward Shortwave Radiation Flux", "Leaf Area Index", "Tree Height", "River Azimuth", "River Width"))
ggplot(data = l_season, aes(fill = variable, y = metric, x = season)) +
  geom_bar(position = "fill", stat = "identity", width = 0.8) +
  theme(aspect.ratio = 2/1) +
  scale_fill_manual(values = c( "darkred", "pink" , "yellow", "#1AC42C", "#A76819","#AB84F0","#0D3CEC"))

season$season <- factor(season$season, levels = c("spring", "summer", "fall", "winter"))
season$variable <- factor(season$variable, levels = c("Latitude", "Longitude", "Downward Shortwave Radiation Flux", "Leaf Area Index", "Tree Height", "River Azimuth", "River Width"))
ggplot(data = season, aes(fill = variable, y = metric, x = season)) +
  geom_bar(position = "fill", stat = "identity", width = 0.8) +
  theme(aspect.ratio = 2/1) +
  scale_fill_manual(values = c( "darkred", "pink" , "yellow", "#1AC42C", "#A76819","#AB84F0","#0D3CEC"))