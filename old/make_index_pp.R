
# These are the calls to use copernicusmarine (Python) to query data
# dataset coordinates [1993-01-01 00:00:00, 2021-06-01 00:00:00]
# copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1M-m -x -140 -X -105 -y 32 -Y 65 -z 120. -Z 150. -v thetao -t 1994-01-01 -T 2024-08-20 -o ./copernicus-data -f glorys_data_for_hake_temperature.nc
# # [2021-07-01 00:00:00, 2024-03-01 00:00:00]
# copernicusmarine subset -i cmems_mod_glo_phy_myint_0.083deg_P1M-m -x -140 -X -105 -y 32 -Y 65 -z 120. -Z 150. -v thetao -t 1994-01-01 -T 2024-08-20 -o ./copernicus-data -f glorys_data_for_hake_temperature2.nc


library(ncdf4) # to load ncdf files in a coherent format
library(tidync)
library(data.table)
library(tidyverse)
library(geosphere)

era <- 1
file_in <- c("glorys_data_for_hake_temperature.nc","glorys_data_for_hake_temperature2.nc")
df <- tidync::tidync(file_in[era]) |>
  hyper_tibble( force = TRUE) |>
  drop_na() |>
  group_by(longitude,latitude,time)

locs <- readRDS(c("grid_cells_1993_2021.rds","grid_cells_2021_2024.rds")[era])
locs$unique_latlon <- paste(locs$lon, locs$lat)

df <- dplyr::mutate(df, unique_latlon = paste(longitude, latitude)) |>
  dplyr::filter(unique_latlon %in% locs$unique_latlon)

# Take mean across space for each time slice
df <- dplyr::group_by(df, time) %>%
  dplyr::summarise(mean = mean(thetao, na.rm=T))

#hours since 1950-01-01
df$date <- as.Date(as.numeric(df$time/24), origin = "1950-01-01")
df$year <- year(df$date)
df$month <- month(df$date)

# final re-arranging
df_1 <- df[,c("year","month","mean")] %>%
  dplyr::rename(mean_thetao = mean)




era <- 2
file_in <- c("glorys_data_for_hake_temperature.nc","glorys_data_for_hake_temperature2.nc")
df <- tidync::tidync(file_in[era]) |>
  hyper_tibble( force = TRUE) |>
  drop_na() |>
  group_by(longitude,latitude,time)

locs <- readRDS(c("grid_cells_1993_2021.rds","grid_cells_2021_2024.rds")[era])
locs$unique_latlon <- paste(locs$lon, locs$lat)

df <- dplyr::mutate(df, unique_latlon = paste(longitude, latitude)) |>
  dplyr::filter(unique_latlon %in% locs$unique_latlon)

# Take mean across space for each time slice
df <- dplyr::group_by(df, time) %>%
  dplyr::summarise(mean = mean(thetao, na.rm=T))

#hours since 1950-01-01
df$date <- as.Date(as.numeric(df$time/24), origin = "1950-01-01")
df$year <- year(df$date)
df$month <- month(df$date)

# final re-arranging
df_2 <- df[,c("year","month","mean")] %>%
  dplyr::rename(mean_thetao = mean)

df_joined <- rbind(df_1, df_2)
saveRDS(df_joined, "temperature_index.rds")
