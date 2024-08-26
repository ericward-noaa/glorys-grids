library(surveyjoin)

grid <- surveyjoin::nwfsc_grid
grid <- dplyr::rename(grid, longitude = lon, latitude = lat)
range(grid$lat) # 32.00000 48.46667
range(grid$lon) # -125.9667 -117.2833

# now load in example GLORYS dataset:
# do different products use the same cells?

# This is temperature data 1994 - 2021
# copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1M-m -x -126 -X -115 -y 32 -Y 50 -z 120. -Z 150. -v thetao -t 1994-01-01 -T 2024-08-20 -o ./copernicus-data -f glorys_data_for_hake_temperature.nc
# This is temperature data 2021 - 2024
# copernicusmarine subset -i cmems_mod_glo_phy_myint_0.083deg_P1M-m -x -126 -X -115 -y 32 -Y 50 -z 120. -Z 150. -v thetao -t 1994-01-01 -T 2024-08-20 -o ./copernicus-data -f glorys_data_for_hake_temperature2.nc
# This is chlorophyll data
# copernicusmarine subset -i cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M -x -126 -X -115 -y 32 -Y 50 -z 120. -Z 150. -v CHL -t 1994-01-01 -T 2024-08-20 -o ./copernicus-data -f chlorophyll_wcbts.nc
# This is primary productivity data
# copernicusmarine subset -i cmems_obs-oc_glo_bgc-pp_my_l4-multi-4km_P1M -x -126 -X -115 -y 32 -Y 50 -z 120. -Z 150. -v PP -t 1994-01-01 -T 2024-08-20 -o ./copernicus-data -f pp_wcbts.nc

library(ncdf4) # to load ncdf files in a coherent format
library(tidync)
library(data.table)
library(tidyverse)
library(geosphere)
library(sf)

temp1 <- tidync::tidync(file.choose()) |>
  hyper_tibble( force = TRUE) |>
  drop_na() |>
  group_by(longitude,latitude,time)

# We can ignore the 2nd temp series, because grid doesn't change
# temp2 <- tidync::tidync(file.choose()) |>
#   hyper_tibble( force = TRUE) |>
#   drop_na() |>
#   group_by(longitude,latitude,time)

# Grid does differ with pp data
pp <- tidync::tidync(file.choose()) |>
  hyper_tibble( force = TRUE) |>
  drop_na() |>
  group_by(longitude,latitude,time)

chl <- tidync::tidync(file.choose()) |>
  hyper_tibble( force = TRUE) |>
  drop_na() |>
  group_by(longitude,latitude,time)
# 
locs_1 <- dplyr::group_by(temp1, longitude, latitude) |>
  dplyr::summarise()
locs_pp <- dplyr::group_by(pp, longitude, latitude) |>
  dplyr::summarise() 


find_points <- function(glorys_grid, survey_grid, km_thresh = 10) {
  # Convert both to UTM
  survey_crs <- sdmTMB::get_crs(survey_grid, ll_names = c("longitude","latitude"))
  df_glorys <- sdmTMB::add_utm_columns(glorys_grid, ll_names = c("longitude","latitude"), utm_crs = survey_crs)
  df_survey <- sdmTMB::add_utm_columns(survey_grid, ll_names = c("longitude","latitude"), utm_crs = survey_crs)
  
  # Convert dataframes to sf objects with a suitable projected CRS (e.g., UTM)
  df_glorys_sf <- st_as_sf(df_glorys, coords = c("X", "Y"), crs = survey_crs)
  df_survey_sf <- st_as_sf(df_survey, coords = c("X", "Y"), crs = survey_crs)

  df_survey_hull <- st_convex_hull(st_union(df_survey_sf))
  
  # create a buffer around the convex hull (e.g., 10 km buffer)
  df_survey_buffer <- st_buffer(df_survey_hull, dist = km_thresh)
  
  # Check which points in df_glorys_sf are within the buffer
  within_buffer <- st_within(df_glorys_sf, df_survey_buffer)
  within_buffer_logical <- lengths(within_buffer) > 0
  
  # Filter df_glorys_sf to keep only points within the buffer
  df_glorys_within_buffer <- df_glorys_sf[within_buffer_logical, ]
  
  # remove points outside the buffer:
  df_glorys_outside_buffer <- df_glorys_sf[!within_buffer_logical, ]
  
  # return df_glorys with points removed that are more than thresh km outside the polygon:
  df_glorys_filtered <- df_glorys_sf[within_buffer_logical, ]
  
  # result back to a regular dataframe
  df_glorys_filtered_df <- as.data.frame(st_coordinates(df_glorys_filtered))
  return(df_glorys_filtered_df)
}

filtered_temp_grid <- find_points(locs_1, grid)
saveRDS(filtered_temp_grid, file="cmems_mod_glo_phy_my_WCBTS.rds")
filtered_pp_grid <- find_points(pp, grid)
saveRDS(filtered_pp_grid, file="cmems_obs-oc_glo_bgc_WCBTS.rds")