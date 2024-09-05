library(ncdf4) # to load ncdf files in a coherent format
library(tidync)
library(data.table)
library(tidyverse)
library(geosphere)
library(sf)
library(nwfscSurvey)

# workaround via https://github.com/pepijn-devries/CopernicusMarine/issues/42
library(reticulate)
install_python() 
virtualenv_create(envname = "thiscopernicus")
virtualenv_install("thiscopernicus", packages = c("copernicusmarine"))
use_virtualenv("thiscopernicus", required = TRUE)
py_install("copernicusmarine")

cm <- import("copernicusmarine")

#copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1M-m -x -126 -X -115 -y 32 -Y 50 -z 120. -Z 150. -v thetao -t 1994-01-01 -T 2024-08-20 -o ./copernicus-data -f glorys_data_for_hake_temperature.nc

haul <- nwfscSurvey::pull_haul(survey="NWFSC.Combo")
haul$month <- month(as.Date(haul$datetime_utc_iso))
haul$yday <- yday(as.Date(haul$datetime_utc_iso))
haul <- sdmTMB::add_utm_columns(haul, ll_names = c("longitude_dd","latitude_dd"))

grid <- readRDS("grids/cmems_mod_glo_phy_my_WCBTS.rds")
grid$XY <- paste(round(grid$X,3), round(grid$Y,3))

year_range <- sort(unique(haul$year))
dataset <- "cmems_mod_glo_phy_my_0.083deg_P1M-m"

depth_bins <- data.frame("depth_bin" = 1:4, 
                         "depth_min" = c(55, 109, 155, 222) - 5,
                         "depth_max" = c(55, 109, 155, 222) + 5)

depth_years <- expand.grid(depth_bin = 1:4, years = year_range)
depth_years <- dplyr::left_join(depth_years, depth_bins)
depth_years$dataset <- "cmems_mod_glo_phy_my_0.083deg_P1M-m"
depth_years$dataset[which(depth_years$years > 2021)] = "cmems_mod_glo_phy_myint_0.083deg_P1M-m"

for(ii in 1:nrow(depth_years)) {
  
  yrs <- depth_years$years[ii]
  
    result <- cm$subset(
      dataset_id = depth_years$dataset[ii],
      start_datetime=paste0(yrs,"-01-01T00:00:00"),
      end_datetime=paste0(yrs,"-12-31T23:59:59"),
      variables = list("thetao","mlotst","uo","vo","zos"),
      minimum_longitude = -126,
      maximum_longitude = -115,
      minimum_latitude = 32,
      maximum_latitude = 50,
      minimum_depth = depth_years$depth_min[ii],
      maximum_depth = depth_years$depth_max[ii],
      output_filename = "CMEMS_tmp.nc",
      force_download = TRUE,
    )
    # load the data into R
    temp <- tidync::tidync("CMEMS_tmp.nc") |>
      hyper_tibble( force = TRUE) |>
      drop_na() |>
      group_by(longitude,latitude,time)
    
  if(yrs == 2021) {
    result2 <- cm$subset(
      dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m",
      start_datetime=paste0(yrs,"-01-01T00:00:00"),
      end_datetime=paste0(yrs,"-12-31T23:59:59"),
      variables = list("thetao","mlotst","uo","vo","zos"),
      minimum_longitude = -126,
      maximum_longitude = -115,
      minimum_latitude = 32,
      maximum_latitude = 50,
      minimum_depth = depth_years$depth_min[ii],
      maximum_depth = depth_years$depth_max[ii],
      output_filename = "CMEMS_tmp2.nc",
      force_download = TRUE,
    )
    # load the data into R
    temp2 <- tidync::tidync("CMEMS_tmp2.nc") |>
      hyper_tibble( force = TRUE) |>
      drop_na() |>
      group_by(longitude,latitude,time)
    temp <- rbind(temp, temp2)
    file.remove("CMEMS_tmp2.nc")
  }

  #depths_to_save <- unique(temp$depth)[c(1,5,7,9)]# 55.76429 109.72930 155.85069 222.47520
  #temp <- dplyr::filter(temp, depth %in% depths_to_save)
  # add utm and convert to a unique id
  temp_locs <- dplyr::group_by(temp, latitude, longitude) %>%
    dplyr::summarise()
  temp_locs <- sdmTMB::add_utm_columns(temp_locs) # add XY
  temp_locs$XY <- paste(round(temp_locs$X,3), round(temp_locs$Y,3))
  temp_locs$in_grid <- 0
  temp_locs$in_grid[which(temp_locs$XY %in% grid$XY)] <- 1
  temp <- dplyr::left_join(temp, temp_locs) |> 
    dplyr::filter(in_grid==1)
  
  # add time
  temp$date <- as.Date(as.numeric(temp$time/24), origin = "1950-01-01")
  temp$year <- year(temp$date)
  temp$month <- month(temp$date)
  
  haul_year <- dplyr::filter(haul, year == yrs)
  haul_year$day <- day(as.Date(haul_year$datetime_utc_iso))
  # Find grid cell closest to this location
  haul_year <- haul_year %>%
    rowwise() %>%
    mutate(
      this_loc = which.min((X - temp$X)^2 + (Y - temp$Y)^2),
      month_d1 = abs(difftime(datetime_utc_iso, ymd(paste0(yrs,"-",month-1,"-15")), units = "days")),# dist to month prior
      month_d2 = abs(difftime(datetime_utc_iso, ymd(paste0(yrs,"-",month,"-15")), units = "days")),# dist to mid of current
      month_d3 = abs(difftime(datetime_utc_iso, ymd(paste0(yrs,"-",month+1,"-15")), units = "days")),# dist to mid of next
      w1 = as.numeric(ifelse(day < 15, 1, 0) * month_d1),
      w2 = 1 * as.numeric(month_d2),
      w3 = ifelse(day > 15, 1, 0) * as.numeric(month_d3),
      # Find locations in temp that match up to the location, and month targeted
      loc_d1 = which.min((X - temp$X)^2 + (Y - temp$Y)^2 + (temp$month - (month-1))^10),
      loc_d2 = which.min((X - temp$X)^2 + (Y - temp$Y)^2 + (temp$month - (month))^10),
      loc_d3 = which.min((X - temp$X)^2 + (Y - temp$Y)^2 + (temp$month - (month+1))^10),
      # Calculate weighted mean 
      thetao = weighted.mean(temp$thetao[c(loc_d1,loc_d2,loc_d3)], w = c(w1, w2, w3)),
      uo = weighted.mean(temp$uo[c(loc_d1,loc_d2,loc_d3)], w = c(w1, w2, w3)),
      vo = weighted.mean(temp$vo[c(loc_d1,loc_d2,loc_d3)], w = c(w1, w2, w3))
    )
  
  haul_year$depth <- temp$depth[1] # add depth for variables
  if(ii==1) {
    all_dat <- haul_year[,c("trawl_id","thetao", "uo", "vo", "depth")]
  } else {
    all_dat <- rbind(all_dat, haul_year[,c("trawl_id","thetao", "uo", "vo", "depth")])
  }
  # clean up
  file.remove("CMEMS_tmp.nc")
}

saveRDS(all_dat, "indices/WCBTS_temp_by_haul.rds")
