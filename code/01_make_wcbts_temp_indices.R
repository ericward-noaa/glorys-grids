library(ncdf4) 
library(tidync)
library(data.table)
library(tidyverse)
library(geosphere)
library(sf)

# workaround via https://github.com/pepijn-devries/CopernicusMarine/issues/42
library(reticulate)
install_python() 
virtualenv_create(envname = "thiscopernicus")
virtualenv_install("thiscopernicus", packages = c("copernicusmarine"))
use_virtualenv("thiscopernicus", required = TRUE)
py_install("copernicusmarine")

cm <- import("copernicusmarine")

#copernicusmarine subset -i cmems_mod_glo_phy_my_0.083deg_P1M-m -x -126 -X -115 -y 32 -Y 50 -z 120. -Z 150. -v thetao -t 1994-01-01 -T 2024-08-20 -o ./copernicus-data -f glorys_data_for_hake_temperature.nc

grid <- readRDS("cmems_mod_glo_phy_my_WCBTS.rds")
grid$XY <- paste(grid$X, grid$Y)

current_year <- format(Sys.Date(), "%Y")

year_range <- sort(c(1994:current_year, 2021))
dataset <- c(rep("cmems_mod_glo_phy_my_0.083deg_P1M-m",length(year_range)),
   rep("cmems_mod_glo_phy_myint_0.083deg_P1M-m",4))

for(ii in 1:length(year_range)) {
  
  yrs <- year_range[ii]
  
  result <- cm$subset(
    dataset_id = dataset[ii],
    start_datetime=paste0(yrs,"-01-01T00:00:00"),
    end_datetime=paste0(yrs,"-12-31T23:59:59"),
    variables = list("thetao","mlotst","uo","vo","zos"),
    minimum_longitude = -126,
    maximum_longitude = -115,
    minimum_latitude = 32,
    maximum_latitude = 50,
    minimum_depth = 50,
    maximum_depth = 250,
    output_filename = "CMEMS_tmp.nc",
    force_download = TRUE,
  )

  temp <- tidync::tidync("CMEMS_tmp.nc") |>
    hyper_tibble( force = TRUE) |>
    drop_na() |>
    group_by(longitude,latitude,time)
  depths_to_save <- unique(temp$depth)[c(1,5,7,9)]# 55.76429 109.72930 155.85069 222.47520
  temp <- dplyr::filter(temp, depth %in% depths_to_save)
  # add utm and convert to a unique id
  temp <- sdmTMB::add_utm_columns(temp) 
  temp$XY <- paste(temp$X, temp$Y)
  # Filter out cells in the WCBTS grid
  temp <- dplyr::filter(temp, XY %in% grid$XY)
  # add time
  temp$date <- as.Date(as.numeric(temp$time/24), origin = "1950-01-01")
  temp$year <- year(temp$date)
  temp$month <- month(temp$date)
  
  # coastwide means
  coastwide_means <- dplyr::group_by(temp, month, depth) |>
    dplyr::summarise(theta_o = mean(thetao, na.rm=T),
                     uo = mean(uo, na.rm=T),
                     vo = mean(vo, na.rm=T),
                     #mlotst = mean(mlotst, na.rm=T),
                     region="coastwide") |>
    dplyr::ungroup()
  
  # cape mendocino 40.4401° N, 124.4095° W
  coords_mend <- sdmTMB::add_utm_columns(data.frame(longitude = -124.4095, latitude = 40.4401))
  coords_concep <- sdmTMB::add_utm_columns(data.frame(longitude = -120.4716, latitude = 34.4486))
  
  north_means <- dplyr::filter(temp, Y > coords_mend$Y) |>
    dplyr::group_by(month, depth) |>
    dplyr::summarise(theta_o = mean(thetao, na.rm=T),
                     uo = mean(uo, na.rm=T),
                     vo = mean(vo, na.rm=T),
                     #mlotst = mean(mlotst, na.rm=T),
                     region="north") |>
    dplyr::ungroup() 
  central_means <- dplyr::filter(temp, Y < coords_mend$Y, Y > coords_concep$Y) |>
    dplyr::group_by(month, depth) |>
    dplyr::summarise(theta_o = mean(thetao, na.rm=T),
                     uo = mean(uo, na.rm=T),
                     vo = mean(vo, na.rm=T),
                     #mlotst = mean(mlotst, na.rm=T),
                     region="central") |>
    dplyr::ungroup() 
  south_means <- dplyr::filter(temp, Y < coords_concep$Y) |>
    dplyr::group_by(month, depth) |>
    dplyr::summarise(theta_o = mean(thetao, na.rm=T),
                     uo = mean(uo, na.rm=T),
                     vo = mean(vo, na.rm=T),
                     #mlotst = mean(mlotst, na.rm=T),
                     region="south") |>
    dplyr::ungroup() 
  
  all_means <- rbind(coastwide_means, north_means, central_means, south_means)
  all_means$year <- year(temp$date[1])
  
  if(yrs == min(year_range)) {
    all_dat <- all_means
  } else {
    all_dat <- rbind(all_dat, all_means)
  }
  # clean up
  file.remove("CMEMS_tmp.nc")
}

saveRDS(all_dat, "indices/temp_indices_WCBTS.rds")
