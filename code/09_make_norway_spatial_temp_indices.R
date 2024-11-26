# Taken from my west coast USA work over here: https://github.com/ericward-noaa/glorys-grids/tree/main
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

grid <- readRDS("grids/cmems_mod_glo_phy_my_norway.rds")
grid$XY <- paste(grid$X, grid$Y)

current_year <- format(Sys.Date(), "%Y")

year_range <- sort(c(2003:2023, 2021))
dataset <- c(rep("cmems_mod_glo_phy_my_0.083deg_P1M-m",19),
             rep("cmems_mod_glo_phy_myint_0.083deg_P1M-m",3))

for(ii in 1:length(year_range)) {

  yrs <- year_range[ii]

  result <- cm$subset(
    dataset_id = dataset[ii],
    start_datetime=paste0(yrs,"-01-01T00:00:00"),
    end_datetime=paste0(yrs,"-12-31T23:59:59"),
    variables = list("thetao"),
    minimum_longitude = 3,
    maximum_longitude = 32,
    minimum_latitude = 62,
    maximum_latitude = 72,
    minimum_depth = 140,
    maximum_depth = 160,
    output_filename = "CMEMS_tmp.nc",
    force_download = TRUE,
  )

  temp <- tidync::tidync("CMEMS_tmp.nc") |>
    hyper_tibble( force = TRUE) |>
    drop_na() |>
    group_by(longitude,latitude,time)

  # add utm and convert to a unique id
  temp <- sdmTMB::add_utm_columns(temp)
  temp$XY <- paste(temp$X, temp$Y)
  # Filter out cells in the WCBTS grid
  temp <- dplyr::filter(temp, XY %in% grid$XY)
  # add time
  temp$date <- as.Date(as.numeric(temp$time/24), origin = "1950-01-01")
  temp$year <- year(temp$date)
  temp$month <- month(temp$date)

  if(yrs == min(year_range)) {
    all_dat <- temp
  } else {
    all_dat <- rbind(all_dat, temp)
  }
  # clean up
  file.remove("CMEMS_tmp.nc")
}

avg_temps_by_loc <- dplyr::group_by(all_dat, X, Y, month) |>
  dplyr::summarise(mean_thetao = mean(thetao), longitude = longitude[1], latitude = latitude[1],
                   XY = XY[1])
saveRDS(avg_temps_by_loc, "avg_temps_by_loc.rds")

library(broom)
results <- all_dat %>%
  group_by(X, Y, month) %>%
  summarize(
    slope = coef(lm(thetao ~ year))[["year"]],
    p_value = summary(lm(thetao ~ year))$coefficients["year", "Pr(>|t|)"],
    .groups = "drop"
  )
saveRDS(results, "avg_slope_by_loc.rds")

ggplot(avg_temps_by_loc, aes(X,Y, col = mean_thetao)) +
  geom_point(size = 0.1) + 
  #scale_color_gradient2() + 
  facet_wrap(~month)

ggplot(results, aes(X,Y, col = slope)) +
  geom_tile(size = 0.1) + 
  scale_color_gradient2() + 
  facet_wrap(~month)

