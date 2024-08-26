
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

era <- 2
file_in <- c("glorys_data_for_hake_temperature.nc","glorys_data_for_hake_temperature2.nc")
df <- tidync::tidync(file_in[era]) |>
  hyper_tibble( force = TRUE) |>
  drop_na() |>
  group_by(longitude,latitude,time)

# bring in hake grid
hake <- readRDS("grid_pred.rds")

hake <- readRDS("hake_biomass_2022_02_04.rds")
hake <- sdmTMB::add_utm_columns(hake, ll_names = c("lon","lat"))

locs <- dplyr::group_by(df, latitude, longitude) |>
  dplyr::summarise(n = n())
locs <- sdmTMB::add_utm_columns(locs, ll_names = c("longitude","latitude"), utm_crs = sdmTMB::get_crs(hake, ll_names = c("lon","lat")))

########## This block defines an outer perimeter of the hake survey
lon_range <- dplyr::group_by(hake, floor(Y)) |>
  dplyr::summarise(min = min(floor(X)), max = max(floor(X)))
lat_range <- dplyr::group_by(hake, floor(X)) |>
  dplyr::summarise(min = min(floor(Y)), max = max(floor(Y)))
hake_filtered <- rbind(as.matrix(lat_range[,c(1,2)]), as.matrix(lat_range[,c(1,3)]), 
                           as.matrix(lon_range[,c(2,1)]), as.matrix(lon_range[,c(3,1)]))

####### This block defines an initial cut of the GLORYS cells
point1 <- c(-140, 57)
point2 <- c(-120, 32)

# Calculate the slope (m) and intercept (b) of the line y = mx + b
slope <- (point2[2] - point1[2]) / (point2[1] - point1[1])
intercept <- point1[2] - slope * point1[1]

# Define a function to check if a point is below the line
is_below_line <- function(x, y) {
  y_line <- slope * x + intercept
  return(y < y_line)
}
locs_filtered <- locs[!mapply(is_below_line, locs$longitude, locs$latitude), ]
# plot(locs_filtered$X, locs_filtered$Y)
# points(filtered_hake[,1], filtered_hake[,2], col="red")

hake_filtered <- as.data.frame(hake_filtered)
names(hake_filtered) <- c("X","Y")
locs_filtered <- as.data.frame(locs_filtered)
names(locs_filtered) <- c("lat","lon", "n","X","Y")
hake_locs <- rbind(hake_filtered[,1:2], locs_filtered[,4:5])
dist <- as.matrix(dist(hake_locs, diag=TRUE, upper = TRUE))
n_hake <- nrow(hake_filtered)
# cut out the hake rows of dist
dist <- dist[c(1:n_hake),-c(1:n_hake)]
min_dist <- apply(dist,2,min)

# filter points  < 10km
locs_filtered <- locs_filtered[which(min_dist < 10),]

saveRDS(dplyr::select(locs_filtered,-n),file = c("grid_cells_1993_2021.rds","grid_cells_2021_2024.rds")[era])
