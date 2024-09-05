library(tidyr)
library(dplyr)

chl_by_haul <- readRDS("indices/WCBTS_chl_by_haul.rds")
chl_by_haul$variable <- "chl"
pp_by_haul <- readRDS("indices/WCBTS_pp_by_haul.rds")
pp_by_haul$variable <- "pp"
temp_by_haul <- readRDS("indices/WCBTS_temp_by_haul.rds")

temp_by_haul$depth <- round(temp_by_haul$depth)

# Pivot the data into a wide format
df <- temp_by_haul[,c("trawl_id","thetao","depth")]
temp_long <- df %>%
  pivot_wider(
    names_from = depth,        # Use the levels of 'group' to create new variable names
    names_prefix = "thetao_",  # Prefix the new variable names with 'thetao_'
    values_from = thetao       # The values to fill in the new columns come from 'thetao'
  ) %>%
  pivot_longer(
    cols = starts_with("thetao_"),  # Pivot these new thetao_* columns to long format
    names_to = "variable",          # The new column that will hold the variable names
    values_to = "value"             # The new column that will hold the values
  )

df <- temp_by_haul[,c("trawl_id","uo","depth")]
uo_long <- df %>%
  pivot_wider(
    names_from = depth,        # Use the levels of 'group' to create new variable names
    names_prefix = "uo_",  # Prefix the new variable names with 'thetao_'
    values_from = uo       # The values to fill in the new columns come from 'thetao'
  ) %>%
  pivot_longer(
    cols = starts_with("uo_"),  # Pivot these new thetao_* columns to long format
    names_to = "variable",          # The new column that will hold the variable names
    values_to = "value"             # The new column that will hold the values
  )


df <- temp_by_haul[,c("trawl_id","vo","depth")]
vo_long <- df %>%
  pivot_wider(
    names_from = depth,        # Use the levels of 'group' to create new variable names
    names_prefix = "vo_",  # Prefix the new variable names with 'thetao_'
    values_from = vo       # The values to fill in the new columns come from 'thetao'
  ) %>%
  pivot_longer(
    cols = starts_with("vo_"),  # Pivot these new thetao_* columns to long format
    names_to = "variable",          # The new column that will hold the variable names
    values_to = "value"             # The new column that will hold the values
  )

all_dat <- bind_rows(chl_by_haul, pp_by_haul, temp_long, uo_long, vo_long)

saveRDS(all_dat, "indices/WCBTS_data_by_haul.rds")