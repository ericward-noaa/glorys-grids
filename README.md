# glorys-grids

This project contains code to generate GLORYS indices and filter by specific survey grids for marine fish surveys

A few notes:
- the queries here are in Python via the `reticulate` package. This is a workaround to having to use the `copernicusmarine` package, but more stable

- the scripts are pretty specific to individual grids and variables. Things like the spatial resolution and reference dates change dataset to dataset unfortunately

- Most of the scripts here are relevant for fish surveys on the west coast of the USA, including the West Coast Groundfish Bottom Trawl Survey (WGBTS) or hake acoustic survey
