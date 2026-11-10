
## Process data on CTmin latitudinal gradients of Anolis carolinensis ##

## Packages ----

library(tidyverse)

## Process data ----

ctmin <- read.csv("raw_data/ctmin_gradient.csv")

# filter columns of interest
ctmin <- ctmin |>
  filter(sp == "caro") |>
  select(site, mass, Tb_CT) |>
  filter(site != "CHA") |>
  mutate(
    lat = case_when(
      site == "MIA" ~ 25.7617,
      site == "PSL" ~ 27.305,
      site == "ORL" ~ 28.5383,
      site == "GNV" ~ 29.6516,
      site == "TIF" ~ 31.463,
      site == "ATL" ~ 33.7490,
      site == "KNX" ~ 35.949
    ),
    lon = case_when(
      site == "MIA" ~ -80.1918,
      site == "PSL" ~ -80.332,
      site == "ORL" ~ -81.3792,
      site == "GNV" ~ -82.3248,
      site == "TIF" ~ -84.272,
      site == "ATL" ~ -84.3880,
      site == "KNX" ~ -83.933
    )
  ) |>
  arrange(lat) |>
  mutate(site = factor(site, levels = unique(site))) |>
  rename(ctmin = Tb_CT) |>
  select(site, lat, lon, mass, ctmin)

# save
saveRDS(ctmin, "data/ctmin_gradient.rds")