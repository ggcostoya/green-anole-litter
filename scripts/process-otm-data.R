## Process OTM Data ##

## Packages ----

library(tidyverse)

## Read and Process Raw OTM Data ----

files_list <- list.files(
  path = "raw_data/otms_raw_data", full.names = TRUE, recursive = TRUE
)

# extract file info as everything after the last /
file_info <- str_extract(files_list, "(?<=/)[^/]+$")

# add leading 0 to iButton numbers that are only 1 digit long
file_info <- str_replace(file_info, "(?<=_)(\\d{1})(?=_)", "0\\1")

# extract locations
location <- str_extract(file_info, "^[^_]+")

# extract iButton numbers
otm_number <- as.numeric(str_extract(file_info, "(?<=_)[^_]+"))

# skip all files for which otm_number is NA
files_list <- files_list[!is.na(otm_number)]
location <- location[!is.na(otm_number)]
otm_number <- otm_number[!is.na(otm_number)]

# initialize list of files to store data frames
data_list <- vector("list", length(files_list))

# read in each file and store in the list
for (i in seq_along(files_list)) {

  file <- read.csv(files_list[i], skip = 14, row.names = NULL) |>
    mutate(location = location[i], id = otm_number[i]) |>
    rename(
      date = row.names,
      time = Date.Time,
      temp = Value
    ) |>
    as_tibble() |>
    mutate(
      year = year(as.Date(date, format = "%m/%d/%Y")),
      doy = yday(as.Date(date, format = "%m/%d/%Y")),
      time_dttm = as.POSIXct(time, format = "%H:%M:%S"),
      mod = hour(time_dttm) * 60 + minute(time_dttm)
    ) |>
    mutate(
      mod = ifelse(str_detect(time, "PM") & mod < 720, mod + 720, mod),
      mod = ifelse(str_detect(time, "AM") & mod > 720, mod - 720, mod)
    ) |>
    select(location, id, year, doy, mod, temp)

  data_list[[i]] <- file
}

# combine all data frames in the list into a single data frame
combined_data <- bind_rows(data_list)

## Read and Process OTM Metadata ----

metadata <- read.csv("raw_data/otms_metadata.csv")

otms_data <- combined_data |>
  left_join(metadata, by = c("location", "id")) |>
  select(location, id, year, doy, mod, temp, microhabitat) |>
  as_tibble() |>
  distinct() |>
  filter(year > 24) |>
  filter(!(year == 25 & doy < 214)) |>
  filter(microhabitat %in% c("branch", "leaflitter")) |>
  filter(temp > -20 & temp < 40) |>
  filter(
    !paste(location, id, microhabitat) %in%
      c(
        "PSL 3 branch",
        "MIA 7 branch",
        "ATL 20 leaflitter"
      )
  ) |>
  group_by(location, id, year, doy, mod, microhabitat) |>
  summarise(temp = mean(temp, na.rm = TRUE)) |>
  ungroup()

## Quality Control: Exposed Leaf-Litter OTMs ----

# leaf-litter OTMs can become partially or fully exposed during deployment
# (e.g., when wind or animals displace the litter covering them), in which case
# they record temperatures closer to those of the open air than to those of the
# litter. Because the litter dampens daily temperature fluctuations, an exposed
# OTM shows a daily temperature range (daily max - daily min) much larger than
# that of other litter OTMs and closer to that of canopy OTMs. For each
# leaf-litter OTM, we calculate its average daily temperature range in January
# relative to the average daily range of the canopy OTMs at the same location,
# and exclude leaf-litter OTMs whose daily range exceeds 50% of the canopy's

# daily temperature range of each OTM
otms_daily <- otms_data |>
  mutate(date = as.Date(paste(year + 2000, doy), format = "%Y %j")) |>
  group_by(location, id, microhabitat, date) |>
  summarise(range_temp = max(temp) - min(temp), .groups = "drop")

# compare the January daily range of each leaf-litter OTM with that of the
# canopy OTMs at the same location
otms_jan <- otms_daily |> filter(month(date) == 1)
canopy_range <- otms_jan |>
  filter(microhabitat == "branch") |>
  group_by(location) |>
  summarise(canopy_range = mean(range_temp), .groups = "drop")
otms_qc <- otms_jan |>
  filter(microhabitat == "leaflitter") |>
  group_by(location, id) |>
  summarise(litter_range = mean(range_temp), .groups = "drop") |>
  left_join(canopy_range, by = "location") |>
  mutate(
    range_ratio = litter_range / canopy_range,
    exposed = range_ratio > 0.5
  )
print(otms_qc, n = Inf)
# in our data, only ATL 3 and ATL 5 are flagged as exposed (ratios of 0.70 and
# 0.75 vs 0.14-0.48 for all other leaf-litter OTMs)

# check how exposure developed over the deployment: the daily range of the
# exposed OTMs increased progressively (~2.8 °C in September 2025 to ~9.3 °C in
# January 2026), consistent with the gradual loss of their litter cover rather
# than with a malfunction
exposed_otms <- otms_qc |> filter(exposed)
otms_daily |>
  semi_join(
    exposed_otms |> mutate(microhabitat = "leaflitter"),
    by = c("location", "id", "microhabitat")
  ) |>
  group_by(month = floor_date(date, "month")) |>
  summarise(range_temp = mean(range_temp), .groups = "drop") |>
  print()

# remove exposed leaf-litter OTMs
otms_data <- otms_data |>
  anti_join(
    exposed_otms |> mutate(microhabitat = "leaflitter"),
    by = c("location", "id", "microhabitat")
  )

# save processed data
saveRDS(otms_data, "data/otms-data.rds")
