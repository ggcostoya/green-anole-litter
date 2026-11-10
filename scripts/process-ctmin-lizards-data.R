
## Proces CTmin data from multiple studies ##

## Packages ----

library(tidyverse)

## GlobTherm ----

globtherm <- read.csv("raw_data/GlobThermReview_TLC.csv")

globtherm <- globtherm |>
  filter(include_TLC == "yes") |>
  mutate(species = paste(Genus, Species, sep = " ")) |>
  mutate(family = NA, source = "globtherm", min_elev = NA, max_elev = NA) |>
  select(
    family, genus = Genus, species, ctmin = tmin, min_elev, max_elev, source
  )

## Bodensteiner et al. 2024 ----

bod <- read.csv("raw_data/bodensteiner_et_al_2024.csv")

bod <- bod |>
  filter(!Species == "") |>
  mutate(species = paste(Genus, Species, sep = " ")) |>
  mutate(ctmin = as.numeric(gsub("±.*", "", CTmin))) |>
  mutate(
    min_elev_temp = as.numeric(gsub(".*-", "", elevation_range)),
    max_elev_temp= as.numeric(gsub("-.*", "", elevation_range))
  ) |>
  # swap min and max elevation if min is greater than max
  mutate(
    min_elev = ifelse(
      min_elev_temp > max_elev_temp, max_elev_temp, min_elev_temp
    ),
    max_elev = ifelse(
      max_elev_temp < min_elev_temp, min_elev_temp, max_elev_temp
    )
  ) |>
  mutate(family = "Dactyloidae") |>
  select(family, species, ctmin, min_elev, max_elev) |>
  mutate(source = "bod")

## Salazar et al. 2025 ----

salazar <- read.csv("raw_data/salazar_et_al_2025.csv")

salazar <- salazar |>
  # remove the "A. " from the species names
  mutate(species = gsub("A. ", "", Species)) |>
  mutate(
    species = paste("Anolis", species, sep = " ")
  ) |>
  # remove character U-2005 from Elevation column
  mutate(
    elev = gsub("\u2005", "", Elevation..m.)
  ) |>
  mutate(
    elevation_avg = gsub("±.*", "", elev),
    elevation_range = gsub(".*±", "", elev)
  ) |>
  mutate(
    min_elev = as.numeric(elevation_avg) - as.numeric(elevation_range),
    max_elev = as.numeric(elevation_avg) + as.numeric(elevation_range)
  ) |>
  mutate(
    ctmin = gsub("\\(.*", "", CTmin...C.),
    ctmin = gsub(" ", "", ctmin),
    ctmin = as.numeric(gsub("±.*", "", ctmin))
  ) |>
  mutate(family = "Dactyloidae") |>
  select(family, species, ctmin, min_elev, max_elev) |>
  mutate(source = "salazar")

## Combine datasets and search complementary info in SquamBase ----

# combine datasets
lizards_ctmin <- bind_rows(globtherm, bod, salazar) |> as_tibble()
lizards_ctmin$gard_id <- NA
lizards_ctmin$substrate_type <- NA

# read and process squamBase data
squambase <- read.csv("raw_data/squamBase.csv")
squambase <- squambase |>
  filter(sub.order == "Sauria") |>
  rename(
    family = Family,
    species = Species.name..Binomial.,
    synonyms = Synonyms,
    substrate = substrate,
    min_elev = Minimum.elevation..m.,
    max_elev = Maximum.elevation..m.,
    gard_id = GARD.id
  ) |>
  mutate(
    genus = gsub(" .*", "", species),
  ) |>
  as_tibble() |>
  filter(!is.na(min_elev) | !is.na(max_elev)) |>
  mutate(
    substrate_type = case_when(
      substrate == "Arboreal" ~ "arboreal",
      grepl("Arboreal", substrate) ~ "semi-arboreal",
      !grepl("Arboreal", substrate) ~ "other",
      TRUE ~ NA_character_
    )
  ) |>
  select(
    family, genus, species, synonyms, substrate_type,
    min_elev, max_elev, gard_id
  )

# search for complementary information in squamBase
for (i in seq_len(nrow(lizards_ctmin))) {

  squambase_species <- squambase |>
    filter(species == lizards_ctmin$species[i])

  if (nrow(squambase_species) == 0) {

    # filter any row that contains a synonym of the species in squamBase
    squambase_species <- squambase |>
      filter(grepl(lizards_ctmin$species[i], synonyms))

    if (nrow(squambase_species) == 0) {
      print(paste0(
        "No info found for ", lizards_ctmin$species[i]
      ))
      next
    }
  }

  # filter only first row if multiple rows are found
  squambase_species <- squambase_species[1, ]

  # add family, genus, min_elev, max_elev, and gard_id to lizards_ctmin if missing
  cols <- c("family", "genus", "min_elev", "max_elev", "gard_id", "substrate_type")
  walk(cols, \(col) {
    if (is.na(lizards_ctmin[[col]][i])) {
      lizards_ctmin[[col]][i] <<- squambase_species[[col]]
    }
  })
}

# correct family names
lizards_ctmin <- lizards_ctmin |>
  mutate(
    family = case_when(
      family == "Anolidae" ~ "Dactyloidae",
      family == "Polychrotidae" ~ "Dactyloidae",
      TRUE ~ family
    )
  )

# filter missing data
lizards_ctmin <- lizards_ctmin |>
  filter(!is.na(gard_id) & !is.na(min_elev) & !is.na(max_elev)) |>
  mutate(
    max_elev = ifelse(max_elev - min_elev < 100, max_elev + 500, max_elev)
  ) |>
  mutate(
    ctmin = ifelse(species == "Anolis carolinensis", 6.8, ctmin)
  ) |>
  mutate(
    substrate_type = ifelse(
      species == "Liolaemus tenuis", "semi-arboreal", substrate_type
    )
  )

# save processed data
saveRDS(lizards_ctmin, "data/ctmin_lizards.rds")
