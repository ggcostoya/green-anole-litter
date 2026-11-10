## Process Sleepy Lizard Survey Data ##

## Packages ----

library(tidyverse)

## Process data ----

survey <- read.csv("raw_data/sleepy-lizard-survey-data.csv")

survey <- survey |>
  mutate(
    date = ymd(DATE),
    year = year(date),
    doy = yday(date),
    time = hm(TIME),
    mod = hour(time) * 60 + minute(time),
    air_temp = as.numeric(AIR_TEMP),
    body_temp = as.numeric(BODY_TEMP),
    start_time = hm(START_TIME),
    end_time = hm(END_TIME),
    start_mod = hour(start_time) * 60 + minute(start_time),
    end_mod = hour(end_time) * 60 + minute(end_time),
    n_participants = str_count(PARTICIPANTS, "\\.") + 1,
    perch_height = as.numeric(HEIGHT_PERCH)
  ) |>
  select(
    year, doy, mod, air_temp, body_temp, start_mod, end_mod, 
    n_participants, perch_height
  ) |>
  as_tibble()

# save processed data
saveRDS(survey, "data/sleepy-lizard-survey-data.rds")


survey |>
  select(DATE, PARTICIPANTS) |>
  distinct() |>
  # divide participants by into a list by .
  as_tibble() |>
  mutate(
    participants = str_split(PARTICIPANTS, "\\.")
  ) |>
  unnest(participants) |>
  group_by(participants) |>
  summarise(n = n()) |>
  arrange(desc(n)) |>
  ggplot(aes(x = reorder(participants, n), y = n)) +
  geom_col(fill = "forestgreen") +
  geom_hline(
    yintercept = c(5, 10, 15, 20, 25),
    color = "grey",
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 8),
    expand = c(0, 0)
  ) +
  labs(
    x = "Participant",
    y = "Number of Surveys"
  ) +
  theme_classic()
