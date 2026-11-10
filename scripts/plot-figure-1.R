## Plot manuscript Figure 1 ##

# Builds the complete figure (maps, daily temperatures, photographs and layout)
# from the data and images folders. Run from the root of the repository:
#   Rscript scripts/plot-figure-1.R

## Packages ----

library(tidyverse)
library(viridis)
library(cowplot)
library(jpeg)
library(grid)

## Data ----

micro_df <- readRDS("data/micro_df.rds")
otm_data <- readRDS("data/otms-data.rds")
ctmin_min <- 6.8 # lowest population-average CTmin (see live-beyond-ctmin.qmd)
font <- "Arial"

# locations where we measured CTmin
sites <- readRDS("data/ctmin_gradient.rds") |>
  group_by(site) |>
  summarise(lat = mean(lat), lon = mean(lon))
site_coords <- function(x) filter(sites, site == x)

## Shared map elements ----

map_scales <- list(
  scale_x_continuous(
    breaks = seq(-100, -75, 5),
    labels = function(x) paste0(abs(x), "°W"),
    expand = c(0, 0)
  ),
  scale_y_continuous(
    breaks = seq(25, 37.5, 2.5),
    labels = function(x) paste0(x, "°N"),
    expand = c(0, 0)
  ),
  coord_quickmap(xlim = c(-101, -75), ylim = c(24, 38))
)
map_sites <- geom_point(
  data = sites,
  aes(x = lon, y = lat),
  inherit.aes = FALSE,
  shape = 21, size = 4, stroke = 0.7, fill = "gray92", color = "black"
)
map_theme <- theme_minimal(base_family = font) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 8),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.25),
    plot.title = element_text(size = 14, hjust = 0.5, margin = margin(b = 3)),
    legend.position = "inside",
    legend.position.inside = c(0.985, 0.08),
    legend.justification = c(1, 0),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.width = unit(0.28, "cm"),
    plot.margin = margin(t = 26, r = 14, b = 2, l = 18)
  )
colorbar <- guide_colorbar(
  label.position = "left",
  barheight = unit(2.6, "cm")
)

## Panels A & B: minimum annual body temperatures in canopy and leaf litter ----

plot_temperature_map <- function(depth, edge_label = FALSE) {
  df <- filter(micro_df, as.character(z) == depth)
  p <- ggplot(df, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = min_annual_body_temp)) +
    geom_contour(
      aes(z = presence),
      breaks = 0.5, color = "forestgreen", linewidth = 0.6
    ) +
    geom_contour(
      aes(z = min_annual_body_temp),
      breaks = 0, color = "steelblue4", linewidth = 1
    ) +
    geom_contour(
      aes(z = min_annual_body_temp),
      breaks = ctmin_min, color = "lightblue", linewidth = 1
    ) +
    map_sites +
    annotate(
      "segment",
      x = -95.6, xend = -93.8, y = c(27, 25.6), yend = c(27, 25.6),
      color = c("steelblue4", "lightblue"), linewidth = 1.8
    ) +
    annotate(
      "text",
      x = -93.3, y = 27, hjust = 0,
      label = "Lethal limit (0 °C)",
      color = "steelblue4", fontface = "bold", size = 4.6, family = font
    ) +
    annotate(
      "text",
      x = -93.3, y = 25.5, hjust = 0, parse = TRUE,
      label = "bold(CT[min])",
      color = "lightblue", size = 4.6, family = font
    ) +
    scale_fill_viridis(
      option = "magma",
      guide = colorbar,
      breaks = seq(-10, 20, 10),
      labels = paste0(seq(-10, 20, 10), " °C"),
      limits = range(micro_df$min_annual_body_temp[
        as.character(micro_df$z) %in% c("-0.04", "1.8")
      ], na.rm = TRUE)
    ) +
    map_scales +
    map_theme
  if (edge_label) {
    p <- p + annotate(
      "text",
      x = -95.2, y = 35.75, angle = 15,
      label = "Range edge",
      color = "forestgreen", fontface = "bold", size = 4.4, family = font
    )
  }
  p
}

fig_1a <- plot_temperature_map("1.8", edge_label = TRUE)
fig_1b <- plot_temperature_map("-0.04")

# two-colored panel title: the black and colored parts are drawn on top of each
# other, each leaving a blank space (phantom) for the other
add_map_title <- function(plot, word, color, y) {
  title_x <- 0.231
  black <- bquote("Lizard Body Temperature in " * phantom(bold(.(word))))
  colored <- bquote(phantom("Lizard Body Temperature in ") * bold(.(word)))
  plot +
    draw_label(black, x = title_x, y = y, size = 14, color = "black", fontfamily = font) +
    draw_label(colored, x = title_x, y = y, size = 14, color = color, fontfamily = font)
}

## Panel C: tree cover ----

city_label <- function(site, x, y, name, leader_x, leader_y) {
  coords <- site_coords(site)
  list(
    annotate(
      "segment",
      x = coords$lon, y = coords$lat, xend = leader_x, yend = leader_y,
      linetype = 2, linewidth = 0.3
    ),
    annotate(
      "text",
      x = x, y = y, label = name, size = 4.4, parse = TRUE, family = font
    )
  )
}

fig_1c <- micro_df |>
  filter(z == 0) |>
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = veg)) +
  geom_contour(aes(z = presence), breaks = 0.5, color = "black", linewidth = 0.5) +
  city_label("ATL", -89.8, 27, "bold(Atlanta)~'(33.7 °N)'", -89.8, 27.55) +
  city_label("MIA", -88.4, 25.2, "bold(Miami)~'(25.7 °N)'", -84.9, 25.2) +
  map_sites +
  scale_fill_gradient(
    low = "lightgoldenrod1",
    high = "darkgreen",
    guide = colorbar,
    breaks = seq(0, 1, 0.2),
    labels = paste0(seq(0, 1, 0.2) * 100, "%")
  ) +
  map_scales +
  map_theme +
  theme(plot.margin = margin(t = 4, r = 14, b = 2, l = 18)) +
  labs(title = "Tree Cover")

## Panel D: daily operative temperatures in Miami and Atlanta ----

otm_summary <- otm_data |>
  filter(
    location %in% c("ATL", "MIA"),
    microhabitat %in% c("branch", "leaflitter"),
    doy == 19
  ) |>
  mutate(
    location = factor(
      location,
      levels = c("MIA", "ATL"),
      labels = c("Miami (25.7 °N)", "Atlanta (33.7 °N)")
    ),
    hour = round(mod / 60)
  ) |>
  group_by(location, microhabitat, hour) |>
  summarise(
    mean_temp = mean(temp, na.rm = TRUE),
    sd_temp = sd(temp, na.rm = TRUE),
    .groups = "drop"
  )

microhabitat_colors <- c("forestgreen", "darkorange")
microhabitat_labels <- c("Canopy", "Leaf Litter")

fig_1d <- otm_summary |>
  ggplot(aes(x = hour, y = mean_temp, color = microhabitat)) +
  geom_hline(yintercept = 0, linetype = 2, color = "steelblue4") +
  geom_hline(yintercept = ctmin_min, linetype = 2, color = "skyblue") +
  annotate(
    "text",
    x = 23.4, y = 7.1, hjust = 1, vjust = 0, parse = TRUE,
    label = "bold(CT[min])", color = "skyblue", size = 5, family = font
  ) +
  annotate(
    "text",
    x = 23.4, y = -0.4, hjust = 1, vjust = 1,
    label = "Lethal limit (0 °C)",
    color = "steelblue4", fontface = "bold", size = 4.4, family = font
  ) +
  geom_ribbon(
    aes(
      ymin = mean_temp - sd_temp,
      ymax = mean_temp + sd_temp,
      fill = microhabitat
    ),
    alpha = 0.2
  ) +
  scale_fill_manual(
    values = microhabitat_colors, labels = microhabitat_labels, name = NULL
  ) +
  scale_color_manual(
    values = microhabitat_colors, labels = microhabitat_labels, name = NULL
  ) +
  facet_wrap(~location, ncol = 2, axes = "all") +
  scale_x_continuous(
    breaks = seq(0, 24, 6),
    labels = paste0(seq(0, 24, 6), ":00"),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(-4, 20.5)) +
  theme_minimal(base_family = font) +
  theme(
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.25),
    strip.text = element_text(size = 19, hjust = 0.5, face = "bold"),
    legend.position = "inside",
    legend.position.inside = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.margin = margin(2, 3, 2, 2),
    legend.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14, margin = margin(r = 8)),
    panel.spacing = unit(1.2, "lines"),
    plot.margin = margin(t = 6, r = 12, b = 4, l = 10)
  ) +
  labs(y = "Lizard Body Temperature (°C)")

## Panels E & F: photographs ----

# crop an image array to a given width / height ratio around a center (as
# proportions of the width and height of the image)
crop_image <- function(img, ratio, zoom = 1, center = c(0.5, 0.5)) {
  h <- dim(img)[1]
  w <- dim(img)[2]
  crop_h <- min(h, w / ratio) * zoom
  crop_w <- crop_h * ratio
  left <- round(min(max(center[1] * w - crop_w / 2, 0), w - crop_w))
  top <- round(min(max(center[2] * h - crop_h / 2, 0), h - crop_h))
  img[top + seq_len(floor(crop_h)), left + seq_len(floor(crop_w)), 1:3]
}

# make everything outside the inscribed circle of a square image transparent
circle_image <- function(img) {
  n <- nrow(img)
  m <- ncol(img)
  r <- matrix(
    sqrt(((row(img[, , 1]) - n / 2) / (n / 2))^2 +
      ((col(img[, , 1]) - m / 2) / (m / 2))^2),
    n, m
  )
  array(c(img, as.numeric(r <= 1)), dim = c(n, m, 4))
}

# light moving average along rows and columns, which smooths the backdrops
# when they are enlarged
smooth_image <- function(img, k = 2) {
  box <- function(v) {
    n <- length(v)
    totals <- cumsum(c(0, v))
    lo <- pmax(seq_len(n) - k, 1)
    hi <- pmin(seq_len(n) + k, n)
    (totals[hi + 1] - totals[lo]) / (hi - lo + 1)
  }
  for (band in 1:3) {
    img[, , band] <- t(apply(apply(img[, , band], 2, box), 1, box))
  }
  img
}

add_photo <- function(plot, path, backdrop_path, x, y, width, height, canvas,
                      circle_d, zoom = 1, center = c(0.5, 0.5),
                      backdrop_center = c(0.5, 0.5)) {
  img <- readJPEG(path)
  backdrop <- smooth_image(crop_image(
    readJPEG(backdrop_path),
    (width * canvas[1]) / (height * canvas[2]),
    center = backdrop_center
  ))
  portrait <- circle_image(crop_image(img, 1, zoom = zoom, center = center))
  circle_w <- circle_d / canvas[1]
  circle_h <- circle_d / canvas[2]
  circle_x <- x + (width - circle_w) / 2
  circle_y <- y + (height - circle_h) / 2
  plot +
    draw_grob(rasterGrob(backdrop, interpolate = TRUE,
                         width = unit(1, "npc"), height = unit(1, "npc")),
              x, y, width, height) +
    draw_grob(rasterGrob(portrait, interpolate = TRUE,
                         width = unit(1, "npc"), height = unit(1, "npc")),
              circle_x, circle_y, circle_w, circle_h) +
    draw_grob(circleGrob(r = 0.5, gp = gpar(fill = NA, col = "white", lwd = 7)),
              circle_x, circle_y, circle_w, circle_h)
}

## Layout ----

canvas <- c(12, 9.87) # width and height in inches
map_h <- 1 / 3
left_w <- 0.42
d_y <- 0.555
photo_w <- 0.235
photo_h <- 0.5

fig_1 <- ggdraw() +
  draw_plot(fig_1a, 0, 2 * map_h, left_w, map_h) +
  draw_plot(fig_1b, 0, map_h, left_w, map_h) +
  draw_plot(fig_1c, 0, 0, left_w, map_h) +
  draw_plot(fig_1d, left_w, d_y, 1 - left_w, 1 - d_y)

fig_1 <- fig_1 |>
  add_map_title("Canopy", "forestgreen", y = 0.982) |>
  add_map_title("Leaf Litter", "darkorange", y = 0.649) |>
  add_photo(
    "images/sleepy_anole_jon_suh.jpg",
    "images/figure_1_backdrop_miami.jpg",
    x = 0.47, y = 0.02, width = photo_w, height = photo_h,
    canvas = canvas, circle_d = 3.15,
    zoom = 0.5, center = c(0.553, 0.55), backdrop_center = c(0.58, 0.5)
  ) |>
  add_photo(
    "images/inat_obs_flown_kimmerling.jpg",
    "images/figure_1_backdrop_atlanta.jpg",
    x = 0.745, y = 0.02, width = photo_w, height = photo_h,
    canvas = canvas, circle_d = 3.15,
    zoom = 0.45, center = c(0.563, 0.51)
  )

fig_1 <- fig_1 +
  draw_plot_label(
    c("a", "b", "c", "d", "e", "f"),
    x = c(0.004, 0.004, 0.004, 0.425, 0.478, 0.753),
    y = c(0.998, 0.665, 0.332, 0.998, 0.512, 0.512),
    size = 24,
    family = font,
    color = c("black", "black", "black", "black", "white", "white"),
    hjust = 0
  )

ggsave(
  "images/figure_1.png", fig_1,
  width = canvas[1], height = canvas[2], dpi = 300, bg = "white"
)
