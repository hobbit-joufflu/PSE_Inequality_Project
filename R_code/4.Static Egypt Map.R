# ENCODING: utf-8
# Author: Adam Bernabeu
# Date: 13/08/2025
# Project: Static Egypt Map exportation

library(sf)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(here)

use_ragg <- requireNamespace("ragg", quietly = TRUE)

# ---- Paths ---

shapefile_path <- here("data", "raw", "egy_admbnda_adm2_capmas_20170421", "egy_admbnda_adm2_capmas_20170421.shp")
excel_path     <- here("data", "final", "Rfinal_merge_land_census.xlsx")
output         <- here("figures", "egypt_map.png")

# ---- CHOOSE ----
year_to_plot <- 1907
var_to_plot  <- "Jew"
n_breaks     <- 6
step_size    <- NA_real_

# ---- Load shapefile ----
gdf <- st_read(shapefile_path, quiet = TRUE) %>%
  rename(Code_1996 = ADM2_PCODE) %>%
  mutate(
    Code_1996 = stringr::str_match(Code_1996, "EG(\\d{4})")[, 2],
    Code_1996 = as.character(Code_1996)
  )

# ---- Load Excel ----
raw_data <- read_excel(excel_path) %>%
  mutate(
    Code_1996 = as.character(Code_1996),
    Code_1996 = str_replace(Code_1996, "^'", ""),
    Code_1996 = str_replace(Code_1996, "\\.0$", ""),
    Code_1996 = case_when(
      str_length(Code_1996) == 6 & str_ends(Code_1996, "00") ~ str_sub(Code_1996, 1, 4),
      str_length(Code_1996) == 5 & str_ends(Code_1996, "0")  ~ str_sub(Code_1996, 1, 4),
      TRUE ~ Code_1996
    )
  )

# ---- Checks ----
stopifnot("Year" %in% names(raw_data))
if (!(var_to_plot %in% names(raw_data))) stop(sprintf("Column '%s' not found.", var_to_plot))
if (!is.numeric(raw_data[[var_to_plot]])) stop(sprintf("Column '%s' must be numeric.", var_to_plot))

# ---- Filter & join ----
data_year <- raw_data %>% filter(Year == year_to_plot)
if (nrow(data_year) == 0) stop(sprintf("No rows for Year == %s.", year_to_plot))
merged_sf <- gdf %>% left_join(data_year, by = "Code_1996")

# ---- Legend breaks ----
vals <- merged_sf[[var_to_plot]]
vals_ok <- vals[is.finite(vals)]
rng <- range(vals_ok, na.rm = TRUE)

get_breaks <- function(rng, n_breaks, step_size) {
  if (is.finite(step_size) && !is.na(step_size) && step_size > 0) {
    lo <- floor(rng[1] / step_size) * step_size
    hi <- ceiling(rng[2] / step_size) * step_size
    seq(lo, hi, by = step_size)
  } else {
    scales::breaks_extended(n = n_breaks)(rng)
  }
}
breaks_vec <- get_breaks(rng, n_breaks, step_size)
breaks_vec <- sort(unique(breaks_vec[breaks_vec >= rng[1] & breaks_vec <= rng[2]]))
if (length(breaks_vec) < 2) {
  breaks_vec <- pretty(vals_ok, n = n_breaks)
}

# ---- Titles ----
title_text   <- sprintf("Egypt (ADM2) — %s in %s by district", var_to_plot, year_to_plot)
subtitle_txt <- "Continuous choropleth — same 'Reds' palette as Shiny"
legend_title <- sprintf("%s (%s)", var_to_plot, year_to_plot)

# ---- Plot ----
p <- ggplot(merged_sf) +
  geom_sf(aes(fill = .data[[var_to_plot]]),
          color = "grey25", linewidth = 0.2) +  # thin grey borders
  scale_fill_distiller(
    palette   = "Reds",
    direction = 1,
    na.value  = "#cccccc",
    name      = legend_title,
    breaks    = breaks_vec,
    labels    = label_number(big.mark = " ")
  ) +
  coord_sf(datum = NA) +
  theme_void(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 11, color = "grey30"),
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 10),
    legend.text      = element_text(size = 9),
    legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.3),
    legend.key.width  = unit(0.5, "cm"),
    legend.key.height = unit(0.6, "cm")
  )
# ---- Show in R ----
print(p)

# ---- Save PNG ----
if (use_ragg) {
  ragg::agg_png(output, width = 2400, height = 1600, res = 300)
  print(p)
  dev.off()
} else {
  ggsave(output, plot = p, width = 8, height = 5, dpi = 300)
}

message("Saved: ", normalizePath(output))
