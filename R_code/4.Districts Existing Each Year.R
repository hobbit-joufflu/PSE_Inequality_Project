# ENCODAGE : utf-8
# Author: Adam Bernabeu
# Date: 05/08/2025
# Project: EgyptMapR — Create interactive map with demographic summaries

# ─── Libraries ───────────────────────────────────────────
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)
library(ggplot2)
library(here)

# ─── Load Data ──────────────────────────────────────────

df <- read_excel(here("data", "final", "Rfinal_merge_land_census.xlsx"))

# ─── Clean Code_1996 ───────────────────────────────────
df <- df %>%
  mutate(
    Code_1996 = as.character(Code_1996),
    Code_1996 = str_replace(Code_1996, "^'", ""),
    Code_1996 = str_replace(Code_1996, "\\.0$", ""),
    Code_1996 = case_when(
      str_length(Code_1996) == 6 & str_ends(Code_1996, "00") ~ str_sub(Code_1996, 1, 4),
      str_length(Code_1996) == 5 & str_ends(Code_1996, "0")  ~ str_sub(Code_1996, 1, 4),
      TRUE ~ Code_1996
    ),
    Dept_Code = str_sub(Code_1996, 1, 2)
  )

# ─── Detect name column automatically ──────────────────
name_col <- names(df)[str_detect(names(df), regex("name", ignore_case = TRUE))][1]

# ─── Filter and ensure integer year ─────────────────────
df <- df %>%
  filter(!is.na(Year), !is.na(Code_1996)) %>%
  mutate(Year = as.integer(Year))

# ─── Get full list of years and base set from 1996 ──────
all_years <- sort(unique(df$Year))
base_codes <- df %>% filter(Year == 1996) %>% distinct(Code_1996)

# ─── Create structure by merging each year to base ──────
year_blocks <- list()

for (year in all_years) {
  sub_df <- df %>%
    filter(Year == year) %>%
    mutate(
      Code_year = as.character(Code_year),
      Code_year = str_replace(Code_year, "^'", ""),
      Code_year = str_replace(Code_year, "\\.0$", ""),
      Code_year = case_when(
        str_length(Code_year) == 6 & str_ends(Code_year, "00") ~ str_sub(Code_year, 1, 4),
        str_length(Code_year) == 5 & str_ends(Code_year, "0")  ~ str_sub(Code_year, 1, 4),
        TRUE ~ Code_year
      )
    ) %>%
    select(Code_1996, Code_year, !!sym(name_col)) %>%
    rename_with(~ paste0(year, "_", .), -Code_1996) %>%
    rename(!!paste0(year, "_district_Code") := paste0(year, "_Code_year"))
  
  year_blocks[[as.character(year)]] <- base_codes %>%
    left_join(sub_df, by = "Code_1996")
}
# ─── Combine horizontally by Code_1996 ──────────────────
final_df <- purrr::reduce(year_blocks, left_join, by = "Code_1996")

# ─── Export to Excel ───────────────────────────────────
write_xlsx(final_df, here("data", "final", "reshaped_by_code_1996.xlsx"))
cat("Saved to: reshaped_by_code_1996.xlsx\n")

# Plot: number of unique districts per year
district_counts <- df %>%
  group_by(Year) %>%
  summarise(n_districts = n_distinct(Code_1996))

# Plot
ggplot(district_counts, aes(x = Year, y = n_districts)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Districts per Year in Dataset",
       x = "Year", y = "Number of Districts") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"))

ggsave(here("figures", "district_counts_per_year.png"), plot = p, width = 8, height = 5)