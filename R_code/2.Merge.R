# ENCODAGE : utf-8
# Author: Adam Bernabeu
# Date: 13/08/2025
# Project: Merge district demographical data across the years (Python translation)

# - Reads all "cleaned_output_*.xlsx" from intermediate_data
# - Prepares (Code_1947, Code_1996) from cleaned_output_1947.xlsx
# - Row-binds all years
# - Drops expected junk columns if present
# - Left-joins 1947 codes on Code_1996
# - Reorders Code_1947 to sit just before Code_1996
# - Cleans Code_1996 (strip trailing .0, replace "nan" with "")
# - Moves [Consistency_check, Population, Male_Total, Female_Total]
#   to just before Male_Agglomerated_Population (if that column exists)
# - Writes final xlsx to ~/Dropbox/Elite_persistence/merging_work/final_data/merged_output_census.xlsx

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(openxlsx)
  library(here)
})

options(stringsAsFactors = FALSE)

# -------------------- Paths --------------------

intermediate_path <- here("data", "intermediate")
final_dir         <- here("data", "final")

if (!dir.exists(final_dir)) dir.create(final_dir, recursive = TRUE)

# -------------------- Load 1947 mapping --------------------
file_1947 <- file.path(intermediate_path, "cleaned_output_1947.xlsx")
if (!file.exists(file_1947)) {
  stop("cleaned_output_1947.xlsx not found at: ", file_1947)
}

# Read as text to preserve codes
df_1947 <- suppressMessages(read_excel(file_1947, sheet = 1, col_types = "text"))

# Select Code_year and Code_1996; drop NA Code_1996 and rows where Code_1996 contains "'nan"
cols_1947 <- df_1947 %>%
  select(any_of(c("Code_year", "Code_1996"))) %>%
  filter(!is.na(Code_1996)) %>%
  filter(!str_detect(Code_1996, regex("\\'nan", ignore_case = TRUE))) %>%
  rename(Code_1947 = Code_year) %>%
  mutate(Code_1996 = str_trim(as.character(Code_1996)))

# -------------------- Gather all year files --------------------
all_files <- list.files(intermediate_path, pattern = "^cleaned_output_\\d{4}\\.xlsx$", full.names = TRUE)
if (length(all_files) == 0) {
  stop("No cleaned_output_*.xlsx files found in: ", intermediate_path)
}

all_dfs <- list()
for (fp in all_files) {
  # year from filename
  yr <- as.integer(str_match(basename(fp), "cleaned_output_(\\d{4})\\.xlsx")[,2])
  message("Appending file ", yr)
  # read as text, first sheet
  df <- suppressMessages(read_excel(fp, sheet = 1, col_types = "text"))
  all_dfs[[length(all_dfs) + 1]] <- df
}

# -------------------- Row-bind everything --------------------
final_df <- bind_rows(all_dfs)

# Drop irrelevant columns if present
expected_cols <- c(
  "Number_of_Occupied_Houses",
  "Number_of_Unoccupied_Houses",
  "Number_of_Houses",
  "Number_of_Households",
  "Number_of_Towns_and_Nahiya",
  "Number_of_Dependences_of_Nahiya",
  "Number_of_Villages",
  "Camps",
  "Number_of_Occupied_Houses_and_Shops",
  "Code_1991",
  "Density",
  "name",
  "Map_Code",
  "Aarabic_Name",
  "Arabic_name",
  "Female",
  "Male"
)

final_df <- final_df %>%
  mutate(Code_1996 = str_trim(as.character(.data[["Code_1996"]])))

drop_these <- intersect(expected_cols, names(final_df))
if (length(drop_these) > 0) {
  final_df <- final_df %>% select(-all_of(drop_these))
}

# -------------------- Merge 1947 codes on Code_1996 --------------------
message("Adding administrative codes of 1947...")
final_df <- final_df %>%
  left_join(cols_1947, by = "Code_1996")
message("Administrative codes of 1947 added")

# -------------------- Move Code_1947 to just before Code_1996 --------------------
if (all(c("Code_1996", "Code_1947") %in% names(final_df))) {
  cn <- names(final_df)
  # remove Code_1947 from current position
  cn <- cn[cn != "Code_1947"]
  # find index of Code_1996 and insert Code_1947 just before it
  idx1996 <- match("Code_1996", cn)
  new_order <- append(cn, values = "Code_1947", after = idx1996 - 1)
  final_df <- final_df[, new_order, drop = FALSE]
}

# -------------------- Clean Code_1996 (strip .0, replace "nan" with "") --------------------
if ("Code_1996" %in% names(final_df)) {
  final_df <- final_df %>%
    mutate(Code_1996 = str_replace(as.character(Code_1996), "\\.0$", ""),
           Code_1996 = str_replace_all(Code_1996, fixed("nan"), ""))
}
# # -------------------- Build Code_1996-Name = Code_1996 + Name (no separator) --------------------
# if (all(c("Code_1996", "Name") %in% names(final_df))) {
#   final_df <- final_df %>%
#     mutate(`Code_1996-Name` = paste0(
#       str_trim(coalesce(Code_1996, "")),
#       str_trim(coalesce(Name, ""))
#     ))
# }
# 
# # Place Code_1996-Name immediately after Code_1996
# if (all(c("Code_1996", "Code_1996-Name") %in% names(final_df))) {
#   cn <- names(final_df)
#   cn <- cn[cn != "Code_1996-Name"]
#   idx1996 <- match("Code_1996", cn)
#   cn <- append(cn, values = "Code_1996-Name", after = idx1996)
#   final_df <- final_df[, cn, drop = FALSE]
# }


# -------------------- Move key totals before Male_Agglomerated_Population --------------------
cols_to_move <- c("Consistency_check", "Population", "Male_Total", "Female_Total")
target_col <- "Male_Agglomerated_Population"
if (target_col %in% names(final_df)) {
  cn <- names(final_df)
  # Only move those that actually exist
  movables <- intersect(cols_to_move, cn)
  if (length(movables) > 0) {
    # Remove movables from current spots
    cn2 <- cn[!(cn %in% movables)]
    target_idx <- match(target_col, cn2)
    # Insert each movable in the specified order before target_idx (maintain order)
    for (mv in rev(movables)) {
      cn2 <- append(cn2, values = mv, after = target_idx - 1)
    }
    final_df <- final_df[, cn2, drop = FALSE]
  }
}

# -------------------- Write output --------------------
out_file <- file.path(final_dir, "Rmerged_output_census.xlsx")
write.xlsx(final_df, out_file, overwrite = TRUE)
message("Fichier final créé : Rmerged_output_census.xlsx")
