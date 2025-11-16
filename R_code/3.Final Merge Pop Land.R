# ENCODAGE : utf-8
# Author: Adam Bernabeu
# Date: 13/08/2025
# Project: Merge land, population data (Python translation)
# Outputs: ~/Dropbox/Elite_persistence/merging_work/final_data/Rfinal_merge_land_census.xlsx

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(openxlsx)
  library(readr)   # parse_integer / parse_double / parse_logical
  library(here)
  })

options(stringsAsFactors = FALSE)

# -------------------- Paths --------------------

land_path   <- here("data", "intermediate", "land_dist_1950_1961.xlsx")
census_path <- here("data", "final", "Rmerged_output_census.xlsx")
out_path    <- here("data", "final", "Rfinal_merge_land_census.xlsx")

# -------------------- Read inputs (as text; blanks/NaN -> NA) --------------------
df1 <- read_excel(land_path,   sheet = 1, col_types = "text", na = c("", "NA", "NaN", "nan"))
df2 <- read_excel(census_path, sheet = 1, col_types = "text", na = c("", "NA", "NaN", "nan"))

# -------------------- Harmonize codes --------------------
normalize_code1996 <- function(x) {
  x <- as.character(x)
  x <- str_replace(x, "\\.0$", "")
  x <- str_replace(x, "^'", "")
  # pad with leading zero if only 5 digits
  ifelse(nchar(x) == 5, paste0("0", x), x)
}

if ("district_code_1996" %in% names(df1)) {
  names(df1)[names(df1) == "district_code_1996"] <- "Code_1996"
}
if ("Code_1996" %in% names(df1)) {
  df1 <- df1 %>% mutate(Code_1996 = normalize_code1996(Code_1996))
}
if ("Code_1996" %in% names(df2)) {
  df2 <- df2 %>% mutate(Code_1996 = normalize_code1996(Code_1996))
}

# -------------------- Prevent Code_year clash before join --------------------
if ("Code_year" %in% names(df1)) {
  names(df1)[names(df1) == "Code_year"] <- "Code_year_land"
}

# -------------------- Merge (left join on Code_1996) --------------------
cat("Merging on Code_1996...\n")
merged_df <- df2 %>% left_join(df1, by = "Code_1996")

# -------------------- Restore Code_year from df2 after join --------------------
if ("Code_year.x" %in% names(merged_df)) {
  merged_df <- merged_df %>% rename(Code_year = Code_year.x)
}
if ("Code_year.y" %in% names(merged_df)) {
  merged_df <- merged_df %>% select(-Code_year.y)
}

# Clean codes
if ("Code_1996" %in% names(merged_df)) {
  merged_df <- merged_df %>%
    mutate(Code_1996 = ifelse(Code_1996 %in% c("nan","NaN"), "", Code_1996))
}
if ("Code_1947" %in% names(merged_df)) {
  merged_df <- merged_df %>%
    mutate(Code_1947 = str_replace(as.character(Code_1947), "\\.0$", ""),
           Code_1947 = ifelse(Code_1947 %in% c("nan","NaN"), "", Code_1947))
}

# -------------------- (Re)build Code_1996-Name (no separator) --------------------
dup_cols <- intersect(names(merged_df), c("Code_1996-Name.x","Code_1996-Name.y"))
if (length(dup_cols)) merged_df <- merged_df %>% select(-all_of(dup_cols))

if (all(c("Code_1996","Name") %in% names(merged_df))) {
  merged_df <- merged_df %>%
    mutate(
      Name = as.character(Name),
      `Code_1996-Name` = paste0(str_trim(coalesce(Code_1996, "")),
                                str_trim(coalesce(Name, ""))),
      `Code_1996-Name` = str_replace(`Code_1996-Name`, "^'", "")
    )
}

# Put Code_1996-Name right after Code_1996
if (all(c("Code_1996","Code_1996-Name") %in% names(merged_df))) {
  cn <- names(merged_df)
  cn <- cn[cn != "Code_1996-Name"]
  idx <- match("Code_1996", cn)
  cn <- append(cn, "Code_1996-Name", after = idx)
  merged_df <- merged_df[, cn, drop = FALSE]
}

# -------------------- Parse types: numbers as numeric --------------------
keep_text <- c("Code_1996","Code_1947","Name","Arabic_Name","Type",
               "district_code_1960","gov","Code_1991","Map_Code",
               "Aarabic_Name","Arabic_name","Female","Male",
               "Code_1996-Name","Code_year")
keep_text <- intersect(keep_text, names(merged_df))

if ("Year" %in% names(merged_df))          merged_df$Year <- readr::parse_integer(merged_df$Year)
if ("Serial_Number" %in% names(merged_df)) merged_df$Serial_Number <- readr::parse_integer(merged_df$Serial_Number)

if ("Consistency_check" %in% names(merged_df)) {
  merged_df$Consistency_check <- readr::parse_logical(merged_df$Consistency_check,
                                                      na = c("", "NA", "NaN", "nan"))
}

num_candidates <- setdiff(names(merged_df), c(keep_text, "Year","Serial_Number","Consistency_check"))
num_candidates <- num_candidates[sapply(merged_df[num_candidates], is.character)]

if (length(num_candidates)) {
  merged_df <- merged_df %>%
    mutate(across(all_of(num_candidates),
                  ~ readr::parse_double(.x,
                                        locale = readr::locale(grouping_mark = ","),
                                        na = c("", "NA", "NaN", "nan"))))
}

# -------------------- Drop irrelevant columns if present --------------------
expected_cols <- c("district_name","Comment","Map_code","Status","Serail_Number","X","Y","Transcription")
drop_these <- intersect(expected_cols, names(merged_df))
if (length(drop_these) > 0) merged_df <- merged_df %>% select(-all_of(drop_these))

cat("Merged, typed, and harmonized\n")

# -------------------- Write Excel --------------------
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", merged_df)

# Auto widths
widths <- sapply(seq_along(merged_df), function(i) {
  col <- merged_df[[i]]
  m1 <- suppressWarnings(max(nchar(as.character(col)), na.rm = TRUE))
  if (!is.finite(m1)) m1 <- nchar(names(merged_df)[i])
  max(m1, nchar(names(merged_df)[i])) + 2
})
setColWidths(wb, "Sheet1", cols = 1:ncol(merged_df), widths = widths)

saveWorkbook(wb, out_path, overwrite = TRUE)
cat("File created:", out_path, "\n")
