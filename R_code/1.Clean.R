# ENCODAGE : utf-8
# Author: Adam Bernabeu
# Date: 13/08/2025
# Project: Clean district demographical data (Python translation)

# Writes: ~/Dropbox/Elite_persistence/merging_work/intermediate_data/Rcleaned_output_<year>.xlsx

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(stringr)
  library(here)
})

options(stringsAsFactors = FALSE)

# -------------------- Config --------------------

folder_path <- here("data", "raw")
output_path <- here("data", "intermediate")

filter_column <- "Type"
filter_values <- c("qism", "markaz")

if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

all_files <- list.files(folder_path, pattern = "\\.xls$", full.names = FALSE)
files <- setdiff(all_files, c("2006.xls", "1966.xls"))

# -------------------- Helpers --------------------
safe_true  <- function(x) isTRUE(x)                 # turn NA → FALSE
safe_false <- function(x) isFALSE(x)                # turn NA → FALSE

to_char_df <- function(d) {
  if (!is.data.frame(d)) return(data.frame())
  bad <- vapply(d, function(col) is.list(col) || is.matrix(col) || is.data.frame(col), logical(1))
  if (any(bad)) d <- d[, !bad, drop = FALSE]
  d[] <- lapply(d, as.character)
  d <- as.data.frame(d, check.names = FALSE)
  rownames(d) <- NULL
  d
}

hconcat_axis1 <- function(dflist) {
  if (length(dflist) == 0) return(data.frame())
  if (length(dflist) == 1) return(to_char_df(dflist[[1]]))
  max_n <- max(vapply(dflist, nrow, integer(1), USE.NAMES = FALSE))
  padded <- lapply(dflist, function(d) {
    d <- to_char_df(d)
    if (nrow(d) < max_n) {
      add <- matrix(NA_character_, nrow = max_n - nrow(d), ncol = ncol(d))
      colnames(add) <- colnames(d)
      d <- rbind(d, as.data.frame(add, check.names = FALSE))
    }
    d
  })
  do.call(cbind, padded)
}

safe_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))

pad_with_zero <- function(val) {
  val <- as.character(val)
  val <- str_trim(val)
  val <- str_replace(val, "\\.0$", "")
  if (is.na(val) || val == "") return("'")
  if (nchar(val) == 5) paste0("'", "0", val) else paste0("'", val)
}

read_sheet_text <- function(path, sh) {
  tryCatch(read_excel(path, sheet = sh, col_types = "text"),
           error = function(e) data.frame())
}
# -----------------------------------------------------------------------

for (file in files) {
  file_path <- file.path(folder_path, file)
  tryCatch({
    cat("🔍 Reading file:", file, "\n")
    
    sheet_names <- tryCatch(excel_sheets(file_path), error = function(e) character(0))
    if (length(sheet_names) == 0) {
      cat("  ⚠️ No readable sheets; skipping.\n")
      next
    }
    
    # Read + clean each sheet
    sheet_dfs <- vector("list", length(sheet_names))
    for (i in seq_along(sheet_names)) {
      df <- read_sheet_text(file_path, sheet_names[i])
      if (ncol(df) > 0) {
        colnames(df) <- str_trim(colnames(df))
        if (safe_true(filter_column %in% colnames(df))) {
          keep_mask <- df[[filter_column]] %in% filter_values
          keep_mask[is.na(keep_mask)] <- FALSE
          df <- df[keep_mask, , drop = FALSE]
        }
      }
      sheet_dfs[[i]] <- to_char_df(df)
    }
  
    last_df <- sheet_dfs[[length(sheet_dfs)]]
    cat("Kept", nrow(last_df), "rows after filtering on '", filter_column, "' in", file, "\n")
    
    # Horizontal concat (axis=1) + drop duplicate columns (keep first)
    df <- hconcat_axis1(sheet_dfs)
    if (ncol(df) > 0) {
      keep <- !duplicated(colnames(df))
      df <- df[, keep, drop = FALSE]
    }
    
    # Strip "Total_" prefix
    if (ncol(df) > 0) colnames(df) <- str_replace(colnames(df), "^Total_", "")
    
    # Year from filename; insert first column
    year <- suppressWarnings(as.integer(str_extract(file, "\\d{4}")))
    df <- cbind(Year = year, df)
    
    # Renames like Python
    nm <- colnames(df)
    nm[nm == "Serial_Numbers"]    <- "Serial_Number"
    nm[nm == "Male_Population"]   <- "Male_Total"
    nm[nm == "Female_Population"] <- "Female_Total"
    colnames(df) <- nm
    
    # Code_<year> → Code_year (1986.xls uses Code_1991)
    code_col <- if (tolower(file) != "1986.xls") paste0("Code_", year) else "Code_1991"
    if (!is.na(code_col) && safe_true(code_col %in% colnames(df))) {
      colnames(df)[colnames(df) == code_col] <- "Code_year"
    }
    
    # If 1996: insert Code_1996 after Code_year
    if (!is.na(year) && year == 1996 && safe_true("Code_year" %in% colnames(df))) {
      pos <- match("Code_year", colnames(df))
      left  <- df[, 1:pos, drop = FALSE]
      right <- if (pos < ncol(df)) df[, (pos + 1):ncol(df), drop = FALSE] else NULL
      df <- cbind(left, Code_1996 = left[["Code_year"]], right)
    }
    
    # Consistency_check after Population
    req <- c("Male_Total", "Female_Total", "Population")
    if (all(req %in% colnames(df))) {
      pop_pos <- match("Population", colnames(df))
      male   <- safe_num(df[["Male_Total"]])
      female <- safe_num(df[["Female_Total"]])
      pop    <- safe_num(df[[pop_pos]])
      consistency_check <- (male + female) == pop
      consistency_check[is.na(consistency_check)] <- TRUE
      
      left  <- df[, 1:pop_pos, drop = FALSE]
      right <- if (pop_pos < ncol(df)) df[, (pop_pos + 1):ncol(df), drop = FALSE] else NULL
      df <- cbind(left, Consistency_check = consistency_check, right)
      
      # Fix Population where FALSE (ensure mask has no NA)
      mask <- df$Consistency_check == FALSE
      mask[is.na(mask)] <- FALSE
      if (any(mask)) {
        df[mask, pop_pos] <- as.character(male[mask] + female[mask])
      }
      cat(sum(df$Consistency_check, na.rm = TRUE), "Population cells aggregated\n")
    }
    
    # Code cleanup & padding — guarded
    # Some files may not have these columns; also avoid NA in if()
    if (safe_true("Code_year" %in% colnames(df))) {
      cy <- df[["Code_year"]]
      cy <- str_replace(as.character(cy), "\\.0$", "")
      df[["Code_year"]] <- vapply(cy, pad_with_zero, character(1), USE.NAMES = FALSE)
    }
    if (safe_true("Code_1996" %in% colnames(df))) {
      c96 <- df[["Code_1996"]]
      c96 <- str_replace(as.character(c96), "\\.0$", "")
      df[["Code_1996"]] <- vapply(c96, pad_with_zero, character(1), USE.NAMES = FALSE)
    }
    
    # Export
    out_file <- file.path(output_path, sprintf("Rcleaned_output_%d.xlsx", year))
    cat("Exporting cleaned and concatenated file for", year, "...\n")
    df_to_write <- df
    colnames(df_to_write) <- make.unique(colnames(df_to_write), sep = "_dup")
    write.xlsx(df_to_write, out_file, overwrite = TRUE)
    cat("✅", year, "done\n")
    
    # Debug sample for Code_1996
    if (safe_true("Code_1996" %in% colnames(df)) && nrow(df) > 0) {
      smp <- head(df[["Code_1996"]], 10)
      cat("Sample Code_1996 values:\n")
      for (i in seq_along(smp)) cat("  ", smp[i], "\n")
    }
    
    rm(df, df_to_write, sheet_dfs); gc()
    
  }, error = function(e) {
    cat("❌ Error processing", file, ":", conditionMessage(e), "\n")
  })
}
