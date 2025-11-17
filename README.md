# 🌍 Research Assistant Project : Land Reform, Inequality, and Elite Persistence

This repository contains the R and Python code I developed during the second part of my research assistantship at the **Paris School of Economics (PSE)**.
This work supported the research paper "Essays on Land Reform, Inequality, and Elite persistence in Egypt and Comparative Perspective. My primary contribution was building a **reproducible data pipeline** to import, clean, process, and analyze multi-decade cross-country datasets on land distribution and demographics in Egypt.

This project culminates in an **interactive RShiny application** that allows for visual exploration of this data.

## Live Interactive Demo

This application is hosted publicly on `shinyapps.io`. The dashboard allows for dynamic exploration of historical demographic and inequality data across Egypt's districts and years.

**[Click here to view the live application](https://hobbit-joufflu.shinyapps.io/PSE_Research_Project/)**

## Key Contributions & Methodology

My role was to transform heterogeneous raw data into a structured format ready for econometric analysis.

### 1. Data Engineering Pipeline
The scripts in the `/R` folder (`1.Clean.R`, `2.Merge.R`, etc.) perform the following:
* **Data Cleaning & Merging:** Reading and cleaning raw Excel files from multiple census years; harmonizing district codes to enable reliable joins.

### 2. Interactive Visualization (Shiny & Leaflet)
The `app.R` file creates the interactive dashboard, which allows users to:
* **Geospatially visualize** any variable (e.g., `Population`) on a `leaflet` map using `sf` shapefiles.
* **Dynamically filter** data by Year and District.
* **Display `ggplot2` charts** showing time-series trends or variable comparisons.
* **Export** all visualizations as `.png` files.

## Tech Stack

* **Language:** R, Python
* **Data Analysis (R):** `dplyr`, `tidyr`, `readxl`, `stringr`
* **Application (R):** `shiny`, `leaflet`, `sf`, `ggplot2`, `scales`
* **Reproducibility:** `here`
* **Deployment:** `rsconnect` (to shinyapps.io)
