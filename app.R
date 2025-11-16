# ENCODAGE : utf-8
# Author: Adam Bernabeu
# Date: 07/08/2025
# Project: Rshiny Display — Egypt district dashboards (robust read for Rmerged_output_census.xlsx)

library(shiny)
library(leaflet)
library(sf)
library(readxl)
library(dplyr)
library(stringr)
library(htmltools)
library(webshot2)
library(ggplot2)
library(tidyr)
library(mapview)
library(scales)
library(readr)
library(here)

`%||%` <- function(x, y) if (is.null(x)) y else x

# -------------------- Paths --------------------

shapefile_path <- here("data", "raw", "egy_admbnda_adm2_capmas_20170421", "egy_admbnda_adm2_capmas_20170421.shp")
excel_path     <- here("data", "final", "Rfinal_merge_land_census.xlsx")

# -------------------- Shapefile --------------------
gdf <- st_read(shapefile_path, quiet = TRUE) %>%
  rename(Code_1996 = ADM2_PCODE) %>%
  mutate(Code_1996 = stringr::str_match(Code_1996, "EG(\\d{4})")[,2],
         Code_1996 = as.character(Code_1996))

# -------------------- Data (robust read: text -> parse) --------------------
raw_data <- readxl::read_excel(excel_path, col_types = "text", na = c("", "NA")) %>%
  mutate(
    Year          = readr::parse_integer(Year),
    Serial_Number = readr::parse_integer(Serial_Number),
    Code_1996     = as.character(Code_1996),
    Code_1996     = str_replace(Code_1996, "^'", ""),
    Code_1996     = str_replace(Code_1996, "\\.0$", ""),
    Code_1996     = case_when(
      str_length(Code_1996) == 6 & str_ends(Code_1996, "00") ~ str_sub(Code_1996, 1, 4),
      str_length(Code_1996) == 5 & str_ends(Code_1996, "0")  ~ str_sub(Code_1996, 1, 4),
      TRUE ~ Code_1996
    ),
    Name          = as.character(Name)
  ) %>%
  {
    # Keep these non-numeric; everything else will be parsed to numeric
    keep_text <- c("Year","Serial_Number","Code_year","Code_1947","Code_1996",
                   "Name","Arabic_Name","Type","district_code_1960",
                   "gov","Consistency_check")
    num_cols <- setdiff(names(.), keep_text)
    mutate(., across(all_of(num_cols), ~ readr::parse_double(.x)))
  }

# -------------------- Variable choices --------------------
exclude_cols <- c("Year","Serial_Number","Code_year","Code_1947","Code_1996",
                  "Name","Arabic_Name","Type","district_code_1960","gov","Consistency_check","Code_1996-Name")
variable_choices <- names(raw_data)[!(names(raw_data) %in% exclude_cols) & sapply(raw_data, is.numeric)]
all_years <- sort(unique(raw_data$Year))

# -------------------- UI --------------------
ui <- fluidPage(
  # ---------- Header ----------
  tags$head(
    tags$style(HTML("
      .logo {
        height: 60px;  /* Définissez la hauteur que vous voulez */
        width: auto;     /* Laissez la largeur s'ajuster */
        margin: 10px;    /* Ajoutez un peu d'espace autour */
      }
      .logo-column {
        display: flex;
        align-items: center;
        justify-content: flex-end; /* Aligne les logos à droite */
      }
    "))
  ),
  div(class = "header-container",
      div(class = "header-text",
          h1("🗺️ Egypt historical demographics - Dashboard"),
          tags$p("Welcome on this interactive dashboard, allowing to visualize historical demographics in Egypt. 
                 You can choose any district and administrative code on a given year, obtain data, and visualize it."),
          tags$p(em("Adam Bernabeu - adam.bernabeu@gmail.com"),
                 style = "color:#555;")
      ),
      div(class = "logo-column",
          tags$img(src = "logo_WID.jpg", class = "logo"),
          tags$img(src = "logo_WIL.png", class = "logo"),
          tags$img(src = "logo_PSE.png", class = "logo")
      )
  ),
  # Selectors + buttons
  fluidRow(
    column(width = 12,
           tags$div(
             style = "display: flex; justify-content: center; gap: 25px; margin-top: 20px; align-items: flex-start;",
             selectInput("year", "Select Year:", choices = all_years, selected = min(all_years, na.rm = TRUE)),
             uiOutput("district_ui"),
             uiOutput("code_ui"),
             selectInput("vars", "Select variables for plots:",
                         choices = variable_choices,
                         selected = head(variable_choices, 3),
                         multiple = TRUE),
             selectInput("map_var", "Select variable for map color:",
                         choices = variable_choices,
                         selected = head(variable_choices, 1))
           ),
           # Code note (generic code + whether others exist)
           uiOutput("code_note"),
           tags$div(
             style = "display: flex; justify-content: center; gap: 12px; margin-top: 8px;",
             downloadButton("save_map_png",   "Save Map (PNG)"),
             downloadButton("save_aggline_png","Save Aggregate Line (PNG)"),
             downloadButton("save_trend_png", "Save District Trends (PNG)"),
             downloadButton("save_bar_png",   "Save District Bar (PNG)")
           )
    )
  ),
  
  # Map + plots
  fluidRow(
    column(width = 12,
           leafletOutput("map", height = "700px"),
           plotOutput("barplot",   height = "400px"),
           plotOutput("trendplot", height = "400px"),
           plotOutput("aggbarplot",height = "300px"))
  )
)

# -------------------- Server --------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(map_widget = NULL,
                       agg_plot   = NULL,
                       trend_plot = NULL,
                       bar_plot   = NULL)
  
  # District selector (by Name for chosen year)
  output$district_ui <- renderUI({
    req(input$year)
    data_year <- raw_data %>% filter(Year == input$year)
    district_choices <- sort(unique(data_year$Name))
    selectInput("district", "Select District:", choices = district_choices)
  })
  
  # Code selector (all distinct codes across all years for the selected Name)
  output$code_ui <- renderUI({
    req(input$district)
    codes_all_years <- raw_data %>%
      filter(Name == input$district) %>%
      mutate(Code_1996 = as.character(Code_1996)) %>%
      pull(Code_1996) %>% unique() %>% sort(na.last = TRUE)
    selectInput("code1996", "Select Code:",
                choices  = c("All codes" = "__ALL__", codes_all_years),
                selected = "__ALL__")
  })
  
  # Code note: generic (mode) + other codes if any
  output$code_note <- renderUI({
    req(input$district)
    codes_all <- raw_data %>%
      filter(Name == input$district, !is.na(Code_1996)) %>%
      pull(Code_1996)
    
    if (length(codes_all) == 0) {
      return(tags$div(style="text-align:center; margin-top:6px; font-size:12px; color:#444;",
                      HTML("<b>Code:</b> not available")))
    }
    
    tbl <- as.data.frame(table(codes_all), stringsAsFactors = FALSE)
    names(tbl) <- c("code","n")
    tbl <- tbl[order(-tbl$n, tbl$code), ]
    generic_code <- tbl$code[1]
    other_codes  <- sort(setdiff(unique(codes_all), generic_code))
    
    msg <- if (length(other_codes) == 0) {
      paste0("<b>Generic code:</b> ", generic_code, " &nbsp;·&nbsp; No other codes in other years")
    } else {
      paste0("<b>Generic code:</b> ", generic_code,
             " &nbsp;·&nbsp; Other codes in other years: ",
             paste(other_codes, collapse = ", "))
    }
    tags$div(style = "text-align:center; margin-top: 6px; font-size: 12px; color: #444;", HTML(msg))
  })
  
  # Helper: filter by Name + optional Code within chosen Year
  filter_by_name_and_code <- function(df, year, name, code_choice) {
    out <- df %>% filter(Year == year, Name == name)
    if (!is.null(code_choice) && code_choice != "__ALL__") {
      out <- out %>% filter(Code_1996 == code_choice)
    }
    out
  }
  
  # Variables available for the chosen year (must have some data)
  available_vars_year <- reactive({
    req(input$year)
    df_year <- raw_data %>% filter(Year == input$year)
    vars <- variable_choices[sapply(variable_choices, function(v) any(!is.na(df_year[[v]]) & df_year[[v]] != 0))]
    vars %||% character(0)
  })
  
  observeEvent(available_vars_year(), {
    vars_available <- available_vars_year()
    updateSelectInput(session, "vars",
                      choices  = vars_available,
                      selected = if (length(vars_available) >= 3) vars_available[1:3] else vars_available
    )
    updateSelectInput(session, "map_var",
                      choices  = vars_available,
                      selected = if (length(vars_available) > 0) vars_available[1] else character(0)
    )
  }, ignoreInit = TRUE)
  
  # -------------------- Map --------------------
  map_widget_reactive <- reactive({
    req(input$year, input$map_var)
    validate(need(input$map_var %in% names(raw_data), "Select a variable for the map."))
    
    data_year <- raw_data %>%
      filter(Year == input$year) %>%
      mutate(label_popup = paste0(
        "<b>District:</b> ", Name, "<br>",
        "<b>Code 1996:</b> ", Code_1996, "<br>",
        "<b>", input$map_var, ":</b> ", scales::comma(.data[[input$map_var]], accuracy = 1)
      ))
    
    merged <- gdf %>% left_join(data_year, by = "Code_1996") %>% filter(!is.na(Name))
    map_var_vals <- merged[[input$map_var]]
    pal <- colorNumeric("Reds", domain = map_var_vals, na.color = "#cccccc")
    
    leaflet(merged) %>%
      addProviderTiles("CartoDB.Positron") %>%
      fitBounds(lng1 = 29.8, lat1 = 30.4, lng2 = 32.3, lat2 = 31.6) %>%
      addPolygons(
        fillColor   = ~pal(map_var_vals),
        fillOpacity = 0.7,
        color       = "black",
        weight      = 0.5,
        label       = ~lapply(label_popup, htmltools::HTML),
        labelOptions = labelOptions(direction = "auto", style = list("font-size" = "12px")),
        layerId     = ~Code_1996,
        highlightOptions = highlightOptions(weight = 2, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright",
                pal = pal, values = map_var_vals,
                title = input$map_var,
                opacity = 0.7,
                labFormat = labelFormat(big.mark = ","))
  })
  
  output$map <- renderLeaflet({
    w <- map_widget_reactive()
    rv$map_widget <- w
    w
  })
  
  # Map click → update Name and Code selector; zoom to polygon
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(click$id, input$year)
    
    selected_row <- raw_data %>%
      filter(Year == input$year, Code_1996 == as.character(click$id)) %>%
      slice(1)
    
    if (nrow(selected_row) == 1) {
      updateSelectInput(session, "district", selected = selected_row$Name)
      
      codes_all_years <- raw_data %>%
        filter(Name == selected_row$Name) %>%
        mutate(Code_1996 = as.character(Code_1996)) %>%
        pull(Code_1996) %>% unique() %>% sort(na.last = TRUE)
      
      updateSelectInput(session, "code1996",
                        choices  = c("All codes"="__ALL__", codes_all_years),
                        selected = selected_row$Code_1996)
      
      data_year <- raw_data %>% filter(Year == input$year)
      merged <- gdf %>% left_join(data_year, by = "Code_1996") %>% filter(!is.na(Name))
      poly <- merged %>% filter(Code_1996 == as.character(click$id))
      if (nrow(poly) == 1) {
        bb <- sf::st_bbox(poly)
        leafletProxy("map") %>% fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
      }
    }
  })
  
  # -------------------- Aggregate line plot --------------------
  agg_line_plot <- reactive({
    req(input$map_var)
    agg_by_year <- raw_data %>%
      group_by(Year) %>%
      summarise(Value = sum(.data[[input$map_var]], na.rm = TRUE), .groups = "drop") %>%
      arrange(Year)
    validate(need(nrow(agg_by_year) > 0, "No data available to plot for this variable."))
    ggplot(agg_by_year, aes(x = Year, y = Value)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_text(aes(label = comma(Value)), vjust = -0.5, size = 3) +
      scale_x_continuous(breaks = unique(agg_by_year$Year)) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)), labels = comma) +
      theme_minimal() +
      labs(title = paste("Aggregate of", input$map_var, "(National level) by year"),
           x = "Year", y = "Sum across districts") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title  = element_text(size = 15, hjust = 0.5))
  })
  output$aggbarplot <- renderPlot({ rv$agg_plot <- agg_line_plot(); rv$agg_plot })
  
  # -------------------- District bar plot --------------------
  district_bar_plot <- reactive({
    req(input$vars, input$district, input$year)
    validate(need(length(input$vars) > 0, "Select at least one variable."))
    
    data_district <- filter_by_name_and_code(raw_data, input$year, input$district, input$code1996)
    validate(need(nrow(data_district) > 0, "Aucune donnée disponible pour cette sélection."))
    
    values <- as.numeric(unlist(dplyr::select(data_district, all_of(input$vars))[1, ]))
    bar_data <- data.frame(Category = input$vars, Value = values)
    
    ggplot(bar_data, aes(x = Category, y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = comma(Value)), vjust = -0.3, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)), labels = comma) +
      theme_minimal() +
      labs(title = paste("Data for", input$district,
                         if (!is.null(input$code1996) && input$code1996 != "__ALL__")
                           paste0("(Code ", input$code1996, ")") else "",
                         "in", input$year),
           y = "Value", x = "Variable") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 15, hjust= 0.5 ),
            legend.position = "none")
  })
  output$barplot <- renderPlot({ rv$bar_plot <- district_bar_plot(); rv$bar_plot })
  
  # -------------------- District trend plot --------------------
  district_trend_plot <- reactive({
    req(input$vars, input$district)
    validate(need(length(input$vars) > 0, "Select at least one variable."))
    
    df <- if (is.null(input$code1996) || input$code1996 == "__ALL__") {
      raw_data %>% filter(Name == input$district)
    } else {
      raw_data %>% filter(Name == input$district, Code_1996 == input$code1996)
    }
    
    data_trend <- df %>%
      select(Year, all_of(input$vars)) %>%
      pivot_longer(cols = input$vars, names_to = "Category", values_to = "Value")
    
    validate(need(nrow(data_trend) > 0, "No data for this selection."))
    
    data_trend_noNA <- data_trend %>%
      group_by(Category) %>%
      arrange(Year, .by_group = TRUE) %>%
      filter(!is.na(Value)) %>%
      ungroup()
    
    ggplot(data_trend_noNA, aes(x = Year, y = Value, color = Category, group = Category)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_text(aes(label = comma(Value)), vjust = -0.5, size = 3, color = "black") +
      scale_x_continuous(breaks = sort(unique(raw_data$Year))) +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      labs(title = paste("Trends for", input$district,
                         if (!is.null(input$code1996) && input$code1996 != "__ALL__")
                           paste0("(Code ", input$code1996, ")") else ""),
           y = "Value", x = "Year") +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 15, hjust= 0.5 ))
  })
  output$trendplot <- renderPlot({ rv$trend_plot <- district_trend_plot(); rv$trend_plot })
  
  # -------------------- Downloads --------------------
  output$save_map_png <- downloadHandler(
    filename = function() { sprintf("map_%s_%s.png", input$year, input$map_var) },
    content  = function(file) {
      req(rv$map_widget)
      mapview::mapshot(rv$map_widget, file = file, vwidth = 1400, vheight = 900, delay = 0.5)
    }
  )
  output$save_aggline_png <- downloadHandler(
    filename = function() { sprintf("aggregate_%s.png", input$map_var) },
    content  = function(file) {
      gg <- isolate(rv$agg_plot %||% agg_line_plot())
      ggplot2::ggsave(file, gg, width = 10, height = 6, dpi = 300)
    }
  )
  output$save_trend_png <- downloadHandler(
    filename = function() { sprintf("trends_%s_%s.png", input$district, input$year) },
    content  = function(file) {
      gg <- isolate(rv$trend_plot %||% district_trend_plot())
      ggplot2::ggsave(file, gg, width = 10, height = 6, dpi = 300)
    }
  )
  output$save_bar_png <- downloadHandler(
    filename = function() { sprintf("bar_%s_%s.png", input$district, input$year) },
    content  = function(file) {
      gg <- isolate(rv$bar_plot %||% district_bar_plot())
      ggplot2::ggsave(file, gg, width = 10, height = 6, dpi = 300)
    }
  )
}

# -------------------- Run app --------------------
shinyApp(ui, server)
