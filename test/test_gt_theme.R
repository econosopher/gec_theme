# Test script for GEC GT table theme
# Tests new colors and fonts

library(gt)
library(dplyr)
source("../gec_gt_theme.R")

# Create output directory
dir.create("output", showWarnings = FALSE)

# Test 1: Basic table with new colors
test_basic_table <- function() {
  data <- data.frame(
    Category = c("Mobile", "Console", "PC", "VR"),
    Revenue = c(45.2, 32.1, 18.7, 4.0),
    Growth = c(12.3, -2.1, 5.4, 45.2),
    Users = c(2500, 1200, 800, 150)
  )
  
  table <- data %>%
    gt() %>%
    theme_gec_gt(weight_strategy = "light", line_thickness = "regular") %>%
    tab_header(
      title = "GAMING PLATFORM METRICS",
      subtitle = "Q4 2024 Performance Summary"
    ) %>%
    fmt_currency(Revenue, decimals = 1) %>%
    fmt_percent(Growth, scale_values = FALSE, decimals = 1) %>%
    fmt_number(Users, decimals = 0)
  
  # Save as HTML
  gtsave(table, "output/test_basic_table.html")
}

# Test 2: Table with conditional formatting
test_conditional_table <- function() {
  data <- data.frame(
    Game = c("Fortnite", "Roblox", "Minecraft", "PUBG", "Apex Legends"),
    DAU = c(8.5, 12.3, 6.2, 4.1, 3.8),
    Revenue = c(520, 380, 290, 180, 150),
    ARPDAU = c(61.2, 30.9, 46.8, 43.9, 39.5),
    Retention_D7 = c(45, 52, 48, 38, 35)
  )
  
  table <- data %>%
    gt() %>%
    theme_gec_gt(weight_strategy = "light", line_thickness = "regular") %>%
    tab_header(
      title = "TOP MOBILE GAMES PERFORMANCE",
      subtitle = "Daily metrics and monetization"
    ) %>%
    fmt_number(DAU, decimals = 1, suffixing = TRUE) %>%
    fmt_currency(Revenue, decimals = 0) %>%
    fmt_currency(ARPDAU, decimals = 2) %>%
    fmt_percent(Retention_D7, scale_values = FALSE, decimals = 0) %>%
    tab_style(
      style = style_gec_positive(),
      locations = cells_body(
        columns = ARPDAU,
        rows = ARPDAU > 50
      )
    ) %>%
    tab_style(
      style = style_gec_negative(),
      locations = cells_body(
        columns = Retention_D7,
        rows = Retention_D7 < 40
      )
    )
  
  gtsave(table, "output/test_conditional_table.html")
}

# Test 3: Table with different border options
test_border_options <- function() {
  data <- data.frame(
    Platform = c("iOS", "Android", "Steam"),
    Users = c(450, 680, 120),
    Revenue = c(12.5, 15.8, 8.2)
  )
  
  # No border
  table_no_border <- data %>%
    gt() %>%
    theme_gec_gt(container_border = FALSE, weight_strategy = "light", line_thickness = "regular") %>%
    tab_header(title = "NO BORDER EXAMPLE") %>%
    fmt_number(Users) %>%
    fmt_currency(Revenue, decimals = 1)
  
  # Custom border color
  table_custom_border <- data %>%
    gt() %>%
    theme_gec_gt(
      container_border = TRUE,
      container_border_color = gec_colors$accent,
      container_border_width = 5,
      weight_strategy = "light",
      line_thickness = "regular"
    ) %>%
    tab_header(title = "CUSTOM BORDER COLOR") %>%
    fmt_number(Users) %>%
    fmt_currency(Revenue, decimals = 1)
  
  gtsave(table_no_border, "output/test_table_no_border.html")
  gtsave(table_custom_border, "output/test_table_custom_border.html")
}

# Test 4: Complex table with groups
test_complex_table <- function() {
  data <- data.frame(
    Region = c("North America", "North America", "Europe", "Europe", "Asia", "Asia"),
    Platform = c("Mobile", "Console", "Mobile", "Console", "Mobile", "Console"),
    Q1 = c(125, 89, 98, 76, 145, 45),
    Q2 = c(132, 92, 102, 78, 158, 48),
    Q3 = c(138, 88, 105, 80, 167, 52),
    Q4 = c(145, 95, 112, 85, 178, 58)
  )
  
  table <- data %>%
    gt(groupname_col = "Region") %>%
    theme_gec_gt(weight_strategy = "light", line_thickness = "regular") %>%
    tab_header(
      title = "REGIONAL GAMING REVENUE BY QUARTER",
      subtitle = "2024 performance in millions USD"
    ) %>%
    fmt_currency(c(Q1, Q2, Q3, Q4), decimals = 0) %>%
    tab_spanner(
      label = "Quarterly Revenue",
      columns = c(Q1, Q2, Q3, Q4)
    ) %>%
    grand_summary_rows(
      columns = c(Q1, Q2, Q3, Q4),
      fns = list(Total = ~sum(.)),
      fmt = ~fmt_currency(., decimals = 0)
    )
  
  gtsave(table, "output/test_complex_table.html")
}

# Test 5: Color showcase table
test_color_showcase <- function() {
  # Create data showing all theme colors
  color_data <- data.frame(
    Color_Name = c("Primary", "Secondary", "Accent", "Green", "White", "Grey Light"),
    Hex_Code = c(gec_colors$primary, gec_colors$secondary, 
                 gec_colors$accent, gec_colors$green,
                 gec_colors$white, gec_colors$grey_light),
    Usage = c("Main brand color", "Text and headers", "Highlights", 
              "Positive values", "Background", "Stripes and borders")
  )
  
  table <- color_data %>%
    gt() %>%
    theme_gec_gt(weight_strategy = "light", line_thickness = "regular") %>%
    tab_header(
      title = "GEC BRAND COLOR PALETTE",
      subtitle = "Official colors for tables and visualizations"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = gec_colors$primary),
        cell_text(color = gec_colors$secondary, weight = "bold")
      ),
      locations = cells_body(rows = Color_Name == "Primary")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = gec_colors$secondary),
        cell_text(color = gec_colors$white, weight = "bold")
      ),
      locations = cells_body(rows = Color_Name == "Secondary")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = gec_colors$accent),
        cell_text(color = gec_colors$white, weight = "bold")
      ),
      locations = cells_body(rows = Color_Name == "Accent")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = gec_colors$green),
        cell_text(color = gec_colors$white, weight = "bold")
      ),
      locations = cells_body(rows = Color_Name == "Green")
    )
  
  gtsave(table, "output/test_color_showcase_table.html")
}

# Run all tests
message("Running GEC GT theme tests...")
message("Testing basic table...")
test_basic_table()

message("Testing conditional formatting...")
test_conditional_table()

message("Testing border options...")
test_border_options()

message("Testing complex table...")
test_complex_table()

message("Testing color showcase...")
test_color_showcase()

message("\nAll GT table tests complete! Check the test/output/ directory for results.")