# Generate all GEC theme examples
# This script creates comprehensive examples showcasing the GEC theme

library(ggplot2)
library(magick)
library(gt)
library(dplyr)

# Source theme files from parent directory
source("../../gec_theme.R")
source("../../gec_gt_theme.R")

# Ensure output directory exists
dir.create("../output", showWarnings = FALSE)

# ============================================================================
# GGPLOT2 EXAMPLES WITH LOGO STRIPS
# ============================================================================

# Example 1: Market Share Bar Chart
create_market_share_example <- function() {
  data <- data.frame(
    category = c("Mobile", "Console", "PC", "VR", "Cloud"),
    value = c(45, 32, 18, 3, 2),
    stringsAsFactors = FALSE
  )
  
  p <- ggplot(data, aes(x = reorder(category, value), y = value)) +
    geom_col(fill = gec_colors$accent, width = 0.7) +
    geom_text(aes(label = paste0(value, "%")),
              hjust = -0.2,
              color = gec_colors$secondary,
              size = 4,
              fontface = "bold") +
    coord_flip() +
    scale_y_continuous(expand = c(0, 0, 0.1, 0), limits = c(0, max(data$value) * 1.1)) +
    labs(
      title = "GAMING PLATFORM MARKET SHARE 2024",
      subtitle = "Mobile gaming dominates with nearly half of the global market",
      x = NULL,
      y = NULL
    ) +
    theme_gec(border = FALSE)
  
  # Create container with yellow border and logo strip
  img <- create_gec_container(
    p,
    logo_strip = TRUE,
    strip_color = gec_colors$primary,
    container_border = TRUE,
    border_color = gec_colors$primary,
    width = 1000,
    height = 600
  )
  
  image_write(img, "../output/01_market_share.png")
  message("Created: 01_market_share.png")
}

# Example 2: Time Series with Multiple Metrics
create_time_series_example <- function() {
  set.seed(42)
  dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)
  
  data <- data.frame(
    date = rep(dates, 3),
    metric = rep(c("Revenue", "Users", "ARPU"), each = 12),
    value = c(
      100 * (1 + cumsum(rnorm(12, 0.02, 0.05))),  # Revenue
      100 * (1 + cumsum(rnorm(12, 0.03, 0.03))),  # Users
      100 * (1 + cumsum(rnorm(12, -0.01, 0.02)))  # ARPU
    )
  )
  
  p <- ggplot(data, aes(x = date, y = value, color = metric)) +
    geom_line(linewidth = 2) +
    geom_point(size = 3) +
    scale_color_manual(values = c(
      "Revenue" = gec_colors$accent,
      "Users" = gec_colors$green,
      "ARPU" = gec_colors$secondary
    )) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, max(data$value) * 1.1), breaks = scales::pretty_breaks()) +
    scale_x_date(date_labels = "%b", date_breaks = "2 months") +
    labs(
      title = "KEY METRICS GROWTH TREND 2024",
      subtitle = "Revenue and user growth outpacing ARPU decline",
      x = NULL,
      y = NULL,
      color = "Metric"
    ) +
    theme_gec(border = FALSE)
  
  img <- create_gec_container(
    p,
    logo_strip = TRUE,
    strip_color = gec_colors$primary,
    container_border = TRUE,
    border_color = gec_colors$primary,
    width = 1000,
    height = 600
  )
  
  image_write(img, "../output/02_time_series.png")
  message("Created: 02_time_series.png")
}

# Example 3: Stacked Area Chart
create_stacked_area_example <- function() {
  quarters <- c("Q1'23", "Q2'23", "Q3'23", "Q4'23",
                "Q1'24", "Q2'24", "Q3'24", "Q4'24")
  
  data <- data.frame(
    quarter = rep(quarters, 4),
    platform = rep(c("iOS", "Android", "Steam", "Other"), each = 8),
    revenue = c(
      32, 35, 38, 42, 45, 48, 52, 55,  # iOS
      28, 30, 32, 35, 38, 41, 44, 48,  # Android
      15, 16, 18, 20, 22, 23, 24, 26,  # Steam
      8, 8, 9, 10, 11, 12, 13, 14      # Other
    )
  )
  
  data$quarter <- factor(data$quarter, levels = quarters)
  data$platform <- factor(data$platform, levels = c("Other", "Steam", "Android", "iOS"))  # Order for stacking
  
  p <- ggplot(data, aes(x = quarter, y = revenue, fill = platform, group = platform)) +
    geom_area(alpha = 0.8, position = "stack") +
    scale_fill_manual(values = c(
      "iOS" = gec_colors$accent,
      "Android" = gec_colors$green,
      "Steam" = gec_colors$grey_dark,
      "Other" = gec_colors$grey_light
    )) +
    scale_y_continuous(labels = function(x) paste0("$", x, "B"), limits = c(0, max(data$revenue) * 1.1), breaks = scales::pretty_breaks()) +
    labs(
      title = "QUARTERLY REVENUE BY PLATFORM",
      subtitle = "Steady growth across all platforms with mobile leading",
      x = NULL,
      y = NULL,
      fill = "Platform"
    ) +
    theme_gec(border = FALSE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  img <- create_gec_container(
    p,
    logo_strip = TRUE,
    strip_color = gec_colors$primary,
    container_border = TRUE,
    border_color = gec_colors$primary,
    width = 1000,
    height = 600
  )
  
  image_write(img, "../output/03_stacked_area.png")
  message("Created: 03_stacked_area.png")
}

# Example 4: Scatter Plot with Regression
create_scatter_example <- function() {
  set.seed(42)
  data <- data.frame(
    user_acquisition_cost = runif(50, 0.5, 5),
    lifetime_value = runif(50, 2, 20)
  )
  data$lifetime_value <- data$user_acquisition_cost * runif(50, 2, 5) +
    rnorm(50, 0, 2)
  data$profitable <- data$lifetime_value > data$user_acquisition_cost * 2
  
  p <- ggplot(data, aes(x = user_acquisition_cost, y = lifetime_value)) +
    geom_abline(slope = 2, intercept = 0,
                linetype = "dashed",
                color = gec_colors$secondary,
                linewidth = 1) +
    geom_point(aes(color = profitable), size = 4, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = gec_colors$accent) +
    scale_color_manual(values = c(
      "TRUE" = gec_colors$green,
      "FALSE" = gec_colors$grey_light
    ),
    labels = c("TRUE" = "Profitable", "FALSE" = "Unprofitable")) +
    scale_x_continuous(labels = function(x) paste0("$", x)) +
    scale_y_continuous(labels = function(x) paste0("$", x), limits = c(0, max(data$lifetime_value) * 1.1), breaks = scales::pretty_breaks()) +
    labs(
      title = "USER ACQUISITION VS LIFETIME VALUE",
      subtitle = "Targeting 2x LTV/CAC ratio for profitability",
      x = "User Acquisition Cost",
      y = "Lifetime Value",
      color = "Status"
    ) +
    theme_gec(border = FALSE)
  
  img <- create_gec_container(
    p,
    logo_strip = TRUE,
    strip_color = gec_colors$primary,
    container_border = TRUE,
    border_color = gec_colors$primary,
    width = 1000,
    height = 600
  )
  
  image_write(img, "../output/04_scatter_ltv_cac.png")
  message("Created: 04_scatter_ltv_cac.png")
}

# Example 5: Faceted Chart
create_faceted_example <- function() {
  data <- expand.grid(
    genre = c("Action", "RPG", "Strategy", "Casual"),
    region = c("NA", "EU", "ASIA"),
    stringsAsFactors = FALSE
  )
  
  set.seed(42)
  data$revenue <- round(runif(12, 50, 200), 1)
  
  p <- ggplot(data, aes(x = genre, y = revenue, fill = genre)) +
    geom_col(width = 0.7) +
    facet_wrap(~region, ncol = 3) +
    scale_fill_manual(values = c(
      "Action" = gec_colors$accent,
      "RPG" = gec_colors$green,
      "Strategy" = gec_colors$secondary,
      "Casual" = gec_colors$grey_light
    )) +
    scale_y_continuous(labels = function(x) paste0("$", x, "M"), limits = c(0, max(data$revenue) * 1.1), breaks = scales::pretty_breaks()) +
    labs(
      title = "REVENUE BY GENRE AND REGION",
      subtitle = "Regional preferences vary significantly across game genres",
      x = NULL,
      y = NULL
    ) +
    theme_gec(border = FALSE) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_rect(fill = gec_colors$primary),
      strip.text = element_text(color = gec_colors$grey_dark, face = "bold")
    )
  
  img <- create_gec_container(
    p,
    logo_strip = TRUE,
    strip_color = gec_colors$primary,
    container_border = TRUE,
    border_color = gec_colors$primary,
    width = 1200,
    height = 600
  )
  
  image_write(img, "../output/05_faceted.png")
  message("Created: 05_faceted.png")
}

# Example 6: Horizontal Bar Chart with Categories
create_horizontal_bar_example <- function() {
  data <- data.frame(
    game = c("Fortnite", "Roblox", "Minecraft", "PUBG Mobile", 
             "Genshin Impact", "Candy Crush", "Pokemon GO", "Honor of Kings"),
    revenue = c(520, 480, 290, 280, 450, 320, 380, 510),
    category = c("Battle Royale", "Sandbox", "Sandbox", "Battle Royale",
                 "RPG", "Puzzle", "AR", "MOBA")
  )
  
  p <- ggplot(data, aes(x = reorder(game, revenue), y = revenue, 
                        fill = category)) +
    geom_col(width = 0.8) +
    geom_text(aes(label = paste0("$", revenue, "M")),
              hjust = -0.1,
              color = gec_colors$secondary,
              size = 3.5,
              fontface = "bold") +
    coord_flip() +
    scale_fill_manual(values = c(
      "Battle Royale" = gec_colors$accent,
      "Sandbox" = gec_colors$green,
      "RPG" = gec_colors$secondary,
      "Puzzle" = gec_colors$grey_light,
      "AR" = "#4A90A4",
      "MOBA" = "#67B26F"
    )) +
    scale_y_continuous(expand = c(0, 0, 0.15, 0), limits = c(0, max(data$revenue) * 1.15), breaks = scales::pretty_breaks()) +
    labs(
      title = "TOP MOBILE GAMES BY REVENUE",
      subtitle = "Monthly revenue across different game genres (USD)",
      x = NULL,
      y = NULL,
      fill = "Genre"
    ) +
    theme_gec(border = FALSE) +
    guides(fill = guide_legend(nrow = 1))  # Force single row legend
  
  img <- create_gec_container(
    p,
    logo_strip = TRUE,
    strip_color = gec_colors$primary,
    container_border = TRUE,
    border_color = gec_colors$primary,
    width = 1000,
    height = 700
  )
  
  image_write(img, "../output/06_horizontal_bars.png")
  message("Created: 06_horizontal_bars.png")
}

# ============================================================================
# GT TABLE EXAMPLES - Commented out as we only want PNG outputs
# ============================================================================

# Example 7: Basic GT Table with Theme
# create_gt_basic_example <- function() {
  data <- data.frame(
    Platform = c("Mobile", "Console", "PC", "VR"),
    Revenue = c(93.2, 50.4, 36.7, 6.2),
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
    fmt_currency(Revenue, decimals = 1, suffixing = TRUE) %>%
    fmt_percent(Growth, scale_values = FALSE, decimals = 1) %>%
    fmt_number(Users, decimals = 0) %>%
    tab_style(
      style = style_gec_positive(),
      locations = cells_body(
        columns = Growth,
        rows = Growth > 10
      )
    ) %>%
    tab_style(
      style = style_gec_negative(),
      locations = cells_body(
        columns = Growth,
        rows = Growth < 0
      )
    )
  
#   gtsave(table, "../output/07_gt_basic_table.html")
#   message("Created: 07_gt_basic_table.html")
# }

# Example 8: Complex GT Table with Groups
# create_gt_complex_example <- function() {
  data <- data.frame(
    Region = c("North America", "North America", "Europe", "Europe", 
               "Asia", "Asia"),
    Platform = c("Mobile", "Console", "Mobile", "Console", 
                 "Mobile", "Console"),
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
  
#   gtsave(table, "../output/08_gt_complex_table.html")
#   message("Created: 08_gt_complex_table.html")
# }

# ============================================================================
# RUN ALL EXAMPLES
# ============================================================================

message("\n=== Generating GEC Theme Examples ===\n")

# ggplot2 examples
message("Creating ggplot2 examples...")
create_market_share_example()
create_time_series_example()
create_stacked_area_example()
create_scatter_example()
create_faceted_example()
create_horizontal_bar_example()

# GT table examples - commented out as we only want PNG outputs
# message("\nCreating GT table examples...")
# create_gt_basic_example()
# create_gt_complex_example()

message("\n=== All examples generated successfully! ===")
message("Check the ../output/ directory for results.")