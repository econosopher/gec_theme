# Test script for GEC theme
# Tests new colors, fonts, and logo functionality

library(ggplot2)
source("../gec_theme.R")

# Create output directory
dir.create("output", showWarnings = FALSE)

# Test 1: Basic bar chart with new colors
test_basic_bar <- function() {
  data <- data.frame(
    category = c("Mobile", "Console", "PC", "VR"),
    value = c(45, 32, 18, 5)
  )
  
  p <- ggplot(data, aes(x = reorder(category, value), y = value)) +
    geom_col(fill = gec_colors$primary, width = 0.7) +
    coord_flip() +
    labs(
      title = "GAMING PLATFORM MARKET SHARE",
      subtitle = "Mobile gaming continues to dominate the market",
      caption = "Game Economist Consulting"
    ) +
    theme_gec()
  
  # Save with default logo
  img_default <- create_gec_container(p, width = 1000, height = 600)
  image_write(img_default, "output/test_basic_bar_default.png")
  
  # Save with alternate logo (if available)
  img_alternate <- create_gec_container(p, width = 1000, height = 600, 
                                       logo_type = "alternate")
  image_write(img_alternate, "output/test_basic_bar_alternate.png")
  
  # Save without logo strip
  img_no_logo <- create_gec_container(p, width = 1000, height = 600, 
                                      logo_strip = FALSE)
  image_write(img_no_logo, "output/test_basic_bar_no_logo.png")
}

# Test 2: Multi-color chart
test_multi_color <- function() {
  data <- data.frame(
    platform = rep(c("iOS", "Android", "Steam", "Epic"), each = 3),
    quarter = rep(c("Q1", "Q2", "Q3"), 4),
    revenue = c(120, 135, 142, 98, 105, 112, 85, 92, 95, 45, 52, 58)
  )
  
  p <- ggplot(data, aes(x = quarter, y = revenue, fill = platform)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_gec("full") +
    labs(
      title = "QUARTERLY REVENUE BY PLATFORM",
      subtitle = "iOS maintains lead across all quarters",
      fill = "Platform"
    ) +
    theme_gec()
  
  img <- create_gec_container(p, width = 1200, height = 700)
  image_write(img, "output/test_multi_color.png")
}

# Test 3: Line chart with new colors
test_line_chart <- function() {
  set.seed(42)
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day")
  data <- data.frame(
    date = rep(dates, 3),
    metric = rep(c("DAU", "Revenue", "ARPDAU"), each = length(dates)),
    value = c(
      1000 + cumsum(rnorm(length(dates), 5, 20)),
      5000 + cumsum(rnorm(length(dates), 10, 50)),
      5 + cumsum(rnorm(length(dates), 0.01, 0.1))
    )
  )
  
  p <- ggplot(data, aes(x = date, y = value, color = metric)) +
    geom_line(linewidth = 1.2) +
    scale_color_gec("main") +
    facet_wrap(~metric, scales = "free_y", ncol = 1) +
    labs(
      title = "Q1 2024 KEY METRICS TREND",
      subtitle = "Daily active users and revenue showing positive growth"
    ) +
    theme_gec() +
    theme(legend.position = "none")
  
  img <- create_gec_container(p, width = 1000, height = 900)
  image_write(img, "output/test_line_chart.png")
}

# Test 4: Different border colors
test_border_colors <- function() {
  data <- data.frame(
    x = 1:5,
    y = c(10, 25, 18, 32, 28)
  )
  
  p <- ggplot(data, aes(x = x, y = y)) +
    geom_point(size = 8, color = gec_colors$accent) +
    geom_line(linewidth = 2, color = gec_colors$accent) +
    labs(
      title = "PLAYER RETENTION CURVE",
      subtitle = "Day 1 to Day 5 retention rates"
    ) +
    theme_gec()
  
  # Test different border colors
  borders <- list(
    primary = gec_colors$primary,
    secondary = gec_colors$secondary,
    accent = gec_colors$accent,
    green = gec_colors$green
  )
  
  for (name in names(borders)) {
    img <- create_gec_container(p, width = 800, height = 500,
                               border_color = borders[[name]])
    image_write(img, paste0("output/test_border_", name, ".png"))
  }
}

# Test 5: Color palette showcase
test_color_showcase <- function() {
  # Create a data frame to showcase all colors
  color_data <- data.frame(
    color_name = c("Primary", "Secondary", "Accent", "Green", "White", "Grey Light"),
    color_value = c(gec_colors$primary, gec_colors$secondary, 
                   gec_colors$accent, gec_colors$green,
                   gec_colors$white, gec_colors$grey_light),
    y = 1:6
  )
  
  p <- ggplot(color_data, aes(x = 1, y = y, fill = color_value)) +
    geom_tile(width = 0.8, height = 0.8) +
    geom_text(aes(label = paste(color_name, "\n", color_value)), 
              color = c("black", "white", "white", "white", "black", "black"),
              size = 5, fontface = "bold") +
    scale_fill_identity() +
    xlim(0.5, 1.5) +
    labs(
      title = "GEC BRAND COLOR PALETTE",
      subtitle = "Official colors for all visualizations"
    ) +
    theme_gec() +
    theme(
      axis.text = element_blank(),
      panel.grid = element_blank()
    )
  
  img <- create_gec_container(p, width = 800, height = 800)
  image_write(img, "output/test_color_showcase.png")
}

# Run all tests
cat("Running GEC theme tests...\n")
cat("Testing basic bar chart...\n")
test_basic_bar()

cat("Testing multi-color chart...\n")
test_multi_color()

cat("Testing line chart...\n")
test_line_chart()

cat("Testing border colors...\n")
test_border_colors()

cat("Testing color showcase...\n")
test_color_showcase()

cat("\nAll tests complete! Check the test/output/ directory for results.\n")