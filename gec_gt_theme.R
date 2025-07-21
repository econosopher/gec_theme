# Deconstructor of Fun GT Table Theme
# Custom GT table styling for GEC podcast

library(gt)
library(dplyr)

# Source the main GEC theme for colors
if (file.exists("gec_theme.R")) {
  source("gec_theme.R")
} else {
  # Define colors if main theme not available
  gec_colors <- list(
    primary = "#E4F577",      # Bright Yellow-Green
    secondary = "#363D46",    # Dark Grey
    accent = "#23648D",       # Blue
    green = "#2D6F31",        # Green
    white = "#FCFFFE",        # Off-White
    black = "#000000",        # Black
    grey_light = "#DCDCDC",   # Light Grey
    grey_dark = "#363D46"     # Same as secondary
  )
  
  # Font setup - GEC font hierarchy with CSS-compatible font stacks
  gec_font_title <- "'Monument Extended', 'Arial Black', 'Helvetica Neue', sans-serif"
  gec_font_subtitle <- "'League Spartan', 'Arial', 'Helvetica Neue', sans-serif" 
  gec_font_body <- "'Monument Extended', 'Helvetica Neue', 'Arial', sans-serif"
  
  # Temporarily disabled custom fonts to avoid crashes
  # TODO: Re-enable once font compatibility issues are resolved
  
  # Use body font as default for tables
  gec_font_family <- gec_font_body
}

# Helper function to find GEC icon path
find_gec_icon_path <- function() {
  search_dirs <- c(
    "../images/Icon",
    "../Images/Icon", 
    "../images",
    "../Images",
    "images/Icon",
    "Images/Icon",
    "images",
    "Images"
  )
  
  for (dir in search_dirs) {
    if (dir.exists(dir)) {
      files <- list.files(dir, pattern = "*Primary*|*primary*", ignore.case = TRUE, full.names = TRUE)
      if (length(files) > 0) {
        return(files[1])
      }
    }
  }
  return("")
}

# GEC GT theme function
theme_gec_gt <- function(gt_table, 
                         header_bg = gec_colors$secondary,
                         header_text = gec_colors$white,
                         stripe_color = gec_colors$grey_light,
                         border_color = gec_colors$grey_light,
                         text_color = gec_colors$secondary,
                         container_border = TRUE,
                         container_border_color = gec_colors$primary,
                         container_border_width = 3) {
  
  result_table <- gt_table %>%
    # Table options
    tab_options(
      # Font settings - use Poppins for table body
      table.font.names = gec_font_body,  # Full font stack
      table.font.size = px(12),
      
      # Reduce heading padding
      heading.padding = px(2),
      heading.border.bottom.style = "none",
      
      # Header styling
      column_labels.background.color = header_bg,
      column_labels.font.weight = "bold",
      column_labels.font.size = px(13),
      
      # Table body
      table.background.color = gec_colors$white,
      data_row.padding = px(8),
      
      # Borders
      table.border.top.style = "solid",
      table.border.top.width = if (container_border) px(container_border_width) else px(2),
      table.border.top.color = if (container_border) container_border_color else header_bg,
      table.border.bottom.style = "solid",
      table.border.bottom.width = if (container_border) px(container_border_width) else px(2),
      table.border.bottom.color = if (container_border) container_border_color else header_bg,
      table.border.left.style = if (container_border) "solid" else "none",
      table.border.left.width = if (container_border) px(container_border_width) else px(0),
      table.border.left.color = if (container_border) container_border_color else "transparent",
      table.border.right.style = if (container_border) "solid" else "none",
      table.border.right.width = if (container_border) px(container_border_width) else px(0),
      table.border.right.color = if (container_border) container_border_color else "transparent",
      
      # Row striping
      row.striping.background_color = stripe_color,
      row.striping.include_table_body = TRUE,
      
      # Source notes
      source_notes.font.size = px(10),
      source_notes.padding = px(4),
      
      # Footnotes
      footnotes.font.size = px(10),
      footnotes.padding = px(4)
    ) %>%
    
    # Style column labels with Poppins
    tab_style(
      style = list(
        cell_text(
          color = header_text,
          weight = "bold",
          font = gec_font_body  # Poppins font stack for headers
        )
      ),
      locations = cells_column_labels(everything())
    ) %>%
    
    # Style table body with Poppins
    tab_style(
      style = list(
        cell_text(
          color = text_color,
          font = gec_font_body  # Poppins font stack for data
        )
      ),
      locations = cells_body()
    )
}

# GEC GT color functions for conditional formatting
style_gec_positive <- function() {
  list(
    cell_fill(color = gec_colors$primary),
    cell_text(color = gec_colors$secondary, weight = "bold")
  )
}

style_gec_negative <- function() {
  list(
    cell_fill(color = gec_colors$accent),
    cell_text(color = gec_colors$white, weight = "bold")
  )
}

style_gec_highlight <- function() {
  list(
    cell_fill(color = gec_colors$green),
    cell_text(color = gec_colors$white, weight = "bold")
  )
}

# Example GEC GT table
create_gec_example_table <- function() {
  # Check for required packages
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required. Install with: install.packages('gt')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install with: install.packages('dplyr')")
  }
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required. Install with: install.packages('htmltools')")
  }
  
  # Sample gaming data
  gaming_table_data <- data.frame(
    Platform = c("Mobile", "Console", "PC", "VR", "Cloud Gaming"),
    `Revenue (Billions)` = c(93.2, 50.4, 36.7, 6.2, 3.7),
    `Growth Rate (%)` = c(7.3, 2.1, -2.8, 31.1, 74.9),
    `Market Share (%)` = c(49.0, 26.5, 19.3, 3.3, 1.9),
    Category = c("Traditional", "Traditional", "Traditional", "Emerging", "Emerging"),
    check.names = FALSE
  )
  
  # Create GT table
  gt_table <- gaming_table_data %>%
    gt() %>%
    
    # Apply GEC theme  
    theme_gec_gt() %>%
    
    # Add title and subtitle (538-style bold, uppercase)
    tab_header(
      title = md("**GAMING PLATFORM PERFORMANCE 2024**"),
      subtitle = "Revenue, growth, and market share analysis"
    ) %>%
    
    # Style title with Monument Extended (bold, uppercase, left-aligned) - matching charts
    tab_style(
      style = list(
        cell_text(
          font = gec_font_title,      # Monument Extended font stack
          weight = "normal",
          size = px(18),              # Proportional to table size
          color = gec_colors$secondary,
          align = "left"
        ),
        cell_fill(color = "white")
      ),
      locations = cells_title("title")
    ) %>%
    
    # Style subtitle with League Spartan (left-aligned) - matching charts
    tab_style(
      style = list(
        cell_text(
          font = gec_font_subtitle,    # League Spartan font stack
          size = px(13),               # Match chart subtitle size (base 12 * 1.1)
          color = gec_colors$grey_dark,
          align = "left"
        ),
        cell_fill(color = "white")
      ),
      locations = cells_title("subtitle")
    ) %>%
    
    # Format numbers
    fmt_currency(
      columns = `Revenue (Billions)`,
      currency = "USD",
      scale_by = 1e-9,
      suffixing = TRUE,
      decimals = 1
    ) %>%
    
    fmt_percent(
      columns = c(`Growth Rate (%)`, `Market Share (%)`),
      scale_values = FALSE,
      decimals = 1
    ) %>%
    
    # Conditional formatting
    tab_style(
      style = style_gec_positive(),
      locations = cells_body(
        columns = `Growth Rate (%)`,
        rows = `Growth Rate (%)` > 10
      )
    ) %>%
    
    tab_style(
      style = style_gec_negative(),
      locations = cells_body(
        columns = `Growth Rate (%)`,
        rows = `Growth Rate (%)` < 0
      )
    ) %>%
    
    # Add source note without image for better PNG compatibility
    tab_source_note(
      source_note = "Game Economist Consulting • Sensor Tower Data"
    )
    
  # Add custom CSS to ensure fonts are properly applied
  base_css <- "
    .gt_title {
      font-family: 'Monument Extended', 'Arial Black', 'Helvetica Neue', sans-serif !important;
      padding-bottom: 2px !important;
      margin-bottom: 0px !important;
    }
    .gt_subtitle {
      font-family: 'League Spartan', 'Arial', 'Helvetica Neue', sans-serif !important;
      padding-top: 0px !important;
      margin-top: 0px !important;
    }
    .gt_heading {
      padding-bottom: 5px !important;
    }
    .gt_col_heading, .gt_column_spanner {
      font-family: 'Monument Extended', 'Helvetica Neue', 'Arial', sans-serif !important;
    }
    .gt_sourcenote, .gt_row {
      font-family: 'Monument Extended', 'Helvetica Neue', 'Arial', sans-serif !important;
    }
    table, .gt_table {
      font-family: 'Monument Extended', 'Helvetica Neue', 'Arial', sans-serif !important;
    }
    /* Source note styling to match charts */
    .gt_sourcenote {
      color: #363D46 !important;
      font-weight: 500 !important;
      text-align: right !important;
    }
    /* Ensure font loading fallbacks work */
    @import url('https://fonts.googleapis.com/css2?family=League+Spartan:wght@400;500;600;700&display=swap');
  "
  
  gt_table <- gt_table %>%
    opt_css(css = base_css) %>%
    
    # Column groups
    tab_spanner(
      label = "Financial Metrics",
      columns = c(`Revenue (Billions)`, `Growth Rate (%)`, `Market Share (%)`)
    )
  
  # Return the gt table
  return(gt_table)
}

# Theme loaded successfully - no output messages