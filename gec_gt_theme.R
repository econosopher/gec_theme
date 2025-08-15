# Deconstructor of Fun GT Table Theme
# Custom GT table styling for GEC podcast

library(gt)
library(dplyr)

# Webfont import (League Spartan) similar to DoF theme
gec_use_web_fonts <- function() {
  "@import url('https://fonts.googleapis.com/css2?family=League+Spartan:wght@400;600;700&display=swap');\n"
}

# Embed Monument Extended locally (base64) so gt PNG exports keep the title font
gec_embed_monument_css <- function() {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop("Required package 'base64enc' is not installed. Install with: install.packages('base64enc')", call. = FALSE)
  }
  # Use the font paths from the main theme
  paths <- gec_get_font_paths()
  if (is.null(paths$monument) || is.na(paths$monument) || !file.exists(paths$monument)) {
    stop("Monument Extended font file not found at style_guide/fonts/MonumentExtended-Regular.otf", call. = FALSE)
  }
  raw <- readBin(paths$monument, what = "raw", n = file.info(paths$monument)$size)
  b64 <- base64enc::base64encode(raw)
  paste0(
    "@font-face { font-family: 'Monument Extended'; src: url('data:font/otf;base64,",
    b64,
    "') format('opentype'); font-weight: normal; font-style: normal; }\n"
  )
}

# Require and source the main GEC theme (fail fast)
# Try to find gec_theme.R in the same directory as this file
.gec_gt_dir <- tryCatch({
  dirname(normalizePath(sys.frame(1)$ofile))
}, error = function(e) {
  # Fallback: check common locations
  if (file.exists("../../gec_theme/gec_theme.R")) {
    normalizePath("../../gec_theme")
  } else {
    getwd()
  }
})

gec_theme_path <- file.path(.gec_gt_dir, "gec_theme.R")
if (file.exists(gec_theme_path)) {
  source(gec_theme_path)
} else if (file.exists("gec_theme.R")) {
  source("gec_theme.R")
} else if (file.exists("../gec_theme.R")) {
  source("../gec_theme.R")
} else {
  # Define colors if main theme not available (fail fast is better)
  stop("gec_theme.R not found. Ensure it exists in the project root.", call. = FALSE)
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
  # Use League Spartan for all non-header/body text for improved legibility
  gec_font_body <- "'League Spartan', 'Arial', 'Helvetica Neue', sans-serif"
  
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
                         container_border_width = 3,
                         weight_strategy = c("light", "regular", "heavy"),
                         line_thickness = c("light", "regular", "heavy")) {
  weight_strategy <- match.arg(weight_strategy)
  line_thickness <- match.arg(line_thickness)
  # Font weight hierarchy tuned for thick display fonts
  label_weight <- if (weight_strategy == "light") "normal" else if (weight_strategy == "regular") "normal" else "bold"
  spanner_weight <- if (weight_strategy == "light") "normal" else if (weight_strategy == "regular") "normal" else "bold"
  body_weight <- "normal"
  # Line thickness scale
  container_w <- if (line_thickness == "light") 2 else if (line_thickness == "regular") container_border_width else max(container_border_width, 4)
  header_rule_w <- if (line_thickness == "light") 1 else if (line_thickness == "regular") 2 else 3
  
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
      column_labels.font.weight = label_weight,
      column_labels.font.size = px(13),
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.color = border_color,
      column_labels.border.bottom.width = px(header_rule_w),
      
      # Table body
      table.background.color = gec_colors$white,
      data_row.padding = px(8),
      
      # Borders
      table.border.top.style = "solid",
      table.border.top.width = if (container_border) px(container_w) else px(2),
      table.border.top.color = if (container_border) container_border_color else header_bg,
      table.border.bottom.style = "solid",
      table.border.bottom.width = if (container_border) px(container_w) else px(2),
      table.border.bottom.color = if (container_border) container_border_color else header_bg,
      table.border.left.style = if (container_border) "solid" else "none",
      table.border.left.width = if (container_border) px(container_w) else px(0),
      table.border.left.color = if (container_border) container_border_color else "transparent",
      table.border.right.style = if (container_border) "solid" else "none",
      table.border.right.width = if (container_border) px(container_w) else px(0),
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
    
    # Style column labels with GEC font stack
    tab_style(
      style = list(
        cell_text(
          color = header_text,
          weight = label_weight,
          font = gec_font_body  # Poppins font stack for headers
        )
      ),
      locations = cells_column_labels(everything())
    ) %>%
    
    # Style table body
    tab_style(
      style = list(
        cell_text(
          color = text_color,
          font = gec_font_body,  # Poppins font stack for data
          weight = body_weight
        )
      ),
      locations = cells_body()
    )

  # Ensure source notes and footnotes use light/normal weight for readability
  result_table <- result_table %>%
    tab_style(
      style = list(cell_text(weight = "normal", color = text_color, font = gec_font_body)),
      locations = cells_source_notes()
    ) %>%
    tab_style(
      style = list(cell_text(weight = "normal", color = text_color, font = gec_font_body)),
      locations = cells_footnotes()
    )

  # Spanner typography weight
  result_table <- result_table %>%
    tab_style(
      style = list(cell_text(weight = spanner_weight, color = header_text)),
      locations = cells_column_spanners()
    )
  
  # Inject CSS with embedded Monument and web import for League Spartan
  base_css <- paste0(
    gec_embed_monument_css(),
    gec_use_web_fonts(),
    "\n",
    "  .gt_title {\n",
    "    font-family: 'Monument Extended', 'Arial Black', 'Helvetica Neue', sans-serif !important;\n",
    "  }\n",
    "  .gt_subtitle {\n",
    "    font-family: 'League Spartan', 'Arial', 'Helvetica Neue', sans-serif !important;\n",
    "  }\n",
    "  .gt_col_heading, .gt_column_spanner {\n",
    "    font-family: 'Monument Extended', 'Helvetica Neue', 'Arial', sans-serif !important;\n",
    "  }\n",
    "  .gt_sourcenote, .gt_row {\n",
    "    font-family: 'League Spartan', 'Arial', 'Helvetica Neue', sans-serif !important;\n",
    "    font-weight: 400 !important;\n",
    "  }\n",
    "  .gt_footnotes, .gt_footnote, .gt_footnote_marks {\n",
    "    font-family: 'League Spartan', 'Arial', 'Helvetica Neue', sans-serif !important;\n",
    "    font-weight: 400 !important;\n",
    "  }\n",
    "  table, .gt_table {\n",
    "    font-family: 'League Spartan', 'Arial', 'Helvetica Neue', sans-serif !important;\n",
    "  }\n"
  )
  
  result_table <- result_table %>% gt::opt_css(css = base_css)
  
  return(result_table)
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