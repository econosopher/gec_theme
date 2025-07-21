# Game Economist Consulting ggplot2 Theme
# Custom theme and color palette for GEC podcast

library(ggplot2)
library(png)
library(grid)
library(scales)
library(gridExtra)

# Load magick for image-based post-processing
if (!require(magick, quietly = TRUE)) {
  cat("Installing magick package for image processing...\n")
  install.packages("magick")
  library(magick)
} else {
  library(magick)
}

# Prevent automatic Rplot.pdf generation
options(device = function(...) png(tempfile(), ...))

# Font setup - GEC font hierarchy
# Primary: Monument Extended (body text, main font)
# Secondary: League Spartan (body text, secondary font)  
# Fonts are included in style_guide/fonts/ folder

# Install fonts to system if needed
install_gec_fonts <- function() {
  font_paths <- list(
    monument = "style_guide/fonts/MonumentExtended-Regular.otf",
    league_spartan = "style_guide/fonts/LeagueSpartan-VariableFont_wght.ttf"
  )
  
  for (font_name in names(font_paths)) {
    font_path <- font_paths[[font_name]]
    if (file.exists(font_path)) {
      system(paste("cp", shQuote(font_path), "~/Library/Fonts/"))
    }
  }
  
  message("GEC fonts installed to ~/Library/Fonts/")
  message("Run library(extrafont); font_import(); loadfonts() to use in R")
}

# Font hierarchy with fallbacks
gec_font_title <- "sans"      # Monument Extended for titles
gec_font_subtitle <- "sans"   # League Spartan for subtitles
gec_font_body <- "sans"       # Monument Extended for body text

# Temporarily disabled custom fonts to avoid crashes
# TODO: Re-enable once font compatibility issues are resolved
# tryCatch({
#   if (require(extrafont, quietly = TRUE)) {
#     available_fonts <- fonts()
#     # ... font detection code ...
#   }
# }, error = function(e) invisible())

# Backward compatibility
gec_font_family <- gec_font_title

# Define GEC color palette (official brand colors)
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

# Create color palette function
gec_palette <- function(type = "main", reverse = FALSE) {
  colors <- switch(type,
    "main" = c(gec_colors$accent, gec_colors$green, gec_colors$secondary),
    "blue_green" = c(gec_colors$accent, gec_colors$green, gec_colors$grey_dark),
    "full" = c(gec_colors$accent, gec_colors$green, gec_colors$secondary, 
               gec_colors$primary, gec_colors$grey_light)
  )
  
  if (reverse) colors <- rev(colors)
  colors
}

# Helper function to automatically convert titles to uppercase (538-style)
format_title_538 <- function(title) {
  if (is.null(title) || title == "") return(title)
  toupper(title)
}

# Custom ggplot2 theme with 538-inspired bold, uppercase titles
theme_gec <- function(base_size = 12, logo_alpha = 0.1, border = TRUE, border_color = NULL, 
                      uppercase_titles = TRUE) {
  
  # Set default border color to GEC primary yellow
  if (is.null(border_color)) {
    border_color <- gec_colors$primary
  }
  
  # Base theme with border
  base_theme <- theme_minimal(base_size = base_size) +
    theme(
      # Text styling with GEC font hierarchy
      text = element_text(color = gec_colors$secondary, family = gec_font_body),
      plot.title = element_text(
        size = base_size * 2.5,  # Even larger for thicker appearance (30px at base 12)
        color = gec_colors$secondary,
        face = "bold",
        family = gec_font_title,  # Monument Extended
        hjust = 0,  # Left-aligned like 538
        margin = margin(l = 0, b = 5, t = 0),  # Reduced margins for tighter spacing
        lineheight = 0.9  # Tighter line spacing for multi-line titles
      ),
      plot.title.position = "plot",  # Align with entire plot area
      plot.subtitle = element_text(
        size = base_size * 1.1,
        color = gec_colors$grey_dark,
        family = gec_font_subtitle,  # League Spartan
        hjust = 0,  # Left-aligned like title
        margin = margin(l = 0, b = 15)  # Increased bottom margin for more space
      ),
      plot.subtitle.position = "plot",  # Align with entire plot area
      
      # Axis styling with Monument Extended (538-inspired: minimal approach)
      axis.title = element_blank(),  # Remove axis titles like 538
      axis.text = element_text(
        color = gec_colors$grey_dark,
        size = base_size * 0.8,
        family = gec_font_body  # Monument Extended
      ),
      axis.line = element_blank(),  # Remove axis lines like 538
      axis.ticks = element_blank(),  # Remove axis ticks like 538
      
      # Grid styling (538-inspired: no grid lines for ultra-clean look)
      panel.grid.major.x = element_blank(),  # No vertical grid lines
      panel.grid.major.y = element_blank(),  # No horizontal grid lines
      panel.grid.minor = element_blank(),
      
      # Background with GEC yellow border
      plot.background = element_rect(
        fill = "white", 
        color = if (border) border_color else NA,
        linewidth = if (border) 3 else 0
      ),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Legend styling with Poppins (538-inspired positioning)
      legend.title = element_text(
        color = gec_colors$secondary,
        size = base_size * 0.9,
        face = "bold",
        family = gec_font_body  # Monument Extended
      ),
      legend.text = element_text(
        color = gec_colors$grey_dark,
        size = base_size * 0.8,
        family = gec_font_body  # Monument Extended
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",  # 538-style horizontal legend
      legend.box = "horizontal",
      legend.box.just = "left",
      legend.margin = margin(t = 15),
      legend.spacing.x = unit(15, "pt"),  # More space between legend items
      legend.key.width = unit(1.5, "cm"),  # Wider legend keys
      
      # Strip styling for facets with Monument Extended
      strip.text = element_text(
        color = gec_colors$grey_dark,  # Dark grey text on yellow background
        face = "bold",
        size = base_size * 0.9,
        family = gec_font_body  # Monument Extended
      ),
      strip.background = element_rect(
        fill = gec_colors$primary,  # Yellow background
        color = NA
      ),
      
      # Adjusted plot margins to accommodate border and logo strip
      plot.margin = margin(t = 25, r = 25, b = 80, l = 25)
    )
    
  return(base_theme)
}

# GEC Chart Container System - Image-Based Post-Processing
# This creates branded charts using magick for precise image manipulation

# Helper function to detect optimal dimensions based on plot content
detect_optimal_dimensions <- function(plot) {
  # Extract plot data and layers for analysis
  plot_data <- tryCatch({
    if (is_ggplot(plot)) {
      list(
        layers = length(plot$layers),
        facets = !is.null(plot$facet),
        has_legend = !identical(plot$guides$colour, "none") || !identical(plot$guides$fill, "none"),
        coord_type = class(plot$coordinates)[1]
      )
    } else {
      list(layers = 1, facets = FALSE, has_legend = FALSE, coord_type = "CoordCartesian")
    }
  }, error = function(e) {
    list(layers = 1, facets = FALSE, has_legend = FALSE, coord_type = "CoordCartesian")
  })
  
  # Base dimensions
  base_width <- 1200
  base_height <- 800
  
  # Adjust for plot complexity
  if (plot_data$facets) {
    base_width <- base_width * 1.3
    base_height <- base_height * 1.2
  }
  
  if (plot_data$has_legend) {
    base_height <- base_height + 60
  }
  
  if (plot_data$coord_type == "CoordFlip") {
    # Swap dimensions for flipped coordinates
    temp <- base_width
    base_width <- base_height * 1.1
    base_height <- temp * 0.9
  }
  
  return(list(width = round(base_width), height = round(base_height)))
}

# Helper function to calculate adaptive margins based on plot characteristics
calculate_adaptive_margins <- function(plot, width, height, logo_strip, strip_height_px) {
  # Base margins
  base_margin <- 20
  
  # Analyze plot for margin requirements
  plot_info <- tryCatch({
    if (is_ggplot(plot)) {
      list(
        has_title = !is.null(plot$labels$title) && plot$labels$title != "",
        has_subtitle = !is.null(plot$labels$subtitle) && plot$labels$subtitle != "",
        facets = !is.null(plot$facet),
        coord_type = class(plot$coordinates)[1]
      )
    } else {
      list(has_title = FALSE, has_subtitle = FALSE, facets = FALSE, coord_type = "CoordCartesian")
    }
  }, error = function(e) {
    list(has_title = FALSE, has_subtitle = FALSE, facets = FALSE, coord_type = "CoordCartesian")
  })
  
  # Calculate adaptive margins
  top_margin <- 10  # Reduced base top margin
  if (plot_info$has_title) top_margin <- top_margin + 2  # Minimal addition for title
  if (plot_info$has_subtitle) top_margin <- top_margin + 2  # Minimal addition for subtitle
  
  # Adjust for facets
  if (plot_info$facets) {
    top_margin <- top_margin + 15
  }
  
  # Bottom margin depends on logo strip
  bottom_margin <- if (logo_strip) 5 else base_margin
  
  # Side margins scale with width
  side_margin <- max(base_margin, width * 0.02)
  
  return(list(
    top = top_margin,
    right = side_margin,
    bottom = bottom_margin,
    left = side_margin
  ))
}

create_gec_container <- function(plot, 
                                 logo_strip = TRUE,
                                 logo_path = NULL, 
                                 icon_path = NULL,
                                 logo_type = "default",      # Options: "default", "alternate", or custom path
                                 logo_color = "primary",     # Options: "primary", "secondary", "black", "white"
                                 icon_color = "primary",     # Options: "primary", "secondary", "black", "white"
                                 strip_height_px = 45,       # Thinner logo strip for better proportions
                                 strip_color = NULL,
                                 container_border = TRUE,
                                 border_color = NULL,
                                 border_width = 9,
                                 width = NULL,              # Auto-detect if NULL
                                 height = NULL,             # Auto-detect if NULL
                                 dpi = 150) {               # Higher DPI for better quality
  
  # Validate inputs
  if (!is_ggplot(plot)) {
    stop("Input must be a ggplot object")
  }
  
  # Check magick availability
  if (!requireNamespace("magick", quietly = TRUE)) {
    warning("magick package not available. Install with: install.packages('magick')")
    return(plot)  # Return original plot
  }
  
  # Auto-detect dimensions if not provided
  if (is.null(width) || is.null(height)) {
    # Default responsive dimensions based on plot type and data
    default_dims <- detect_optimal_dimensions(plot)
    if (is.null(width)) width <- default_dims$width
    if (is.null(height)) height <- default_dims$height
  }
  
  # Validate dimensions
  if (width < 300 || height < 200) {
    warning("Dimensions too small, using minimum sizes")
    width <- max(width, 600)
    height <- max(height, 400)
  }
  
  # Set default colors
  if (is.null(strip_color)) {
    strip_color <- gec_colors$primary
  }
  if (is.null(border_color)) {
    border_color <- gec_colors$primary
  }
  
  # Adaptive margin system based on plot complexity
  plot_margins <- calculate_adaptive_margins(plot, width, height, logo_strip, strip_height_px)
  
  # Create a clean plot with adaptive margins
  clean_plot <- plot + theme_gec(border = FALSE) +  # No border on ggplot itself
    theme(
      plot.margin = margin(t = plot_margins$top, r = plot_margins$right, 
                          b = plot_margins$bottom, l = plot_margins$left),
      panel.spacing = unit(0, "pt"),                        # No panel spacing
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # Safe file operations with error handling
  temp_plot_file <- tempfile(fileext = ".png")
  
  tryCatch({
    # Calculate chart dimensions (accounting for logo strip if needed)
    chart_height <- if (logo_strip) height - strip_height_px else height
    
    ggsave(temp_plot_file, clean_plot, 
           width = width/100, height = chart_height/100, 
           units = "in", dpi = 100, bg = "white")
    
    # Validate file was created
    if (!file.exists(temp_plot_file)) {
      stop("Failed to create temporary plot file")
    }
    
    # Load the chart image with validation
    chart_img <- magick::image_read(temp_plot_file)
    
  }, error = function(e) {
    if (file.exists(temp_plot_file)) unlink(temp_plot_file)
    stop(paste("Error creating plot image:", e$message))
  })
  
  # Resize to exact dimensions with validation
  tryCatch({
    chart_img <- magick::image_resize(chart_img, paste0(width, "x", chart_height, "!"))
    
    # Create final canvas with full dimensions
    final_height <- height
    canvas <- magick::image_blank(width, final_height, color = "white")
    
    # Add the chart to the canvas
    canvas <- magick::image_composite(canvas, chart_img, offset = "+0+0")
    
    # Add logo strip if requested
    if (logo_strip) {
      canvas <- add_gec_logo_strip_image(canvas, logo_path, icon_path, strip_color, 
                                         strip_height_px, width, logo_type, logo_color, icon_color)
    }
    
    # Add border if requested
    if (container_border) {
      canvas <- magick::image_border(canvas, border_color, 
                            paste0(border_width, "x", border_width))
    }
    
  }, error = function(e) {
    warning(paste("Error in image processing:", e$message))
    # Return a basic bordered plot as fallback
    return(plot + theme_gec(border = container_border, border_color = border_color))
  }, finally = {
    # Always clean up temporary file
    if (file.exists(temp_plot_file)) unlink(temp_plot_file)
  })
  
  return(canvas)
}

# Image-based logo strip creation using magick with robust error handling
add_gec_logo_strip_image <- function(canvas, logo_path = NULL, icon_path = NULL, 
                                    strip_color = NULL, strip_height_px = 45, 
                                    canvas_width = 1200, logo_type = "default",
                                    logo_color = "primary", icon_color = "primary") {
  
  # Validate inputs
  if (!requireNamespace("magick", quietly = TRUE)) {
    warning("magick package not available. Returning original canvas.")
    return(canvas)
  }
  
  # Set default strip color to yellow
  if (is.null(strip_color)) {
    strip_color <- gec_colors$primary  # Yellow for the strip
  }
  
  # Robust logo path detection with color-specific patterns
  find_asset_path <- function(asset_type = "logo", color = "primary", logo_variant = "default") {
    # Try multiple paths to find the files
    base_paths <- c("", "../", "../../")
    
    # Use style guide logos
    if (asset_type == "logo") {
      # Map color options to available logo files
      
      for (base in base_paths) {
        logo_files <- list(
          "primary" = paste0(base, "style_guide/logo/Copy of logo_dark.png"),
          "secondary" = paste0(base, "style_guide/logo/Copy of logo_dark.png"), 
          "black" = paste0(base, "style_guide/logo/Copy of logo_dark.png"),
          "white" = paste0(base, "style_guide/logo/Copy of logo_white.png"),
          "green" = paste0(base, "style_guide/logo/Copy of logo_green.png")
        )
        
        file_path <- logo_files[[tolower(color)]]
        if (!is.null(file_path) && file.exists(file_path)) {
          return(file_path)
        }
      }
      
      # Fallback to any available logo
      search_dirs <- c(
        "style_guide/logo",
        "../style_guide/logo"
      )
    } else {
      # For icon, use the short logo versions
      # Try multiple paths
      for (base in base_paths) {
        icon_files <- list(
          "primary" = paste0(base, "style_guide/logo/Copy of logo_short_dark.png"),
          "secondary" = paste0(base, "style_guide/logo/Copy of logo_short_dark.png"),
          "black" = paste0(base, "style_guide/logo/Copy of logo_short_dark.png"),
          "white" = paste0(base, "style_guide/logo/Copy of logo_short_white.png")
        )
        
        file_path <- icon_files[[tolower(color)]]
        if (!is.null(file_path) && file.exists(file_path)) {
          return(file_path)
        }
      }
      
      search_dirs <- c(
        "style_guide/logo",
        "../style_guide/logo"
      )
    }
    
    for (dir in search_dirs) {
      if (dir.exists(dir)) {
        files <- list.files(dir, pattern = "*.png", ignore.case = TRUE, full.names = TRUE)
        if (length(files) > 0) {
          return(files[1])  # Return first match
        }
      }
    }
    return(NULL)
  }
  
  # Handle logo type selection
  if (is.null(logo_path)) {
    if (logo_type != "default" && logo_type != "alternate") {
      # logo_type is a custom path
      logo_path <- logo_type
    } else {
      # Auto-detect based on logo type
      logo_path <- find_asset_path("logo", logo_color, logo_type)
    }
  }
  
  # Auto-detect icon path with color-specific fallbacks  
  if (is.null(icon_path)) {
    icon_path <- find_asset_path("icon", icon_color)
  }
  
  # Create yellow background strip
  strip_bg <- image_blank(canvas_width, strip_height_px, color = strip_color)
  
  # Calculate positioning
  logo_margin <- 15        # 15px from right edge
  icon_margin <- 60        # Space between main logo and icon logo
  logo_height <- 26        # Logo height in pixels
  icon_size <- 30          # Icon height in pixels (slightly smaller than main logo)
  
  # Add icon logo on the right if available
  if (file.exists(icon_path)) {
    icon_img <- image_read(icon_path)
    
    # Resize icon
    icon_info <- image_info(icon_img)
    icon_aspect <- icon_info$width / icon_info$height
    icon_width <- round(icon_size * icon_aspect)
    icon_img <- image_resize(icon_img, paste0(icon_width, "x", icon_size, "!"))
    
    # Get actual dimensions
    resized_icon_info <- image_info(icon_img)
    actual_icon_height <- resized_icon_info$height
    actual_icon_width <- resized_icon_info$width
    
    # Position icon on the right
    icon_x <- canvas_width - actual_icon_width - logo_margin
    icon_y <- round((strip_height_px - actual_icon_height) / 2) + 3
    icon_y <- max(icon_y, 1)
    
    # Composite icon onto strip
    strip_bg <- image_composite(strip_bg, icon_img, 
                               offset = paste0("+", icon_x, "+", icon_y))
  } else {
    warning("Icon file not found.")
  }
  
  # Get canvas dimensions
  canvas_info <- image_info(canvas)
  canvas_height <- canvas_info$height
  
  # Calculate position for logo strip (bottom of canvas)
  strip_y <- canvas_height - strip_height_px
  
  # Composite the logo strip onto the bottom of the canvas
  result <- image_composite(canvas, strip_bg, 
                           offset = paste0("+0+", strip_y))
  
  return(result)
}

# Backward compatibility function - now uses image-based approach
add_gec_logo_strip <- function(plot, logo_path = NULL, strip_height = 0.075, 
                               strip_color = NULL, logo_type = "default",
                               width = 1200, height = 800) {
  # Convert strip_height proportion to pixels
  strip_height_px <- round(height * strip_height)
  
  create_gec_container(
    plot = plot,
    logo_strip = TRUE,
    logo_path = logo_path,
    logo_type = logo_type,
    strip_height_px = strip_height_px,
    strip_color = strip_color,
    width = width,
    height = height
  )
}

# Scale functions for consistent GEC colors
scale_color_gec <- function(type = "main", ...) {
  scale_color_manual(values = gec_palette(type), ...)
}

scale_fill_gec <- function(type = "main", ...) {
  scale_fill_manual(values = gec_palette(type), ...)
}

# Use scales package label_dollar and label_number for smart formatting
# The scales package already handles K/M/B formatting automatically

# For backwards compatibility, map old function names to scales functions
format_gec_currency <- label_dollar()
format_gec_number <- label_number(big.mark = ",")
format_gec_percent <- label_percent(accuracy = 0.1)
format_gec_billions <- label_dollar(suffix = "B", scale = 1e-9, accuracy = 0.1)
format_gec_millions <- label_dollar(suffix = "M", scale = 1e-6, accuracy = 0.1)

# Custom formatter for small values (ARPDAU, etc) with 2 decimal places
format_gec_smart_currency <- function(x) {
  # Use label_dollar with custom logic for small values
  ifelse(abs(x) < 100 & abs(x) > 0,
         dollar(x, accuracy = 0.01),  # 2 decimals for small values
         label_dollar(accuracy = NULL, scale_cut = cut_short_scale())(x))  # Auto K/M/B
}

# For non-currency numbers
format_gec_smart_number <- label_number(accuracy = NULL, scale_cut = cut_short_scale())

# Example chart demonstrating the theme
create_gec_example_chart <- function(save_path = NULL) {
  # Sample gaming industry data
  gaming_data <- data.frame(
    platform = c("Mobile", "Console", "PC", "VR", "Cloud"),
    revenue_billions = c(93.2, 50.4, 36.7, 6.2, 3.7),
    growth_rate = c(7.3, 2.1, -2.8, 31.1, 74.9),
    category = c("Traditional", "Traditional", "Traditional", "Emerging", "Emerging")
  )
  
  # Calculate average for reference line
  avg_revenue <- mean(gaming_data$revenue_billions)
  
  # Calculate dynamic position for average text with scalable logic
  # For horizontal bar charts (coord_flip), positioning needs special handling
  max_revenue <- max(gaming_data$revenue_billions)
  min_revenue <- min(gaming_data$revenue_billions)
  
  # Position text horizontally: slightly to the right of the average line
  text_y_position <- avg_revenue + (max_revenue * 0.025)  # 2.5% offset (half of previous)
  
  # Position text vertically: find a gap in the data to avoid overlap
  # Sort platforms by revenue to find the best gap
  sorted_revenues <- sort(gaming_data$revenue_billions, decreasing = TRUE)
  
  # Find which platforms are closest to the average
  platform_positions <- seq_along(gaming_data$platform)
  revenues_ordered <- gaming_data$revenue_billions[order(gaming_data$revenue_billions, decreasing = TRUE)]
  
  # Place text at a position that avoids overlap with bars
  # Using 0.7 positions the text above the highest bar (30% up from top)
  text_x_position <- 0.7
  
  # Create the plot with new border and logo strip
  p <- ggplot(gaming_data, aes(x = reorder(platform, revenue_billions), y = revenue_billions)) +
    geom_col(fill = gec_colors$accent, width = 0.7) +
    geom_hline(yintercept = avg_revenue, linetype = "solid", 
               color = gec_colors$secondary, linewidth = 1.2, alpha = 0.9) +
    annotate("text", y = text_y_position, x = text_x_position,
             label = paste0("Avg: $", round(avg_revenue, 1), "B"),
             color = gec_colors$secondary, size = 3.5, family = gec_font_body,
             hjust = 0, vjust = 0.5, fontface = "bold") +
    scale_fill_manual(values = c(
      "Traditional" = gec_colors$accent,
      "Emerging" = gec_colors$green
    )) +
    scale_y_continuous(labels = format_gec_billions, expand = c(0, 0, 0.1, 0)) +
    coord_flip() +
    labs(
      title = "MOBILE GAMING DOMINATES PLATFORM REVENUE IN 2024",
      subtitle = "Traditional platforms like mobile, console, and PC still drive most revenue,\nwhile emerging tech like VR and cloud gaming remain small",
      x = NULL,  # Remove axis labels for 538-style
      y = NULL,  # Remove axis labels for 538-style  
      fill = "Platform Type",
      caption = "Game Economist Consulting • Sensor Tower Data"
    ) +
    theme_gec(base_size = 12) +
    theme(
      plot.caption = element_text(
        color = gec_colors$grey_dark,
        size = 10,
        hjust = 1,
        family = gec_font_subtitle,  # League Spartan for captions
        margin = margin(t = 15)
      )
    )
  
  # Create the image-based container with logo strip
  result_image <- create_gec_container(p, 
    logo_strip = TRUE,
    strip_color = gec_colors$primary,
    container_border = TRUE,
    border_color = gec_colors$primary,
    width = 1200, 
    height = 800)
  
  # Save if path provided
  if (!is.null(save_path)) {
    image_write(result_image, save_path)
    cat("Chart with border and logo strip saved as '", save_path, "'\n", sep = "")
  }
  
  return(result_image)
}

# Line chart example with reference line
create_gec_line_chart <- function(save_path = NULL) {
  # Generate sample daily revenue data for popular games over 30 days
  set.seed(42)  # For reproducible data
  dates <- seq(from = Sys.Date() - 29, to = Sys.Date(), by = "day")
  
  # Create realistic daily revenue data for different games
  games_data <- expand.grid(
    date = dates,
    game = c("Fortnite", "Roblox", "Genshin Impact", "PUBG Mobile", "Candy Crush")
  )
  
  # Generate revenue with different patterns for each game
  games_data$daily_revenue <- sapply(1:nrow(games_data), function(i) {
    game <- games_data$game[i]
    day_num <- as.numeric(games_data$date[i] - min(games_data$date)) + 1
    
    base_revenue <- switch(as.character(game),
      "Fortnite" = 850000,
      "Roblox" = 720000, 
      "Genshin Impact" = 950000,
      "PUBG Mobile" = 680000,
      "Candy Crush" = 520000
    )
    
    # Add trend and seasonality
    trend <- base_revenue * (1 + (day_num - 15) * 0.01)
    seasonality <- sin(day_num * 2 * pi / 7) * base_revenue * 0.1  # Weekly pattern
    noise <- rnorm(1, 0, base_revenue * 0.05)
    
    max(trend + seasonality + noise, 0)
  })
  
  # Calculate overall average for reference line
  overall_avg <- mean(games_data$daily_revenue)
  
  # Create the line plot
  p <- ggplot(games_data, aes(x = date, y = daily_revenue, color = game)) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 2, alpha = 0.5) +  # Increased transparency
    geom_hline(yintercept = overall_avg, linetype = "solid", 
               color = gec_colors$secondary, linewidth = 1.2, alpha = 0.9) +  # Made line more prominent
    annotate("text", x = max(games_data$date), y = overall_avg,
             label = paste0("Avg: ", format_gec_smart_currency(overall_avg)),
             color = gec_colors$secondary, size = 3.5, family = gec_font_body,
             hjust = 1.1, vjust = -0.5, fontface = "bold") +  # Made text bold and larger
    scale_color_manual(values = c(
      "Fortnite" = gec_colors$accent,
      "Roblox" = gec_colors$green,
      "Genshin Impact" = gec_colors$secondary,
      "PUBG Mobile" = gec_colors$grey_dark,
      "Candy Crush" = gec_colors$grey_light
    )) +
    scale_y_continuous(labels = format_gec_smart_currency, expand = c(0, 0, 0.05, 0)) +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
    labs(
      title = "DAILY MOBILE GAME REVENUE TRENDS OVER 30 DAYS",
      subtitle = "Fortnite and Genshin Impact lead in daily revenue generation,\\nwith clear weekly patterns visible across all titles",
      x = NULL,
      y = NULL,
      color = "Game Title",
      caption = "Game Economist Consulting • Sensor Tower Data"
    ) +
    theme_gec(base_size = 12) +
    theme(
      plot.caption = element_text(
        color = gec_colors$grey_dark,
        size = 10,
        hjust = 1,
        family = gec_font_subtitle,
        margin = margin(t = 15)
      ),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Create the image-based container with logo strip
  result_image <- create_gec_container(p, 
    logo_strip = TRUE,
    strip_color = gec_colors$primary,
    container_border = TRUE,
    border_color = gec_colors$primary,
    width = 1200, 
    height = 800)
  
  # Save if path provided
  if (!is.null(save_path)) {
    image_write(result_image, save_path)
    cat("Line chart with border and logo strip saved as '", save_path, "'\\n", sep = "")
  }
  
  return(result_image)
}

# Stacked bar chart example
create_gec_stacked_chart <- function(save_path = NULL) {
  # Generate sample data for different game genres by platform
  stacked_data <- data.frame(
    platform = rep(c("Mobile", "Console", "PC", "VR"), each = 4),
    genre = rep(c("Action", "Strategy", "RPG", "Casual"), 4),
    revenue_millions = c(
      # Mobile
      12500, 8200, 15300, 9800,
      # Console  
      8900, 3200, 11200, 2100,
      # PC
      7800, 5900, 9200, 1800,
      # VR
      420, 180, 680, 290
    )
  )
  
  # Calculate totals for each platform
  platform_totals <- aggregate(revenue_millions ~ platform, stacked_data, sum)
  
  # Create the stacked bar plot
  p <- ggplot(stacked_data, aes(x = platform, y = revenue_millions, fill = genre)) +
    geom_col(width = 0.7, alpha = 0.9) +
    # Add labels inside each segment
    geom_text(aes(label = paste0("$", round(revenue_millions/1000, 1), "B")), 
              position = position_stack(vjust = 0.5),
              color = "white",
              size = 3.5,
              fontface = "bold",
              family = gec_font_body) +
    # Add total labels on top of each bar
    geom_text(data = platform_totals, 
              aes(x = platform, y = revenue_millions, label = paste0("$", round(revenue_millions/1000, 1), "B")),
              inherit.aes = FALSE,
              vjust = -0.5,
              color = gec_colors$secondary,
              size = 4,
              fontface = "bold",
              family = gec_font_body) +
    scale_fill_gec("full") +
    scale_y_continuous(labels = function(x) paste0("$", round(x/1000, 1), "B"), 
                       expand = c(0, 0, 0.1, 0)) +
    labs(
      title = "GAMING REVENUE BY PLATFORM AND GENRE IN 2024",
      subtitle = "Mobile gaming dominates across all genres, with RPG and Action\\ngames generating the highest revenue per platform",
      x = NULL,
      y = NULL,
      fill = "Game Genre",
      caption = "Game Economist Consulting • Sensor Tower Data"
    ) +
    theme_gec(base_size = 12) +
    theme(
      plot.caption = element_text(
        color = gec_colors$grey_dark,
        size = 10,
        hjust = 1,
        family = gec_font_subtitle,
        margin = margin(t = 15)
      ),
      # Remove y-axis text since we have labels
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  # Create the image-based container with logo strip
  result_image <- create_gec_container(p, 
    logo_strip = TRUE,
    strip_color = gec_colors$primary,
    container_border = TRUE,
    border_color = gec_colors$primary,
    width = 1200, 
    height = 800)
  
  # Save if path provided
  if (!is.null(save_path)) {
    image_write(result_image, save_path)
    cat("Stacked bar chart with border and logo strip saved as '", save_path, "'\\n", sep = "")
  }
  
  return(result_image)
}

# 100% stacked bar chart example
create_gec_100_stacked_chart <- function(save_path = NULL) {
  # Generate sample data for monetization models by platform
  monetization_data <- data.frame(
    platform = rep(c("Mobile", "Console", "PC", "VR"), each = 4),
    model = rep(c("Free-to-Play", "Premium", "Subscription", "Ad-Supported"), 4),
    percentage = c(
      # Mobile
      65, 15, 12, 8,
      # Console
      25, 55, 15, 5,
      # PC  
      35, 40, 20, 5,
      # VR
      30, 60, 8, 2
    )
  )
  
  # Create the 100% stacked bar plot
  p <- ggplot(monetization_data, aes(x = platform, y = percentage, fill = model)) +
    geom_col(width = 0.7, alpha = 0.9, position = "fill") +
    geom_text(aes(label = paste0(percentage, "%")), 
              position = position_fill(vjust = 0.5),
              color = "white",
              size = 3.5,
              fontface = "bold",
              family = gec_font_body) +
    scale_fill_gec("full") +
    scale_y_continuous(labels = percent_format(), expand = c(0, 0, 0.02, 0)) +
    labs(
      title = "MONETIZATION MODEL DISTRIBUTION BY PLATFORM 2024",
      subtitle = "Free-to-play dominates mobile while premium sales lead console gaming,\\nsubscription models gaining traction across all platforms",
      x = NULL,
      y = NULL,
      fill = "Monetization Model",
      caption = "Game Economist Consulting • Sensor Tower Data"
    ) +
    theme_gec(base_size = 12) +
    theme(
      plot.caption = element_text(
        color = gec_colors$grey_dark,
        size = 10,
        hjust = 1,
        family = gec_font_subtitle,
        margin = margin(t = 15)
      )
    )
  
  # Create the image-based container with logo strip
  result_image <- create_gec_container(p, 
    logo_strip = TRUE,
    strip_color = gec_colors$primary,
    container_border = TRUE,
    border_color = gec_colors$primary,
    width = 1200, 
    height = 800)
  
  # Save if path provided
  if (!is.null(save_path)) {
    image_write(result_image, save_path)
    cat("100% stacked bar chart with border and logo strip saved as '", save_path, "'\\n", sep = "")
  }
  
  return(result_image)
}

# Theme loaded successfully - no output messages