# Configuration file for GEC logo paths
# Update these paths to point to your actual logo and icon files

# Default logo and icon paths
# Replace these with the actual paths to your GEC logo and icon files
GEC_LOGO_PATH <- "path/to/your/gec_logo.png"  # Main GEC logo (horizontal preferred)
GEC_ICON_PATH <- "path/to/your/gec_icon.png"  # GEC icon (square preferred)

# Alternative logo paths if you have different versions
GEC_LOGO_WHITE <- "path/to/your/gec_logo_white.png"
GEC_LOGO_BLACK <- "path/to/your/gec_logo_black.png"
GEC_ICON_WHITE <- "path/to/your/gec_icon_white.png"
GEC_ICON_BLACK <- "path/to/your/gec_icon_black.png"

# Function to create a chart with hardcoded logo paths
create_gec_chart_with_logos <- function(plot, 
                                       use_white_logos = FALSE,
                                       width = 1200, 
                                       height = 800) {
  
  # Select appropriate logo/icon based on background
  if (use_white_logos) {
    logo <- GEC_LOGO_WHITE
    icon <- GEC_ICON_WHITE
  } else {
    logo <- GEC_LOGO_PATH
    icon <- GEC_ICON_PATH
  }
  
  # Create the container with logos
  create_gec_container(
    plot,
    logo_strip = TRUE,
    logo_path = logo,
    icon_path = icon,
    strip_color = gec_colors$primary,  # Yellow strip
    container_border = TRUE,
    border_color = gec_colors$primary,  # Yellow border
    width = width,
    height = height
  )
}