# GEC Theme

A custom ggplot2 theme for Game Economist Consulting featuring clean, FiveThirtyEight-inspired visualizations with distinctive yellow branding.

## Installation

Since this package is not on CRAN, install directly from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install GEC theme from GitHub
devtools::source_url("https://raw.githubusercontent.com/econosopher/gec_theme/main/gec_theme.R")
devtools::source_url("https://raw.githubusercontent.com/econosopher/gec_theme/main/gec_gt_theme.R")
```

Or clone the repository and source locally:

```r
# After cloning the repo
source("path/to/gec_theme/gec_theme.R")
source("path/to/gec_theme/gec_gt_theme.R")
```

## Quick Start

```r
library(ggplot2)

# Create a simple bar chart with GEC theme
ggplot(mtcars, aes(x = reorder(row.names(mtcars), mpg), y = mpg)) +
  geom_col(fill = gec_colors$accent) +
  coord_flip() +
  labs(
    title = "FUEL EFFICIENCY BY CAR MODEL",
    subtitle = "Miles per gallon comparison",
    x = NULL,
    y = NULL
  ) +
  theme_gec()

# Add the logo strip and yellow border
p <- last_plot()
img <- create_gec_container(p, width = 1000, height = 600)
```

## Key Features

- **Clean aesthetic**: No grid lines, minimal elements following FiveThirtyEight style
- **Bold typography**: Uppercase titles with Monument Extended font
- **Brand colors**: Bright yellow (#E4F577) accents with professional data colors
- **Logo integration**: Automatic logo placement in bottom strip
- **Smart formatting**: Built-in number and currency formatters

## Color Palette

```r
# Main data colors
gec_colors$accent    # Blue (#23648D)
gec_colors$green     # Green (#2D6F31)
gec_colors$secondary # Dark Grey (#363D46)
gec_colors$primary   # Yellow (#E4F577) - for borders/accents only
```

## Creating Publication-Ready Charts

```r
# Generate all examples
source("examples/scripts/generate_all_examples.R")

# Basic usage pattern
p <- ggplot(data, aes(...)) +
  geom_*() +
  scale_*_gec() +
  labs(
    title = "YOUR TITLE IN UPPERCASE",
    subtitle = "Descriptive subtitle",
    x = NULL,  # Remove axis labels
    y = NULL
  ) +
  theme_gec()

# Add branding
img <- create_gec_container(
  p,
  logo_strip = TRUE,
  width = 1000,
  height = 600
)

# Save
magick::image_write(img, "output.png")
```

## GT Table Styling

```r
library(gt)

data %>%
  gt() %>%
  theme_gec_gt() %>%
  tab_header(
    title = "TABLE TITLE",
    subtitle = "Table subtitle"
  )
```

## Requirements

- R >= 3.5.0
- ggplot2
- magick (for logo functionality)
- gt (for table styling)
- scales (for number formatting)

## Font Setup

The theme uses Monument Extended and League Spartan fonts. These are included in `style_guide/fonts/`. Install them on your system for best results, though the theme will fall back to system fonts if needed.

## License

Proprietary - Game Economist Consulting

## Contact

Game Economist Consulting
https://gameeconomist.com