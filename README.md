# GEC Theme

A custom ggplot2 and GT theme for Game Economist Consulting data visualizations.

## Overview

This R theme package provides:
- Custom ggplot2 theme with professional styling
- GT table theme for consistent table formatting
- Color palettes and typography settings
- Smart number and currency formatting
- Image-based chart container system with branding

## Installation

```r
# Source the theme files
source("gec_theme.R")
source("gec_gt_theme.R")
```

## Usage

### Basic ggplot2 theme

```r
library(ggplot2)

# Apply the GEC theme to a plot
ggplot(data, aes(x, y)) +
  geom_line() +
  theme_gec()
```

### Using color scales

```r
# Discrete colors
ggplot(data, aes(x, y, color = category)) +
  geom_line() +
  scale_color_gec() +
  theme_gec()

# Continuous colors
ggplot(data, aes(x, y, fill = value)) +
  geom_tile() +
  scale_fill_gec_continuous() +
  theme_gec()
```

### Creating branded chart containers

```r
# Create a chart with GEC branding
p <- ggplot(data, aes(x, y)) +
  geom_line() +
  theme_gec()

create_gec_container(
  plot = p,
  title = "Chart Title",
  subtitle = "Chart subtitle"
)
```

### GT table theming

```r
library(gt)

# Apply GEC theme to a GT table
gt_table <- data %>%
  gt() %>%
  theme_gec_gt()
```

### Number formatting

```r
# Format currency
format_gec_currency(1234567)  # "$1.2M"

# Format large numbers
format_gec_number(1234567)    # "1.2M"

# Format percentages
format_gec_percent(0.123)     # "12.3%"
```

## Customization

The theme uses the following default settings that can be customized:

### Colors
- Primary: `#FF66A5` (pink)
- Secondary: `#0F0D4F` (dark blue)
- Accent colors for data visualization
- Background and text colors

### Typography
- Title font: Agrandir
- Body font: Inter Tight
- Caption font: Poppins

To customize these settings, modify the color and font definitions in `gec_theme.R`.

## Examples

See the `examples/` directory for sample implementations:
- `example_charts.R` - Various chart types with GEC theme
- `example_table.R` - GT table examples
- `run_all_examples.R` - Generate all example outputs

## License

[Your license here]

## Contact

Game Economist Consulting
[Your contact information]