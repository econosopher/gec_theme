# GEC Theme Project Settings

This file contains project-specific settings and information for the GEC Theme project.

## Project Overview

The GEC Theme is a custom R visualization theme package for Game Economist Consulting. It provides consistent styling for ggplot2 charts and GT tables with bright yellow accents and dark professional colors.

## Key Components

- `gec_theme.R` - Main ggplot2 theme and functions
- `gec_gt_theme.R` - GT table theme and styling
- `examples/` - Example implementations

## Development Notes

When working on this project:
1. Test all changes with the example scripts
2. Ensure consistent naming conventions (gec_*)
3. Update documentation when adding new functions
4. **ALWAYS regenerate example images after making theme changes** - Run `Rscript examples/scripts/generate_all_examples.R`
5. **Never output HTML files** - Only PNG outputs for examples
6. All example scripts must complete without errors before committing

## TODO

- [x] Update color palette with GEC brand colors
- [x] Add GEC logo files to style_guide/logo/ directory
- [x] Update font stack (Monument Extended + League Spartan)
- [x] Generate example outputs with new branding
- [ ] Create automated tests for theme functions
- [ ] Add more complex example visualizations
- [ ] Create vignette with best practices

## Logo Configuration

Logos are located in `style_guide/logo/`:
- Main logos: `Copy of logo_dark.png`, `Copy of logo_white.png`, `Copy of logo_green.png`
- Icon versions: `Copy of logo_short_dark.png`, `Copy of logo_short_white.png`

The theme automatically uses these logos for the bottom strip in charts.

## Automated Hooks

### Pre-Commit Hook
Before any commit:
1. Automatically run `Rscript examples/scripts/generate_all_examples.R`
2. Block commit if examples fail to generate
3. Stage all updated PNG files in `examples/output/`
4. Remove any HTML outputs (only PNG allowed)

### PR Submission Hook
Before submitting a pull request:
1. Run all example generation scripts
2. Verify no errors in output
3. Update README with summary of changes if >10 lines modified
4. Stage all updated example images

## Project-Specific Rules

1. **Color Usage**:
   - Yellow (#E4F577) ONLY for borders and logo strips
   - Data visualizations use darker colors: blue (#23648D), green (#2D6F31), grey (#363D46)
   - Never use bright yellow for large data areas

2. **Font Hierarchy**:
   - Monument Extended for body text and titles
   - League Spartan for subtitles only
   - Maintain consistency across all outputs

3. **Example Generation**:
   - All examples must use `create_gec_container()` for logo strips
   - PNG outputs only (no HTML files in examples/output/)
   - Examples must demonstrate professional gaming analytics use cases

4. **Code Style**:
   - No `cat()` statements in example scripts
   - No emoji usage in any outputs
   - Function names follow gec_* convention
   - Comments focus on what, not how

## Inherited from Global Settings

- No references to Claude, Anthropic, or AI assistance in any documentation
- No "Generated with Claude Code" attributions
- Use pacman for R package management when applicable
- Clean up old build artifacts (*.tar.gz) before building