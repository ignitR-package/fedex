# fedex R Package

This package provides utilities for querying a local STAC catalog and retrieving asset information, designed for wildfire resilience data workflows.

## Key Features
- Hard-coded local STAC catalog path
- Functions to list, filter, and extract properties from STAC items
- Asset URL extraction for COG and other file types
- Ready for extension to remote STAC APIs and COG tile retrieval

## Development Workflow
- Use `devtools::document()` to generate documentation
- Use `devtools::check()` to run package checks
- Use `testthat` for unit testing
- Use `usethis::use_r()` to add new R scripts
- Use `pkgdown` to build package website

## Example Usage
```r
library(fedex)
items <- load_stac_items()
domains <- get_domains(items)
asset_urls <- get_asset_urls(items[[1]])
```

See [r-pkgs.org](https://r-pkgs.org/whole-game.html) for best practices.
