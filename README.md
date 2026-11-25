# Metamodel-Based Colorectal Cancer Screening Decision Tool

An R Shiny app. It reads pre-computed metamodel predictions (`tool_grid.rds`) and prices them in the browser.

## How the predictions reach the app

`tool_grid.rds` is not stored in this repository. It is attached to the newest GitHub Release, and the app downloads
it from `releases/latest/download/tool_grid.rds` each time it starts. If a `tool_grid.rds` sits next to `app.R`, the
app uses that copy instead (for local testing).

## Updating to new metamodels

1. Build the new grid with `make_tool_grid_v2.R` and check it with `test_tool_v2.R` (kept locally, not in this repo).
2. On GitHub: Releases > Draft a new release > new tag (for example `grid-2027-01-15`) > attach the new
   `tool_grid.rds` (same file name) > Publish release. Do not mark it as a pre-release.
3. The online app uses the new file the next time it starts. To force that right away, restart the app in
   Posit Connect Cloud.

The new grid must have the same structure as the old one (same programs, inputs and outcomes). New inputs or outcomes
need changes to `app.R`.

## Updating the app itself

Edit `app.R`, then commit and push. Posit Connect Cloud republishes automatically. If a new R package is used, rerun
`rsconnect::writeManifest(appFiles = "app.R")` and commit `manifest.json`.
