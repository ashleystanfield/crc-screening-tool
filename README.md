# Metamodel-Based Colorectal Cancer Screening Decision Tool

An R Shiny app for comparing colorectal cancer screening programs. It shows the predictions of the final random
forest metamodels for every whole-number screening trajectory (before / during / after the intervention) and
prices them with editable unit costs.

## How the predictions reach the app

The random forests were run ahead of time for every allowed whole-number trajectory (`make_tool_grid_1pt.R`, kept
locally). The results are attached to the newest GitHub Release, not stored in this repository:

- `grid_index.rds`: the trajectory list, quantity order and which part file holds each trajectory
- `<program>_partN.bin`: float32 predictions, one block of 180 person types x 35 quantities per trajectory

At start-up the app finds the newest release, downloads `grid_index.rds`, and then fetches only the ~25 KB block for
the trajectory a user picks. If a folder `grid/` with these files sits next to `app.R`, the app reads it instead
(local use).

## Updating to new metamodels

1. Run `make_tool_grid_1pt.R` with the new models (it checks itself at the end).
2. Publish a new GitHub Release (for example tag `grid-2027-01-15`) with `grid_index.rds` and every `*_part*.bin`
   file attached, all with the same names. Do not mark it as a pre-release.
3. The online app uses the new release the next time it starts. Restart it in Posit Connect Cloud to switch at once.

The new predictions must have the same structure (same programs, inputs and outcomes). New inputs or outcomes need
changes to `app.R`.

## Updating the app itself

Edit `app.R`, then commit and push. Posit Connect Cloud republishes automatically. If a new R package is used, rerun
`rsconnect::writeManifest(appFiles = "app.R")` and commit `manifest.json`.
