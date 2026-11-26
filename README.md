# Colorectal Cancer Screening Decision Tool

## ▶ [Open the tool](#open-the-tool)

<a id="open-the-tool"></a>
**Link coming soon.** The online tool is being published; this link will go straight to it.

A free online tool for health systems, public health programs and policymakers planning colorectal cancer screening.
Choose your population, the share reached by each colonoscopy or FIT intervention, screening rates and prices, then see
projected costs, cancers averted and life-years gained. Nothing to install: it runs in your web browser.

---

<details>
<summary><b>How to use the tool</b></summary>

1. **Population.** Pick who you are planning for (for example a census-based population) and how many people.
2. **Programs.** Turn on one or more programs, set the share of the population each one reaches, and set screening
   before, during and after the intervention (any whole-number percentage).
3. **Prices.** Keep the default prices or enter your own, including an optional program cost per person reached.
4. **Results.** Read the cost and health-outcome cards and the per-program table; add more outcomes from the list.

</details>

<details>
<summary><b>Where the numbers come from</b></summary>

Outcomes come from random forest metamodels of a colorectal cancer simulation model. The forests were run ahead of
time for every whole-number screening trajectory (before ≤ after ≤ during), for each of 180 person types, so the
tool shows the forests' own predictions instantly. Costs are predicted procedure counts and cancer-care quantities
multiplied by the prices on screen.

</details>

<details>
<summary><b>Technical details (for maintainers)</b></summary>

**Files in this repository:** `app.R` (the R Shiny app), `manifest.json` (R packages, for hosting), this README.

**Predictions** are attached to the newest
[GitHub Release](https://github.com/ashleystanfield/crc-screening-tool/releases/latest), not stored in the code:

- `grid_index.rds`: the trajectory list, quantity order and which part file holds each trajectory
- `<program>_partN.bin`: float32 predictions, one block of 180 person types x 35 quantities per trajectory

At start-up the app finds the newest release, downloads `grid_index.rds`, and then fetches only the ~25 KB block for
the trajectory a user picks. If a folder `grid/` with these files sits next to `app.R`, the app reads it instead.

**Updating to new metamodels**

1. Run `make_tool_grid_1pt.R` (kept locally) with the new models; it checks itself at the end.
2. Publish a new release (for example tag `grid-2027-01-15`) with `grid_index.rds` and every `*_part*.bin` file,
   same names. Do not mark it as a pre-release.
3. The online tool uses the new release the next time it starts (restart it in Posit Connect Cloud to switch at once).

New inputs or outcomes need changes to `app.R`.

**Updating the app:** edit `app.R`, commit and push; Posit Connect Cloud republishes automatically. If a new R package
is used, rerun `rsconnect::writeManifest(appFiles = "app.R")` and commit `manifest.json`.

**Run locally:** `shiny::runApp()` in this folder (needs the shiny and curl packages).

</details>
