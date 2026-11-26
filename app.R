# =============================================================================
# Metamodel-Based Colorectal Cancer Screening Decision Tool (v2)  --  R Shiny stakeholder tool
# Set how much of the population is on each program (MCIC/MCIF/MFIC/MFIF), each program's screening before /
# during / after the intervention, and the prices; see blended cost and health outcomes.
#
# Outcomes come from the final random forest metamodels, run ahead of time (make_tool_grid_1pt.R) for every
# whole-number screening trajectory, so no model is loaded here.
# Cost is computed as: predicted procedure counts x unit prices + predicted
# cancer-care quantities x stage- and phase-specific prices. Every price can be changed in the Prices panel.
# No intervention (program delivery) cost is included.
# Run:   shiny::runApp()   from this folder (needs shiny and curl)
#
# Predictions: grid_index.rds (small) plus <combo>_partN.bin files (float32, one 180 x 35 block per trajectory).
# If a folder "grid" with these files sits next to app.R, the app reads it (local use). Otherwise it uses the newest
# GitHub Release of GRID_REPO: it downloads grid_index.rds at start-up and then fetches only the ~25 KB block for the
# trajectory a user picks. Publishing a new release with new files updates the online tool with no code change.
# =============================================================================
library(shiny)
GRID_REPO <- "ashleystanfield/crc-screening-tool"
GRID_SRC <- local({
  if (file.exists(file.path("grid", "grid_index.rds"))) return(list(local = TRUE, base = "grid"))
  h <- curl::new_handle(followlocation = FALSE, nobody = TRUE)
  r <- curl::curl_fetch_memory(sprintf("https://github.com/%s/releases/latest", GRID_REPO), handle = h)
  loc <- curl::parse_headers_list(r$headers)[["location"]]
  if (is.null(loc) || !grepl("/releases/tag/", loc)) stop("Could not find the latest release of ", GRID_REPO)
  tag <- sub(".*/releases/tag/", "", loc)
  list(local = FALSE, base = sprintf("https://github.com/%s/releases/download/%s", GRID_REPO, tag), tag = tag)
})
GRID <- local({
  f <- file.path(GRID_SRC$base, "grid_index.rds")
  if (!GRID_SRC$local) {
    dest <- file.path(tempdir(), "grid_index.rds")
    curl::curl_download(f, dest, handle = curl::new_handle(followlocation = TRUE)); f <- dest
  }
  readRDS(f)
})
if (!is.list(GRID) || !all(c("meta", "combos") %in% names(GRID))) stop("The predictions index is not a tool grid.")
GRID_KEYS <- lapply(GRID$combos, function(g) paste(g$grid$before, g$grid$during, g$grid$after))
# one trajectory's predictions (180 persons x quantities) from its part file
read_row <- function(k, r) {
  m <- GRID$meta; bpr <- m$bytes_per_row
  part <- GRID$combos[[k]]$parts[(r - 1) %/% m$rows_per_part + 1]
  off <- ((r - 1) %% m$rows_per_part) * bpr
  raw_block <- if (GRID_SRC$local) {
    con <- file(file.path(GRID_SRC$base, part), "rb"); on.exit(close(con)); seek(con, off); readBin(con, "raw", bpr)
  } else {
    url <- file.path(GRID_SRC$base, part); got <- NULL
    for (try_n in 1:3) {
      h <- curl::new_handle(followlocation = TRUE, range = sprintf("%.0f-%.0f", off, off + bpr - 1))
      res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
      if (!is.null(res) && res$status_code == 206 && length(res$content) == bpr) { got <- res$content; break }
      Sys.sleep(0.5)
    }
    if (is.null(got)) stop("Could not read predictions from ", url)
    got
  }
  matrix(readBin(raw_block, "numeric", n = bpr / 4, size = 4, endian = "little"), m$n_person,
         dimnames = list(NULL, m$quantities))
}

# ---- prices (default prices; 2026 dollars, not discounted) ---------------------
DEFAULT_PRICES <- list(
  screen = 691.44, sympt = 821.44, survei = 611.00, diag = 776.40, fit = 15.92,
  eol  = c(119097.8345, 134258.3367, 140563.0496, 176747.7226),
  init = c(56293.87787, 79834.40379, 115975.9813, 172345.5231),
  cont = c(6655.975254, 7761.236729, 12023.641, 56922.70926),
  neol = c(43602.89042, 46921.05469, 64162.23616, 154343.3043))
PROC_PRICE_LABELS <- c(screen = "Screening (routine) colonoscopy", sympt = "Symptomatic colonoscopy",
                       survei = "Surveillance colonoscopy", diag = "Diagnostic (follow-up) colonoscopy", fit = "FIT test")
PHASE_LABELS <- c(init = "Initial care (survived ≥ 1 year)", cont = "Continuing care, per year",
                  eol = "End of life, died of CRC", neol = "End of life, died of other cause")

# ---- 180 census person types (US adults 45-74), gender->race->age order ------
w <- c(
  1515627,1488055,1522187,1625140,1737660,1680184,1631290,1595939,1618951,1641612,1715154,1751555,1747661,1777326,1758660,
  1725618,1699694,1691459,1630251,1573742,1531923,1472594,1419601,1377160,1266357,1229386,1202556,1216572,1228905,841133,
  251798,247715,256483,265573,277712,267316,254038,253067,254008,262465,267974,264693,261234,257417,255099,
  251726,240954,233856,222290,210964,199268,182287,169376,159231,150340,142509,131010,122198,113795,86102,
  210169,206569,206557,205598,206020,196655,188942,176632,170924,171814,172141,172628,170656,159725,157513,
  155203,147889,145569,139744,134948,129683,120474,115423,105666,101711,100544,93955,88183,83786,66184,
  1468157,1443686,1479626,1574896,1676846,1627517,1586794,1555202,1593255,1621646,1712994,1750358,1761161,1791909,1784401,
  1752513,1744138,1745425,1693173,1653679,1620460,1573527,1529486,1489639,1389251,1354438,1333202,1351121,1349732,967560,
  280525,277565,286615,295347,309228,294077,280736,281344,284949,293616,302240,297643,295600,293831,291273,
  288973,279042,272039,264906,254313,243046,228313,215105,206806,197925,188719,176262,165102,152376,121784,
  227332,224407,224820,223280,223265,213093,204845,194240,186720,188443,189966,192538,188933,178491,177946,
  175410,169908,166692,161228,158802,154055,143575,139149,129228,126314,124414,114883,107541,102118,81313)
PT <- expand.grid(age = 45:74, race = 1:3, gender = 1:2)
PT$w <- w; PT$person_idx <- seq_len(nrow(PT)); CENSUS_TOTAL <- sum(PT$w)

# census marginals (for defaults) + raking to build 180 weights from margins
BAND <- (PT$age - 45) %/% 5 + 1
AGE_BANDS <- c("45–49","50–54","55–59","60–64","65–69","70–74")
CENS_BAND <- as.numeric(tapply(PT$w, BAND, sum)) / CENSUS_TOTAL
CENS_RACE <- as.numeric(tapply(PT$w, PT$race, sum)) / CENSUS_TOTAL
CENS_SEX  <- as.numeric(tapply(PT$w, PT$gender, sum)) / CENSUS_TOTAL
rake_weights <- function(tgtBand, tgtRace, tgtSex, iters = 40) {
  w <- PT$w
  for (it in seq_len(iters)) {
    for (b in 1:6) { i <- BAND==b;       s <- sum(w[i]); if (s>0) w[i] <- w[i]*tgtBand[b]/s }
    for (r in 1:3) { i <- PT$race==r;    s <- sum(w[i]); if (s>0) w[i] <- w[i]*tgtRace[r]/s }
    for (g in 1:2) { i <- PT$gender==g;  s <- sum(w[i]); if (s>0) w[i] <- w[i]*tgtSex[g]/s }
  }
  w / sum(w)
}
# census age-band x sex joint (6x2), and raking to a known joint + race margin
CENS_AS <- tapply(PT$w, list(BAND, PT$gender), sum) / CENSUS_TOTAL   # 6 x 2
rake_joint_as <- function(tgtAS, tgtRace, iters = 40) {   # tgtAS: 6x2 proportions; tgtRace: length 3
  w <- PT$w
  for (it in seq_len(iters)) {
    for (b in 1:6) for (g in 1:2) { i <- BAND==b & PT$gender==g; s <- sum(w[i]); if (s>0) w[i] <- w[i]*tgtAS[b,g]/s }
    for (r in 1:3) { i <- PT$race==r; s <- sum(w[i]); if (s>0) w[i] <- w[i]*tgtRace[r]/s }
  }
  w / sum(w)
}

PROGS <- list(
  MCIC = list(label = "Colonoscopy intervention for those with colonoscopy preference", col = "#01927e"),
  MCIF = list(label = "FIT intervention for those with colonoscopy preference",         col = "#0467b1"),
  MFIF = list(label = "FIT intervention for those with FIT preference",                 col = "#014b81"),
  MFIC = list(label = "Colonoscopy intervention for those with FIT preference",         col = "#c77d0a"))
ORDER <- names(PROGS); UNCOV <- "#9aa8ad"

# ---- screening trajectory -> pre-computed predictions ----------------------------------------
# colonoscopy-primary programs: 10-year colonoscopy %; FIT-primary programs: annual FIT %
COLO_PRIMARY <- c(MCIC = TRUE, MCIF = TRUE, MFIC = FALSE, MFIF = FALSE)
LIM <- function(C) if (COLO_PRIMARY[[C]]) list(bmax = 50, dmax = 80, unit = "10-year colonoscopy") else
                                          list(bmax = 40, dmax = 60, unit = "annual FIT")
# snap to the model grid (whole numbers) and enforce before <= after <= during (the design constraint)
GRID_STEP <- GRID$meta$step
snap <- function(C, b, d, a) {
  L <- LIM(C); rs <- function(v) GRID_STEP * round(v / GRID_STEP)
  b <- min(L$bmax, max(0, rs(b))); d <- min(L$dmax, max(b, rs(d))); a <- min(d, max(b, rs(a)))
  c(before = b, during = d, after = a)
}
.qcache <- new.env()
# 180 x quantities matrix of per-person predictions for one program and trajectory
lookup <- function(C, b, d, a) {
  s <- snap(C, b, d, a); key <- paste(C, s[1], s[2], s[3], sep = "|")
  if (!is.null(.qcache[[key]])) return(.qcache[[key]])
  k <- tolower(C)
  i <- match(paste(s[["before"]], s[["during"]], s[["after"]]), GRID_KEYS[[k]])
  if (is.na(i)) stop("Trajectory not in the prediction grid: ", key)
  Q <- read_row(k, i)
  .qcache[[key]] <- Q; Q
}
# Eq. 4: per-person cost components from predicted quantities and prices
price_Q <- function(Q, P) {
  q <- function(n) Q[, n]
  colo <- P$screen * (q("routine_colo_before") + q("routine_colo_during") + q("routine_colo_after")) +
          P$sympt  * (q("sympt_colo_before")  + q("sympt_colo_during")  + q("sympt_colo_after")) +
          P$survei * (q("survei_colo_before") + q("survei_colo_during") + q("survei_colo_after")) +
          P$diag   * (q("diag_colo_before")   + q("diag_colo_at")       + q("diag_colo_after"))
  fit <- P$fit * (q("num_fits_before") + q("num_fits_during") + q("num_fits_after"))
  canc <- 0
  for (s in 1:4) canc <- canc + P$eol[s] * q(paste0("cc_eol_", s)) + P$neol[s] * q(paste0("cc_neol_", s)) +
                                P$init[s] * q(paste0("cc_init_", s)) + P$cont[s] * q(paste0("cc_cont_", s))
  cbind(proc = colo, fit = fit, cancer_care = canc, total = colo + fit + canc)
}

# ---- formatting -------------------------------------------------------------
dol  <- function(v) ifelse(is.na(v)|is.infinite(v), "—", paste0(ifelse(v < 0, "−$", "$"), formatC(round(abs(v)), big.mark=",", format="d")))
dolK <- function(v) { if (is.na(v)) return("—"); sg <- if (v < 0) "−" else ""; a <- abs(v)
  paste0(sg, if (a>=1e9) sprintf("$%.1fB",a/1e9) else if (a>=1e6) sprintf("$%.1fM",a/1e6) else if (a>=1e3) sprintf("$%.0fK",a/1e3) else dol(a)) }
numK <- function(v) if (is.na(v)) "—" else if (abs(v)>=1e6) sprintf("%.2fM",v/1e6) else if (abs(v)>=1e3) sprintf("%.1fK",v/1e3) else formatC(round(v), big.mark=",", format="d")
comma<- function(v) formatC(round(v), big.mark=",", format="d")
pctf <- function(v) sprintf("%.1f%%", v*100)

# ---- optional extra outcomes (every pre-computed quantity beyond the cards) ----------------
OUT_LABELS <- c(cancer_death = "Colorectal cancer deaths", cancer_at_all = "All colorectal cancers diagnosed",
  routine_colo_before = "Routine colonoscopies, before", routine_colo_during = "Routine colonoscopies, during",
  routine_colo_after = "Routine colonoscopies, after", sympt_colo_before = "Symptomatic colonoscopies, before",
  sympt_colo_during = "Symptomatic colonoscopies, during", sympt_colo_after = "Symptomatic colonoscopies, after",
  survei_colo_before = "Surveillance colonoscopies, before", survei_colo_during = "Surveillance colonoscopies, during",
  survei_colo_after = "Surveillance colonoscopies, after", diag_colo_before = "Diagnostic colonoscopies, before",
  diag_colo_at = "Diagnostic colonoscopies, during", diag_colo_after = "Diagnostic colonoscopies, after",
  num_fits_before = "FIT tests, before", num_fits_during = "FIT tests, during", num_fits_after = "FIT tests, after")
EXTRAS <- intersect(names(OUT_LABELS), GRID$meta$quantities)
EXTRA_CHOICES <- setNames(EXTRAS, OUT_LABELS[EXTRAS])
fmt_out <- function(v) if (is.na(v)) "—" else sprintf("%.2f per 1,000", v * 1000)

# score covered people; usual=TRUE holds during & after at the before level
score_pop <- function(cov, prices, usual = FALSE) {
  if (!nrow(cov)) return(cov)
  for (v in c("total","proc","fit","cancer_care","progcost","cancer","death","lyl")) cov[[v]] <- NA_real_
  cov$dur <- if (usual) cov$before else cov$during
  cov$aft <- if (usual) cov$before else cov$after
  grps <- unique(cov[, c("prog","before","dur","aft")])
  for (i in seq_len(nrow(grps))) {
    g <- grps[i, ]
    idx <- cov$prog==g$prog & cov$before==g$before & cov$dur==g$dur & cov$aft==g$aft
    Q <- lookup(g$prog, g$before, g$dur, g$aft); C <- price_Q(Q, prices)
    pid <- cov$person_idx[idx]
    cov$total[idx] <- C[pid, "total"]; cov$proc[idx] <- C[pid, "proc"]; cov$fit[idx] <- C[pid, "fit"]
    cov$cancer_care[idx] <- C[pid, "cancer_care"]
    pc <- if (usual || is.null(prices$program[[g$prog]])) 0 else prices$program[[g$prog]]
    cov$progcost[idx] <- pc; cov$total[idx] <- cov$total[idx] + pc
    cov$cancer[idx] <- Q[pid, "cancer_after"]; cov$death[idx] <- Q[pid, "cancer_death"]; cov$lyl[idx] <- Q[pid, "LifeY_lost"]
  }
  cov
}

# ---- population-mix block: X = "ALL" (whole population) or a program code ----------------
POP_MODES <- c("US census (45–74)" = "census", "Enter age, race and sex percentages" = "props",
               "Enter age-by-sex and race percentages" = "jointas",
               "Upload counts or percentages for all 180 person types" = "upload")
popMixUI <- function(X) {
  who <- if (X == "ALL") "your population's" else "this program's"
  sub <- function(t) tags$div(style="font-size:12px;color:#42555e;margin:4px 0 2px", t)
  cond <- function(m) sprintf("input.popmode_%s == '%s'", X, m)
  tagList(
    selectInput(paste0("popmode_",X), if (X == "ALL") "Population definition" else "Who is on this program", POP_MODES),
    conditionalPanel(cond("census"),
      if (X == "ALL")
        helpText("We use US census counts to set the share of each of the 180 person types (each single year of age 45–74 × race [White, Black, Other] × sex), so the population matches the US age, race and sex mix.")
      else tagList(
        helpText("This program's people follow the US census mix. Narrow it with the age, race and sex filters below."),
        sliderInput(paste0("age_",X), "Ages", 45, 74, c(45,74), step=1, sep=""),
        div(class="row2",
          checkboxGroupInput(paste0("race_",X), "Race", c("White"="1","Black"="2","Other"="3"), c("1","2","3"), inline=TRUE),
          checkboxGroupInput(paste0("sex_",X), "Sex", c("Male"="1","Female"="2"), c("1","2"), inline=TRUE)))),
    conditionalPanel(cond("props"),
      helpText(sprintf("Enter %s percentage in each age group, race and sex. Edit only the values you know; the rest stay at US census values. Make each group sum to 100%% (otherwise it is rescaled).", who)),
      sub("Age (%)"),
      fluidRow(lapply(1:3, function(i) column(4, numericInput(paste0("ab_",X,"_",i), AGE_BANDS[i], round(CENS_BAND[i]*100,1), 0, 100, 0.1)))),
      fluidRow(lapply(4:6, function(i) column(4, numericInput(paste0("ab_",X,"_",i), AGE_BANDS[i], round(CENS_BAND[i]*100,1), 0, 100, 0.1)))),
      sub("Race (%)"),
      fluidRow(lapply(1:3, function(i) column(4, numericInput(paste0("rc_",X,"_",i), c("White","Black","Other")[i], round(CENS_RACE[i]*100,1), 0, 100, 0.1)))),
      sub("Sex (%)"),
      fluidRow(lapply(1:2, function(i) column(6, numericInput(paste0("sx_",X,"_",i), c("Male","Female")[i], round(CENS_SEX[i]*100,1), 0, 100, 0.1))))),
    conditionalPanel(cond("jointas"),
      helpText(sprintf("Enter %s percentage in each age-and-sex group and each race. Edit only the values you know; the rest stay at US census values. Make the 12 age-and-sex values sum to 100%% and the race values sum to 100%% (otherwise they are rescaled).", who)),
      sub("Age × sex (%)"),
      lapply(1:6, function(b) fluidRow(lapply(1:2, function(g) column(6,
        numericInput(paste0("asm_",X,"_",b,"_",g), paste(AGE_BANDS[b], c("Male","Female")[g]), round(CENS_AS[b,g]*100,2), 0, 100, 0.1))))),
      sub("Race (%)"),
      fluidRow(lapply(1:3, function(i) column(4, numericInput(paste0("rcj_",X,"_",i), c("White","Black","Other")[i], round(CENS_RACE[i]*100,1), 0, 100, 0.1))))),
    conditionalPanel(cond("upload"),
      helpText(HTML(paste0("1. Download the template (180 rows, one per person type, filled with US census counts).<br>
        2. Replace the <b>count</b> column with ", who, " counts or percentages. Do not change the age, race or gender columns (race 1 = White, 2 = Black, 3 = Other; gender 1 = Male, 2 = Female).<br>
        3. Save as CSV and upload it.<br>
        The file sets the mix only; the size comes from ", if (X == "ALL") "the total population." else "this program's share of the total population."))),
      downloadLink(paste0("poptemplate_",X), "Download 180-row template"),
      fileInput(paste0("popfile_",X), NULL, accept = ".csv", buttonLabel = "Upload CSV…"),
      uiOutput(paste0("popfilemsg_",X))))
}

# ---- one program panel ------------------------------------------------------
progPanel <- function(C) {
  meta <- PROGS[[C]]
  def <- switch(C,
    MCIC = list(en=TRUE,  pct=30, before=30, dmode="mod", dval=40, amode="same", aval=30),
    MCIF = list(en=FALSE, pct=10, before=30, dmode="mod", dval=40, amode="same", aval=30),
    MFIF = list(en=TRUE,  pct=70, before=10, dmode="mod", dval=20, amode="same", aval=10),
    MFIC = list(en=FALSE, pct=10, before=10, dmode="mod", dval=20, amode="same", aval=10))
  L <- LIM(C)
  div(class="prog", style=sprintf("border-left:4px solid %s", meta$col),
    div(class="prog-head",
      checkboxInput(paste0("en_",C),
        HTML(sprintf("<b style='color:%s'>%s</b> <span class='prog-sub'>%s</span>", meta$col, C, meta$label)), value=def$en),
      span(class="cov", textOutput(paste0("cov_",C), inline=TRUE))),
    conditionalPanel(sprintf("input.en_%s", C),
      tags$div(class="elig-label", sprintf("Screening trajectory (%s %%)", L$unit)),
      sliderInput(paste0("before_",C), "Before intervention — baseline (%)", 0, L$bmax, def$before, step=1),
      radioButtons(paste0("dmode_",C), "During intervention",
                   c("Moderate (+10 pts)"="mod","Intensive (+20 pts)"="int","Exact %"="exact"), def$dmode, inline=TRUE),
      conditionalPanel(sprintf("input.dmode_%s == 'exact'", C),
                   sliderInput(paste0("dval_",C), "During screening (%)", 0, L$dmax, def$dval, step=1)),
      radioButtons(paste0("amode_",C), "After intervention",
                   c("Same as before"="same","Specify %"="spec"), def$amode, inline=TRUE),
      conditionalPanel(sprintf("input.amode_%s == 'spec'", C),
                   sliderInput(paste0("aval_",C), "After screening (%) — kept between before and during", 0, L$dmax, def$aval, step=1)),
      tags$div(class="elig-label", "Who is on this program (population mix)"),
      popMixUI(C)))
}

# =============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#eef3f5;color:#0e1e26;font-family:Arial,Helvetica,sans-serif}
    .container-fluid{max-width:1180px;margin:0 auto;padding-top:14px}
    h2.title{font-family:Arial,Helvetica,sans-serif;font-weight:600;margin:0 0 2px}
    .eyebrow{font-size:11px;letter-spacing:.14em;text-transform:uppercase;color:#017061;font-weight:700}
    .lead{color:#42555e;font-size:14px;max-width:72ch;margin:4px 0 10px}
    .badge{display:inline-block;font-size:11px;font-weight:700;letter-spacing:.05em;text-transform:uppercase;padding:3px 9px;border-radius:999px;margin-left:6px}
    .badge.demo{color:#c77d0a;background:#fbf0da;border:1px solid #e6c98a}
    .badge.live{color:#017061;background:#d9f0ea;border:1px solid #8fd3c4}
    .prog{background:#fff;border:1px solid #d9e3e7;border-radius:12px;padding:12px 14px;margin-bottom:12px}
    .prog-head{display:flex;justify-content:space-between;align-items:center;gap:8px}
    .prog-sub{color:#42555e;font-weight:500;font-size:13px}
    .elig-label{font-size:11px;color:#8a99a0;text-transform:uppercase;letter-spacing:.06em;font-weight:600;margin:10px 0 2px;border-top:1px dashed #e3ebee;padding-top:8px}
    .cov{font-size:12px;color:#42555e;font-variant-numeric:tabular-nums;white-space:nowrap}
    .row2{display:grid;grid-template-columns:1fr 1fr;gap:6px}
    .prog .form-group{margin-bottom:8px} .prog .control-label{font-size:12px;color:#6b7d85;margin-bottom:2px}
    .cardgrid{display:grid;grid-template-columns:repeat(3,1fr);gap:12px}
    @media(max-width:640px){.cardgrid{grid-template-columns:1fr 1fr}}
    .card{background:#fff;border:1px solid #d9e3e7;border-left:3px solid #01927e;border-radius:12px;padding:14px 15px}
    .card.blue{border-left-color:#0467b1}.card.navy{border-left-color:#014b81}.card.good{border-left-color:#2f8f5b}
    .card.good .v{color:#2f8f5b}
    .card .k{font-size:12px;color:#6b7d85;font-weight:600}
    .card .v{font-size:23px;font-weight:700;font-family:Arial,Helvetica,sans-serif;margin:5px 0 1px;font-variant-numeric:tabular-nums}
    .card .s{font-size:12px;color:#42555e}
    .block{background:#fff;border:1px solid #d9e3e7;border-radius:12px;padding:15px 17px;margin-top:16px}
    .block h4{margin:0 0 8px;font-size:15px}
    .block{overflow-x:auto;min-width:0}
    .block table{width:100%;max-width:100%;font-size:12.5px;margin-bottom:0}
    .block table th{white-space:normal;vertical-align:bottom;line-height:1.2;padding:6px 6px}
    .block table td{white-space:nowrap;padding:6px 6px;font-variant-numeric:tabular-nums}
    .card{min-width:0;overflow:hidden}
    .card .v{overflow-wrap:anywhere}
    .covbar{display:flex;height:26px;border-radius:7px;overflow:hidden;border:1px solid #d9e3e7;margin-bottom:6px}
    .covbar > div{height:100%}
    .covleg{display:flex;flex-wrap:wrap;gap:12px;font-size:12px;color:#42555e}
    .covleg span b{display:inline-block;width:10px;height:10px;border-radius:2px;margin-right:5px;vertical-align:middle}
    table.brk{width:100%;border-collapse:collapse;font-size:13px}
    table.brk th,table.brk td{text-align:right;padding:7px 8px;border-bottom:1px solid #eef3f5;font-variant-numeric:tabular-nums}
    table.brk th:first-child,table.brk td:first-child{text-align:left}
    table.brk thead th{color:#6b7d85;font-weight:600;border-bottom:1px solid #d9e3e7}
    .warn{color:#c0453a;font-size:12.5px;margin:8px 0 0}
  "))),
  tags$h2(class="title", "Metamodel-Based Colorectal Cancer Screening Decision Tool"),
  p(class="lead", "This tool estimates the cancers, cancer deaths, life-years and costs of colorectal cancer screening programs for a population you define, compared with usual care. It uses metamodels trained on the NC-CRC simulation, so you can test different populations, screening levels and prices and see the results instantly."),
  sidebarLayout(
    sidebarPanel(width = 5,
      helpText("Set each program's share of the total population, then choose who is on each program in its panel below."),
      numericInput("popTotal", "Total population", 116615996, min=1, step=1000),
      tags$hr(style="margin:12px 0"),
      tags$h4(style="font-size:15px;margin:0 0 4px", "Screening Preference and Program"),
      div(class="prog", style="border-left:4px solid #5b6b72",
        div(class="prog-head", tags$b("Share of population on each program")),
        helpText("Enter the percentage of the total population on each program. Any remainder stays on usual care. Programs that are switched off below are not counted."),
        fluidRow(lapply(ORDER, function(C) column(6,
          numericInput(paste0("pct_",C), sprintf("%s (%%)", C),
                       c(MCIC = 30, MCIF = 0, MFIF = 70, MFIC = 0)[[C]], min = 0, max = 100, step = 1)))),
        uiOutput("sharemsg")),
      p(style="font-size:12.5px;color:#6b7d85;margin:0 0 10px",
        "Then switch on each program and set its screening levels."),
      lapply(ORDER, progPanel),
      div(class="prog", style="border-left:4px solid #5b6b72",
        div(class="prog-head", tags$b("Prices")),
        tags$div(class="elig-label", "Procedures (per test)"),
        lapply(names(PROC_PRICE_LABELS), function(k)
          numericInput(paste0("p_", k), PROC_PRICE_LABELS[[k]], DEFAULT_PRICES[[k]], min = 0, step = 1)),
        tags$div(class="elig-label", "Cancer care, by stage at diagnosis (per person)"),
        lapply(names(PHASE_LABELS), function(ph) tagList(
          tags$div(style="font-size:12px;color:#42555e;margin:6px 0 2px", PHASE_LABELS[[ph]]),
          fluidRow(lapply(1:4, function(s) column(6,
            numericInput(paste0("p_", ph, "_", s), paste("Stage", s), round(DEFAULT_PRICES[[ph]][s]), min = 0, step = 100)))))),
        tags$div(class="elig-label", "Program cost (per person reached)"),
        helpText("Enter the cost of delivering each program (for example outreach, mailing or navigation) per person it covers. Leave blank to exclude."),
        fluidRow(lapply(ORDER, function(C) column(6,
          numericInput(paste0("p_prog_", C), paste(C, "program"), value = NA, min = 0, step = 1)))),
        actionButton("resetPrices", "Reset to default prices", class = "btn-sm"),
        helpText("Cost = predicted procedure counts × procedure prices + predicted cancer-care quantities × these prices. Program costs are added only when entered above."))
    ),
    mainPanel(width = 7,
      div(class="block", tags$h4("Program mix"),
        uiOutput("covtotal"), uiOutput("covbar"), uiOutput("covleg"), uiOutput("covwarn")),
      uiOutput("cards"),
      div(class="block", tags$h4("By program"), tableOutput("brk")),
      if (length(EXTRAS)) div(class="block",
        tags$h4("Other outcomes"),
        p(style="font-size:12.5px;color:#6b7d85;margin:-2px 0 8px", "Per 1,000 covered people, weighted by your population. Off by default — check any to show."),
        tags$div(style="max-height:160px;overflow:auto;border:1px solid #eef3f5;border-radius:8px;padding:6px 10px;margin-bottom:8px",
          checkboxGroupInput("extras", NULL, choices = EXTRA_CHOICES)),
        tableOutput("extratab"))
    )
  )
)

# =============================================================================
server <- function(input, output, session) {

  `%||%` <- function(x, y) if (is.null(x) || (length(x)==1 && is.na(x))) y else x

  # ---- population: total size + each program's population mix (180 relative weights) ----
  popN <- reactive(max(1, input$popTotal %||% CENSUS_TOTAL))

  # uploaded 180-type file for one program: read, check, and report what was used
  readUpload <- function(C) {
    f <- input[[paste0("popfile_", C)]]
    if (is.null(f)) return(list(ok = FALSE, msg = "No file uploaded yet: using US census values."))
    df <- tryCatch(read.csv(f$datapath, check.names = FALSE), error = function(e) NULL)
    if (is.null(df)) return(list(ok = FALSE, msg = "Could not read the file. Save it as CSV and try again. Using US census values."))
    names(df) <- tolower(trimws(names(df)))
    need <- c("age", "race", "gender", "count")
    if (!all(need %in% names(df))) return(list(ok = FALSE, msg = paste0(
      "Missing column(s): ", paste(setdiff(need, names(df)), collapse = ", "), ". Use the template. Using US census values.")))
    df$count <- suppressWarnings(as.numeric(df$count))
    key <- paste(df$age, df$race, df$gender)
    cnt <- df$count[match(paste(PT$age, PT$race, PT$gender), key)]
    missing <- sum(is.na(cnt)); cnt[is.na(cnt) | cnt < 0] <- 0
    if (sum(cnt) <= 0) return(list(ok = FALSE, msg = "All values are zero or blank. Using US census values."))
    extra <- sum(!key %in% paste(PT$age, PT$race, PT$gender))
    msg <- sprintf("Using your file: %d of 180 person types matched%s%s.", 180 - missing,
                   if (missing) sprintf(" (%d missing, set to 0)", missing) else "",
                   if (extra) sprintf("; %d row(s) ignored (age, race or gender out of range)", extra) else "")
    list(ok = TRUE, cnt = cnt, msg = msg, warn = missing > 0 || extra > 0)
  }
  for (C in c("ALL", ORDER)) local({
    Cc <- C
    output[[paste0("poptemplate_", Cc)]] <- downloadHandler(
      filename = function() "population_180_template.csv",
      content = function(file) write.csv(
        data.frame(age=PT$age, race=PT$race, gender=PT$gender, count=round(PT$w)), file, row.names=FALSE))
    output[[paste0("popfilemsg_", Cc)]] <- renderUI({
      u <- readUpload(Cc)
      p(style = sprintf("font-size:12.5px;margin:4px 0 0;color:%s", if (!u$ok || isTRUE(u$warn)) "#c0453a" else "#2f8f5b"), u$msg)
    })
  })

  # relative weights of the 180 person types among one program's people (non-census modes)
  progW <- function(C) {
    mode <- input[[paste0("popmode_", C)]] %||% "census"
    v <- function(name, def) { x <- input[[name]]; if (is.null(x) || is.na(x) || x < 0) def else x }
    if (mode == "upload") { u <- readUpload(C); return(if (u$ok) u$cnt / sum(u$cnt) else PT$w / CENSUS_TOTAL) }
    if (mode == "props") {
      b <- vapply(1:6, function(i) v(paste0("ab_",C,"_",i), CENS_BAND[i]*100), numeric(1))
      r <- vapply(1:3, function(i) v(paste0("rc_",C,"_",i), CENS_RACE[i]*100), numeric(1))
      s <- vapply(1:2, function(i) v(paste0("sx_",C,"_",i), CENS_SEX[i]*100), numeric(1))
      if (sum(b)<=0 || sum(r)<=0 || sum(s)<=0) return(PT$w / CENSUS_TOTAL)
      return(rake_weights(b/sum(b), r/sum(r), s/sum(s)))
    }
    if (mode == "jointas") {
      AS <- matrix(0, 6, 2)
      for (b in 1:6) for (g in 1:2) AS[b,g] <- v(paste0("asm_",C,"_",b,"_",g), CENS_AS[b,g]*100)
      r <- vapply(1:3, function(i) v(paste0("rcj_",C,"_",i), CENS_RACE[i]*100), numeric(1))
      if (sum(AS)<=0 || sum(r)<=0) return(PT$w / CENSUS_TOTAL)
      return(rake_joint_as(AS/sum(AS), r/sum(r)))
    }
    PT$w / CENSUS_TOTAL
  }


  # prices (blank or negative inputs fall back to the default value)
  prices <- reactive({
    P <- DEFAULT_PRICES
    for (k in names(PROC_PRICE_LABELS)) { v <- input[[paste0("p_", k)]]; if (!is.null(v) && !is.na(v) && v >= 0) P[[k]] <- v }
    for (ph in names(PHASE_LABELS)) for (s in 1:4) {
      v <- input[[paste0("p_", ph, "_", s)]]; if (!is.null(v) && !is.na(v) && v >= 0) P[[ph]][s] <- v }
    P$program <- setNames(lapply(ORDER, function(C) { v <- input[[paste0("p_prog_", C)]]
      if (is.null(v) || is.na(v) || v < 0) 0 else v }), ORDER)
    P
  })
  observeEvent(input$resetPrices, {
    for (k in names(PROC_PRICE_LABELS)) updateNumericInput(session, paste0("p_", k), value = DEFAULT_PRICES[[k]])
    for (ph in names(PHASE_LABELS)) for (s in 1:4)
      updateNumericInput(session, paste0("p_", ph, "_", s), value = round(DEFAULT_PRICES[[ph]][s]))
    for (C in ORDER) updateNumericInput(session, paste0("p_prog_", C), value = NA)
  })

  # resolve each program's before/during/after % from its controls (snapped to the model grid)
  rates_of <- function(C) {
    L <- LIM(C)
    b <- input[[paste0("before_",C)]] %||% 0
    d <- switch(input[[paste0("dmode_",C)]] %||% "mod",
                mod = min(L$dmax, b+10), int = min(L$dmax, b+20), exact = input[[paste0("dval_",C)]] %||% b)
    a <- if ((input[[paste0("amode_",C)]] %||% "same") == "same") b else (input[[paste0("aval_",C)]] %||% b)
    snap(C, b, d, a)
  }

  for (C in ORDER) local({
    Cc <- C
    output[[paste0("cov_", Cc)]] <- renderText({
      n <- covByProg()[[Cc]]
      if (isTRUE(input[[paste0("en_", Cc)]]) && n > 0) sprintf("= %s (%s)", comma(n), pctf(n/allocation()$total)) else ""
    })
  })

  allocation <- reactive({
    total <- popN()
    rows <- list(); warns <- character(0); assigned <- 0
    for (C in ORDER) {
      if (!isTRUE(input[[paste0("en_",C)]])) next
      {
        pct <- input[[paste0("pct_",C)]]; if (is.null(pct) || is.na(pct) || pct <= 0) next
        if ((input[[paste0("popmode_",C)]] %||% "census") == "census") {
          rng <- input[[paste0("age_",C)]] %||% c(45, 74)
          races <- as.integer(input[[paste0("race_",C)]]); sexes <- as.integer(input[[paste0("sex_",C)]])
          if (!length(races) || !length(sexes)) { warns <- c(warns, sprintf("%s has no race or sex selected.", C)); next }
          pool <- PT[PT$age>=rng[1] & PT$age<=rng[2] & PT$race %in% races & PT$gender %in% sexes, ]
        } else {
          pool <- PT; pool$w <- progW(C); pool <- pool[pool$w > 0, ]
        }
        if (!nrow(pool) || sum(pool$w) <= 0) { warns <- c(warns, sprintf("%s has no one in its selected population mix.", C)); next }
        cnt <- pct/100 * total
        pool$count <- cnt * pool$w / sum(pool$w)
      }
      r <- rates_of(C)
      pool$prog <- C; pool$before <- as.numeric(r["before"]); pool$during <- as.numeric(r["during"]); pool$after <- as.numeric(r["after"])
      rows[[C]] <- pool[, c("person_idx","age","race","gender","prog","before","during","after","count")]
      assigned <- assigned + cnt
    }
    alloc <- if (length(rows)) do.call(rbind, rows) else
      data.frame(person_idx=integer(0), age=integer(0), race=integer(0), gender=integer(0),
                 prog=character(0), before=numeric(0), during=numeric(0), after=numeric(0), count=numeric(0))
    list(alloc = alloc, total = total, uncovered = max(0, total - assigned), warns = warns)
  })

  covered     <- reactive(allocation()$alloc)
  scored      <- reactive(score_pop(covered(), prices()))
  scoredUsual <- reactive(score_pop(covered(), prices(), usual = TRUE))

  covByProg <- reactive({
    a <- allocation()
    v <- sapply(ORDER, function(C) sum(a$alloc$count[a$alloc$prog == C]))
    c(v, NONE = a$uncovered)
  })

  output$covtotal <- renderUI({
    a <- allocation()
    p(style="font-size:13px;color:#42555e;margin:0 0 8px",
      HTML(sprintf("Total population <strong>%s</strong> &middot; on a program <strong>%s</strong> &middot; usual care <strong>%s</strong>",
        comma(a$total), comma(a$total - a$uncovered), comma(a$uncovered))))
  })
  output$covbar <- renderUI({
    a <- allocation(); cbp <- covByProg(); tot <- a$total; if (tot <= 0) return(NULL)
    div(class="covbar", lapply(c(ORDER,"NONE"), function(C){
      if (cbp[[C]] <= 0) return(NULL)
      div(style=sprintf("width:%.3f%%;background:%s", cbp[[C]]/tot*100, if (C=="NONE") UNCOV else PROGS[[C]]$col),
          title=sprintf("%s: %s", C, comma(cbp[[C]]))) }))
  })
  output$covleg <- renderUI({
    a <- allocation(); cbp <- covByProg(); tot <- a$total; if (tot <= 0) return(NULL)
    div(class="covleg", lapply(c(ORDER,"NONE"), function(C){
      if (cbp[[C]] <= 0) return(NULL)
      tags$span(HTML(sprintf("<b style='background:%s'></b>%s — %s (%s)",
        if (C=="NONE") UNCOV else PROGS[[C]]$col, if (C=="NONE") "Usual care" else C, comma(cbp[[C]]), pctf(cbp[[C]]/tot)))) }))
  })
  output$sharemsg <- renderUI({
    on  <- ORDER[vapply(ORDER, function(C) isTRUE(input[[paste0("en_",C)]]), logical(1))]
    val <- function(C) { v <- input[[paste0("pct_",C)]]; if (is.null(v) || is.na(v) || v < 0) 0 else v }
    tot <- sum(vapply(on, val, numeric(1)))
    off <- setdiff(ORDER, on); off <- off[vapply(off, val, numeric(1)) > 0]
    st <- "font-size:12.5px;margin:4px 0 0;color:%s"
    main <- if (tot > 100.0001)
      p(style = sprintf(st, "#c0453a"), sprintf("Programs add up to %.1f%% of the population, which is more than 100%%. Lower one or more shares.", tot))
    else if (tot < 99.9999)
      p(style = sprintf(st, "#42555e"), sprintf("Programs add up to %.1f%%; the remaining %.1f%% stays on usual care.", tot, 100 - tot))
    else p(style = sprintf(st, "#2f8f5b"), "Programs add up to 100% of the population.")
    tagList(main, if (length(off)) p(style = sprintf(st, "#c77d0a"),
      sprintf("%s %s switched off below, so %s share is not counted.", paste(off, collapse = " and "),
              if (length(off) > 1) "are" else "is", if (length(off) > 1) "their" else "its")))
  })
  output$covwarn <- renderUI({
    msgs <- allocation()$warns; if (!length(msgs)) return(NULL)
    div(lapply(msgs, function(m) p(class="warn", m)))
  })

  output$cards <- renderUI({
    cov <- scored(); usual <- scoredUsual()
    if (!nrow(cov)) return(div(class="block", "Enable at least one program to see outcomes."))
    cnt <- sum(cov$count); cost <- sum(cov$total*cov$count); cases <- sum(cov$cancer*cov$count); lyl <- sum(cov$lyl*cov$count)
    deaths <- sum(cov$death*cov$count)
    averted <- sum(usual$cancer*usual$count) - cases
    lyg <- sum(usual$lyl*usual$count) - lyl
    dcost <- cost - sum(usual$total*usual$count)
    ce <- if (lyg > 0 && dcost <= 0) "Saves money and life-years" else if (lyg > 0) paste(dol(dcost/lyg), "per life-year gained") else
          if (dcost < 0) "Saves money, no life-years gained" else "No life-years gained"
    card <- function(cls,k,v,s) div(class=paste("card",cls), div(class="k",k), div(class="v",v), div(class="s",s))
    div(class="cardgrid",
      card("good","Cancer cases averted", comma(averted), "vs. usual care (screening unchanged)"),
      card("good","Life-years gained", comma(lyg), "vs. usual care"),
      card(if (dcost <= 0) "good" else "","Cost vs. usual care", dolK(dcost), ce),
      card("","Cost per person", dol(cost/cnt), sprintf("procedures %s · FIT %s · cancer care %s · program %s",
           dol(sum(cov$proc*cov$count)/cnt), dol(sum(cov$fit*cov$count)/cnt), dol(sum(cov$cancer_care*cov$count)/cnt),
           dol(sum(cov$progcost*cov$count)/cnt))),
      card("blue","Cancer cases", numK(cases), sprintf("%.1f per 1,000 after the intervention", cases/cnt*1000)),
      card("navy","Cancer deaths", numK(deaths), sprintf("%.1f per 1,000", deaths/cnt*1000)),
      card("navy","Life-years lost", numK(lyl), "to colorectal cancer"),
      card("","Total cost", dolK(cost), "across covered people"),
      card("blue","Cost per life-year lost", dol(cost/lyl), "total ÷ LY lost"))
  })

  output$brk <- renderTable({
    cov <- scored(); usual <- scoredUsual(); if (!nrow(cov)) return(data.frame())
    pop <- allocation()$total
    do.call(rbind, lapply(ORDER, function(C){
      s <- cov[cov$prog==C, ]; if (!nrow(s)) return(NULL)
      u <- usual[usual$prog==C, ]
      cnt<-sum(s$count); cost<-sum(s$total*s$count); cases<-sum(s$cancer*s$count)
      averted <- sum(u$cancer*u$count) - cases; lyg <- sum(u$lyl*u$count) - sum(s$lyl*s$count)
      dcost <- cost - sum(u$total*u$count)
      data.frame(Program=C, People=comma(cnt), `% pop`=pctf(cnt/pop),
                 `Cost/person`=dol(cost/cnt), Cases=numK(cases), Averted=comma(averted), `LY gained`=comma(lyg),
                 `Cost vs usual`=dolK(dcost), `Total cost`=dolK(cost), check.names=FALSE)
    }))
  }, align="lrrrrrrrr", sanitize.text.function=function(x) x, width="100%")

  output$extratab <- renderTable({
    sel <- input$extras; cv <- covered()
    if (is.null(sel) || !length(sel) || !nrow(cv)) return(data.frame())
    grps <- unique(cv[, c("prog","before","during","after")])
    do.call(rbind, lapply(sel, function(o) {
      num <- 0; den <- 0
      for (i in seq_len(nrow(grps))) {
        g <- grps[i, ]
        idx <- cv$prog==g$prog & cv$before==g$before & cv$during==g$during & cv$after==g$after
        v <- lookup(g$prog, g$before, g$during, g$after)[, o]
        num <- num + sum(v[cv$person_idx[idx]] * cv$count[idx]); den <- den + sum(cv$count[idx])
      }
      data.frame(Outcome = OUT_LABELS[[o]], Value = fmt_out(if (den>0) num/den else NA), check.names = FALSE)
    }))
  }, align="lr", sanitize.text.function=function(x) x, width="100%")
}

shinyApp(ui, server)

