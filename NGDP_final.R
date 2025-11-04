## ===========================
  # Mixed-Frequency MIDAS (Nealmon) — COMPLETE SCRIPT
  # ===========================

# --- Packages
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(midasr)

# --- Read data
predictors <- read_excel("predictors.xlsx")
predictors2 <- read_excel("predictors2.xlsx")

quart_gdp  <- read_excel("quart_gdp.xlsx")

# --- Helpers
# Increment a (year, period) start by 1 within a given frequency
.inc_start <- function(y, p, freq){
  p2 <- p + 1
  if(p2 > freq) c(y + 1, 1) else c(y, p2)
}
# Safe log-diff (%): guard against <=0 before log
logdiff_pct <- function(x) 100*diff(log(pmax(as.numeric(x), 1e-9)))

# ======================
# 1) Monthly predictors
# ======================
# Normalize odd column names (remove CR/LF etc.)
names(predictors) <- gsub("[\r\n]+", " ", names(predictors))

# Robustly locate needed columns by pattern (handles name quirks)
imports_col <- names(predictors)[grepl("Total.*Merchandise.*Imports", names(predictors), ignore.case=TRUE)][1]
oilgas_col  <- names(predictors)[grepl("Oil.*Gaz.*Exports",          names(predictors), ignore.case=TRUE)][1]
nno_col     <- names(predictors)[grepl("Non.*Oil.*Omani.*Exports",   names(predictors), ignore.case=TRUE)][1]
rex_col     <- names(predictors)[grepl("^Re-?Exports$",              names(predictors), ignore.case=TRUE)][1]
stopifnot(!is.na(imports_col), !is.na(oilgas_col), !is.na(nno_col), !is.na(rex_col))

pred_m <- predictors %>%
  transmute(
    month        = as.Date(month_year),      # works if month_year is POSIXct/Date
    imports      = .data[[imports_col]],
    oilgas       = .data[[oilgas_col]],
    nonoil_omani = .data[[nno_col]],
    reexports    = .data[[rex_col]]
  ) %>%
  arrange(month)

# Monthly ts objects
m_start <- c(year(min(pred_m$month)), month(min(pred_m$month)))
imp_m <- ts(pred_m$imports,      start = m_start, frequency = 12)
oil_m <- ts(pred_m$oilgas,       start = m_start, frequency = 12)
nno_m <- ts(pred_m$nonoil_omani, start = m_start, frequency = 12)
rex_m <- ts(pred_m$reexports,    start = m_start, frequency = 12)

################################################################################################################
## the 2nd set of variables
names(predictors2) <- gsub("[\r\n]+", " ", names(predictors2))
col_oprice <- grep("Average.*oil.*price|US\\$/bbl", names(predictors2), ignore.case=TRUE, value=TRUE)[1]
col_oprod  <- grep("Daily.*Average.*Production",    names(predictors2), ignore.case=TRUE, value=TRUE)[1]
col_gprice <- grep("Natural.*(oil)?\\s*Gas",        names(predictors2), ignore.case=TRUE, value=TRUE)[1]
col_m1     <- grep("Narrow.*Money|\\(M1\\)",        names(predictors2), ignore.case=TRUE, value=TRUE)[1]
col_m2     <- grep("Broad.*Money|\\(M2\\)",         names(predictors2), ignore.case=TRUE, value=TRUE)[1]

mk_ts <- function(col) if (!is.na(col)) ts(predictors2[[col]], start = m_start, frequency = 12) else NULL
oprice_m <- mk_ts(col_oprice)  ; oprod_m  <- mk_ts(col_oprod)
gprice_m <- mk_ts(col_gprice)  ; m1_m     <- mk_ts(col_m1)
m2_m     <- mk_ts(col_m2)

# Growth (% MoM) with same safe log-diff and same starts
oprice_g <- if (!is.null(oprice_m)) ts(logdiff_pct(oprice_m), start = .inc_start(m_start[1], m_start[2], 12), freq=12)
oprod_g  <- if (!is.null(oprod_m))  ts(logdiff_pct(oprod_m),  start = .inc_start(m_start[1], m_start[2], 12), freq=12)
gprice_g <- if (!is.null(gprice_m)) ts(logdiff_pct(gprice_m), start = .inc_start(m_start[1], m_start[2], 12), freq=12)
m1_g     <- if (!is.null(m1_m))     ts(logdiff_pct(m1_m),     start = .inc_start(m_start[1], m_start[2], 12), freq=12)
m2_g     <- if (!is.null(m2_m))     ts(logdiff_pct(m2_m),     start = .inc_start(m_start[1], m_start[2], 12), freq=12)

# Align to the exact quarter span used by y_g (same hf_start/hf_end you already computed)
align12 <- function(x) if (!is.null(x)) window(x, start = hf_start, end = hf_end) else NULL
oprice_g <- align12(oprice_g); oprod_g <- align12(oprod_g); gprice_g <- align12(gprice_g)
m1_g     <- align12(m1_g)    ; m2_g    <- align12(m2_g)
#############################################################################################################################


# ======================
# 2) Quarterly GDP target
# ======================
gdp_q <- quart_gdp %>%
  mutate(qtr = as.yearqtr(gsub("-", " ", year_quarter))) %>%  # "2010-Q1" -> yearqtr
  arrange(qtr) %>%
  select(qtr, NGDP, NGDP_oil, NGDP_Noil)

q_start <- c(year(as.Date(gdp_q$qtr[1])), quarter(as.Date(gdp_q$qtr[1])))

# >>> Choose the target here <<<
y_q <- ts(gdp_q$NGDP, start = q_start, frequency = 4)
# y_q <- ts(gdp_q$NGDP_oil,  start = q_start, frequency = 4)   # alt
# y_q <- ts(gdp_q$NGDP_Noil, start = q_start, frequency = 4)   # alt

# ======================
# 3) Transform to growth and ALIGN spans
# ======================
# Growth series (QoQ for GDP, MoM for monthly), with proper starts
y_g   <- ts(logdiff_pct(y_q),   start = .inc_start(q_start[1], q_start[2], 4),  frequency = 4)
imp_g <- ts(logdiff_pct(imp_m), start = .inc_start(m_start[1], m_start[2], 12), frequency = 12)
oil_g <- ts(logdiff_pct(oil_m), start = .inc_start(m_start[1], m_start[2], 12), frequency = 12)
nno_g <- ts(logdiff_pct(nno_m), start = .inc_start(m_start[1], m_start[2], 12), frequency = 12)
rex_g <- ts(logdiff_pct(rex_m), start = .inc_start(m_start[1], m_start[2], 12), frequency = 12)

# Align monthly series to the exact quarter span of y_g
qy_start <- start(y_g)                          # c(year, quarter)
qy_end   <- end(y_g)                            # c(year, quarter)
hf_start <- c(qy_start[1], (qy_start[2]-1)*3 + 1)  # Jan/Apr/Jul/Oct
hf_end   <- c(qy_end[1],   qy_end[2]*3)            # Mar/Jun/Sep/Dec

imp_g <- window(imp_g, start = hf_start, end = hf_end)
oil_g <- window(oil_g, start = hf_start, end = hf_end)
nno_g <- window(nno_g, start = hf_start, end = hf_end)
rex_g <- window(rex_g, start = hf_start, end = hf_end)

# ==========================================================
# Nealmon-MIDAS: SEPARATE models + suggested small combos
# (search over L_set, robust summary, diagnostics, holdout)
# Assumes y_g and monthly *_g series are ALREADY in memory & aligned.
# ==========================================================
suppressPackageStartupMessages({
  library(midasr)
  library(forecast)
})

# -------- Tunables --------
K <- 3                 # 3 months per quarter
L_set <- 3:6           # search 9–18 monthly lags  (kmax = 3L - 1)
max_predictors <- 5    # safety cap

# -------- Availability helper (no data wrangling here) --------
present <- function(v) exists(v, inherits = TRUE) && !is.null(get(v, inherits = TRUE))

# -------- MIDAS building blocks --------
nealmon_starts <- function(xnames, yname){
  ans <- setNames(replicate(length(xnames), c(0.1,0.1), simplify = FALSE), xnames)
  ans[[yname]] <- c(0.1,0.1,0.1) # for mls(y, c(1,2,4), m=1)
  ans
}

build_formula <- function(yname, xnames, kmax){
  form <- as.formula(paste0(yname, " ~ mls(", yname, ", c(1,2,4), m=1)"))
  for (v in xnames) {
    form <- update(form, paste0(". ~ . + fmls(", v, ", ", kmax, ", ", K, ", nealmon)"))
  }
  form
}

fit_nealmon_over_L <- function(yname, xnames, L_set){
  fits <- lapply(L_set, function(L){
    kmax <- K*L - 1
    form <- build_formula(yname, xnames, kmax)
    starts <- nealmon_starts(xnames, yname)
    fit <- midas_r(formula = form, start = starts, OLS = TRUE)
    list(fit = fit, AIC = AIC(fit), BIC = BIC(fit), L = L, kmax = kmax, x = xnames)
  })
  fits[[ which.min(sapply(fits, `[[`, "AIC")) ]]
}

safe_summary <- function(fit){
  out <- try(summary(fit, robust = TRUE), silent = TRUE)
  if (inherits(out, "try-error")) summary(fit, robust = FALSE) else out
}

diag_report <- function(fit, y_full){
  r <- residuals(fit); f <- fitted(fit)
  y_use <- tail(y_full, length(f))
  RSS <- sum(r^2); TSS <- sum((y_use - mean(y_use))^2)
  R2  <- 1 - RSS/TSS
  n <- length(r); k <- length(coef(fit))
  adjR2 <- 1 - ((1-R2)*(n-1)/(n-k))
  lbp <- try(Box.test(r, lag = 10, type = "Ljung-Box")$p.value, silent = TRUE)
  if (inherits(lbp, "try-error")) lbp <- NA_real_
  list(R2 = R2, AdjR2 = adjR2, Ljung_p = lbp)
}

# simple rolling holdout: last up to 8 quarters
holdout_eval <- function(best, y_full, xnames, n_test = min(8, length(y_full) - 20)){
  if (is.null(best) || n_test <= 0) return(list(rmse=NA, mae=NA, mase=NA, h=0))
  
  # 1) train target
  y_tr <- ts(head(y_full, length(y_full) - n_test), start = start(y_full), frequency = frequency(y_full))
  
  # 2) re-create train-aligned monthly predictors (assumes originals are aligned to full span)
  qy_tr_start <- start(y_tr); qy_tr_end <- end(y_tr)
  hf_start_tr <- c(qy_tr_start[1], (qy_tr_start[2]-1)*3 + 1)
  hf_end_tr   <- c(qy_tr_end[1],   qy_tr_end[2]*3)
  
  x_tr_names <- paste0(xnames, "_tr")
  for (i in seq_along(xnames)) {
    xi <- get(xnames[i], inherits = TRUE)
    assign(x_tr_names[i], window(xi, start = hf_start_tr, end = hf_end_tr), envir = .GlobalEnv)
  }
  
  # 3) fit on train with same kmax
  form_tr <- build_formula("y_tr", x_tr_names, best$kmax)
  starts_tr <- nealmon_starts(x_tr_names, "y_tr")
  fit_tr <- midas_r(form_tr, start = starts_tr, OLS = TRUE)
  
  # 4) forecast last n_test quarters
  newdat <- c(setNames(lapply(x_tr_names, get, inherits = TRUE), x_tr_names), list(y_tr = y_tr))
  fc <- forecast(fit_tr, newdata = newdat, h = n_test)
  
  y_te <- tail(y_full, n_test)
  ae   <- abs(y_te - fc$mean)
  rmse <- sqrt(mean((y_te - fc$mean)^2, na.rm = TRUE))
  mae  <- mean(ae, na.rm = TRUE)
  mase <- mae / mean(abs(diff(y_tr, lag = 4)), na.rm = TRUE)
  list(rmse=rmse, mae=mae, mase=mase, h=n_test)
}

# -------- Which predictors? (assumes they exist) --------
all_candidates <- c("oil_g","imp_g","nno_g","rex_g","oprice_g","oprod_g","gprice_g","m1_g","m2_g")
available <- all_candidates[sapply(all_candidates, present)]

# -------- Separate models (one-at-a-time) --------
separate_specs <- lapply(available, function(x) c(x))

# -------- Suggested small combos (stable) --------
suggested_specs <- list()
# oil value + one trade or money
if (present("oil_g")){
  if (present("imp_g")) suggested_specs <- c(suggested_specs, list(c("oil_g","imp_g")))
  if (present("nno_g")) suggested_specs <- c(suggested_specs, list(c("oil_g","nno_g")))
  if (present("rex_g")) suggested_specs <- c(suggested_specs, list(c("oil_g","rex_g")))
  if (present("m1_g"))  suggested_specs <- c(suggested_specs, list(c("oil_g","m1_g")))
  if (present("m2_g"))  suggested_specs <- c(suggested_specs, list(c("oil_g","m2_g")))
}
# oil components (avoid mixing with oil_g to reduce duplication)
if (present("oprice_g") && present("oprod_g")){
  suggested_specs <- c(suggested_specs, list(c("oprice_g","oprod_g")))
  if (present("m1_g")) suggested_specs <- c(suggested_specs, list(c("oprice_g","oprod_g","m1_g")))
  if (present("m2_g")) suggested_specs <- c(suggested_specs, list(c("oprice_g","oprod_g","m2_g")))
}
# gas + oil price
if (present("oprice_g") && present("gprice_g")){
  suggested_specs <- c(suggested_specs, list(c("oprice_g","gprice_g")))
}
# trade trio (baseline without energy)
if (all(sapply(c("imp_g","nno_g","rex_g"), present))){
  suggested_specs <- c(suggested_specs, list(c("imp_g","nno_g","rex_g")))
}
# dedupe / cap length
norm_key <- function(v) paste(sort(unique(v)), collapse=",")
seen <- new.env(parent=emptyenv())
suggested_specs <- Filter(function(v) length(v)>0 && length(v)<=max_predictors, suggested_specs)
suggested_specs <- Filter(function(v){ k <- norm_key(v); if (exists(k, seen)) FALSE else { assign(k, TRUE, seen); TRUE }}, suggested_specs)

# -------- Master spec list --------
spec_list <- c(separate_specs, suggested_specs)

cat("Total models to estimate:", length(spec_list), "\n")

# -------- Fit, summarize, diagnose, holdout --------
summary_table <- data.frame(
  model_id = integer(), type = character(), predictors = character(),
  L = integer(), kmax = integer(), AIC = numeric(), BIC = numeric(),
  Ljung_p = numeric(), R2 = numeric(), AdjR2 = numeric(),
  RMSE = numeric(), MAE = numeric(), MASE = numeric(),
  stringsAsFactors = FALSE
)
results <- vector("list", length(spec_list))

for (i in seq_along(spec_list)) {
  xnames <- spec_list[[i]]
  label <- if (length(xnames) == 1) "SEPARATE" else "SUGGESTED"
  
  cat("\n==============================\n",
      "Spec ", i, " [", label, "]: ", paste(xnames, collapse=", "), "\n",
      "==============================\n", sep="")
  
  best <- try(fit_nealmon_over_L("y_g", xnames, L_set), silent = TRUE)
  if (inherits(best, "try-error")) {
    cat("  -> skipped (fit failed)\n")
    next
  }
  
  print(safe_summary(best$fit))
  dg <- diag_report(best$fit, y_g)
  hv <- holdout_eval(best, y_g, xnames)
  
  summary_table <- rbind(summary_table, data.frame(
    model_id = i, type = label, predictors = paste(xnames, collapse=","),
    L = best$L, kmax = best$kmax, AIC = best$AIC, BIC = best$BIC,
    Ljung_p = dg$Ljung_p, R2 = dg$R2, AdjR2 = dg$AdjR2,
    RMSE = hv$rmse, MAE = hv$mae, MASE = hv$mase
  ))
  results[[i]] <- list(best = best, diag = dg, holdout = hv, xnames = xnames)
}

# -------- Leader board --------
summary_table <- summary_table[order(summary_table$type, summary_table$AIC, summary_table$MASE), ]
cat("\n\n==== SUMMARY (Top by AIC within type) ====\n")
print(do.call(rbind, by(summary_table, summary_table$type, function(dd) head(dd[order(dd$AIC), ], 10))), row.names = FALSE)

# -------- Optional: plot Nealmon weights for best overall --------
if (nrow(summary_table) > 0) {
  top_i <- summary_table$model_id[which.min(summary_table$AIC)]
  top <- results[[top_i]]
  if (!is.null(top)) {
    get2 <- function(obj, base) unname(coef(obj)[paste0(base, 1:2)])
    w_block <- function(v) nealmon(get2(top$best$fit, v), top$best$kmax)
    vars <- top$xnames
    op <- par(no.readonly = TRUE); on.exit(par(op))
    par(mfrow=c(min(2,length(vars)), ceiling(length(vars)/2)))
    for (v in vars) {
      w <- w_block(v)
      plot(w, type="h", main=paste("Nealmon Weights:", v), xlab="Monthly lag", ylab="weight")
    }
  }
}
















# ============================
# Add & fit extra multi-predictor combos
# ============================

# 1) Define raw combos (economic logic, <=5 predictors)
specs_extra_raw <- list(
  Trade_M1          = c("imp_g","nno_g","rex_g","m1_g"),
  Trade_M2          = c("imp_g","nno_g","rex_g","m2_g"),
  OilComp_Gas       = c("oprice_g","oprod_g","gprice_g"),
  OilComp_M1        = c("oprice_g","oprod_g","m1_g"),
  OilComp_M2        = c("oprice_g","oprod_g","m2_g"),
  OilVal_Trade_M1   = c("oil_g","imp_g","nno_g","m1_g"),
  OilVal_Trade_M2   = c("oil_g","imp_g","rex_g","m2_g")
)

# 2) Guards ---------------------------------------------------------
if (!exists("present", inherits = TRUE)) {
  present <- function(v) exists(v, inherits = TRUE) && !is.null(get(v, inherits = TRUE))
}

mixes_oil_value_and_components <- function(v){
  has_val  <- "oil_g" %in% v
  has_comp <- any(c("oprice_g","oprod_g","gprice_g") %in% v)
  has_val && has_comp
}

filter_combo <- function(v) {
  v2 <- unique(v[sapply(v, present)])
  if (length(v2) == 0) return(character(0))
  if (exists("max_predictors", inherits = TRUE) && length(v2) > max_predictors) return(character(0))
  if (mixes_oil_value_and_components(v2)) return(character(0))
  v2
}

specs_extra <- lapply(specs_extra_raw, filter_combo)
specs_extra <- specs_extra[vapply(specs_extra, length, 1L) > 0]

# De-duplicate against existing 'specs' (if any)
exists_specs <- exists("specs", inherits = TRUE) && is.list(specs)
if (exists_specs) {
  have_keys <- vapply(specs, function(v) paste(sort(v), collapse=","), "")
  new_specs <- list()
  for (nm in names(specs_extra)) {
    key <- paste(sort(specs_extra[[nm]]), collapse=",")
    if (!(key %in% have_keys)) new_specs[[nm]] <- specs_extra[[nm]]
  }
  specs <- c(specs, new_specs)
} else {
  specs <- specs_extra
}

cat("Extra candidate specs added:", length(specs_extra), "\n")
if (length(specs_extra) == 0) {
  cat("No new viable combos (availability/collinearity/size rules).\n")
}

# 3) Fit the extra specs only --------------------------------------
if (!exists("results", inherits = TRUE) || !is.list(results)) results <- list()

needed_cols <- c("model_id","type","predictors","L","kmax","AIC","BIC",
                 "Ljung_p","R2","AdjR2","RMSE","MAE","MASE")
if (!exists("summary_table", inherits = TRUE) || !all(needed_cols %in% names(summary_table))) {
  summary_table <- data.frame(
    model_id = integer(), type = character(), predictors = character(),
    L = integer(), kmax = integer(), AIC = numeric(), BIC = numeric(),
    Ljung_p = numeric(), R2 = numeric(), AdjR2 = numeric(),
    RMSE = numeric(), MAE = numeric(), MASE = numeric(),
    stringsAsFactors = FALSE
  )
}

# scalarizers to avoid length-0 issues
# scalarizers to avoid length-0 issues
.as_num1 <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x[1])
.as_chr1 <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[1])

# little helper to tolerate name variants (e.g., "AdjR2" vs "adjR2")
.get_dg_num <- function(dg, candidates) {
  for (nm in candidates) {
    v <- try(dg[[nm]], silent = TRUE)
    if (!inherits(v, "try-error") && !is.null(v)) return(.as_num1(v))
  }
  NA_real_
}


# -------- Robust AdjR2 grabber and appender --------
get_adjR2 <- function(dg, best) {
  # Try both name variants first
  if (!is.null(dg$AdjR2)) return(as.numeric(dg$AdjR2))
  if (!is.null(dg$adjR2)) return(as.numeric(dg$adjR2))
  # Fallback: compute from R2 if possible
  R2 <- suppressWarnings(as.numeric(dg$R2))
  if (is.na(R2)) return(NA_real_)
  # n and k
  n <- tryCatch(length(residuals(best$fit)), error = function(e) NA_integer_)
  k <- tryCatch(length(coef(best$fit)),      error = function(e) NA_integer_)
  if (is.na(n) || is.na(k) || n <= k + 1) return(NA_real_)
  1 - (1 - R2) * ((n - 1) / (n - k))
}

append_summary_row <- function(tbl, model_id, type, xnames, best, dg, hv){
  adjR2_val <- get_adjR2(dg, best)
  newrow <- data.frame(
    model_id   = .as_num1(model_id),
    type       = .as_chr1(type),
    predictors = .as_chr1(paste(xnames, collapse=",")),
    L          = .as_num1(best$L),
    kmax       = .as_num1(best$kmax),
    AIC        = .as_num1(best$AIC),
    BIC        = .as_num1(best$BIC),
    Ljung_p    = .as_num1(dg$Ljung_p),
    R2         = .as_num1(dg$R2),
    AdjR2      = .as_num1(adjR2_val),
    RMSE       = .as_num1(hv$rmse),
    MAE        = .as_num1(hv$mae),
    MASE       = .as_num1(hv$mase),
    stringsAsFactors = FALSE
  )
  # align columns
  missing_cols <- setdiff(names(tbl), names(newrow))
  if (length(missing_cols)) newrow[missing_cols] <- NA
  newrow <- newrow[, names(tbl), drop = FALSE]
  rbind(tbl, newrow)
}


type_tag <- "ADDED"

# start model_id after current summary_table rows (simple, robust)
next_model_id <- if (nrow(summary_table)) max(summary_table$model_id, na.rm = TRUE) + 1L else 1L

for (nm in names(specs_extra)) {
  xnames <- specs_extra[[nm]]
  cat("\n==============================\nCombo [", type_tag, "] ", nm, ": ",
      paste(xnames, collapse=", "), "\n", sep = "")
  best <- try(fit_nealmon_over_L("y_g", xnames, L_set), silent = TRUE)
  if (inherits(best, "try-error") || is.null(best$fit)) {
    cat("  -> skipped (fit failed)\n")
    next
  }
  print(safe_summary(best$fit))
  
  # diagnostics and holdout
  dg <- try(diag_report(best$fit, y_g), silent = TRUE)
  if (inherits(dg, "try-error")) dg <- list(R2=NA, adjR2=NA, Ljung_p=NA)
  hv <- try(holdout_eval(best, y_g, xnames), silent = TRUE)
  if (inherits(hv, "try-error")) hv <- list(rmse=NA, mae=NA, mase=NA, h=0)
  
  # store & append
  model_id <- next_model_id; next_model_id <- next_model_id + 1L
  results[[model_id]] <- list(best = best, diag = dg, holdout = hv,
                              xnames = xnames, name = nm, type = type_tag)
  
  summary_table <- append_summary_row(summary_table, model_id, type_tag, xnames, best, dg, hv)
}

# 4) Leader board: show best of the newly ADDED sets
dd <- subset(summary_table, type == type_tag)
if (nrow(dd)) {
  dd <- dd[order(dd$AIC, dd$MASE), ]
  cat("\n\n==== SUMMARY (Top ADDED by AIC) ====\n")
  print(head(dd, 10), row.names = FALSE)
} else {
  cat("\nNo ADDED models were successfully estimated.\n")
}

if (exists("summary_table", inherits = TRUE) &&
    nrow(summary_table) && exists("results", inherits = TRUE)) {
  na_rows <- which(is.na(summary_table$AdjR2))
  for (ii in na_rows) {
    mid <- summary_table$model_id[ii]
    if (!is.na(mid) && !is.null(results[[mid]]) &&
        !is.null(results[[mid]]$best) && !is.null(results[[mid]]$best$fit)) {
      # Rebuild a minimal dg to reuse the helper
      dg_tmp <- list(R2 = summary_table$R2[ii], Ljung_p = summary_table$Ljung_p[ii])
      adjR2_new <- get_adjR2(dg_tmp, results[[mid]]$best)
      summary_table$AdjR2[ii] <- adjR2_new
    }
  }
}


# Split into SEPARATE, SUGGESTED, ADDED
separate_tbl  <- subset(summary_table, type == "SEPARATE")
suggested_tbl <- subset(summary_table, type == "SUGGESTED")
added_tbl     <- subset(summary_table, type == "ADDED")

write_xlsx(
  list(
    SEPARATE  = separate_tbl,
    SUGGESTED = suggested_tbl,
    ADDED     = added_tbl
  ),
  "MIDAS_results_NGDP.xlsx"
)




# 
# # ==========================================================
# # EXPORT: per-model coefficient results (with full variable names)
# # ==========================================================
# suppressPackageStartupMessages({
#   library(dplyr); library(purrr); library(stringr); library(tidyr)
#   library(writexl)
# })
# 
# stopifnot(exists("results", inherits = TRUE), is.list(results))
# 
# # --- Dictionary: short code → descriptive name ---
# var_labels <- c(
#   "oil_g"   = "Average oil price",
#   "oprod_g" = "Daily Average Production",
#   "gprice_g"= "Natural oil Gas",
#   "m1_g"    = "Narrow Money (M1)",
#   "m2_g"    = "Broad Money",
#   "imp_g"   = "Total Recorded Merchandise Imports",
#   "nno_g"   = "Non-Oil Omani Exports",
#   "rex_g"   = "Re-Exports",
#   "nonoil_g"= "Non-Oil Exports",
#   "y_g"     = "Non-Oil GDP"
# )
# 
# # --- Helpers ---
# sheet_name <- function(mid, obj) {
#   x <- try(obj$xnames, silent = TRUE)
#   lab <- if (!inherits(x, "try-error") && length(x))
#     paste(x, collapse = "_") else "model"
#   nm <- sprintf("M%02d_%s", as.integer(mid), lab)
#   nm <- gsub("[^A-Za-z0-9_]", "_", nm)
#   substr(nm, 1, 31)
# }
# 
# safe_summary <- function(fit){
#   out <- try(summary(fit, robust = TRUE), silent = TRUE)
#   if (inherits(out, "try-error")) summary(fit, robust = FALSE) else out
# }
# 
# coef_table_from_fit <- function(fit) {
#   sm <- safe_summary(fit)
#   cm <- NULL
#   for (cand in c("coefficients", "Coef", "coef")) {
#     v <- try(sm[[cand]], silent = TRUE)
#     if (!inherits(v, "try-error") && !is.null(v)) { cm <- v; break }
#   }
#   if (is.null(cm)) {
#     cf <- try(coef(fit), silent = TRUE)
#     V  <- try(vcov(fit),  silent = TRUE)
#     if (inherits(cf, "try-error") || is.null(cf)) {
#       return(tibble(term=character(), Estimate=double(),
#                     `Std. Error`=double(), `t value`=double(),
#                     `Pr(>|t|)`=double(), term_full=character()))
#     }
#     se <- if (!inherits(V, "try-error") && !is.null(V)) sqrt(pmax(diag(as.matrix(V)), 0)) else NA_real_
#     tt <- cf / se
#     pp <- 2*pnorm(abs(tt), lower.tail = FALSE)
#     df <- tibble(term = names(cf),
#                  Estimate = as.numeric(cf),
#                  `Std. Error` = as.numeric(se),
#                  `t value` = as.numeric(tt),
#                  `Pr(>|t|)` = as.numeric(pp))
#   } else {
#     df <- as.data.frame(cm)
#     df <- tibble::rownames_to_column(df, "term")
#     names(df) <- sub("^Std\\.Error$", "Std. Error", names(df))
#     names(df) <- sub("^Std\\. Error$", "Std. Error", names(df))
#     names(df) <- sub("^t$", "t value", names(df))
#     names(df) <- sub("^Pr\\(.+\\)$", "Pr(>|t|)", names(df))
#     need <- c("term","Estimate","Std. Error","t value","Pr(>|t|)")
#     for (n in need) if (!n %in% names(df)) df[[n]] <- NA_real_
#     df <- df[need]
#   }
#   
#   # --- add full descriptive name (strip trailing digits) ---
#   # --- add full descriptive name with lag index (no 'base' column) ---
#   df <- df %>%
#     mutate(
#       is_lag = grepl("[0-9]+$", term),
#       lag_no = ifelse(is_lag, sub(".*?(\\d+)$", "\\1", term), NA_character_),
#       base   = sub("[0-9]+$", "", term),
#       term_full = dplyr::case_when(
#         term == "(Intercept)"                 ~ "(Intercept)",
#         base == "y_g" & !is.na(lag_no)       ~ paste0("Non-Oil GDP [AR lag ", lag_no, "]"),
#         base %in% names(var_labels) & !is.na(lag_no)
#         ~ paste0(var_labels[base], " [lag ", lag_no, "]"),
#         base %in% names(var_labels)          ~ var_labels[base],
#         TRUE                                 ~ term
#       )
#     ) %>%
#     select(term, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`, term_full)
# }
# 
# 
# header_block <- function(mid, obj){
#   best <- obj$best
#   preds_codes <- if (!is.null(obj$xnames)) obj$xnames else character(0)
#   preds_full  <- if (length(preds_codes)) {
#     paste(ifelse(preds_codes %in% names(var_labels),
#                  unname(var_labels[preds_codes]), preds_codes), collapse = ", ")
#   } else NA_character_
#   
#   tibble(
#     Field = c("model_id","type","predictors","L","kmax","AIC","BIC"),
#     Value = c(
#       mid,
#       if (!is.null(obj$type)) obj$type else NA_character_,
#       preds_full,
#       if (!is.null(best$L)) best$L else NA_real_,
#       if (!is.null(best$kmax)) best$kmax else NA_real_,
#       if (!is.null(best$AIC)) best$AIC else NA_real_,
#       if (!is.null(best$BIC)) best$BIC else NA_real_
#     )
#   )
# }
# 
# # --- Build all sheets ---
# sheets <- list()
# for (i in seq_along(results)) {
#   obj <- results[[i]]
#   if (is.null(obj) || is.null(obj$best) || is.null(obj$best$fit)) next
#   fit <- obj$best$fit
#   
#   head_df <- header_block(i, obj)
#   coef_df <- coef_table_from_fit(fit)
#   
#   out_df <- bind_rows(
#     tibble(term = paste(head_df$Field, head_df$Value, sep = ": "),
#            Estimate = NA_real_, `Std. Error` = NA_real_,
#            `t value` = NA_real_, `Pr(>|t|)` = NA_real_,
#            term_full = NA_character_),
#     tibble(term = "", Estimate = NA_real_, `Std. Error` = NA_real_,
#            `t value` = NA_real_, `Pr(>|t|)` = NA_real_,
#            term_full = ""),
#     coef_df
#   )
#   
#   nm <- sheet_name(i, obj)
#   k <- 1; base_nm <- nm
#   while (nm %in% names(sheets)) {
#     suffix <- sprintf("_%d", k); k <- k + 1
#     nm <- substr(paste0(base_nm, suffix), 1, 31)
#   }
#   sheets[[nm]] <- out_df
# }
# 
# outfile <- "MIDAS_model_results_ALL.xlsx"
# write_xlsx(sheets, path = outfile)
# cat("Wrote per-model results (with full names) to:", outfile, "\n")






















# ==========================================================
# EXPORT: all model coefficients in ONE sheet (NOMINAL GDP target = y_g)
# ==========================================================
suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(stringr); library(tidyr)
  library(writexl)
})

stopifnot(exists("results", inherits = TRUE), is.list(results))

# --- Dictionary: short code → descriptive name ---
var_labels <- c(
  "oil_g"   = "Average oil price",
  "oprod_g" = "Daily Average Production",
  "gprice_g"= "Natural oil Gas",
  "m1_g"    = "Narrow Money (M1)",
  "m2_g"    = "Broad Money",
  "imp_g"   = "Total Recorded Merchandise Imports",
  "nno_g"   = "Non-Oil Omani Exports",
  "rex_g"   = "Re-Exports",
  "nonoil_g"= "Non-Oil Exports",
  "y_g"     = "Nominal GDP"
)

safe_summary <- function(fit){
  out <- try(summary(fit, robust = TRUE), silent = TRUE)
  if (inherits(out, "try-error")) summary(fit, robust = FALSE) else out
}

coef_table_from_fit <- function(fit) {
  sm <- safe_summary(fit)
  cm <- NULL
  for (cand in c("coefficients","Coef","coef")) {
    v <- try(sm[[cand]], silent = TRUE)
    if (!inherits(v, "try-error") && !is.null(v)) { cm <- v; break }
  }
  
  if (is.null(cm)) {
    cf <- try(coef(fit), silent = TRUE)
    V  <- try(vcov(fit),  silent = TRUE)
    if (inherits(cf, "try-error") || is.null(cf)) {
      return(tibble::tibble(term=character(), Estimate=double(),
                            `Std. Error`=double(), `t value`=double(),
                            `Pr(>|t|)`=double(), term_full=character()))
    }
    se <- if (!inherits(V, "try-error") && !is.null(V)) sqrt(pmax(diag(as.matrix(V)), 0)) else NA_real_
    tt <- cf / se
    pp <- 2*pnorm(abs(tt), lower.tail = FALSE)
    df <- tibble::tibble(term = names(cf),
                         Estimate = as.numeric(cf),
                         `Std. Error` = as.numeric(se),
                         `t value` = as.numeric(tt),
                         `Pr(>|t|)` = as.numeric(pp))
  } else {
    df <- as.data.frame(cm)
    df <- tibble::rownames_to_column(df, "term")
    names(df) <- sub("^Std\\.Error$", "Std. Error", names(df))
    names(df) <- sub("^Std\\. Error$", "Std. Error", names(df))
    names(df) <- sub("^t$", "t value", names(df))
    names(df) <- sub("^Pr\\(.+\\)$", "Pr(>|t|)", names(df))
    need <- c("term","Estimate","Std. Error","t value","Pr(>|t|)")
    for (n in need) if (!n %in% names(df)) df[[n]] <- NA_real_
    df <- df[need]
  }
  
  # ---- Robust 'term_full' (vector-safe) ----
  term   <- df$term
  is_int <- term == "(Intercept)"
  is_lag <- grepl("[0-9]+$", term)
  lag_no <- ifelse(is_lag, sub(".*?(\\d+)$", "\\1", term), NA_character_)
  base   <- sub("[0-9]+$", "", term)
  
  labels <- unname(var_labels[base])
  labels[is.na(labels)] <- NA_character_
  
  term_full <- term
  # AR terms for NOMINAL GDP (y_g)
  mask_ar <- base == "y_g" & !is.na(lag_no) & !is_int
  term_full[mask_ar] <- paste0("Nominal GDP [AR lag ", lag_no[mask_ar], "]")
  
  # Monthly lags with known labels
  mask_lag <- !mask_ar & !is.na(labels) & !is.na(lag_no) & !is_int
  term_full[mask_lag] <- paste0(labels[mask_lag], " [lag ", lag_no[mask_lag], "]")
  
  # Unlagged terms with known labels (rare)
  mask_base <- !mask_ar & is.na(lag_no) & !is.na(labels) & !is_int
  term_full[mask_base] <- labels[mask_base]
  
  df$term_full <- term_full
  df[, c("term","term_full","Estimate","Std. Error","t value","Pr(>|t|)")]
}

# ---- Build one big data frame: add model metadata as columns ----
all_coefs <- purrr::map2_dfr(seq_along(results), results, function(mid, obj){
  if (is.null(obj) || is.null(obj$best) || is.null(obj$best$fit)) return(NULL)
  fit  <- obj$best$fit
  meta <- tibble::tibble(
    model_id   = mid,
    type       = if (!is.null(obj$type)) obj$type else if (length(obj$xnames)==1) "SEPARATE" else "SUGGESTED",
    predictors = paste(obj$xnames, collapse = ", "),
    predictors_full = paste(ifelse(obj$xnames %in% names(var_labels),
                                   unname(var_labels[obj$xnames]), obj$xnames), collapse = ", "),
    L          = obj$best$L,
    kmax       = obj$best$kmax,
    AIC        = obj$best$AIC,
    BIC        = obj$best$BIC
  )
  coef_table_from_fit(fit) %>%
    tidyr::crossing(meta) %>%
    dplyr::select(model_id, type, predictors, predictors_full, L, kmax, AIC, BIC,
                  term, term_full, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)
})

# ---- Write ONE sheet ----
outfile <- "MIDAS_model_results_NominalGDP.xlsx"
writexl::write_xlsx(list(ALL_MODELS = all_coefs), path = outfile)
cat("Wrote all model coefficients to one sheet:", outfile, "\n")
