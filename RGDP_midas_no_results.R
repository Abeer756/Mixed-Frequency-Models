## ===========================
# Mixed-Frequency MIDAS (Nealmon) — COMPLETE SCRIPT
# ===========================

# --- Packages
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(midasr)
library(janitor)
library(forecast)

# --- Inference helpers (for p-values)
library(sandwich)  # robust SEs for OLS
library(lmtest)    # coeftest tables
# we'll call car::linearHypothesis explicitly (no need to attach car)


# --- Read data
predictors1 <- read_excel("predictors.xlsx") %>% 
  clean_names() %>% 
  transmute(
    month = as.Date(zoo::as.yearmon(month_year)),
    tot_mer_imports = total_recorded_merchandise_imports,
    oil_gaz_exports = oil_gaz_exports,
    noil_omani_exports = non_oil_omani_exports,
    re_exports = re_exports,
    noil_exports = non_oil_exports) %>%
  arrange(month)


predictors2 <- read_excel("predictors2.xlsx") %>% 
  clean_names() %>% 
  transmute(
    month = as.Date(zoo::as.yearmon(month_year)),
    oil_price = average_oil_price,
    d_avg_prod = daily_average_production,
    gas_price = natural_oil_gas,
    M1 = narrow_money_m1,
    M2 = broad_money) %>% 
  arrange(month)
    
predictors <- merge(predictors1, predictors2, by = "month", all.x = TRUE)


predictors <- predictors %>%
  mutate(across(-month, ~ as.numeric(as.character(.))))
## if for all columns: across(everything(), as.numeric)

quart_rgdp  <- read_excel("quart_rgdp.xlsx")

m_start  <- c(lubridate::year(min(predictors$month)),  lubridate::month(min(predictors$month)))


# ======================
# 1) Monthly predictors (Build monthly level ts)
# ======================
tot_mer_imports    <- ts(predictors$tot_mer_imports, start = m_start,  frequency = 12)
oil_gaz_exports    <- ts(predictors$oil_gaz_exports, start = m_start,  frequency = 12)
noil_omani_exports <- ts(predictors$noil_omani_exports, start = m_start,  frequency = 12)
re_exports         <- ts(predictors$re_exports, start = m_start,  frequency = 12)
noil_exports       <- ts(predictors$noil_exports, start = m_start,  frequency = 12)
oil_price          <- ts(predictors$oil_price, start = m_start,  frequency = 12)
d_avg_prod         <- ts(predictors$d_avg_prod, start = m_start,  frequency = 12)
gas_price          <- ts(predictors$gas_price, start = m_start,  frequency = 12)
M1                 <- ts(predictors$M1, start = m_start,  frequency = 12)
M2                 <- ts(predictors$M2, start = m_start,  frequency = 12)
#############################################################################################################################
# --- Helpers
# Increment a (year, period) start by 1 within a given frequency
.inc_start <- function(y, p, freq){
  p2 <- p + 1
  if(p2 > freq) c(y + 1, 1) else c(y, p2)
}
# Safe log-diff (%): guard against <=0 before log
logdiff_pct <- function(x) 100*diff(log(pmax(as.numeric(x), 1e-9)))

# make monthly growth then window to the quarterly span
mk_m_g_aligned <- function(x_m) {
  x_g <- ts(
    logdiff_pct(x_m),
    start = .inc_start(start(x_m)[1], start(x_m)[2], 12),
    frequency = 12
  )
  window(x_g, start = hf_start, end = hf_end)
}
#############################################################################################################################
# ====================================================================
# 2) Quarterly GDP target  (REAL GDP)
# ====================================================================
rgdp_q <- quart_rgdp %>%
  mutate(qtr = as.yearqtr(gsub("-", " ", year_quarter))) %>%  # "2010-Q1" -> yearqtr
  arrange(qtr) %>%
  select(qtr, RGDP, RGDP_oil, RGDP_Noil)

rq_start <- c(year(as.Date(rgdp_q$qtr[1])), quarter(as.Date(rgdp_q$qtr[1])))

# >>> Choose the target here <<<
y_rq <- ts(rgdp_q$RGDP, start = rq_start, frequency = 4)
# y_rq <- ts(rgdp_q$NGDP_oil,  start = q_start, frequency = 4)   # alt
# y_rq <- ts(rgdp_q$NGDP_Noil, start = q_start, frequency = 4)   # alt

# ======================
# 3) Transform to growth and ALIGN spans
# ======================
# Growth series (QoQ for GDP, MoM for monthly), with proper starts
# Quarterly REAL GDP growth (QoQ)
y_rg <- ts(logdiff_pct(y_rq), start = .inc_start(rq_start[1], rq_start[2], 4), frequency = 4)
# Align monthly series to the exact quarter span of y_rg
# Quarter span for alignment of monthly series
qy_start <- start(y_rg)                          # c(year, quarter)
qy_end   <- end(y_rg)                            # c(year, quarter)
hf_start <- c(qy_start[1], (qy_start[2]-1)*3 + 1)  # Jan/Apr/Jul/Oct
hf_end   <- c(qy_end[1],   qy_end[2]*3)            # Mar/Jun/Sep/Dec

# Helper: make monthly growth then window to the quarterly span
mk_m_g_aligned <- function(x_m) {
  x_g <- ts(logdiff_pct(x_m),
            start = .inc_start(m_start[1], m_start[2], 12),
            frequency = 12)
  window(x_g, start = hf_start, end = hf_end)
}

tot_mer_imports_g    <- mk_m_g_aligned(tot_mer_imports)
oil_gaz_exports_g    <- mk_m_g_aligned(oil_gaz_exports)
noil_omani_exports_g <- mk_m_g_aligned(noil_omani_exports)
re_exports_g         <- mk_m_g_aligned(re_exports)
noil_exports_g       <- mk_m_g_aligned(noil_exports)
oil_price_g          <- mk_m_g_aligned(oil_price)
d_avg_prod_g         <- mk_m_g_aligned(d_avg_prod)
gas_price_g          <- mk_m_g_aligned(gas_price)
M1_g                 <- mk_m_g_aligned(M1)
M2_g                 <- mk_m_g_aligned(M2)
#############################################################################################################################
# ==============================================================================================================
# Nealmon-MIDAS: SEPARATE models + suggested small combos
# (search over L_set, robust summary, diagnostics, holdout)
# Assumes y_rg and monthly *_g series are ALREADY in memory & aligned.
# ============================================================================================================
# -------- Tunables --------
K <- 3                 # 3 months per quarter
L_set <- 3:5           # search 9–18 monthly lags  (kmax = 3L - 1)
max_predictors <- 5    # safety cap

# -------- Availability helper (no data wrangling here) --------
present <- function(v) exists(v, inherits = TRUE) && !is.null(get(v, inherits = TRUE))

# -------- MIDAS building blocks --------
# nealmon_starts <- function(xnames, yname){
#   ans <- setNames(replicate(length(xnames), c(0.1,0.1), simplify = FALSE), xnames)
#   ans[[yname]] <- c(0.1,0.1,0.1) # for mls(y, c(1,2,4), m=1)
#   ans
# }

nealmon_starts <- function(xnames, yname){
  set.seed(123)
  ans <- setNames(lapply(seq_along(xnames), function(i) runif(2, 0.05, 0.20)), xnames)
  ans[[yname]] <- c(0.1,0.1,0.1) # AR lags
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

# Dictionary: short code → descriptive name
var_labels <- c(
  "tot_mer_imports_g"   = "Total Recorded Merchandise Imports",
  "oil_gaz_exports_g"   = "Oil & Gaz Exports",
  "noil_omani_exports_g"= "Non-Oil Omani Exports",
  "re_exports_g"        = "Re-Exports",
  "noil_exports_g"      = "Total Non-Oil Exports",
  "oil_price_g"         = "Average oil price",
  "d_avg_prod_g"        = "Daily Average Oil Production",
  "gas_price_g"         = "Natural Gas Price",
  "M1_g"                = "Narrow Money (M1)",
  "M2_g"                = "Broad Money (M2)"
)

#############################################################################################################################
## sanity Checks
cat("Target span  :", paste(start(y_rg), collapse="-"), "to", paste(end(y_rg), collapse="-"), "\n")
for (nm in c("tot_mer_imports_g","oil_gaz_exports_g","noil_omani_exports_g","re_exports_g",
             "noil_exports_g","oil_price_g","d_avg_prod_g","gas_price_g","M1_g","M2_g")) {
  s <- start(get(nm)); e <- end(get(nm))
  cat(sprintf("%-22s %s to %s\n", nm, paste(s, collapse="-"), paste(e, collapse="-")))
}

present <- function(v) {
  if (!exists(v, inherits = TRUE)) return(FALSE)
  obj <- get(v, inherits = TRUE)
  is.ts(obj) && length(obj) > 0 && all(is.finite(obj[is.finite(obj)]))
}

all_candidates <- c("tot_mer_imports_g","oil_gaz_exports_g","noil_omani_exports_g","re_exports_g","noil_exports_g",
                    "oil_price_g","d_avg_prod_g","gas_price_g","M1_g", "M2_g")

available <- all_candidates[sapply(all_candidates, present)]
#############################################################################################################################

# Helper to include a spec only if all variables are available
keep_if_available <- function(v) if (all(sapply(v, present))) v else NULL

# A) Trade baseline (with oil exports)
A_oilex <- keep_if_available(c("oil_gaz_exports_g","noil_omani_exports_g","re_exports_g","tot_mer_imports_g"))

# B) Add money (M1 or M2)
B1_oilex <- keep_if_available(c(A_oilex, "M1_g"))
B2_oilex <- keep_if_available(c(A_oilex, "M2_g"))

# C) Add oil price (with M1 and with M2)
C1_oilex <- keep_if_available(c(A_oilex, "M1_g", "oil_price_g"))
C2_oilex <- keep_if_available(c(A_oilex, "M2_g", "oil_price_g"))

# -------- Replace oil exports with oil production --------
A_prod  <- keep_if_available(c("d_avg_prod_g","noil_omani_exports_g","re_exports_g","tot_mer_imports_g"))
B1_prod <- keep_if_available(c(A_prod, "M1_g"))
B2_prod <- keep_if_available(c(A_prod, "M2_g"))
C1_prod <- keep_if_available(c(A_prod, "M1_g", "oil_price_g"))
C2_prod <- keep_if_available(c(A_prod, "M2_g", "oil_price_g"))

# Put them together (drop NULLs)
specs_custom <- Filter(Negate(is.null),
                       list(
                         A_oilex = A_oilex,  B1_oilex = B1_oilex,  B2_oilex = B2_oilex,
                         C1_oilex = C1_oilex, C2_oilex = C2_oilex,
                         A_prod = A_prod, B1_prod = B1_prod, B2_prod = B2_prod,
                         C1_prod = C1_prod, C2_prod = C2_prod
                       )
)



for (nm in names(specs_custom)) {
  cat(nm, ":", paste(specs_custom[[nm]], collapse=", "), "\n")
}



#============================================================================
## Check collinearity
#============================================================================
# pick the vars used in the spec you want to screen (example: C2_prod)
vars <- c("d_avg_prod_g","tot_mer_imports_g","noil_omani_exports_g",
          "re_exports_g","M2_g","oil_price_g")

HF <- do.call(cbind, lapply(vars, get))
colnames(HF) <- vars
HF <- window(HF, start = hf_start, end = hf_end)

round(cor(HF, use="pairwise.complete.obs"), 2)

#============================================================================
## Check collinearity by VIF
if (!requireNamespace("car", quietly=TRUE)) install.packages("car")

# Aggregate a monthly ts (freq=12) to quarterly (freq=4)
to_q <- function(x, fun = mean) {
  # (x is already aligned monthly growth on [hf_start, hf_end])
  x <- window(x, start = hf_start, end = hf_end)
  aggregate(x, nfrequency = 4, FUN = fun)  # NOT ts.aggregate()
}

Xq <- cbind(
  d_avg_prod_q      = to_q(d_avg_prod_g),
  tot_mer_imports_q = to_q(tot_mer_imports_g),
  noil_omani_q      = to_q(noil_omani_exports_g),
  re_exports_q      = to_q(re_exports_g),
  M2_q              = to_q(M2_g),
  oil_price_q       = to_q(oil_price_g)
)

# align with y_rg (both are quarterly now)
Xq <- window(Xq, start = start(y_rg), end = end(y_rg))
Yq <- window(y_rg, start = start(Xq), end = end(Xq))

df <- na.omit(data.frame(y = as.numeric(Yq), Xq))

m  <- lm(y ~ ., data = df)
car::vif(m)


#============================================================================
# ========= Fit the custom specs to y_rg (Real GDP growth) =========
#============================================================================

# 1) use ONLY your custom specs
spec_list <- unname(specs_custom)
spec_names <- names(specs_custom)

# 2) containers
summary_table <- data.frame(
  model_id = integer(), group = character(), predictors = character(),
  L = integer(), kmax = integer(), AIC = numeric(), BIC = numeric(),
  Ljung_p = numeric(), R2 = numeric(), AdjR2 = numeric(),
  RMSE = numeric(), MAE = numeric(), MASE = numeric(),
  cond_vcov = numeric(), min_eig = numeric(),          # <— NEW
  stringsAsFactors = FALSE
)
results <- vector("list", length(spec_list))


# --- Safe summary: try robust SEs; if they fail, fall back cleanly
# Try robust; if it explodes, fall back quietly to non-robust
safe_summary <- function(fit){
  out <- try(summary(fit, robust = TRUE), silent = TRUE)
  if (inherits(out, "try-error")) summary(fit, robust = FALSE) else out
}

safe_vcov_stats <- function(fit){
  V <- try(vcov(fit), silent = TRUE)
  if (inherits(V, "try-error") || is.null(V)) return(list(cond_vcov = NA_real_, min_eig = NA_real_))
  V <- as.matrix(V)
  V <- (V + t(V))/2  # force symmetry
  list(
    cond_vcov = tryCatch(kappa(V, exact = TRUE), error = function(e) NA_real_),
    min_eig   = tryCatch(min(Re(eigen(V, symmetric = FALSE, only.values = TRUE)$values)),
                         error = function(e) NA_real_)
  )
}

# Per-variable kmax map (default = current kmax; shorten some vars if needed)
kmax_map_for <- function(xnames, L, K, default_kmax){
  km <- setNames(rep(default_kmax, length(xnames)), xnames)
  # If both M2 and oil price are in the spec, shorten their blocks
  if (all(c("M2_g","oil_price_g") %in% xnames)) {
    short_k <- K*3 - 1   # L=3 => kmax=8
    km["M2_g"]        <- min(km["M2_g"],        short_k)
    km["oil_price_g"] <- min(km["oil_price_g"], short_k)
  }
  km
}

# Build formula with per-variable kmax
build_formula_kmap <- function(yname, xnames, kmap, K){
  form <- as.formula(paste0(yname, " ~ mls(", yname, ", c(1,2,4), m=1)"))
  for (v in xnames) {
    form <- update(form, paste0(". ~ . + fmls(", v, ", ", kmap[[v]], ", ", K, ", nealmon)"))
  }
  form
}

# Let the L-search use per-variable kmax
fit_nealmon_over_L_kmap <- function(yname, xnames, L_set, K){
  fits <- lapply(L_set, function(L){
    kmax_def <- K*L - 1
    kmap     <- kmax_map_for(xnames, L, K, kmax_def)
    form     <- build_formula_kmap(yname, xnames, kmap, K)
    starts   <- nealmon_starts(xnames, yname)
    fit      <- midas_r(formula = form, start = starts, OLS = TRUE)
    list(fit = fit, AIC = AIC(fit), BIC = BIC(fit), L = L,
         kmax = kmax_def, kmap = kmap, x = xnames)
  })
  fits[[ which.min(sapply(fits, `[[`, "AIC")) ]]
}


# 3) main loop (exactly like before)
for (i in seq_along(spec_list)) {
  xnames <- spec_list[[i]]
  label  <- spec_names[i]
  
  cat("\n==============================\n",
      "Custom Spec ", i, " [", label, "]: ", paste(xnames, collapse=", "), "\n",
      "==============================\n", sep="")
  
  # search over L in L_set and pick the best by AIC
  best <- try(fit_nealmon_over_L_kmap("y_rg", xnames, L_set, K), silent = TRUE)
  
  
  invisible(try(print(safe_summary(best$fit)), silent = TRUE))
  
  dg <- diag_report(best$fit, y_rg)
  hv <- holdout_eval(best, y_rg, xnames)
  
  sv <- safe_vcov_stats(best$fit)
  cond_vcov <- sv$cond_vcov
  min_eig   <- sv$min_eig
  
  summary_table <- rbind(summary_table, data.frame(
    model_id = i, group = label, predictors = paste(xnames, collapse=","),
    L = best$L, kmax = best$kmax, AIC = best$AIC, BIC = best$BIC,
    Ljung_p = dg$Ljung_p, R2 = dg$R2, AdjR2 = dg$AdjR2,
    RMSE = hv$rmse, MAE = hv$mae, MASE = hv$mase,
    cond_vcov = cond_vcov, min_eig = min_eig                # <— append
  ))
  
  results[[i]] <- list(best = best, diag = dg, holdout = hv, xnames = xnames)
}

# 4) leaderboards
summary_table <- summary_table[order(summary_table$AIC, summary_table$MASE), ]
cat("\n==== LEADERBOARD (overall, sorted by AIC then MASE) ====\n")
print(summary_table, row.names = FALSE)

# Best model PER custom group (A_oilex, B1_oilex, ..., C2_prod)
best_per_group <- do.call(rbind, lapply(split(summary_table, summary_table$group), function(dd){
  dd[order(dd$AIC, dd$MASE), ][1, , drop = FALSE]
}))
cat("\n==== BEST PER GROUP ====\n")
print(best_per_group[order(best_per_group$AIC), ], row.names = FALSE)





# ---------- SAFE: write the MIDAS weights to a file (no RStudio pane needed) ----------
plot_midas_weights <- function(summary_table, results,
                               file = "midas_weights.png",
                               width = 1400, height = 900, res = 150) {
  stopifnot(nrow(summary_table) > 0)
  top_i <- summary_table$model_id[which.min(summary_table$AIC)]
  top   <- results[[top_i]]
  stopifnot(!is.null(top), !is.null(top$best$fit))
  
  # fetch the 2 nealmon params for a predictor block
  get2 <- function(obj, base) unname(coef(obj)[paste0(base, 1:2)])
  
  # build weights using the variable-specific k from kmap
  w_block <- function(v) {
    kv <- top$best$kmap[[v]]  # <- per-variable k, not global kmax
    nealmon(get2(top$best$fit, v), kv)
  }
  
  vars <- top$xnames
  if (length(vars) == 0) stop("No predictor blocks found in the best model.")
  
  grDevices::png(filename = file, width = width, height = height, res = res)
  op <- par(no.readonly = TRUE)
  on.exit({ par(op); dev.off() }, add = TRUE)
  
  ncol <- ifelse(length(vars) >= 2, 2, 1)
  nrow <- ceiling(length(vars) / ncol)
  par(mfrow = c(nrow, ncol), mar = c(3.2, 3.2, 2.0, 0.8), mgp = c(2, 0.6, 0))
  
  for (v in vars) {
    w <- w_block(v)
    plot(seq_along(w), w, type = "h",
         main = paste("Nealmon weights:", v),
         xlab = "Monthly lag", ylab = "Weight")
    abline(h = 0, lty = 3)
  }
}

# call it:
plot_midas_weights(summary_table, results)
# -> Check the image written to: ./midas_weights.png





