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
  
  assign("y_tr", y_tr, envir = .GlobalEnv)
  
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







# 
# library(forecast)
# 
# # --- Helper you already have (kept here for completeness)
# extend_monthly <- function(x, n_future, method = c("arima","naive")) {
#   method <- match.arg(method)
#   if (n_future <= 0) return(x)
#   if (method == "arima") {
#     fit <- forecast::auto.arima(x, stepwise = TRUE, approximation = TRUE, seasonal = TRUE)
#     xf  <- forecast::forecast(fit, h = n_future)$mean
#   } else {
#     xf <- rep(tail(x, 1), n_future)
#   }
#   ts(c(as.numeric(x), as.numeric(xf)), start = start(x), frequency = frequency(x))
# }
# 
# # --- SAFE MIDAS forecaster: builds COMPLETE `newdata` with ALL regressors
# midas_forecast_all <- function(model_id, h = 4,
#                                method_monthly = c("arima","naive"),
#                                method = c("dynamic","static"),
#                                strict = TRUE) {
#   method_monthly <- match.arg(method_monthly)
#   method <- match.arg(method)
#   
#   # 1) Get the trained model + the names of monthly regressors in that spec
#   best   <- results[[model_id]]$best
#   fit    <- best$fit
#   xnames <- best$x        # character vector of monthly regressor names used
#   need   <- 3 * h         # months needed to form h quarters ahead
#   
#   # 2) Extend each MONTHLY regressor and force the SAME start as training
#   ext <- lapply(xnames, function(v) {
#     xm   <- get(v, inherits = TRUE)                      # training monthly (aligned to hf_start .. hf_end)
#     xm_e <- extend_monthly(xm, need, method_monthly)     # add 3*h months
#     window(xm_e, start = start(xm))                      # keep EXACT hf_start alignment
#   })
#   names(ext) <- xnames
#   
#   # 3) Sanity checks — prevent the dreaded “variable lengths differ”
#   lens   <- sapply(ext, length)
#   frqs   <- sapply(ext, frequency)
#   starts <- sapply(ext, function(z) paste(start(z), collapse = "-"))
#   
#   if (length(unique(frqs)) != 1 || unique(frqs) != 12) {
#     stop("All monthly regressors must have frequency 12. Got: ", paste(frqs, collapse = ", "))
#   }
#   if (length(unique(starts)) != 1) {
#     stop("Monthly regressors do not share the same start (hf_start). Starts: ",
#          paste(paste(xnames, starts, sep=":"), collapse = " | "))
#   }
#   if (length(unique(lens)) != 1) {
#     if (strict) {
#       stop("After extension, monthly regressors must have identical lengths. ",
#            "Lengths: ", paste(paste(xnames, lens, sep=":"), collapse = " | "))
#     } else {
#       # fallback: force a common end (min length) if you set strict = FALSE
#       Lmin <- min(lens)
#       ext  <- lapply(ext, function(z) window(z, end = tsp(z)[1] + (Lmin - 1)/12))
#       lens <- sapply(ext, length)
#     }
#   }
#   
#   # 4) Build COMPLETE `newdata`: y_rg (quarterly) + ALL monthly regressors
#   nd <- c(list(y_rg = y_rg), ext)
#   
#   # 5) Forecast
#   forecast(fit, newdata = nd, h = h, method = method)
# }
# 
# # ===== RUN IT =====
# top_i  <- summary_table$model_id[which.min(summary_table$AIC)]
# fc_top <- midas_forecast_all(top_i, h = 4, method_monthly = "arima", method = "dynamic")
# fc_top
# 
# 
# 
# 
# 
# 
# 
# 
# 
# midas_forecast_newdata <- function(model_id, h = 4, method_monthly = "arima") {
#   best   <- results[[model_id]]$best
#   xnames <- best$x
#   need   <- 3 * h
#   
#   # helper: extend and then force-start at hf_start to match training alignment
#   extend_and_align <- function(x) {
#     x_ext <- extend_monthly(x, need, method_monthly)
#     # force the same start and frequency as the training object
#     window(x_ext, start = start(x))   # x already starts at hf_start
#   }
#   
#   newdata <- list(y_rg = y_rg)  # AR lags of y
#   for (v in xnames) {
#     xv <- get(v, inherits = TRUE)     # training monthly series (already aligned)
#     newdata[[v]] <- extend_and_align(xv)
#   }
#   
#   forecast(best$fit, newdata = newdata, h = h)
# }
# 
# ## Try:
# fc_top <- midas_forecast_newdata(top_i, h = 4, method_monthly = "arima")
# fc_top
# 
# 
# 
# 
# 
# # ---- make sure these helpers exist ----
# extend_monthly <- function(x, n_future, method = c("arima","naive")) {
#   method <- match.arg(method)
#   if (n_future <= 0) return(x)
#   if (method == "arima") {
#     fit <- forecast::auto.arima(x, stepwise = TRUE, approximation = TRUE, seasonal = TRUE)
#     xf  <- forecast::forecast(fit, h = n_future)$mean
#   } else {
#     xf <- rep(tail(x, 1), n_future)
#   }
#   ts(c(as.numeric(x), as.numeric(xf)), start = start(x), frequency = frequency(x))
# }
# 
# .inc_start <- function(y, p, freq) {  # increment (year, period) within given freq
#   p2 <- p + 1
#   if (p2 > freq) c(y + 1, 1) else c(y, p2)
# }
# 
# # ---- robust MIDAS forecaster that returns EXACTLY h quarters ----
# midas_forecast_h <- function(model_id,
#                              h = 4,
#                              method_monthly = c("arima","naive"),
#                              method = c("dynamic","static")) {
#   method_monthly <- match.arg(method_monthly)
#   method <- match.arg(method)
#   
#   best   <- results[[model_id]]$best
#   fit    <- best$fit
#   xnames <- best$x
#   need   <- 3 * h
#   
#   # 1) extend each monthly series by exactly 3*h months and clamp its length
#   ext <- lapply(xnames, function(v){
#     xm       <- get(v, inherits = TRUE)            # aligned monthly training series
#     base_len <- length(xm)
#     xm_e     <- extend_monthly(xm, need, method_monthly)
#     # clamp to base_len + need (hard cap), preserve start/frequency
#     end_time <- tsp(xm)[1] + (base_len + need - 1)/12
#     window(xm_e, start = start(xm), end = end_time)
#   })
#   names(ext) <- xnames
#   
#   # 2) build newdata (be sure AR lags of y come from training target)
#   nd <- c(list(y_rg = y_rg), ext)
#   
#   # 3) forecast
#   fc <- forecast::forecast(fit, newdata = nd, h = h, method = method)
#   
#   # 4) normalize to a quarterly ts and cut to h steps
#   #    (some midasr/forecast paths can return a numeric vector)
#   if (!is.ts(fc)) {
#     fc <- ts(as.numeric(fc),
#              start = .inc_start(end(y_rg)[1], end(y_rg)[2], 4),
#              frequency = 4)
#   }
#   
#   # Now ensure we return exactly the first h quarters that follow the last observed y
#   # If fc already starts at the next quarter (typical), this simply trims fc to h.
#   start_fc <- start(fc)
#   freq_fc  <- frequency(fc)
#   # Take the first h observations from fc (safe even if times aren’t comparable)
#   fc_h <- window(fc, end = start_fc + (h - 1)/freq_fc)
#   
#   return(fc_h)
# }
# 
# # ---- usage: grab the AIC-best model and get its next 4 quarter-ahead forecasts ----
# top_i <- summary_table$model_id[which.min(summary_table$AIC)]
# fc_4q <- midas_forecast_h(top_i, h = 4, method_monthly = "arima", method = "dynamic")
# fc_4q



# # 5) (Optional) plot weights for the overall best model
# ## --- Replace your current weight-plot section with this ---
# 
# if (nrow(summary_table) > 0) {
#   top_i <- summary_table$model_id[which.min(summary_table$AIC)]
#   top   <- results[[top_i]]
#   
#   if (!is.null(top) && !is.null(top$best$fit)) {
#     # helper to fetch the 2 nealmon parameters for a given variable block
#     get2 <- function(obj, base) unname(coef(obj)[paste0(base, 1:2)])
#     
#     # proper weight builder: use the variable-specific k (from best$kmap)
#     w_block <- function(v) {
#       kv <- top$best$kmap[[v]]  # <- this is the correct length for v
#       nealmon(get2(top$best$fit, v), kv)
#     }
#     
#     vars <- top$xnames
#     
#     # Open a device with enough space (prevents "figure margins too large")
#     # In RStudio, dev.cur() == 1 is the null device; try to open a larger one
#     if (identical(dev.cur(), 1L)) {
#       try(dev.new(width = 10, height = 6), silent = TRUE)
#     }
#     
#     op <- par(no.readonly = TRUE)
#     on.exit(par(op), add = TRUE)
#     
#     # Tighter margins; 2 columns is usually a good default
#     nrow_pl <- ceiling(length(vars) / 2)
#     par(mfrow = c(nrow_pl, 2), mar = c(3.5, 3.5, 2, 1), mgp = c(2.1, 0.6, 0))
#     
#     for (v in vars) {
#       w <- w_block(v)
#       plot(seq_along(w), w, type = "h",
#            main = paste("Nealmon weights:", v),
#            xlab = "Monthly lag", ylab = "Weight")
#       abline(h = 0, lty = 3)
#     }
#   }
# }

# (Optional) If your plotting pane is still tiny, write to file instead:
# png("midas_weights.png", width = 1400, height = 900, res = 150)
#   ... same plotting loop ...
# dev.off()














# 
# # -------- Separate models (one-at-a-time) --------
# separate_specs <- lapply(available, function(x) c(x))
# 
# # # -------- Suggested small combos (stable) --------
# # suggested_specs <- list()
# # # oil value + one trade or money
# # if (present("oil_g")){
# #   if (present("imp_g")) suggested_specs <- c(suggested_specs, list(c("oil_g","imp_g")))
# #   if (present("nno_g")) suggested_specs <- c(suggested_specs, list(c("oil_g","nno_g")))
# #   if (present("rex_g")) suggested_specs <- c(suggested_specs, list(c("oil_g","rex_g")))
# #   if (present("m1_g"))  suggested_specs <- c(suggested_specs, list(c("oil_g","m1_g")))
# #   if (present("m2_g"))  suggested_specs <- c(suggested_specs, list(c("oil_g","m2_g")))
# # }
# # # oil components (avoid mixing with oil_g to reduce duplication)
# # if (present("oprice_g") && present("oprod_g")){
# #   suggested_specs <- c(suggested_specs, list(c("oprice_g","oprod_g")))
# #   if (present("m1_g")) suggested_specs <- c(suggested_specs, list(c("oprice_g","oprod_g","m1_g")))
# #   if (present("m2_g")) suggested_specs <- c(suggested_specs, list(c("oprice_g","oprod_g","m2_g")))
# # }
# # # gas + oil price
# # if (present("oprice_g") && present("gprice_g")){
# #   suggested_specs <- c(suggested_specs, list(c("oprice_g","gprice_g")))
# # }
# # # trade trio (baseline without energy)
# # if (all(sapply(c("imp_g","nno_g","rex_g"), present))){
# #   suggested_specs <- c(suggested_specs, list(c("imp_g","nno_g","rex_g")))
# # }
# # # dedupe / cap length
# # norm_key <- function(v) paste(sort(unique(v)), collapse=",")
# # seen <- new.env(parent=emptyenv())
# # suggested_specs <- Filter(function(v) length(v)>0 && length(v)<=max_predictors, suggested_specs)
# # suggested_specs <- Filter(function(v){ k <- norm_key(v); if (exists(k, seen)) FALSE else { assign(k, TRUE, seen); TRUE }}, suggested_specs)
# # 
# # # -------- Master spec list --------
# # spec_list <- c(separate_specs, suggested_specs)
# # 
# # cat("Total models to estimate:", length(spec_list), "\n")
# 
# # -------- Fit, summarize, diagnose, holdout --------
# summary_table <- data.frame(
#   model_id = integer(), type = character(), predictors = character(),
#   L = integer(), kmax = integer(), AIC = numeric(), BIC = numeric(),
#   Ljung_p = numeric(), R2 = numeric(), AdjR2 = numeric(),
#   RMSE = numeric(), MAE = numeric(), MASE = numeric(),
#   stringsAsFactors = FALSE
# )
# results <- vector("list", length(spec_list))
# 
# for (i in seq_along(spec_list)) {
#   xnames <- spec_list[[i]]
#   label <- if (length(xnames) == 1) "SEPARATE" else "SUGGESTED"
#   
#   cat("\n==============================\n",
#       "Spec ", i, " [", label, "]: ", paste(xnames, collapse=", "), "\n",
#       "==============================\n", sep="")
#   
#   best <- try(fit_nealmon_over_L("y_rg", xnames, L_set), silent = TRUE)
#   if (inherits(best, "try-error")) {
#     cat("  -> skipped (fit failed)\n")
#     next
#   }
#   
#   print(safe_summary(best$fit))
#   dg <- diag_report(best$fit, y_rg)
#   hv <- holdout_eval(best, y_rg, xnames)
#   
#   summary_table <- rbind(summary_table, data.frame(
#     model_id = i, type = label, predictors = paste(xnames, collapse=","),
#     L = best$L, kmax = best$kmax, AIC = best$AIC, BIC = best$BIC,
#     Ljung_p = dg$Ljung_p, R2 = dg$R2, AdjR2 = dg$AdjR2,
#     RMSE = hv$rmse, MAE = hv$mae, MASE = hv$mase
#   ))
#   results[[i]] <- list(best = best, diag = dg, holdout = hv, xnames = xnames)
# }
# 
# # -------- Leader board --------
# summary_table <- summary_table[order(summary_table$type, summary_table$AIC, summary_table$MASE), ]
# cat("\n\n==== SUMMARY (Top by AIC within type) ====\n")
# print(do.call(rbind, by(summary_table, summary_table$type, function(dd) head(dd[order(dd$AIC), ], 10))), row.names = FALSE)
# 
# # -------- Optional: plot Nealmon weights for best overall --------
# if (nrow(summary_table) > 0) {
#   top_i <- summary_table$model_id[which.min(summary_table$AIC)]
#   top <- results[[top_i]]
#   if (!is.null(top)) {
#     get2 <- function(obj, base) unname(coef(obj)[paste0(base, 1:2)])
#     w_block <- function(v) nealmon(get2(top$best$fit, v), top$best$kmax)
#     vars <- top$xnames
#     op <- par(no.readonly = TRUE); on.exit(par(op))
#     par(mfrow=c(min(2,length(vars)), ceiling(length(vars)/2)))
#     for (v in vars) {
#       w <- w_block(v)
#       plot(w, type="h", main=paste("Nealmon Weights:", v), xlab="Monthly lag", ylab="weight")
#     }
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ============================
# # Add & fit extra multi-predictor combos
# # ============================
# 
# # 1) Define raw combos (economic logic, <=5 predictors)
# specs_extra_raw <- list(
#   Trade_M1          = c("imp_g","nno_g","rex_g","m1_g"),
#   Trade_M2          = c("imp_g","nno_g","rex_g","m2_g"),
#   OilComp_Gas       = c("oprice_g","oprod_g","gprice_g"),
#   OilComp_M1        = c("oprice_g","oprod_g","m1_g"),
#   OilComp_M2        = c("oprice_g","oprod_g","m2_g"),
#   OilVal_Trade_M1   = c("oil_g","imp_g","nno_g","m1_g"),
#   OilVal_Trade_M2   = c("oil_g","imp_g","rex_g","m2_g")
# )
# 
# # 2) Guards ---------------------------------------------------------
# if (!exists("present", inherits = TRUE)) {
#   present <- function(v) exists(v, inherits = TRUE) && !is.null(get(v, inherits = TRUE))
# }
# 
# mixes_oil_value_and_components <- function(v){
#   has_val  <- "oil_g" %in% v
#   has_comp <- any(c("oprice_g","oprod_g","gprice_g") %in% v)
#   has_val && has_comp
# }
# 
# filter_combo <- function(v) {
#   v2 <- unique(v[sapply(v, present)])
#   if (length(v2) == 0) return(character(0))
#   if (exists("max_predictors", inherits = TRUE) && length(v2) > max_predictors) return(character(0))
#   if (mixes_oil_value_and_components(v2)) return(character(0))
#   v2
# }
# 
# specs_extra <- lapply(specs_extra_raw, filter_combo)
# specs_extra <- specs_extra[vapply(specs_extra, length, 1L) > 0]
# 
# # De-duplicate against existing 'specs' (if any)
# exists_specs <- exists("specs", inherits = TRUE) && is.list(specs)
# if (exists_specs) {
#   have_keys <- vapply(specs, function(v) paste(sort(v), collapse=","), "")
#   new_specs <- list()
#   for (nm in names(specs_extra)) {
#     key <- paste(sort(specs_extra[[nm]]), collapse=",")
#     if (!(key %in% have_keys)) new_specs[[nm]] <- specs_extra[[nm]]
#   }
#   specs <- c(specs, new_specs)
# } else {
#   specs <- specs_extra
# }
# 
# cat("Extra candidate specs added:", length(specs_extra), "\n")
# if (length(specs_extra) == 0) {
#   cat("No new viable combos (availability/collinearity/size rules).\n")
# }
# 
# # 3) Fit the extra specs only --------------------------------------
# if (!exists("results", inherits = TRUE) || !is.list(results)) results <- list()
# 
# needed_cols <- c("model_id","type","predictors","L","kmax","AIC","BIC",
#                  "Ljung_p","R2","AdjR2","RMSE","MAE","MASE")
# if (!exists("summary_table", inherits = TRUE) || !all(needed_cols %in% names(summary_table))) {
#   summary_table <- data.frame(
#     model_id = integer(), type = character(), predictors = character(),
#     L = integer(), kmax = integer(), AIC = numeric(), BIC = numeric(),
#     Ljung_p = numeric(), R2 = numeric(), AdjR2 = numeric(),
#     RMSE = numeric(), MAE = numeric(), MASE = numeric(),
#     stringsAsFactors = FALSE
#   )
# }
# 
# # scalarizers to avoid length-0 issues
# # scalarizers to avoid length-0 issues
# .as_num1 <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x[1])
# .as_chr1 <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[1])
# 
# # little helper to tolerate name variants (e.g., "AdjR2" vs "adjR2")
# .get_dg_num <- function(dg, candidates) {
#   for (nm in candidates) {
#     v <- try(dg[[nm]], silent = TRUE)
#     if (!inherits(v, "try-error") && !is.null(v)) return(.as_num1(v))
#   }
#   NA_real_
# }
# 
# 
# # -------- Robust AdjR2 grabber and appender --------
# get_adjR2 <- function(dg, best) {
#   # Try both name variants first
#   if (!is.null(dg$AdjR2)) return(as.numeric(dg$AdjR2))
#   if (!is.null(dg$adjR2)) return(as.numeric(dg$adjR2))
#   # Fallback: compute from R2 if possible
#   R2 <- suppressWarnings(as.numeric(dg$R2))
#   if (is.na(R2)) return(NA_real_)
#   # n and k
#   n <- tryCatch(length(residuals(best$fit)), error = function(e) NA_integer_)
#   k <- tryCatch(length(coef(best$fit)),      error = function(e) NA_integer_)
#   if (is.na(n) || is.na(k) || n <= k + 1) return(NA_real_)
#   1 - (1 - R2) * ((n - 1) / (n - k))
# }
# 
# append_summary_row <- function(tbl, model_id, type, xnames, best, dg, hv){
#   adjR2_val <- get_adjR2(dg, best)
#   newrow <- data.frame(
#     model_id   = .as_num1(model_id),
#     type       = .as_chr1(type),
#     predictors = .as_chr1(paste(xnames, collapse=",")),
#     L          = .as_num1(best$L),
#     kmax       = .as_num1(best$kmax),
#     AIC        = .as_num1(best$AIC),
#     BIC        = .as_num1(best$BIC),
#     Ljung_p    = .as_num1(dg$Ljung_p),
#     R2         = .as_num1(dg$R2),
#     AdjR2      = .as_num1(adjR2_val),
#     RMSE       = .as_num1(hv$rmse),
#     MAE        = .as_num1(hv$mae),
#     MASE       = .as_num1(hv$mase),
#     stringsAsFactors = FALSE
#   )
#   # align columns
#   missing_cols <- setdiff(names(tbl), names(newrow))
#   if (length(missing_cols)) newrow[missing_cols] <- NA
#   newrow <- newrow[, names(tbl), drop = FALSE]
#   rbind(tbl, newrow)
# }
# 
# 
# type_tag <- "ADDED"
# 
# # start model_id after current summary_table rows (simple, robust)
# next_model_id <- if (nrow(summary_table)) max(summary_table$model_id, na.rm = TRUE) + 1L else 1L
# 
# for (nm in names(specs_extra)) {
#   xnames <- specs_extra[[nm]]
#   cat("\n==============================\nCombo [", type_tag, "] ", nm, ": ",
#       paste(xnames, collapse=", "), "\n", sep = "")
#   best <- try(fit_nealmon_over_L("y_rg", xnames, L_set), silent = TRUE)
#   if (inherits(best, "try-error") || is.null(best$fit)) {
#     cat("  -> skipped (fit failed)\n")
#     next
#   }
#   print(safe_summary(best$fit))
#   
#   # diagnostics and holdout
#   dg <- try(diag_report(best$fit, y_rg), silent = TRUE)
#   if (inherits(dg, "try-error")) dg <- list(R2=NA, adjR2=NA, Ljung_p=NA)
#   hv <- try(holdout_eval(best, y_rg, xnames), silent = TRUE)
#   if (inherits(hv, "try-error")) hv <- list(rmse=NA, mae=NA, mase=NA, h=0)
#   
#   # store & append
#   model_id <- next_model_id; next_model_id <- next_model_id + 1L
#   results[[model_id]] <- list(best = best, diag = dg, holdout = hv,
#                               xnames = xnames, name = nm, type = type_tag)
#   
#   summary_table <- append_summary_row(summary_table, model_id, type_tag, xnames, best, dg, hv)
# }
# 
# # 4) Leader board: show best of the newly ADDED sets
# dd <- subset(summary_table, type == type_tag)
# if (nrow(dd)) {
#   dd <- dd[order(dd$AIC, dd$MASE), ]
#   cat("\n\n==== SUMMARY (Top ADDED by AIC) ====\n")
#   print(head(dd, 10), row.names = FALSE)
# } else {
#   cat("\nNo ADDED models were successfully estimated.\n")
# }
# 
# if (exists("summary_table", inherits = TRUE) &&
#     nrow(summary_table) && exists("results", inherits = TRUE)) {
#   na_rows <- which(is.na(summary_table$AdjR2))
#   for (ii in na_rows) {
#     mid <- summary_table$model_id[ii]
#     if (!is.na(mid) && !is.null(results[[mid]]) &&
#         !is.null(results[[mid]]$best) && !is.null(results[[mid]]$best$fit)) {
#       # Rebuild a minimal dg to reuse the helper
#       dg_tmp <- list(R2 = summary_table$R2[ii], Ljung_p = summary_table$Ljung_p[ii])
#       adjR2_new <- get_adjR2(dg_tmp, results[[mid]]$best)
#       summary_table$AdjR2[ii] <- adjR2_new
#     }
#   }
# }
# 
# 
# # Split into SEPARATE, SUGGESTED, ADDED
# separate_tbl  <- subset(summary_table, type == "SEPARATE")
# suggested_tbl <- subset(summary_table, type == "SUGGESTED")
# added_tbl     <- subset(summary_table, type == "ADDED")
# 
# # write_xlsx(
# #   list(
# #     SEPARATE  = separate_tbl,
# #     SUGGESTED = suggested_tbl,
# #     ADDED     = added_tbl
# #   ),
# #   "MIDAS_results_RGDP.xlsx"
# # )
# 
# 
# 
# 
# # 
# # # ==========================================================
# # # EXPORT: per-model coefficient results (with full variable names)
# # # ==========================================================
# # suppressPackageStartupMessages({
# #   library(dplyr); library(purrr); library(stringr); library(tidyr)
# #   library(writexl)
# # })
# # 
# # stopifnot(exists("results", inherits = TRUE), is.list(results))
# # 
# # # --- Dictionary: short code → descriptive name ---
# # var_labels <- c(
# #   "oil_g"   = "Average oil price",
# #   "oprod_g" = "Daily Average Production",
# #   "gprice_g"= "Natural oil Gas",
# #   "m1_g"    = "Narrow Money (M1)",
# #   "m2_g"    = "Broad Money",
# #   "imp_g"   = "Total Recorded Merchandise Imports",
# #   "nno_g"   = "Non-Oil Omani Exports",
# #   "rex_g"   = "Re-Exports",
# #   "nonoil_g"= "Non-Oil Exports",
# #   "y_rg"     = "Real GDP"
# # )
# # 
# # # --- Helpers ---
# # sheet_name <- function(mid, obj) {
# #   x <- try(obj$xnames, silent = TRUE)
# #   lab <- if (!inherits(x, "try-error") && length(x))
# #     paste(x, collapse = "_") else "model"
# #   nm <- sprintf("M%02d_%s", as.integer(mid), lab)
# #   nm <- gsub("[^A-Za-z0-9_]", "_", nm)
# #   substr(nm, 1, 31)
# # }
# # 
# # safe_summary <- function(fit){
# #   out <- try(summary(fit, robust = TRUE), silent = TRUE)
# #   if (inherits(out, "try-error")) summary(fit, robust = FALSE) else out
# # }
# # 
# # coef_table_from_fit <- function(fit) {
# #   sm <- safe_summary(fit)
# #   cm <- NULL
# #   for (cand in c("coefficients", "Coef", "coef")) {
# #     v <- try(sm[[cand]], silent = TRUE)
# #     if (!inherits(v, "try-error") && !is.null(v)) { cm <- v; break }
# #   }
# #   if (is.null(cm)) {
# #     cf <- try(coef(fit), silent = TRUE)
# #     V  <- try(vcov(fit),  silent = TRUE)
# #     if (inherits(cf, "try-error") || is.null(cf)) {
# #       return(tibble(term=character(), Estimate=double(),
# #                     `Std. Error`=double(), `t value`=double(),
# #                     `Pr(>|t|)`=double(), term_full=character()))
# #     }
# #     se <- if (!inherits(V, "try-error") && !is.null(V)) sqrt(pmax(diag(as.matrix(V)), 0)) else NA_real_
# #     tt <- cf / se
# #     pp <- 2*pnorm(abs(tt), lower.tail = FALSE)
# #     df <- tibble(term = names(cf),
# #                  Estimate = as.numeric(cf),
# #                  `Std. Error` = as.numeric(se),
# #                  `t value` = as.numeric(tt),
# #                  `Pr(>|t|)` = as.numeric(pp))
# #   } else {
# #     df <- as.data.frame(cm)
# #     df <- tibble::rownames_to_column(df, "term")
# #     names(df) <- sub("^Std\\.Error$", "Std. Error", names(df))
# #     names(df) <- sub("^Std\\. Error$", "Std. Error", names(df))
# #     names(df) <- sub("^t$", "t value", names(df))
# #     names(df) <- sub("^Pr\\(.+\\)$", "Pr(>|t|)", names(df))
# #     need <- c("term","Estimate","Std. Error","t value","Pr(>|t|)")
# #     for (n in need) if (!n %in% names(df)) df[[n]] <- NA_real_
# #     df <- df[need]
# #   }
# #   
# #   # --- add full descriptive name (strip trailing digits) ---
# #   # --- add full descriptive name with lag index (no 'base' column) ---
# #   df <- df %>%
# #     mutate(
# #       is_lag = grepl("[0-9]+$", term),
# #       lag_no = ifelse(is_lag, sub(".*?(\\d+)$", "\\1", term), NA_character_),
# #       base   = sub("[0-9]+$", "", term),
# #       term_full = dplyr::case_when(
# #         term == "(Intercept)"                 ~ "(Intercept)",
# #         base == "y_rg" & !is.na(lag_no)       ~ paste0("Non-Oil GDP [AR lag ", lag_no, "]"),
# #         base %in% names(var_labels) & !is.na(lag_no)
# #         ~ paste0(var_labels[base], " [lag ", lag_no, "]"),
# #         base %in% names(var_labels)          ~ var_labels[base],
# #         TRUE                                 ~ term
# #       )
# #     ) %>%
# #     select(term, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`, term_full)
# # }
# # 
# # 
# # header_block <- function(mid, obj){
# #   best <- obj$best
# #   preds_codes <- if (!is.null(obj$xnames)) obj$xnames else character(0)
# #   preds_full  <- if (length(preds_codes)) {
# #     paste(ifelse(preds_codes %in% names(var_labels),
# #                  unname(var_labels[preds_codes]), preds_codes), collapse = ", ")
# #   } else NA_character_
# #   
# #   tibble(
# #     Field = c("model_id","type","predictors","L","kmax","AIC","BIC"),
# #     Value = c(
# #       mid,
# #       if (!is.null(obj$type)) obj$type else NA_character_,
# #       preds_full,
# #       if (!is.null(best$L)) best$L else NA_real_,
# #       if (!is.null(best$kmax)) best$kmax else NA_real_,
# #       if (!is.null(best$AIC)) best$AIC else NA_real_,
# #       if (!is.null(best$BIC)) best$BIC else NA_real_
# #     )
# #   )
# # }
# # 
# # # --- Build all sheets ---
# # sheets <- list()
# # for (i in seq_along(results)) {
# #   obj <- results[[i]]
# #   if (is.null(obj) || is.null(obj$best) || is.null(obj$best$fit)) next
# #   fit <- obj$best$fit
# #   
# #   head_df <- header_block(i, obj)
# #   coef_df <- coef_table_from_fit(fit)
# #   
# #   out_df <- bind_rows(
# #     tibble(term = paste(head_df$Field, head_df$Value, sep = ": "),
# #            Estimate = NA_real_, `Std. Error` = NA_real_,
# #            `t value` = NA_real_, `Pr(>|t|)` = NA_real_,
# #            term_full = NA_character_),
# #     tibble(term = "", Estimate = NA_real_, `Std. Error` = NA_real_,
# #            `t value` = NA_real_, `Pr(>|t|)` = NA_real_,
# #            term_full = ""),
# #     coef_df
# #   )
# #   
# #   nm <- sheet_name(i, obj)
# #   k <- 1; base_nm <- nm
# #   while (nm %in% names(sheets)) {
# #     suffix <- sprintf("_%d", k); k <- k + 1
# #     nm <- substr(paste0(base_nm, suffix), 1, 31)
# #   }
# #   sheets[[nm]] <- out_df
# # }
# # 
# # outfile <- "MIDAS_model_results_ALLRGDP.xlsx"
# # write_xlsx(sheets, path = outfile)
# # cat("Wrote per-model results (with full names) to:", outfile, "\n")
# 
# 
# 
# 
# 
# # ==========================================================
# # EXPORT: all model coefficients in ONE sheet (REAL GDP target = y_rg)
# # ==========================================================
# suppressPackageStartupMessages({
#   library(dplyr); library(purrr); library(stringr); library(tidyr)
#   library(writexl)
# })
# 
# stopifnot(exists("results", inherits = TRUE), is.list(results))
# 
# # Dictionary: short code → descriptive name
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
#   "y_rg"    = "Real GDP"
# )
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
#   
#   if (is.null(cm)) {
#     cf <- try(coef(fit), silent = TRUE)
#     V  <- try(vcov(fit),  silent = TRUE)
#     if (inherits(cf, "try-error") || is.null(cf)) {
#       return(tibble::tibble(term=character(), Estimate=double(),
#                             `Std. Error`=double(), `t value`=double(),
#                             `Pr(>|t|)`=double(), term_full=character()))
#     }
#     se <- if (!inherits(V, "try-error") && !is.null(V)) sqrt(pmax(diag(as.matrix(V)), 0)) else NA_real_
#     tt <- cf / se
#     pp <- 2*pnorm(abs(tt), lower.tail = FALSE)
#     df <- tibble::tibble(term = names(cf),
#                          Estimate = as.numeric(cf),
#                          `Std. Error` = as.numeric(se),
#                          `t value` = as.numeric(tt),
#                          `Pr(>|t|)` = as.numeric(pp))
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
#   # ----- Build human-readable 'term_full' (no case_when, no recycling) -----
#   term   <- df$term
#   is_int <- term == "(Intercept)"
#   is_lag <- grepl("[0-9]+$", term)
#   lag_no <- ifelse(is_lag, sub(".*?(\\d+)$", "\\1", term), NA_character_)
#   base   <- sub("[0-9]+$", "", term)
#   
#   labels <- unname(var_labels[base])
#   labels[is.na(labels)] <- NA_character_
#   
#   term_full <- term
#   # AR terms for REAL GDP (base == "y_rg")
#   mask_ar <- base == "y_rg" & !is.na(lag_no) & !is_int
#   term_full[mask_ar] <- paste0("Real GDP [AR lag ", lag_no[mask_ar], "]")
#   
#   # Monthly lags with known labels
#   mask_lag <- !mask_ar & !is.na(labels) & !is.na(lag_no) & !is_int
#   term_full[mask_lag] <- paste0(labels[mask_lag], " [lag ", lag_no[mask_lag], "]")
#   
#   # Unlagged terms with known labels (rare here)
#   mask_base <- !mask_ar & is.na(lag_no) & !is.na(labels) & !is_int
#   term_full[mask_base] <- labels[mask_base]
#   
#   df$term_full <- term_full
#   df[, c("term","term_full","Estimate","Std. Error","t value","Pr(>|t|)")]
# }
# 
# # ---- Build one big data frame: add model metadata as columns ----
# all_coefs <- purrr::map2_dfr(seq_along(results), results, function(mid, obj){
#   if (is.null(obj) || is.null(obj$best) || is.null(obj$best$fit)) return(NULL)
#   fit  <- obj$best$fit
#   meta <- tibble::tibble(
#     model_id   = mid,
#     type       = if (!is.null(obj$type)) obj$type else if (length(obj$xnames)==1) "SEPARATE" else "SUGGESTED",
#     predictors = paste(obj$xnames, collapse = ", "),
#     predictors_full = paste(ifelse(obj$xnames %in% names(var_labels),
#                                    unname(var_labels[obj$xnames]), obj$xnames), collapse = ", "),
#     L          = obj$best$L,
#     kmax       = obj$best$kmax,
#     AIC        = obj$best$AIC,
#     BIC        = obj$best$BIC
#   )
#   coef_table_from_fit(fit) %>%
#     tidyr::crossing(meta) %>%   # attach meta to each coefficient row
#     dplyr::select(model_id, type, predictors, predictors_full, L, kmax, AIC, BIC,
#                   term, term_full, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)
# })
# 
# # ---- Write ONE sheet ----
# outfile <- "MIDAS_model_results_RealGDP.xlsx"
# # writexl::write_xlsx(list(ALL_MODELS = all_coefs), path = outfile)
# cat("Wrote all model coefficients to one sheet:", outfile, "\n")
