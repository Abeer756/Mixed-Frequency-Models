## ===========================
# Mixed-Frequency MIDAS (Nealmon) — NOMINAL GDP
# FIXED VERSION - Addresses Multicollinearity
# ===========================

# --- Packages
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(midasr)
library(janitor)
library(forecast)
library(sandwich)
library(lmtest)
library(openxlsx)

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
predictors <- predictors %>% mutate(across(-month, ~ as.numeric(as.character(.))))

# Read NOMINAL GDP data
quart_ngdp <- read_excel("quart_ngdp.xlsx")

m_start <- c(lubridate::year(min(predictors$month)), lubridate::month(min(predictors$month)))

# ======================
# 1) Monthly predictors (Build monthly level ts)
# ======================
tot_mer_imports    <- ts(predictors$tot_mer_imports, start = m_start, frequency = 12)
oil_gaz_exports    <- ts(predictors$oil_gaz_exports, start = m_start, frequency = 12)
noil_omani_exports <- ts(predictors$noil_omani_exports, start = m_start, frequency = 12)
re_exports         <- ts(predictors$re_exports, start = m_start, frequency = 12)
noil_exports       <- ts(predictors$noil_exports, start = m_start, frequency = 12)
oil_price          <- ts(predictors$oil_price, start = m_start, frequency = 12)
d_avg_prod         <- ts(predictors$d_avg_prod, start = m_start, frequency = 12)
gas_price          <- ts(predictors$gas_price, start = m_start, frequency = 12)
M1                 <- ts(predictors$M1, start = m_start, frequency = 12)
M2                 <- ts(predictors$M2, start = m_start, frequency = 12)

# --- Helper functions
.inc_start <- function(y, p, freq){
  p2 <- p + 1
  if(p2 > freq) c(y + 1, 1) else c(y, p2)
}

logdiff_pct <- function(x) 100*diff(log(pmax(as.numeric(x), 1e-9)))

# ====================================================================
# 2) Quarterly NOMINAL GDP target
# ====================================================================
ngdp_q <- quart_ngdp %>%
  mutate(qtr = as.yearqtr(gsub("-", " ", year_quarter))) %>%
  arrange(qtr) %>%
  select(qtr, NGDP, NGDP_oil, NGDP_Noil)

nq_start <- c(year(as.Date(ngdp_q$qtr[1])), quarter(as.Date(ngdp_q$qtr[1])))

# Target: NOMINAL GDP
y_nq <- ts(ngdp_q$NGDP, start = nq_start, frequency = 4)

# ======================
# 3) Transform to growth and ALIGN spans
# ======================
# Quarterly NOMINAL GDP growth (QoQ)
y_ng <- ts(logdiff_pct(y_nq), start = .inc_start(nq_start[1], nq_start[2], 4), frequency = 4)

# Quarter span for alignment of monthly series
qy_start <- start(y_ng)
qy_end   <- end(y_ng)
hf_start <- c(qy_start[1], (qy_start[2]-1)*3 + 1)
hf_end   <- c(qy_end[1],   qy_end[2]*3)

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

# ==============================================================================================================
# MIDAS Model Functions
# ==============================================================================================================
K <- 3                 # 3 months per quarter
L_set <- 3:5           # search 9–15 monthly lags
max_predictors <- 5

present <- function(v) {
  if (!exists(v, inherits = TRUE)) return(FALSE)
  obj <- get(v, inherits = TRUE)
  is.ts(obj) && length(obj) > 0 && all(is.finite(obj[is.finite(obj)]))
}

nealmon_starts <- function(xnames, yname){
  set.seed(123)
  ans <- setNames(lapply(seq_along(xnames), function(i) runif(2, 0.05, 0.20)), xnames)
  ans[[yname]] <- c(0.1,0.1,0.1)
  ans
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

safe_vcov_stats <- function(fit){
  V <- try(vcov(fit), silent = TRUE)
  if (inherits(V, "try-error") || is.null(V)) return(list(cond_vcov = NA_real_, min_eig = NA_real_))
  V <- as.matrix(V)
  V <- (V + t(V))/2
  list(
    cond_vcov = tryCatch(kappa(V, exact = TRUE), error = function(e) NA_real_),
    min_eig   = tryCatch(min(Re(eigen(V, symmetric = FALSE, only.values = TRUE)$values)),
                         error = function(e) NA_real_)
  )
}

kmax_map_for <- function(xnames, L, K, default_kmax){
  km <- setNames(rep(default_kmax, length(xnames)), xnames)
  # No special shortening needed for lean models
  km
}

build_formula_kmap <- function(yname, xnames, kmap, K){
  form <- as.formula(paste0(yname, " ~ mls(", yname, ", c(1,2,4), m=1)"))
  for (v in xnames) {
    form <- update(form, paste0(". ~ . + fmls(", v, ", ", kmap[[v]], ", ", K, ", nealmon)"))
  }
  form
}

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

holdout_eval <- function(best, y_full, xnames, n_test = min(8, length(y_full) - 20)){
  if (is.null(best) || n_test <= 0) return(list(rmse=NA, mae=NA, mase=NA, h=0))
  
  y_tr <- ts(head(y_full, length(y_full) - n_test), start = start(y_full), frequency = frequency(y_full))
  
  qy_tr_start <- start(y_tr); qy_tr_end <- end(y_tr)
  hf_start_tr <- c(qy_tr_start[1], (qy_tr_start[2]-1)*3 + 1)
  hf_end_tr   <- c(qy_tr_end[1],   qy_tr_end[2]*3)
  
  x_tr_names <- paste0(xnames, "_tr")
  for (i in seq_along(xnames)) {
    xi <- get(xnames[i], inherits = TRUE)
    assign(x_tr_names[i], window(xi, start = hf_start_tr, end = hf_end_tr), envir = .GlobalEnv)
  }
  
  # Create kmap for training data with renamed variables
  kmap_tr <- setNames(best$kmap[xnames], x_tr_names)
  
  form_tr <- build_formula_kmap("y_tr", x_tr_names, kmap_tr, K)
  starts_tr <- nealmon_starts(x_tr_names, "y_tr")
  fit_tr <- midas_r(form_tr, start = starts_tr, OLS = TRUE)
  
  newdat <- c(setNames(lapply(x_tr_names, get, inherits = TRUE), x_tr_names), list(y_tr = y_tr))
  fc <- forecast(fit_tr, newdata = newdat, h = n_test)
  
  y_te <- tail(y_full, n_test)
  ae   <- abs(y_te - fc$mean)
  rmse <- sqrt(mean((y_te - fc$mean)^2, na.rm = TRUE))
  mae  <- mean(ae, na.rm = TRUE)
  mase <- mae / mean(abs(diff(y_tr, lag = 4)), na.rm = TRUE)
  list(rmse=rmse, mae=mae, mase=mase, h=n_test)
}

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

# ==============================================================================================================
# Sanity Checks
# ==============================================================================================================
cat("\n===============================================\n")
cat("MIDAS MODEL FOR NOMINAL GDP GROWTH (FIXED)\n")
cat("===============================================\n")
cat("Target (NGDP) span:", paste(start(y_ng), collapse="-"), "to", paste(end(y_ng), collapse="-"), "\n\n")

for (nm in c("tot_mer_imports_g","oil_gaz_exports_g","noil_omani_exports_g","re_exports_g",
             "noil_exports_g","oil_price_g","d_avg_prod_g","gas_price_g","M1_g","M2_g")) {
  s <- start(get(nm)); e <- end(get(nm))
  cat(sprintf("%-22s %s to %s\n", nm, paste(s, collapse="-"), paste(e, collapse="-")))
}

# ==============================================================================================================
# Define Model Specifications - FIXED TO AVOID MULTICOLLINEARITY
# ==============================================================================================================
keep_if_available <- function(v) if (all(sapply(v, present))) v else NULL

# ORIGINAL SPECS (Keep baseline models that work)
A_oilex  <- keep_if_available(c("oil_gaz_exports_g","noil_omani_exports_g","re_exports_g","tot_mer_imports_g"))
B1_oilex <- keep_if_available(c("oil_gaz_exports_g","noil_omani_exports_g","re_exports_g","tot_mer_imports_g","M1_g"))
B2_oilex <- keep_if_available(c("oil_gaz_exports_g","noil_omani_exports_g","re_exports_g","tot_mer_imports_g","M2_g"))

# LEAN SPECS - Drop weak/collinear predictors (re-exports, imports often insignificant)
D1_oilex <- keep_if_available(c("oil_gaz_exports_g","noil_omani_exports_g","M1_g"))
D2_oilex <- keep_if_available(c("oil_gaz_exports_g","noil_omani_exports_g","M2_g"))

# OIL PRICE SPECS - Use oil price instead of oil exports (avoids collinearity)
E1_price <- keep_if_available(c("oil_price_g","noil_omani_exports_g","M1_g"))
E2_price <- keep_if_available(c("oil_price_g","noil_omani_exports_g","M2_g"))

# PRODUCTION SPECS - Simplified production-based models
D1_prod <- keep_if_available(c("d_avg_prod_g","noil_omani_exports_g","M1_g"))
D2_prod <- keep_if_available(c("d_avg_prod_g","noil_omani_exports_g","M2_g"))

# VERY LEAN - Minimal models with only strongest predictors
F1_minimal <- keep_if_available(c("oil_gaz_exports_g","M1_g"))
F2_minimal <- keep_if_available(c("oil_price_g","M1_g"))

specs_custom <- Filter(Negate(is.null),
                       list(
                         A_oilex = A_oilex,  
                         B1_oilex = B1_oilex,  
                         B2_oilex = B2_oilex,
                         D1_oilex = D1_oilex,
                         D2_oilex = D2_oilex,
                         E1_price = E1_price,
                         E2_price = E2_price,
                         D1_prod = D1_prod,
                         D2_prod = D2_prod,
                         F1_minimal = F1_minimal,
                         F2_minimal = F2_minimal
                       ))

cat("\n===============================================\n")
cat("MODEL SPECIFICATIONS (FIXED):\n")
cat("===============================================\n")
for (nm in names(specs_custom)) {
  cat(nm, ":", paste(specs_custom[[nm]], collapse=", "), "\n")
}

# ==============================================================================================================
# Fit MIDAS Models
# ==============================================================================================================
spec_list <- unname(specs_custom)
spec_names <- names(specs_custom)

summary_table <- data.frame(
  model_id = integer(), group = character(), predictors = character(),
  L = integer(), kmax = integer(), AIC = numeric(), BIC = numeric(),
  Ljung_p = numeric(), R2 = numeric(), AdjR2 = numeric(),
  RMSE = numeric(), MAE = numeric(), MASE = numeric(),
  cond_vcov = numeric(), min_eig = numeric(), 
  singular_warn = character(),
  stringsAsFactors = FALSE
)
results <- vector("list", length(spec_list))

for (i in seq_along(spec_list)) {
  xnames <- spec_list[[i]]
  label  <- spec_names[i]
  
  cat("\n==============================\n")
  cat("MIDAS Spec ", i, " [", label, "]: ", paste(xnames, collapse=", "), "\n")
  cat("==============================\n")
  
  best <- try(fit_nealmon_over_L_kmap("y_ng", xnames, L_set, K), silent = TRUE)
  
  if (inherits(best, "try-error")) {
    cat("ERROR: Model fitting failed\n")
    next
  }
  
  invisible(try(print(safe_summary(best$fit)), silent = TRUE))
  
  dg <- diag_report(best$fit, y_ng)
  hv <- holdout_eval(best, y_ng, xnames)
  sv <- safe_vcov_stats(best$fit)
  
  # Flag numerical issues
  singular_flag <- ""
  if (!is.na(sv$cond_vcov) && sv$cond_vcov > 1e15) {
    singular_flag <- "SEVERE"
  } else if (!is.na(sv$cond_vcov) && sv$cond_vcov > 1e10) {
    singular_flag <- "MODERATE"
  } else if (!is.na(sv$min_eig) && sv$min_eig < 1e-6) {
    singular_flag <- "NEAR-SINGULAR"
  } else {
    singular_flag <- "OK"
  }
  
  summary_table <- rbind(summary_table, data.frame(
    model_id = i, group = label, predictors = paste(xnames, collapse=","),
    L = best$L, kmax = best$kmax, AIC = best$AIC, BIC = best$BIC,
    Ljung_p = dg$Ljung_p, R2 = dg$R2, AdjR2 = dg$AdjR2,
    RMSE = hv$rmse, MAE = hv$mae, MASE = hv$mase,
    cond_vcov = sv$cond_vcov, min_eig = sv$min_eig,
    singular_warn = singular_flag
  ))
  
  results[[i]] <- list(best = best, diag = dg, holdout = hv, xnames = xnames)
}

# ==============================================================================================================
# Results Summary
# ==============================================================================================================
summary_table <- summary_table[order(summary_table$AIC, summary_table$MASE), ]

cat("\n===============================================\n")
cat("MIDAS LEADERBOARD (NOMINAL GDP - FIXED)\n")
cat("===============================================\n")
print(summary_table, row.names = FALSE)

# Separate reliable from problematic models
reliable <- subset(summary_table, singular_warn %in% c("OK", "MODERATE"))
problematic <- subset(summary_table, singular_warn %in% c("SEVERE", "NEAR-SINGULAR"))

if (nrow(reliable) > 0) {
  cat("\n===============================================\n")
  cat("RELIABLE MODELS (OK/MODERATE)\n")
  cat("===============================================\n")
  print(reliable[, c("model_id","group","AIC","R2","RMSE","MASE","singular_warn")], row.names = FALSE)
}

if (nrow(problematic) > 0) {
  cat("\n===============================================\n")
  cat("PROBLEMATIC MODELS (DO NOT REPORT)\n")
  cat("===============================================\n")
  print(problematic[, c("model_id","group","AIC","singular_warn","cond_vcov")], row.names = FALSE)
}

best_per_group <- do.call(rbind, lapply(split(summary_table, summary_table$group), function(dd){
  dd[order(dd$AIC, dd$MASE), ][1, , drop = FALSE]
}))

cat("\n===============================================\n")
cat("BEST MODEL PER GROUP\n")
cat("===============================================\n")
print(best_per_group[order(best_per_group$AIC), ], row.names = FALSE)

# Plot MIDAS weights for best reliable model
plot_midas_weights <- function(summary_table, results,
                               file = "midas_weights_ngdp_fixed.png",
                               width = 1400, height = 900, res = 150) {
  reliable <- subset(summary_table, singular_warn %in% c("OK", "MODERATE"))
  if (nrow(reliable) == 0) {
    cat("No reliable models to plot\n")
    return(invisible(NULL))
  }
  
  top_i <- reliable$model_id[which.min(reliable$AIC)]
  top   <- results[[top_i]]
  
  if (is.null(top) || is.null(top$best$fit)) {
    cat("Top model not available for plotting\n")
    return(invisible(NULL))
  }
  
  get2 <- function(obj, base) unname(coef(obj)[paste0(base, 1:2)])
  
  w_block <- function(v) {
    kv <- top$best$kmap[[v]]
    nealmon(get2(top$best$fit, v), kv)
  }
  
  vars <- top$xnames
  if (length(vars) == 0) {
    cat("No predictor blocks found in the best model\n")
    return(invisible(NULL))
  }
  
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
         xlab = "Monthly lag", ylab = "Weight", lwd = 2, col = "steelblue")
    abline(h = 0, lty = 3, col = "gray40")
  }
  
  cat("\nMIDAS weights plot saved to:", file, "\n")
}

plot_midas_weights(summary_table, results)

# Save results
write.csv(summary_table, "midas_ngdp_fixed_leaderboard.csv", row.names = FALSE)
cat("\nResults saved to: midas_ngdp_fixed_leaderboard.csv\n")

cat("\n===============================================\n")
cat("MIDAS ANALYSIS COMPLETE (FIXED VERSION)\n")
cat("===============================================\n")
cat("\nKEY IMPROVEMENTS:\n")
cat("1. Removed problematic oil_price + oil_exports combinations\n")
cat("2. Created lean specifications without weak predictors\n")
cat("3. Added numerical stability flags\n")
cat("4. Separated reliable from problematic models\n")
cat("\nRECOMMENDATION: Use models flagged as 'OK' for reporting.\n")