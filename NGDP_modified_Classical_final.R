## ===========================
# Classical Quarterly OLS Regression — NOMINAL GDP
# Complete Standalone Script (fixed)
# ===========================

# --- Packages
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(janitor)
library(sandwich)
library(lmtest)
library(car)

# --- Robust SE options (set once)
use_neweywest <- FALSE   # TRUE = HAC (Newey–West); FALSE = HC1 (White)
nw_lag <- NULL           # optional integer lag for Newey–West; NULL = auto rule inside function

# --- Read data
predictors1 <- read_excel("predictors.xlsx") %>%
  clean_names() %>%
  transmute(
    month = as.Date(zoo::as.yearmon(month_year)),
    tot_mer_imports = total_recorded_merchandise_imports,
    oil_gaz_exports = oil_gaz_exports,
    noil_omani_exports = non_oil_omani_exports,
    re_exports = re_exports,
    noil_exports = non_oil_exports
  ) %>%
  arrange(month)

predictors2 <- read_excel("predictors2.xlsx") %>%
  clean_names() %>%
  transmute(
    month = as.Date(zoo::as.yearmon(month_year)),
    oil_price = average_oil_price,
    d_avg_prod = daily_average_production,
    gas_price = natural_oil_gas,
    M1 = narrow_money_m1,
    M2 = broad_money
  ) %>%
  arrange(month)

predictors <- merge(predictors1, predictors2, by = "month", all.x = TRUE)
predictors <- predictors %>% mutate(across(-month, ~ as.numeric(as.character(.))))

# --- Read NOMINAL GDP data
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

# --- Helpers
.inc_start <- function(y, p, freq){
  p2 <- p + 1
  if (p2 > freq) c(y + 1, 1) else c(y, p2)
}
logdiff_pct <- function(x) 100*diff(log(pmax(as.numeric(x), 1e-9)))

# ======================
# 2) Quarterly NOMINAL GDP target
# ======================
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

# Span alignment
qy_start <- start(y_ng); qy_end <- end(y_ng)
hf_start <- c(qy_start[1], (qy_start[2]-1)*3 + 1)
hf_end   <- c(qy_end[1],   qy_end[2]*3)

# Helper: make monthly growth then window to quarterly span
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

# ======================
# Variable Dictionary (base names without _g/_q suffixes)
# ======================
var_labels <- c(
  "tot_mer_imports"    = "Total Recorded Merchandise Imports",
  "oil_gaz_exports"    = "Oil & Gaz Exports",
  "noil_omani_exports" = "Non-Oil Omani Exports",
  "re_exports"         = "Re-Exports",
  "noil_exports"       = "Total Non-Oil Exports",
  "oil_price"          = "Average oil price",
  "d_avg_prod"         = "Daily Average Oil Production",
  "gas_price"          = "Natural Gas Price",
  "M1"                 = "Narrow Money (M1)",
  "M2"                 = "Broad Money (M2)"
)

# ======================
# 4) Quarterly Aggregation (monthly growth -> quarterly mean) + OLS
# ======================
fit_quarterly_spec <- function(xnames, add_ar = TRUE, fun = mean, robust = TRUE) {
  # Aggregate monthly growth to quarterly (mean within quarter)
  to_q <- function(x, fun = mean) {
    x <- window(x, start = hf_start, end = hf_end)
    q <- stats::aggregate(x, nfrequency = 4, FUN = fun)
    window(q, start = start(y_ng), end = end(y_ng))
  }
  
  Xlist <- lapply(xnames, function(v) to_q(get(v), fun = fun))
  Xq    <- do.call(cbind, Xlist)
  xq_names <- paste0(xnames, "_q")
  colnames(Xq) <- xq_names
  
  # Quarterly target + AR terms
  y  <- window(y_ng, start = start(y_ng), end = end(y_ng))
  y1 <- stats::lag(y, -1); y2 <- stats::lag(y, -2); y4 <- stats::lag(y, -4)
  
  Zdf <- as.data.frame(cbind(y_ng = y, y_l1 = y1, y_l2 = y2, y_l4 = y4, unname(Xq)))
  colnames(Zdf) <- c("y_ng","y_l1","y_l2","y_l4", xq_names)
  Zdf <- stats::na.omit(Zdf)
  
  # Model formula
  form <- if (add_ar) {
    as.formula(paste("y_ng ~ y_l1 + y_l2 + y_l4 +", paste(xq_names, collapse = " + ")))
  } else {
    as.formula(paste("y_ng ~", paste(xq_names, collapse = " + ")))
  }
  
  fit <- lm(form, data = Zdf)
  
  # Holdout evaluation (last 8 quarters)
  n_test <- min(8, nrow(Zdf) - 20)
  hv <- list(rmse = NA_real_, mae = NA_real_, mase = NA_real_, h = 0L)
  if (n_test > 0) {
    tr <- head(Zdf, nrow(Zdf) - n_test)
    te <- tail(Zdf, n_test)
    fit_tr <- lm(form, data = tr)
    pred   <- predict(fit_tr, newdata = te)
    ae     <- abs(te$y_ng - pred)
    hv$rmse <- sqrt(mean((te$y_ng - pred)^2))
    hv$mae  <- mean(ae)
    hv$mase <- hv$mae / mean(abs(diff(tr$y_ng, lag = 4)), na.rm = TRUE)
    hv$h    <- n_test
  }
  
  # Standardized betas (predictor blocks only)
  sdy <- sd(Zdf$y_ng)
  std_beta <- setNames(numeric(length(xq_names)), xq_names)
  for (p in xq_names) std_beta[p] <- coef(fit)[p] * sd(Zdf[[p]]) / sdy
  
  # Robust inference
  if (robust) {
    if (exists("use_neweywest") && isTRUE(use_neweywest)) {
      L <- if (!is.null(nw_lag)) nw_lag else max(1, floor(4 * (nrow(Zdf) / 100)^(2/9)))
      ct <- lmtest::coeftest(
        fit,
        vcov = sandwich::NeweyWest(fit, lag = L, prewhite = FALSE, adjust = TRUE)
      )
    } else {
      ct <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = "HC1"))
    }
  } else {
    ct <- lmtest::coeftest(fit)
  }
  
  ct_df <- data.frame(
    term      = rownames(ct),
    estimate  = ct[, "Estimate"],
    std.error = ct[, "Std. Error"],
    t.value   = ct[, "t value"],
    p.value   = ct[, "Pr(>|t|)"],
    row.names = NULL, check.names = FALSE
  )
  
  # Keep only predictor rows
  ct_pred <- subset(ct_df, term %in% xq_names)
  
  # Names for merging and labeling
  ct_pred$term <- as.character(ct_pred$term)
  ct_pred$var  <- sub("_q$", "", ct_pred$term)          # e.g., "M1_g_q" -> "M1_g"
  ct_pred$base_var <- sub("_g$", "", ct_pred$var)       # e.g., "M1_g"   -> "M1"
  
  ct_pred$std_beta  <- std_beta[ct_pred$term]
  ct_pred$var_label <- unname(var_labels[ct_pred$base_var])
  
  cut_sig <- function(p) cut(p, c(-Inf,.001,.01,.05,.1,Inf),
                             c("***","**","*","."," "), right=TRUE)
  ct_pred$sig <- cut_sig(ct_pred$p.value)
  
  # Diagnostics
  vif_vals <- tryCatch(car::vif(fit), error = function(e) NULL)
  lb <- tryCatch(Box.test(residuals(fit), lag = 8, type = "Ljung-Box")$p.value, error = function(e) NA_real_)
  bp <- tryCatch(lmtest::bptest(fit)$p.value, error = function(e) NA_real_)
  
  list(
    fit = fit,
    aic = AIC(fit), bic = BIC(fit),
    hv = hv, std_beta = std_beta, n = nrow(Zdf),
    coef_df = ct_pred, vif = vif_vals,
    Ljung_p = lb, BP_p = bp
  )
}

# ======================
# 5) Helpers for specs
# ======================
present <- function(v) {
  if (!exists(v, inherits = TRUE)) return(FALSE)
  obj <- get(v, inherits = TRUE)
  is.ts(obj) && length(obj) > 0 && all(is.finite(obj[is.finite(obj)]))
}
keep_if_available <- function(v) { if (all(sapply(v, present))) v else NULL }

# ======================
# 6) Model Specifications (USE GROWTH *_g* series)
# ======================
A_oilex  <- keep_if_available(c("oil_gaz_exports_g","noil_omani_exports_g","re_exports_g","tot_mer_imports_g"))
B1_oilex <- keep_if_available(c(A_oilex, "M1_g"))
B2_oilex <- keep_if_available(c(A_oilex, "M2_g"))
C1_oilex <- keep_if_available(c(A_oilex, "M1_g", "oil_price_g"))
C2_oilex <- keep_if_available(c(A_oilex, "M2_g", "oil_price_g"))

A_prod  <- keep_if_available(c("d_avg_prod_g","noil_omani_exports_g","re_exports_g","tot_mer_imports_g"))
B1_prod <- keep_if_available(c(A_prod, "M1_g"))
B2_prod <- keep_if_available(c(A_prod, "M2_g"))
C1_prod <- keep_if_available(c(A_prod, "M1_g", "oil_price_g"))
C2_prod <- keep_if_available(c(A_prod, "M2_g", "oil_price_g"))

specs_custom <- Filter(Negate(is.null),
                       list(
                         A_oilex = A_oilex,  B1_oilex = B1_oilex,  B2_oilex = B2_oilex,
                         C1_oilex = C1_oilex, C2_oilex = C2_oilex,
                         A_prod = A_prod, B1_prod = B1_prod, B2_prod = B2_prod,
                         C1_prod = C1_prod, C2_prod = C2_prod
                       ))

# ======================
# 7) Display + Fit
# ======================
cat("\n===============================================\n")
cat("CLASSICAL QUARTERLY OLS FOR NOMINAL GDP GROWTH\n")
cat("===============================================\n")
cat("Target (NGDP) span:", paste(start(y_ng), collapse="-"), "to", paste(end(y_ng), collapse="-"), "\n\n")

cat("MODEL SPECIFICATIONS:\n")
cat("===============================================\n")
for (nm in names(specs_custom)) {
  cat(nm, ":", paste(specs_custom[[nm]], collapse=", "), "\n")
}

spec_list  <- specs_custom
spec_names <- names(spec_list)

q_rows     <- list()
coef_rows  <- list()
q_models   <- vector("list", length(spec_list))

for (i in seq_along(spec_list)) {
  lab <- spec_names[i]
  xs  <- spec_list[[i]]
  
  cat("\n==============================\n")
  cat("Classical Spec ", i, " [", lab, "]: ", paste(xs, collapse=", "), "\n")
  cat("==============================\n")
  
  out <- fit_quarterly_spec(xs, add_ar = TRUE, fun = mean, robust = TRUE)
  
  q_models[[i]] <- out
  
  cat("\nModel Summary:\n")
  print(summary(out$fit))
  
  if (!is.null(out$vif)) {
    cat("\nVariance Inflation Factors (VIF):\n")
    print(round(out$vif, 2))
  }
  
  q_rows[[i]] <- data.frame(
    model_id = i,
    group = lab,
    predictors = paste(xs, collapse=","),
    AIC = out$aic,
    BIC = out$bic,
    R2 = summary(out$fit)$r.squared,
    AdjR2 = summary(out$fit)$adj.r.squared,
    RMSE = out$hv$rmse,
    MAE = out$hv$mae,
    MASE = out$hv$mase,
    Ljung_p = out$Ljung_p,
    BP_p = out$BP_p,
    stringsAsFactors = FALSE
  )
  
  if (!is.null(out$coef_df) && nrow(out$coef_df) > 0) {
    cr <- out$coef_df
    cr$model_id <- i
    cr$group    <- lab
    coef_rows[[i]] <- cr
  }
}

# ======================
# 8) Results Summary Tables
# ======================
summary_table_q <- do.call(rbind, q_rows)
summary_table_q <- summary_table_q[order(summary_table_q$AIC, summary_table_q$MAE), ]

cat("\n===============================================\n")
cat("CLASSICAL QUARTERLY LEADERBOARD (NOMINAL GDP)\n")
cat("===============================================\n")
print(summary_table_q, row.names = FALSE)

coef_table_q <- do.call(rbind, coef_rows)
coef_table_q <- coef_table_q[order(coef_table_q$group, coef_table_q$p.value), ]

cat("\n===============================================\n")
cat("COEFFICIENTS WITH ROBUST P-VALUES\n")
cat("===============================================\n")
print(coef_table_q[, c("model_id","group","var","base_var","var_label","estimate","std_beta","p.value","sig")],
      row.names = FALSE)

# --- STANDARDIZED BETAS TABLE
std_list <- lapply(seq_along(q_models), function(i){
  sb <- q_models[[i]]$std_beta
  if (is.null(sb)) return(NULL)
  data.frame(
    model_id = i,
    group    = spec_names[i],
    var      = sub("_q$", "", names(sb)),   # "M1_g_q" -> "M1_g" (matches coef_table_q$var)
    std_beta = as.numeric(sb),
    stringsAsFactors = FALSE
  )
})
std_table <- do.call(rbind, std_list)

# Add labels and p-values (merge by the shared 'var', not the base name)
std_table <- merge(
  std_table,
  coef_table_q[, c("model_id","var","estimate","p.value","sig")],
  by = c("model_id","var"),
  all.x = TRUE
)

# Human-friendly labels from base variable
std_table$base_var  <- sub("_g$", "", std_table$var)
std_table$var_label <- unname(var_labels[std_table$base_var])

cat("\n===============================================\n")
cat("STANDARDIZED BETAS WITH P-VALUES\n")
cat("===============================================\n")
print(std_table[order(std_table$group, -abs(std_table$std_beta)),
                c("model_id","group","var","base_var","var_label","std_beta","estimate","p.value","sig")],
      row.names = FALSE)

# ======================
# 9) Export Results to CSV
# ======================
write.csv(summary_table_q, "classical_ngdp_leaderboard.csv", row.names = FALSE)
write.csv(coef_table_q,   "classical_ngdp_coef_table.csv",   row.names = FALSE)
write.csv(std_table,      "classical_ngdp_std_betas.csv",    row.names = FALSE)

cat("\n===============================================\n")
cat("FILES SAVED:\n")
cat("===============================================\n")
cat("1. classical_ngdp_leaderboard.csv\n")
cat("2. classical_ngdp_coef_table.csv\n")
cat("3. classical_ngdp_std_betas.csv\n")

cat("\n===============================================\n")
cat("CLASSICAL QUARTERLY ANALYSIS COMPLETE\n")
cat("===============================================\n")
