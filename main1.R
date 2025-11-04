# ======================
# 4) REVISED MIDAS MODEL WITH LAG 4 SEASONALITY FIX
# ======================
K <- 3                 # 3 months per quarter
L_set <- 3:6           # try using 9–18 monthly lags (3–6 quarters)
poly_order <- 2        # Nealmon has 2 parameters per block


# REVISED function with lag 4 added
fit_midas_L <- function(L){
  kmax <- K*L - 1      # scalar max monthly lag
  form <- y_g ~ mls(y_g, c(1,2,4), m = 1) +  # CHANGED: Added lag 4
    fmls(imp_g, kmax, K, nealmon) +
    fmls(oil_g, kmax, K, nealmon) +
    fmls(nno_g, kmax, K, nealmon) +
    fmls(rex_g, kmax, K, nealmon)
  
  start_list <- list(
    imp_g = rep(0.1, poly_order),
    oil_g = rep(0.1, poly_order),
    nno_g = rep(0.1, poly_order),
    rex_g = rep(0.1, poly_order),
    y_g   = c(0.1, 0.1, 0.1)  # CHANGED: Now 3 values for lags 1,2,4
  )
  
  fit <- midas_r(formula = form, start = start_list, OLS = TRUE)
  list(fit = fit, AIC = AIC(fit), BIC = BIC(fit), L = L, kmax = kmax)
}

fits    <- lapply(L_set, fit_midas_L)
aics    <- sapply(fits, `[[`, "AIC")
best    <- fits[[which.min(aics)]]
best_fit <- best$fit
best_L   <- best$L
kmax     <- best$kmax

cat("METHOD: MIDAS (parametric Nealmon weights) with Seasonal Lag\n")
cat("Chosen L (quarters of monthly history):", best_L, "\n")
print(summary(best_fit, robust = TRUE))

# NEW: Check if seasonality is fixed
ljung_test <- Box.test(residuals(best_fit), lag = 10, type = "Ljung-Box")
cat("\n=== DIAGNOSTIC TESTS ===\n")
cat("Ljung-Box p-value:", ljung_test$p.value, "\n")
cat("Autocorrelation resolved:", ifelse(ljung_test$p.value > 0.05, "YES", "NO"), "\n")

# NEW: Calculate R-squared manually using residuals (simpler and correct)
resid <- residuals(best_fit)
fitted_values <- fitted(best_fit)
# R-squared using residuals directly
RSS <- sum(resid^2)
# For TSS, we need the y values that correspond to fitted values
y_subset <- y_g[(length(y_g) - length(fitted_values) + 1):length(y_g)]
TSS <- sum((y_subset - mean(y_subset))^2)
r_squared <- 1 - (RSS/TSS)
n <- length(resid)
k <- length(coef(best_fit))
adj_r_squared <- 1 - ((1-r_squared)*(n-1)/(n-k))
cat("R-squared:", round(r_squared, 3), "\n")
cat("Adjusted R-squared:", round(adj_r_squared, 3), "\n")
# ======================
# 5) Diagnostics and implied weights (keep as is)
# ======================
par(mfrow=c(1,2))
acf(residuals(best_fit), main="ACF of residuals")
pacf(residuals(best_fit), main="PACF of residuals")
par(mfrow=c(1,1))

# Fitted vs actual
fitted_y <- ts(fitted(best_fit), start = start(y_g), frequency = 4)
ts.plot(y_g, fitted_y, col=c(1,2), lty=c(1,2),
        main="QoQ GDP growth: Actual vs Fitted (MIDAS - Nealmon with Lag 4)")
legend("bottomleft", c("Actual","Fitted"), col=c(1,2), lty=c(1,2), bty="n")

# Fix the length mismatch issue
actual <- y_g
fitted_vals <- fitted(best_fit)

# Align the lengths - fitted values are shorter
n_fitted <- length(fitted_vals)
actual_subset <- tail(actual, n_fitted)

# Now calculate metrics on aligned data
# 1. Directional accuracy
direction_correct <- sum(sign(actual_subset[-1]) == sign(fitted_vals[-1])) / (length(actual_subset)-1)
cat("Directional accuracy:", round(direction_correct*100, 1), "%\n")
## Directional accuracy: 81.1% - Good! Model gets the direction right 4 out of 5 times

# 2. Tracking correlation
tracking_corr <- cor(actual_subset, fitted_vals)
cat("Tracking correlation:", round(tracking_corr, 3), "\n")
## Tracking correlation: 0.876 - Strong positive relationship

# 3. Mean error (bias)
mean_error <- mean(actual_subset - fitted_vals)
cat("Mean error (bias):", round(mean_error, 3), "\n")
## Mean error: 0 - No systematic bias (good)

# 4. Volatility ratio
vol_ratio <- sd(fitted_vals) / sd(actual_subset)
cat("Volatility ratio (fitted/actual):", round(vol_ratio, 3), "\n")
### Volatility ratio: 0.876 - Model underestimates volatility by 12.4%

# 5. Check exactly how many observations are missing
cat("\nNote: Using", n_fitted, "of", length(actual), "observations (lost", length(actual)-n_fitted, "due to lags)\n")



# Implied Nealmon weights per block
get_nealmon_pars <- function(obj, base) unname(coef(obj)[paste0(base, 1:2)])
w_imp <- nealmon(get_nealmon_pars(best_fit, "imp_g"), kmax)
w_oil <- nealmon(get_nealmon_pars(best_fit, "oil_g"), kmax)
w_nno <- nealmon(get_nealmon_pars(best_fit, "nno_g"), kmax)
w_rex <- nealmon(get_nealmon_pars(best_fit, "rex_g"), kmax)

par(mfrow=c(2,2))
plot(w_imp, type="h", main="Weights: Imports (Nealmon)", xlab="Monthly lag", ylab="weight")
plot(w_oil, type="h", main="Weights: Oil exports (Nealmon)", xlab="Monthly lag", ylab="weight")
plot(w_nno, type="h", main="Weights: Non-oil Omani (Nealmon)", xlab="Monthly lag", ylab="weight")
plot(w_rex, type="h", main="Weights: Re-exports (Nealmon)", xlab="Monthly lag", ylab="weight")
par(mfrow=c(1,1))


# The weights reveal the temporal dynamics of how monthly trade data translates to quarterly GDP. 
# For example, if oil exports have sharply declining weights, it means oil revenue affects GDP immediately, 
# while if non-oil exports have dispersed weights, their effects propagate slowly through the economy.


# ======================
# 6) REVISED Holdout evaluation with lag 4
# ======================
n_test <- min(8, length(y_g) - 20)  # keep enough train obs
y_tr   <- ts(head(y_g, length(y_g) - n_test), start = start(y_g), frequency = 4)

# Align monthly predictors to TRAIN span
qy_tr_start <- start(y_tr); qy_tr_end <- end(y_tr)
hf_start_tr <- c(qy_tr_start[1], (qy_tr_start[2]-1)*3 + 1)
hf_end_tr   <- c(qy_tr_end[1],   qy_tr_end[2]*3)

imp_g_tr <- window(imp_g, start = hf_start_tr, end = hf_end_tr)
oil_g_tr <- window(oil_g, start = hf_start_tr, end = hf_end_tr)
nno_g_tr <- window(nno_g, start = hf_start_tr, end = hf_end_tr)
rex_g_tr <- window(rex_g, start = hf_start_tr, end = hf_end_tr)

# REVISED: Refit on train with lag 4 included
form_tr <- y_tr ~ mls(y_tr, c(1,2,4), m = 1) +  # CHANGED: Added lag 4
  fmls(imp_g_tr, kmax, K, nealmon) +
  fmls(oil_g_tr, kmax, K, nealmon) +
  fmls(nno_g_tr, kmax, K, nealmon) +
  fmls(rex_g_tr, kmax, K, nealmon)

start_tr <- list(
  imp_g_tr = c(0.1,0.1),
  oil_g_tr = c(0.1,0.1),
  nno_g_tr = c(0.1,0.1),
  rex_g_tr = c(0.1,0.1),
  y_tr     = c(0.1,0.1,0.1)  # CHANGED: 3 values for lags 1,2,4
)

fit_tr <- midas_r(form_tr, start = start_tr, OLS = TRUE)

# Forecast next n_test quarters
x_list_tr <- list(
  imp_g_tr = imp_g_tr,
  oil_g_tr = oil_g_tr,
  nno_g_tr = nno_g_tr,
  rex_g_tr = rex_g_tr,
  y_tr     = y_tr
)
fc  <- forecast(fit_tr, newdata = x_list_tr, h = n_test)
y_te <- tail(y_g, n_test)

# Accuracy measures
ae   <- abs(y_te - fc$mean)
rmse <- sqrt(mean((y_te - fc$mean)^2, na.rm = TRUE))
mae  <- mean(ae, na.rm = TRUE)
mase <- mae / mean(abs(diff(y_tr, lag = 4)), na.rm = TRUE)

cat(sprintf("\nHoldout — RMSE: %.3f | MAE: %.3f | MASE: %.3f\n", rmse, mae, mase))

# ======================
# 7) ALTERNATIVE MODEL: Reduced with only significant vars
# ======================
cat("\n=== ALTERNATIVE MODEL: Only Oil & Imports with Lag 4 ===\n")

# Fit reduced model
form_reduced <- y_g ~ mls(y_g, c(1,2,4), m = 1) +
  fmls(imp_g, kmax, K, nealmon) +
  fmls(oil_g, kmax, K, nealmon)

start_reduced <- list(
  imp_g = c(0.1, 0.1),
  oil_g = c(0.1, 0.1),
  y_g   = c(0.1, 0.1, 0.1)
)

fit_reduced <- midas_r(form_reduced, start = start_reduced, OLS = TRUE)
print(summary(fit_reduced, robust = TRUE))

# Test reduced model
ljung_reduced <- Box.test(residuals(fit_reduced), lag = 10, type = "Ljung-Box")
cat("Reduced model Ljung-Box p-value:", ljung_reduced$p.value, "\n")

# ======================
# 8) MODEL COMPARISON
# ======================
cat("\n=== MODEL COMPARISON ===\n")

# Compare AICs
models_aic <- c(
  "Full with lag 4" = AIC(best_fit),
  "Reduced (oil+imports)" = AIC(fit_reduced)
)

# Add simple AR benchmark
ar_model <- arima(y_g, order = c(4,0,0))
models_aic["AR(4) benchmark"] = AIC(ar_model)

print(models_aic)
cat("\nBest model by AIC:", names(models_aic)[which.min(models_aic)], "\n")

# Final diagnostic summary
cat("\n=== FINAL DIAGNOSTICS SUMMARY ===\n")
cat("Full model Ljung-Box p-value:", ljung_test$p.value, "\n")
cat("Autocorrelation fixed:", ifelse(ljung_test$p.value > 0.05, "YES ✓", "NO ✗"), "\n")
cat("R-squared:", round(r_squared, 3), "\n")
cat("Out-of-sample MASE:", round(mase, 3), "\n")
cat("Beats naive forecast:", ifelse(mase < 1, "YES ✓", "NO ✗"), "\n")