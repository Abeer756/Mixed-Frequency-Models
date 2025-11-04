# ===========================
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

# ======================
# 4) MIDAS(Nealmon) model selection over L = 3:6
# ======================
K <- 3                 # 3 months per quarter
L_set <- 3:6           # try using 9–18 monthly lags (3–6 quarters)
poly_order <- 2        # Nealmon has 2 parameters per block

fit_midas_L <- function(L){
  kmax <- K*L - 1      # scalar max monthly lag
  form <- y_g ~ mls(y_g, 1:2, m = 1) +
    fmls(imp_g, kmax, K, nealmon) +
    fmls(oil_g, kmax, K, nealmon) +
    fmls(nno_g, kmax, K, nealmon) +
    fmls(rex_g, kmax, K, nealmon)
  
  start_list <- list(
    imp_g = rep(0.1, poly_order),
    oil_g = rep(0.1, poly_order),
    nno_g = rep(0.1, poly_order),
    rex_g = rep(0.1, poly_order),
    y_g   = c(0.1, 0.1)
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

cat("METHOD: MIDAS (parametric Nealmon weights)\n")
cat("Chosen L (quarters of monthly history):", best_L, "\n")
print(summary(best_fit, robust = TRUE))
summary(best_fit)


# Maybe 2014 oil crash or 2020 COVID changed relationships
library(strucchange)
breakpoints(best_fit)

# ======================
# 5) Diagnostics and implied weights
# ======================

# Calculate R-squared manually
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


# ACF = Auto-Correlation Function (correlation with past values)
# PACF = Partial Auto-Correlation Function (direct correlation, removing indirect effects)

par(mfrow=c(1,2))
acf(residuals(best_fit), main="ACF of residuals")
pacf(residuals(best_fit), main="PACF of residuals")
par(mfrow=c(1,1))

# ACF: Bars at lags 1 and 4 exceed the blue lines
# PACF: Bar at lag 4 exceeds the blue line
## This indicates autocorrelation at lag 4 (yearly pattern)


Box.test(residuals(best_fit), lag = 10, type = "Ljung-Box")
## p-value = 0.01467
## p < 0.05 means REJECT the null hypothesis of no autocorrelation
## residuals are NOT white noise - there's still pattern left that the model didn't capture!

# Fitted vs actual
fitted_y <- ts(fitted(best_fit), start = start(y_g), frequency = 4)
ts.plot(y_g, fitted_y, col=c(1,2), lty=c(1,2),
        main="QoQ GDP growth: Actual vs Fitted (MIDAS - Nealmon)")
legend("bottomleft", c("Actual","Fitted"), col=c(1,2), lty=c(1,2), bty="n")

# Implied Nealmon weights per block (pass SCALAR kmax!)
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



## GDP Growth Volatility
# GDP growth volatility
sd(y_g)  # Standard deviation of GDP growth
range(y_g)  # Min and max values
quantile(y_g, c(0.05, 0.95))  # 90% range
# ======================
# 6) Holdout evaluation (last up to 8 quarters)
# ======================
n_test <- min(8, length(y_g) - 24)  # keep enough train obs
y_tr   <- ts(head(y_g, length(y_g) - n_test), start = start(y_g), frequency = 4)

# Align monthly predictors to TRAIN span
qy_tr_start <- start(y_tr); qy_tr_end <- end(y_tr)
hf_start_tr <- c(qy_tr_start[1], (qy_tr_start[2]-1)*3 + 1)
hf_end_tr   <- c(qy_tr_end[1],   qy_tr_end[2]*3)

imp_g_tr <- window(imp_g, start = hf_start_tr, end = hf_end_tr)
oil_g_tr <- window(oil_g, start = hf_start_tr, end = hf_end_tr)
nno_g_tr <- window(nno_g, start = hf_start_tr, end = hf_end_tr)
rex_g_tr <- window(rex_g, start = hf_start_tr, end = hf_end_tr)

# Refit on train with same L/kmax
form_tr <- y_tr ~ mls(y_tr, 1:2, m = 1) +
  fmls(imp_g_tr, kmax, K, nealmon) +
  fmls(oil_g_tr, kmax, K, nealmon) +
  fmls(nno_g_tr, kmax, K, nealmon) +
  fmls(rex_g_tr, kmax, K, nealmon)

start_tr <- list(
  imp_g_tr = c(0.1,0.1),
  oil_g_tr = c(0.1,0.1),
  nno_g_tr = c(0.1,0.1),
  rex_g_tr = c(0.1,0.1),
  y_tr     = c(0.1,0.1)
)

fit_tr <- midas_r(form_tr, start = start_tr, OLS = TRUE)

# Forecast next n_test quarters (newdata must include ALL RHS vars)
x_list_tr <- list(
  imp_g_tr = imp_g_tr,
  oil_g_tr = oil_g_tr,
  nno_g_tr = nno_g_tr,
  rex_g_tr = rex_g_tr,
  y_tr     = y_tr
)
fc  <- forecast(fit_tr, newdata = x_list_tr, h = n_test)
y_te <- tail(y_g, n_test)

# Accuracy (avoid MAPE for near-zero growth)
ae   <- abs(y_te - fc$mean)
rmse <- sqrt(mean((y_te - fc$mean)^2, na.rm = TRUE))
mae  <- mean(ae, na.rm = TRUE)
mase <- mae / mean(abs(diff(y_tr, lag = 4)), na.rm = TRUE)  # seasonal naive denom

cat(sprintf("Holdout — RMSE: %.3f | MAE: %.3f | MASE: %.3f\n", rmse, mae, mase))

# ======================
# 7) Optional: stable alternative (Almon weights) — full sample
# ======================
# Uncomment to fit an Almon-weight MIDAS as a robustness check.
# form_almon <- y_g ~ mls(y_g, 1:2, m = 1) +
#   fmls(imp_g,  kmax, K, almonp) +
#   fmls(oil_g,  kmax, K, almonp) +
#   fmls(nno_g,  kmax, K, almonp) +
#   fmls(rex_g,  kmax, K, almonp)
# start_almon <- list(
#   imp_g = c(0.1,0.1,0.1),
#   oil_g = c(0.1,0.1,0.1),
#   nno_g = c(0.1,0.1,0.1),
#   rex_g = c(0.1,0.1,0.1),
#   y_g   = c(0.1,0.1)
# )
# fit_almon <- midas_r(form_almon, start = start_almon, OLS = TRUE)
# cat("\nAlmon MIDAS (full sample):\n")
# print(summary(fit_almon, robust = TRUE))
