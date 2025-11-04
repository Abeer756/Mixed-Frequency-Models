# =====================================================
# MIDAS (Nealmon) — multiple model sets, auto-selection
# =====================================================
library(midasr)

# ----------------------
# 0) Shared parameters
# ----------------------
K <- 3                 # 3 months per quarter
L_set <- 3:6           # try 9–18 monthly lags (3–6 quarters)

# # Align all monthly series to the exact quarter span of y_g
# qy_start <- start(y_g); qy_end <- end(y_g)
# hf_start <- c(qy_start[1], (qy_start[2]-1)*3 + 1)
# hf_end   <- c(qy_end[1],   qy_end[2]*3)
# align12 <- function(x) if (!is.null(x)) window(x, start = hf_start, end = hf_end) else NULL
# 
# # Try to align every monthly series that might exist
# safe_align <- function(nm){
#   if (exists(nm, inherits = TRUE)) {
#     x <- get(nm, inherits = TRUE)
#     if (!is.null(x)) assign(nm, align12(x), envir = .GlobalEnv)
#   }
# }
# for(nm in c("imp_g","oil_g","nno_g","rex_g","oprice_g","oprod_g","gprice_g","m1_g","m2_g")) safe_align(nm)

# ----------------------
# 1) Utilities
# ----------------------
# Starting values for Nealmon (2 params per block) + AR lags at 1,2,4 for y
nealmon_starts <- function(xnames, yname){
  ans <- setNames(replicate(length(xnames), c(0.1,0.1), simplify = FALSE), xnames)
  ans[[yname]] <- c(0.1,0.1,0.1)  # for mls(y, c(1,2,4), m=1)
  ans
}

# Build a MIDAS formula with chosen regressors and Nealmon weights
build_formula <- function(yname, xnames, kmax){
  form <- as.formula(paste0(yname, " ~ mls(", yname, ", c(1,2,4), m=1)"))
  for (v in xnames) {
    form <- update(form, paste0(". ~ . + fmls(", v, ", ", kmax, ", ", K, ", nealmon)"))
  }
  form
}

# Fit over L_set and pick best (AIC)
fit_nealmon_over_L <- function(yname, xnames, L_set){
  fits <- lapply(L_set, function(L){
    kmax <- K*L - 1
    form <- build_formula(yname, xnames, kmax)
    starts <- nealmon_starts(xnames, yname)
    fit <- midas_r(formula = form, start = starts, OLS = TRUE)
    list(fit = fit, AIC = AIC(fit), BIC = BIC(fit), L = L, kmax = kmax)
  })
  fits[[ which.min(sapply(fits, `[[`, "AIC")) ]]
}

# Simple diagnostics: Ljung-Box + R2/AdjR2
diag_report <- function(fit, y_full){
  r <- residuals(fit); f <- fitted(fit)
  lb <- Box.test(r, lag = 10, type = "Ljung-Box")
  y_use <- tail(y_full, length(f))
  RSS <- sum(r^2); TSS <- sum((y_use - mean(y_use))^2)
  R2  <- 1 - RSS/TSS
  n <- length(r); k <- length(coef(fit))
  adjR2 <- 1 - ((1-R2)*(n-1)/(n-k))
  list(lb_p = lb$p.value, R2 = R2, adjR2 = adjR2)
}

# Holdout evaluation (re-fit on train, forecast last n_test quarters)
holdout_eval <- function(best, y_full, xnames, n_test = min(8, length(y_full) - 20)){
  if (n_test <= 0) return(NULL)
  # 1) train target
  y_tr <- ts(head(y_full, length(y_full) - n_test), start = start(y_full), frequency = 4)
  
  # 2) align monthly predictors to train span
  qy_tr_start <- start(y_tr); qy_tr_end <- end(y_tr)
  hf_start_tr <- c(qy_tr_start[1], (qy_tr_start[2]-1)*3 + 1)
  hf_end_tr   <- c(qy_tr_end[1],   qy_tr_end[2]*3)
  
  # make _tr copies of each regressor trimmed to train horizon
  x_tr_names <- paste0(xnames, "_tr")
  for (i in seq_along(xnames)) {
    xi <- get(xnames[i], inherits = TRUE)
    assign(x_tr_names[i], window(xi, start = hf_start_tr, end = hf_end_tr), envir = .GlobalEnv)
  }
  
  # 3) re-fit on train with same L/kmax
  form_tr <- build_formula("y_tr", x_tr_names, best$kmax)
  starts_tr <- nealmon_starts(x_tr_names, "y_tr")
  fit_tr <- midas_r(form_tr, start = starts_tr, OLS = TRUE)
  
  # 4) forecast and score
  newdat <- c(setNames(lapply(x_tr_names, get, inherits = TRUE), x_tr_names), list(y_tr = y_tr))
  fc <- forecast(fit_tr, newdata = newdat, h = n_test)
  
  y_te <- tail(y_full, n_test)
  ae   <- abs(y_te - fc$mean)
  rmse <- sqrt(mean((y_te - fc$mean)^2, na.rm = TRUE))
  mae  <- mean(ae, na.rm = TRUE)
  mase <- mae / mean(abs(diff(y_tr, lag = 4)), na.rm = TRUE)  # seasonal naive denom
  list(rmse = rmse, mae = mae, mase = mase, h = n_test)
}

# Helper: variable availability
present <- function(v) exists(v, inherits = TRUE) && !is.null(get(v, inherits = TRUE))

# ----------------------
# 2) Model sets (Nealmon)
# ----------------------
# A) Trade + Oil value (your original)
spec_A <- c("imp_g","oil_g","nno_g","rex_g")

# B) Trade + Oil components (price & production); optionally gas price if available
spec_B <- c("imp_g","oprice_g","oprod_g","nno_g","rex_g")
if (present("gprice_g")) spec_B <- c(spec_B, "gprice_g")  # add only if you want it

# C) A + Monetary M1
spec_C <- if (present("m1_g")) c(spec_A, "m1_g") else NULL

# D) A + Monetary M2  (don’t use M1 & M2 together)
spec_D <- if (present("m2_g")) c(spec_A, "m2_g") else NULL

# E) Oil components + M2 (if present)
spec_E <- if (present("m2_g")) c(setdiff(spec_B, "gprice_g"), "m2_g") else NULL

# Keep only the specs that are actually viable (all vars must be present)
keep_if_present <- function(vars) if (!is.null(vars)) vars[sapply(vars, present)] else NULL
specs <- list(
  A = keep_if_present(spec_A),
  B = keep_if_present(spec_B),
  C = keep_if_present(spec_C),
  D = keep_if_present(spec_D),
  E = keep_if_present(spec_E)
)
specs <- specs[ sapply(specs, function(v) !is.null(v) && length(v)>0) ]

# ----------------------
# 3) Fit & compare
# ----------------------
results <- list()
summary_table <- data.frame(
  model = character(), L = integer(), kmax = integer(),
  AIC = numeric(), BIC = numeric(), Ljung_p = numeric(),
  R2 = numeric(), AdjR2 = numeric(),
  RMSE = numeric(), MAE = numeric(), MASE = numeric(),
  stringsAsFactors = FALSE
)

for (nm in names(specs)) {
  xnames <- specs[[nm]]
  cat("\n==============================\n",
      "Model", nm, " (Nealmon): ", paste(xnames, collapse=", "), "\n",
      "==============================\n", sep="")
  
  best <- fit_nealmon_over_L("y_g", xnames, L_set)
  print(summary(best$fit, robust = TRUE))
  dg <- diag_report(best$fit, y_g)
  
  # Holdout (last up to 8 quarters)
  hv <- holdout_eval(best, y_g, xnames)
  if (is.null(hv)) hv <- list(rmse = NA, mae = NA, mase = NA)
  
  summary_table <- rbind(summary_table, data.frame(
    model = nm, L = best$L, kmax = best$kmax,
    AIC = best$AIC, BIC = best$BIC,
    Ljung_p = dg$lb_p, R2 = dg$R2, AdjR2 = dg$adjR2,
    RMSE = hv$rmse, MAE = hv$mae, MASE = hv$mase
  ))
  
  results[[nm]] <- list(best = best, diag = dg, holdout = hv, xnames = xnames)
}

cat("\n\n==== SUMMARY (Nealmon) ====\n")
print(summary_table)

# ----------------------
# 4) (Optional) Weights for a chosen model (e.g., A)
# ----------------------
if ("A" %in% names(results)) {
  bestA <- results$A$best
  get2 <- function(obj, base) unname(coef(obj)[paste0(base, 1:2)])
  w_block <- function(v) nealmon(get2(bestA$fit, v), bestA$kmax)
  
  # Only plot weights for variables actually included
  varsA <- results$A$xnames
  par(mfrow=c( min(2, length(varsA)), ceiling(length(varsA)/2) ))
  for (v in varsA) {
    w <- w_block(v)
    plot(w, type="h", main=paste("Weights:", v), xlab="Monthly lag", ylab="weight")
  }
  par(mfrow=c(1,1))
}
