# B) “No-lag” alternative: do it classically at quarterly frequency
# 
# MIDAS is inherently a distributed-lag setup. If you truly want no lags, the clean way is to aggregate monthly predictors to quarterly (mean/last/sum), 
# and run a standard quarterly OLS (optionally with AR lags of y). Below builds a parallel leaderboard for classical quarterly models using the same specs:

library(dynlm)

# Quarterly aggregator you already defined:
# to_q(x, fun = mean)

# Aggregate monthly growth to quarterly and fit a classical (no-MIDAS) model
# fun = mean (stock-like), sum (flow), or function(x) tail(x, 1) (last-month)
fit_quarterly_spec <- function(xnames, add_ar = TRUE, fun = mean, robust = TRUE) {
  # helper: aggregate one monthly ts → quarterly and align to y_rg span
  to_q <- function(x, fun = mean) {
    x <- window(x, start = hf_start, end = hf_end)      # same monthly window as MIDAS
    q <- stats::aggregate(x, nfrequency = 4, FUN = fun) # quarterly aggregate
    window(q, start = start(y_rg), end = end(y_rg))     # align to y_rg quarters
  }
  
  # 1) aggregate each predictor to Q and align to y_rg
  Xlist <- lapply(xnames, function(v) to_q(get(v), fun = fun))
  Xq    <- do.call(cbind, Xlist)
  xq_names <- paste0(xnames, "_q")
  colnames(Xq) <- xq_names
  
  # 2) build quarterly AR terms of y on the same window
  y  <- window(y_rg,  start = start(y_rg), end = end(y_rg))
  y1 <- stats::lag(y, -1); y2 <- stats::lag(y, -2); y4 <- stats::lag(y, -4)
  
  # 3) clean data.frame (exact names, no ts.union)
  Zdf <- as.data.frame(cbind(y_rg = y, y_l1 = y1, y_l2 = y2, y_l4 = y4, unname(Xq)))
  colnames(Zdf) <- c("y_rg","y_l1","y_l2","y_l4", xq_names)
  Zdf <- stats::na.omit(Zdf)
  
  # 4) sanity check
  missing <- setdiff(xq_names, names(Zdf))
  if (length(missing)) {
    stop("Quarterly columns not present after align/na.omit: ",
         paste(missing, collapse = ", "),
         "\ncolnames(Zdf) = ", paste(names(Zdf), collapse = ", "))
  }
  
  # 5) formula
  form <- if (add_ar) {
    as.formula(paste("y_rg ~ y_l1 + y_l2 + y_l4 +", paste(xq_names, collapse = " + ")))
  } else {
    as.formula(paste("y_rg ~", paste(xq_names, collapse = " + ")))
  }
  
  fit <- lm(form, data = Zdf)
  
  # 6) simple 8-quarter holdout (like MIDAS eval)
  n_test <- min(8, nrow(Zdf) - 20)
  hv <- list(rmse = NA, mae = NA, mase = NA, h = 0)
  if (n_test > 0) {
    tr <- head(Zdf, nrow(Zdf) - n_test)
    te <- tail(Zdf, n_test)
    fit_tr <- lm(form, data = tr)
    pred   <- predict(fit_tr, newdata = te)
    ae     <- abs(te$y_rg - pred)
    hv$rmse <- sqrt(mean((te$y_rg - pred)^2))
    hv$mae  <- mean(ae)
    hv$mase <- hv$mae / mean(abs(diff(tr$y_rg, lag = 4)), na.rm = TRUE)
    hv$h    <- n_test
  }
  
  # 7) standardized betas for predictor blocks (not AR terms)
  sdy <- sd(Zdf$y_rg)
  std_beta <- setNames(numeric(length(xq_names)), xq_names)
  for (p in xq_names) std_beta[p] <- coef(fit)[p] * sd(Zdf[[p]]) / sdy
  
  # 8) coefficient table (with robust p-values by default)
  if (robust) {
    ct <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = "HC1"))
  } else {
    ct <- lmtest::coeftest(fit)
  }
  ct_df <- data.frame(term = rownames(ct),
                      estimate = ct[, "Estimate"],
                      std.error = ct[, "Std. Error"],
                      t.value   = ct[, "t value"],
                      p.value   = ct[, "Pr(>|t|)"],
                      row.names = NULL,
                      check.names = FALSE)
  
  # keep only predictors (drop intercept & AR terms)
  ct_pred <- subset(ct_df, term %in% xq_names)
  ct_pred$var      <- sub("_q$", "", ct_pred$term)
  ct_pred$std_beta <- std_beta[ct_pred$term]
  ct_pred$var_label <- unname(var_labels[ct_pred$var])
  cut_sig <- function(p) cut(p, c(-Inf,.001,.01,.05,.1,Inf), c("***","**","*","."," "), right=TRUE)
  ct_pred$sig <- cut_sig(ct_pred$p.value)
  
  # 9) quick multicollinearity check (VIF) for info
  # (car::vif returns only for numeric regressors, ignores intercept)
  vif_vals <- tryCatch(car::vif(fit), error = function(e) NULL)
  
  # 10) a couple residual diagnostics
  lb <- tryCatch(Box.test(residuals(fit), lag = 8, type = "Ljung-Box")$p.value, error = function(e) NA_real_)
  bp <- tryCatch(lmtest::bptest(fit)$p.value, error = function(e) NA_real_)
  
  list(
    fit = fit,
    aic = AIC(fit), bic = BIC(fit),
    hv = hv, std_beta = std_beta, n = nrow(Zdf),
    coef_df = ct_pred,             # << per-predictor estimates & p-values
    vif = vif_vals,                # << VIF (named vector), may be NULL
    Ljung_p = lb, BP_p = bp        # << residual tests
  )
}



spec_list  <- specs_custom          # keep names on the list
spec_names <- names(spec_list)      # derive names once

# ========== CLASSICAL (quarterly) — RUN ALL SPECS ==========
q_rows   <- list()
coef_rows <- list()
q_models <- vector("list", length(spec_list))

for (i in seq_along(spec_list)) {
  lab <- spec_names[i]; xs <- spec_list[[i]]
  out <- fit_quarterly_spec(xs, add_ar = TRUE, fun = mean, robust = TRUE)  # mean vs sum vs last-month
  
  q_models[[i]] <- out
  
  # model-level leaderboard row
  q_rows[[i]] <- data.frame(
    model_id = i, group = lab, predictors = paste(xs, collapse=","),
    AIC = out$aic, BIC = out$bic,
    R2 = summary(out$fit)$r.squared,
    AdjR2 = summary(out$fit)$adj.r.squared,
    RMSE = out$hv$rmse, MAE = out$hv$mae, MASE = out$hv$mase,
    Ljung_p = out$Ljung_p, BP_p = out$BP_p,
    stringsAsFactors = FALSE
  )
  
  # coefficient-level rows (with p-values & std betas)
  if (!is.null(out$coef_df) && nrow(out$coef_df) > 0) {
    cr <- out$coef_df
    cr$model_id <- i
    cr$group    <- lab
    coef_rows[[i]] <- cr
  }
}

# MODEL-LEVEL leaderboard
summary_table_q <- do.call(rbind, q_rows)
summary_table_q <- summary_table_q[order(summary_table_q$AIC, summary_table_q$MAE), ]

cat("\n==== CLASSICAL (quarterly) LEADERBOARD ====\n")
print(summary_table_q, row.names = FALSE)

# COEFFICIENT-LEVEL table (predictors only), sorted by group then p-value
coef_table_q <- do.call(rbind, coef_rows)
coef_table_q <- coef_table_q[order(coef_table_q$group, coef_table_q$p.value), ]

cat("\n==== CLASSICAL (quarterly) — COEFFICIENTS (robust p-values) ====\n")
print(coef_table_q[, c("model_id","group","var","var_label","estimate","std_beta","p.value","sig")],
      row.names = FALSE)

# If you still want the separate “std_table” you printed earlier:
std_list <- lapply(seq_along(q_models), function(i){
  sb <- q_models[[i]]$std_beta
  if (is.null(sb)) return(NULL)
  data.frame(model_id = i,
             group    = spec_names[i],
             var      = sub("_q$", "", names(sb)),
             std_beta = as.numeric(sb),
             stringsAsFactors = FALSE)
})
std_table <- do.call(rbind, std_list)

# Your dictionary stays simple
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

labelize <- function(vars) {
  key <- as.character(vars)
  key <- trimws(key)
  key <- sub("^Xq\\.", "", key)  # drop cbind/as.data.frame prefix if present
  key <- sub("_q$",   "", key)   # drop quarterly suffix
  out <- unname(var_labels[key])
  # fallback: show the cleaned key when label missing
  out[is.na(out)] <- key[is.na(out)]
  out
}

# Use it everywhere
coef_table_q$var_label <- labelize(coef_table_q$var)
std_table$var_label    <- labelize(std_table$var)

std_table$var_label <- unname(var_labels[std_table$var])

# Merge p-values into your std_table for convenience
std_table <- merge(std_table,
                   coef_table_q[, c("model_id","var","estimate","p.value","sig")],
                   by = c("model_id","var"),
                   all.x = TRUE)

cat("\n==== CLASSICAL (quarterly) — STANDARDIZED BETAS + p-values ====\n")
print(std_table[order(std_table$group, -abs(std_table$std_beta)), ],
      row.names = FALSE)

# Optional: write to disk
write.csv(coef_table_q, "quarterly_coef_table.csv", row.names = FALSE)
write.csv(std_table,    "quarterly_std_betas.csv",  row.names = FALSE)





# Check for hidden characters
cat(coef_table_q$var[1])
cat(names(var_labels)[1])

# Check lengths
nchar(coef_table_q$var[1])
nchar(names(var_labels)[1])
