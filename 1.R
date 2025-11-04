# The overall best model by AIC is C1_oilex
# (oil_gaz_exports_g, noil_omani_exports_g, re_exports_g, tot_mer_imports_g, M1_g, oil_price_g; L=5 → kmax 14)
# with solid diagnostics (Adj-R² ≈ 0.868, Ljung-Box p ≈ 0.63, cond_vcov ≈ 1.0e3 → well-conditioned).

#===========================================================================
# Use the best model to forecast (1–4Q ahead)
#===========================================================================
# pick the AIC winner
top_i <- summary_table$model_id[which.min(summary_table$AIC)]
top   <- results[[top_i]]$best
xvars <- top$x

# build newdata from the *full* monthly series you already have in memory
newdat <- c(setNames(lapply(xvars, get), xvars), list(y_rg = y_rg))

# 1–4 quarter ahead forecasts of QoQ % RGDP
fc_top <- forecast(top$fit, newdata = newdat, h = 4)
print(fc_top)

#===========================================================================
# Convert QoQ % back to levels (from last observed RGDP level)
#===========================================================================
last_lvl <- tail(y_rq, 1)
g <- as.numeric(fc_top$mean)      # QoQ % growth
lvl_path <- numeric(length(g))
lvl <- last_lvl
for(j in seq_along(g)){
  lvl <- lvl * exp(g[j]/100)
  lvl_path[j] <- lvl
}
lvl_path <- ts(lvl_path,
               start = .inc_start(end(y_rq)[1], end(y_rq)[2], 4),
               frequency = 4)
lvl_path

#===========================================================================
# AIC-weighted combo forecast (ensemble of top 3)
#===========================================================================
# This is a robust way to present a single forecast while hedging model risk.
topN <- head(summary_table[order(summary_table$AIC), ], 3)
aic_w <- exp(-0.5*(topN$AIC - min(topN$AIC)))
aic_w <- aic_w / sum(aic_w)

fcs <- lapply(topN$model_id, function(i){
  b  <- results[[i]]$best
  vv <- results[[i]]$xnames
  nd <- c(setNames(lapply(vv, get), vv), list(y_rg = y_rg))
  forecast(b$fit, newdata = nd, h = 4)$mean
})
fcmat  <- do.call(cbind, lapply(fcs, as.numeric))
fc_ens <- ts(as.numeric(fcmat %*% aic_w),
             start = .inc_start(end(y_rg)[1], end(y_rg)[2], 4),
             frequency = 4)
fc_ens
#===========================================================================
# Ragged-edge “nowcast” (partial monthly data for the current quarter)
#===========================================================================

# If you only have months up to, say, 2024-10 for the current quarter, do:
nowcast_quarter <- function(best, xnames, end_month_vec){  # end_month_vec = c(YYYY, MM)
  nd <- setNames(lapply(xnames, function(v) window(get(v), end = end_month_vec)), xnames)
  nd$y_rg <- y_rg
  forecast(best$fit, newdata = nd, h = 1)  # nowcast current quarter
}

# example: nowcast using data through Oct 2024 (adjust as needed)
fc_now <- nowcast_quarter(top, xvars, c(2024, 10))
print(fc_now)

# Tip: the function uses your per-variable kmax map automatically (because the fitted object already “knows” it), 
# so ragged-edge nowcasts respect the shortened blocks for M2_g/oil_price_g.

#===========================================================================
# Export your leaderboard and plot weights
#===========================================================================
# save leaderboard
write.csv(summary_table, "midas_leaderboard.csv", row.names = FALSE)

# plot Nealmon weights of the AIC winner (you already have this, just keeping it here)


