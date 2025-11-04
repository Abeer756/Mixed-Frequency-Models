# ===============================================
# Quick Fix for RGDP MIDAS HTML Export
# ===============================================
# Safe exporter that does NOT rely on NGDP-only columns (e.g., singular_warn)
# and avoids brittle "$" lookups that can fail on unexpected objects.
# Run AFTER you execute RGDP_modified_Midas.R (so objects are in memory).

export_rgdp_midas_html_fixed <- function() {
  # 0) Guards -----------------------------------------------------------
  if (!exists("summary_table")) {
    cat("Error: summary_table not found. Run RGDP_modified_Midas.R first.\n")
    return(NULL)
  }
  if (!is.data.frame(summary_table) || nrow(summary_table) == 0) {
    cat("Error: summary_table is empty.\n")
    return(NULL)
  }
  # results/best_per_group are optional; we handle gracefully
  has_results <- exists("results") && is.list(results) && length(results) > 0
  has_bestpg  <- exists("best_per_group") && is.data.frame(best_per_group) && nrow(best_per_group) > 0

  cat("Generating RGDP MIDAS detailed HTML report...\n")

  # 1) Header -----------------------------------------------------------
  html <- paste0(
    '<!DOCTYPE html>\n',
    '<html>\n',
    '<head>\n',
    '  <meta charset="UTF-8">\n',
    '  <title>MIDAS Model Results - Real GDP Growth (Detailed)</title>\n',
    '  <style>\n',
    '    body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }\n',
    '    .container { max-width: 1600px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }\n',
    '    h1 { color: #2c3e50; text-align: center; border-bottom: 3px solid #3498db; padding-bottom: 10px; }\n',
    '    h2 { color: #34495e; margin-top: 30px; border-bottom: 2px solid #ecf0f1; padding-bottom: 5px; }\n',
    '    h3 { color: #2c3e50; margin-top: 20px; }\n',
    '    table { width: 100%; border-collapse: collapse; margin: 20px 0; font-size: 12px; }\n',
    '    th, td { border: 1px solid #ddd; padding: 8px; text-align: center; }\n',
    '    th { background-color: #3498db; color: white; font-weight: bold; }\n',
    '    tr:nth-child(even) { background-color: #f9f9f9; }\n',
    '    .best { background-color: #fff3cd !important; font-weight: bold; }\n',
    '    .model-box { margin: 25px 0; border: 2px solid #3498db; border-radius: 10px; padding: 20px; background: #fafbfc; }\n',
  '    .coef-table { width: 100%; font-size: 11px; margin: 15px 0; }\n',
  '    .coef-table th { background-color: #34495e; color: white; }\n',
  '    /* Coefficient coloring now reflects p-value significance (not sign only) */\n',
  '    .coef-sig-pos-strong { color: #1e8449; font-weight: bold; }  /* p < 0.01, positive */\n',
  '    .coef-sig-neg-strong { color: #c0392b; font-weight: bold; }  /* p < 0.01, negative */\n',
  '    .coef-sig-pos { color: #27ae60; font-weight: bold; }         /* 0.01 <= p < 0.05, positive */\n',
  '    .coef-sig-neg { color: #e74c3c; font-weight: bold; }         /* 0.01 <= p < 0.05, negative */\n',
  '    .coef-weak-pos { color: #f39c12; font-weight: bold; }        /* 0.05 <= p < 0.10, positive */\n',
  '    .coef-weak-neg { color: #d35400; font-weight: bold; }        /* 0.05 <= p < 0.10, negative */\n',
  '    .coef-ns { color: #7f8c8d; }                                 /* p >= 0.10 or NA */\n',
  '    .sig-high { background-color: #1e8449; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }\n',
  '    .sig-med { background-color: #27ae60; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }\n',
  '    .sig-low { background-color: #f39c12; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }\n',
  '    .sig-none { color: #7f8c8d; padding: 2px 4px; font-size: 9px; }\n',
    '    .diagnostics { background: #e8f5e8; padding: 12px; border-radius: 5px; margin-top: 15px; font-size: 11px; }\n',
    '    code { background: #f1f1f1; padding: 1px 4px; border-radius: 3px; font-size: 10px; }\n',
    '  </style>\n',
    '</head>\n',
    '<body>\n',
    '  <div class="container">\n',
    '    <h1>MIDAS Model Analysis Results (Detailed)</h1>\n',
    '    <h2>Mixed-Frequency Data Sampling for Real GDP Growth</h2>\n'
  )

  # 2) Summary cards ----------------------------------------------------
  best_aic <- suppressWarnings(min(summary_table$AIC, na.rm = TRUE))
  best_r2  <- suppressWarnings(max(summary_table$R2, na.rm = TRUE))
  html <- paste0(html,
    '    <div style="background: #ecf0f1; padding: 15px; border-radius: 5px; margin: 10px 0;">\n',
    '      <strong>Analysis Summary:</strong><br>\n',
    '      • Total Models Tested: <strong>', nrow(summary_table), '</strong><br>\n',
    '      • Best AIC Score: <strong>', ifelse(is.finite(best_aic), round(best_aic, 2), 'NA'), '</strong><br>\n',
    '      • Highest R²: <strong>', ifelse(is.finite(best_r2), round(best_r2 * 100, 1), 'NA'), '%</strong><br>\n',
    '      • Analysis Date: <strong>', as.character(Sys.Date()), '</strong>\n',
    '    </div>\n'
  )

  # 3) Leaderboard ------------------------------------------------------
  html <- paste0(html,
    '    <h2>MIDAS Model Leaderboard</h2>\n',
    '    <table>\n',
    '      <tr>\n',
    '        <th>Model</th><th>Group</th><th>Predictors</th>',
    '        <th>AIC</th><th>BIC</th><th>R²</th><th>Adj R²</th><th>RMSE</th><th>MAE</th><th>MASE</th>',
    if ("cond_vcov" %in% names(summary_table)) '<th>cond(vcov)</th>' else '',
    if ("min_eig" %in% names(summary_table)) '<th>min eigen</th>' else '',
    '      </tr>'
  )

  ord <- order(summary_table$AIC, summary_table$MASE)
  st  <- summary_table[ord, , drop = FALSE]
  for (i in seq_len(nrow(st))) {
    row <- st[i, , drop = FALSE]
    row_class <- if (i == 1) ' class="best"' else ''
    html <- paste0(html,
      '      <tr', row_class, '>',
      '<td>', row$model_id, '</td>',
      '<td>', row$group, '</td>',
      '<td style="font-size:10px; text-align:left;">', gsub(",", ", ", row$predictors), '</td>',
      '<td>', round(row$AIC, 2), '</td>',
      '<td>', round(row$BIC, 2), '</td>',
      '<td>', round(row$R2 * 100, 1), '%</td>',
      '<td>', round(row$AdjR2 * 100, 1), '%</td>',
      '<td>', round(row$RMSE, 3), '</td>',
      '<td>', round(row$MAE, 3), '</td>',
      '<td>', round(row$MASE, 3), '</td>',
      if ("cond_vcov" %in% names(row)) paste0('<td>', sprintf('%.1f', row$cond_vcov), '</td>') else '',
      if ("min_eig" %in% names(row))   paste0('<td>', sprintf('%.4f', row$min_eig),   '</td>') else '',
      '</tr>'
    )
  }
  html <- paste0(html, '    </table>\n')

  # 4) Best per group (optional) ---------------------------------------
  if (has_bestpg) {
    html <- paste0(html,
      '    <h2>Best Model per Group</h2>\n',
      '    <table>\n',
      '      <tr>\n',
      '        <th>Group</th><th>Model</th><th>AIC</th><th>BIC</th><th>R²</th><th>Adj R²</th><th>RMSE</th><th>MAE</th><th>MASE</th>\n',
      if ("cond_vcov" %in% names(best_per_group)) '<th>cond(vcov)</th>' else '',
      if ("min_eig" %in% names(best_per_group))   '<th>min eigen</th>' else '',
      '      </tr>'
    )
    bpg <- best_per_group[order(best_per_group$AIC), , drop = FALSE]
    for (i in seq_len(nrow(bpg))) {
      row <- bpg[i, , drop = FALSE]
      html <- paste0(html,
        '      <tr>',
        '<td>', row$group, '</td>',
        '<td>', row$model_id, '</td>',
        '<td>', round(row$AIC, 2), '</td>',
        '<td>', round(row$BIC, 2), '</td>',
        '<td>', round(row$R2 * 100, 1), '%</td>',
        '<td>', round(row$AdjR2 * 100, 1), '%</td>',
        '<td>', round(row$RMSE, 3), '</td>',
        '<td>', round(row$MAE, 3), '</td>',
        '<td>', round(row$MASE, 3), '</td>',
        if ("cond_vcov" %in% names(row)) paste0('<td>', sprintf('%.1f', row$cond_vcov), '</td>') else '',
        if ("min_eig" %in% names(row))   paste0('<td>', sprintf('%.4f', row$min_eig),   '</td>') else '',
        '</tr>'
      )
    }
    html <- paste0(html, '    </table>\n')
  }

  # 5) Detailed model sections (optional) -------------------------------
  if (has_results) {
    html <- paste0(html,
      '    <h2>Detailed Model Summaries</h2>\n',
      '    <p><em>Coefficient estimates and diagnostics for each MIDAS model</em></p>\n',
      '    <div style="font-size:11px; background:#f6f8fa; padding:10px; border-radius:6px;">\n',
      '      <strong>Color legend (now linked to p-values):</strong>\n',
      '      <ul style="margin:6px 0 0 18px;">\n',
      '        <li><span class="coef-sig-pos-strong">dark green</span>/<span class="coef-sig-neg-strong">dark red</span>: p < 0.01</li>\n',
      '        <li><span class="coef-sig-pos">green</span>/<span class="coef-sig-neg">red</span>: 0.01 ≤ p < 0.05</li>\n',
      '        <li><span class="coef-weak-pos">amber</span>/<span class="coef-weak-neg">amber</span>: 0.05 ≤ p < 0.10</li>\n',
      '        <li><span class="coef-ns">grey</span>: p ≥ 0.10 (not significant)</li>\n',
      '      </ul>\n',
      '    </div>\n'
    )

    # helper to get safe summary
    .safe_summary <- function(fit) {
      if (exists("safe_summary", inherits = TRUE)) {
        out <- try(safe_summary(fit), silent = TRUE)
        if (!inherits(out, "try-error")) return(out)
      }
      tryCatch(summary(fit), error = function(e) NULL)
    }

    for (i in seq_along(results)) {
      mi <- results[[i]]
      if (is.null(mi) || is.null(mi$best) || is.null(mi$best$fit)) next
      fit <- mi$best$fit
      sm  <- .safe_summary(fit)
      if (is.null(sm)) next

      # match row in summary_table by model_id if present
      row <- tryCatch({
        dd <- summary_table[summary_table$model_id == i, , drop = FALSE]
        if (nrow(dd) == 0) summary_table[1, , drop = FALSE] else dd[1, , drop = FALSE]
      }, error = function(e) summary_table[1, , drop = FALSE])

      spec <- tryCatch(paste(mi$xnames, collapse = ", "), error = function(e) "")

      html <- paste0(html,
        '    <div class="model-box">\n',
        '      <h3>Model ', i, if (!is.null(row$group)) paste0(' - ', row$group) else '', '</h3>\n',
        '      <p><strong>Specification:</strong> ', spec, '</p>\n'
      )

      # coefficient table if available
      coef_mat <- tryCatch(sm$coefficients, error = function(e) NULL)
      if (!is.null(coef_mat)) {
        cf <- as.data.frame(coef_mat)
        cf$Variable <- rownames(coef_mat)
        html <- paste0(html,
          '      <h4>Coefficient Estimates:</h4>\n',
          '      <table class="coef-table">\n',
          '        <tr><th style="text-align:left;">Variable</th><th>Estimate</th><th>Std. Error</th><th>t-value</th><th>p-value</th><th>Sig.</th></tr>\n'
        )
        for (j in seq_len(nrow(cf))) {
          est <- suppressWarnings(as.numeric(cf[j, "Estimate"]))
          se  <- suppressWarnings(as.numeric(cf[j, "Std. Error"]))
          tt  <- suppressWarnings(as.numeric(cf[j, "t value"]))
          pp  <- suppressWarnings(as.numeric(cf[j, "Pr(>|t|)"]))
          sig <- if (is.na(pp)) "" else if (pp < 0.001) "***" else if (pp < 0.01) "**" else if (pp < 0.05) "*" else if (pp < 0.1) "." else ""
          sig_class <- if (sig %in% c("***","**")) "sig-high" else if (sig == "*") "sig-med" else if (sig == ".") "sig-low" else "sig-none"
          # Color by p-value significance; direction only changes shade for significant cases
          coef_class <- {
            if (is.na(pp) || pp >= 0.10) {
              'coef-ns'
            } else if (pp < 0.01) {
              if (!is.na(est) && est < 0) 'coef-sig-neg-strong' else 'coef-sig-pos-strong'
            } else if (pp < 0.05) {
              if (!is.na(est) && est < 0) 'coef-sig-neg' else 'coef-sig-pos'
            } else { # 0.05 <= pp < 0.10
              if (!is.na(est) && est < 0) 'coef-weak-neg' else 'coef-weak-pos'
            }
          }
          html <- paste0(html,
            '        <tr>',
            '<td style="text-align:left; font-family:monospace; font-size:10px;">', cf$Variable[j], '</td>',
            '<td><span class="', coef_class, '">', ifelse(is.na(est), "NA", sprintf('%.6f', est)), '</span></td>',
            '<td>', ifelse(is.na(se),  "NA", sprintf('%.6f', se)),  '</td>',
            '<td>', ifelse(is.na(tt),  "NA", sprintf('%.3f', tt)),  '</td>',
            '<td>', ifelse(is.na(pp),  "NA", sprintf('%.6f', pp)),  '</td>',
            '<td><span class="', sig_class, '">', ifelse(sig == "", "n.s.", sig), '</span></td>',
            '</tr>\n'
          )
        }
        html <- paste0(html, '      </table>\n')
      }

      # diagnostics panel
      rse <- tryCatch(sprintf('%.3f', sm$sigma), error = function(e) 'N/A')
      df2 <- tryCatch(as.character(sm$df[2]), error = function(e) 'N/A')
      html <- paste0(html,
        '      <div class="diagnostics">\n',
        '        <strong>Model Diagnostics:</strong><br>\n',
        '        • Residual Std. Error: <code>', rse, '</code> on <code>', df2, '</code> df<br>\n',
        '        • AIC: <code>', ifelse("AIC" %in% names(row), sprintf('%.2f', row$AIC), 'NA'), '</code> | ',
        'BIC: <code>', ifelse("BIC" %in% names(row), sprintf('%.2f', row$BIC), 'NA'), '</code><br>\n',
        '        • R²: <code>', ifelse("R2" %in% names(row), sprintf('%.3f', row$R2), 'NA'), '</code> | ',
        'RMSE: <code>', ifelse("RMSE" %in% names(row), sprintf('%.3f', row$RMSE), 'NA'), '</code> | ',
        'MASE: <code>', ifelse("MASE" %in% names(row), sprintf('%.3f', row$MASE), 'NA'), '</code>\n',
        '      </div>\n',
        '    </div>\n'
      )
    }
  }

  # 6) Close and write --------------------------------------------------
  html <- paste0(html,
    '  </div>\n',
    '</body>\n',
    '</html>\n'
  )

  outfile <- "RGDP_MIDAS_Results_Detailed.html"
  writeLines(html, outfile)
  cat("\u2714 RGDP detailed HTML report generated.\nFile saved as:", outfile, "\n")
  invisible(outfile)
}

# Run once when sourcing
export_rgdp_midas_html_fixed()
