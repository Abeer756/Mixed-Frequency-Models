# ===============================================
# Quick Fix for RGDP Classical (Quarterly) HTML Export
# ===============================================
# Safe exporter that prefers in-memory objects from RGDP_modified_Classical.R
# and falls back to CSVs (coef/std betas) when needed. Leaderboard requires
# in-memory summary_table_q (not currently written to CSV by the script).

export_rgdp_classical_html_fixed <- function() {
  # Helper: try to load in-memory; otherwise CSV fallback where possible
  have_summary <- exists("summary_table_q") && is.data.frame(summary_table_q) && nrow(summary_table_q) > 0
  have_coef    <- exists("coef_table_q")    && is.data.frame(coef_table_q)    && nrow(coef_table_q) > 0
  have_std     <- exists("std_table")       && is.data.frame(std_table)       && nrow(std_table) > 0

  if (!have_coef) {
    if (file.exists("quarterly_coef_table.csv")) {
      coef_table_q <<- tryCatch(read.csv("quarterly_coef_table.csv", stringsAsFactors = FALSE), error = function(e) NULL)
      have_coef <- is.data.frame(coef_table_q) && nrow(coef_table_q) > 0
    }
  }
  if (!have_std) {
    if (file.exists("quarterly_std_betas.csv")) {
      std_table <<- tryCatch(read.csv("quarterly_std_betas.csv", stringsAsFactors = FALSE), error = function(e) NULL)
      have_std <- is.data.frame(std_table) && nrow(std_table) > 0
    }
  }

  cat("Generating RGDP Classical detailed HTML report...\n")

  # Start HTML
  html <- paste0(
    '<!DOCTYPE html>\n',
    '<html>\n',
    '<head>\n',
    '  <meta charset="UTF-8">\n',
    '  <title>Classical Model Results - Real GDP Growth (Detailed)</title>\n',
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
    '    /* Coefficient coloring linked to p-values */\n',
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
    '    <h1>Classical (Quarterly) Model Analysis Results</h1>\n',
    '    <h2>Aggregated Monthly Predictors → Quarterly OLS for Real GDP Growth</h2>\n'
  )

  # Summary cards
  if (have_summary) {
    best_aic <- suppressWarnings(min(summary_table_q$AIC, na.rm = TRUE))
    best_r2  <- suppressWarnings(max(summary_table_q$R2, na.rm = TRUE))
    html <- paste0(html,
      '    <div style="background: #ecf0f1; padding: 15px; border-radius: 5px; margin: 10px 0;">\n',
      '      <strong>Analysis Summary:</strong><br>\n',
      '      • Total Models Tested: <strong>', nrow(summary_table_q), '</strong><br>\n',
      '      • Best AIC Score: <strong>', ifelse(is.finite(best_aic), round(best_aic, 2), 'NA'), '</strong><br>\n',
      '      • Highest R²: <strong>', ifelse(is.finite(best_r2), round(best_r2 * 100, 1), 'NA'), '%</strong><br>\n',
      '      • Analysis Date: <strong>', as.character(Sys.Date()), '</strong>\n',
      '    </div>\n'
    )
  }

  # Leaderboard (in-memory only)
  if (have_summary) {
    html <- paste0(html,
      '    <h2>Classical Leaderboard</h2>\n',
      '    <table>\n',
      '      <tr>\n',
      '        <th>Model</th><th>Group</th><th>Predictors</th>',
      '        <th>AIC</th><th>BIC</th><th>R²</th><th>Adj R²</th><th>RMSE</th><th>MAE</th><th>MASE</th>',
      '        <th>Ljung p</th><th>BP p</th>',
      '      </tr>'
    )

    ord <- order(summary_table_q$AIC, summary_table_q$MAE)
    st  <- summary_table_q[ord, , drop = FALSE]
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
        '<td>', ifelse(is.na(row$RMSE), 'NA', sprintf('%.3f', row$RMSE)), '</td>',
        '<td>', ifelse(is.na(row$MAE),  'NA', sprintf('%.3f', row$MAE)),  '</td>',
        '<td>', ifelse(is.na(row$MASE), 'NA', sprintf('%.3f', row$MASE)), '</td>',
        '<td>', ifelse(is.na(row$Ljung_p), 'NA', sprintf('%.3f', row$Ljung_p)), '</td>',
        '<td>', ifelse(is.na(row$BP_p),    'NA', sprintf('%.3f', row$BP_p)),    '</td>',
        '</tr>'
      )
    }
    html <- paste0(html, '    </table>\n')
  } else {
    html <- paste0(html,
      '    <div style="background:#fff3cd; padding:12px; border-radius:6px;">\n',
      '      Note: Full leaderboard requires running <code>RGDP_modified_Classical.R</code> in this session to populate <code>summary_table_q</code>.\n',
      '      Showing coefficients/standardized betas from CSV fallbacks below.\n',
      '    </div>'
    )
  }

  # Detailed coefficients by model (prefer coef_table_q)
  if (have_coef) {
    html <- paste0(html,
      '    <h2>Coefficient Estimates (Robust p-values)</h2>\n',
      '    <div style="font-size:11px; background:#f6f8fa; padding:10px; border-radius:6px;">\n',
      '      <strong>Color legend (linked to p-values):</strong>\n',
      '      <ul style="margin:6px 0 0 18px;">\n',
      '        <li><span class="coef-sig-pos-strong">dark green</span>/<span class="coef-sig-neg-strong">dark red</span>: p < 0.01</li>\n',
      '        <li><span class="coef-sig-pos">green</span>/<span class="coef-sig-neg">red</span>: 0.01 ≤ p < 0.05</li>\n',
      '        <li><span class="coef-weak-pos">amber</span>/<span class="coef-weak-neg">amber</span>: 0.05 ≤ p < 0.10</li>\n',
      '        <li><span class="coef-ns">grey</span>: p ≥ 0.10 (not significant)</li>\n',
      '      </ul>\n',
      '    </div>\n'
    )

    # Split by (group, model_id)
    ctab <- coef_table_q
    # Ensure expected columns exist
    for (nm in c("model_id","group","var","var_label","estimate","p.value","std_beta")) {
      if (!nm %in% names(ctab)) ctab[[nm]] <- NA
    }

    key <- paste(ctab$group, ctab$model_id, sep = " :: ")
    split_idx <- split(seq_len(nrow(ctab)), key)

    for (nm in names(split_idx)) {
      idx <- split_idx[[nm]]
      df  <- ctab[idx, , drop = FALSE]
      gid <- unique(df$group)[1]
      mid <- unique(df$model_id)[1]
      html <- paste0(html,
        '    <div class="model-box">\n',
        '      <h3>Group: ', gid, ' — Model ', mid, '</h3>\n',
        '      <table class="coef-table">\n',
        '        <tr><th style="text-align:left;">Variable</th><th>Estimate</th><th>Std. Beta</th><th>p-value</th><th>Sig.</th></tr>\n'
      )

      for (i in seq_len(nrow(df))) {
        est <- suppressWarnings(as.numeric(df$estimate[i]))
        sb  <- suppressWarnings(as.numeric(df$std_beta[i]))
        pp  <- suppressWarnings(as.numeric(df$p.value[i]))
        sig <- df$sig[i]
        sig <- ifelse(is.na(pp), "", sig)

        coef_class <- if (is.na(pp) || pp >= 0.10) {
          'coef-ns'
        } else if (pp < 0.01) {
          if (!is.na(est) && est < 0) 'coef-sig-neg-strong' else 'coef-sig-pos-strong'
        } else if (pp < 0.05) {
          if (!is.na(est) && est < 0) 'coef-sig-neg' else 'coef-sig-pos'
        } else {
          if (!is.na(est) && est < 0) 'coef-weak-neg' else 'coef-weak-pos'
        }

        html <- paste0(html,
          '        <tr>',
          '<td style="text-align:left; font-family:monospace; font-size:10px;">',
          ifelse(is.na(df$var_label[i]) || df$var_label[i] == "", df$var[i], df$var_label[i]),
          '</td>',
          '<td><span class="', coef_class, '">', ifelse(is.na(est), "NA", sprintf('%.6f', est)), '</span></td>',
          '<td>', ifelse(is.na(sb),  "NA", sprintf('%.6f', sb)),  '</td>',
          '<td>', ifelse(is.na(pp),  "NA", sprintf('%.6f', pp)),  '</td>',
          '<td>', ifelse(sig == "" || is.na(sig), 'n.s.', sig),  '</td>',
          '</tr>\n'
        )
      }

      html <- paste0(html, '      </table>\n')

      # If in-memory diagnostics are available via q_models
      if (exists("q_models") && length(q_models) >= mid && !is.null(q_models[[mid]])) {
        qm <- q_models[[mid]]
        rse <- tryCatch({summary(qm$fit)$sigma}, error = function(e) NA)
        nobs <- tryCatch({qm$n}, error = function(e) NA)
        ljp <- tryCatch({qm$Ljung_p}, error = function(e) NA)
        bpp <- tryCatch({qm$BP_p}, error = function(e) NA)
        aic <- tryCatch({qm$aic}, error = function(e) NA)
        bic <- tryCatch({qm$bic}, error = function(e) NA)
        html <- paste0(html,
          '      <div class="diagnostics">\n',
          '        <strong>Diagnostics</strong><br>\n',
          '        • Residual Std. Error: <code>', ifelse(is.na(rse), 'NA', sprintf('%.3f', rse)), '</code> | ',
          'Observations: <code>', ifelse(is.na(nobs), 'NA', nobs), '</code><br>\n',
          '        • AIC: <code>', ifelse(is.na(aic), 'NA', sprintf('%.2f', aic)), '</code> | ',
          'BIC: <code>', ifelse(is.na(bic), 'NA', sprintf('%.2f', bic)), '</code><br>\n',
          '        • Ljung-Box p: <code>', ifelse(is.na(ljp), 'NA', sprintf('%.3f', ljp)), '</code> | ',
          'BP p: <code>', ifelse(is.na(bpp), 'NA', sprintf('%.3f', bpp)), '</code>\n',
          '      </div>\n'
        )
      }

      html <- paste0(html, '    </div>\n')
    }
  }

  # Standardized betas summary (optional)
  if (have_std) {
    html <- paste0(html,
      '    <h2>Standardized Betas (by model)</h2>\n',
      '    <table>\n',
      '      <tr><th>Model</th><th>Group</th><th>Variable</th><th>Std. Beta</th></tr>\n'
    )
    st <- std_table[order(std_table$group, -abs(std_table$std_beta)), , drop = FALSE]
    for (i in seq_len(nrow(st))) {
      row <- st[i, , drop = FALSE]
      vlabel <- if (!is.null(row$var_label) && !is.na(row$var_label) && row$var_label != "") row$var_label else row$var
      html <- paste0(html,
        '      <tr>',
        '<td>', row$model_id, '</td>',
        '<td>', row$group, '</td>',
        '<td style="text-align:left;">', vlabel, '</td>',
        '<td>', ifelse(is.na(row$std_beta), 'NA', sprintf('%.6f', row$std_beta)), '</td>',
        '</tr>'
      )
    }
    html <- paste0(html, '    </table>\n')
  }

  # Close & write
  html <- paste0(html,
    '  </div>\n',
    '</body>\n',
    '</html>\n'
  )

  outfile <- "RGDP_Classical_Results_Detailed.html"
  writeLines(html, outfile)
  cat("\u2714 RGDP Classical detailed HTML report generated.\nFile saved as:", outfile, "\n")
  invisible(outfile)
}

# Run once when sourcing
export_rgdp_classical_html_fixed()
