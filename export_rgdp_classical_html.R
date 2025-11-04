# ===============================================
# Classical OLS Results to HTML Converter (RGDP)
# Matches NGDP_Classical_Results.html style/layout
# ===============================================

export_rgdp_classical_to_html <- function() {
  # Require in-memory objects (like NGDP exporter)
  if (!exists("summary_table_q") || !is.data.frame(summary_table_q) || nrow(summary_table_q) == 0) {
    cat("Error: summary_table_q not found. Run RGDP classical analysis first.\n")
    return(NULL)
  }
  if (!exists("coef_table_q") || !is.data.frame(coef_table_q) || nrow(coef_table_q) == 0) {
    cat("Error: coef_table_q not found. Run RGDP classical analysis first.\n")
    return(NULL)
  }
  if (!exists("std_table") || !is.data.frame(std_table) || nrow(std_table) == 0) {
    cat("Error: std_table not found. Run RGDP classical analysis first.\n")
    return(NULL)
  }

  html_content <- paste0(
    '<!DOCTYPE html>\n',
    '<html>\n',
    '<head>\n',
    '  <title>Classical OLS Results - Real GDP Growth</title>\n',
    '  <style>\n',
    '    body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }\n',
    '    .container { max-width: 1400px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }\n',
    '    h1 { color: #2c3e50; text-align: center; border-bottom: 3px solid #e74c3c; padding-bottom: 10px; }\n',
    '    h2 { color: #34495e; margin-top: 30px; border-bottom: 2px solid #ecf0f1; padding-bottom: 5px; }\n',
    '    table { width: 100%; border-collapse: collapse; margin: 20px 0; font-size: 11px; }\n',
    '    th, td { border: 1px solid #ddd; padding: 6px; text-align: center; }\n',
    '    th { background-color: #e74c3c; color: white; font-weight: bold; }\n',
    '    tr:nth-child(even) { background-color: #f9f9f9; }\n',
    '    .best { background-color: #fff3cd !important; font-weight: bold; }\n',
    '    .summary-box { background: #ecf0f1; padding: 15px; border-radius: 5px; margin: 10px 0; }\n',
    '    .metric { font-family: monospace; background: #3498db; color: white; padding: 2px 5px; border-radius: 3px; }\n',
    '    .sig-high { background-color: #27ae60; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }\n',
    '    .sig-med { background-color: #f39c12; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }\n',
    '    .sig-low { background-color: #e74c3c; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }\n',
    '    .sig-none { background-color: #95a5a6; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }\n',
    '    .coef-pos { color: #27ae60; font-weight: bold; }\n',
    '    .coef-neg { color: #e74c3c; font-weight: bold; }\n',
    '    .model-group { background-color: #f8f9fa; padding: 2px 6px; border-radius: 4px; font-size: 10px; }\n',
    '  </style>\n',
    '</head>\n',
    '<body>\n',
    '  <div class="container">\n',
    '    <h1>Classical OLS Analysis Results</h1>\n',
    '    <h2>Quarterly Regression Models for Real GDP Growth</h2>\n',
    '    <div class="summary-box">\n',
    '      <strong>Analysis Summary:</strong><br>\n',
    '      • Total Models Tested: <span class="metric">', nrow(summary_table_q), '</span><br>\n',
    '      • Best AIC Score: <span class="metric">', round(min(summary_table_q$AIC, na.rm = TRUE), 2), '</span><br>\n',
    '      • Highest R²: <span class="metric">', round(max(summary_table_q$R2, na.rm = TRUE) * 100, 1), '%</span><br>\n',
    '      • Highest Adj R²: <span class="metric">', round(max(summary_table_q$AdjR2, na.rm = TRUE) * 100, 1), '%</span><br>\n',
    '      • Robust Standard Errors: <span class="metric">', ifelse(exists("use_neweywest") && use_neweywest, "Newey-West HAC", "White HC1"), '</span><br>\n',
    '      • Analysis Date: <span class="metric">', Sys.Date(), '</span>\n',
    '    </div>\n'
  )

  # Leaderboard
  html_content <- paste0(html_content, '
    <h2>Model Performance Leaderboard</h2>
    <p><em>Classical OLS models ranked by AIC. Lower AIC indicates better model fit.</em></p>
    <table>
      <tr>
        <th>Model ID</th>
        <th>Group</th>
        <th>Predictors</th>
        <th>AIC</th>
        <th>BIC</th>
        <th>R²</th>
        <th>Adj R²</th>
        <th>RMSE</th>
        <th>MAE</th>
        <th>MASE</th>
        <th>Ljung-Box p</th>
        <th>BP Test p</th>
      </tr>')

  sorted_summary <- summary_table_q[order(summary_table_q$AIC, summary_table_q$MAE), ]
  for (i in seq_len(nrow(sorted_summary))) {
    row <- sorted_summary[i, ]
    row_class <- ifelse(i == 1, 'class="best"', '')
    html_content <- paste0(html_content,
      '<tr ', row_class, '>',
      '<td>', row$model_id, '</td>',
      '<td><span class="model-group">', row$group, '</span></td>',
      '<td style="font-size:9px; text-align:left;">', gsub(",", ",<br>", row$predictors), '</td>',
      '<td>', round(row$AIC, 2), '</td>',
      '<td>', round(row$BIC, 2), '</td>',
      '<td>', round(row$R2 * 100, 1), '%</td>',
      '<td>', round(row$AdjR2 * 100, 1), '%</td>',
      '<td>', round(row$RMSE, 3), '</td>',
      '<td>', round(row$MAE, 3), '</td>',
      '<td>', round(row$MASE, 3), '</td>',
      '<td>', ifelse(is.na(row$Ljung_p), "N/A", round(row$Ljung_p, 3)), '</td>',
      '<td>', ifelse(is.na(row$BP_p),    "N/A", round(row$BP_p, 3)),    '</td>',
      '</tr>')
  }
  html_content <- paste0(html_content, '</table>')

  # Individual Model Analysis (separated sections per model)
  html_content <- paste0(html_content, '
    <h2>Individual Model Analysis</h2>
    <p><em>Detailed coefficient analysis for each model with standardized effects</em></p>')

  unique_models <- sort(unique(std_table$model_id))
  for (model_num in unique_models) {
    model_data <- std_table[std_table$model_id == model_num, , drop = FALSE]
    if (nrow(model_data) == 0) next
    model_data <- model_data[order(-abs(model_data$std_beta)), , drop = FALSE]

    model_info <- summary_table_q[summary_table_q$model_id == model_num, , drop = FALSE]
    if (nrow(model_info) == 0) next
    model_info <- model_info[1, , drop = FALSE]

    html_content <- paste0(html_content,
      '\n      <div style="margin: 25px 0; border: 2px solid #e74c3c; border-radius: 10px; padding: 20px; background: #fafbfc;">',
      '<h3 style="margin-top: 0; color: #2c3e50;">Model ', model_num, ' - ', model_info$group, '</h3>',
      '<div style="background: #e8f5e8; padding: 12px; border-radius: 5px; margin: 10px 0;">',
      '<strong>Model Performance:</strong><br>',
      '• AIC: <code>', sprintf('%.2f', model_info$AIC), '</code> | BIC: <code>', sprintf('%.2f', model_info$BIC), '</code><br>',
      '• R²: <code>', sprintf('%.3f', model_info$R2), '</code> | Adj R²: <code>', sprintf('%.3f', model_info$AdjR2), '</code><br>',
      '• RMSE: <code>', ifelse(is.na(model_info$RMSE), 'NA', sprintf('%.3f', model_info$RMSE)), '</code> | MAE: <code>', ifelse(is.na(model_info$MAE), 'NA', sprintf('%.3f', model_info$MAE)), '</code> | MASE: <code>', ifelse(is.na(model_info$MASE), 'NA', sprintf('%.3f', model_info$MASE)), '</code>',
      '</div>',
      '<h4>Coefficient Analysis (Ranked by Effect Size):</h4>',
      '<table style="width: 100%; font-size: 11px; margin: 15px 0; border-collapse: collapse;">',
      '<tr style="background-color: #34495e; color: white;">',
      '<th style="border: 1px solid #ddd; padding: 8px; text-align:left;">Variable</th>',
      '<th style="border: 1px solid #ddd; padding: 8px;">Description</th>',
      '<th style="border: 1px solid #ddd; padding: 8px;">Std Beta</th>',
      '<th style="border: 1px solid #ddd; padding: 8px;">Raw Coef</th>',
      '<th style="border: 1px solid #ddd; padding: 8px;">p-value</th>',
      '<th style="border: 1px solid #ddd; padding: 8px;">Sig</th>',
      '</tr>')

    for (j in seq_len(nrow(model_data))) {
      row <- model_data[j, , drop = FALSE]
      beta_class <- ifelse(row$std_beta > 0, 'coef-pos', 'coef-neg')
      sig_class <- switch(as.character(row$sig), '***'='sig-high', '**'='sig-high', '*'='sig-med', '.'='sig-low', 'sig-none')
      sig_text  <- switch(as.character(row$sig), '***'='p&lt;0.001', '**'='p&lt;0.01', '*'='p&lt;0.05', '.'='p&lt;0.1', 'n.s.')
      vlabel <- if (!is.null(row$var_label) && !is.na(row$var_label) && nzchar(as.character(row$var_label))) row$var_label else row$var
      row_bg <- ifelse(j %% 2 == 0, '#f9f9f9', 'white')

      html_content <- paste0(html_content,
        '<tr style="background-color: ', row_bg, ';">',
        '<td style="border: 1px solid #ddd; padding: 6px; font-family: monospace; font-size: 10px;">', row$var, '</td>',
        '<td style="border: 1px solid #ddd; padding: 6px; text-align:left; font-size:10px;">', vlabel, '</td>',
        '<td style="border: 1px solid #ddd; padding: 6px; text-align:center;"><span class="', beta_class, '"><strong>', sprintf('%.4f', row$std_beta), '</strong></span></td>',
        '<td style="border: 1px solid #ddd; padding: 6px; text-align:center;">', ifelse(is.na(row$estimate), 'NA', sprintf('%.6f', row$estimate)), '</td>',
        '<td style="border: 1px solid #ddd; padding: 6px; text-align:center;">', ifelse(is.na(row$p.value), 'NA', sprintf('%.6f', row$p.value)), '</td>',
        '<td style="border: 1px solid #ddd; padding: 6px; text-align:center;"><span class="', sig_class, '">', sig_text, '</span></td>',
        '</tr>')
    }

    html_content <- paste0(html_content,
      '</table>',
      '<div style="background: #fff3cd; padding: 8px; border-radius: 4px; margin-top: 10px; font-size: 11px;">',
      '<strong>Key Insights:</strong><br>',
      '• <strong>Most Important Predictor:</strong> ', ifelse(nrow(model_data)>0, as.character(model_data$var_label[1]), '—'),
      ' (Std β = ', ifelse(nrow(model_data)>0 && !is.na(model_data$std_beta[1]), sprintf('%.3f', model_data$std_beta[1]), 'NA'), ')<br>',
      '• <strong>Significant Predictors:</strong> ', sum(model_data$sig %in% c('***','**','*')), ' out of ', nrow(model_data), ' variables<br>',
      '• <strong>Model Rank by AIC:</strong> ', which(summary_table_q$model_id[order(summary_table_q$AIC)] == model_num), ' out of ', nrow(summary_table_q), ' models',
      '</div>',
      '</div>')
  }

  # Coefficient estimates table
  html_content <- paste0(html_content, '
    <h2>Coefficient Estimates with Robust Standard Errors</h2>
    <p><em>All predictor variables with statistical significance indicators</em></p>
    <table>
      <tr>
        <th>Model</th>
        <th>Group</th>
        <th>Variable</th>
        <th>Description</th>
        <th>Coefficient</th>
        <th>Std Beta</th>
        <th>p-value</th>
        <th>Sig</th>
      </tr>')

  sorted_coef <- coef_table_q[order(coef_table_q$group, coef_table_q$p.value), ]
  for (i in seq_len(nrow(sorted_coef))) {
    row <- sorted_coef[i, ]
    coef_class <- ifelse(row$estimate > 0, 'coef-pos', 'coef-neg')
    sig_class <- switch(as.character(row$sig), '***'='sig-high', '**'='sig-high', '*'='sig-med', '.'='sig-low', 'sig-none')
    sig_text  <- switch(as.character(row$sig), '***'='p&lt;0.001', '**'='p&lt;0.01', '*'='p&lt;0.05', '.'='p&lt;0.1', 'n.s.')

    # safe label fallback
    desc <- if ("var_label" %in% names(row) && !is.na(row$var_label) && nzchar(as.character(row$var_label))) {
      row$var_label
    } else if ("base_var" %in% names(row) && !is.na(row$base_var) && nzchar(as.character(row$base_var))) {
      row$base_var
    } else {
      row$var
    }

    html_content <- paste0(html_content,
      '<tr>',
      '<td>', row$model_id, '</td>',
      '<td><span class="model-group">', row$group, '</span></td>',
      '<td style="font-size:10px;">', row$var, '</td>',
      '<td style="text-align:left; font-size:10px;">', desc, '</td>',
      '<td><span class="', coef_class, '">', round(row$estimate, 4), '</span></td>',
      '<td>', round(row$std_beta, 3), '</td>',
      '<td>', round(row$p.value, 4), '</td>',
      '<td><span class="', sig_class, '">', sig_text, '</span></td>',
      '</tr>')
  }
  html_content <- paste0(html_content, '</table>')

  # Standardized betas summary
  html_content <- paste0(html_content, '
    <h2>Standardized Beta Coefficients Summary</h2>
    <p><em>Effect sizes for comparing predictor importance across models</em></p>
    <table>
      <tr>
        <th>Model</th>
        <th>Group</th>
        <th>Variable</th>
        <th>Description</th>
        <th>Std Beta</th>
        <th>Raw Coef</th>
        <th>p-value</th>
        <th>Sig</th>
      </tr>')

  sorted_std <- std_table[order(std_table$group, -abs(std_table$std_beta)), ]
  for (i in seq_len(nrow(sorted_std))) {
    row <- sorted_std[i, ]
    beta_class <- ifelse(row$std_beta > 0, 'coef-pos', 'coef-neg')
    sig_class <- switch(as.character(row$sig), '***'='sig-high', '**'='sig-high', '*'='sig-med', '.'='sig-low', 'sig-none')
    sig_text  <- switch(as.character(row$sig), '***'='p&lt;0.001', '**'='p&lt;0.01', '*'='p&lt;0.05', '.'='p&lt;0.1', 'n.s.')

    desc <- if ("var_label" %in% names(row) && !is.na(row$var_label) && nzchar(as.character(row$var_label))) {
      row$var_label
    } else if ("base_var" %in% names(row) && !is.na(row$base_var) && nzchar(as.character(row$base_var))) {
      row$base_var
    } else {
      row$var
    }

    html_content <- paste0(html_content,
      '<tr>',
      '<td>', row$model_id, '</td>',
      '<td><span class="model-group">', row$group, '</span></td>',
      '<td style="font-size:10px;">', row$var, '</td>',
      '<td style="text-align:left; font-size:10px;">', desc, '</td>',
      '<td><span class="', beta_class, '"><strong>', round(row$std_beta, 3), '</strong></span></td>',
      '<td>', round(row$estimate, 4), '</td>',
      '<td>', round(row$p.value, 4), '</td>',
      '<td><span class="', sig_class, '">', sig_text, '</span></td>',
      '</tr>')
  }
  html_content <- paste0(html_content, '</table>')

  # Specs & Methodology (copy, with RGDP wording)
  html_content <- paste0(html_content, '
    <h2>Model Specifications</h2>
    <p><em>All models include autoregressive terms: y(t-1), y(t-2), y(t-4)</em></p>
    <ul>
      <li><strong>A_oilex:</strong> Oil & Gas exports, Non-oil Omani exports, Re-exports, Total imports</li>
      <li><strong>B1_oilex:</strong> A_oilex + Narrow Money (M1)</li>
      <li><strong>B2_oilex:</strong> A_oilex + Broad Money (M2)</li>
      <li><strong>C1_oilex:</strong> A_oilex + M1 + Oil Price</li>
      <li><strong>C2_oilex:</strong> A_oilex + M2 + Oil Price</li>
      <li><strong>A_prod:</strong> Daily Oil Production, Non-oil Omani exports, Re-exports, Total imports</li>
      <li><strong>B1_prod:</strong> A_prod + Narrow Money (M1)</li>
      <li><strong>B2_prod:</strong> A_prod + Broad Money (M2)</li>
      <li><strong>C1_prod:</strong> A_prod + M1 + Oil Price</li>
      <li><strong>C2_prod:</strong> A_prod + M2 + Oil Price</li>
    </ul>

    <h2>Key Findings & Methodology</h2>
    <div class="summary-box">
      <p><strong>Model Selection Guidelines:</strong></p>
      <ul>
        <li><strong>AIC/BIC:</strong> Lower values indicate better model fit</li>
        <li><strong>R² / Adj R²:</strong> Proportion of variance explained</li>
        <li><strong>RMSE/MAE:</strong> Out-of-sample forecast accuracy</li>
        <li><strong>MASE:</strong> Mean Absolute Scaled Error (&lt; 1 beats naive forecast)</li>
      </ul>
      <p><strong>Technical Details:</strong></p>
      <ul>
        <li>Monthly predictors aggregated to quarterly growth rates</li>
        <li>Quarterly aggregation using mean within each quarter</li>
        <li>All growth rates calculated as 100 × log differences</li>
        <li>Robust standard errors for heteroscedasticity correction</li>
        <li>Out-of-sample evaluation on last 8 quarters</li>
      </ul>
    </div>
  </div>
</body>
</html>')

  output_file <- "RGDP_Classical_Results.html"
  writeLines(html_content, output_file)
  cat("\n===============================================\n")
  cat("CLASSICAL OLS HTML REPORT (RGDP) GENERATED\n")
  cat("===============================================\n")
  cat("File saved as:", output_file, "\n")
  cat("Open this file in your web browser to view the results.\n")
  return(output_file)
}

# Auto-run
export_rgdp_classical_to_html()
