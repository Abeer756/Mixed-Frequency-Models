# ===============================================
# Simple MIDAS Results to HTML - Direct Conversion
# Run this immediately after your MIDAS analysis
# ===============================================

# Function to convert current results to HTML
export_midas_to_html <- function() {
  
  # Check if results exist in environment
  if (!exists("summary_table")) {
    cat("Error: summary_table not found. Run MIDAS analysis first.\n")
    return(NULL)
  }
  
  # Create HTML content manually
  html_content <- paste0(
    '<!DOCTYPE html>
    <html>
    <head>
        <title>MIDAS Model Results - Nominal GDP Growth</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }
            .container { max-width: 1200px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
            h1 { color: #2c3e50; text-align: center; border-bottom: 3px solid #3498db; padding-bottom: 10px; }
            h2 { color: #34495e; margin-top: 30px; border-bottom: 2px solid #ecf0f1; padding-bottom: 5px; }
            table { width: 100%; border-collapse: collapse; margin: 20px 0; font-size: 12px; }
            th, td { border: 1px solid #ddd; padding: 8px; text-align: center; }
            th { background-color: #3498db; color: white; font-weight: bold; }
            tr:nth-child(even) { background-color: #f9f9f9; }
            .reliable { background-color: #d4edda !important; }
            .problematic { background-color: #f8d7da !important; }
            .best { background-color: #fff3cd !important; font-weight: bold; }
            .summary-box { background: #ecf0f1; padding: 15px; border-radius: 5px; margin: 10px 0; }
            .metric { font-family: monospace; background: #f1c40f; padding: 2px 5px; border-radius: 3px; }
            .status-ok { background-color: #2ecc71; color: white; padding: 2px 6px; border-radius: 10px; font-size: 10px; }
            .status-problem { background-color: #e74c3c; color: white; padding: 2px 6px; border-radius: 10px; font-size: 10px; }
            .sig-high { background-color: #27ae60; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-med { background-color: #f39c12; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-low { background-color: #e74c3c; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-none { background-color: #95a5a6; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .coef-pos { color: #27ae60; font-weight: bold; }
            .coef-neg { color: #e74c3c; font-weight: bold; }
            code { background-color: #f1f1f1; padding: 2px 4px; border-radius: 3px; font-family: monospace; font-size: 10px; }
        </style>
    </head>
    <body>
        <div class="container">
            <h1>MIDAS Model Analysis Results</h1>
            <h2>Mixed-Frequency Data Sampling for Nominal GDP Growth</h2>
            
            <div class="summary-box">
                <strong>Analysis Summary:</strong><br>
                • Total Models Tested: <span class="metric">', nrow(summary_table), '</span><br>
                • Reliable Models: <span class="metric">', sum(summary_table$singular_warn %in% c("OK", "MODERATE")), '</span><br>
                • Best AIC Score: <span class="metric">', round(min(summary_table$AIC, na.rm = TRUE), 2), '</span><br>
                • Highest R²: <span class="metric">', round(max(summary_table$R2, na.rm = TRUE) * 100, 1), '%</span><br>
                • Analysis Date: <span class="metric">', Sys.Date(), '</span>
            </div>
    ')
  
  # Add complete leaderboard table
  html_content <- paste0(html_content, '
            <h2>Complete MIDAS Leaderboard</h2>
            <p><em>Models ranked by AIC. Green = Reliable, Red = Problematic, Yellow = Best Overall</em></p>
            <table>
                <tr>
                    <th>Model ID</th>
                    <th>Group</th>
                    <th>Predictors</th>
                    <th>L</th>
                    <th>AIC</th>
                    <th>BIC</th>
                    <th>R²</th>
                    <th>Adj R²</th>
                    <th>RMSE</th>
                    <th>MAE</th>
                    <th>MASE</th>
                    <th>Status</th>
                </tr>')
  
  # Sort by AIC for display
  sorted_table <- summary_table[order(summary_table$AIC, summary_table$MASE), ]
  
  for (i in 1:nrow(sorted_table)) {
    row <- sorted_table[i, ]
    
    # Determine row class
    row_class <- ""
    if (i == 1) {
      row_class <- 'class="best"'
    } else if (row$singular_warn %in% c("OK", "MODERATE")) {
      row_class <- 'class="reliable"'
    } else {
      row_class <- 'class="problematic"'
    }
    
    # Status badge
    status_class <- ifelse(row$singular_warn %in% c("OK", "MODERATE"), 
                          "status-ok", "status-problem")
    
    html_content <- paste0(html_content, 
      '<tr ', row_class, '>
          <td>', row$model_id, '</td>
          <td>', row$group, '</td>
          <td style="font-size:10px;">', gsub(",", ",<br>", row$predictors), '</td>
          <td>', row$L, '</td>
          <td>', round(row$AIC, 2), '</td>
          <td>', round(row$BIC, 2), '</td>
          <td>', round(row$R2 * 100, 1), '%</td>
          <td>', round(row$AdjR2 * 100, 1), '%</td>
          <td>', round(row$RMSE, 3), '</td>
          <td>', round(row$MAE, 3), '</td>
          <td>', round(row$MASE, 3), '</td>
          <td><span class="', status_class, '">', row$singular_warn, '</span></td>
      </tr>')
  }
  
  html_content <- paste0(html_content, '</table>')
  
  # Add reliable models section
  reliable <- sorted_table[sorted_table$singular_warn %in% c("OK", "MODERATE"), ]
  if (nrow(reliable) > 0) {
    html_content <- paste0(html_content, '
            <h2>Recommended Models (Reliable)</h2>
            <p><em>Models with good numerical stability - recommended for forecasting</em></p>
            <table>
                <tr>
                    <th>Model ID</th>
                    <th>Group</th>
                    <th>AIC</th>
                    <th>R²</th>
                    <th>RMSE</th>
                    <th>MASE</th>
                    <th>Status</th>
                </tr>')
    
    for (i in 1:nrow(reliable)) {
      row <- reliable[i, ]
      row_class <- ifelse(i == 1, 'class="best"', '')
      
      html_content <- paste0(html_content,
        '<tr ', row_class, '>
            <td>', row$model_id, '</td>
            <td>', row$group, '</td>
            <td>', round(row$AIC, 2), '</td>
            <td>', round(row$R2 * 100, 1), '%</td>
            <td>', round(row$RMSE, 3), '</td>
            <td>', round(row$MASE, 3), '</td>
            <td><span class="status-ok">', row$singular_warn, '</span></td>
        </tr>')
    }
    html_content <- paste0(html_content, '</table>')
  }
  
  # Add detailed model summaries
  if (exists("results") && length(results) > 0) {
    html_content <- paste0(html_content, '
            <h2>Detailed MIDAS Model Summaries</h2>
            <p><em>Individual coefficient estimates for each MIDAS specification</em></p>')
    
    # Add each model's detailed results
    for (i in seq_along(results)) {
      if (is.null(results[[i]]) || is.null(results[[i]]$best)) next
      
      model_info <- results[[i]]
      fit <- model_info$best$fit
      group_name <- sorted_table[sorted_table$model_id == i, "group"][1]
      predictors <- paste(model_info$xnames, collapse=", ")
      
      # Get model summary
      model_summary <- try(safe_summary(fit), silent = TRUE)
      if (inherits(model_summary, "try-error")) next
      
      # Extract coefficients
      coef_df <- as.data.frame(model_summary$coefficients)
      coef_df$Variable <- rownames(coef_df)
      
      html_content <- paste0(html_content, '
            <div style="margin: 20px 0; border: 2px solid #3498db; border-radius: 8px; padding: 15px;">
                <h3 style="color: #2c3e50; margin-top: 0;">Model ', i, ' - ', group_name, '</h3>
                <p><strong>Specification:</strong> ', predictors, '</p>
                <p><strong>Formula:</strong> <code style="background:#f1f1f1; padding:2px 4px;">', 
                    format(formula(fit)), '</code></p>
                <p><strong>Sample:</strong> ', format(start(fit$model$y)), ' to ', format(end(fit$model$y)), 
                ' | <strong>Observations:</strong> ', nobs(fit), '</p>
                
                <h4>Coefficient Estimates:</h4>
                <table style="font-size: 11px;">
                    <tr style="background-color: #3498db;">
                        <th>Variable</th>
                        <th>Estimate</th>
                        <th>Std. Error</th>
                        <th>t-value</th>
                        <th>p-value</th>
                        <th>Significance</th>
                    </tr>')
      
      # Add coefficient rows
      for (j in 1:nrow(coef_df)) {
        coef_row <- coef_df[j, ]
        
        # Significance stars
        p_val <- coef_row[["Pr(>|t|)"]]
        if (is.na(p_val)) {
          sig_stars <- ""
          sig_class <- "sig-none"
        } else if (p_val < 0.001) {
          sig_stars <- "***"
          sig_class <- "sig-high"
        } else if (p_val < 0.01) {
          sig_stars <- "**"
          sig_class <- "sig-high"
        } else if (p_val < 0.05) {
          sig_stars <- "*"
          sig_class <- "sig-med"
        } else if (p_val < 0.1) {
          sig_stars <- "."
          sig_class <- "sig-low"
        } else {
          sig_stars <- ""
          sig_class <- "sig-none"
        }
        
        # Color coefficient by sign
        est_val <- coef_row[["Estimate"]]
        coef_class <- ifelse(!is.na(est_val) && est_val > 0, "coef-pos", "coef-neg")
        
        html_content <- paste0(html_content, '
                    <tr>
                        <td style="text-align:left; font-family:monospace; font-size:10px;">', 
                            coef_row$Variable, '</td>
                        <td><span class="', coef_class, '">', 
                            ifelse(is.na(est_val), "NA", sprintf("%.4f", est_val)), '</span></td>
                        <td>', ifelse(is.na(coef_row[["Std. Error"]]), "NA", 
                                    sprintf("%.4f", coef_row[["Std. Error"]])), '</td>
                        <td>', ifelse(is.na(coef_row[["t value"]]), "NA", 
                                    sprintf("%.3f", coef_row[["t value"]])), '</td>
                        <td>', ifelse(is.na(p_val), "NA", sprintf("%.6f", p_val)), '</td>
                        <td><span class="', sig_class, '">', sig_stars, '</span></td>
                    </tr>')
      }
      
      # Add model diagnostics
      rse <- ifelse(is.null(model_summary$sigma), "NA", sprintf("%.3f", model_summary$sigma))
      df_resid <- ifelse(is.null(model_summary$df[2]), "NA", model_summary$df[2])
      
      html_content <- paste0(html_content, '
                </table>
                
                <div style="margin-top: 10px; background-color: #ecf0f1; padding: 8px; border-radius: 4px; font-size: 11px;">
                    <strong>Model Diagnostics:</strong><br>
                    • Residual Standard Error: ', rse, ' on ', df_resid, ' degrees of freedom<br>
                    • AIC: ', sprintf("%.2f", AIC(fit)), ' | BIC: ', sprintf("%.2f", BIC(fit)), '<br>
                    • Signif. codes: 0 "***" 0.001 "**" 0.01 "*" 0.05 "." 0.1 " " 1
                </div>
            </div>')
    }
  }

  # Add best per group if available
  if (exists("best_per_group")) {
    html_content <- paste0(html_content, '
            <h2>Best Model per Group</h2>
            <table>
                <tr>
                    <th>Group</th>
                    <th>AIC</th>
                    <th>BIC</th>
                    <th>R²</th>
                    <th>RMSE</th>
                    <th>MASE</th>
                    <th>Status</th>
                </tr>')
    
    best_sorted <- best_per_group[order(best_per_group$AIC), ]
    for (i in seq_len(nrow(best_sorted))) {
      row <- best_sorted[i, ]
      row_class <- ifelse(i == 1, 'class="best"', '')
      status_class <- ifelse(row$singular_warn %in% c("OK", "MODERATE"), 
                            "status-ok", "status-problem")
      
      html_content <- paste0(html_content,
        '<tr ', row_class, '>
            <td>', row$group, '</td>
            <td>', round(row$AIC, 2), '</td>
            <td>', round(row$BIC, 2), '</td>
            <td>', round(row$R2 * 100, 1), '%</td>
            <td>', round(row$RMSE, 3), '</td>
            <td>', round(row$MASE, 3), '</td>
            <td><span class="', status_class, '">', row$singular_warn, '</span></td>
        </tr>')
    }
    html_content <- paste0(html_content, '</table>')
  }
  
  # Add methodology and conclusions
  html_content <- paste0(html_content, '
            <h2>Model Specifications Tested</h2>
            <ul>
                <li><strong>A_oilex:</strong> Oil & Gas exports, Non-oil Omani exports, Re-exports, Total imports</li>
                <li><strong>B1_oilex:</strong> A_oilex + Narrow Money (M1)</li>
                <li><strong>B2_oilex:</strong> A_oilex + Broad Money (M2)</li>
                <li><strong>D1_oilex:</strong> Oil & Gas exports, Non-oil Omani exports, M1 (lean)</li>
                <li><strong>D2_oilex:</strong> Oil & Gas exports, Non-oil Omani exports, M2 (lean)</li>
                <li><strong>E1_price:</strong> Oil price, Non-oil Omani exports, M1</li>
                <li><strong>E2_price:</strong> Oil price, Non-oil Omani exports, M2</li>
                <li><strong>D1_prod:</strong> Daily oil production, Non-oil Omani exports, M1</li>
                <li><strong>D2_prod:</strong> Daily oil production, Non-oil Omani exports, M2</li>
                <li><strong>F1_minimal:</strong> Oil & Gas exports, M1 (minimal)</li>
                <li><strong>F2_minimal:</strong> Oil price, M1 (minimal)</li>
            </ul>
            
            <h2>Key Findings</h2>
            <div class="summary-box">
                <p><strong>Recommendations:</strong></p>
                <ul>
                    <li>Use only models flagged as <span class="status-ok">OK</span> or <span class="status-ok">MODERATE</span> for forecasting</li>
                    <li>Lean specifications generally outperform complex models</li>
                    <li>Avoid using oil price and oil exports together (multicollinearity)</li>
                    <li>Both M1 and M2 show predictive power for nominal GDP growth</li>
                </ul>
            </div>
            
            <h2>Technical Notes</h2>
            <p><em>MIDAS (Mixed-Frequency Data Sampling) allows incorporation of monthly predictors into quarterly GDP models without temporal aggregation. Models are evaluated using AIC, out-of-sample metrics (RMSE, MAE, MASE), and numerical stability diagnostics.</em></p>
            
        </div>
    </body>
    </html>')
  
  # Write to file
  output_file <- "NGDP_MIDAS_Results.html"
  writeLines(html_content, output_file)
  
  cat("\n===============================================\n")
  cat("HTML REPORT GENERATED SUCCESSFULLY\n")
  cat("===============================================\n")
  cat("File saved as:", output_file, "\n")
  cat("Open this file in your web browser to view the results.\n")
  
  return(output_file)
}

# Run the export function
export_midas_to_html()