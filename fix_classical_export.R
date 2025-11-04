# ===============================================
# Fixed Classical OLS Results to HTML Converter
# Robust version that handles potential data structure issues
# ===============================================

# Function to convert Classical OLS results to HTML
export_classical_to_html_fixed <- function() {
  
  # Check if results exist in environment
  if (!exists("summary_table_q")) {
    cat("Error: summary_table_q not found. Run Classical analysis first.\n")
    cat("Make sure you've run: source('NGDP_modified_Classical_final.R')\n")
    return(NULL)
  }
  
  cat("Generating Classical OLS HTML report...\n")
  
  # Create HTML content
  html_content <- paste0(
    '<!DOCTYPE html>
    <html>
    <head>
        <title>Classical OLS Results - Nominal GDP Growth</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }
            .container { max-width: 1600px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
            h1 { color: #2c3e50; text-align: center; border-bottom: 3px solid #e74c3c; padding-bottom: 10px; }
            h2 { color: #34495e; margin-top: 30px; border-bottom: 2px solid #ecf0f1; padding-bottom: 5px; }
            h3 { color: #2c3e50; margin-top: 20px; }
            table { width: 100%; border-collapse: collapse; margin: 20px 0; font-size: 12px; }
            th, td { border: 1px solid #ddd; padding: 8px; text-align: center; }
            th { background-color: #e74c3c; color: white; font-weight: bold; }
            tr:nth-child(even) { background-color: #f9f9f9; }
            .best { background-color: #fff3cd !important; font-weight: bold; }
            .summary-box { background: #ecf0f1; padding: 15px; border-radius: 5px; margin: 10px 0; }
            .metric { font-family: monospace; background: #3498db; color: white; padding: 2px 5px; border-radius: 3px; }
            .sig-high { background-color: #27ae60; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; font-weight: bold; }
            .sig-med { background-color: #f39c12; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-low { background-color: #e67e22; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-marginal { background-color: #95a5a6; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-none { color: #7f8c8d; padding: 2px 4px; font-size: 9px; }
            .coef-pos { color: #27ae60; font-weight: bold; }
            .coef-neg { color: #e74c3c; font-weight: bold; }
            .model-group { background-color: #f8f9fa; padding: 2px 6px; border-radius: 4px; font-size: 10px; }
            .coef-table { width: 100%; font-size: 11px; margin: 15px 0; }
            .coef-table th { background-color: #34495e; color: white; }
            .diagnostics { background: #e8f5e8; padding: 12px; border-radius: 5px; margin-top: 15px; font-size: 11px; }
            code { background: #f1f1f1; padding: 1px 4px; border-radius: 3px; font-size: 10px; }
        </style>
    </head>
    <body>
        <div class="container">
            <h1>Classical OLS Analysis Results</h1>
            <h2>Quarterly Regression Models for Nominal GDP Growth</h2>
            
            <div class="summary-box">
                <strong>Analysis Summary:</strong><br>
                • Total Models Tested: <span class="metric">', nrow(summary_table_q), '</span><br>
                • Best AIC Score: <span class="metric">', round(min(summary_table_q$AIC, na.rm = TRUE), 2), '</span><br>
                • Highest R²: <span class="metric">', round(max(summary_table_q$R2, na.rm = TRUE) * 100, 1), '%</span><br>
                • Highest Adj R²: <span class="metric">', round(max(summary_table_q$AdjR2, na.rm = TRUE) * 100, 1), '%</span><br>
                • Robust Standard Errors: <span class="metric">',
                ifelse(exists("use_neweywest") && use_neweywest, "Newey-West HAC", "White HC1"), '</span><br>
                • Analysis Date: <span class="metric">', Sys.Date(), '</span>
            </div>
    ')
  
  # Add model leaderboard table
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
  
  # Sort by AIC for display
  sorted_summary <- summary_table_q[order(summary_table_q$AIC, summary_table_q$MAE), ]
  
  for (i in seq_len(nrow(sorted_summary))) {
    row <- sorted_summary[i, ]
    row_class <- ifelse(i == 1, 'class="best"', '')
    
    html_content <- paste0(html_content, 
      '<tr ', row_class, '>
          <td>', row$model_id, '</td>
          <td><span class="model-group">', row$group, '</span></td>
          <td style="font-size:9px; text-align:left;">', gsub(",", ",<br>", row$predictors), '</td>
          <td>', round(row$AIC, 2), '</td>
          <td>', round(row$BIC, 2), '</td>
          <td>', round(row$R2 * 100, 1), '%</td>
          <td>', round(row$AdjR2 * 100, 1), '%</td>
          <td>', round(row$RMSE, 3), '</td>
          <td>', round(row$MAE, 3), '</td>
          <td>', round(row$MASE, 3), '</td>
          <td>', ifelse(is.na(row$Ljung_p), "N/A", round(row$Ljung_p, 3)), '</td>
          <td>', ifelse(is.na(row$BP_p), "N/A", round(row$BP_p, 3)), '</td>
      </tr>')
  }
  
  html_content <- paste0(html_content, '</table>')
  
  # Coefficient estimates table removed to focus on individual model analysis
  
  # Add individual model sections if available
  if (exists("std_table")) {
    html_content <- paste0(html_content, '
            <h2>Individual Model Analysis</h2>
            <p><em>Detailed coefficient analysis for each model with standardized effects</em></p>')
    
    # Get unique model IDs and process each model separately
    unique_models <- sort(unique(std_table$model_id))
    
    for (model_num in unique_models) {
      model_data <- std_table[std_table$model_id == model_num, ]
      model_data <- model_data[order(-abs(model_data$std_beta)), ]  # Sort by magnitude
      
      # Get model info from summary table
      model_info <- summary_table_q[summary_table_q$model_id == model_num, ]
      if (nrow(model_info) == 0) next
      
      model_info <- model_info[1, ]  # Take first row if multiple
      
      html_content <- paste0(html_content, '
            <div style="margin: 25px 0; border: 2px solid #e74c3c; border-radius: 10px; padding: 20px; background: #fafbfc;">
                <h3 style="margin-top: 0; color: #2c3e50;">📊 Model ', model_num, ' - ', model_info$group, '</h3>
                
                <div style="background: #e8f5e8; padding: 12px; border-radius: 5px; margin: 10px 0;">
                    <strong>Model Performance:</strong><br>
                    • AIC: <code>', sprintf("%.2f", model_info$AIC), '</code> | BIC: <code>', sprintf("%.2f", model_info$BIC), '</code><br>
                    • R²: <code>', sprintf("%.3f", model_info$R2), '</code> | Adj R²: <code>', sprintf("%.3f", model_info$AdjR2), '</code><br>
                    • RMSE: <code>', sprintf("%.3f", model_info$RMSE), '</code> | MAE: <code>', sprintf("%.3f", model_info$MAE), '</code> | MASE: <code>', sprintf("%.3f", model_info$MASE), '</code>
                </div>
                
                <h4>Coefficient Analysis (Ranked by Effect Size):</h4>
                <table style="width: 100%; font-size: 11px; margin: 15px 0; border-collapse: collapse;">
                    <tr style="background-color: #34495e; color: white;">
                        <th style="border: 1px solid #ddd; padding: 8px; text-align:left;">Variable</th>
                        <th style="border: 1px solid #ddd; padding: 8px;">Description</th>
                        <th style="border: 1px solid #ddd; padding: 8px;">Std Beta</th>
                        <th style="border: 1px solid #ddd; padding: 8px;">Raw Coef</th>
                        <th style="border: 1px solid #ddd; padding: 8px;">p-value</th>
                        <th style="border: 1px solid #ddd; padding: 8px;">Sig</th>
                    </tr>')
      
      # Add coefficient rows for this model
      for (j in seq_len(nrow(model_data))) {
        row <- model_data[j, ]
        
        # Color std beta based on magnitude and sign
        beta_class <- ifelse(row$std_beta > 0, "coef-pos", "coef-neg")
        
        # Significance badge  
        sig_class <- switch(as.character(row$sig),
                           "***" = "sig-high",
                           "**" = "sig-high",
                           "*" = "sig-med", 
                           "." = "sig-low",
                           "sig-marginal")
        
        sig_text <- switch(as.character(row$sig),
                          "***" = "p&lt;0.001",
                          "**" = "p&lt;0.01",
                          "*" = "p&lt;0.05",
                          "." = "p&lt;0.1", 
                          "n.s.")
        
        # Row background alternating
        row_bg <- ifelse(j %% 2 == 0, "#f9f9f9", "white")
        
        html_content <- paste0(html_content,
          '<tr style="background-color: ', row_bg, ';">
              <td style="border: 1px solid #ddd; padding: 6px; font-family: monospace; font-size: 10px;">', row$var, '</td>
              <td style="border: 1px solid #ddd; padding: 6px; text-align:left; font-size:10px;">', 
                ifelse(is.na(row$var_label), row$base_var, row$var_label), '</td>
              <td style="border: 1px solid #ddd; padding: 6px; text-align:center;"><span class="', beta_class, '"><strong>', 
                sprintf("%.4f", row$std_beta), '</strong></span></td>
              <td style="border: 1px solid #ddd; padding: 6px; text-align:center;">', sprintf("%.6f", row$estimate), '</td>
              <td style="border: 1px solid #ddd; padding: 6px; text-align:center;">', sprintf("%.6f", row$p.value), '</td>
              <td style="border: 1px solid #ddd; padding: 6px; text-align:center;"><span class="', sig_class, '">', sig_text, '</span></td>
          </tr>')
      }
      
      html_content <- paste0(html_content, '
                </table>
                
                <div style="background: #fff3cd; padding: 8px; border-radius: 4px; margin-top: 10px; font-size: 11px;">
                    <strong>Key Insights:</strong><br>
                    • <strong>Most Important Predictor:</strong> ', model_data$var_label[1], 
                    ' (Std β = ', sprintf("%.3f", model_data$std_beta[1]), ')<br>
                    • <strong>Significant Predictors:</strong> ', 
                    sum(model_data$sig %in% c("***", "**", "*")), ' out of ', nrow(model_data), ' variables<br>
                    • <strong>Model Rank by AIC:</strong> ', 
                    which(summary_table_q$model_id[order(summary_table_q$AIC)] == model_num), 
                    ' out of ', nrow(summary_table_q), ' models
                </div>
            </div>')
    }
  }
  
  # Add model specifications and methodology
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
                    <li><strong>MASE:</strong> Mean Absolute Scaled Error (< 1 beats naive forecast)</li>
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
  
  # Write to file
  output_file <- "NGDP_Classical_Results.html"
  writeLines(html_content, output_file)
  
  cat("✓ Classical OLS HTML report generated successfully!\n")
  cat("File saved as:", output_file, "\n")
  cat("Open this file in your web browser to view the results.\n")
  
  return(output_file)
}

# Run the fixed export function
export_classical_to_html_fixed()