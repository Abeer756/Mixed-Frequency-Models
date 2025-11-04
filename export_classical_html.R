# ===============================================
# Classical OLS Results to HTML Converter
# Run this immediately after your Classical analysis
# ===============================================

# Function to convert Classical OLS results to HTML
export_classical_to_html <- function() {
  
  # Check if results exist in environment
  if (!exists("summary_table_q")) {
    cat("Error: summary_table_q not found. Run Classical analysis first.\n")
    return(NULL)
  }
  
  if (!exists("coef_table_q")) {
    cat("Error: coef_table_q not found. Run Classical analysis first.\n") 
    return(NULL)
  }
  
  if (!exists("std_table")) {
    cat("Error: std_table not found. Run Classical analysis first.\n")
    return(NULL)
  }
  
  # Create HTML content
  html_content <- paste0(
    '<!DOCTYPE html>
    <html>
    <head>
        <title>Classical OLS Results - Nominal GDP Growth</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }
            .container { max-width: 1400px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
            h1 { color: #2c3e50; text-align: center; border-bottom: 3px solid #e74c3c; padding-bottom: 10px; }
            h2 { color: #34495e; margin-top: 30px; border-bottom: 2px solid #ecf0f1; padding-bottom: 5px; }
            table { width: 100%; border-collapse: collapse; margin: 20px 0; font-size: 11px; }
            th, td { border: 1px solid #ddd; padding: 6px; text-align: center; }
            th { background-color: #e74c3c; color: white; font-weight: bold; }
            tr:nth-child(even) { background-color: #f9f9f9; }
            .best { background-color: #fff3cd !important; font-weight: bold; }
            .summary-box { background: #ecf0f1; padding: 15px; border-radius: 5px; margin: 10px 0; }
            .metric { font-family: monospace; background: #3498db; color: white; padding: 2px 5px; border-radius: 3px; }
            .sig-high { background-color: #27ae60; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-med { background-color: #f39c12; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-low { background-color: #e74c3c; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-none { background-color: #95a5a6; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .coef-pos { color: #27ae60; font-weight: bold; }
            .coef-neg { color: #e74c3c; font-weight: bold; }
            .model-group { background-color: #f8f9fa; padding: 2px 6px; border-radius: 4px; font-size: 10px; }
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
          <td>', round(row$Ljung_p, 3), '</td>
          <td>', round(row$BP_p, 3), '</td>
      </tr>')
  }
  
  html_content <- paste0(html_content, '</table>')
  
  # Add coefficient estimates table
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
  
  # Sort coefficients by group and significance
  sorted_coef <- coef_table_q[order(coef_table_q$group, coef_table_q$p.value), ]
  
  for (i in seq_len(nrow(sorted_coef))) {
    row <- sorted_coef[i, ]
    
    # Color coefficient based on sign
    coef_class <- ifelse(row$estimate > 0, "coef-pos", "coef-neg")
    
    # Significance badge
    sig_class <- switch(as.character(row$sig),
                       "***" = "sig-high",
                       "**" = "sig-high", 
                       "*" = "sig-med",
                       "." = "sig-low",
                       "sig-none")
    
    sig_text <- switch(as.character(row$sig),
                      "***" = "p&lt;0.001",
                      "**" = "p&lt;0.01",
                      "*" = "p&lt;0.05", 
                      "." = "p&lt;0.1",
                      "n.s.")
    
    html_content <- paste0(html_content,
      '<tr>
          <td>', row$model_id, '</td>
          <td><span class="model-group">', row$group, '</span></td>
          <td style="font-size:10px;">', row$var, '</td>
          <td style="text-align:left; font-size:10px;">', 
            ifelse(is.na(row$var_label), row$base_var, row$var_label), '</td>
          <td><span class="', coef_class, '">', round(row$estimate, 4), '</span></td>
          <td>', round(row$std_beta, 3), '</td>
          <td>', round(row$p.value, 4), '</td>
          <td><span class="', sig_class, '">', sig_text, '</span></td>
      </tr>')
  }
  
  html_content <- paste0(html_content, '</table>')
  
  # Add standardized betas summary
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
  
  # Sort std betas by group and magnitude
  sorted_std <- std_table[order(std_table$group, -abs(std_table$std_beta)), ]
  
  for (i in seq_len(nrow(sorted_std))) {
    row <- sorted_std[i, ]
    
    # Color std beta based on magnitude and sign
    beta_class <- ifelse(row$std_beta > 0, "coef-pos", "coef-neg")
    
    # Significance badge  
    sig_class <- switch(as.character(row$sig),
                       "***" = "sig-high",
                       "**" = "sig-high",
                       "*" = "sig-med", 
                       "." = "sig-low",
                       "sig-none")
    
    sig_text <- switch(as.character(row$sig),
                      "***" = "p&lt;0.001",
                      "**" = "p&lt;0.01",
                      "*" = "p&lt;0.05",
                      "." = "p&lt;0.1", 
                      "n.s.")
    
    html_content <- paste0(html_content,
      '<tr>
          <td>', row$model_id, '</td>
          <td><span class="model-group">', row$group, '</span></td>
          <td style="font-size:10px;">', row$var, '</td>
          <td style="text-align:left; font-size:10px;">', 
            ifelse(is.na(row$var_label), row$base_var, row$var_label), '</td>
          <td><span class="', beta_class, '"><strong>', round(row$std_beta, 3), '</strong></span></td>
          <td>', round(row$estimate, 4), '</td>
          <td>', round(row$p.value, 4), '</td>
          <td><span class="', sig_class, '">', sig_text, '</span></td>
      </tr>')
  }
  
  html_content <- paste0(html_content, '</table>')
  
  # Add methodology and model specifications
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
            
            <h2>Methodology Notes</h2>
            <div class="summary-box">
                <p><strong>Data Transformation:</strong></p>
                <ul>
                    <li>Monthly predictors transformed to quarterly growth rates</li>
                    <li>Quarterly aggregation using mean within each quarter</li>
                    <li>All growth rates calculated as 100 × log differences</li>
                    <li>Models include AR(1), AR(2), and AR(4) terms for GDP growth</li>
                </ul>
                
                <p><strong>Statistical Inference:</strong></p>
                <ul>
                    <li>Robust standard errors (White HC1 or Newey-West HAC)</li>
                    <li>Out-of-sample evaluation on last 8 quarters</li>
                    <li>Diagnostic tests: Ljung-Box (residual autocorrelation), Breusch-Pagan (heteroscedasticity)</li>
                    <li>VIF computed for multicollinearity assessment</li>
                </ul>
            </div>
            
            <h2>Key Findings & Interpretation</h2>
            <div class="summary-box">
                <p><strong>Model Selection Guidelines:</strong></p>
                <ul>
                    <li><strong>AIC/BIC:</strong> Lower values indicate better model fit</li>
                    <li><strong>R² / Adj R²:</strong> Proportion of variance explained</li>
                    <li><strong>RMSE/MAE:</strong> Out-of-sample forecast accuracy</li>
                    <li><strong>MASE:</strong> Mean Absolute Scaled Error (< 1 beats naive forecast)</li>
                </ul>
                
                <p><strong>Coefficient Interpretation:</strong></p>
                <ul>
                    <li><strong>Standardized Beta:</strong> Effect size in standard deviation units</li>
                    <li><strong>Significance Levels:</strong> *** p<0.001, ** p<0.01, * p<0.05, . p<0.1</li>
                    <li><strong>Green coefficients:</strong> Positive relationship with GDP growth</li>
                    <li><strong>Red coefficients:</strong> Negative relationship with GDP growth</li>
                </ul>
            </div>
            
        </div>
    </body>
    </html>')
  
  # Write to file
  output_file <- "NGDP_Classical_Results.html"
  writeLines(html_content, output_file)
  
  cat("\n===============================================\n")
  cat("CLASSICAL OLS HTML REPORT GENERATED\n")
  cat("===============================================\n")
  cat("File saved as:", output_file, "\n")
  cat("Open this file in your web browser to view the results.\n")
  
  return(output_file)
}

# Run the export function
export_classical_to_html()