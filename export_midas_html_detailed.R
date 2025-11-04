# ===============================================
# Enhanced MIDAS Results to HTML Converter
# Includes detailed model summaries from R console output
# ===============================================

# Function to safely get a summary of MIDAS fit
get_midas_summary <- function(fit) {
  if (is.null(fit)) return(NULL)
  tryCatch({
    if (exists("safe_summary", envir = .GlobalEnv)) {
      return(safe_summary(fit))
    } else {
      return(summary(fit))
    }
  }, error = function(e) {
    return(summary(fit))
  })
}

# Enhanced export function with detailed model information
export_midas_to_html_detailed <- function() {
  
  # Check if results exist in environment
  if (!exists("summary_table")) {
    cat("Error: summary_table not found. Run MIDAS analysis first.\n")
    return(NULL)
  }
  
  # Create HTML content with enhanced CSS
  html_content <- paste0(
    '<!DOCTYPE html>
    <html>
    <head>
        <title>MIDAS Model Results - Nominal GDP Growth (Detailed)</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }
            .container { max-width: 1600px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
            h1 { color: #2c3e50; text-align: center; border-bottom: 3px solid #3498db; padding-bottom: 10px; }
            h2 { color: #34495e; margin-top: 30px; border-bottom: 2px solid #ecf0f1; padding-bottom: 5px; }
            h3 { color: #2c3e50; margin-top: 20px; }
            h4 { color: #34495e; margin-bottom: 10px; }
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
            .sig-high { background-color: #27ae60; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; font-weight: bold; }
            .sig-med { background-color: #f39c12; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-low { background-color: #e67e22; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-marginal { background-color: #95a5a6; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-none { color: #7f8c8d; padding: 2px 4px; font-size: 9px; }
            .coef-pos { color: #27ae60; font-weight: bold; }
            .coef-neg { color: #e74c3c; font-weight: bold; }
            .model-box { margin: 25px 0; border: 2px solid #3498db; border-radius: 10px; padding: 20px; background: #fafbfc; }
            .model-box h3 { margin-top: 0; color: #2c3e50; }
            .formula-box { background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; font-family: monospace; font-size: 11px; }
            .coef-table { width: 100%; font-size: 11px; margin: 15px 0; }
            .coef-table th { background-color: #34495e; }
            .diagnostics { background: #e8f5e8; padding: 12px; border-radius: 5px; margin-top: 15px; font-size: 11px; }
            .var-name { font-family: monospace; font-size: 10px; text-align: left; }
            .num-val { font-family: monospace; }
            code { background: #f1f1f1; padding: 1px 4px; border-radius: 3px; font-size: 10px; }
        </style>
    </head>
    <body>
        <div class="container">
            <h1>MIDAS Model Analysis Results (Detailed)</h1>
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
  
  for (i in seq_len(nrow(sorted_table))) {
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
          <td style="font-size:10px; text-align:left;">', gsub(",", ",<br>", row$predictors), '</td>
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
  
  # Add detailed model summaries if results object exists
  if (exists("results") && length(results) > 0) {
    html_content <- paste0(html_content, '
            <h2>Detailed MIDAS Model Summaries</h2>
            <p><em>Complete coefficient estimates and diagnostics for each MIDAS specification</em></p>')
    
    # Process each model
    for (i in seq_along(results)) {
      if (is.null(results[[i]]) || is.null(results[[i]]$best)) next
      
      model_info <- results[[i]]
      fit <- model_info$best$fit
      
      # Get model details from summary table
      model_row <- summary_table[summary_table$model_id == i, ]
      if (nrow(model_row) == 0) next
      
      group_name <- model_row$group[1]
      predictors <- paste(model_info$xnames, collapse=", ")
      
      # Get model summary
      model_summary <- get_midas_summary(fit)
      if (is.null(model_summary)) next
      
      # Extract coefficients
      coef_matrix <- model_summary$coefficients
      if (is.null(coef_matrix)) next
      
      coef_df <- as.data.frame(coef_matrix)
      coef_df$Variable <- rownames(coef_df)
      
      # Start model box
      html_content <- paste0(html_content, '
            <div class="model-box">
                <h3>Model ', i, ' - ', group_name, '</h3>
                <p><strong>Specification:</strong> ', predictors, '</p>')
      
      # Add formula if available
      if (!is.null(fit$terms)) {
        formula_text <- deparse(formula(fit$terms), width.cutoff = 80)
        html_content <- paste0(html_content, '
                <div class="formula-box">
                    <strong>Formula:</strong><br>
                    ', paste(formula_text, collapse=" "), '
                </div>')
      }
      
      # Sample information - handle different MIDAS model structures
      start_date <- "N/A"
      end_date <- "N/A" 
      n_obs <- "N/A"
      
      # Try different ways to get sample information
      tryCatch({
        if (!is.null(fit$model)) {
          # For MIDAS models, try different structures
          y_data <- NULL
          if (is.list(fit$model) && "y" %in% names(fit$model)) {
            y_data <- fit$model$y
          } else if (is.ts(fit$model)) {
            y_data <- fit$model
          } else if (!is.null(fit$fitted.values)) {
            y_data <- fit$fitted.values
          }
          
          if (!is.null(y_data) && is.ts(y_data)) {
            start_info <- start(y_data)
            end_info <- end(y_data)
            start_date <- paste0(start_info[1], "(", start_info[2], ")")
            end_date <- paste0(end_info[1], "(", end_info[2], ")")
            n_obs <- length(y_data)
          } else if (!is.null(y_data)) {
            n_obs <- length(y_data)
          }
        }
        
        # Try to get from model summary if still not found
        if (n_obs == "N/A" && !is.null(model_summary$df)) {
          n_obs <- sum(model_summary$df)
        }
      }, error = function(e) {
        # Silently handle any errors in sample info extraction
      })
      
      html_content <- paste0(html_content, '
                <p><strong>Sample Period:</strong> ', start_date, ' to ', end_date, 
                ' | <strong>Observations:</strong> ', n_obs, '</p>
                
                <h4>Coefficient Estimates:</h4>
                <table class="coef-table">
                    <tr>
                        <th style="text-align:left;">Variable</th>
                        <th>Estimate</th>
                        <th>Std. Error</th>
                        <th>t-value</th>
                        <th>p-value</th>
                        <th>Sig.</th>
                    </tr>')
      
      # Process coefficient rows
      for (j in seq_len(nrow(coef_df))) {
        coef_row <- coef_df[j, ]
        var_name <- coef_row$Variable
        
        # Extract values safely
        estimate <- ifelse("Estimate" %in% names(coef_row), coef_row$Estimate, NA)
        std_err <- ifelse("Std. Error" %in% names(coef_row), coef_row[["Std. Error"]], NA)
        t_val <- ifelse("t value" %in% names(coef_row), coef_row[["t value"]], NA)
        p_val <- ifelse("Pr(>|t|)" %in% names(coef_row), coef_row[["Pr(>|t|)"]], NA)
        
        # Significance classification
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
          sig_class <- "sig-marginal"
        }
        
        # Color coefficient by sign
        coef_class <- ifelse(!is.na(estimate) && estimate > 0, "coef-pos", "coef-neg")
        
        # Format values
        est_fmt <- ifelse(is.na(estimate), "NA", sprintf("%.4f", estimate))
        se_fmt <- ifelse(is.na(std_err), "NA", sprintf("%.4f", std_err))
        t_fmt <- ifelse(is.na(t_val), "NA", sprintf("%.3f", t_val))
        p_fmt <- ifelse(is.na(p_val), "NA", sprintf("%.6f", p_val))
        
        html_content <- paste0(html_content, '
                    <tr>
                        <td class="var-name">', var_name, '</td>
                        <td class="num-val"><span class="', coef_class, '">', est_fmt, '</span></td>
                        <td class="num-val">', se_fmt, '</td>
                        <td class="num-val">', t_fmt, '</td>
                        <td class="num-val">', p_fmt, '</td>
                        <td><span class="', sig_class, '">', sig_stars, '</span></td>
                    </tr>')
      }
      
      # Close coefficient table and add diagnostics
      rse <- "N/A"
      df_resid <- "N/A"
      
      # Safely extract diagnostic information
      tryCatch({
        if (!is.null(model_summary$sigma)) {
          rse <- sprintf("%.3f", model_summary$sigma)
        }
        if (!is.null(model_summary$df) && length(model_summary$df) >= 2) {
          df_resid <- model_summary$df[2]
        }
      }, error = function(e) {
        # Use fallback values
      })
      
      # Safely format model metrics
      aic_val <- tryCatch(sprintf("%.2f", model_row$AIC), error = function(e) "N/A")
      bic_val <- tryCatch(sprintf("%.2f", model_row$BIC), error = function(e) "N/A")
      r2_val <- tryCatch(sprintf("%.3f", model_row$R2), error = function(e) "N/A")
      adjr2_val <- tryCatch(sprintf("%.3f", model_row$AdjR2), error = function(e) "N/A")
      rmse_val <- tryCatch(sprintf("%.3f", model_row$RMSE), error = function(e) "N/A")
      mase_val <- tryCatch(sprintf("%.3f", model_row$MASE), error = function(e) "N/A")
      
      status_class <- ifelse(model_row$singular_warn %in% c("OK", "MODERATE"), "status-ok", "status-problem")
      
      html_content <- paste0(html_content, '
                </table>
                
                <div class="diagnostics">
                    <strong>Model Diagnostics:</strong><br>
                    • Residual Standard Error: <code>', rse, '</code> on <code>', df_resid, '</code> degrees of freedom<br>
                    • AIC: <code>', aic_val, '</code> | BIC: <code>', bic_val, '</code><br>
                    • R²: <code>', r2_val, '</code> | Adj R²: <code>', adjr2_val, '</code><br>
                    • RMSE: <code>', rmse_val, '</code> | MASE: <code>', mase_val, '</code><br>
                    • Status: <span class="', status_class, '">', model_row$singular_warn, '</span><br>
                    • Signif. codes: 0 "***" 0.001 "**" 0.01 "*" 0.05 "." 0.1 " " 1
                </div>
            </div>')
    }
  }
  
  # Add methodology section
  html_content <- paste0(html_content, '
            <h2>Model Specifications</h2>
            <ul>
                <li><strong>A_oilex:</strong> Oil & Gas exports, Non-oil Omani exports, Re-exports, Total imports</li>
                <li><strong>B1_oilex:</strong> A_oilex + Narrow Money (M1)</li>
                <li><strong>B2_oilex:</strong> A_oilex + Broad Money (M2)</li>
                <li><strong>D1_oilex:</strong> Oil & Gas exports, Non-oil Omani exports, M1 (lean specification)</li>
                <li><strong>D2_oilex:</strong> Oil & Gas exports, Non-oil Omani exports, M2 (lean specification)</li>
                <li><strong>E1_price:</strong> Oil price, Non-oil Omani exports, M1</li>
                <li><strong>E2_price:</strong> Oil price, Non-oil Omani exports, M2</li>
                <li><strong>D1_prod:</strong> Daily oil production, Non-oil Omani exports, M1</li>
                <li><strong>D2_prod:</strong> Daily oil production, Non-oil Omani exports, M2</li>
                <li><strong>F1_minimal:</strong> Oil & Gas exports, M1 (minimal specification)</li>
                <li><strong>F2_minimal:</strong> Oil price, M1 (minimal specification)</li>
            </ul>
            
            <h2>Key Findings & Recommendations</h2>
            <div class="summary-box">
                <p><strong>Model Selection:</strong></p>
                <ul>
                    <li>Use only models flagged as <span class="status-ok">OK</span> or <span class="status-ok">MODERATE</span> for forecasting</li>
                    <li>Lean specifications (D1/D2, E1/E2, F1/F2) generally outperform complex models</li>
                    <li>Avoid oil price and oil export combinations (multicollinearity issues)</li>
                    <li>Both M1 and M2 show predictive power for nominal GDP growth</li>
                </ul>
            </div>
            
        </div>
    </body>
    </html>')
  
  # Write to file
  output_file <- "NGDP_MIDAS_Results_Detailed.html"
  writeLines(html_content, output_file)
  
  cat("\n===============================================\n")
  cat("DETAILED MIDAS HTML REPORT GENERATED\n")
  cat("===============================================\n")
  cat("File saved as:", output_file, "\n")
  cat("This report includes complete coefficient tables\n")
  cat("matching the R console output you provided.\n")
  cat("Open this file in your web browser to view the results.\n")
  
  return(output_file)
}

# Run the enhanced export function
export_midas_to_html_detailed()