# ===============================================
# Quick Fix for MIDAS HTML Export Error
# ===============================================

# This script fixes the "$" operator error by providing
# a more robust version of the export function

export_midas_to_html_detailed_fixed <- function() {
  
  # Check if results exist in environment
  if (!exists("summary_table")) {
    cat("Error: summary_table not found. Run MIDAS analysis first.\n")
    return(NULL)
  }
  
  cat("Generating enhanced MIDAS HTML report...\n")
  
  # Create HTML content with basic information first
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
            table { width: 100%; border-collapse: collapse; margin: 20px 0; font-size: 12px; }
            th, td { border: 1px solid #ddd; padding: 8px; text-align: center; }
            th { background-color: #3498db; color: white; font-weight: bold; }
            tr:nth-child(even) { background-color: #f9f9f9; }
            .best { background-color: #fff3cd !important; font-weight: bold; }
            .reliable { background-color: #d4edda !important; }
            .problematic { background-color: #f8d7da !important; }
            .model-box { margin: 25px 0; border: 2px solid #3498db; border-radius: 10px; padding: 20px; background: #fafbfc; }
            .coef-table { width: 100%; font-size: 11px; margin: 15px 0; }
            .coef-table th { background-color: #34495e; color: white; }
            .coef-pos { color: #27ae60; font-weight: bold; }
            .coef-neg { color: #e74c3c; font-weight: bold; }
            .sig-high { background-color: #27ae60; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-med { background-color: #f39c12; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-low { background-color: #e67e22; color: white; padding: 2px 4px; border-radius: 10px; font-size: 9px; }
            .sig-none { color: #7f8c8d; padding: 2px 4px; font-size: 9px; }
            .status-ok { background-color: #2ecc71; color: white; padding: 2px 6px; border-radius: 10px; font-size: 10px; }
            .status-problem { background-color: #e74c3c; color: white; padding: 2px 6px; border-radius: 10px; font-size: 10px; }
            .diagnostics { background: #e8f5e8; padding: 12px; border-radius: 5px; margin-top: 15px; font-size: 11px; }
            code { background: #f1f1f1; padding: 1px 4px; border-radius: 3px; font-size: 10px; }
        </style>
    </head>
    <body>
        <div class="container">
            <h1>MIDAS Model Analysis Results (Detailed)</h1>
            <h2>Mixed-Frequency Data Sampling for Nominal GDP Growth</h2>
            
            <div style="background: #ecf0f1; padding: 15px; border-radius: 5px; margin: 10px 0;">
                <strong>Analysis Summary:</strong><br>
                • Total Models Tested: <strong>', nrow(summary_table), '</strong><br>
                • Reliable Models: <strong>', sum(summary_table$singular_warn %in% c("OK", "MODERATE")), '</strong><br>
                • Best AIC Score: <strong>', round(min(summary_table$AIC, na.rm = TRUE), 2), '</strong><br>
                • Highest R²: <strong>', round(max(summary_table$R2, na.rm = TRUE) * 100, 1), '%</strong><br>
                • Analysis Date: <strong>', Sys.Date(), '</strong>
            </div>
    ')
  
  # Add leaderboard table
  html_content <- paste0(html_content, '
            <h2>MIDAS Model Leaderboard</h2>
            <table>
                <tr>
                    <th>Model</th>
                    <th>Group</th>
                    <th>Predictors</th>
                    <th>AIC</th>
                    <th>R²</th>
                    <th>RMSE</th>
                    <th>MASE</th>
                    <th>Status</th>
                </tr>')
  
  # Sort and display summary table
  sorted_table <- summary_table[order(summary_table$AIC), ]
  
  for (i in seq_len(nrow(sorted_table))) {
    row <- sorted_table[i, ]
    row_class <- ""
    if (i == 1) row_class <- 'class="best"'
    else if (row$singular_warn %in% c("OK", "MODERATE")) row_class <- 'class="reliable"'
    else row_class <- 'class="problematic"'
    
    status_class <- ifelse(row$singular_warn %in% c("OK", "MODERATE"), "status-ok", "status-problem")
    
    html_content <- paste0(html_content, 
      '<tr ', row_class, '>
          <td>', row$model_id, '</td>
          <td>', row$group, '</td>
          <td style="font-size:10px; text-align:left;">', gsub(",", ", ", row$predictors), '</td>
          <td>', round(row$AIC, 2), '</td>
          <td>', round(row$R2 * 100, 1), '%</td>
          <td>', round(row$RMSE, 3), '</td>
          <td>', round(row$MASE, 3), '</td>
          <td><span class="', status_class, '">', row$singular_warn, '</span></td>
      </tr>')
  }
  
  html_content <- paste0(html_content, '</table>')
  
  # Add detailed model summaries if results exist
  if (exists("results") && length(results) > 0) {
    html_content <- paste0(html_content, '
            <h2>Detailed Model Summaries</h2>
            <p><em>Coefficient estimates and diagnostics for each MIDAS model</em></p>')
    
    for (i in seq_along(results)) {
      if (is.null(results[[i]])) next
      
      model_info <- results[[i]]
      if (is.null(model_info$best) || is.null(model_info$best$fit)) next
      
      fit <- model_info$best$fit
      model_row <- summary_table[summary_table$model_id == i, ]
      if (nrow(model_row) == 0) next
      
      group_name <- model_row$group[1]
      predictors <- paste(model_info$xnames, collapse=", ")
      
      # Get model summary safely
      model_summary <- tryCatch({
        if (exists("safe_summary", envir = .GlobalEnv)) {
          safe_summary(fit)
        } else {
          summary(fit)
        }
      }, error = function(e) NULL)
      
      if (is.null(model_summary)) next
      
      # Start model box
      html_content <- paste0(html_content, '
            <div class="model-box">
                <h3>Model ', i, ' - ', group_name, '</h3>
                <p><strong>Specification:</strong> ', predictors, '</p>')
      
      # Try to get coefficient table
      coef_matrix <- tryCatch(model_summary$coefficients, error = function(e) NULL)
      
      if (!is.null(coef_matrix)) {
        html_content <- paste0(html_content, '
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
        
        # Process coefficients
        coef_df <- as.data.frame(coef_matrix)
        coef_df$Variable <- rownames(coef_df)
        
        for (j in seq_len(nrow(coef_df))) {
          coef_row <- coef_df[j, ]
          
          # Extract values safely
          estimate <- tryCatch(coef_row[["Estimate"]], error = function(e) NA)
          std_err <- tryCatch(coef_row[["Std. Error"]], error = function(e) NA)
          t_val <- tryCatch(coef_row[["t value"]], error = function(e) NA)
          p_val <- tryCatch(coef_row[["Pr(>|t|)"]], error = function(e) NA)
          
          # Significance
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
          
          # Color coefficient
          coef_class <- ifelse(!is.na(estimate) && estimate > 0, "coef-pos", "coef-neg")
          
          # Format safely
          est_fmt <- ifelse(is.na(estimate), "NA", sprintf("%.4f", estimate))
          se_fmt <- ifelse(is.na(std_err), "NA", sprintf("%.4f", std_err))
          t_fmt <- ifelse(is.na(t_val), "NA", sprintf("%.3f", t_val))
          p_fmt <- ifelse(is.na(p_val), "NA", sprintf("%.6f", p_val))
          
          html_content <- paste0(html_content, '
                    <tr>
                        <td style="text-align:left; font-family:monospace; font-size:10px;">', coef_row$Variable, '</td>
                        <td><span class="', coef_class, '">', est_fmt, '</span></td>
                        <td>', se_fmt, '</td>
                        <td>', t_fmt, '</td>
                        <td>', p_fmt, '</td>
                        <td><span class="', sig_class, '">', sig_stars, '</span></td>
                    </tr>')
        }
        
        html_content <- paste0(html_content, '</table>')
      }
      
      # Add diagnostics
      rse <- tryCatch(sprintf("%.3f", model_summary$sigma), error = function(e) "N/A")
      df_resid <- tryCatch(model_summary$df[2], error = function(e) "N/A")
      
      html_content <- paste0(html_content, '
                <div class="diagnostics">
                    <strong>Model Diagnostics:</strong><br>
                    • Residual Standard Error: <code>', rse, '</code> on <code>', df_resid, '</code> degrees of freedom<br>
                    • AIC: <code>', sprintf("%.2f", model_row$AIC), '</code> | BIC: <code>', sprintf("%.2f", model_row$BIC), '</code><br>
                    • R²: <code>', sprintf("%.3f", model_row$R2), '</code> | RMSE: <code>', sprintf("%.3f", model_row$RMSE), '</code><br>
                    • Status: <span class="', ifelse(model_row$singular_warn %in% c("OK", "MODERATE"), "status-ok", "status-problem"), 
                    '">', model_row$singular_warn, '</span>
                </div>
            </div>')
    }
  }
  
  # Close HTML
  html_content <- paste0(html_content, '
        </div>
    </body>
    </html>')
  
  # Write file
  output_file <- "NGDP_MIDAS_Results_Detailed.html"
  writeLines(html_content, output_file)
  
  cat("✓ Enhanced MIDAS HTML report generated successfully!\n")
  cat("File saved as:", output_file, "\n")
  cat("Open this file in your web browser to view the detailed results.\n")
  
  return(output_file)
}

# Run the fixed export function
export_midas_to_html_detailed_fixed()