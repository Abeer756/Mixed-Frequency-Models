# ===========================
# RGDP MIDAS HTML Export Script  
# Enhanced with Individual Model Sections
# ===========================

# This script generates a comprehensive HTML report for RGDP MIDAS analysis
# Run this AFTER completing the RGDP_modified_Midas.R analysis

export_rgdp_midas_html <- function() {
  
  # Check if required objects exist
  if (!exists("summary_table")) {
    stop("summary_table object not found. Please run RGDP_modified_Midas.R first.")
  }
  
  if (!exists("results")) {
    stop("results object not found. Please run RGDP_modified_Midas.R first.")
  }
  
  if (!exists("best_per_group")) {
    stop("best_per_group object not found. Please run RGDP_modified_Midas.R first.")
  }
  
  # Start HTML content with enhanced styling
  html_content <- '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Real GDP MIDAS Analysis Results</title>
    <style>
        body { 
            font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif; 
            margin: 20px; 
            background-color: #f8f9fa; 
            color: #2c3e50;
        }
        .header { 
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white; 
            padding: 25px; 
            border-radius: 15px; 
            text-align: center;
            margin-bottom: 30px;
            box-shadow: 0 8px 32px rgba(102, 126, 234, 0.3);
        }
        .header h1 { 
            margin: 0; 
            font-size: 2.2em; 
            font-weight: 300;
        }
        .header p { 
            margin: 10px 0 0 0; 
            opacity: 0.9; 
            font-size: 1.1em;
        }
        .summary-stats {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin: 25px 0;
        }
        .stat-card {
            background: white;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
            border-left: 4px solid #667eea;
            text-align: center;
        }
        .stat-number { 
            font-size: 2em; 
            font-weight: bold; 
            color: #667eea;
            display: block;
        }
        .stat-label { 
            color: #7f8c8d; 
            font-size: 0.9em;
            margin-top: 5px;
        }
        table { 
            width: 100%; 
            border-collapse: collapse; 
            margin: 20px 0; 
            background: white;
            border-radius: 10px;
            overflow: hidden;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        th { 
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white; 
            padding: 15px 8px; 
            text-align: center;
            font-weight: 500;
            font-size: 0.9em;
        }
        td { 
            padding: 12px 8px; 
            text-align: center; 
            border-bottom: 1px solid #ecf0f1;
            font-size: 0.9em;
        }
        tr:nth-child(even) { 
            background-color: #f8f9fa; 
        }
        tr:hover { 
            background-color: #e3f2fd; 
            transition: background-color 0.3s;
        }
        .model-group { 
            background-color: #3498db; 
            color: white; 
            padding: 4px 8px; 
            border-radius: 15px; 
            font-size: 0.8em; 
            font-weight: bold;
        }
        .coef-pos { color: #27ae60; font-weight: bold; }
        .coef-neg { color: #e74c3c; font-weight: bold; }
        .sig-high { 
            background-color: #e74c3c; 
            color: white; 
            padding: 2px 6px; 
            border-radius: 10px; 
            font-size: 0.75em;
            font-weight: bold;
        }
        .sig-med { 
            background-color: #f39c12; 
            color: white; 
            padding: 2px 6px; 
            border-radius: 10px; 
            font-size: 0.75em;
            font-weight: bold;
        }
        .sig-low { 
            background-color: #95a5a6; 
            color: white; 
            padding: 2px 6px; 
            border-radius: 10px; 
            font-size: 0.75em;
        }
        .sig-marginal { 
            background-color: #bdc3c7; 
            color: #2c3e50; 
            padding: 2px 6px; 
            border-radius: 10px; 
            font-size: 0.75em;
        }
        h2 { 
            color: #2c3e50; 
            border-bottom: 3px solid #667eea; 
            padding-bottom: 10px; 
            margin-top: 40px;
        }
        .timestamp { 
            text-align: center; 
            color: #95a5a6; 
            font-style: italic; 
            margin-top: 30px;
            padding: 15px;
            background: white;
            border-radius: 10px;
        }
        .best-model { 
            background: linear-gradient(135deg, #2ecc71 0%, #27ae60 100%); 
            color: white; 
        }
        .coef-table th { font-size: 0.8em; padding: 10px 5px; }
        .coef-table td { font-size: 0.8em; padding: 8px 5px; }
    </style>
</head>
<body>'

  html_content <- paste0(html_content, '
    <div class="header">
        <h1>📊 Real GDP MIDAS Analysis Results</h1>
        <p>Mixed-Frequency Data Sampling with Nealmon Polynomial Structure</p>
        <p>Comprehensive Model Performance & Coefficient Analysis</p>
    </div>')

  # Add summary statistics
  total_models <- nrow(summary_table)
  best_aic <- min(summary_table$AIC, na.rm = TRUE)
  best_r2 <- max(summary_table$R2, na.rm = TRUE) * 100
  avg_mase <- mean(summary_table$MASE, na.rm = TRUE)
  
  html_content <- paste0(html_content, '
    <div class="summary-stats">
        <div class="stat-card">
            <span class="stat-number">', total_models, '</span>
            <div class="stat-label">Total Models</div>
        </div>
        <div class="stat-card">
            <span class="stat-number">', sprintf("%.1f", best_aic), '</span>
            <div class="stat-label">Best AIC</div>
        </div>
        <div class="stat-card">
            <span class="stat-number">', sprintf("%.1f%%", best_r2), '</span>
            <div class="stat-label">Highest R²</div>
        </div>
        <div class="stat-card">
            <span class="stat-number">', sprintf("%.3f", avg_mase), '</span>
            <div class="stat-label">Avg MASE</div>
        </div>
    </div>')

  # Model Performance Leaderboard
  html_content <- paste0(html_content, '
    <h2>🏆 Model Performance Leaderboard</h2>
    <p><em>All models ranked by AIC (lower is better), with comprehensive performance metrics</em></p>
    <table>
        <tr>
            <th>Rank</th>
            <th>Model ID</th>
            <th>Group</th>
            <th>AIC</th>
            <th>BIC</th>
            <th>R²</th>
            <th>Adj R²</th>
            <th>RMSE</th>
            <th>MAE</th>
            <th>MASE</th>
        </tr>')
  
  # Add leaderboard rows
  for (i in seq_len(nrow(summary_table))) {
    row <- summary_table[i, ]
    row_class <- ifelse(i == 1, 'class="best-model"', '')
    
    html_content <- paste0(html_content,
      '<tr ', row_class, '>
          <td><strong>', i, '</strong></td>
          <td>', row$model_id, '</td>
          <td><span class="model-group">', row$group, '</span></td>
          <td>', round(row$AIC, 2), '</td>
          <td>', round(row$BIC, 2), '</td>
          <td>', round(row$R2 * 100, 1), '%</td>
          <td>', round(row$AdjR2 * 100, 1), '%</td>
          <td>', round(row$RMSE, 3), '</td>
          <td>', round(row$MAE, 3), '</td>
          <td>', round(row$MASE, 3), '</td>
      </tr>')
  }
  
  html_content <- paste0(html_content, '</table>')

  # Best Per Group Analysis
  html_content <- paste0(html_content, '
    <h2>🎯 Best Model Per Group</h2>
    <p><em>Top performing model in each specification category</em></p>
    <table>
        <tr>
            <th>Group</th>
            <th>Model ID</th>
            <th>AIC</th>
            <th>BIC</th>
            <th>R²</th>
            <th>Adj R²</th>
            <th>RMSE</th>
            <th>MAE</th>
            <th>MASE</th>
        </tr>')
  
  # Sort best_per_group by AIC
  best_sorted <- best_per_group[order(best_per_group$AIC), ]
  
  for (i in seq_len(nrow(best_sorted))) {
    row <- best_sorted[i, ]
    
    html_content <- paste0(html_content,
      '<tr>
          <td><span class="model-group">', row$group, '</span></td>
          <td><strong>', row$model_id, '</strong></td>
          <td>', round(row$AIC, 2), '</td>
          <td>', round(row$BIC, 2), '</td>
          <td>', round(row$R2 * 100, 1), '%</td>
          <td>', round(row$AdjR2 * 100, 1), '%</td>
          <td>', round(row$RMSE, 3), '</td>
          <td>', round(row$MAE, 3), '</td>
          <td>', round(row$MASE, 3), '</td>
      </tr>')
  }
  
  html_content <- paste0(html_content, '</table>')

  # Individual Model Analysis
  html_content <- paste0(html_content, '
    <h2>🔍 Individual Model Analysis</h2>
    <p><em>Detailed coefficient analysis for each model with MIDAS structure insights</em></p>')
  
  # Get unique model IDs and process each model separately
  unique_models <- sort(unique(summary_table$model_id))
  
  for (model_num in unique_models) {
    model_info <- summary_table[summary_table$model_id == model_num, ]
    if (nrow(model_info) == 0) next
    
    model_info <- model_info[1, ]  # Take first row if multiple
    
    html_content <- paste0(html_content, '
        <div style="margin: 25px 0; border: 2px solid #667eea; border-radius: 10px; padding: 20px; background: #fafbfc;">
            <h3 style="margin-top: 0; color: #2c3e50;">📈 Model ', model_num, ' - ', model_info$group, '</h3>
            
            <div style="background: #e8f5e8; padding: 12px; border-radius: 5px; margin: 10px 0;">
                <strong>Model Performance:</strong><br>
                • AIC: <code>', sprintf("%.2f", model_info$AIC), '</code> | BIC: <code>', sprintf("%.2f", model_info$BIC), '</code><br>
                • R²: <code>', sprintf("%.3f", model_info$R2), '</code> | Adj R²: <code>', sprintf("%.3f", model_info$AdjR2), '</code><br>
                • RMSE: <code>', sprintf("%.3f", model_info$RMSE), '</code> | MAE: <code>', sprintf("%.3f", model_info$MAE), '</code> | MASE: <code>', sprintf("%.3f", model_info$MASE), '</code>
            </div>')
    
    # Try to extract coefficient information from results object
    tryCatch({
      model_result <- results[[model_num]]
      if (!is.null(model_result) && !is.null(model_result$best)) {
        model_obj <- model_result$best
        
        # Extract coefficients with error handling
        if (inherits(model_obj, "midas_r")) {
          coefs <- tryCatch({
            coef(model_obj)
          }, error = function(e) {
            tryCatch({
              model_obj$coefficients
            }, error = function(e2) {
              NULL
            })
          })
          
          if (!is.null(coefs) && length(coefs) > 0) {
            # Calculate t-statistics and p-values if possible  
            tryCatch({
              se <- sqrt(diag(vcov(model_obj)))
              t_stats <- coefs / se
              p_values <- 2 * (1 - pt(abs(t_stats), df = model_obj$df.residual))
              
              html_content <- paste0(html_content, '
                <h4>MIDAS Coefficient Analysis:</h4>
                <table style="width: 100%; font-size: 11px; margin: 15px 0; border-collapse: collapse;">
                    <tr style="background-color: #34495e; color: white;">
                        <th style="border: 1px solid #ddd; padding: 8px; text-align:left;">Parameter</th>
                        <th style="border: 1px solid #ddd; padding: 8px;">Coefficient</th>
                        <th style="border: 1px solid #ddd; padding: 8px;">Std Error</th>
                        <th style="border: 1px solid #ddd; padding: 8px;">t-stat</th>
                        <th style="border: 1px solid #ddd; padding: 8px;">p-value</th>
                        <th style="border: 1px solid #ddd; padding: 8px;">Sig</th>
                    </tr>')
              
              coef_names <- names(coefs)
              for (j in seq_along(coefs)) {
                coef_val <- coefs[j]
                se_val <- se[j]
                t_val <- t_stats[j]
                p_val <- p_values[j]
                
                # Significance indicators
                sig_indicator <- if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else if (p_val < 0.1) "." else ""
                sig_class <- if (sig_indicator %in% c("***", "**")) "sig-high" else if (sig_indicator == "*") "sig-med" else if (sig_indicator == ".") "sig-low" else "sig-marginal"
                
                # Coefficient coloring
                coef_class <- ifelse(coef_val > 0, "coef-pos", "coef-neg")
                
                # Row background alternating
                row_bg <- ifelse(j %% 2 == 0, "#f9f9f9", "white")
                
                html_content <- paste0(html_content,
                  '<tr style="background-color: ', row_bg, ';">
                      <td style="border: 1px solid #ddd; padding: 6px; font-family: monospace; font-size: 10px; text-align: left;">', 
                        ifelse(is.null(coef_names[j]) || is.na(coef_names[j]), paste0("coef_", j), coef_names[j]), '</td>
                      <td style="border: 1px solid #ddd; padding: 6px; text-align:center;"><span class="', coef_class, '">', 
                        sprintf("%.6f", coef_val), '</span></td>
                      <td style="border: 1px solid #ddd; padding: 6px; text-align:center;">', sprintf("%.6f", se_val), '</td>
                      <td style="border: 1px solid #ddd; padding: 6px; text-align:center;">', sprintf("%.3f", t_val), '</td>
                      <td style="border: 1px solid #ddd; padding: 6px; text-align:center;">', sprintf("%.6f", p_val), '</td>
                      <td style="border: 1px solid #ddd; padding: 6px; text-align:center;"><span class="', sig_class, '">', 
                        ifelse(sig_indicator == "", "n.s.", sig_indicator), '</span></td>
                  </tr>')
              }
              
              html_content <- paste0(html_content, '</table>')
              
            }, error = function(e) {
              html_content <<- paste0(html_content, '
                <div style="background: #fff3cd; padding: 8px; border-radius: 4px; margin-top: 10px;">
                    <strong>Coefficient Details:</strong> Statistical inference not available for this model structure.
                </div>')
            })
            
          } else {
            html_content <- paste0(html_content, '
              <div style="background: #fff3cd; padding: 8px; border-radius: 4px; margin-top: 10px;">
                  <strong>Coefficient Information:</strong> Unable to extract detailed coefficients from MIDAS model structure.
              </div>')
          }
        }
      }
    }, error = function(e) {
      html_content <<- paste0(html_content, '
        <div style="background: #fff3cd; padding: 8px; border-radius: 4px; margin-top: 10px;">
            <strong>Model Details:</strong> Detailed coefficient analysis not available for this model.
        </div>')
    })
    
    html_content <- paste0(html_content, '
            <div style="background: #fff3cd; padding: 8px; border-radius: 4px; margin-top: 10px; font-size: 11px;">
                <strong>Key Insights:</strong><br>
                • <strong>Model Type:</strong> MIDAS with Nealmon Polynomial Structure<br>
                • <strong>Model Rank by AIC:</strong> ', 
                which(summary_table$model_id[order(summary_table$AIC)] == model_num), 
                ' out of ', nrow(summary_table), ' models<br>
                • <strong>Group Performance:</strong> ', model_info$group, ' specification
            </div>
        </div>')
  }

  # Add timestamp
  html_content <- paste0(html_content, '
    <div class="timestamp">
        <p>Report generated on ', Sys.time(), '</p>
        <p><em>Real GDP MIDAS Analysis | Mixed-Frequency Econometric Modeling</em></p>
    </div>
</body>
</html>')

  # Write to file
  writeLines(html_content, "RGDP_MIDAS_Results.html")
  cat("✅ RGDP MIDAS HTML report saved as: RGDP_MIDAS_Results.html\n")
  
  # Try to open in browser (cross-platform)
  tryCatch({
    if (.Platform$OS.type == "windows") {
      shell.exec("RGDP_MIDAS_Results.html")
    } else {
      system2("open", "RGDP_MIDAS_Results.html")  # macOS
    }
    cat("🌐 HTML report opened in default browser\n")
  }, error = function(e) {
    cat("📁 Please manually open RGDP_MIDAS_Results.html in your browser\n")
  })
}

# Execute the function
cat("🚀 Starting RGDP MIDAS HTML export...\n")
export_rgdp_midas_html()