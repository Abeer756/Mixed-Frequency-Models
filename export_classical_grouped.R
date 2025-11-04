# ===============================================
# Enhanced Classical HTML Export with Model Grouping
# Separates models by categories for better organization
# ===============================================

export_classical_grouped_html <- function() {
  
  # Check if results exist
  if (!exists("summary_table_q")) {
    cat("Error: summary_table_q not found. Run Classical analysis first.\n")
    return(NULL)
  }
  
  cat("Generating grouped Classical HTML report...\n")
  
  # Define model categories based on specification patterns
  categorize_model <- function(group_name) {
    if (grepl("^A_", group_name)) {
      return("Basic Models")
    } else if (grepl("^B1_", group_name)) {
      return("Basic + M1 Models")
    } else if (grepl("^B2_", group_name)) {
      return("Basic + M2 Models")  
    } else if (grepl("^C1_", group_name)) {
      return("Comprehensive + M1 Models")
    } else if (grepl("^C2_", group_name)) {
      return("Comprehensive + M2 Models")
    } else {
      return("Other Models")
    }
  }
  
  # Add category to summary table
  summary_table_q$category <- sapply(summary_table_q$group, categorize_model)
  summary_table_q$approach <- ifelse(grepl("_oilex$", summary_table_q$group), "Oil Export Approach", "Production Approach")
  
  # Create HTML content
  html_content <- paste0(
    '<!DOCTYPE html>
    <html>
    <head>
        <title>Classical OLS Results - Nominal GDP Growth (Grouped)</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }
            .container { max-width: 1800px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
            h1 { color: #2c3e50; text-align: center; border-bottom: 3px solid #e74c3c; padding-bottom: 10px; }
            h2 { color: #34495e; margin-top: 30px; border-bottom: 2px solid #ecf0f1; padding-bottom: 5px; }
            h3 { color: #2c3e50; margin-top: 25px; background: #f8f9fa; padding: 10px; border-radius: 5px; }
            h4 { color: #34495e; margin-top: 20px; }
            table { width: 100%; border-collapse: collapse; margin: 15px 0; font-size: 11px; }
            th, td { border: 1px solid #ddd; padding: 6px; text-align: center; }
            th { background-color: #e74c3c; color: white; font-weight: bold; }
            tr:nth-child(even) { background-color: #f9f9f9; }
            .best-overall { background-color: #fff3cd !important; font-weight: bold; }
            .best-category { background-color: #d1ecf1 !important; }
            .oil-export { border-left: 4px solid #3498db; }
            .production { border-left: 4px solid #e67e22; }
            .summary-box { background: #ecf0f1; padding: 15px; border-radius: 5px; margin: 10px 0; }
            .metric { font-family: monospace; background: #3498db; color: white; padding: 2px 5px; border-radius: 3px; }
            .category-header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 12px; border-radius: 8px; margin: 20px 0 10px 0; }
            .approach-badge { 
                padding: 3px 8px; border-radius: 12px; font-size: 10px; font-weight: bold;
                background-color: #3498db; color: white; margin-left: 5px;
            }
            .production-badge { background-color: #e67e22 !important; }
            .model-count { font-size: 12px; opacity: 0.8; }
            .performance-highlight { background-color: #e8f5e9; padding: 8px; border-radius: 4px; margin: 5px 0; }
        </style>
    </head>
    <body>
        <div class="container">
            <h1>Classical OLS Analysis Results (Model Groups)</h1>
            <h2>Quarterly Regression Models for Nominal GDP Growth</h2>
            
            <div class="summary-box">
                <strong>Analysis Overview:</strong><br>
                • Total Models Tested: <span class="metric">', nrow(summary_table_q), '</span><br>
                • Oil Export Approach: <span class="metric">', sum(summary_table_q$approach == "Oil Export Approach"), '</span> models<br>
                • Production Approach: <span class="metric">', sum(summary_table_q$approach == "Production Approach"), '</span> models<br>
                • Best Overall AIC: <span class="metric">', round(min(summary_table_q$AIC, na.rm = TRUE), 2), '</span><br>
                • Highest R²: <span class="metric">', round(max(summary_table_q$R2, na.rm = TRUE) * 100, 1), '%</span><br>
                • Analysis Date: <span class="metric">', Sys.Date(), '</span>
            </div>
    ')
  
  # Add overall best model summary
  best_model <- summary_table_q[which.min(summary_table_q$AIC), ]
  html_content <- paste0(html_content, '
            <div class="performance-highlight">
                <strong>🏆 Best Overall Model:</strong> Model ', best_model$model_id, 
                ' (', best_model$group, ') - AIC: ', round(best_model$AIC, 2), 
                ', R²: ', round(best_model$R2 * 100, 1), '%',
                '<span class="approach-badge ', 
                ifelse(best_model$approach == "Production Approach", "production-badge", ""), 
                '">', best_model$approach, '</span>
            </div>')
  
  # Get unique categories and process each
  categories <- unique(summary_table_q$category)
  categories <- categories[order(categories)]  # Sort alphabetically
  
  for (cat_name in categories) {
    cat_models <- summary_table_q[summary_table_q$category == cat_name, ]
    cat_models <- cat_models[order(cat_models$AIC), ]  # Sort by AIC within category
    
    html_content <- paste0(html_content, '
            <div class="category-header">
                <h3 style="margin: 0; color: white;">📊 ', cat_name, 
                ' <span class="model-count">(', nrow(cat_models), ' models)</span></h3>
            </div>')
    
    # Category performance summary
    best_cat <- cat_models[1, ]
    oil_count <- sum(cat_models$approach == "Oil Export Approach")
    prod_count <- sum(cat_models$approach == "Production Approach")
    
    html_content <- paste0(html_content, '
            <div style="background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;">
                <strong>Category Best:</strong> ', best_cat$group, 
                ' (AIC: ', round(best_cat$AIC, 2), ', R²: ', round(best_cat$R2 * 100, 1), '%) | ',
                '<strong>Approaches:</strong> ', oil_count, ' Oil Export, ', prod_count, ' Production
            </div>')
    
    # Category table
    html_content <- paste0(html_content, '
            <table>
                <tr>
                    <th>Model ID</th>
                    <th>Specification</th>
                    <th>Approach</th>
                    <th>AIC</th>
                    <th>BIC</th>
                    <th>R²</th>
                    <th>Adj R²</th>
                    <th>RMSE</th>
                    <th>MAE</th>
                    <th>MASE</th>
                </tr>')
    
    for (i in seq_len(nrow(cat_models))) {
      row <- cat_models[i, ]
      
      # Determine row styling
      row_class <- ""
      if (row$model_id == best_model$model_id) {
        row_class <- 'class="best-overall"'
      } else if (i == 1) {
        row_class <- 'class="best-category"'
      }
      
      approach_class <- ifelse(row$approach == "Oil Export Approach", "oil-export", "production")
      approach_badge <- ifelse(row$approach == "Production Approach", "production-badge", "")
      
      html_content <- paste0(html_content, 
        '<tr ', row_class, ' style="border-left: 4px solid ', 
        ifelse(row$approach == "Oil Export Approach", "#3498db", "#e67e22"), ';">
            <td><strong>', row$model_id, '</strong></td>
            <td style="text-align:left; font-size:10px;">', row$group, '</td>
            <td><span class="approach-badge ', approach_badge, '">', 
                substr(row$approach, 1, 3), '</span></td>
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
  }
  
  # Add approach comparison
  html_content <- paste0(html_content, '
            <h2>📈 Approach Comparison</h2>')
  
  for (approach in c("Oil Export Approach", "Production Approach")) {
    app_models <- summary_table_q[summary_table_q$approach == approach, ]
    app_models <- app_models[order(app_models$AIC), ]
    
    best_app <- app_models[1, ]
    avg_aic <- mean(app_models$AIC, na.rm = TRUE)
    avg_r2 <- mean(app_models$R2, na.rm = TRUE)
    
    badge_class <- ifelse(approach == "Production Approach", "production-badge", "")
    
    html_content <- paste0(html_content, '
            <h4>', approach, ' <span class="approach-badge ', badge_class, '">', 
            nrow(app_models), ' models</span></h4>
            <div class="performance-highlight">
                <strong>Best Model:</strong> ', best_app$group, ' (Model ', best_app$model_id, 
                ') - AIC: ', round(best_app$AIC, 2), ', R²: ', round(best_app$R2 * 100, 1), '%<br>
                <strong>Average Performance:</strong> AIC: ', round(avg_aic, 2), 
                ', R²: ', round(avg_r2 * 100, 1), '%
            </div>')
  }
  
  # Add model specifications reference
  html_content <- paste0(html_content, '
            <h2>📋 Model Specifications Guide</h2>
            
            <h4>🔸 Basic Models (A_*)</h4>
            <ul>
                <li><strong>A_oilex:</strong> Oil & Gas exports + Non-oil Omani exports + Re-exports + Total imports</li>
                <li><strong>A_prod:</strong> Daily Oil Production + Non-oil Omani exports + Re-exports + Total imports</li>
            </ul>
            
            <h4>🔸 Basic + Money Supply Models (B1_*/B2_*)</h4>
            <ul>
                <li><strong>B1_*:</strong> Basic specification + Narrow Money (M1)</li>
                <li><strong>B2_*:</strong> Basic specification + Broad Money (M2)</li>
            </ul>
            
            <h4>🔸 Comprehensive Models (C1_*/C2_*)</h4>
            <ul>
                <li><strong>C1_*:</strong> Basic + M1 + Oil Price (most comprehensive with M1)</li>
                <li><strong>C2_*:</strong> Basic + M2 + Oil Price (most comprehensive with M2)</li>
            </ul>
            
            <h4>🔸 Approach Differences</h4>
            <ul>
                <li><strong>Oil Export Approach (*_oilex):</strong> Uses oil & gas export quantities</li>
                <li><strong>Production Approach (*_prod):</strong> Uses daily average oil production</li>
            </ul>
            
            <p><em>All models include autoregressive terms: y(t-1), y(t-2), y(t-4)</em></p>
            
        </div>
    </body>
    </html>')
  
  # Write file
  output_file <- "NGDP_Classical_Results_Grouped.html"
  writeLines(html_content, output_file)
  
  cat("✓ Grouped Classical HTML report generated!\n")
  cat("File saved as:", output_file, "\n")
  cat("Models organized by:", length(categories), "categories\n")
  cat("Approaches compared: Oil Export vs Production\n")
  
  return(output_file)
}

# Run the grouped export function
export_classical_grouped_html()