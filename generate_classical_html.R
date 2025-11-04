# ===============================================
# STANDALONE: Generate Classical OLS HTML Report
# Run this after your Classical analysis is complete
# ===============================================

cat("=================================================\n")
cat("CLASSICAL OLS HTML REPORT GENERATOR\n")
cat("=================================================\n")

# Check if Classical analysis results exist
if (!exists("summary_table_q")) {
  cat("ERROR: Classical analysis results not found!\n")
  cat("\nPlease run your Classical analysis first:\n")
  cat("source('NGDP_modified_Classical_final.R')\n")
  cat("\nThen run this script again.\n")
  stop("Classical analysis results required")
}

cat("✓ Found Classical analysis results\n")
cat("✓ Summary table:", nrow(summary_table_q), "models\n")

if (exists("coef_table_q")) {
  cat("✓ Found coefficient table:", nrow(coef_table_q), "coefficients\n")
}

if (exists("std_table")) {
  cat("✓ Found standardized beta table:", nrow(std_table), "entries\n")
}

cat("\nGenerating HTML report...\n")

# Generate the HTML report
tryCatch({
  source("fix_classical_export.R")
  cat("\n=================================================\n")
  cat("SUCCESS: Classical HTML report generated!\n")
  cat("=================================================\n")
  cat("File: NGDP_Classical_Results.html\n")
  cat("Location: ", getwd(), "\n")
  cat("\nOpen this file in your web browser to view the results.\n")
}, error = function(e) {
  cat("\n=================================================\n")
  cat("ERROR: Failed to generate HTML report\n")
  cat("=================================================\n")
  cat("Error message:", conditionMessage(e), "\n")
  
  # Try a basic version
  cat("\nAttempting basic HTML generation...\n")
  
  basic_html <- paste0(
    '<!DOCTYPE html>
    <html>
    <head>
        <title>Classical OLS Results</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; }
            table { border-collapse: collapse; width: 100%; }
            th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
            th { background-color: #f2f2f2; }
        </style>
    </head>
    <body>
        <h1>Classical OLS Results - Nominal GDP Growth</h1>
        <h2>Model Performance Summary</h2>
        <p>Generated on: ', Sys.Date(), '</p>
        <p>Total models: ', nrow(summary_table_q), '</p>
        <p>Best AIC: ', round(min(summary_table_q$AIC, na.rm = TRUE), 3), '</p>
        <p>Best R²: ', round(max(summary_table_q$R2, na.rm = TRUE), 3), '</p>
        
        <h3>Model Leaderboard</h3>
        <table>
            <tr><th>Model</th><th>Group</th><th>AIC</th><th>R²</th><th>Adj R²</th><th>RMSE</th></tr>')
  
  # Add model rows
  sorted_models <- summary_table_q[order(summary_table_q$AIC), ]
  for (i in seq_len(min(10, nrow(sorted_models)))) {
    row <- sorted_models[i, ]
    basic_html <- paste0(basic_html, 
      '<tr><td>', row$model_id, '</td><td>', row$group, '</td><td>', 
      round(row$AIC, 2), '</td><td>', round(row$R2, 3), '</td><td>', 
      round(row$AdjR2, 3), '</td><td>', round(row$RMSE, 3), '</td></tr>')
  }
  
  basic_html <- paste0(basic_html, '
        </table>
        
        <p><em>Basic HTML report generated due to error in detailed version.</em></p>
        <p><em>Check R console for detailed results.</em></p>
    </body>
    </html>')
  
  # Write basic version
  writeLines(basic_html, "NGDP_Classical_Results_Basic.html")
  cat("✓ Basic HTML report saved as: NGDP_Classical_Results_Basic.html\n")
})

cat("\n=================================================\n")
cat("REPORT GENERATION COMPLETE\n")
cat("=================================================\n")