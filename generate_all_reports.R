# ===============================================
# MASTER SCRIPT: Generate All HTML Reports
# ===============================================
# This script runs both MIDAS and Classical analyses 
# and generates comprehensive HTML reports for each

cat("===============================================\n")
cat("MIXED-FREQUENCY ANALYSIS SUITE\n") 
cat("===============================================\n")
cat("Starting comprehensive analysis...\n\n")

# Clear workspace for clean start
rm(list = ls())

# ===============================================
# PART 1: NGDP MIDAS ANALYSIS
# ===============================================
cat(">>> Running NGDP MIDAS Analysis...\n")
start_time_ngdp_midas <- Sys.time()

tryCatch({
  source("NGDP_modified_Midas.R")
  cat("✓ NGDP MIDAS analysis completed successfully\n")
  
  cat(">>> Generating NGDP MIDAS HTML report...\n")
  source("export_midas_html_detailed.R") 
  cat("✓ NGDP MIDAS HTML report generated\n\n")
  
}, error = function(e) {
  cat("✗ Error in NGDP MIDAS analysis:", conditionMessage(e), "\n")
})

end_time_ngdp_midas <- Sys.time()
ngdp_midas_duration <- round(difftime(end_time_ngdp_midas, start_time_ngdp_midas, units = "secs"), 1)

# ===============================================
# PART 2: RGDP MIDAS ANALYSIS
# ===============================================
cat(">>> Running RGDP MIDAS Analysis...\n")
start_time_rgdp_midas <- Sys.time()

tryCatch({
  source("RGDP_modified_Midas.R")
  cat("✓ RGDP MIDAS analysis completed successfully\n")
  
  cat(">>> Generating RGDP MIDAS HTML report...\n")
  source("export_rgdp_midas_html.R") 
  cat("✓ RGDP MIDAS HTML report generated\n\n")
  
}, error = function(e) {
  cat("✗ Error in RGDP MIDAS analysis:", conditionMessage(e), "\n")
})

end_time_rgdp_midas <- Sys.time()
rgdp_midas_duration <- round(difftime(end_time_rgdp_midas, start_time_rgdp_midas, units = "secs"), 1)

# ===============================================  
# PART 3: CLASSICAL ANALYSIS
# ===============================================
cat(">>> Running Classical OLS Analysis...\n")
start_time_classical <- Sys.time()

tryCatch({
  source("NGDP_modified_Classical_final.R")
  cat("✓ Classical analysis completed successfully\n")
  
  cat(">>> Generating Classical HTML reports...\n")
  source("fix_classical_export.R")
  cat("✓ Classical HTML report with individual model sections generated\n\n")
  
}, error = function(e) {
  cat("✗ Error in Classical analysis:", conditionMessage(e), "\n")
})

end_time_classical <- Sys.time()
classical_duration <- round(difftime(end_time_classical, start_time_classical, units = "secs"), 1)

# ===============================================
# SUMMARY REPORT
# ===============================================
total_duration <- round(difftime(end_time_classical, start_time_ngdp_midas, units = "secs"), 1)

cat("===============================================\n")
cat("ANALYSIS COMPLETE - SUMMARY\n")
cat("===============================================\n")
cat("NGDP MIDAS Analysis Duration:", ngdp_midas_duration, "seconds\n")
cat("RGDP MIDAS Analysis Duration:", rgdp_midas_duration, "seconds\n")
cat("Classical Analysis Duration:", classical_duration, "seconds\n") 
cat("Total Processing Time:", total_duration, "seconds\n\n")

cat("FILES GENERATED:\n")
cat("===============================================\n")

# Check which files were created
files_created <- c()

if (file.exists("NGDP_MIDAS_Results_Detailed.html")) {
  files_created <- c(files_created, "✓ NGDP_MIDAS_Results_Detailed.html")
}

if (file.exists("NGDP_MIDAS_Results.html")) {
  files_created <- c(files_created, "✓ NGDP_MIDAS_Results.html")
}

if (file.exists("RGDP_MIDAS_Results.html")) {
  files_created <- c(files_created, "✓ RGDP_MIDAS_Results.html")
}

if (file.exists("NGDP_Classical_Results.html")) {
  files_created <- c(files_created, "✓ NGDP_Classical_Results.html")
}

if (file.exists("midas_ngdp_fixed_leaderboard.csv")) {
  files_created <- c(files_created, "✓ midas_ngdp_fixed_leaderboard.csv")
}

if (file.exists("classical_ngdp_leaderboard.csv")) {
  files_created <- c(files_created, "✓ classical_ngdp_leaderboard.csv")  
}

if (file.exists("classical_ngdp_coef_table.csv")) {
  files_created <- c(files_created, "✓ classical_ngdp_coef_table.csv")
}

if (file.exists("classical_ngdp_std_betas.csv")) {
  files_created <- c(files_created, "✓ classical_ngdp_std_betas.csv")
}

if (file.exists("midas_weights_ngdp_fixed.png")) {
  files_created <- c(files_created, "✓ midas_weights_ngdp_fixed.png")
}

for (file in files_created) {
  cat(file, "\n")
}

cat("NEXT STEPS:\n")
cat("===============================================\n")
cat("1. Open NGDP_MIDAS_Results.html in your web browser\n")
cat("2. Open RGDP_MIDAS_Results.html in your web browser\n")
cat("3. Open NGDP_Classical_Results.html in your web browser\n") 
cat("4. Compare model performance across methodologies and target variables\n")
cat("5. Use CSV files for further analysis if needed\n\n")

cat("ANALYSIS METHODOLOGY COMPARISON:\n")
cat("===============================================\n")
cat("MIDAS Models:\n")
cat("• Mixed-frequency approach (monthly → quarterly)\n")
cat("• Nealmon polynomial lag structures\n") 
cat("• Direct handling of frequency mismatch\n")
cat("• Optimal lag selection via AIC\n\n")

cat("Classical Models:\n")
cat("• Temporal aggregation approach\n")
cat("• Quarterly mean of monthly growth rates\n")
cat("• AR(1,2,4) structure for GDP dynamics\n") 
cat("• Robust standard errors (White/Newey-West)\n\n")

cat("Both approaches provide complementary insights into\n")
cat("nominal GDP growth forecasting using mixed-frequency data.\n\n")

cat("===============================================\n")
cat("Happy analyzing! 📊📈\n")
cat("===============================================\n")