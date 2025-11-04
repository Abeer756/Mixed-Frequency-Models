# ===============================================
# Runner: Generate RGDP MIDAS HTML Report
# ===============================================
# This runner sources the current RGDP MIDAS analysis script and then
# generates the styled HTML report (like the NGDP flow).

cat("===============================================\n")
cat("RGDP MIDAS HTML REPORT GENERATOR\n")
cat("===============================================\n\n")

# 1) Run the current modified RGDP MIDAS script
cat(">>> Running RGDP_modified_Midas.R...\n")
start_time <- Sys.time()

tryCatch({
  source("RGDP_modified_Midas.R")
  cat("✓ RGDP MIDAS analysis completed\n")
}, error = function(e) {
  cat("✗ Error while running RGDP_modified_Midas.R:\n   ", conditionMessage(e), "\n")
  stop(e)
})

# 2) Build the HTML report (same style as NGDP)
cat(">>> Building RGDP MIDAS HTML report...\n")

tryCatch({
  source("export_rgdp_midas_html.R")
  cat("✓ RGDP MIDAS HTML report generated: RGDP_MIDAS_Results.html\n")
}, error = function(e) {
  cat("✗ Error while generating RGDP HTML report:\n   ", conditionMessage(e), "\n")
  stop(e)
})

end_time <- Sys.time()
cat("\nDone in", round(difftime(end_time, start_time, units = "secs"), 1), "seconds.\n")
