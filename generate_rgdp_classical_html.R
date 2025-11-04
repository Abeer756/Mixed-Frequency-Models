# Runner: RGDP Classical HTML generation
# Sources the analysis script and then exports the HTML report.

# 1) Run the analysis to populate in-memory objects
source("RGDP_modified_Classical.R")

# 2) Generate the HTML matching NGDP style
source("export_rgdp_classical_html.R")
