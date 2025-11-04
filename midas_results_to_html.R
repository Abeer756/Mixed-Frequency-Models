# ===============================================
# MIDAS Results to HTML Converter
# ===============================================

library(knitr)
library(kableExtra)
library(htmltools)
library(dplyr)

# Assuming your MIDAS analysis has already been run and variables are in environment
# If not, source the main script first:
# source("NGDP_modified_Midas.R")

# Function to create HTML report
create_midas_html_report <- function(output_file = "midas_ngdp_results.html") {
  
  # Read the CSV results if they exist
  if (file.exists("midas_ngdp_fixed_leaderboard.csv")) {
    summary_table <- read.csv("midas_ngdp_fixed_leaderboard.csv", stringsAsFactors = FALSE)
  }
  
  # Create HTML content
  html_content <- tags$html(
    tags$head(
      tags$title("MIDAS Model Results - Nominal GDP Growth"),
      tags$style(HTML("
        body { 
          font-family: Arial, sans-serif; 
          margin: 40px; 
          background-color: #f8f9fa;
        }
        .header { 
          background-color: #2c3e50; 
          color: white; 
          padding: 20px; 
          border-radius: 8px;
          text-align: center;
          margin-bottom: 30px;
        }
        .section { 
          background-color: white; 
          padding: 20px; 
          margin-bottom: 20px; 
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .section h2 { 
          color: #2c3e50; 
          border-bottom: 2px solid #3498db;
          padding-bottom: 10px;
        }
        .section h3 { 
          color: #34495e; 
          margin-top: 25px;
        }
        table { 
          width: 100%; 
          border-collapse: collapse; 
          margin: 15px 0;
        }
        th, td { 
          border: 1px solid #ddd; 
          padding: 8px; 
          text-align: left;
        }
        th { 
          background-color: #3498db; 
          color: white;
          font-weight: bold;
        }
        tr:nth-child(even) { 
          background-color: #f2f2f2;
        }
        .reliable { 
          background-color: #d4edda !important; 
        }
        .problematic { 
          background-color: #f8d7da !important; 
        }
        .best-model {
          background-color: #fff3cd !important;
          font-weight: bold;
        }
        .metric {
          font-family: monospace;
          background-color: #f1f1f1;
          padding: 2px 4px;
          border-radius: 3px;
        }
        .summary-stats {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
          gap: 15px;
          margin: 20px 0;
        }
        .stat-box {
          background-color: #ecf0f1;
          padding: 15px;
          border-radius: 6px;
          text-align: center;
        }
        .stat-value {
          font-size: 24px;
          font-weight: bold;
          color: #2c3e50;
        }
        .stat-label {
          font-size: 14px;
          color: #7f8c8d;
          margin-top: 5px;
        }
      "))
    ),
    tags$body(
      # Header
      div(class = "header",
        h1("MIDAS Model Analysis Results"),
        h2("Mixed-Frequency Data Sampling for Nominal GDP Growth"),
        p(paste("Analysis Date:", Sys.Date()))
      ),
      
      # Executive Summary
      div(class = "section",
        h2("Executive Summary"),
        p("This report presents the results of Mixed-Frequency Data Sampling (MIDAS) models for forecasting Nominal GDP growth using various economic indicators at different frequencies."),
        
        # Summary statistics if data is available
        if (exists("summary_table")) {
          div(class = "summary-stats",
            div(class = "stat-box",
              div(class = "stat-value", nrow(summary_table)),
              div(class = "stat-label", "Total Models Tested")
            ),
            div(class = "stat-box",
              div(class = "stat-value", 
                  sum(summary_table$singular_warn %in% c("OK", "MODERATE"), na.rm = TRUE)),
              div(class = "stat-label", "Reliable Models")
            ),
            div(class = "stat-box",
              div(class = "stat-value", 
                  round(min(summary_table$AIC, na.rm = TRUE), 2)),
              div(class = "stat-label", "Best AIC Score")
            ),
            div(class = "stat-box",
              div(class = "stat-value", 
                  paste0(round(max(summary_table$R2, na.rm = TRUE) * 100, 1), "%")),
              div(class = "stat-label", "Highest R²")
            )
          )
        }
      ),
      
      # Model Specifications
      div(class = "section",
        h2("Model Specifications"),
        p("The following model specifications were tested:"),
        tags$ul(
          tags$li(strong("A_oilex:"), " Oil & Gas exports, Non-oil Omani exports, Re-exports, Total imports"),
          tags$li(strong("B1_oilex:"), " A_oilex + Narrow Money (M1)"),
          tags$li(strong("B2_oilex:"), " A_oilex + Broad Money (M2)"),
          tags$li(strong("D1_oilex:"), " Oil & Gas exports, Non-oil Omani exports, M1 (lean specification)"),
          tags$li(strong("D2_oilex:"), " Oil & Gas exports, Non-oil Omani exports, M2 (lean specification)"),
          tags$li(strong("E1_price:"), " Oil price, Non-oil Omani exports, M1"),
          tags$li(strong("E2_price:"), " Oil price, Non-oil Omani exports, M2"),
          tags$li(strong("D1_prod:"), " Daily oil production, Non-oil Omani exports, M1"),
          tags$li(strong("D2_prod:"), " Daily oil production, Non-oil Omani exports, M2"),
          tags$li(strong("F1_minimal:"), " Oil & Gas exports, M1 (minimal specification)"),
          tags$li(strong("F2_minimal:"), " Oil price, M1 (minimal specification)")
        )
      )
    )
  )
  
  # Add results tables if data exists
  if (exists("summary_table")) {
    
    # Full leaderboard
    leaderboard_html <- div(class = "section",
      h2("Complete MIDAS Leaderboard"),
      p("All models ranked by AIC (Akaike Information Criterion). Lower values indicate better model fit."),
      
      # Create formatted table
      HTML(
        summary_table %>%
          mutate(
            AIC = round(AIC, 2),
            BIC = round(BIC, 2),
            R2 = paste0(round(R2 * 100, 1), "%"),
            AdjR2 = paste0(round(AdjR2 * 100, 1), "%"),
            RMSE = round(RMSE, 3),
            MAE = round(MAE, 3),
            MASE = round(MASE, 3),
            Ljung_p = round(Ljung_p, 3)
          ) %>%
          select(model_id, group, L, AIC, BIC, R2, AdjR2, RMSE, MAE, MASE, singular_warn) %>%
          kable("html", escape = FALSE, table.attr = 'class="table"') %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
          row_spec(which(summary_table$singular_warn %in% c("OK", "MODERATE")), 
                   background = "#d4edda") %>%
          row_spec(which(summary_table$singular_warn %in% c("SEVERE", "NEAR-SINGULAR")), 
                   background = "#f8d7da")
      )
    )
    
    # Reliable models section
    reliable_models <- summary_table[summary_table$singular_warn %in% c("OK", "MODERATE"), ]
    if (nrow(reliable_models) > 0) {
      reliable_html <- div(class = "section",
        h2("Reliable Models"),
        p("Models with good numerical stability (OK/MODERATE flags). These models are recommended for forecasting."),
        HTML(
          reliable_models %>%
            arrange(AIC, MASE) %>%
            mutate(
              AIC = round(AIC, 2),
              R2 = paste0(round(R2 * 100, 1), "%"),
              RMSE = round(RMSE, 3),
              MASE = round(MASE, 3)
            ) %>%
            select(model_id, group, AIC, R2, RMSE, MASE, singular_warn) %>%
            kable("html", escape = FALSE) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
            row_spec(1, background = "#fff3cd", bold = TRUE)  # Highlight best model
        )
      )
    }
    
    # Best per group
    if (exists("best_per_group")) {
      best_group_html <- div(class = "section",
        h2("Best Model per Group"),
        p("Top performing model from each specification group."),
        HTML(
          best_per_group %>%
            arrange(AIC) %>%
            mutate(
              AIC = round(AIC, 2),
              BIC = round(BIC, 2),
              R2 = paste0(round(R2 * 100, 1), "%"),
              RMSE = round(RMSE, 3),
              MASE = round(MASE, 3)
            ) %>%
            select(group, AIC, BIC, R2, RMSE, MASE, singular_warn) %>%
            kable("html", escape = FALSE) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
        )
      )
    }
    
    # Add these sections to the HTML
    html_content <- tagAppendChild(html_content$children[[2]], leaderboard_html)
    if (exists("reliable_html")) {
      html_content <- tagAppendChild(html_content$children[[2]], reliable_html)
    }
    if (exists("best_group_html")) {
      html_content <- tagAppendChild(html_content$children[[2]], best_group_html)
    }
  }
  
  # Add methodology and conclusions
  methodology_html <- div(class = "section",
    h2("Methodology"),
    h3("MIDAS Approach"),
    p("Mixed-Frequency Data Sampling (MIDAS) allows for the incorporation of higher-frequency (monthly) predictor variables into models for lower-frequency (quarterly) target variables without the need for temporal aggregation."),
    
    h3("Model Selection Criteria"),
    tags$ul(
      tags$li(strong("AIC (Akaike Information Criterion):"), " Primary criterion for model selection"),
      tags$li(strong("Numerical Stability:"), " Models flagged for multicollinearity issues"),
      tags$li(strong("Out-of-sample Performance:"), " RMSE, MAE, and MASE on holdout data"),
      tags$li(strong("Statistical Diagnostics:"), " R², Adjusted R², Ljung-Box test for residual autocorrelation")
    ),
    
    h3("Key Improvements in Fixed Version"),
    tags$ul(
      tags$li("Removed problematic oil_price + oil_exports combinations to avoid multicollinearity"),
      tags$li("Created lean specifications without weak predictors"),
      tags$li("Added numerical stability flags to identify unreliable models"),
      tags$li("Separated reliable from problematic models for clear reporting")
    )
  )
  
  conclusions_html <- div(class = "section",
    h2("Conclusions and Recommendations"),
    tags$ul(
      tags$li(strong("Model Selection:"), " Use only models flagged as 'OK' or 'MODERATE' for forecasting"),
      tags$li(strong("Best Performing Specifications:"), " Models with lean predictor sets generally perform better"),
      tags$li(strong("Multicollinearity:"), " Oil price and oil export variables should not be used together"),
      tags$li(strong("Monetary Variables:"), " Both M1 and M2 show predictive power for nominal GDP growth")
    )
  )
  
  html_content <- tagAppendChild(html_content$children[[2]], methodology_html)
  html_content <- tagAppendChild(html_content$children[[2]], conclusions_html)
  
  # Save HTML file
  writeLines(as.character(html_content), output_file)
  cat("HTML report saved to:", output_file, "\n")
  
  return(output_file)
}

# Generate the HTML report
create_midas_html_report()

# Optional: Also create a simple table-only HTML for embedding
create_simple_results_table <- function(output_file = "midas_results_table.html") {
  
  if (!exists("summary_table")) {
    if (file.exists("midas_ngdp_fixed_leaderboard.csv")) {
      summary_table <- read.csv("midas_ngdp_fixed_leaderboard.csv", stringsAsFactors = FALSE)
    } else {
      cat("No results data found. Run MIDAS analysis first.\n")
      return(NULL)
    }
  }
  
  # Create simple HTML table
  simple_table <- summary_table %>%
    arrange(AIC, MASE) %>%
    mutate(
      AIC = round(AIC, 2),
      BIC = round(BIC, 2),
      R2 = paste0(round(R2 * 100, 1), "%"),
      AdjR2 = paste0(round(AdjR2 * 100, 1), "%"),
      RMSE = round(RMSE, 3),
      MAE = round(MAE, 3),
      MASE = round(MASE, 3)
    ) %>%
    select(Model = model_id, Specification = group, L, AIC, BIC, 
           `R²` = R2, `Adj R²` = AdjR2, RMSE, MAE, MASE, Status = singular_warn)
  
  html_table <- kable(simple_table, "html", escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE) %>%
    row_spec(which(summary_table$singular_warn %in% c("OK", "MODERATE")), 
             background = "#d4edda") %>%
    row_spec(which(summary_table$singular_warn %in% c("SEVERE", "NEAR-SINGULAR")), 
             background = "#f8d7da") %>%
    row_spec(1, background = "#fff3cd", bold = TRUE)
  
  writeLines(as.character(html_table), output_file)
  cat("Simple HTML table saved to:", output_file, "\n")
  
  return(output_file)
}

# Create simple table version
create_simple_results_table()

cat("\n===============================================\n")
cat("HTML CONVERSION COMPLETE\n")
cat("===============================================\n")
cat("Files created:\n")
cat("1. midas_ngdp_results.html - Full report\n")
cat("2. midas_results_table.html - Simple table\n")
cat("\nOpen these files in your web browser to view the results.\n")