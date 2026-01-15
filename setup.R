# ============================================================================
# Crypto Trading Bot - Setup Script
# ============================================================================
# This script installs and loads all required packages for the trading bot
# Run this file once before starting the bot for the first time

cat("=== Crypto Trading Bot Setup ===\n\n")

# Required packages
required_packages <- c(
  "httr",        # For HTTP requests to APIs
  "jsonlite",    # For parsing JSON responses
  "tidyverse",   # Data manipulation (dplyr, tidyr, ggplot2, etc.)
  "lubridate",   # Date/time handling
  "TTR",         # Technical Trading Rules (indicators)
  "quantmod",    # Quantitative Financial Modelling
  "zoo",         # Time series objects
  "logger"       # Logging functionality
)

# Function to install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat(paste0("Installing package: ", pkg, "\n"))
      install.packages(pkg, dependencies = TRUE, repos = "http://cran.rstudio.com/")
    } else {
      cat(paste0("✓ ", pkg, " already installed\n"))
    }
  }
}

# Install packages
cat("\nChecking and installing required packages...\n")
install_if_missing(required_packages)

# Load all packages
cat("\nLoading packages...\n")
invisible(lapply(required_packages, library, character.only = TRUE))

# Set up project paths
PROJECT_ROOT <- getwd()
CONFIG_DIR <- file.path(PROJECT_ROOT, "config")
DATA_DIR <- file.path(PROJECT_ROOT, "data")
RAW_DATA_DIR <- file.path(DATA_DIR, "raw")
PROCESSED_DATA_DIR <- file.path(DATA_DIR, "processed")
SRC_DIR <- file.path(PROJECT_ROOT, "src")
LOGS_DIR <- file.path(PROJECT_ROOT, "logs")

# Create directories if they don't exist
dirs_to_check <- c(CONFIG_DIR, DATA_DIR, RAW_DATA_DIR, 
                   PROCESSED_DATA_DIR, SRC_DIR, LOGS_DIR)

for (dir in dirs_to_check) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(paste0("Created directory: ", dir, "\n"))
  }
}

# Set global options
options(
  digits = 10,           # Precision for price data
  scipen = 999,          # Avoid scientific notation
  stringsAsFactors = FALSE
)

# Setup logging
log_file <- file.path(LOGS_DIR, paste0("setup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_appender(appender_file(log_file))
log_info("Setup completed successfully")

cat("\n=== Setup Complete! ===\n")
cat("All packages installed and loaded.\n")
cat("Project structure verified.\n")
cat(paste0("Log file created: ", log_file, "\n"))
cat("\nYou can now run main.R to start the bot.\n")