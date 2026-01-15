# ============================================================================
# Crypto Trading Bot - Main Execution Script
# ============================================================================
# Entry point for running the cryptocurrency trading bot

cat("\n")
cat("========================================\n")
cat("  CRYPTOCURRENCY TRADING BOT\n")
cat("========================================\n")
cat("\n")

# ============================================================================
# SETUP
# ============================================================================

# Set working directory to project root
if (basename(getwd()) != "crypto-trading-bot") {
  if (file.exists("crypto-trading-bot")) {
    setwd("crypto-trading-bot")
  }
}

cat("Project directory:", getwd(), "\n\n")

# Load configuration
cat("Loading configuration...\n")
source("config/config.R")

# Load required packages (assumes setup.R has been run)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(logger)

# Setup logging
log_file <- file.path(LOGS_DIR, paste0("bot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_appender(appender_file(log_file))
log_threshold(LOG_LEVEL)

log_info("=== Bot Starting ===")

# Source modules
cat("Loading modules...\n")
source("src/data_acquisition/api_client.R")
source("src/data_acquisition/real_time.R")
source("src/data_acquisition/historical.R")

cat("\n")

# ============================================================================
# CONFIGURATION DISPLAY
# ============================================================================

print_config()

# ============================================================================
# API CONNECTION TEST
# ============================================================================

cat("Testing API connection...\n")
if (check_api_connection()) {
  cat("✓ Connected to CryptoCompare API\n\n")
} else {
  cat("✗ Failed to connect to API\n")
  cat("Please check your internet connection and try again.\n")
  stop("API connection failed")
}

# ============================================================================
# MAIN BOT LOGIC
# ============================================================================

run_bot <- function(mode = "demo") {
  
  if (mode == "demo") {
    cat("\n=== DEMO MODE ===\n\n")
    
    # Demo 1: Get current prices
    cat("--- Demo 1: Current Prices ---\n")
    prices <- get_multiple_prices(CRYPTOCURRENCIES[1:3])
    if (!is.null(prices)) {
      print(prices)
    }
    cat("\n")
    
    # Demo 2: Get full market data
    cat("--- Demo 2: Full Market Data ---\n")
    market_data <- get_full_market_data(CRYPTOCURRENCIES[1:2])
    if (!is.null(market_data)) {
      print(market_data)
    }
    cat("\n")
    
    # Demo 3: Get historical daily data
    cat("--- Demo 3: Historical Data (Last 7 Days) ---\n")
    historical <- get_historical_days("BTC", limit = 7)
    if (!is.null(historical)) {
      print(historical)
      cat("\nBasic Statistics:\n")
      stats <- calculate_stats(historical)
      print(stats)
    }
    cat("\n")
    
    # Demo 4: Monitor prices (short duration for demo)
    cat("--- Demo 4: Price Monitoring (30 seconds) ---\n")
    cat("Monitoring BTC and ETH prices every 10 seconds...\n")
    price_history <- monitor_prices(
      cryptos = c("BTC", "ETH"),
      duration_minutes = 0.5,  # 30 seconds
      interval_seconds = 10,
      save_data = TRUE
    )
    
    cat("\n=== Demo Complete ===\n")
    
  } else if (mode == "collect_data") {
    cat("\n=== DATA COLLECTION MODE ===\n\n")
    
    # Download historical data for all configured cryptocurrencies
    all_data <- download_historical_data(
      cryptos = CRYPTOCURRENCIES,
      days = 30,
      interval = "day"
    )
    
    cat("\nData collection complete!\n")
    
  } else if (mode == "monitor") {
    cat("\n=== MONITORING MODE ===\n\n")
    
    # Continuous price monitoring
    cat("Press Ctrl+C to stop monitoring\n\n")
    
    while (TRUE) {
      prices <- get_multiple_prices(CRYPTOCURRENCIES)
      
      if (!is.null(prices)) {
        cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
        print(prices[, c("cryptocurrency", "price")])
        cat("\n")
      }
      
      Sys.sleep(DATA_INTERVALS$real_time_refresh)
    }
    
  } else {
    cat("Unknown mode:", mode, "\n")
    cat("Available modes: demo, collect_data, monitor\n")
  }
}

# ============================================================================
# EXECUTION
# ============================================================================

# Default: Run in demo mode
# Change the mode parameter to run different operations:
# - "demo": Quick demonstration of all features
# - "collect_data": Download historical data
# - "monitor": Continuous price monitoring

cat("\nStarting bot...\n")

tryCatch({
  run_bot(mode = "demo")
  
  log_info("=== Bot Completed Successfully ===")
  
}, error = function(e) {
  log_error(paste("Bot error:", e$message))
  cat("\n✗ Error occurred:", e$message, "\n")
  
}, finally = {
  cat("\nLog file:", log_file, "\n")
  cat("\nBot stopped.\n")
})

# ============================================================================
# USAGE EXAMPLES
# ============================================================================

# Uncomment to run different modes:

# Run demo (default)
# run_bot(mode = "demo")

# Collect 30 days of historical data
# run_bot(mode = "collect_data")

# Start continuous monitoring
# run_bot(mode = "monitor")

# Custom operations:
# btc_price <- get_current_price("BTC")
# eth_history <- get_historical_days("ETH", limit = 90)
# market_data <- get_full_market_data(c("BTC", "ETH", "SOL"))