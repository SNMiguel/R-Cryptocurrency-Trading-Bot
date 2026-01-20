# ============================================================================
# Crypto Trading Bot - Configuration File
# ============================================================================
# Central configuration for API endpoints, trading parameters, and settings

# ============================================================================
# API CONFIGURATION
# ============================================================================

# CryptoCompare API endpoints
API_BASE_URL <- "https://min-api.cryptocompare.com"

API_ENDPOINTS <- list(
  price = paste0(API_BASE_URL, "/data/price"),
  price_multi = paste0(API_BASE_URL, "/data/pricemulti"),
  price_multi_full = paste0(API_BASE_URL, "/data/pricemultifull"),
  historical_minute = paste0(API_BASE_URL, "/data/v2/histominute"),
  historical_hour = paste0(API_BASE_URL, "/data/v2/histohour"),
  historical_day = paste0(API_BASE_URL, "/data/v2/histoday")
)

# API rate limiting (free tier: ~100,000 calls/month, ~3,000 calls/day)
API_RATE_LIMIT <- list(
  calls_per_second = 1,
  calls_per_minute = 50,
  calls_per_hour = 2000
)

# API retry settings
API_RETRY <- list(
  max_attempts = 3,
  delay_seconds = 2
)

# ============================================================================
# PROJECT PATHS
# ============================================================================

# Get project root directory
PROJECT_ROOT <- getwd()

# Define directory paths
CONFIG_DIR <- file.path(PROJECT_ROOT, "config")
DATA_DIR <- file.path(PROJECT_ROOT, "data")
RAW_DATA_DIR <- file.path(DATA_DIR, "raw")
PROCESSED_DATA_DIR <- file.path(DATA_DIR, "processed")
SRC_DIR <- file.path(PROJECT_ROOT, "src")
LOGS_DIR <- file.path(PROJECT_ROOT, "logs")

# ============================================================================
# TRADING CONFIGURATION
# ============================================================================

# Cryptocurrencies to track
CRYPTOCURRENCIES <- c("BTC", "ETH", "BNB", "SOL", "ADA")

# Base currency for trading
BASE_CURRENCY <- "USD"

# Alternative quote currencies (optional)
QUOTE_CURRENCIES <- c("USD", "EUR", "GBP")

# Data collection intervals
DATA_INTERVALS <- list(
  real_time_refresh = 10,    # seconds
  historical_minute = 1,      # minute candles
  historical_hour = 1,        # hour candles
  historical_day = 1          # day candles
)

# Historical data limits
HISTORICAL_LIMITS <- list(
  minute = 2000,   # Max data points per API call
  hour = 2000,
  day = 2000
)

# ============================================================================
# TRADING PARAMETERS
# ============================================================================

# Trading mode
TRADING_MODE <- "PAPER"  # Options: "PAPER" (simulation) or "LIVE"

# Position sizing
POSITION_SIZING <- list(
  initial_capital = 10000,        # USD
  max_position_size = 0.10,       # 10% of capital per trade
  min_position_size = 100         # USD minimum
)

# Risk management
RISK_MANAGEMENT <- list(
  stop_loss_pct = 0.02,          # 2% stop loss
  take_profit_pct = 0.05,        # 5% take profit
  max_daily_loss_pct = 0.05,     # 5% max daily loss
  max_open_positions = 3
)

# ============================================================================
# DATA STORAGE
# ============================================================================

# Data file formats
DATA_FORMAT <- "rds"  # Options: "rds", "csv", "parquet"

# Data retention
DATA_RETENTION <- list(
  raw_data_days = 30,
  processed_data_days = 90,
  log_files_days = 30
)

# File naming conventions
FILE_NAMING <- list(
  raw_data = "{crypto}_{interval}_{date}",
  processed_data = "{crypto}_processed_{date}",
  log_file = "bot_log_{date}"
)

# ============================================================================
# LOGGING CONFIGURATION
# ============================================================================

# Logging level
LOG_LEVEL <- "INFO"  # Options: "DEBUG", "INFO", "WARN", "ERROR"

# Log to console
LOG_TO_CONSOLE <- TRUE

# Log to file
LOG_TO_FILE <- TRUE

# ============================================================================
# FEATURE FLAGS
# ============================================================================

FEATURES <- list(
  enable_real_time_data = TRUE,
  enable_historical_data = TRUE,
  enable_technical_indicators = FALSE,  # Will enable in Priority 2
  enable_backtesting = FALSE,           # Will enable in Priority 2
  enable_paper_trading = FALSE,         # Will enable later
  enable_live_trading = FALSE           # Will enable much later
)

# ============================================================================
# SYSTEM CONFIGURATION
# ============================================================================

# Timezone
TIMEZONE <- "UTC"

# Number of decimal places for prices
PRICE_DECIMALS <- 8

# Number of decimal places for calculations
CALC_DECIMALS <- 10

# ============================================================================
# HELPER FUNCTION
# ============================================================================

# Function to print current configuration
print_config <- function() {
  cat("\n=== Current Configuration ===\n")
  cat("Trading Mode:", TRADING_MODE, "\n")
  cat("Cryptocurrencies:", paste(CRYPTOCURRENCIES, collapse = ", "), "\n")
  cat("Base Currency:", BASE_CURRENCY, "\n")
  cat("Initial Capital:", POSITION_SIZING$initial_capital, BASE_CURRENCY, "\n")
  cat("Features Enabled:\n")
  for (feature in names(FEATURES)) {
    if (FEATURES[[feature]]) {
      cat("  ✓", feature, "\n")
    }
  }
  cat("\n")
}

cat("Configuration loaded successfully.\n")