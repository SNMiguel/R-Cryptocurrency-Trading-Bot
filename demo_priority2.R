# ============================================================================
# Crypto Trading Bot - Priority 2 Demo
# ============================================================================
# Demonstration of technical indicators, strategies, and backtesting

cat("\n")
cat("========================================\n")
cat("  PRIORITY 2 DEMO\n")
cat("  Indicators & Strategies\n")
cat("========================================\n")
cat("\n")

# ============================================================================
# SETUP
# ============================================================================

# Load configuration
source("config/config.R")

# Load required packages
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(logger)
library(TTR)

# Setup logging
log_file <- file.path(LOGS_DIR, paste0("demo_p2_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_appender(appender_file(log_file))
log_threshold(LOG_LEVEL)

log_info("=== Priority 2 Demo Starting ===")

# Load modules
cat("Loading modules...\n")
source("src/data_acquisition/api_client.R")
source("src/data_acquisition/historical.R")
source("src/indicators/technical.R")
source("src/strategies/base_strategy.R")
source("src/strategies/ma_crossover.R")
source("src/strategies/rsi_strategy.R")
source("src/backtesting/backtest_engine.R")

cat("\n")

# ============================================================================
# STEP 1: FETCH HISTORICAL DATA
# ============================================================================

cat("=== STEP 1: Fetching Historical Data ===\n\n")

# Get 90 days of Bitcoin data
cat("Downloading 90 days of BTC daily data...\n")
btc_data <- get_historical_days("BTC", "USD", limit = 90)

if (is.null(btc_data)) {
  stop("Failed to fetch historical data")
}

cat(sprintf("✓ Downloaded %d days of data\n", nrow(btc_data)))
cat(sprintf("  Date range: %s to %s\n", 
           format(min(btc_data$timestamp), "%Y-%m-%d"),
           format(max(btc_data$timestamp), "%Y-%m-%d")))
cat(sprintf("  Price range: $%.2f - $%.2f\n", 
           min(btc_data$close), max(btc_data$close)))
cat("\n")

# ============================================================================
# STEP 2: ADD TECHNICAL INDICATORS
# ============================================================================

cat("=== STEP 2: Calculating Technical Indicators ===\n\n")

# Add all indicators
btc_data <- add_all_indicators(
  btc_data,
  ma_periods = c(10, 20, 50),
  rsi_period = 14,
  macd_params = list(fast = 12, slow = 26, signal = 9),
  bb_params = list(n = 20, sd = 2)
)

cat("✓ All indicators calculated\n\n")

# Show current indicators
cat("Current Indicator Values (Latest Data Point):\n")
latest <- tail(btc_data, 1)
cat(sprintf("  Price:           $%.2f\n", latest$close))
cat(sprintf("  SMA(10):         $%.2f\n", latest$sma_10))
cat(sprintf("  SMA(20):         $%.2f\n", latest$sma_20))
cat(sprintf("  SMA(50):         $%.2f\n", latest$sma_50))
cat(sprintf("  RSI(14):         %.2f\n", latest$rsi))
cat(sprintf("  MACD:            %.2f\n", latest$macd))
cat(sprintf("  MACD Signal:     %.2f\n", latest$macd_signal))
cat(sprintf("  BB Upper:        $%.2f\n", latest$bb_upper))
cat(sprintf("  BB Lower:        $%.2f\n", latest$bb_lower))
cat("\n")

# ============================================================================
# STEP 3: RUN MOVING AVERAGE CROSSOVER STRATEGY
# ============================================================================

cat("=== STEP 3: Testing MA Crossover Strategy ===\n\n")

cat("Strategy: Buy when SMA(10) crosses above SMA(20)\n")
cat("          Sell when SMA(10) crosses below SMA(20)\n\n")

# Run backtest
ma_results <- run_backtest(
  strategy = create_ma_crossover_strategy(fast_period = 10, slow_period = 20),
  data = btc_data,
  initial_capital = 10000,
  commission = 0.001,  # 0.1% commission
  slippage = 0.0005    # 0.05% slippage
)

# Print results
print_backtest_results(ma_results)

# Save results
save_backtest_results(ma_results, "ma_crossover_demo")

# ============================================================================
# STEP 4: RUN RSI MEAN REVERSION STRATEGY
# ============================================================================

cat("=== STEP 4: Testing RSI Mean Reversion Strategy ===\n\n")

cat("Strategy: Buy when RSI < 30 (oversold)\n")
cat("          Sell when RSI > 70 (overbought)\n\n")

# Run backtest
rsi_results <- run_backtest(
  strategy = create_rsi_strategy(rsi_period = 14, oversold = 30, overbought = 70),
  data = btc_data,
  initial_capital = 10000,
  commission = 0.001,
  slippage = 0.0005
)

# Print results
print_backtest_results(rsi_results)

# Save results
save_backtest_results(rsi_results, "rsi_strategy_demo")

# ============================================================================
# STEP 5: COMPARE STRATEGIES
# ============================================================================

cat("=== STEP 5: Strategy Comparison ===\n\n")

# Create multiple strategies to compare
strategies <- list(
  create_ma_crossover_strategy(10, 20, "SMA"),
  create_ma_crossover_strategy(20, 50, "SMA"),
  create_rsi_strategy(14, 30, 70),
  create_rsi_strategy(14, 20, 80)
)

# Compare all strategies
comparison <- compare_strategies(strategies, btc_data, initial_capital = 10000)

cat("\n")

# ============================================================================
# STEP 6: OPTIMIZATION EXAMPLE (Optional - takes longer)
# ============================================================================

cat("=== STEP 6: Strategy Optimization (Optional) ===\n\n")
cat("Skipping optimization to keep demo fast.\n")
cat("To run optimization, uncomment the code in demo_priority2.R\n\n")

# Uncomment to run optimization:
# cat("Optimizing MA Crossover parameters...\n")
# optimization_results <- optimize_ma_crossover(
#   btc_data,
#   fast_range = c(5, 10, 15),
#   slow_range = c(20, 30, 50),
#   ma_type = "SMA",
#   initial_capital = 10000
# )

# ============================================================================
# SUMMARY
# ============================================================================

cat("========================================\n")
cat("  DEMO COMPLETE!\n")
cat("========================================\n\n")

cat("What You Just Did:\n")
cat("  ✓ Downloaded 90 days of BTC historical data\n")
cat("  ✓ Calculated technical indicators (MA, RSI, MACD, BB)\n")
cat("  ✓ Backtested MA Crossover strategy\n")
cat("  ✓ Backtested RSI Mean Reversion strategy\n")
cat("  ✓ Compared multiple strategies\n")
cat("  ✓ Saved backtest results\n\n")

cat("Next Steps:\n")
cat("  1. Try different cryptocurrencies (ETH, SOL, etc.)\n")
cat("  2. Adjust strategy parameters\n")
cat("  3. Create your own custom strategy\n")
cat("  4. Run optimizations to find best parameters\n")
cat("  5. Test on different time periods\n\n")

cat("Log file:", log_file, "\n")
cat("Results saved in: results/backtest_results/\n\n")

log_info("=== Priority 2 Demo Completed Successfully ===")