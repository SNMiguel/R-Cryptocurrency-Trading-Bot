# ============================================================================
# Crypto Trading Bot - Priority 3 Demo
# ============================================================================
# Demonstration of risk management, paper trading, and visualization

cat("\n")
cat("========================================\n")
cat("  PRIORITY 3 DEMO\n")
cat("  Risk, Paper Trading & Viz\n")
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
library(ggplot2)

# Setup logging
log_file <- file.path(LOGS_DIR, paste0("demo_p3_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_appender(appender_file(log_file))
log_threshold(LOG_LEVEL)

log_info("=== Priority 3 Demo Starting ===")

# Load modules
cat("Loading modules...\n")
source("src/data_acquisition/api_client.R")
source("src/data_acquisition/historical.R")
source("src/indicators/technical.R")
source("src/strategies/base_strategy.R")
source("src/strategies/ma_crossover.R")
source("src/strategies/rsi_strategy.R")
source("src/backtesting/backtest_engine.R")
source("src/risk_management/position_sizing.R")
source("src/trading/paper_trading.R")
source("src/visualization/plots.R")

cat("\n")

# ============================================================================
# STEP 1: GET DATA AND ADD INDICATORS
# ============================================================================

cat("=== STEP 1: Preparing Data ===\n\n")

# Get 90 days of BTC data
cat("Downloading BTC data...\n")
btc_data <- get_historical_days("BTC", "USD", limit = 90)
btc_data <- add_all_indicators(btc_data)

cat(sprintf("✓ Downloaded %d days of data\n", nrow(btc_data)))
cat("\n")

# ============================================================================
# STEP 2: RISK MANAGEMENT DEMO
# ============================================================================

cat("=== STEP 2: Risk Management ===\n\n")

# Position sizing examples
capital <- 10000
cat(sprintf("Portfolio Capital: $%s\n\n", format(capital, big.mark = ",")))

# Fixed percentage
cat("1. Fixed Percentage Position Sizing (2%):\n")
fixed_pos <- calculate_position_size_fixed(capital, 0.02)
cat(sprintf("   Position Size: $%.2f\n\n", fixed_pos))

# Kelly Criterion
cat("2. Kelly Criterion Position Sizing:\n")
kelly_pos <- calculate_position_size_kelly(
  capital = capital,
  win_rate = 0.55,
  avg_win = 500,
  avg_loss = 300,
  max_kelly_fraction = 0.5
)
cat(sprintf("   Position Size: $%.2f\n\n", kelly_pos))

# Stop-loss and take-profit
entry_price <- tail(btc_data$close, 1)
cat(sprintf("3. Entry Price: $%.2f\n", entry_price))

stop_loss <- calculate_stop_loss(entry_price, 0.02)
take_profit <- calculate_take_profit(entry_price, 0.05)
rr_ratio <- calculate_risk_reward_ratio(entry_price, stop_loss, take_profit)

cat(sprintf("   Stop-Loss:    $%.2f (-2%%)\n", stop_loss))
cat(sprintf("   Take-Profit:  $%.2f (+5%%)\n", take_profit))
cat(sprintf("   Risk:Reward:  %.2f:1\n\n", rr_ratio))

# Portfolio risk example
cat("4. Portfolio Risk Example:\n")
example_positions <- list(
  list(position_value = 5000, risk_amount = 100),
  list(position_value = 3000, risk_amount = 60)
)
portfolio_risk <- calculate_portfolio_risk(example_positions, capital)
cat(sprintf("   Total Exposure: $%.2f\n", portfolio_risk$total_exposure))
cat(sprintf("   Portfolio Risk: %.2f%%\n\n", portfolio_risk$portfolio_risk_pct))

# ============================================================================
# STEP 3: PAPER TRADING DEMO
# ============================================================================

cat("=== STEP 3: Paper Trading Simulation ===\n\n")

cat("Running RSI Mean Reversion strategy with paper trading...\n")

# Create strategy
strategy <- create_rsi_strategy(14, 30, 70)

# Run paper trading
paper_results <- run_paper_trading(
  strategy = strategy,
  data = btc_data,
  initial_capital = 10000,
  stop_loss_pct = 0.02,
  take_profit_pct = 0.05
)

# Print results
print_paper_results(paper_results)

# ============================================================================
# STEP 4: BACKTEST WITH VISUALIZATION
# ============================================================================

cat("=== STEP 4: Backtest & Visualization ===\n\n")

cat("Running MA Crossover backtest...\n")

# Run backtest
ma_strategy <- create_ma_crossover_strategy(10, 20)
ma_results <- run_backtest(ma_strategy, btc_data, initial_capital = 10000)

print_backtest_results(ma_results)

# ============================================================================
# STEP 5: CREATE VISUALIZATIONS
# ============================================================================

cat("=== STEP 5: Creating Visualizations ===\n\n")

# Create results/plots directory if it doesn't exist
plots_dir <- file.path(getwd(), "results", "plots")
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

cat("Generating charts...\n")

# 1. Equity Curve
cat("  1. Equity curve plot\n")
equity_plot <- plot_equity_curve(ma_results$equity_curve, "MA Crossover Equity Curve")
save_plot(equity_plot, "equity_curve_demo", width = 10, height = 6)

# 2. Drawdown
cat("  2. Drawdown analysis\n")
drawdown_plot <- plot_drawdown(ma_results$equity_curve, "Drawdown Analysis")
save_plot(drawdown_plot, "drawdown_demo", width = 10, height = 6)

# 3. Price with Indicators
cat("  3. Price with indicators\n")
price_plot <- plot_price_with_indicators(btc_data, show_ma = TRUE, show_bb = TRUE)
save_plot(price_plot, "price_indicators_demo", width = 12, height = 6)

# 4. RSI
cat("  4. RSI indicator\n")
rsi_plot <- plot_rsi(btc_data, oversold = 30, overbought = 70)
save_plot(rsi_plot, "rsi_demo", width = 10, height = 4)

# 5. MACD
cat("  5. MACD indicator\n")
macd_plot <- plot_macd(btc_data)
save_plot(macd_plot, "macd_demo", width = 10, height = 4)

# 6. Trades on Chart
cat("  6. Trades on price chart\n")
trades_plot <- plot_trades(btc_data, ma_results$trades$history, "MA Crossover Trades")
save_plot(trades_plot, "trades_demo", width = 12, height = 6)

# 7. Strategy Comparison
cat("  7. Strategy comparison\n")
strategies <- list(
  create_ma_crossover_strategy(10, 20),
  create_ma_crossover_strategy(20, 50),
  create_rsi_strategy(14, 30, 70)
)
comparison <- compare_strategies(strategies, btc_data)
comparison_plot <- plot_strategy_comparison(comparison, "return_pct")
save_plot(comparison_plot, "strategy_comparison_demo", width = 10, height = 6)

cat("\n✓ All charts saved to: results/plots/\n\n")

# ============================================================================
# STEP 6: COMPREHENSIVE RISK REPORT
# ============================================================================

cat("=== STEP 6: Risk Management Report ===\n")

# Create example portfolio with positions
example_portfolio <- list(
  list(
    symbol = "BTC",
    position_value = 5000,
    risk_amount = 100,
    entry_price = 90000,
    stop_loss = 88200
  ),
  list(
    symbol = "ETH",
    position_value = 3000,
    risk_amount = 60,
    entry_price = 3000,
    stop_loss = 2940
  )
)

generate_risk_report(example_portfolio, capital = 10000, max_risk_pct = 0.10)

# ============================================================================
# SUMMARY
# ============================================================================

cat("========================================\n")
cat("  DEMO COMPLETE!\n")
cat("========================================\n\n")

cat("What You Just Did:\n")
cat("  ✓ Calculated position sizes (Fixed, Kelly, ATR)\n")
cat("  ✓ Set stop-loss and take-profit levels\n")
cat("  ✓ Ran paper trading simulation\n")
cat("  ✓ Generated portfolio risk reports\n")
cat("  ✓ Created 7 professional charts\n")
cat("  ✓ Visualized strategy performance\n\n")

cat("Charts saved in: results/plots/\n")
cat("  - equity_curve_demo.png\n")
cat("  - drawdown_demo.png\n")
cat("  - price_indicators_demo.png\n")
cat("  - rsi_demo.png\n")
cat("  - macd_demo.png\n")
cat("  - trades_demo.png\n")
cat("  - strategy_comparison_demo.png\n\n")

cat("Next Steps:\n")
cat("  1. Test paper trading with real-time data\n")
cat("  2. Experiment with different risk parameters\n")
cat("  3. Create custom visualizations\n")
cat("  4. Build multi-asset portfolio strategies\n\n")

cat("Log file:", log_file, "\n\n")

log_info("=== Priority 3 Demo Completed Successfully ===")