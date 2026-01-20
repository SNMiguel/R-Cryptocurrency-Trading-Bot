# ============================================================================
# Crypto Trading Bot - Moving Average Crossover Strategy
# ============================================================================
# Strategy: Buy when fast MA crosses above slow MA, sell when it crosses below

library(tidyverse)
library(logger)

# Source dependencies (will be loaded by main script)
# source("base_strategy.R")
# source("../indicators/technical.R")

# ============================================================================
# MOVING AVERAGE CROSSOVER STRATEGY
# ============================================================================

#' Create MA Crossover Strategy
#' 
#' @param fast_period Period for fast moving average
#' @param slow_period Period for slow moving average
#' @param ma_type Type of MA ("SMA" or "EMA")
#' @return Strategy object
create_ma_crossover_strategy <- function(fast_period = 10,
                                        slow_period = 20,
                                        ma_type = "SMA") {
  
  strategy <- create_base_strategy(
    name = "Moving Average Crossover",
    description = paste0("Buy when ", ma_type, fast_period, " crosses above ", 
                        ma_type, slow_period, ", sell when it crosses below"),
    parameters = list(
      fast_period = fast_period,
      slow_period = slow_period,
      ma_type = ma_type,
      position_size = 0.95  # Use 95% of capital per trade
    )
  )
  
  class(strategy) <- c("MACrossoverStrategy", "TradingStrategy", "list")
  
  return(strategy)
}


#' Generate Signals for MA Crossover Strategy
#' 
#' @param strategy MA Crossover strategy object
#' @param data Data frame with OHLCV data
#' @return Data frame with signals added
generate_signals.MACrossoverStrategy <- function(strategy, data) {
  
  params <- strategy$parameters
  
  # Validate data
  if (!validate_data(data)) {
    return(data)
  }
  
  log_info(paste("Generating MA Crossover signals with", 
                params$ma_type, params$fast_period, "and", params$slow_period))
  
  # Add moving averages if not present
  fast_col <- paste0(tolower(params$ma_type), "_", params$fast_period)
  slow_col <- paste0(tolower(params$ma_type), "_", params$slow_period)
  
  if (!fast_col %in% names(data)) {
    data <- add_moving_averages(data, 
                               periods = c(params$fast_period), 
                               type = params$ma_type)
  }
  
  if (!slow_col %in% names(data)) {
    data <- add_moving_averages(data, 
                               periods = c(params$slow_period), 
                               type = params$ma_type)
  }
  
  # Initialize signals
  data$signal <- "HOLD"
  data$signal_strength <- 0
  
  # Get MA columns
  fast_ma <- data[[fast_col]]
  slow_ma <- data[[slow_col]]
  
  # Generate crossover signals
  for (i in 2:nrow(data)) {
    # Skip if MA values are NA
    if (is.na(fast_ma[i]) || is.na(slow_ma[i]) || 
        is.na(fast_ma[i-1]) || is.na(slow_ma[i-1])) {
      next
    }
    
    # Bullish crossover: fast MA crosses above slow MA
    if (fast_ma[i-1] <= slow_ma[i-1] && fast_ma[i] > slow_ma[i]) {
      data$signal[i] <- "BUY"
      data$signal_strength[i] <- 1
      log_debug(paste("BUY signal at", data$timestamp[i]))
    }
    
    # Bearish crossover: fast MA crosses below slow MA
    else if (fast_ma[i-1] >= slow_ma[i-1] && fast_ma[i] < slow_ma[i]) {
      data$signal[i] <- "SELL"
      data$signal_strength[i] <- -1
      log_debug(paste("SELL signal at", data$timestamp[i]))
    }
  }
  
  # Count signals
  num_buy <- sum(data$signal == "BUY")
  num_sell <- sum(data$signal == "SELL")
  
  log_info(paste("Generated", num_buy, "BUY and", num_sell, "SELL signals"))
  
  return(data)
}


#' Run MA Crossover Strategy Backtest
#' 
#' @param data Historical OHLCV data
#' @param fast_period Fast MA period
#' @param slow_period Slow MA period
#' @param ma_type Type of MA
#' @param initial_capital Starting capital
#' @return Backtest results
run_ma_crossover <- function(data, 
                             fast_period = 10,
                             slow_period = 20,
                             ma_type = "SMA",
                             initial_capital = 10000) {
  
  log_info("Running Moving Average Crossover backtest...")
  
  # Create strategy
  strategy <- create_ma_crossover_strategy(fast_period, slow_period, ma_type)
  
  # Execute strategy
  results <- execute_strategy(strategy, data, initial_capital)
  
  # Print results
  print_performance(results$performance)
  
  return(results)
}


# ============================================================================
# OPTIMIZATION FUNCTIONS
# ============================================================================

#' Optimize MA Crossover Parameters
#' 
#' @param data Historical data
#' @param fast_range Range of fast MA periods to test
#' @param slow_range Range of slow MA periods to test
#' @param ma_type Type of MA
#' @param initial_capital Starting capital
#' @return Data frame with optimization results
optimize_ma_crossover <- function(data,
                                  fast_range = c(5, 10, 15, 20),
                                  slow_range = c(20, 30, 50, 100),
                                  ma_type = "SMA",
                                  initial_capital = 10000) {
  
  log_info("Optimizing MA Crossover parameters...")
  
  results <- data.frame(
    fast_period = integer(),
    slow_period = integer(),
    total_return = numeric(),
    return_pct = numeric(),
    num_trades = integer(),
    win_rate = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Test all combinations
  for (fast in fast_range) {
    for (slow in slow_range) {
      # Skip if fast >= slow
      if (fast >= slow) next
      
      log_info(paste("Testing fast:", fast, "slow:", slow))
      
      # Run backtest
      backtest <- run_ma_crossover(data, fast, slow, ma_type, initial_capital)
      
      # Store results
      results <- rbind(results, data.frame(
        fast_period = fast,
        slow_period = slow,
        total_return = backtest$performance$total_return,
        return_pct = backtest$performance$total_return_pct,
        num_trades = backtest$performance$num_completed_trades,
        win_rate = backtest$performance$win_rate,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Sort by return
  results <- results %>% arrange(desc(return_pct))
  
  log_info("Optimization complete")
  
  cat("\n=== TOP 5 PARAMETER COMBINATIONS ===\n")
  print(head(results, 5))
  
  return(results)
}


# ============================================================================
# INITIALIZATION
# ============================================================================

log_info("Moving Average Crossover strategy loaded")

# Public functions:
# - create_ma_crossover_strategy()
# - run_ma_crossover()
# - optimize_ma_crossover()