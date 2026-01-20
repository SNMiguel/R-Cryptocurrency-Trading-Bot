# ============================================================================
# Crypto Trading Bot - RSI Mean Reversion Strategy
# ============================================================================
# Strategy: Buy when RSI is oversold, sell when RSI is overbought

library(tidyverse)
library(logger)

# Source dependencies (will be loaded by main script)
# source("base_strategy.R")
# source("../indicators/technical.R")

# ============================================================================
# RSI STRATEGY
# ============================================================================

#' Create RSI Strategy
#' 
#' @param rsi_period Period for RSI calculation
#' @param oversold RSI oversold threshold (buy signal)
#' @param overbought RSI overbought threshold (sell signal)
#' @return Strategy object
create_rsi_strategy <- function(rsi_period = 14,
                               oversold = 30,
                               overbought = 70) {
  
  strategy <- create_base_strategy(
    name = "RSI Mean Reversion",
    description = paste0("Buy when RSI < ", oversold, ", sell when RSI > ", overbought),
    parameters = list(
      rsi_period = rsi_period,
      oversold = oversold,
      overbought = overbought,
      position_size = 0.95
    )
  )
  
  class(strategy) <- c("RSIStrategy", "TradingStrategy", "list")
  
  return(strategy)
}


#' Generate Signals for RSI Strategy
#' 
#' @param strategy RSI strategy object
#' @param data Data frame with OHLCV data
#' @return Data frame with signals added
generate_signals.RSIStrategy <- function(strategy, data) {
  
  params <- strategy$parameters
  
  # Validate data
  if (!validate_data(data)) {
    return(data)
  }
  
  log_info(paste("Generating RSI signals with period", params$rsi_period,
                "oversold:", params$oversold, "overbought:", params$overbought))
  
  # Add RSI if not present
  if (!"rsi" %in% names(data)) {
    data <- add_rsi(data, n = params$rsi_period)
  }
  
  # Initialize signals
  data$signal <- "HOLD"
  data$signal_strength <- 0
  
  # Generate signals based on RSI
  for (i in 1:nrow(data)) {
    rsi_val <- data$rsi[i]
    
    if (is.na(rsi_val)) next
    
    # Oversold - buy signal
    if (rsi_val <= params$oversold) {
      data$signal[i] <- "BUY"
      # Stronger signal the more oversold
      data$signal_strength[i] <- (params$oversold - rsi_val) / params$oversold
      log_debug(paste("BUY signal at", data$timestamp[i], "RSI:", rsi_val))
    }
    
    # Overbought - sell signal
    else if (rsi_val >= params$overbought) {
      data$signal[i] <- "SELL"
      # Stronger signal the more overbought
      data$signal_strength[i] <- -(rsi_val - params$overbought) / (100 - params$overbought)
      log_debug(paste("SELL signal at", data$timestamp[i], "RSI:", rsi_val))
    }
  }
  
  # Count signals
  num_buy <- sum(data$signal == "BUY")
  num_sell <- sum(data$signal == "SELL")
  
  log_info(paste("Generated", num_buy, "BUY and", num_sell, "SELL signals"))
  
  return(data)
}


#' Run RSI Strategy Backtest
#' 
#' @param data Historical OHLCV data
#' @param rsi_period RSI period
#' @param oversold Oversold threshold
#' @param overbought Overbought threshold
#' @param initial_capital Starting capital
#' @return Backtest results
run_rsi_strategy <- function(data,
                             rsi_period = 14,
                             oversold = 30,
                             overbought = 70,
                             initial_capital = 10000) {
  
  log_info("Running RSI Mean Reversion backtest...")
  
  # Create strategy
  strategy <- create_rsi_strategy(rsi_period, oversold, overbought)
  
  # Execute strategy
  results <- execute_strategy(strategy, data, initial_capital)
  
  # Print results
  print_performance(results$performance)
  
  return(results)
}


#' Optimize RSI Strategy Parameters
#' 
#' @param data Historical data
#' @param rsi_periods Vector of RSI periods to test
#' @param oversold_levels Vector of oversold levels to test
#' @param overbought_levels Vector of overbought levels to test
#' @param initial_capital Starting capital
#' @return Data frame with optimization results
optimize_rsi_strategy <- function(data,
                                  rsi_periods = c(7, 14, 21),
                                  oversold_levels = c(20, 30, 40),
                                  overbought_levels = c(60, 70, 80),
                                  initial_capital = 10000) {
  
  log_info("Optimizing RSI strategy parameters...")
  
  results <- data.frame(
    rsi_period = integer(),
    oversold = integer(),
    overbought = integer(),
    total_return = numeric(),
    return_pct = numeric(),
    num_trades = integer(),
    win_rate = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Test all combinations
  for (period in rsi_periods) {
    for (os in oversold_levels) {
      for (ob in overbought_levels) {
        # Skip if oversold >= overbought
        if (os >= ob) next
        
        log_info(paste("Testing RSI period:", period, "OS:", os, "OB:", ob))
        
        # Run backtest
        backtest <- run_rsi_strategy(data, period, os, ob, initial_capital)
        
        # Store results
        results <- rbind(results, data.frame(
          rsi_period = period,
          oversold = os,
          overbought = ob,
          total_return = backtest$performance$total_return,
          return_pct = backtest$performance$total_return_pct,
          num_trades = backtest$performance$num_completed_trades,
          win_rate = backtest$performance$win_rate,
          stringsAsFactors = FALSE
        ))
      }
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

log_info("RSI Mean Reversion strategy loaded")

# Public functions:
# - create_rsi_strategy()
# - run_rsi_strategy()
# - optimize_rsi_strategy()