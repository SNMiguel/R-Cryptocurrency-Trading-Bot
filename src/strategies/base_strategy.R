# ============================================================================
# Crypto Trading Bot - Base Strategy Framework
# ============================================================================
# Template for creating trading strategies

library(tidyverse)
library(logger)

# ============================================================================
# BASE STRATEGY CLASS (R6 Style)
# ============================================================================

#' Base Strategy Template
#' 
#' This is a template for creating trading strategies.
#' All strategies should follow this structure.
#' 
#' @param name Strategy name
#' @param description Strategy description
#' @param parameters List of strategy parameters
#' @return Strategy object
create_base_strategy <- function(name = "Base Strategy",
                                description = "Template strategy",
                                parameters = list()) {
  
  strategy <- list(
    name = name,
    description = description,
    parameters = parameters,
    trades = list(),
    performance = list()
  )
  
  class(strategy) <- c("TradingStrategy", "list")
  
  return(strategy)
}


# ============================================================================
# STRATEGY METHODS
# ============================================================================

#' Generate Trading Signals
#' 
#' @param strategy Strategy object
#' @param data Data frame with OHLCV and indicator data
#' @return Data frame with 'signal' column added ("BUY", "SELL", "HOLD")
generate_signals <- function(strategy, data) {
  UseMethod("generate_signals")
}


#' Default signal generation (override this in specific strategies)
generate_signals.default <- function(strategy, data) {
  log_warn("Using default signal generation - override this in your strategy")
  
  # Default: hold everything
  data$signal <- "HOLD"
  data$signal_strength <- 0
  
  return(data)
}


#' Execute Strategy on Data
#' 
#' @param strategy Strategy object
#' @param data Data frame with OHLCV data
#' @param initial_capital Starting capital
#' @return List with trades and final portfolio
execute_strategy <- function(strategy, data, initial_capital = 10000) {
  log_info(paste("Executing strategy:", strategy$name))
  
  # Generate signals
  data <- generate_signals(strategy, data)
  
  # Simulate trades based on signals
  trades <- simulate_trades(data, initial_capital, strategy$parameters)
  
  # Calculate performance metrics
  performance <- calculate_performance(trades, initial_capital)
  
  result <- list(
    strategy_name = strategy$name,
    data = data,
    trades = trades,
    performance = performance,
    initial_capital = initial_capital
  )
  
  log_info(paste("Strategy execution complete:", nrow(trades$history), "trades"))
  
  return(result)
}


#' Simulate Trades Based on Signals
#' 
#' @param data Data with signals
#' @param initial_capital Starting capital
#' @param params Strategy parameters
#' @return List with trade history and portfolio state
simulate_trades <- function(data, initial_capital, params = list()) {
  
  # Initialize portfolio
  cash <- initial_capital
  position <- 0  # Number of units held
  position_value <- 0
  
  # Trade history
  trade_history <- data.frame(
    timestamp = as.POSIXct(character()),
    action = character(),
    price = numeric(),
    quantity = numeric(),
    cash_flow = numeric(),
    portfolio_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Position sizing parameters
  position_size_pct <- ifelse("position_size" %in% names(params), 
                               params$position_size, 0.95)
  
  # Iterate through data
  for (i in 1:nrow(data)) {
    signal <- data$signal[i]
    price <- data$close[i]
    timestamp <- data$timestamp[i]
    
    # Skip if signal is NA or price is NA
    if (is.na(signal) || is.na(price)) next
    
    # BUY signal
    if (signal == "BUY" && position == 0 && cash > 0) {
      # Buy with available cash (leave small buffer for fees)
      buy_amount <- cash * position_size_pct
      quantity <- buy_amount / price
      
      position <- quantity
      cash <- cash - buy_amount
      
      trade_history <- rbind(trade_history, data.frame(
        timestamp = timestamp,
        action = "BUY",
        price = price,
        quantity = quantity,
        cash_flow = -buy_amount,
        portfolio_value = cash + (position * price),
        stringsAsFactors = FALSE
      ))
      
      log_debug(paste("BUY:", quantity, "units at", price))
    }
    
    # SELL signal
    else if (signal == "SELL" && position > 0) {
      # Sell entire position
      sell_amount <- position * price
      
      cash <- cash + sell_amount
      
      trade_history <- rbind(trade_history, data.frame(
        timestamp = timestamp,
        action = "SELL",
        price = price,
        quantity = position,
        cash_flow = sell_amount,
        portfolio_value = cash,
        stringsAsFactors = FALSE
      ))
      
      log_debug(paste("SELL:", position, "units at", price))
      
      position <- 0
    }
  }
  
  # Calculate final portfolio value
  final_price <- tail(data$close, 1)
  final_value <- cash + (position * final_price)
  
  result <- list(
    history = trade_history,
    final_cash = cash,
    final_position = position,
    final_value = final_value
  )
  
  return(result)
}


#' Calculate Performance Metrics
#' 
#' @param trades Trade simulation results
#' @param initial_capital Starting capital
#' @return List of performance metrics
calculate_performance <- function(trades, initial_capital) {
  
  if (nrow(trades$history) == 0) {
    log_warn("No trades executed")
    return(list(
      total_return = 0,
      total_return_pct = 0,
      num_trades = 0,
      win_rate = 0,
      profit_factor = 0
    ))
  }
  
  # Total return
  total_return <- trades$final_value - initial_capital
  total_return_pct <- (total_return / initial_capital) * 100
  
  # Number of trades
  num_trades <- nrow(trades$history)
  
  # Calculate individual trade profits
  buy_trades <- trades$history[trades$history$action == "BUY", ]
  sell_trades <- trades$history[trades$history$action == "SELL", ]
  
  num_completed_trades <- min(nrow(buy_trades), nrow(sell_trades))
  
  if (num_completed_trades > 0) {
    trade_profits <- numeric(num_completed_trades)
    
    for (i in 1:num_completed_trades) {
      buy_price <- buy_trades$price[i]
      sell_price <- sell_trades$price[i]
      quantity <- buy_trades$quantity[i]
      
      trade_profits[i] <- (sell_price - buy_price) * quantity
    }
    
    # Win rate
    winning_trades <- sum(trade_profits > 0)
    win_rate <- (winning_trades / num_completed_trades) * 100
    
    # Profit factor
    gross_profit <- sum(trade_profits[trade_profits > 0])
    gross_loss <- abs(sum(trade_profits[trade_profits < 0]))
    profit_factor <- ifelse(gross_loss > 0, gross_profit / gross_loss, NA)
    
    # Average trade
    avg_trade <- mean(trade_profits)
    
  } else {
    win_rate <- 0
    profit_factor <- 0
    avg_trade <- 0
  }
  
  # Compile metrics
  performance <- list(
    initial_capital = initial_capital,
    final_value = trades$final_value,
    total_return = total_return,
    total_return_pct = total_return_pct,
    num_trades = num_trades,
    num_completed_trades = num_completed_trades,
    win_rate = win_rate,
    profit_factor = profit_factor,
    avg_trade = avg_trade
  )
  
  return(performance)
}


#' Print Strategy Performance
#' 
#' @param performance Performance metrics list
print_performance <- function(performance) {
  cat("\n=== STRATEGY PERFORMANCE ===\n\n")
  cat(sprintf("Initial Capital:     $%.2f\n", performance$initial_capital))
  cat(sprintf("Final Value:         $%.2f\n", performance$final_value))
  cat(sprintf("Total Return:        $%.2f (%.2f%%)\n", 
              performance$total_return, performance$total_return_pct))
  cat(sprintf("Number of Trades:    %d\n", performance$num_trades))
  cat(sprintf("Completed Trades:    %d\n", performance$num_completed_trades))
  
  if (performance$num_completed_trades > 0) {
    cat(sprintf("Win Rate:            %.2f%%\n", performance$win_rate))
    cat(sprintf("Profit Factor:       %.2f\n", performance$profit_factor))
    cat(sprintf("Avg Trade:           $%.2f\n", performance$avg_trade))
  }
  
  cat("\n")
}


#' Validate Strategy Parameters
#' 
#' @param params List of parameters
#' @param required_params Vector of required parameter names
#' @return TRUE if valid, FALSE otherwise
validate_parameters <- function(params, required_params) {
  missing <- setdiff(required_params, names(params))
  
  if (length(missing) > 0) {
    log_error(paste("Missing required parameters:", paste(missing, collapse = ", ")))
    return(FALSE)
  }
  
  return(TRUE)
}


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Check if Data Has Required Columns
#' 
#' @param data Data frame
#' @param required_cols Vector of required column names
#' @return TRUE if valid, FALSE otherwise
validate_data <- function(data, required_cols = c("timestamp", "close")) {
  missing <- setdiff(required_cols, names(data))
  
  if (length(missing) > 0) {
    log_error(paste("Missing required columns:", paste(missing, collapse = ", ")))
    return(FALSE)
  }
  
  return(TRUE)
}


# ============================================================================
# INITIALIZATION
# ============================================================================

log_info("Base strategy framework loaded")

# Public functions:
# - create_base_strategy()
# - generate_signals()
# - execute_strategy()
# - simulate_trades()
# - calculate_performance()
# - print_performance()
# - validate_parameters()
# - validate_data()