# ============================================================================
# Crypto Trading Bot - Paper Trading Simulator
# ============================================================================
# Simulate live trading without real money

library(tidyverse)
library(logger)
library(lubridate)

# Source dependencies
# source("../risk_management/position_sizing.R")

# ============================================================================
# PAPER TRADING PORTFOLIO
# ============================================================================

#' Initialize Paper Trading Portfolio
#' 
#' @param initial_capital Starting capital
#' @return Portfolio object
create_paper_portfolio <- function(initial_capital = 10000) {
  
  portfolio <- list(
    initial_capital = initial_capital,
    cash = initial_capital,
    positions = list(),
    trade_history = data.frame(
      timestamp = as.POSIXct(character()),
      symbol = character(),
      action = character(),
      quantity = numeric(),
      price = numeric(),
      value = numeric(),
      cash_after = numeric(),
      portfolio_value = numeric(),
      stringsAsFactors = FALSE
    ),
    performance = list(
      total_trades = 0,
      winning_trades = 0,
      losing_trades = 0,
      total_profit = 0,
      total_loss = 0
    )
  )
  
  class(portfolio) <- c("PaperPortfolio", "list")
  
  log_info(paste("Paper portfolio created with $", initial_capital))
  
  return(portfolio)
}


#' Get Current Portfolio Value
#' 
#' @param portfolio Portfolio object
#' @param current_prices Named list of current prices
#' @return Total portfolio value
get_portfolio_value <- function(portfolio, current_prices) {
  
  position_value <- 0
  
  if (length(portfolio$positions) > 0) {
    for (symbol in names(portfolio$positions)) {
      pos <- portfolio$positions[[symbol]]
      if (symbol %in% names(current_prices)) {
        position_value <- position_value + (pos$quantity * current_prices[[symbol]])
      }
    }
  }
  
  total_value <- portfolio$cash + position_value
  
  return(total_value)
}


# ============================================================================
# POSITION MANAGEMENT
# ============================================================================

#' Open Position (Buy)
#' 
#' @param portfolio Portfolio object
#' @param symbol Cryptocurrency symbol
#' @param quantity Quantity to buy
#' @param price Current price
#' @param stop_loss Optional stop-loss price
#' @param take_profit Optional take-profit price
#' @return Updated portfolio
open_position <- function(portfolio, 
                         symbol, 
                         quantity, 
                         price,
                         stop_loss = NULL,
                         take_profit = NULL) {
  
  cost <- quantity * price
  
  # Check if enough cash
  if (cost > portfolio$cash) {
    log_warn(paste("Insufficient cash for", symbol, "trade. Need:", cost, "Have:", portfolio$cash))
    return(portfolio)
  }
  
  # Check if position already exists
  if (symbol %in% names(portfolio$positions)) {
    log_warn(paste("Position already open for", symbol))
    return(portfolio)
  }
  
  # Create position
  portfolio$positions[[symbol]] <- list(
    symbol = symbol,
    quantity = quantity,
    entry_price = price,
    entry_time = Sys.time(),
    stop_loss = stop_loss,
    take_profit = take_profit,
    current_value = cost,
    unrealized_pnl = 0
  )
  
  # Update cash
  portfolio$cash <- portfolio$cash - cost
  
  # Log trade
  portfolio$trade_history <- rbind(portfolio$trade_history, data.frame(
    timestamp = Sys.time(),
    symbol = symbol,
    action = "BUY",
    quantity = quantity,
    price = price,
    value = cost,
    cash_after = portfolio$cash,
    portfolio_value = portfolio$cash + cost,
    stringsAsFactors = FALSE
  ))
  
  portfolio$performance$total_trades <- portfolio$performance$total_trades + 1
  
  log_info(paste("OPENED position:", symbol, quantity, "units @", price, "Cost:", cost))
  
  return(portfolio)
}


#' Close Position (Sell)
#' 
#' @param portfolio Portfolio object
#' @param symbol Cryptocurrency symbol
#' @param price Current price
#' @param reason Reason for closing (optional)
#' @return Updated portfolio
close_position <- function(portfolio, symbol, price, reason = "Manual") {
  
  # Check if position exists
  if (!symbol %in% names(portfolio$positions)) {
    log_warn(paste("No open position for", symbol))
    return(portfolio)
  }
  
  position <- portfolio$positions[[symbol]]
  
  # Calculate proceeds
  proceeds <- position$quantity * price
  profit <- proceeds - (position$quantity * position$entry_price)
  
  # Update cash
  portfolio$cash <- portfolio$cash + proceeds
  
  # Log trade
  portfolio$trade_history <- rbind(portfolio$trade_history, data.frame(
    timestamp = Sys.time(),
    symbol = symbol,
    action = "SELL",
    quantity = position$quantity,
    price = price,
    value = proceeds,
    cash_after = portfolio$cash,
    portfolio_value = get_portfolio_value(portfolio, setNames(list(price), symbol)),
    stringsAsFactors = FALSE
  ))
  
  # Update performance
  portfolio$performance$total_trades <- portfolio$performance$total_trades + 1
  
  if (profit > 0) {
    portfolio$performance$winning_trades <- portfolio$performance$winning_trades + 1
    portfolio$performance$total_profit <- portfolio$performance$total_profit + profit
  } else {
    portfolio$performance$losing_trades <- portfolio$performance$losing_trades + 1
    portfolio$performance$total_loss <- portfolio$performance$total_loss + abs(profit)
  }
  
  # Remove position
  portfolio$positions[[symbol]] <- NULL
  
  log_info(paste("CLOSED position:", symbol, "@", price, "P/L:", round(profit, 2), "Reason:", reason))
  
  return(portfolio)
}


#' Update Position with Current Price
#' 
#' @param portfolio Portfolio object
#' @param symbol Cryptocurrency symbol
#' @param current_price Current price
#' @return Updated portfolio
update_position <- function(portfolio, symbol, current_price) {
  
  if (!symbol %in% names(portfolio$positions)) {
    return(portfolio)
  }
  
  position <- portfolio$positions[[symbol]]
  
  # Update current value and unrealized P/L
  position$current_value <- position$quantity * current_price
  position$unrealized_pnl <- position$current_value - (position$quantity * position$entry_price)
  position$unrealized_pnl_pct <- (position$unrealized_pnl / (position$quantity * position$entry_price)) * 100
  
  # Check stop-loss
  if (!is.null(position$stop_loss) && current_price <= position$stop_loss) {
    log_info(paste("Stop-loss triggered for", symbol))
    portfolio <- close_position(portfolio, symbol, current_price, "Stop-Loss")
    return(portfolio)
  }
  
  # Check take-profit
  if (!is.null(position$take_profit) && current_price >= position$take_profit) {
    log_info(paste("Take-profit triggered for", symbol))
    portfolio <- close_position(portfolio, symbol, current_price, "Take-Profit")
    return(portfolio)
  }
  
  portfolio$positions[[symbol]] <- position
  
  return(portfolio)
}


# ============================================================================
# PAPER TRADING EXECUTION
# ============================================================================

#' Execute Paper Trading Session
#' 
#' @param strategy Strategy object
#' @param data Historical data for paper trading
#' @param initial_capital Starting capital
#' @param stop_loss_pct Stop-loss percentage
#' @param take_profit_pct Take-profit percentage
#' @return Paper trading results
run_paper_trading <- function(strategy,
                              data,
                              initial_capital = 10000,
                              stop_loss_pct = 0.02,
                              take_profit_pct = 0.05) {
  
  log_info("Starting paper trading session...")
  
  # Create portfolio
  portfolio <- create_paper_portfolio(initial_capital)
  
  # Generate signals
  data <- generate_signals(strategy, data)
  
  # Track portfolio value over time
  equity_curve <- data.frame(
    timestamp = data$timestamp,
    portfolio_value = initial_capital,
    stringsAsFactors = FALSE
  )
  
  # Execute trades
  for (i in 1:nrow(data)) {
    signal <- data$signal[i]
    price <- data$close[i]
    symbol <- unique(data$cryptocurrency)[1]
    
    if (is.na(signal) || is.na(price)) next
    
    # Get current portfolio value
    current_prices <- setNames(list(price), symbol)
    
    # Update existing positions
    if (symbol %in% names(portfolio$positions)) {
      portfolio <- update_position(portfolio, symbol, price)
    }
    
    # Execute new signals
    if (signal == "BUY" && !symbol %in% names(portfolio$positions)) {
      # Calculate position size (use 95% of available cash)
      position_value <- portfolio$cash * 0.95
      quantity <- position_value / price
      
      # Calculate stop-loss and take-profit
      stop_loss <- calculate_stop_loss(price, stop_loss_pct)
      take_profit <- calculate_take_profit(price, take_profit_pct)
      
      portfolio <- open_position(portfolio, symbol, quantity, price, stop_loss, take_profit)
      
    } else if (signal == "SELL" && symbol %in% names(portfolio$positions)) {
      portfolio <- close_position(portfolio, symbol, price, "Signal")
    }
    
    # Update equity curve
    equity_curve$portfolio_value[i] <- get_portfolio_value(portfolio, current_prices)
  }
  
  # Close any remaining positions at final price
  final_price <- tail(data$close, 1)
  if (length(portfolio$positions) > 0) {
    for (symbol in names(portfolio$positions)) {
      portfolio <- close_position(portfolio, symbol, final_price, "End of Session")
    }
  }
  
  # Calculate final performance
  final_value <- portfolio$cash
  total_return <- final_value - initial_capital
  total_return_pct <- (total_return / initial_capital) * 100
  
  results <- list(
    portfolio = portfolio,
    equity_curve = equity_curve,
    final_value = final_value,
    total_return = total_return,
    total_return_pct = total_return_pct,
    win_rate = ifelse(portfolio$performance$total_trades > 0,
                     (portfolio$performance$winning_trades / portfolio$performance$total_trades) * 100,
                     0)
  )
  
  log_info("Paper trading session complete")
  
  return(results)
}


#' Print Paper Trading Results
#' 
#' @param results Paper trading results
print_paper_results <- function(results) {
  
  cat("\n========================================\n")
  cat("  PAPER TRADING RESULTS\n")
  cat("========================================\n\n")
  
  cat("Initial Capital:      $", format(results$portfolio$initial_capital, big.mark = ","), "\n", sep = "")
  cat("Final Value:          $", format(round(results$final_value, 2), big.mark = ","), "\n", sep = "")
  cat("Total Return:         $", format(round(results$total_return, 2), big.mark = ","), 
      " (", round(results$total_return_pct, 2), "%)\n", sep = "")
  
  cat("\nTrading Activity:\n")
  cat("Total Trades:         ", results$portfolio$performance$total_trades, "\n", sep = "")
  cat("Winning Trades:       ", results$portfolio$performance$winning_trades, "\n", sep = "")
  cat("Losing Trades:        ", results$portfolio$performance$losing_trades, "\n", sep = "")
  cat("Win Rate:             ", round(results$win_rate, 2), "%\n", sep = "")
  
  if (results$portfolio$performance$total_trades > 0) {
    cat("Total Profit:         $", format(round(results$portfolio$performance$total_profit, 2), big.mark = ","), "\n", sep = "")
    cat("Total Loss:           $", format(round(results$portfolio$performance$total_loss, 2), big.mark = ","), "\n", sep = "")
  }
  
  cat("\nOpen Positions:       ", length(results$portfolio$positions), "\n", sep = "")
  
  cat("\n")
}


# ============================================================================
# INITIALIZATION
# ============================================================================

log_info("Paper trading module loaded")

# Public functions:
# - create_paper_portfolio()
# - get_portfolio_value()
# - open_position()
# - close_position()
# - update_position()
# - run_paper_trading()
# - print_paper_results()