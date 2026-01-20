# ============================================================================
# Crypto Trading Bot - Backtesting Engine
# ============================================================================
# Framework for testing trading strategies on historical data

library(tidyverse)
library(logger)
library(lubridate)

# ============================================================================
# BACKTESTING CORE FUNCTIONS
# ============================================================================

#' Run Comprehensive Backtest
#' 
#' @param strategy Strategy object
#' @param data Historical OHLCV data
#' @param initial_capital Starting capital
#' @param commission Commission rate (default: 0.001 = 0.1%)
#' @param slippage Slippage rate (default: 0.0005 = 0.05%)
#' @return Comprehensive backtest results
run_backtest <- function(strategy, 
                        data, 
                        initial_capital = 10000,
                        commission = 0.001,
                        slippage = 0.0005) {
  
  log_info(paste("Starting backtest for strategy:", strategy$name))
  log_info(paste("Data period:", min(data$timestamp), "to", max(data$timestamp)))
  log_info(paste("Initial capital: $", initial_capital))
  
  # Execute strategy
  results <- execute_strategy(strategy, data, initial_capital)
  
  # Apply transaction costs
  results <- apply_transaction_costs(results, commission, slippage)
  
  # Calculate detailed metrics
  results$metrics <- calculate_detailed_metrics(
    results$trades, 
    results$data, 
    initial_capital
  )
  
  # Generate equity curve
  results$equity_curve <- generate_equity_curve(results$trades, results$data, initial_capital)
  
  log_info("Backtest complete")
  
  return(results)
}


#' Apply Transaction Costs to Trades
#' 
#' @param results Strategy execution results
#' @param commission Commission rate
#' @param slippage Slippage rate
#' @return Updated results with costs applied
apply_transaction_costs <- function(results, commission, slippage) {
  
  if (nrow(results$trades$history) == 0) {
    return(results)
  }
  
  total_commission <- 0
  total_slippage <- 0
  
  for (i in 1:nrow(results$trades$history)) {
    trade <- results$trades$history[i, ]
    
    # Calculate commission
    comm_cost <- abs(trade$cash_flow) * commission
    total_commission <- total_commission + comm_cost
    
    # Calculate slippage
    slip_cost <- abs(trade$cash_flow) * slippage
    total_slippage <- total_slippage + slip_cost
    
    # Adjust portfolio value
    results$trades$history$portfolio_value[i] <- 
      results$trades$history$portfolio_value[i] - comm_cost - slip_cost
  }
  
  # Adjust final value
  results$trades$final_value <- results$trades$final_value - total_commission - total_slippage
  results$performance$total_return <- results$trades$final_value - results$performance$initial_capital
  results$performance$total_return_pct <- 
    (results$performance$total_return / results$performance$initial_capital) * 100
  
  # Store costs
  results$transaction_costs <- list(
    total_commission = total_commission,
    total_slippage = total_slippage,
    total_costs = total_commission + total_slippage
  )
  
  log_info(paste("Transaction costs applied: $", 
                round(results$transaction_costs$total_costs, 2)))
  
  return(results)
}


#' Calculate Detailed Performance Metrics
#' 
#' @param trades Trade history
#' @param data Full dataset with signals
#' @param initial_capital Starting capital
#' @return List of detailed metrics
calculate_detailed_metrics <- function(trades, data, initial_capital) {
  
  if (nrow(trades$history) == 0) {
    return(list(
      sharpe_ratio = 0,
      max_drawdown = 0,
      max_drawdown_pct = 0,
      profit_factor = 0,
      avg_win = 0,
      avg_loss = 0,
      largest_win = 0,
      largest_loss = 0
    ))
  }
  
  # Generate equity curve for calculations
  equity <- generate_equity_curve(trades, data, initial_capital)
  
  # Calculate returns
  if (nrow(equity) > 1) {
    returns <- diff(equity$portfolio_value) / head(equity$portfolio_value, -1)
    
    # Sharpe Ratio (annualized, assuming daily data)
    if (sd(returns) > 0) {
      sharpe_ratio <- (mean(returns) / sd(returns)) * sqrt(252)
    } else {
      sharpe_ratio <- 0
    }
    
    # Maximum Drawdown
    cummax_equity <- cummax(equity$portfolio_value)
    drawdown <- (equity$portfolio_value - cummax_equity) / cummax_equity
    max_drawdown <- min(drawdown) * 100  # as percentage
    max_drawdown_value <- min(equity$portfolio_value - cummax_equity)
    
  } else {
    sharpe_ratio <- 0
    max_drawdown <- 0
    max_drawdown_value <- 0
  }
  
  # Trade-by-trade analysis
  buy_trades <- trades$history[trades$history$action == "BUY", ]
  sell_trades <- trades$history[trades$history$action == "SELL", ]
  
  num_completed <- min(nrow(buy_trades), nrow(sell_trades))
  
  if (num_completed > 0) {
    trade_profits <- numeric(num_completed)
    
    for (i in 1:num_completed) {
      buy_price <- buy_trades$price[i]
      sell_price <- sell_trades$price[i]
      quantity <- buy_trades$quantity[i]
      
      trade_profits[i] <- (sell_price - buy_price) * quantity
    }
    
    winning_trades <- trade_profits[trade_profits > 0]
    losing_trades <- trade_profits[trade_profits < 0]
    
    avg_win <- ifelse(length(winning_trades) > 0, mean(winning_trades), 0)
    avg_loss <- ifelse(length(losing_trades) > 0, mean(losing_trades), 0)
    largest_win <- ifelse(length(winning_trades) > 0, max(winning_trades), 0)
    largest_loss <- ifelse(length(losing_trades) > 0, min(losing_trades), 0)
    
    # Profit Factor
    gross_profit <- sum(winning_trades)
    gross_loss <- abs(sum(losing_trades))
    profit_factor <- ifelse(gross_loss > 0, gross_profit / gross_loss, NA)
    
  } else {
    avg_win <- 0
    avg_loss <- 0
    largest_win <- 0
    largest_loss <- 0
    profit_factor <- 0
  }
  
  metrics <- list(
    sharpe_ratio = sharpe_ratio,
    max_drawdown = max_drawdown,
    max_drawdown_value = max_drawdown_value,
    profit_factor = profit_factor,
    avg_win = avg_win,
    avg_loss = avg_loss,
    largest_win = largest_win,
    largest_loss = largest_loss
  )
  
  return(metrics)
}


#' Generate Equity Curve
#' 
#' @param trades Trade history
#' @param data Full dataset
#' @param initial_capital Starting capital
#' @return Data frame with equity curve
generate_equity_curve <- function(trades, data, initial_capital) {
  
  equity_curve <- data.frame(
    timestamp = data$timestamp,
    portfolio_value = initial_capital
  )
  
  if (nrow(trades$history) == 0) {
    return(equity_curve)
  }
  
  # Update equity at each trade
  for (i in 1:nrow(trades$history)) {
    trade_time <- trades$history$timestamp[i]
    trade_value <- trades$history$portfolio_value[i]
    
    # Find matching timestamp in equity curve
    idx <- which(equity_curve$timestamp >= trade_time)[1]
    
    if (!is.na(idx)) {
      equity_curve$portfolio_value[idx:nrow(equity_curve)] <- trade_value
    }
  }
  
  return(equity_curve)
}


#' Compare Multiple Strategies
#' 
#' @param strategies List of strategy objects
#' @param data Historical data
#' @param initial_capital Starting capital
#' @return Comparison results
compare_strategies <- function(strategies, data, initial_capital = 10000) {
  
  log_info(paste("Comparing", length(strategies), "strategies..."))
  
  comparison <- data.frame(
    strategy_name = character(),
    total_return = numeric(),
    return_pct = numeric(),
    sharpe_ratio = numeric(),
    max_drawdown = numeric(),
    num_trades = integer(),
    win_rate = numeric(),
    profit_factor = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (strategy in strategies) {
    log_info(paste("Testing:", strategy$name))
    
    results <- run_backtest(strategy, data, initial_capital)
    
    # Handle missing metrics safely
    sharpe <- ifelse(is.null(results$metrics$sharpe_ratio), 0, results$metrics$sharpe_ratio)
    drawdown <- ifelse(is.null(results$metrics$max_drawdown), 0, results$metrics$max_drawdown)
    profit_factor <- ifelse(is.null(results$metrics$profit_factor), 0, results$metrics$profit_factor)
    win_rate <- ifelse(is.null(results$performance$win_rate), 0, results$performance$win_rate)
    
    comparison <- rbind(comparison, data.frame(
        strategy_name = strategy$name,
        total_return = results$performance$total_return,
        return_pct = results$performance$total_return_pct,
        sharpe_ratio = sharpe,
        max_drawdown = drawdown,
        num_trades = results$performance$num_completed_trades,
        win_rate = win_rate,
        profit_factor = profit_factor,
        stringsAsFactors = FALSE
    ))
    }
  
  # Sort by return
  comparison <- comparison %>% arrange(desc(return_pct))
  
  log_info("Strategy comparison complete")
  
  cat("\n=== STRATEGY COMPARISON ===\n")
  print(comparison)
  
  return(comparison)
}


#' Print Detailed Backtest Results
#' 
#' @param results Backtest results
print_backtest_results <- function(results) {
  
  cat("\n")
  cat("========================================\n")
  cat("  BACKTEST RESULTS\n")
  cat("========================================\n")
  cat("\nStrategy:", results$strategy_name, "\n")
  cat("\n--- PERFORMANCE METRICS ---\n")
  cat(sprintf("Initial Capital:     $%.2f\n", results$performance$initial_capital))
  cat(sprintf("Final Value:         $%.2f\n", results$trades$final_value))
  cat(sprintf("Total Return:        $%.2f (%.2f%%)\n", 
              results$performance$total_return, 
              results$performance$total_return_pct))
  
  cat("\n--- TRADING ACTIVITY ---\n")
  cat(sprintf("Total Trades:        %d\n", results$performance$num_trades))
  cat(sprintf("Completed Trades:    %d\n", results$performance$num_completed_trades))
  
  if (results$performance$num_completed_trades > 0) {
    cat(sprintf("Win Rate:            %.2f%%\n", results$performance$win_rate))
    cat(sprintf("Average Win:         $%.2f\n", results$metrics$avg_win))
    cat(sprintf("Average Loss:        $%.2f\n", results$metrics$avg_loss))
    cat(sprintf("Largest Win:         $%.2f\n", results$metrics$largest_win))
    cat(sprintf("Largest Loss:        $%.2f\n", results$metrics$largest_loss))
  }
  
  cat("\n--- RISK METRICS ---\n")
  cat(sprintf("Sharpe Ratio:        %.2f\n", results$metrics$sharpe_ratio))
  cat(sprintf("Max Drawdown:        %.2f%%\n", results$metrics$max_drawdown))
  cat(sprintf("Profit Factor:       %.2f\n", results$metrics$profit_factor))
  
  if ("transaction_costs" %in% names(results)) {
    cat("\n--- TRANSACTION COSTS ---\n")
    cat(sprintf("Total Commission:    $%.2f\n", results$transaction_costs$total_commission))
    cat(sprintf("Total Slippage:      $%.2f\n", results$transaction_costs$total_slippage))
    cat(sprintf("Total Costs:         $%.2f\n", results$transaction_costs$total_costs))
  }
  
  cat("\n")
}


#' Save Backtest Results
#' 
#' @param results Backtest results
#' @param filename Output filename (without extension)
save_backtest_results <- function(results, filename = NULL) {
  
  if (is.null(filename)) {
    filename <- paste0(
      gsub(" ", "_", tolower(results$strategy_name)),
      "_",
      format(Sys.time(), "%Y%m%d_%H%M%S")
    )
  }
  
  # Save to results directory
  results_dir <- file.path(getwd(), "results", "backtest_results")
  
  if (!dir.exists(results_dir)) {
    dir.create(results_dir, recursive = TRUE)
  }
  
  filepath <- file.path(results_dir, paste0(filename, ".rds"))
  saveRDS(results, filepath)
  
  log_info(paste("Backtest results saved to:", filepath))
  
  cat("\nResults saved to:", filepath, "\n")
  
  return(filepath)
}


# ============================================================================
# INITIALIZATION
# ============================================================================

log_info("Backtesting engine loaded")

# Public functions:
# - run_backtest()
# - compare_strategies()
# - print_backtest_results()
# - save_backtest_results()
# - generate_equity_curve()
# - calculate_detailed_metrics()