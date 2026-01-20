# ============================================================================
# Crypto Trading Bot - Risk Management
# ============================================================================
# Functions for position sizing, stop-loss, risk calculations

library(tidyverse)
library(logger)

# ============================================================================
# POSITION SIZING
# ============================================================================

#' Calculate Position Size (Fixed Percentage)
#' 
#' @param capital Available capital
#' @param risk_pct Percentage of capital to risk per trade
#' @return Position size in dollars
calculate_position_size_fixed <- function(capital, risk_pct = 0.02) {
  position_size <- capital * risk_pct
  log_debug(paste("Fixed position size:", position_size, "for capital:", capital))
  return(position_size)
}


#' Calculate Position Size (Kelly Criterion)
#' 
#' @param capital Available capital
#' @param win_rate Historical win rate (0-1)
#' @param avg_win Average winning trade
#' @param avg_loss Average losing trade (positive number)
#' @param max_kelly_fraction Maximum fraction of Kelly to use (default: 0.5)
#' @return Position size in dollars
calculate_position_size_kelly <- function(capital, 
                                         win_rate, 
                                         avg_win, 
                                         avg_loss,
                                         max_kelly_fraction = 0.5) {
  
  if (avg_loss == 0 || is.na(win_rate) || win_rate == 0) {
    log_warn("Invalid parameters for Kelly Criterion, using 2% fixed")
    return(capital * 0.02)
  }
  
  # Kelly formula: f = (p * b - q) / b
  # where p = win rate, q = loss rate, b = win/loss ratio
  win_loss_ratio <- avg_win / avg_loss
  loss_rate <- 1 - win_rate
  
  kelly_fraction <- (win_rate * win_loss_ratio - loss_rate) / win_loss_ratio
  
  # Apply safety cap (typically 50% of Kelly)
  kelly_fraction <- min(kelly_fraction, max_kelly_fraction)
  kelly_fraction <- max(kelly_fraction, 0)  # Never negative
  
  position_size <- capital * kelly_fraction
  
  log_debug(paste("Kelly position size:", position_size, 
                 "Kelly%:", kelly_fraction * 100))
  
  return(position_size)
}


#' Calculate Position Size with ATR (Average True Range)
#' 
#' @param capital Available capital
#' @param risk_pct Risk percentage per trade
#' @param atr Average True Range value
#' @param price Current price
#' @param atr_multiplier ATR multiplier for stop distance
#' @return Number of units to buy
calculate_position_size_atr <- function(capital,
                                       risk_pct,
                                       atr,
                                       price,
                                       atr_multiplier = 2) {
  
  risk_amount <- capital * risk_pct
  stop_distance <- atr * atr_multiplier
  
  # Number of units = Risk Amount / Stop Distance
  units <- risk_amount / stop_distance
  
  position_value <- units * price
  
  log_debug(paste("ATR position sizing:",
                 "Units:", units,
                 "Value:", position_value,
                 "Stop:", stop_distance))
  
  return(list(
    units = units,
    position_value = position_value,
    stop_distance = stop_distance
  ))
}


# ============================================================================
# STOP-LOSS AND TAKE-PROFIT
# ============================================================================

#' Calculate Stop-Loss Price
#' 
#' @param entry_price Entry price
#' @param stop_pct Stop-loss percentage (e.g., 0.02 for 2%)
#' @param direction "LONG" or "SHORT"
#' @return Stop-loss price
calculate_stop_loss <- function(entry_price, stop_pct = 0.02, direction = "LONG") {
  
  if (direction == "LONG") {
    stop_price <- entry_price * (1 - stop_pct)
  } else {
    stop_price <- entry_price * (1 + stop_pct)
  }
  
  log_debug(paste("Stop-loss:", stop_price, "for entry:", entry_price, direction))
  
  return(stop_price)
}


#' Calculate Take-Profit Price
#' 
#' @param entry_price Entry price
#' @param profit_pct Take-profit percentage
#' @param direction "LONG" or "SHORT"
#' @return Take-profit price
calculate_take_profit <- function(entry_price, profit_pct = 0.05, direction = "LONG") {
  
  if (direction == "LONG") {
    target_price <- entry_price * (1 + profit_pct)
  } else {
    target_price <- entry_price * (1 - profit_pct)
  }
  
  log_debug(paste("Take-profit:", target_price, "for entry:", entry_price, direction))
  
  return(target_price)
}


#' Calculate Risk-Reward Ratio
#' 
#' @param entry_price Entry price
#' @param stop_loss Stop-loss price
#' @param take_profit Take-profit price
#' @return Risk-reward ratio
calculate_risk_reward_ratio <- function(entry_price, stop_loss, take_profit) {
  
  risk <- abs(entry_price - stop_loss)
  reward <- abs(take_profit - entry_price)
  
  if (risk == 0) {
    log_warn("Risk is zero, cannot calculate R:R ratio")
    return(NA)
  }
  
  rr_ratio <- reward / risk
  
  log_debug(paste("Risk-Reward Ratio:", round(rr_ratio, 2), ":1"))
  
  return(rr_ratio)
}


#' Check if Stop-Loss Hit
#' 
#' @param current_price Current price
#' @param stop_loss Stop-loss price
#' @param direction "LONG" or "SHORT"
#' @return TRUE if stop hit, FALSE otherwise
check_stop_loss <- function(current_price, stop_loss, direction = "LONG") {
  
  if (direction == "LONG") {
    hit <- current_price <= stop_loss
  } else {
    hit <- current_price >= stop_loss
  }
  
  if (hit) {
    log_info(paste("STOP-LOSS HIT:", current_price, "vs", stop_loss))
  }
  
  return(hit)
}


#' Check if Take-Profit Hit
#' 
#' @param current_price Current price
#' @param take_profit Take-profit price
#' @param direction "LONG" or "SHORT"
#' @return TRUE if target hit, FALSE otherwise
check_take_profit <- function(current_price, take_profit, direction = "LONG") {
  
  if (direction == "LONG") {
    hit <- current_price >= take_profit
  } else {
    hit <- current_price <= take_profit
  }
  
  if (hit) {
    log_info(paste("TAKE-PROFIT HIT:", current_price, "vs", take_profit))
  }
  
  return(hit)
}


# ============================================================================
# PORTFOLIO RISK METRICS
# ============================================================================

#' Calculate Portfolio Risk
#' 
#' @param positions List of open positions
#' @param total_capital Total portfolio capital
#' @return Portfolio risk metrics
calculate_portfolio_risk <- function(positions, total_capital) {
  
  if (length(positions) == 0) {
    return(list(
      total_exposure = 0,
      total_risk = 0,
      portfolio_risk_pct = 0,
      num_positions = 0
    ))
  }
  
  total_exposure <- sum(sapply(positions, function(p) p$position_value))
  total_risk <- sum(sapply(positions, function(p) p$risk_amount))
  
  portfolio_risk_pct <- (total_risk / total_capital) * 100
  
  metrics <- list(
    total_exposure = total_exposure,
    total_risk = total_risk,
    portfolio_risk_pct = portfolio_risk_pct,
    num_positions = length(positions),
    leverage = total_exposure / total_capital
  )
  
  log_info(paste("Portfolio Risk:", round(portfolio_risk_pct, 2), "%",
                "Positions:", length(positions),
                "Leverage:", round(metrics$leverage, 2), "x"))
  
  return(metrics)
}


#' Calculate Maximum Position Size Based on Portfolio Risk
#' 
#' @param capital Available capital
#' @param max_portfolio_risk Maximum portfolio risk percentage
#' @param current_positions List of current positions
#' @return Maximum allowed position size
calculate_max_position_size <- function(capital, 
                                       max_portfolio_risk = 0.10,
                                       current_positions = list()) {
  
  # Calculate current portfolio risk
  current_risk <- calculate_portfolio_risk(current_positions, capital)
  
  # Remaining risk capacity
  remaining_risk_pct <- max_portfolio_risk - (current_risk$portfolio_risk_pct / 100)
  remaining_risk_pct <- max(0, remaining_risk_pct)
  
  max_position <- capital * remaining_risk_pct
  
  log_debug(paste("Max position size:", max_position,
                 "Remaining risk:", remaining_risk_pct * 100, "%"))
  
  return(max_position)
}


#' Check if Risk Limits Exceeded
#' 
#' @param proposed_position Proposed new position
#' @param current_positions Current open positions
#' @param capital Total capital
#' @param max_risk Maximum portfolio risk percentage
#' @return TRUE if within limits, FALSE if exceeded
check_risk_limits <- function(proposed_position,
                              current_positions,
                              capital,
                              max_risk = 0.10) {
  
  # Add proposed position to current
  all_positions <- c(current_positions, list(proposed_position))
  
  # Calculate new portfolio risk
  new_risk <- calculate_portfolio_risk(all_positions, capital)
  
  within_limits <- new_risk$portfolio_risk_pct <= (max_risk * 100)
  
  if (!within_limits) {
    log_warn(paste("Risk limit exceeded:",
                  round(new_risk$portfolio_risk_pct, 2), "% >",
                  max_risk * 100, "%"))
  }
  
  return(within_limits)
}


# ============================================================================
# TRAILING STOP
# ============================================================================

#' Calculate Trailing Stop
#' 
#' @param entry_price Entry price
#' @param current_price Current price
#' @param current_stop Current stop-loss price
#' @param trail_pct Trailing percentage
#' @param direction "LONG" or "SHORT"
#' @return New stop-loss price
calculate_trailing_stop <- function(entry_price,
                                   current_price,
                                   current_stop,
                                   trail_pct = 0.02,
                                   direction = "LONG") {
  
  if (direction == "LONG") {
    # For long positions, trail stop upwards
    new_stop <- current_price * (1 - trail_pct)
    # Never lower the stop
    new_stop <- max(new_stop, current_stop)
  } else {
    # For short positions, trail stop downwards
    new_stop <- current_price * (1 + trail_pct)
    # Never raise the stop
    new_stop <- min(new_stop, current_stop)
  }
  
  if (new_stop != current_stop) {
    log_info(paste("Trailing stop updated:", current_stop, "->", new_stop))
  }
  
  return(new_stop)
}


# ============================================================================
# RISK REPORTING
# ============================================================================

#' Generate Risk Report
#' 
#' @param positions Current positions
#' @param capital Total capital
#' @param max_risk_pct Maximum risk percentage
#' @return Risk report
generate_risk_report <- function(positions, capital, max_risk_pct = 0.10) {
  
  portfolio_risk <- calculate_portfolio_risk(positions, capital)
  
  cat("\n========================================\n")
  cat("  RISK MANAGEMENT REPORT\n")
  cat("========================================\n\n")
  
  cat("Portfolio Capital:    $", format(capital, big.mark = ","), "\n", sep = "")
  cat("Number of Positions:  ", portfolio_risk$num_positions, "\n", sep = "")
  cat("Total Exposure:       $", format(round(portfolio_risk$total_exposure, 2), big.mark = ","), "\n", sep = "")
  cat("Total Risk Amount:    $", format(round(portfolio_risk$total_risk, 2), big.mark = ","), "\n", sep = "")
  cat("Portfolio Risk:       ", round(portfolio_risk$portfolio_risk_pct, 2), "%\n", sep = "")
  cat("Max Risk Allowed:     ", max_risk_pct * 100, "%\n", sep = "")
  cat("Risk Utilization:     ", round((portfolio_risk$portfolio_risk_pct / (max_risk_pct * 100)) * 100, 1), "%\n", sep = "")
  cat("Leverage:             ", round(portfolio_risk$leverage, 2), "x\n", sep = "")
  
  if (portfolio_risk$portfolio_risk_pct > max_risk_pct * 100) {
    cat("\n⚠️  WARNING: Portfolio risk exceeds maximum!\n")
  } else {
    cat("\n✓ Portfolio risk within limits\n")
  }
  
  cat("\n")
  
  return(portfolio_risk)
}


# ============================================================================
# INITIALIZATION
# ============================================================================

log_info("Risk management module loaded")

# Public functions:
# - calculate_position_size_fixed()
# - calculate_position_size_kelly()
# - calculate_position_size_atr()
# - calculate_stop_loss()
# - calculate_take_profit()
# - calculate_risk_reward_ratio()
# - check_stop_loss()
# - check_take_profit()
# - calculate_portfolio_risk()
# - calculate_max_position_size()
# - check_risk_limits()
# - calculate_trailing_stop()
# - generate_risk_report()