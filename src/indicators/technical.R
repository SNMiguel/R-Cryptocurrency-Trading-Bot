# ============================================================================
# Crypto Trading Bot - Technical Indicators
# ============================================================================
# Functions for calculating technical indicators (RSI, MACD, MA, BB, etc.)

library(TTR)
library(tidyverse)
library(logger)

# ============================================================================
# MOVING AVERAGES
# ============================================================================

#' Calculate Simple Moving Average (SMA)
#' 
#' @param prices Vector of prices
#' @param n Period for moving average
#' @return Vector of SMA values
calculate_sma <- function(prices, n = 20) {
  if (length(prices) < n) {
    log_warn(paste("Insufficient data for SMA calculation. Need", n, "points, have", length(prices)))
    return(rep(NA, length(prices)))
  }
  
  sma <- SMA(prices, n = n)
  return(sma)
}


#' Calculate Exponential Moving Average (EMA)
#' 
#' @param prices Vector of prices
#' @param n Period for moving average
#' @return Vector of EMA values
calculate_ema <- function(prices, n = 20) {
  if (length(prices) < n) {
    log_warn(paste("Insufficient data for EMA calculation. Need", n, "points, have", length(prices)))
    return(rep(NA, length(prices)))
  }
  
  ema <- EMA(prices, n = n)
  return(ema)
}


#' Add Multiple Moving Averages to Data
#' 
#' @param data Data frame with OHLCV data
#' @param periods Vector of periods for MAs
#' @param type "SMA" or "EMA"
#' @return Data frame with MA columns added
add_moving_averages <- function(data, periods = c(10, 20, 50, 200), type = "SMA") {
  if (!"close" %in% names(data)) {
    log_error("Data must contain 'close' column")
    return(data)
  }
  
  for (period in periods) {
    col_name <- paste0(tolower(type), "_", period)
    
    if (type == "SMA") {
      data[[col_name]] <- calculate_sma(data$close, n = period)
    } else if (type == "EMA") {
      data[[col_name]] <- calculate_ema(data$close, n = period)
    }
    
    log_info(paste("Added", type, "with period", period))
  }
  
  return(data)
}


# ============================================================================
# RELATIVE STRENGTH INDEX (RSI)
# ============================================================================

#' Calculate RSI
#' 
#' @param prices Vector of prices
#' @param n Period for RSI calculation (default: 14)
#' @return Vector of RSI values (0-100)
calculate_rsi <- function(prices, n = 14) {
  if (length(prices) < n + 1) {
    log_warn(paste("Insufficient data for RSI calculation. Need", n + 1, "points, have", length(prices)))
    return(rep(NA, length(prices)))
  }
  
  rsi <- RSI(prices, n = n)
  return(rsi)
}


#' Add RSI to Data
#' 
#' @param data Data frame with OHLCV data
#' @param n Period for RSI
#' @return Data frame with RSI column added
add_rsi <- function(data, n = 14) {
  if (!"close" %in% names(data)) {
    log_error("Data must contain 'close' column")
    return(data)
  }
  
  data$rsi <- calculate_rsi(data$close, n = n)
  log_info(paste("Added RSI with period", n))
  
  return(data)
}


#' Get RSI Signal
#' 
#' @param rsi RSI value
#' @param oversold Oversold threshold (default: 30)
#' @param overbought Overbought threshold (default: 70)
#' @return "BUY", "SELL", or "NEUTRAL"
get_rsi_signal <- function(rsi, oversold = 30, overbought = 70) {
  if (is.na(rsi)) return("NEUTRAL")
  
  if (rsi <= oversold) {
    return("BUY")  # Oversold - potential buy signal
  } else if (rsi >= overbought) {
    return("SELL")  # Overbought - potential sell signal
  } else {
    return("NEUTRAL")
  }
}


# ============================================================================
# MACD (Moving Average Convergence Divergence)
# ============================================================================

#' Calculate MACD
#' 
#' @param prices Vector of prices
#' @param n_fast Fast EMA period (default: 12)
#' @param n_slow Slow EMA period (default: 26)
#' @param n_signal Signal line period (default: 9)
#' @return Data frame with MACD, signal, and histogram
calculate_macd <- function(prices, n_fast = 12, n_slow = 26, n_signal = 9) {
  if (length(prices) < n_slow + n_signal) {
    log_warn(paste("Insufficient data for MACD calculation"))
    return(data.frame(
      macd = rep(NA, length(prices)),
      signal = rep(NA, length(prices)),
      histogram = rep(NA, length(prices))
    ))
  }
  
  macd_result <- MACD(prices, nFast = n_fast, nSlow = n_slow, nSig = n_signal)
  
  result <- data.frame(
    macd = macd_result[, "macd"],
    signal = macd_result[, "signal"],
    histogram = macd_result[, "macd"] - macd_result[, "signal"]
  )
  
  return(result)
}


#' Add MACD to Data
#' 
#' @param data Data frame with OHLCV data
#' @param n_fast Fast EMA period
#' @param n_slow Slow EMA period
#' @param n_signal Signal line period
#' @return Data frame with MACD columns added
add_macd <- function(data, n_fast = 12, n_slow = 26, n_signal = 9) {
  if (!"close" %in% names(data)) {
    log_error("Data must contain 'close' column")
    return(data)
  }
  
  macd_data <- calculate_macd(data$close, n_fast, n_slow, n_signal)
  
  data$macd <- macd_data$macd
  data$macd_signal <- macd_data$signal
  data$macd_histogram <- macd_data$histogram
  
  log_info(paste("Added MACD with periods", n_fast, n_slow, n_signal))
  
  return(data)
}


#' Get MACD Signal
#' 
#' @param macd MACD line value
#' @param signal Signal line value
#' @param prev_macd Previous MACD value
#' @param prev_signal Previous signal value
#' @return "BUY", "SELL", or "NEUTRAL"
get_macd_signal <- function(macd, signal, prev_macd = NULL, prev_signal = NULL) {
  if (is.na(macd) || is.na(signal)) return("NEUTRAL")
  
  # Bullish crossover: MACD crosses above signal
  if (!is.null(prev_macd) && !is.null(prev_signal)) {
    if (prev_macd <= prev_signal && macd > signal) {
      return("BUY")
    }
    # Bearish crossover: MACD crosses below signal
    if (prev_macd >= prev_signal && macd < signal) {
      return("SELL")
    }
  }
  
  # Simple position-based signal
  if (macd > signal) {
    return("BUY")
  } else if (macd < signal) {
    return("SELL")
  }
  
  return("NEUTRAL")
}


# ============================================================================
# BOLLINGER BANDS
# ============================================================================

#' Calculate Bollinger Bands
#' 
#' @param prices Vector of prices
#' @param n Period for moving average (default: 20)
#' @param sd Number of standard deviations (default: 2)
#' @return Data frame with upper, middle, and lower bands
calculate_bollinger_bands <- function(prices, n = 20, sd = 2) {
  if (length(prices) < n) {
    log_warn(paste("Insufficient data for Bollinger Bands calculation"))
    return(data.frame(
      bb_upper = rep(NA, length(prices)),
      bb_middle = rep(NA, length(prices)),
      bb_lower = rep(NA, length(prices))
    ))
  }
  
  bb <- BBands(prices, n = n, sd = sd)
  
  result <- data.frame(
    bb_upper = bb[, "up"],
    bb_middle = bb[, "mavg"],
    bb_lower = bb[, "dn"],
    bb_pctb = bb[, "pctB"]  # Percent B indicator
  )
  
  return(result)
}


#' Add Bollinger Bands to Data
#' 
#' @param data Data frame with OHLCV data
#' @param n Period for moving average
#' @param sd Number of standard deviations
#' @return Data frame with BB columns added
add_bollinger_bands <- function(data, n = 20, sd = 2) {
  if (!"close" %in% names(data)) {
    log_error("Data must contain 'close' column")
    return(data)
  }
  
  bb_data <- calculate_bollinger_bands(data$close, n, sd)
  
  data$bb_upper <- bb_data$bb_upper
  data$bb_middle <- bb_data$bb_middle
  data$bb_lower <- bb_data$bb_lower
  data$bb_pctb <- bb_data$bb_pctb
  
  log_info(paste("Added Bollinger Bands with period", n, "and", sd, "std dev"))
  
  return(data)
}


#' Get Bollinger Bands Signal
#' 
#' @param price Current price
#' @param bb_upper Upper band
#' @param bb_lower Lower band
#' @return "BUY", "SELL", or "NEUTRAL"
get_bb_signal <- function(price, bb_upper, bb_lower) {
  if (is.na(price) || is.na(bb_upper) || is.na(bb_lower)) return("NEUTRAL")
  
  # Price near or below lower band - oversold
  if (price <= bb_lower) {
    return("BUY")
  }
  # Price near or above upper band - overbought
  else if (price >= bb_upper) {
    return("SELL")
  }
  
  return("NEUTRAL")
}


# ============================================================================
# VOLUME INDICATORS
# ============================================================================

#' Calculate Volume Moving Average
#' 
#' @param volume Vector of volume data
#' @param n Period for moving average
#' @return Vector of volume MA values
calculate_volume_ma <- function(volume, n = 20) {
  if (length(volume) < n) {
    log_warn(paste("Insufficient data for Volume MA calculation"))
    return(rep(NA, length(volume)))
  }
  
  volume_ma <- SMA(volume, n = n)
  return(volume_ma)
}


#' Add Volume Indicators to Data
#' 
#' @param data Data frame with OHLCV data
#' @param n Period for volume MA
#' @return Data frame with volume indicators added
add_volume_indicators <- function(data, n = 20) {
  if (!"volume_from" %in% names(data)) {
    log_warn("Data does not contain 'volume_from' column")
    return(data)
  }
  
  data$volume_ma <- calculate_volume_ma(data$volume_from, n = n)
  data$volume_ratio <- data$volume_from / data$volume_ma
  
  log_info(paste("Added volume indicators with period", n))
  
  return(data)
}


# ============================================================================
# COMPREHENSIVE INDICATOR FUNCTION
# ============================================================================

#' Add All Technical Indicators to Data
#' 
#' @param data Data frame with OHLCV data
#' @param ma_periods Periods for moving averages
#' @param rsi_period Period for RSI
#' @param macd_params List with fast, slow, signal periods
#' @param bb_params List with period and sd
#' @return Data frame with all indicators added
add_all_indicators <- function(data, 
                              ma_periods = c(10, 20, 50),
                              rsi_period = 14,
                              macd_params = list(fast = 12, slow = 26, signal = 9),
                              bb_params = list(n = 20, sd = 2)) {
  
  log_info("Adding all technical indicators...")
  
  # Add moving averages
  data <- add_moving_averages(data, periods = ma_periods, type = "SMA")
  data <- add_moving_averages(data, periods = ma_periods, type = "EMA")
  
  # Add RSI
  data <- add_rsi(data, n = rsi_period)
  
  # Add MACD
  data <- add_macd(data, 
                  n_fast = macd_params$fast, 
                  n_slow = macd_params$slow, 
                  n_signal = macd_params$signal)
  
  # Add Bollinger Bands
  data <- add_bollinger_bands(data, n = bb_params$n, sd = bb_params$sd)
  
  # Add volume indicators
  data <- add_volume_indicators(data)
  
  log_info("All technical indicators added successfully")
  
  return(data)
}


# ============================================================================
# INITIALIZATION
# ============================================================================

log_info("Technical indicators module loaded")

# Public functions:
# - calculate_sma(), calculate_ema(), add_moving_averages()
# - calculate_rsi(), add_rsi(), get_rsi_signal()
# - calculate_macd(), add_macd(), get_macd_signal()
# - calculate_bollinger_bands(), add_bollinger_bands(), get_bb_signal()
# - calculate_volume_ma(), add_volume_indicators()
# - add_all_indicators()