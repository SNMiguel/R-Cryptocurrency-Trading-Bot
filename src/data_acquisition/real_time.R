# ============================================================================
# Crypto Trading Bot - Real-time Data Acquisition
# ============================================================================
# Functions for fetching real-time cryptocurrency prices

library(tidyverse)
library(logger)

# Source dependencies
# source(file.path(dirname(dirname(dirname(getwd()))), "config", "config.R"))
# source(file.path(dirname(getwd()), "api_client.R"))

# ============================================================================
# REAL-TIME PRICE FUNCTIONS
# ============================================================================

#' Get Current Price for Single Cryptocurrency
#' 
#' @param crypto Cryptocurrency symbol (e.g., "BTC")
#' @param currency Quote currency (e.g., "USD")
#' @return Numeric price value or NULL on failure
get_current_price <- function(crypto, currency = BASE_CURRENCY) {
  log_info(paste("Fetching current price for", crypto, "in", currency))
  
  # Make API call
  response <- make_api_call(
    endpoint = API_ENDPOINTS$price,
    params = list(
      fsym = crypto,
      tsyms = currency
    )
  )
  
  if (!validate_response(response)) {
    return(NULL)
  }
  
  price <- response[[currency]]
  log_info(paste(crypto, "price:", price, currency))
  
  return(price)
}


#' Get Current Prices for Multiple Cryptocurrencies
#' 
#' @param cryptos Vector of cryptocurrency symbols
#' @param currency Quote currency
#' @return Data frame with prices or NULL on failure
get_multiple_prices <- function(cryptos = CRYPTOCURRENCIES, currency = BASE_CURRENCY) {
  log_info(paste("Fetching prices for", length(cryptos), "cryptocurrencies"))
  
  # Make API call
  response <- make_api_call(
    endpoint = API_ENDPOINTS$price_multi,
    params = list(
      fsyms = paste(cryptos, collapse = ","),
      tsyms = currency
    )
  )
  
  if (!validate_response(response)) {
    return(NULL)
  }
  
  # Convert to data frame
  prices_df <- data.frame(
    timestamp = Sys.time(),
    cryptocurrency = names(response),
    price = unlist(response),
    currency = currency,
    stringsAsFactors = FALSE
  )
  
  rownames(prices_df) <- NULL
  
  log_info(paste("Successfully fetched", nrow(prices_df), "prices"))
  
  return(prices_df)
}


#' Get Full Market Data for Cryptocurrencies
#' 
#' @param cryptos Vector of cryptocurrency symbols
#' @param currency Quote currency
#' @return Data frame with comprehensive market data
get_full_market_data <- function(cryptos = CRYPTOCURRENCIES, currency = BASE_CURRENCY) {
  log_info(paste("Fetching full market data for", length(cryptos), "cryptocurrencies"))
  
  # Make API call
  response <- make_api_call(
    endpoint = API_ENDPOINTS$price_multi_full,
    params = list(
      fsyms = paste(cryptos, collapse = ","),
      tsyms = currency
    )
  )
  
  if (!validate_response(response)) {
    return(NULL)
  }
  
  # Extract RAW data (contains all market info)
  if ("RAW" %in% names(response)) {
    raw_data <- response$RAW
    
    # Convert to data frame
    market_data <- map_df(names(raw_data), function(crypto) {
      crypto_data <- raw_data[[crypto]][[currency]]
      
      data.frame(
        timestamp = parse_timestamp(crypto_data$LASTUPDATE),
        cryptocurrency = crypto,
        price = crypto_data$PRICE,
        open_24h = crypto_data$OPEN24HOUR,
        high_24h = crypto_data$HIGH24HOUR,
        low_24h = crypto_data$LOW24HOUR,
        volume_24h = crypto_data$VOLUME24HOUR,
        market_cap = crypto_data$MKTCAP,
        change_24h = crypto_data$CHANGE24HOUR,
        change_pct_24h = crypto_data$CHANGEPCT24HOUR,
        currency = currency,
        stringsAsFactors = FALSE
      )
    })
    
    log_info(paste("Successfully fetched full market data for", nrow(market_data), "cryptocurrencies"))
    
    return(market_data)
  } else {
    log_error("No RAW data in response")
    return(NULL)
  }
}


#' Monitor Real-time Prices
#' 
#' @param cryptos Vector of cryptocurrency symbols
#' @param currency Quote currency
#' @param duration_minutes How long to monitor (in minutes)
#' @param interval_seconds Refresh interval (in seconds)
#' @param save_data Whether to save collected data
#' @return Data frame with time series of prices
monitor_prices <- function(cryptos = CRYPTOCURRENCIES, 
                          currency = BASE_CURRENCY,
                          duration_minutes = 5,
                          interval_seconds = DATA_INTERVALS$real_time_refresh,
                          save_data = TRUE) {
  
  log_info(paste("Starting price monitoring for", duration_minutes, "minutes"))
  log_info(paste("Refresh interval:", interval_seconds, "seconds"))
  
  # Initialize storage
  all_prices <- list()
  start_time <- Sys.time()
  end_time <- start_time + duration_minutes * 60
  iteration <- 1
  
  # Monitoring loop
  while (Sys.time() < end_time) {
    log_info(paste("Iteration", iteration, "- Time:", format(Sys.time(), "%H:%M:%S")))
    
    # Fetch prices
    prices <- get_multiple_prices(cryptos, currency)
    
    if (!is.null(prices)) {
      all_prices[[iteration]] <- prices
      
      # Display current prices
      for (i in 1:nrow(prices)) {
        cat(sprintf("  %s: %s %.2f\n", 
                   prices$cryptocurrency[i], 
                   currency,
                   prices$price[i]))
      }
    }
    
    iteration <- iteration + 1
    
    # Wait for next interval
    Sys.sleep(interval_seconds)
  }
  
  # Combine all data
  if (length(all_prices) > 0) {
    price_history <- bind_rows(all_prices)
    
    log_info(paste("Monitoring complete. Collected", nrow(price_history), "data points"))
    
    # Save data if requested
    if (save_data) {
      filename <- file.path(
        dirname(dirname(dirname(getwd()))),
        "data", "raw",
        paste0("realtime_prices_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      )
      saveRDS(price_history, filename)
      log_info(paste("Data saved to:", filename))
    }
    
    return(price_history)
  } else {
    log_warn("No data collected during monitoring period")
    return(NULL)
  }
}


#' Get Price Change Statistics
#' 
#' @param crypto Cryptocurrency symbol
#' @param currency Quote currency
#' @return List with price change statistics
get_price_stats <- function(crypto, currency = BASE_CURRENCY) {
  log_info(paste("Fetching price statistics for", crypto))
  
  market_data <- get_full_market_data(crypto, currency)
  
  if (is.null(market_data)) {
    return(NULL)
  }
  
  stats <- list(
    cryptocurrency = crypto,
    current_price = market_data$price,
    change_24h = market_data$change_24h,
    change_pct_24h = market_data$change_pct_24h,
    high_24h = market_data$high_24h,
    low_24h = market_data$low_24h,
    volume_24h = market_data$volume_24h,
    market_cap = market_data$market_cap,
    timestamp = market_data$timestamp
  )
  
  return(stats)
}


# ============================================================================
# INITIALIZATION
# ============================================================================

log_info("Real-time data acquisition module loaded")

# Public functions:
# - get_current_price()
# - get_multiple_prices()
# - get_full_market_data()
# - monitor_prices()
# - get_price_stats()