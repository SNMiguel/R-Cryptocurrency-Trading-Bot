# ============================================================================
# Crypto Trading Bot - Historical Data Acquisition
# ============================================================================
# Functions for fetching historical cryptocurrency OHLCV data

library(tidyverse)
library(logger)
library(lubridate)

# Source dependencies
# source(file.path(dirname(dirname(dirname(getwd()))), "config", "config.R"))
# source(file.path(dirname(getwd()), "api_client.R"))

# ============================================================================
# HISTORICAL DATA FUNCTIONS
# ============================================================================

#' Get Historical Data (Generic)
#' 
#' @param crypto Cryptocurrency symbol
#' @param currency Quote currency
#' @param interval Time interval ("minute", "hour", "day")
#' @param limit Number of data points (max 2000)
#' @param to_timestamp End timestamp (default: current time)
#' @return Data frame with OHLCV data
get_historical_data <- function(crypto, 
                               currency = BASE_CURRENCY,
                               interval = "day",
                               limit = 100,
                               to_timestamp = NULL) {
  
  # Validate interval
  if (!interval %in% c("minute", "hour", "day")) {
    log_error("Invalid interval. Must be 'minute', 'hour', or 'day'")
    return(NULL)
  }
  
  # Validate limit
  max_limit <- HISTORICAL_LIMITS[[interval]]
  if (limit > max_limit) {
    log_warn(paste("Limit exceeds maximum of", max_limit, "- adjusting"))
    limit <- max_limit
  }
  
  log_info(paste("Fetching", limit, interval, "candles for", crypto))
  
  # Select appropriate endpoint
  endpoint <- API_ENDPOINTS[[paste0("historical_", interval)]]
  
  # Prepare parameters
  params <- list(
    fsym = crypto,
    tsym = currency,
    limit = limit
  )
  
  # Add timestamp if provided
  if (!is.null(to_timestamp)) {
    params$toTs <- as.numeric(as.POSIXct(to_timestamp))
  }
  
  # Make API call
  response <- make_api_call(endpoint, params)
  
  if (!validate_response(response)) {
    return(NULL)
  }
  
  # Extract data
  if ("Data" %in% names(response) && "Data" %in% names(response$Data)) {
    candles <- response$Data$Data
    
    # Convert to data frame
    historical_df <- data.frame(
      timestamp = parse_timestamp(candles$time),
      cryptocurrency = crypto,
      currency = currency,
      open = candles$open,
      high = candles$high,
      low = candles$low,
      close = candles$close,
      volume_from = candles$volumefrom,
      volume_to = candles$volumeto,
      interval = interval,
      stringsAsFactors = FALSE
    )
    
    log_info(paste("Successfully fetched", nrow(historical_df), "data points"))
    
    return(historical_df)
  } else {
    log_error("Unexpected response structure")
    return(NULL)
  }
}


#' Get Historical Minute Data
#' 
#' @param crypto Cryptocurrency symbol
#' @param currency Quote currency
#' @param limit Number of minutes (max 2000)
#' @return Data frame with minute candles
get_historical_minutes <- function(crypto, currency = BASE_CURRENCY, limit = 100) {
  get_historical_data(crypto, currency, interval = "minute", limit = limit)
}


#' Get Historical Hourly Data
#' 
#' @param crypto Cryptocurrency symbol
#' @param currency Quote currency
#' @param limit Number of hours (max 2000)
#' @return Data frame with hourly candles
get_historical_hours <- function(crypto, currency = BASE_CURRENCY, limit = 100) {
  get_historical_data(crypto, currency, interval = "hour", limit = limit)
}


#' Get Historical Daily Data
#' 
#' @param crypto Cryptocurrency symbol
#' @param currency Quote currency
#' @param limit Number of days (max 2000)
#' @return Data frame with daily candles
get_historical_days <- function(crypto, currency = BASE_CURRENCY, limit = 100) {
  get_historical_data(crypto, currency, interval = "day", limit = limit)
}


#' Get Historical Data for Date Range
#' 
#' @param crypto Cryptocurrency symbol
#' @param currency Quote currency
#' @param start_date Start date (Date or character)
#' @param end_date End date (Date or character, default: today)
#' @param interval Time interval
#' @return Data frame with OHLCV data
get_historical_range <- function(crypto, 
                                currency = BASE_CURRENCY,
                                start_date,
                                end_date = Sys.Date(),
                                interval = "day") {
  
  log_info(paste("Fetching data from", start_date, "to", end_date))
  
  # Convert dates to timestamps
  start_ts <- as.POSIXct(as.Date(start_date))
  end_ts <- as.POSIXct(as.Date(end_date))
  
  # Calculate required data points
  interval_seconds <- switch(interval,
    "minute" = 60,
    "hour" = 3600,
    "day" = 86400
  )
  
  required_points <- as.numeric(difftime(end_ts, start_ts, units = "secs")) / interval_seconds
  
  # Check if we need multiple API calls
  max_limit <- HISTORICAL_LIMITS[[interval]]
  
  if (required_points <= max_limit) {
    # Single API call sufficient
    return(get_historical_data(crypto, currency, interval, 
                              limit = ceiling(required_points), 
                              to_timestamp = end_date))
  } else {
    # Need multiple API calls
    log_info("Date range requires multiple API calls")
    
    all_data <- list()
    current_end <- end_ts
    remaining_points <- required_points
    call_count <- 1
    
    while (remaining_points > 0) {
      fetch_limit <- min(remaining_points, max_limit)
      
      log_info(paste("API call", call_count, "- fetching", fetch_limit, "points"))
      
      data_chunk <- get_historical_data(crypto, currency, interval,
                                       limit = fetch_limit,
                                       to_timestamp = current_end)
      
      if (!is.null(data_chunk)) {
        all_data[[call_count]] <- data_chunk
        
        # Move to earlier time period
        current_end <- min(data_chunk$timestamp) - interval_seconds
        remaining_points <- remaining_points - fetch_limit
        call_count <- call_count + 1
        
        # Rate limiting
        rate_limiter()
      } else {
        log_error("Failed to fetch data chunk")
        break
      }
    }
    
    if (length(all_data) > 0) {
      # Combine all chunks
      combined_data <- bind_rows(all_data) %>%
        arrange(timestamp) %>%
        filter(timestamp >= start_ts & timestamp <= end_ts) %>%
        distinct(timestamp, .keep_all = TRUE)
      
      log_info(paste("Total data points fetched:", nrow(combined_data)))
      
      return(combined_data)
    } else {
      return(NULL)
    }
  }
}


#' Download and Save Historical Data
#' 
#' @param cryptos Vector of cryptocurrency symbols
#' @param currency Quote currency
#' @param days Number of days to fetch
#' @param interval Time interval
#' @param save_format File format ("rds", "csv")
#' @return List of data frames
download_historical_data <- function(cryptos = CRYPTOCURRENCIES,
                                    currency = BASE_CURRENCY,
                                    days = 30,
                                    interval = "day",
                                    save_format = DATA_FORMAT) {
  
  log_info(paste("Downloading", days, "days of", interval, "data for", 
                length(cryptos), "cryptocurrencies"))
  
  all_data <- list()
  
  for (crypto in cryptos) {
    log_info(paste("Processing", crypto))
    
    # Fetch data
    data <- get_historical_data(crypto, currency, interval, limit = days)
    
    if (!is.null(data)) {
      all_data[[crypto]] <- data
      
      # Save data
      data_dir <- RAW_DATA_DIR
      filename <- paste0(crypto, "_", interval, "_", days, "days_", 
                        format(Sys.Date(), "%Y%m%d"))
      
      if (save_format == "rds") {
        filepath <- file.path(data_dir, paste0(filename, ".rds"))
        saveRDS(data, filepath)
      } else if (save_format == "csv") {
        filepath <- file.path(data_dir, paste0(filename, ".csv"))
        write.csv(data, filepath, row.names = FALSE)
      }
      
      log_info(paste("Saved:", filepath))
      
      # Rate limiting between cryptocurrencies
      rate_limiter()
    }
  }
  
  log_info(paste("Downloaded data for", length(all_data), "cryptocurrencies"))
  
  return(all_data)
}


#' Calculate Basic Statistics from Historical Data
#' 
#' @param historical_data Data frame with OHLCV data
#' @return List with summary statistics
calculate_stats <- function(historical_data) {
  if (is.null(historical_data) || nrow(historical_data) == 0) {
    return(NULL)
  }
  
  stats <- list(
    cryptocurrency = unique(historical_data$cryptocurrency),
    start_date = min(historical_data$timestamp),
    end_date = max(historical_data$timestamp),
    n_observations = nrow(historical_data),
    price_mean = mean(historical_data$close, na.rm = TRUE),
    price_median = median(historical_data$close, na.rm = TRUE),
    price_sd = sd(historical_data$close, na.rm = TRUE),
    price_min = min(historical_data$close, na.rm = TRUE),
    price_max = max(historical_data$close, na.rm = TRUE),
    total_volume = sum(historical_data$volume_from, na.rm = TRUE),
    avg_volume = mean(historical_data$volume_from, na.rm = TRUE)
  )
  
  return(stats)
}


# ============================================================================
# INITIALIZATION
# ============================================================================

log_info("Historical data acquisition module loaded")

# Public functions:
# - get_historical_data()
# - get_historical_minutes()
# - get_historical_hours()
# - get_historical_days()
# - get_historical_range()
# - download_historical_data()
# - calculate_stats()