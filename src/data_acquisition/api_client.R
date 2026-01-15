# ============================================================================
# Crypto Trading Bot - API Client
# ============================================================================
# Core functions for making API calls to CryptoCompare

library(httr)
library(jsonlite)
library(logger)

# Source configuration
source(file.path(dirname(dirname(dirname(getwd()))), "config", "config.R"))

# ============================================================================
# API CLIENT FUNCTIONS
# ============================================================================

#' Make API Call to CryptoCompare
#' 
#' @param endpoint API endpoint URL
#' @param params List of query parameters
#' @param max_retries Maximum number of retry attempts
#' @return Parsed JSON response or NULL on failure
make_api_call <- function(endpoint, params = list(), max_retries = API_RETRY$max_attempts) {
  
  attempt <- 1
  
  while (attempt <= max_retries) {
    tryCatch({
      # Log the API call
      log_debug(paste("API Call attempt", attempt, "to:", endpoint))
      
      # Make the GET request
      response <- GET(
        url = endpoint,
        query = params,
        timeout(30)
      )
      
      # Check if request was successful
      if (status_code(response) == 200) {
        # Parse JSON response
        content <- content(response, as = "text", encoding = "UTF-8")
        data <- fromJSON(content, flatten = TRUE)
        
        log_info(paste("API call successful:", endpoint))
        return(data)
        
      } else if (status_code(response) == 429) {
        # Rate limit exceeded
        log_warn(paste("Rate limit exceeded. Status:", status_code(response)))
        Sys.sleep(API_RETRY$delay_seconds * attempt)
        attempt <- attempt + 1
        
      } else {
        # Other HTTP errors
        log_error(paste("API call failed with status:", status_code(response)))
        log_error(paste("Response:", content(response, as = "text")))
        return(NULL)
      }
      
    }, error = function(e) {
      log_error(paste("Error in API call:", e$message))
      
      if (attempt < max_retries) {
        log_info(paste("Retrying in", API_RETRY$delay_seconds * attempt, "seconds..."))
        Sys.sleep(API_RETRY$delay_seconds * attempt)
        attempt <- attempt + 1
      } else {
        log_error("Max retry attempts reached. Giving up.")
        return(NULL)
      }
    })
  }
  
  return(NULL)
}


#' Check API Connection
#' 
#' @return TRUE if API is accessible, FALSE otherwise
check_api_connection <- function() {
  log_info("Checking API connection...")
  
  # Simple test call to get BTC price
  result <- make_api_call(
    endpoint = API_ENDPOINTS$price,
    params = list(fsym = "BTC", tsyms = "USD")
  )
  
  if (!is.null(result)) {
    log_info("✓ API connection successful")
    return(TRUE)
  } else {
    log_error("✗ API connection failed")
    return(FALSE)
  }
}


#' Validate API Response
#' 
#' @param response API response object
#' @return TRUE if valid, FALSE otherwise
validate_response <- function(response) {
  if (is.null(response)) {
    log_error("Response is NULL")
    return(FALSE)
  }
  
  # Check for error messages in response
  if ("Response" %in% names(response)) {
    if (response$Response == "Error") {
      log_error(paste("API returned error:", response$Message))
      return(FALSE)
    }
  }
  
  return(TRUE)
}


#' Rate Limiter
#' 
#' Simple rate limiting to avoid exceeding API limits
rate_limiter <- function() {
  # Add delay between calls
  Sys.sleep(1 / API_RATE_LIMIT$calls_per_second)
}


#' Get Available Cryptocurrencies
#' 
#' @return Vector of available cryptocurrency symbols
get_available_coins <- function() {
  log_info("Fetching available cryptocurrencies...")
  
  # This is a placeholder - in practice, you might want to fetch this from the API
  # CryptoCompare has an endpoint for this: /data/all/coinlist
  
  available_coins <- c("BTC", "ETH", "BNB", "SOL", "ADA", "XRP", "DOT", 
                       "DOGE", "MATIC", "AVAX", "LINK", "UNI", "ATOM")
  
  log_info(paste("Available coins:", paste(available_coins, collapse = ", ")))
  return(available_coins)
}


#' Parse Timestamp
#' 
#' @param timestamp Unix timestamp
#' @return POSIXct datetime object
parse_timestamp <- function(timestamp) {
  as.POSIXct(timestamp, origin = "1970-01-01", tz = TIMEZONE)
}


#' Format Response
#' 
#' @param response Raw API response
#' @param format_type Type of formatting to apply
#' @return Formatted data frame or list
format_response <- function(response, format_type = "default") {
  if (!validate_response(response)) {
    return(NULL)
  }
  
  # Different formatting based on response type
  # This will be expanded as we add more API endpoints
  
  return(response)
}


# ============================================================================
# INITIALIZATION
# ============================================================================

# Log that API client is loaded
log_info("API Client module loaded")

# Export functions (R doesn't have explicit exports like Python, but this documents what's public)
# Public functions:
# - make_api_call()
# - check_api_connection()
# - validate_response()
# - rate_limiter()
# - get_available_coins()
# - parse_timestamp()
# - format_response()