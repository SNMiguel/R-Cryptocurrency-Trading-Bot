# ============================================================================
# Crypto Trading Bot - Visualization
# ============================================================================
# Functions for creating charts and performance visualizations

library(ggplot2)
library(tidyverse)
library(logger)

# ============================================================================
# EQUITY CURVE PLOTS
# ============================================================================

#' Plot Equity Curve
#' 
#' @param equity_curve Data frame with timestamp and portfolio_value
#' @param title Plot title
#' @return ggplot object
plot_equity_curve <- function(equity_curve, title = "Portfolio Equity Curve") {
  
  if (nrow(equity_curve) == 0) {
    log_warn("No equity curve data to plot")
    return(NULL)
  }
  
  initial_value <- equity_curve$portfolio_value[1]
  final_value <- tail(equity_curve$portfolio_value, 1)
  return_pct <- ((final_value - initial_value) / initial_value) * 100
  
  p <- ggplot(equity_curve, aes(x = timestamp, y = portfolio_value)) +
    geom_line(color = "#2E86AB", linewidth = 1) +
    geom_hline(yintercept = initial_value, linetype = "dashed", 
               color = "#666", alpha = 0.7) +
    labs(
      title = title,
      subtitle = sprintf("Return: %.2f%%  |  Start: $%.2f  |  End: $%.2f", 
                        return_pct, initial_value, final_value),
      x = "Date",
      y = "Portfolio Value ($)"
    ) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "#666"),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}


#' Plot Drawdown
#' 
#' @param equity_curve Data frame with equity curve
#' @param title Plot title
#' @return ggplot object
plot_drawdown <- function(equity_curve, title = "Drawdown Analysis") {
  
  if (nrow(equity_curve) == 0) {
    log_warn("No data to plot")
    return(NULL)
  }
  
  # Calculate running maximum
  equity_curve$running_max <- cummax(equity_curve$portfolio_value)
  
  # Calculate drawdown
  equity_curve$drawdown <- ((equity_curve$portfolio_value - equity_curve$running_max) / 
                             equity_curve$running_max) * 100
  
  max_dd <- min(equity_curve$drawdown)
  
  p <- ggplot(equity_curve, aes(x = timestamp, y = drawdown)) +
    geom_area(fill = "#E63946", alpha = 0.3) +
    geom_line(color = "#E63946", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid", color = "#666") +
    labs(
      title = title,
      subtitle = sprintf("Maximum Drawdown: %.2f%%", max_dd),
      x = "Date",
      y = "Drawdown (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "#666"),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}


# ============================================================================
# PRICE AND INDICATOR PLOTS
# ============================================================================

#' Plot Price with Indicators
#' 
#' @param data Data frame with OHLCV and indicators
#' @param show_ma Show moving averages
#' @param show_bb Show Bollinger Bands
#' @param title Plot title
#' @return ggplot object
plot_price_with_indicators <- function(data, 
                                       show_ma = TRUE, 
                                       show_bb = TRUE,
                                       title = "Price Chart with Indicators") {
  
  p <- ggplot(data, aes(x = timestamp, y = close)) +
    geom_line(color = "#2E86AB", linewidth = 0.8)
  
  # Add moving averages
  if (show_ma) {
    if ("sma_10" %in% names(data)) {
      p <- p + geom_line(aes(y = sma_10), color = "#F77F00", linewidth = 0.6, alpha = 0.7)
    }
    if ("sma_20" %in% names(data)) {
      p <- p + geom_line(aes(y = sma_20), color = "#06A77D", linewidth = 0.6, alpha = 0.7)
    }
    if ("sma_50" %in% names(data)) {
      p <- p + geom_line(aes(y = sma_50), color = "#D90429", linewidth = 0.6, alpha = 0.7)
    }
  }
  
  # Add Bollinger Bands
  if (show_bb && "bb_upper" %in% names(data)) {
    p <- p + 
      geom_ribbon(aes(ymin = bb_lower, ymax = bb_upper), 
                 fill = "#2E86AB", alpha = 0.1) +
      geom_line(aes(y = bb_upper), color = "#666", linewidth = 0.4, 
               linetype = "dashed", alpha = 0.5) +
      geom_line(aes(y = bb_lower), color = "#666", linewidth = 0.4, 
               linetype = "dashed", alpha = 0.5)
  }
  
  p <- p +
    labs(
      title = title,
      x = "Date",
      y = "Price ($)"
    ) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}


#' Plot RSI
#' 
#' @param data Data frame with RSI
#' @param oversold Oversold level
#' @param overbought Overbought level
#' @param title Plot title
#' @return ggplot object
plot_rsi <- function(data, 
                    oversold = 30, 
                    overbought = 70,
                    title = "RSI Indicator") {
  
  if (!"rsi" %in% names(data)) {
    log_warn("No RSI data in dataset")
    return(NULL)
  }
  
  p <- ggplot(data, aes(x = timestamp, y = rsi)) +
    geom_line(color = "#2E86AB", linewidth = 0.8) +
    geom_hline(yintercept = oversold, linetype = "dashed", 
              color = "#06A77D", alpha = 0.7) +
    geom_hline(yintercept = overbought, linetype = "dashed", 
              color = "#E63946", alpha = 0.7) +
    geom_hline(yintercept = 50, linetype = "solid", 
              color = "#666", alpha = 0.3) +
    labs(
      title = title,
      x = "Date",
      y = "RSI"
    ) +
    ylim(0, 100) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}


#' Plot MACD
#' 
#' @param data Data frame with MACD
#' @param title Plot title
#' @return ggplot object
plot_macd <- function(data, title = "MACD Indicator") {
  
  if (!"macd" %in% names(data)) {
    log_warn("No MACD data in dataset")
    return(NULL)
  }
  
  p <- ggplot(data, aes(x = timestamp)) +
    geom_col(aes(y = macd_histogram), 
            fill = ifelse(data$macd_histogram >= 0, "#06A77D", "#E63946"),
            alpha = 0.5) +
    geom_line(aes(y = macd), color = "#2E86AB", linewidth = 0.8) +
    geom_line(aes(y = macd_signal), color = "#F77F00", linewidth = 0.8) +
    geom_hline(yintercept = 0, color = "#666", alpha = 0.5) +
    labs(
      title = title,
      x = "Date",
      y = "MACD"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}


# ============================================================================
# STRATEGY COMPARISON PLOTS
# ============================================================================

#' Plot Strategy Comparison
#' 
#' @param comparison_df Data frame with strategy comparison
#' @param metric Metric to plot ("return_pct", "sharpe_ratio", "win_rate")
#' @param title Plot title
#' @return ggplot object
plot_strategy_comparison <- function(comparison_df, 
                                    metric = "return_pct",
                                    title = "Strategy Comparison") {
  
  if (nrow(comparison_df) == 0) {
    log_warn("No comparison data to plot")
    return(NULL)
  }
  
  # Reorder by metric
  # Reorder by metric
  comparison_df <- comparison_df %>%
    arrange(desc(.data[[metric]]))

    # Make strategy names unique by adding index
  comparison_df$display_name <- paste0(comparison_df$strategy_name, " (", 1:nrow(comparison_df), ")")

  comparison_df$display_name <- factor(comparison_df$display_name, 
                                    levels = comparison_df$display_name)
  
  # Choose color based on metric
  color_col <- ifelse(metric == "return_pct", "return_pct", metric)
  
  p <- ggplot(comparison_df, aes(x = display_name, y = .data[[metric]], 
                                 fill = .data[[color_col]])) +
    geom_col() +
    scale_fill_gradient2(low = "#E63946", mid = "#F1FAEE", high = "#06A77D",
                        midpoint = 0) +
    labs(
      title = title,
      subtitle = paste("Metric:", metric),
      x = "Strategy",
      y = metric
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "#666"),
      axis.title = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
  
  return(p)
}


# ============================================================================
# TRADE PLOTS
# ============================================================================

#' Plot Trades on Price Chart
#' 
#' @param data Price data
#' @param trades Trade history data frame
#' @param title Plot title
#' @return ggplot object
plot_trades <- function(data, trades, title = "Trades on Price Chart") {
  
  if (nrow(trades) == 0) {
    log_warn("No trades to plot")
    return(plot_price_with_indicators(data, title = title))
  }
  
  buy_trades <- trades %>% filter(action == "BUY")
  sell_trades <- trades %>% filter(action == "SELL")
  
  p <- ggplot(data, aes(x = timestamp, y = close)) +
    geom_line(color = "#2E86AB", linewidth = 0.8)
  
  if (nrow(buy_trades) > 0) {
    p <- p + geom_point(data = buy_trades, aes(x = timestamp, y = price),
                       color = "#06A77D", size = 3, shape = 24, fill = "#06A77D")
  }
  
  if (nrow(sell_trades) > 0) {
    p <- p + geom_point(data = sell_trades, aes(x = timestamp, y = price),
                       color = "#E63946", size = 3, shape = 25, fill = "#E63946")
  }
  
  p <- p +
    labs(
      title = title,
      subtitle = sprintf("Buy: %d  |  Sell: %d", nrow(buy_trades), nrow(sell_trades)),
      x = "Date",
      y = "Price ($)"
    ) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "#666"),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}


# ============================================================================
# SAVE PLOTS
# ============================================================================

#' Save Plot to File
#' 
#' @param plot ggplot object
#' @param filename Filename (without extension)
#' @param width Width in inches
#' @param height Height in inches
save_plot <- function(plot, filename, width = 10, height = 6) {
  
  if (is.null(plot)) {
    log_warn("Cannot save NULL plot")
    return(NULL)
  }
  
  results_dir <- file.path(getwd(), "results", "plots")
  
  if (!dir.exists(results_dir)) {
    dir.create(results_dir, recursive = TRUE)
  }
  
  filepath <- file.path(results_dir, paste0(filename, ".png"))
  
  ggsave(filepath, plot, width = width, height = height, dpi = 300)
  
  log_info(paste("Plot saved to:", filepath))
  
  return(filepath)
}


# ============================================================================
# INITIALIZATION
# ============================================================================

log_info("Visualization module loaded")

# Public functions:
# - plot_equity_curve()
# - plot_drawdown()
# - plot_price_with_indicators()
# - plot_rsi()
# - plot_macd()
# - plot_strategy_comparison()
# - plot_trades()
# - save_plot()