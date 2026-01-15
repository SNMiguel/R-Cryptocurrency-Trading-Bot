# Cryptocurrency Trading Bot

An R-based cryptocurrency trading bot that fetches real-time and historical crypto data, implements trading strategies, and executes trades.

## Project Status

**Current Phase:** Data Acquisition ✓
- ✅ Real-time price fetching
- ✅ Historical data retrieval
- ✅ API integration with CryptoCompare
- 🔄 Technical indicators (Coming in Priority 2)
- 🔄 Trading strategies (Coming in Priority 2)
- 🔄 Backtesting framework (Coming in Priority 2)

## Features

### Data Acquisition
- Fetch real-time cryptocurrency prices
- Download historical OHLCV data (Open, High, Low, Close, Volume)
- Support for minute, hour, and daily intervals
- Multiple cryptocurrency tracking
- Automatic data saving and management

### Supported Cryptocurrencies
- Bitcoin (BTC)
- Ethereum (ETH)
- Binance Coin (BNB)
- Solana (SOL)
- Cardano (ADA)
- And more...

## Project Structure

```
crypto-trading-bot/
├── config/
│   └── config.R              # Configuration settings
├── data/
│   ├── raw/                  # Raw API data
│   └── processed/            # Processed data
├── src/
│   └── data_acquisition/
│       ├── api_client.R      # Core API functions
│       ├── real_time.R       # Real-time data fetching
│       └── historical.R      # Historical data fetching
├── logs/                     # Log files
├── setup.R                   # Setup and installation
├── main.R                    # Main execution script
└── README.md                 # This file
```

## Installation

### Prerequisites
- R (version 4.0 or higher)
- Internet connection for API access

### Setup

1. Clone or download this project

2. Run the setup script to install required packages:
```r
source("setup.R")
```

This will install:
- `httr` - HTTP requests
- `jsonlite` - JSON parsing
- `tidyverse` - Data manipulation
- `lubridate` - Date/time handling
- `TTR` - Technical indicators
- `quantmod` - Financial modeling
- `logger` - Logging

## Usage

### Quick Start

Run the bot in demo mode:
```r
source("main.R")
```

### Configuration

Edit `config/config.R` to customize:
- Cryptocurrencies to track
- Base currency (USD, EUR, etc.)
- Data collection intervals
- Trading parameters
- Risk management settings

### Different Modes

**Demo Mode** (Default):
```r
source("main.R")  # Runs quick demo of all features
```

**Data Collection Mode**:
```r
run_bot(mode = "collect_data")  # Downloads 30 days of historical data
```

**Monitoring Mode**:
```r
run_bot(mode = "monitor")  # Continuous price monitoring
```

### Custom Operations

```r
# Get current price
btc_price <- get_current_price("BTC")

# Get multiple prices
prices <- get_multiple_prices(c("BTC", "ETH", "SOL"))

# Get historical data
btc_history <- get_historical_days("BTC", limit = 30)

# Get full market data
market_data <- get_full_market_data(c("BTC", "ETH"))

# Monitor prices for 5 minutes
price_history <- monitor_prices(
  cryptos = c("BTC", "ETH"),
  duration_minutes = 5,
  interval_seconds = 10
)

# Download and save historical data
download_historical_data(
  cryptos = c("BTC", "ETH", "BNB"),
  days = 90,
  interval = "day"
)
```

## API Information

This bot uses the **CryptoCompare API** for data:
- Free tier: ~100,000 calls/month
- No API key required for basic usage
- Rate limited to prevent abuse

API Documentation: https://min-api.cryptocompare.com/

## Data Files

### Raw Data
Stored in `data/raw/`:
- Real-time price snapshots
- Historical OHLCV data
- Format: RDS (R Data Serialization)

### Processed Data
Stored in `data/processed/` (coming in Priority 2):
- Cleaned and transformed data
- Feature-engineered datasets
- Model-ready data

## Logs

All bot activity is logged to `logs/`:
- API calls and responses
- Errors and warnings
- Data collection events
- Trading activities (when implemented)

## Roadmap

### Priority 2 (Next Steps)
- [ ] Technical indicators (RSI, MACD, Bollinger Bands)
- [ ] Trading strategy implementation
- [ ] Backtesting framework
- [ ] Performance metrics and visualization

### Priority 3 (Future)
- [ ] Risk management system
- [ ] Paper trading
- [ ] Live trading integration
- [ ] Real-time alerts and notifications
- [ ] Web dashboard

## Development Notes

### Adding New Cryptocurrencies
Edit `CRYPTOCURRENCIES` in `config/config.R`:
```r
CRYPTOCURRENCIES <- c("BTC", "ETH", "BNB", "SOL", "ADA", "YOUR_COIN")
```

### Changing Data Collection Frequency
Edit `DATA_INTERVALS` in `config/config.R`:
```r
DATA_INTERVALS <- list(
  real_time_refresh = 10,    # seconds
  historical_minute = 1,      # minutes
  historical_hour = 1,        # hours
  historical_day = 1          # days
)
```

## Troubleshooting

### API Connection Issues
- Check internet connection
- Verify CryptoCompare API is accessible
- Check rate limits haven't been exceeded

### Package Installation Issues
- Ensure R is version 4.0+
- Run `setup.R` again
- Install packages manually if needed

### Data Collection Issues
- Verify cryptocurrency symbols are correct
- Check API rate limits
- Review log files in `logs/` directory

## Contributing

This is a personal project, but suggestions are welcome!

## Disclaimer

This bot is for educational and research purposes only. Cryptocurrency trading involves substantial risk. Always do your own research and never invest more than you can afford to lose.

## License

MIT License - Feel free to use and modify for your own purposes.

## Contact

Questions or issues? Check the log files first, then review the configuration settings.

---

**Last Updated:** January 2026
**Version:** 0.1.0 (Data Acquisition Phase)