# 🚀 R Cryptocurrency Trading Bot

> **A professional-grade algorithmic trading system built in R with advanced technical analysis, risk management, and paper trading capabilities.**

[![R Version](https://img.shields.io/badge/R-4.5%2B-blue.svg)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)
[![Status](https://img.shields.io/badge/status-active-success.svg)]()
[![Code Size](https://img.shields.io/badge/code-4200%2B%20lines-orange.svg)]()

---

## 📊 Overview

An end-to-end algorithmic trading system designed for cryptocurrency markets. Features real-time data acquisition, technical analysis, multiple trading strategies, comprehensive backtesting, risk management, and paper trading simulation—all built from scratch in R.

### ✨ Key Features

- 📈 **Real-time & Historical Data** - CryptoCompare API integration
- 🔬 **Technical Analysis** - 10+ indicators (RSI, MACD, MA, Bollinger Bands)
- 🎯 **Multiple Strategies** - MA Crossover, RSI Mean Reversion, customizable templates
- 🧪 **Professional Backtesting** - Transaction costs, performance metrics, equity curves
- 🛡️ **Risk Management** - Position sizing (Fixed, Kelly Criterion, ATR), stop-loss/take-profit
- 💰 **Paper Trading** - Test strategies with simulated capital
- 📊 **Visualization** - Publication-quality charts (equity curves, drawdowns, indicators)

---

## 🎯 Performance Highlights

```
Strategy: RSI Mean Reversion
Period: 90 days (Oct 2025 - Jan 2026)
═══════════════════════════════════════
Initial Capital:    $10,000
Final Value:        $10,432
Total Return:       +4.32%
Win Rate:           25%
Sharpe Ratio:       -1.67
Max Drawdown:       -0.14%
```

---

## 🏗️ Architecture

```
crypto-trading-bot/
├── config/                  # Configuration files
├── data/                    # Data storage
│   ├── raw/                # Raw market data
│   └── processed/          # Processed data with indicators
├── src/
│   ├── data_acquisition/   # API integration & data fetching
│   ├── indicators/         # Technical indicators
│   ├── strategies/         # Trading strategies
│   ├── backtesting/        # Backtesting engine
│   ├── risk_management/    # Position sizing & risk tools
│   ├── trading/            # Paper trading simulator
│   └── visualization/      # Chart generation
├── results/
│   ├── backtest_results/   # Backtest outputs
│   └── plots/              # Generated charts
├── logs/                    # Application logs
└── tests/                   # Unit tests
```

**Total Lines of Code:** ~4,200  
**Total Files:** 18  
**Development Time:** 5 weeks

---

## 🚀 Quick Start

### Prerequisites

- R (version 4.5+)
- RStudio (recommended)

### Installation

1. **Clone the repository**
```bash
git clone https://github.com/SNMiguel/R-Cryptocurrency-Trading-Bot.git
cd R-Cryptocurrency-Trading-Bot
```

2. **Install dependencies**
```r
source("setup.R")
```

3. **Configure settings**
```r
# Edit config/config.R to customize:
# - Cryptocurrencies to track
# - Risk parameters
# - Initial capital
# - Trading mode (PAPER/LIVE)
```

### Run Your First Backtest

```r
# Load the system
source("demo_priority2.R")

# This will:
# 1. Download 90 days of BTC data
# 2. Calculate technical indicators
# 3. Run MA Crossover strategy
# 4. Run RSI Mean Reversion strategy
# 5. Compare results
# 6. Save backtest reports
```

### Paper Trading Example

```r
source("demo_priority3.R")

# This will:
# 1. Set up paper trading portfolio
# 2. Run strategies with stop-loss/take-profit
# 3. Generate risk reports
# 4. Create professional charts
```

---

## 📚 Documentation

### Technical Indicators

| Indicator | Description | Parameters |
|-----------|-------------|------------|
| **SMA/EMA** | Moving Averages | Periods: 10, 20, 50, 200 |
| **RSI** | Relative Strength Index | Period: 14, Oversold: 30, Overbought: 70 |
| **MACD** | Moving Average Convergence Divergence | Fast: 12, Slow: 26, Signal: 9 |
| **Bollinger Bands** | Volatility bands | Period: 20, Std Dev: 2 |
| **Volume** | Volume analysis | MA Period: 20 |

### Trading Strategies

#### 1. Moving Average Crossover
```r
strategy <- create_ma_crossover_strategy(
  fast_period = 10,
  slow_period = 20,
  ma_type = "SMA"
)
```
**Logic:** Buy when fast MA crosses above slow MA, sell when it crosses below.

#### 2. RSI Mean Reversion
```r
strategy <- create_rsi_strategy(
  rsi_period = 14,
  oversold = 30,
  overbought = 70
)
```
**Logic:** Buy when RSI < 30 (oversold), sell when RSI > 70 (overbought).

#### 3. Custom Strategy
```r
# Create your own strategy using the base template
my_strategy <- create_base_strategy(
  name = "My Strategy",
  parameters = list(...)
)

# Define signal generation
generate_signals.MyStrategy <- function(strategy, data) {
  # Your logic here
  return(data)
}
```

### Risk Management

```r
# Position sizing
position <- calculate_position_size_fixed(capital = 10000, risk_pct = 0.02)
position <- calculate_position_size_kelly(capital, win_rate, avg_win, avg_loss)

# Stop-loss & Take-profit
stop_loss <- calculate_stop_loss(entry_price = 90000, stop_pct = 0.02)
take_profit <- calculate_take_profit(entry_price = 90000, profit_pct = 0.05)

# Risk-reward ratio
rr_ratio <- calculate_risk_reward_ratio(entry_price, stop_loss, take_profit)
```

---

## 📊 Example Results

### Equity Curve
![Preview](https://i.imgur.com/OwoV7rH.png)

### Drawdown Analysis
![Preview](https://i.imgur.com/9JE19vo.png)

### Strategy Comparison
![Preview](https://i.imgur.com/X5bWoJG.png)

---

## 🧪 Backtesting

### Run a Backtest

```r
# Get historical data
btc_data <- get_historical_days("BTC", limit = 90)

# Add indicators
btc_data <- add_all_indicators(btc_data)

# Create strategy
strategy <- create_ma_crossover_strategy(10, 20)

# Run backtest with transaction costs
results <- run_backtest(
  strategy = strategy,
  data = btc_data,
  initial_capital = 10000,
  commission = 0.001,    # 0.1%
  slippage = 0.0005      # 0.05%
)

# View results
print_backtest_results(results)
```

### Performance Metrics

The backtesting engine calculates:
- **Returns:** Total return, return percentage
- **Risk Metrics:** Sharpe ratio, maximum drawdown, volatility
- **Trade Stats:** Win rate, profit factor, avg win/loss
- **Execution Costs:** Commission, slippage

---

## 🛡️ Risk Management Features

### Position Sizing Methods

1. **Fixed Percentage**
   - Risk fixed % of capital per trade
   - Simple and conservative

2. **Kelly Criterion**
   - Optimal bet sizing based on edge
   - Maximizes long-term growth

3. **ATR-Based**
   - Volatility-adjusted position sizing
   - Adapts to market conditions

### Stop-Loss & Take-Profit

- Automatic stop-loss calculation
- Trailing stop support
- Risk-reward ratio validation
- Portfolio risk monitoring

---

## 📈 Visualization

Generate professional charts:

```r
# Equity curve
plot <- plot_equity_curve(results$equity_curve)
save_plot(plot, "my_equity_curve")

# Price with indicators
plot <- plot_price_with_indicators(data, show_ma = TRUE, show_bb = TRUE)
save_plot(plot, "price_chart")

# RSI indicator
plot <- plot_rsi(data, oversold = 30, overbought = 70)
save_plot(plot, "rsi")

# Trades on chart
plot <- plot_trades(data, trade_history)
save_plot(plot, "trades")
```

All charts saved as high-resolution PNG files (300 DPI).

---

## 🔧 Configuration

### API Settings

```r
# config/config.R
API_BASE_URL <- "https://min-api.cryptocompare.com"
CRYPTOCURRENCIES <- c("BTC", "ETH", "BNB", "SOL", "ADA")
BASE_CURRENCY <- "USD"
```

### Risk Parameters

```r
RISK_MANAGEMENT <- list(
  stop_loss_pct = 0.02,       # 2% stop-loss
  take_profit_pct = 0.05,     # 5% take-profit
  max_daily_loss_pct = 0.05,  # 5% max daily loss
  max_open_positions = 3
)
```

### Position Sizing

```r
POSITION_SIZING <- list(
  initial_capital = 10000,
  max_position_size = 0.10,   # 10% per trade
  min_position_size = 100
)
```

---

## 🎓 Learning Resources

### For Beginners
1. Start with `demo_priority2.R` - Learn backtesting basics
2. Experiment with indicator parameters
3. Try different cryptocurrencies
4. Compare strategy performance

### For Advanced Users
1. Create custom strategies using `base_strategy.R`
2. Optimize parameters with grid search
3. Implement multi-indicator strategies
4. Add your own risk management rules

---

## 📊 Sample Output

```
=== BACKTEST RESULTS ===

Strategy: Moving Average Crossover

--- PERFORMANCE METRICS ---
Initial Capital:     $10,000.00
Final Value:         $9,338.02
Total Return:        -$661.98 (-6.62%)

--- TRADING ACTIVITY ---
Total Trades:        3
Completed Trades:    1
Win Rate:            0.00%

--- RISK METRICS ---
Sharpe Ratio:        -1.71
Max Drawdown:        -6.00%
Profit Factor:       0.00

--- TRANSACTION COSTS ---
Total Commission:    $27.36
Total Slippage:      $13.68
Total Costs:         $41.03
```

---

## 🗺️ Roadmap

### ✅ Completed (Phase 1-3)
- [x] Real-time & historical data acquisition
- [x] Technical indicators (10+ indicators)
- [x] Multiple trading strategies
- [x] Professional backtesting engine
- [x] Risk management system
- [x] Paper trading simulator
- [x] Visualization tools

### 🚧 In Progress (Phase 4)
- [ ] Live trading integration
- [ ] Advanced order types (limit, stop)
- [ ] Multi-asset portfolio management
- [ ] Machine learning strategies
- [ ] Real-time alerts & notifications

### 🔮 Future
- [ ] Web interface (React + FastAPI)
- [ ] Mobile app
- [ ] Social trading features
- [ ] Strategy marketplace
- [ ] API for external integrations

---

## 🤝 Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

---

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## ⚠️ Disclaimer

**This software is for educational purposes only.**

- Cryptocurrency trading carries significant risk
- Past performance does not guarantee future results
- Always do your own research (DYOR)
- Never invest more than you can afford to lose
- The authors are not responsible for any financial losses

---

## 🙏 Acknowledgments

- **CryptoCompare API** - Real-time cryptocurrency data
- **R Community** - Excellent packages (tidyverse, TTR, ggplot2)
- **Quantitative Finance** - Algorithmic trading research

---

## 📧 Contact

**Chawana Smith**
- GitHub: [@SNMiguel](https://github.com/migztech)
- LinkedIn: [MigzTech LinkedIn](https://linkedin.com/in/your-profile)
- Email: shemamiguel2023@gmail.com

---

## 🌟 Support

If you find this project helpful, please consider:
- ⭐ Starring the repository
- 🐛 Reporting bugs
- 💡 Suggesting new features
- 📢 Sharing with others

---

<div align="center">

**Built with ❤️ using R**

⭐ Star this repo if you found it useful! ⭐

</div>

**Last Updated:** January 2026
**Version:** 0.1.0 (Data Acquisition Phase)
