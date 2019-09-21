library(PortfolioAnalytics)
data("indexes") 
returns <- indexes[,1:4]
print(returns)

n <- ncol(returns)
equal_weights <- rep(1/n,n)

benchmark_returns <- Return.portfolio(R= returns, weights = equal_weights, rebalance_on = "years")
colnames(benchmark_returns) <- "benchmark" 

table.AnnualizedReturns(benchmark_returns)

base_port_spec <- portfolio.spec(assets = colnames(returns))
base_port_spec <- add.constraint(portfolio = base_port_spec, type = "full_investment")
base_port_spec <- add.constraint(portfolio = base_port_spec, type = "long_only")
base_port_spec <- add.objective(portfolio = base_port_spec, type = "risk", name = "StdDev")

opt_base <- optimize.portfolio.rebalancing(R = returns, optimize_method = "ROI", portfolio = base_port_spec, rebalance_on = "quarters", training_period = 60, rolling_window = 60)

#calculate portfolio returns
base_returns <- Return.portfolio(returns, extractWeights(opt_base))
colnames(base_returns) <- "base"

chart.Weights(opt_base)

#merge benchmark and base returns
ret <- cbind(benchmark_returns, base_returns)

#annualized performance
table.AnnualizedReturns(ret)



##reframe constraints
#copy of portfolio specification
box_port_spec <- base_port_spec

box_port_spec <- add.constraint(portfolio = box_port_spec, type = "box", min = .05, max = .4, indexnum = 2)

#backtest
opt_box <- optimize.portfolio.rebalancing(R = returns, optimize_method = "ROI", portfolio = box_port_spec, rebalance_on = "quarters", training_period = 60, rolling_window = 60)

#box returns
box_returns <- Return.portfolio(returns, extractWeights(opt_box))
colnames(box_returns) <- "box"

chart.Weights(opt_box)

ret <- cbind(ret, box_returns)

table.AnnualizedReturns(ret)

