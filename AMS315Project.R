
# First, getting the stock data and importing it into R
msft_data = read.csv(".\\AMS 315 Project Data\\MSFT.csv", header = TRUE)
amzn_data = read.csv(".\\AMS 315 Project Data\\AMZN.csv")
# Original Strings: C:\\Users\\dsli\\Desktop\\AMS 315 Project Data\\MSFT.csv and C:\\Users\\dsli\\Desktop\\AMS 315 Project Data\\AMZN.csv

# Part a: Calculating the differential in log returns for all the days
msft_returns = diff(log(msft_data$Close))
amzn_returns = diff(log(amzn_data$Close))
# Getting the mean log return
e_return_msft = mean(msft_returns)
e_return_amzn = mean(amzn_returns)
# Printing the mean
print(e_return_msft)
print(e_return_amzn)
# The values are almost equal, and very close to zero also

# Part b: Testing for significance in difference between the two means
# H_0: mu_1 - mu_2 = 0 vs H_1: mu_1 - mu_2 != 0
# sigma values are unknown for BOTH populations right now, so calculate them
diff = e_return_msft - e_return_amzn
var_msft = var(msft_returns)
var_amzn = var(amzn_returns)
n_1 = length(msft_returns)
n_2 = length(amzn_returns)
var_1_value = var_msft / n_1
var_2_value = var_amzn / n_2
z_stat_bottom = sqrt(var_1_value + var_2_value)
z_stat = abs(diff / z_stat_bottom)
# Find if the z_stat > z_alpha/2 --> make alpha = 0.20, that's the smallest z_alpha/2 value
# Rejection region is if z_stat > 1.28, our value of z_alpha/2
if (z_stat > 1.28) {
  print("Reject H_0 and accept H_1.  Significant difference")
} else { # else must be on the same line as the end of the if block
  print("Accept H_0.  Insignificant difference.")
}

# Part c: Finding correlation between two stocks
p = cor(msft_returns, amzn_returns)
# Significance testing: rho = 0 vs rho != 0
# Get the test statistic t
t = abs((sqrt(length(msft_returns) - 2) / sqrt(1 - p^2)) * p)
# Rejection region is abs(t) > t_alpha/2 ... t_alpha/2 = 3.291 at the worst case scenario
if (t > 3.291) {
  print("Reject H_0 and accept H_1, there is a significant difference")
} else {
  print("Accept H_0 --> Insignificant difference")
}

# Part d: Linear Model
# We use msft as x and amzn as y
# Calculate beta_1 first --> beta_1 = p * sqrt(var_msft/var_amzn)
beta_1 = p * sqrt(var_amzn/var_msft)
beta_0 = mean(amzn_returns) - beta_1 * mean(msft_returns)
sprintf("The linear model equation is %.2f + %.5fx", beta_0, beta_1)
# Now find the confidence interval, using t_alpha/2 = 1.282
# MSE = (n - 1) S_Y^2 p^2
mse = (n_1 - 1) * var_amzn * p^2
ci_beta1_low = beta_1 - sqrt(mse / ((n_1 - 1) * var_amzn))
ci_beta1_hi = beta_1 + sqrt(mse / ((n_1 - 1) * var_amzn))
sprintf("The confidence interval is (%.5f, %.5f)", ci_beta1_low, ci_beta1_hi)
# Report R square
sprintf("The value of R square is %.2f", p^2)

# Part e: 1-step analysis
# Use 6 "checkpoints" for July 2017 - July 1, July 6, July 11, July 16, JUly 21, July 26, and July 31
x_values = list(1, 6, 11, 16, 21, 26, 31)
y_values = list()
for (i in 1:7) {
  # Get the data for each model
  msft_data_dir = gettextf(".\\AMS 315 Project Data\\MSFT Model %d.csv", i)
  amzn_data_dir = gettextf(".\\AMS 315 Project Data\\AMZN Model %d.csv", i)
  msft_data_set = read.csv(msft_data_dir)
  amzn_data_set = read.csv(amzn_data_dir)
  msft_returns_data_set = diff(log(msft_data_set$Close))
  amzn_returns_data_set = diff(log(amzn_data_set$Close))
  
  # Now, data in hand, let's calculate the basic values
  p_data_set = cor(msft_returns_data_set, amzn_returns_data_set)
  var_msft_data_set = var(msft_returns_data_set)
  var_amzn_data_set = var(amzn_returns_data_set)
  n_1_data_set = length(msft_returns_data_set)
  n_2_data_set = length(amzn_returns_data_set)
  
  # Get beta_1 and beta_0 for our linear equation for the particular model
  beta_1_data_set = p_data_set * sqrt(var_amzn_data_set/var_msft_data_set)
  beta_0_data_set = mean(amzn_returns_data_set) - (beta_1_data_set * mean(msft_returns_data_set))
  
  datasetvalue = beta_0_data_set + (beta_1_data_set * (n_1_data_set + 1))
  y_values[i] = datasetvalue
  
}

 # Now plot the graph
plot(x_values, y_values)
arrows(x, 1, y, 1.388, length=0.05, angle=90, code=3)
dfx = unlist(x_values, use.names = FALSE)
dfy = unlist(y_values, use.names = FALSE)
data_correlation = cor(dfx, dfy)
beta_1_lineplot = data_correlation * sqrt(var(dfy) / var(dfx))
beta_0_lineplot = mean(dfy) - (beta_1_lineplot * mean(dfx))
sprintf("The equation for the lineplot in the graph is %.2f + %.2fx", beta_0_lineplot, beta_1_lineplot)
lines(dfx, dfy)
#abline(beta_0_lineplot, beta_1_lineplot)

# After plotting the graph, find the 95% confidence interval (alpha = 0.05 and t_alpha/2 = 2.042)
mse_plot = (6) * var(dfy) * data_correlation^2
ci_plot_high = beta_1_lineplot + sqrt(mse_plot / ((6) * var(dfy)))
ci_plot_lo = beta_1_lineplot - sqrt(mse_plot / ((6) * var(dfy)))
sprintf("The confidence interval for the July 2017 1-step plot is (%.5f, %.5f)", ci_plot_lo, ci_plot_high)
xy_dataframe = data.frame(dfx, dfy)
