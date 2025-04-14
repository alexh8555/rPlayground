library(forecast)
library(tseries)
library(ggplot2)
library(readr)

data <- read_csv("0307_homework/lettuce_summary.csv")
# Convert date format
data$date <- as.Date(data$date, format = "%d/%m/%Y")

# Create a time series for each store (assuming the data is daily frequency)
# 計算起始日期在2015年的第幾天
start_date <- as.Date("2015-03-05")
start_day <- as.numeric(start_date - as.Date("2015-01-01")) + 1

# 創建時間序列對象（用於後續分析）
lettuce_ts_12631 <- ts(data$store_12631, frequency = 7, start = c(2015, 10))
lettuce_ts_46673 <- ts(data$store_46673, frequency = 7, start = c(2015, 10))
lettuce_ts_20974 <- ts(data$store_20974, frequency = 7, start = c(2015, 10))
lettuce_ts_4904  <- ts(data$store_4904,  frequency = 7, start = c(2015, 10))
# print(lettuce_ts_4904, calendar = T)

# 設置圖形佈局
par(mfrow = c(2, 2))

# 繪製STL分解圖
# plot(stl(lettuce_ts_12631, s.window = 7), main = "Store 12631")
# plot(stl(lettuce_ts_46673, s.window = 7), main = "Store 46673")
# plot(stl(lettuce_ts_20974, s.window = 7), main = "Store 20974")
# plot(stl(lettuce_ts_4904, s.window = 7), main = "Store 4904")

# 恢復單圖模式
par(mfrow = c(1, 1))

# 2015/3/5 ~ 2015/6/15

# Setting up training and test sets
train_12631 <- window(lettuce_ts_12631, end = c(2015, 90))
test_12631  <- window(lettuce_ts_12631, start = c(2015, 91))
# print(train_12631)
# print(test_12631)

train_46673 <- window(lettuce_ts_46673, end = c(2015, 90))
test_46673  <- window(lettuce_ts_46673, start = c(2015, 91))

train_20974 <- window(lettuce_ts_20974, end = c(2015, 90))
test_20974  <- window(lettuce_ts_20974, start = c(2015, 91))

train_4904 <- window(lettuce_ts_4904, end = c(2015, 90))
test_4904  <- window(lettuce_ts_4904, start = c(2015, 91))

#Holt-Winters model
#visualize (using STL)
train_12631 %>% stl(s.window = "periodic") %>% autoplot()
train_46673 %>% stl(s.window = "periodic") %>% autoplot()
train_20974 %>% stl(s.window = "periodic") %>% autoplot()
train_4904 %>% stl(s.window = "periodic") %>% autoplot()

# 使用 ggplot2 的方式修改圖形
print(train_4904 %>%
        stl(s.window = "periodic") %>%
        autoplot() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # 斜45度顯示x軸標籤
        scale_x_continuous(breaks = time(train_4904)[seq(1, length(time(train_4904)), by = 7)],  # 每7天顯示一個刻度
                           labels = format(seq(from = start_date,
                                               by = "week",
                                               length.out = length(seq(1, length(time(train_4904)), by = 7))),
                                           "%Y-%m-%d")))


#estimation (all of them have trend and seasonal factors based on visualization)
hw_12631 <- HoltWinters(train_12631, beta = TRUE, gamma = TRUE)
hw_46673 <- HoltWinters(train_46673, beta = FALSE, gamma = TRUE)
hw_20974 <- HoltWinters(train_20974, beta = FALSE, gamma = TRUE)
hw_4904  <- HoltWinters(train_4904, beta = FALSE, gamma = TRUE)


#estimation (Manually set the initial value)
#(a)Using STL to estimate initial values
get_initial_values <- function(train_data) {
  stl_result <- stl(train_data, s.window = "periodic")
  l_start <- stl_result$time.series[1, "trend"]
  b_start <- mean(diff(stl_result$time.series[, "trend"]), na.rm = TRUE)
  return(c(l_start, b_start))
}

init_12631 <- get_initial_values(train_12631)
init_46673 <- get_initial_values(train_46673)
init_20974 <- get_initial_values(train_20974)
init_4904  <- get_initial_values(train_4904)


#(b)Manual setting of Holt-Winters
hw_manual_12631 <- HoltWinters(train_12631, beta = TRUE, gamma = TRUE,
                               optim.start = c(alpha = 0, beta = 0),
                               l.start = init_12631[1], b.start = init_12631[2])

hw_manual_46673 <- HoltWinters(train_46673,beta = FALSE, gamma = TRUE,
                               optim.start = c(alpha = 0, beta = 0),
                               l.start = init_46673[1], b.start = init_46673[2])

hw_manual_20974 <- HoltWinters(train_20974, beta = FALSE, gamma = TRUE,
                               optim.start = c(alpha = 0, beta = 0),
                               l.start = init_20974[1], b.start = init_20974[2])

hw_manual_4904 <- HoltWinters(train_4904, beta = FALSE, gamma = TRUE,
                              optim.start = c(alpha = 0, beta = 0),
                              l.start = init_4904[1], b.start = init_4904[2])


#RMSE(The RMSE of the automatic estimates of the four stores is lower than that of the estimates with manually set initial values.)
sqrt(hw_12631$SSE / (length(train_12631) - 2))
sqrt(hw_manual_12631$SSE / (length(train_12631) - 2))

sqrt(hw_46673$SSE / (length(train_46673) - 2))
sqrt(hw_manual_46673$SSE / (length(train_46673) - 2))

sqrt(hw_20974$SSE / (length(train_20974) - 2))
sqrt(hw_manual_20974$SSE / (length(train_20974) - 2))

sqrt(hw_4904$SSE / (length(train_4904) - 2))
sqrt(hw_manual_4904$SSE / (length(train_4904) - 2))

#ets model
ets_12631 <- ets(train_12631, model = "ZZZ")
ets_46673 <- ets(train_46673, model = "ZZZ")
ets_20974 <- ets(train_20974, model = "ZZZ")
ets_4904  <- ets(train_4904, model = "ZZZ")

#Among hw automatic, hw manual and ets, ets has the smallest in-sample RMSE.
#but still need to compare the RMSE of out-of-sample
accuracy(ets_12631)
accuracy(ets_46673)
accuracy(ets_20974)
accuracy(ets_4904)

#out-of-sample performance
##forecast and model evaluation
# Holt-Winters（automatic）
hw_forecast_12631 <- forecast(hw_12631, h = length(test_12631))
hw_forecast_46673 <- forecast(hw_46673, h = length(test_46673))
hw_forecast_20974 <- forecast(hw_20974, h = length(test_20974))
hw_forecast_4904  <- forecast(hw_4904, h = length(test_4904))

# Holt-Winters（Manual initial value）
hw_manual_forecast_12631 <- forecast(hw_manual_12631, h = length(test_12631))
hw_manual_forecast_46673 <- forecast(hw_manual_46673, h = length(test_46673))
hw_manual_forecast_20974 <- forecast(hw_manual_20974, h = length(test_20974))
hw_manual_forecast_4904  <- forecast(hw_manual_4904, h = length(test_4904))

# ETS
ets_forecast_12631 <- forecast(ets_12631, h = length(test_12631))
ets_forecast_46673 <- forecast(ets_46673, h = length(test_46673))
ets_forecast_20974 <- forecast(ets_20974, h = length(test_20974))
ets_forecast_4904  <- forecast(ets_4904, h = length(test_4904))

# visualize
par(mfrow = c(2, 2))

# # Store 12631
# plot(hw_forecast_12631, main = "Store 12631: HW vs ETS", xaxt = "n")

# # 獲取完整的時間序列（包括原始數據和預測值的時間點）
# all_times <- c(time(hw_forecast_12631$x), time(hw_forecast_12631$mean))
# # 每7個點取一個點（因為frequency=7）
# time_indices <- seq(1, length(all_times), by = 7)

# # 修改x軸
# axis(1, at = all_times[time_indices],
#      labels = format(seq(from = start_date,
#                         by = "week",
#                         length.out = length(all_times[time_indices])),
#                     "%Y-%m-%d"),
#      las = 3)

# lines(fitted(hw_manual_forecast_12631), col = "blue", lty = 2)
# lines(fitted(ets_forecast_12631), col = "red", lty = 2)
# legend("topleft", c("Actual", "HW Auto", "HW Manual", "ETS"),
#        lty = c(1, 2, 2, 2), col = c("black", "blue", "green", "red"))

# # Store 46673
# plot(hw_forecast_46673, main = "Store 46673: HW vs ETS", xaxt = "n")

# axis(1, at = all_times[time_indices],
#      labels = format(seq(from = start_date,
#                         by = "week",
#                         length.out = length(all_times[time_indices])),
#                     "%Y-%m-%d"),
#      las = 3)

# lines(fitted(hw_manual_forecast_46673), col = "blue", lty = 2)
# lines(fitted(ets_forecast_46673), col = "red", lty = 2)

# # Store 20974
# plot(hw_forecast_20974, main = "Store 20974: HW vs ETS", xaxt = "n")
# axis(1, at = all_times[time_indices],
#      labels = format(seq(from = start_date,
#                         by = "week",
#                         length.out = length(all_times[time_indices])),
#                     "%Y-%m-%d"),
#      las = 3)
# lines(fitted(hw_manual_forecast_20974), col = "blue", lty = 2)
# lines(fitted(ets_forecast_20974), col = "red", lty = 2)

# # Store 4904
# plot(hw_forecast_4904, main = "Store 4904: HW vs ETS", xaxt = "n")
# axis(1, at = all_times[time_indices],
#      labels = format(seq(from = start_date,
#                         by = "week",
#                         length.out = length(all_times[time_indices])),
#                     "%Y-%m-%d"),
#      las = 3)
# lines(fitted(hw_manual_forecast_4904), col = "blue", lty = 2)
# lines(fitted(ets_forecast_4904), col = "red", lty = 2)

# par(mfrow = c(1, 1))  # 恢復單圖模式



#compare RMSE
# Store 12631
rmse_hw_12631 <- accuracy(hw_forecast_12631, test_12631)["Test set", "RMSE"]
rmse_hw_manual_12631 <- accuracy(hw_manual_forecast_12631, test_12631)["Test set", "RMSE"]
rmse_ets_12631 <- accuracy(ets_forecast_12631, test_12631)["Test set", "RMSE"]

# Store 46673
rmse_hw_46673 <- accuracy(hw_forecast_46673, test_46673)["Test set", "RMSE"]
rmse_hw_manual_46673 <- accuracy(hw_manual_forecast_46673, test_46673)["Test set", "RMSE"]
rmse_ets_46673 <- accuracy(ets_forecast_46673, test_46673)["Test set", "RMSE"]

# Store 20974
rmse_hw_20974 <- accuracy(hw_forecast_20974, test_20974)["Test set", "RMSE"]
rmse_hw_manual_20974 <- accuracy(hw_manual_forecast_20974, test_20974)["Test set", "RMSE"]
rmse_ets_20974 <- accuracy(ets_forecast_20974, test_20974)["Test set", "RMSE"]

# Store 4904
rmse_hw_4904 <- accuracy(hw_forecast_4904, test_4904)["Test set", "RMSE"]
rmse_hw_manual_4904 <- accuracy(hw_manual_forecast_4904, test_4904)["Test set", "RMSE"]
rmse_ets_4904 <- accuracy(ets_forecast_4904, test_4904)["Test set", "RMSE"]

# 創建 RMSE 比較表
rmse_results <- data.frame(
  Store = c("12631", "46673", "20974", "4904"),
  HW_Auto = c(rmse_hw_12631, rmse_hw_46673, rmse_hw_20974, rmse_hw_4904),
  HW_Manual = c(rmse_hw_manual_12631, rmse_hw_manual_46673, rmse_hw_manual_20974, rmse_hw_manual_4904),
  ETS = c(rmse_ets_12631, rmse_ets_46673, rmse_ets_20974, rmse_ets_4904)
)

# 顯示 RMSE 比較表
print(rmse_results)






# plot.ts(lettuce_ts_12631)
# autoplot(lettuce_ts_12631)
# ggtsdisplay(lettuce_ts_12631)
# ndiffs(lettuce_ts_12631)  # 測試趨勢是否需要差分（d）
# nsdiffs(lettuce_ts_12631) # 測試季節性是否需要差分（D）

# plot.ts(lettuce_ts_46673)
# autoplot(lettuce_ts_46673)
# ggtsdisplay(lettuce_ts_46673)

# plot.ts(lettuce_ts_20974)
# autoplot(lettuce_ts_20974)
# ggtsdisplay(lettuce_ts_20974)

# plot.ts(lettuce_ts_4904)
# autoplot(lettuce_ts_4904)
# ggtsdisplay(lettuce_ts_4904)
