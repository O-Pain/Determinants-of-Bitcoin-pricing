library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(purrr)
library(car)
library(lmtest)
library(sandwich)
library(corrplot)
library(forecast)
library(tseries)

btc = read.csv("C:/Users/Zrenp/OneDrive/Desktop/Projets/économétrie/Les déterminants des prix des cryptoactifs Assas/Data/dataset.csv")
fedfunds = read.csv("C:/Users/Zrenp/OneDrive/Desktop/Projets/économétrie/Les déterminants des prix des cryptoactifs Assas/Data/DFF.csv")
snp = read.csv("C:/Users/Zrenp/OneDrive/Desktop/Projets/économétrie/Les déterminants des prix des cryptoactifs Assas/Data/SP500.csv")
electricity = read.csv("C:/Users/Zrenp/OneDrive/Desktop/Projets/économétrie/Les déterminants des prix des cryptoactifs Assas/Data/APU000072610.csv")

btc$Date = ymd(btc$Date)
fedfunds$Date = ymd(fedfunds$observation_date)
snp$Date = ymd(snp$observation_date)
electricity$Date = ymd(electricity$observation_date)

btc_monthly = btc %>%
  group_by(YearMonth = floor_date(Date, "month")) %>%
  summarise(Price_BTC = mean(BTC_Closing, na.rm = TRUE),
            BTC_volume = mean(BTC_Volume, na.rm = TRUE),
            FearGreed = mean(Value, na.rm = TRUE))
fedfunds_monthly = fedfunds %>%
  group_by(YearMonth = floor_date(Date, "month")) %>%
  summarise(Rate = mean(DFF, na.rm = TRUE))
snp_monthly = snp %>%
  group_by(YearMonth = floor_date(Date, "month")) %>%
  summarise(Price_SP = mean(SP500, na.rm = TRUE))
electricity_monthly = electricity %>%
  group_by(YearMonth = floor_date(Date, "month")) %>%
  summarise(Consumption = mean(APU000072610, na.rm = TRUE))

df_monthly = reduce(list(btc_monthly, fedfunds_monthly, snp_monthly, electricity_monthly),
                    full_join, by = "YearMonth")

df_monthly$DummyChina <- ifelse(df_monthly$YearMonth == ymd("2021-09-01"), 1, 0)
df_monthly$DummyMiCA  <- ifelse(df_monthly$YearMonth == ymd("2023-04-01"), 1, 0)
df_monthly$DummyETF   <- ifelse(df_monthly$YearMonth == ymd("2024-01-01"), 1, 0)

df_monthly$RegShock <- ifelse(df_monthly$DummyChina == 1 |
                              df_monthly$DummyMiCA == 1 |
                              df_monthly$DummyETF == 1, 1, 0)

model = lm(Price_BTC ~ BTC_volume + Rate + Price_SP + Consumption + FearGreed,
           data = df_monthly)

model_reg = lm(Price_BTC ~ BTC_volume + Rate + Price_SP + Consumption + FearGreed +
                 DummyChina + DummyMiCA + DummyETF,
               data = df_monthly)

model_reg2 = lm(Price_BTC ~ BTC_volume + Rate + Price_SP + Consumption + FearGreed +
                  RegShock,
                data = df_monthly)

summary(model)
summary(model_reg)
summary(model_reg2)

vif(model_reg2)
dwtest(model_reg2)
bptest(model_reg2)
coeftest(model_reg2, vcov = vcovHC(model_reg2, type = "HC"))

cor_matrix = cor(df_monthly[, c("Price_BTC", "BTC_volume", "Rate", "Price_SP",
                                "Consumption", "FearGreed")],
                 use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper")

btc_ts = ts(df_monthly$Price_BTC, frequency = 12, start = c(2018,2))
arima_model = auto.arima(btc_ts)
summary(arima_model)
