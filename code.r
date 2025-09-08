# Chargement des packages
library(readr)
library(lubridate)
library(dplyr)
library(zoo)
library(purrr)
library(car)        # Multicolinéarité (VIF)
library(lmtest)     # Tests statistiques
library(tseries)    # Stationnarité (ADF)
library(forecast)   # ARIMA
library(ggplot2)    # Visualisation
library(corrplot)   # Corrélation
library(sandwich)   # Erreurs robustes
library(vars)       # VAR

# Importation des données
btc = read.csv("dataset.csv")
fedfunds = read_csv("DFF.csv")
snp = read.csv("SP500.csv")
electricity = read.csv("APU000072610.csv")

# Conversion des dates
btc$Date <- ymd(btc$Date)
fedfunds$Date <- ymd(fedfunds$observation_date)
snp$Date <- ymd(snp$observation_date)
electricity$Date <- ymd(electricity$observation_date)

# Données mensuelles
btc_monthly <- btc %>%
  group_by(YearMonth = floor_date(Date, "month")) %>%
  summarise(Price_BTC = mean(BTC_Closing, na.rm = TRUE),
            BTC_volume = mean(BTC_Volume, na.rm = TRUE),
            FearGreed = mean(Value, na.rm = TRUE))

fedfunds_monthly <- fedfunds %>%
  group_by(YearMonth = floor_date(Date, "month")) %>%
  summarise(Rate = mean(DFF, na.rm = TRUE))

snp_monthly <- snp %>%
  group_by(YearMonth = floor_date(Date, "month")) %>%
  summarise(Price_SP = mean(SP500, na.rm = TRUE))

electricity_monthly <- electricity %>%
  group_by(YearMonth = floor_date(Date, "month")) %>%
  summarise(Consumption = mean(APU000072610, na.rm = TRUE))

# Fusion des datasets
df_monthly <- reduce(list(btc_monthly, fedfunds_monthly, snp_monthly, electricity_monthly),
                     full_join, by = "YearMonth")

# Régression multiple
model = lm(Price_BTC ~ BTC_volume + Rate + Price_SP + Consumption + FearGreed,
           data = df_monthly)
summary(model)

# Diagnostics
vif(model)                       # Multicolinéarité
adf.test(df_monthly$Price_BTC)   # Stationnarité
dwtest(model)                    # Autocorrélation
bptest(model)                    # Hétéroscédasticité
coeftest(model, vcov = vcovHC(model, type = "HC")) # Erreurs robustes

# Corrélation
cor_matrix <- cor(df_monthly[, c("Price_BTC","BTC_volume","Rate","Price_SP","Consumption","FearGreed")],
                  use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper")

# ARIMA
arima_model <- auto.arima(df_monthly$Price_BTC)
summary(arima_model)

# VAR
var_data <- df_monthly[, c("Price_BTC","BTC_volume","Rate","Price_SP","Consumption","FearGreed")]
var_model <- VAR(var_data, p = 2, type = "const")
summary(var_model)
