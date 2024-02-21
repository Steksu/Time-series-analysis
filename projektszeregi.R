library(readxl)
getwd()
setwd('D:/materialystudia/ProjektSzeregi')
getwd()
dane1 <- read.csv("PCU721110721110.csv", header=T, sep=",")
dane1
dane2 <- read.csv("RSXFS.csv", header=T, sep=",")
dane2

library(lubridate)

szereg1 <- ts(dane1$PCU721110721110, start=decimal_date(ymd("2003-12-01")), freq = 12)
is.ts(szereg1)
plot(szereg1)

szereg2 <- ts(dane2$RSXFS, start=decimal_date(ymd("1992-01-01")), freq = 12)
is.ts(szereg2)
plot(szereg2)

monthplot(szereg1)
monthplot(szereg2)

boxplot(szereg1 ~ cycle(szereg1), names=month.name)
boxplot(szereg2 ~ cycle(szereg2), names=month.name)

library(forecast)
seasonplot(szereg2, col = rainbow(12), year.labels = TRUE, pch=19, lwd=1.0)
seasonplot(szereg1, col = rainbow(12), year.labels = TRUE, pch=19, lwd=1.0)

lag.plot(szereg1, lags=12)
lag.plot(szereg2, lags=12)

Acf(szereg1, lag.max = 60)
Acf(szereg2, lag.max = 60)

Pacf(szereg1, lag.max = 60)
Pacf(szereg2, lag.max = 60)

plot(decompose(szereg1, type="additive"))
plot(decompose(szereg2, type="additive"))

plot(decompose(szereg1, type="multiplicative"))
plot(decompose(szereg2, type="multiplicative"))

ruchomesrednie1 <- filter(szereg1, sides = 2, filter = rep(1/12,12))
plot(szereg1)
lines(ruchomesrednie1, col = "purple")

ruchomesrednie2 <- filter(szereg2, sides = 2, filter = rep(1/12,12))
plot(szereg2)
lines(ruchomesrednie2, col = "purple")

#Dekompozycja na podstawie modelu regresji szereg1
d4 <- szereg1
elec_model <- tslm(d4 ~ trend)
plot(d4)
lines(fitted(elec_model), col = "blue", lty = 2)

tsdisplay(residuals(elec_model))

#### D.4.2
elec_model_sezon <- tslm(d4 ~ trend +season)
plot(d4)
lines(fitted(elec_model_sezon), col="blue", lty=2)
tsdisplay(residuals(elec_model_sezon))

#### D.4.3
elec_model_boxcox <- tslm(d4 ~ trend + season, lambda="auto")
plot(d4)
lines(fitted(elec_model_boxcox), col="blue", lty=2)

#### D4.4
d4
bc <- tslm(d4 ~ poly(trend, raw=TRUE, degree = 2))
plot(d4)
lines(fitted(bc), col = "blue", lty = 2)

bc_season <- tslm(d4 ~ poly(trend, raw=TRUE, degree = 2)+ season)
plot(d4)
lines(fitted(bc_season), col = "blue", lty = 2)

bc_season_boxcox <- tslm(d4 ~ poly(trend, raw=TRUE, degree = 2)+ season, lambda="auto")
plot(d4)
lines(fitted(bc_season_boxcox), col = "blue", lty = 2)


plot(d4)
lines(fitted(bc), col = "blue", lty = 2)
lines(fitted(bc_season), col = "green", lty = 2)
lines(fitted(bc_season_boxcox), col = "red", lty = 2)

tsdisplay(residuals(bc_season_boxcox))

#Dekompozycja na podstawie modelu regresji szereg2
     
d4 <- szereg2
elec_model <- tslm(d4 ~ trend)
plot(d4)
lines(fitted(elec_model), col = "blue", lty = 2)

tsdisplay(residuals(elec_model))

#### D.4.2
elec_model_sezon <- tslm(d4 ~ trend +season)
plot(d4)
lines(fitted(elec_model_sezon), col="blue", lty=2)
tsdisplay(residuals(elec_model_sezon))

#### D.4.3
elec_model_boxcox <- tslm(d4 ~ trend + season, lambda="auto")
plot(d4)
lines(fitted(elec_model_boxcox), col="blue", lty=2)

#### D4.4
d4
bc <- tslm(d4 ~ poly(trend, raw=TRUE, degree = 2))
plot(d4)
lines(fitted(bc), col = "blue", lty = 2)

bc_season <- tslm(d4 ~ poly(trend, raw=TRUE, degree = 2)+ season)
plot(d4)
lines(fitted(bc_season), col = "blue", lty = 2)

bc_season_boxcox <- tslm(d4 ~ poly(trend, raw=TRUE, degree = 2)+ season, lambda="auto")
plot(d4)
lines(fitted(bc_season_boxcox), col = "blue", lty = 2)


plot(d4)
lines(fitted(bc), col = "blue", lty = 2)
lines(fitted(bc_season), col = "green", lty = 2)
lines(fitted(bc_season_boxcox), col = "red", lty = 2)

tsdisplay(residuals(bc_season_boxcox))

# dla wielomianu st 5
d4
bc <- tslm(d4 ~ poly(trend, raw=TRUE, degree = 5))
plot(d4)
lines(fitted(bc), col = "blue", lty = 2)

bc_season <- tslm(d4 ~ poly(trend, raw=TRUE, degree = 5)+ season)
plot(d4)
lines(fitted(bc_season), col = "blue", lty = 2)

bc_season_boxcox <- tslm(d4 ~ poly(trend, raw=TRUE, degree = 5)+ season, lambda="auto")
plot(d4)
lines(fitted(bc_season_boxcox), col = "blue", lty = 2)


plot(d4)
lines(fitted(bc), col = "blue", lty = 2)
lines(fitted(bc_season), col = "green", lty = 2)
lines(fitted(bc_season_boxcox), col = "red", lty = 2)

tsdisplay(residuals(bc_season_boxcox))

# Uczynienie szeregu stacjonanrym
# Eliminacja trendu, sezonowości szereg1
#usuwamy trend
szereg1diff <- diff(szereg1)
tsdisplay(szereg1diff)

#trendu nie ma, jest sezonowosc
# usuwamy sezonowosc
szereg1diff12 <- diff(szereg1diff, lag=12)
tsdisplay(szereg1diff12)

# Eliminacja trendu, sezonowości szereg2
#usuwamy trend
szereg2diff <- diff(szereg2)
tsdisplay(szereg2diff)
#trendu nie ma i nei ma sezonowosci
szereg2diff12 <- szereg2diff
tsdisplay(szereg2diff12)

#Wyznaczenie współczynników modelu AR,
#porównanie dopasowania z wykorzystaniem różnych metod estymacji, automatycznie dobrana wartość rzędu.
#MA(23)
#AR(14)
tsdisplay(szereg1diff12, lag.max = 60)
szereg1diff12yw <- ar(szereg1diff12, aic=FALSE, order.max = 14, method=c("yule-walker"))
szereg1diff12yw

szereg1diff12mle <- ar(szereg1diff12, aic=FALSE, order.max = 14, method=c("mle"))
szereg1diff12mle

szereg1diff12auto <- ar(szereg1diff12, aic=TRUE)
szereg1diff12auto

#
#MA(11)
#AR(23)
tsdisplay(szereg2diff12, lag.max = 60)
szereg2diff12yw <- ar(szereg2diff12, aic=FALSE, order.max = 23, method=c("yule-walker"))
szereg2diff12yw

szereg2diff12mle <- ar(szereg2diff12, aic=FALSE, order.max = 23, method=c("mle"))
szereg2diff12mle

szereg2diff12auto <- ar(szereg2diff12, aic=TRUE)
szereg2diff12auto

# Wyznaczenie wspolczynikow MA(q)

#szereg1
szereg1arima <- Arima(szereg1diff12 , order = c(0,0,23))
summary(szereg1arima)


#szereg2
szereg2arima <- Arima(szereg2diff12 , order = c(0,0,11))
summary(szereg2arima)
tsdisplay(szereg2diff12)

# Wyznaczenie optymalnych modeli z wykorzystanie funckji auto.arima()

autoarimaAICs1 <- auto.arima(szereg1diff12, ic="aic")
autoarimaAICs1

autoarimaAICCs1 <- auto.arima(szereg1diff12, ic="aicc")
autoarimaAICCs1

autoarimaBICs1 <- auto.arima(szereg1diff12, ic="bic")
autoarimaBICs1

####

autoarimaAICs2 <- auto.arima(szereg2diff12, ic="aic")
autoarimaAICs2

autoarimaAICCs2 <- auto.arima(szereg2diff12, ic="aicc")
autoarimaAICCs2

autoarimaBICs2 <- auto.arima(szereg2diff12, ic="bic")
autoarimaBICs2

# Prognozowanie z wykorzystaniem metod naiwnych

# prognozowanie naiwne oparte na sredniej
szereg1.meanf <- meanf(szereg1, h = 60)
plot(szereg1.meanf)

#usuniecie trendu i sezonowosci

szereg1.meanf2 <- meanf(szereg1diff12, h=24)
plot(szereg1.meanf2)

#prognozowanie naiwne
pnaiwnes1 <- naive(szereg1, h=24)
plot(pnaiwnes1)

psnaiwnes1 <- snaive(szereg1, h=24)
plot(psnaiwnes1)

#z uwzglednieniem dryftu
zdryftems1 <- rwf(szereg1, h=24, drift=TRUE)
plot(zdryftems1)

#

# prognozowanie naiwne oparte na sredniej
szereg2.meanf <- meanf(szereg2, h = 60)
plot(szereg2.meanf)

#usuniecie trendu i sezonowosci

szereg2.meanf2 <- meanf(szereg2diff12, h=24)
plot(szereg2.meanf2)

#prognozowanie naiwne
pnaiwnes2 <- naive(szereg2, h=24)
plot(pnaiwnes2)


psnaiwnes2 <- snaive(szereg2, h=24)
plot(psnaiwnes2)

#z uwzglednieniem dryftu
zdryftems2 <- rwf(szereg2, h=24, drift=TRUE)
plot(zdryftems2)

# Sprawdzenie najlepszej metody
accuracy(szereg1.meanf)
accuracy(szereg1.meanf2)
accuracy(pnaiwnes1)
accuracy(psnaiwnes1)
accuracy(zdryftems1)
#Dla szereg1 metoda najlepsza to naive()

accuracy(szereg2.meanf)
accuracy(szereg2.meanf2)
accuracy(pnaiwnes2)
accuracy(psnaiwnes2)
accuracy(zdryftems2)
#Dla szereg2 metoda najlepsza to naive()

#Prognozowanie na modelach z rodziny ARIMA
model1.Arimas1 <- Arima(szereg1, order=c(0,1,23), seasonal=c(0,1,0), lambda=0)
model2.Arimas1 <- Arima(szereg1, order=c(14,1,0), seasonal=c(0,1,0), lambda=0)
model3.Arimas1 <- Arima(szereg1, order=c(1,0,1), seasonal=c(0,0,1), lambda=0)
prognozy.model1.Arimas1 <- forecast(model1.Arimas1, h=24)
prognozy.model2.Arimas1 <- forecast(model2.Arimas1, h=24)
prognozy.model3.Arimas1 <- forecast(model3.Arimas1, h=24)
plot(prognozy.model1.Arimas1)
plot(prognozy.model2.Arimas1)
plot(prognozy.model3.Arimas1)

tsdisplay(szereg1diff12, lag.max = 60)

model1.Arimas2 <- Arima(szereg2, order=c(0,1,11), seasonal=c(0,1,0), lambda=0)
model2.Arimas2 <- Arima(szereg2, order=c(23,1,0), seasonal=c(0,1,0), lambda=0)
model3.Arimas2 <- Arima(szereg2, order=c(0,0,2), seasonal=c(0,0,1), lambda=0)
prognozy.model1.Arimas2 <- forecast(model1.Arimas2, h=24)
prognozy.model2.Arimas2 <- forecast(model2.Arimas2, h=24)
prognozy.model3.Arimas2 <- forecast(model3.Arimas2, h=24)
plot(prognozy.model1.Arimas2)
plot(prognozy.model2.Arimas2)
plot(prognozy.model3.Arimas2)
accuracy(prognozy.model1.Arimas2)
accuracy(prognozy.model2.Arimas2)
accuracy(prognozy.model3.Arimas2)
accuracy(model1.Arimas2)
