
library(dplyr)
library(readxl)
library(dplyr)
library(lubridate)
library(forecast)
library(bsts)




data <- read.csv('/home/yeiver/Documentos/universidad/proyectoCatch/personas_mensuales_desde_indice_base2018.csv')
attach(data)

data <- data %>%
  mutate(`fecha` = ym(`fecha`)) 

manufactura_ts <- ts(data$personas, start = c(2007, 1), freq = 12)
print(manufactura_ts)
train <- window(manufactura_ts, end = c(2024, 12))
test  <- window(manufactura_ts, start = c(2025, 1), end = c(2025, 8))


yticks <- pretty(range(manufactura_ts), n = 8)
plot(manufactura_ts,
     axes = FALSE,                      # ocultar ejes por defecto
     main = "Cantidad de personas empleadas en la Industria Manufacturera en México (2007–2025)",
     xlab = "",
     ylab = "Personas (millones)"
)

axis(1)
axis(2, at = yticks,
     labels = format(yticks / 1000000, big.mark = ","),
     las = 1)

abline(h = yticks, col = "gray90", lty = 3)
box()

mtext("Fuente: INEGI – Banco de Indicadores (BISE).", side = 1, line = 3, adj = 0, cex = 0.8)


plot(decompose(manufactura_ts))

diff_12 <- diff(manufactura_ts, lag = 12)

ac <- acf(diff_12, lag.max = 60, plot = FALSE)
plot(acf(manufactura_ts))


plot(ac, 
     xaxt = "n",
     main = "Diferencia interanual del empleo manufacturero en Mexico",
     ylab = "Valor",  # Es buena práctica ponerle título al eje Y
     xlab = "Rezagos (Lags)" # Puse un título de ejemplo para el eje X
)

axis(1,
     at = seq(0, 60, by = 6) / frequency(manufactura_ts), 
     labels = seq(0, 60, by = 6)
)


manufactura_hw <- HoltWinters (manufactura_ts, seasonal = "mult") 
manufactura_hw$coef ; manufactura_hw$SS

plot (manufactura_hw,
      main = "Empleo en Industria manufacturera Ajustada mediante Holt-Winters",
      ylab="Personas empleadas")

legend("topleft",
       legend = c("Serie original", "Serie ajustada (Holt–Winters)"),
       col = c("black", "red"), lty = 1, lwd = 1.2, bty = "n")


fc <- predict(manufactura_hw, n.ahead = 12)


plot(manufactura_hw, ylab = "Personas empleadas")
lines(fc, col = "blue", lwd = 2, lty = 2) 
legend("topleft",
       legend = c("Observado/Ajuste", "Pronóstico 1 año"),
       lty = c(1,2), col = c("black","blue"), bty = "n")
mtext("Pronostico a un año para empleabilidad en el sector manufacturero en Mexico.",
      side = 3, outer = TRUE, line = 1, cex = 1.5, font = 2)



adf_level <- tseries::adf.test(manufactura_ts, k = trunc((length(manufactura_ts)-1)^(1/3)))
print(adf_level)


adf_level <- tseries::adf.test(diff_12, k = trunc((length(diff_12)-1)^(1/3)))
print(adf_level)

ts_diff <- diff(diff_12, lag=1)
adf_level <- tseries::adf.test(ts_diff, k = trunc((length(ts_diff)-1)^(1/3)))
print(adf_level)


manufactura_arima <- auto.arima(manufactura_ts, seasonal = TRUE)
summary(manufactura_arima)


##BSTS ---------------------

ss <- list()
ss <- AddLocalLevel(ss, manufactura_ts)
ss <- AddSeasonal(ss, manufactura_ts, nseasons = 12)
fit <- bsts(manufactura_ts, state.specification = ss, niter = 5000, ping = 0)
summary(fit)

plot(fit)

# Pronóstico bayesiano con intervalos creíbles
fc <- predict(fit, horizon = h, burn = 1000)
plot(fc, main = "Pronóstico BSTS (bayesiano)")



#holt-winters vs arima vs arima bayesiano################################################
#holt------------------

train_hw <- HoltWinters (train, seasonal = "mult") 
train_hw$coef ; train_hw$SS


h <- length(test)
fc_hw <- predict(train_hw, n.ahead = h)


mse_hw <- mean((as.numeric(test) - as.numeric(fc_hw))^2, na.rm = TRUE)
print(mse_hw)

plot(train_hw, ylab = "Personas empleadas")
lines(fc_hw, col = "blue", lwd = 2, lty = 2) 
legend("topleft",
       legend = c("Observado/Ajuste", "Pronóstico 1 año"),
       lty = c(1,2), col = c("black","blue"), bty = "n")


#arima-----------------

fit_arima <- auto.arima(train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
fc_arima  <- forecast(fit_arima, h = h)

plot(fc_arima, main="Pronostico empleabilidad sector manufacturero Mexico 2025 ARIMA")

mse_arima <- mean((as.numeric(test) - as.numeric(fc_arima$mean))^2, na.rm = TRUE)
print(mse_arima)

fc_arima_mean <- fc_arima$mean


#mse arima bayseiano ----------------
# Priors: conjetura al 5% de la escala de la serie; prior moderado (sample.size=3)

y <- train

ss <- list()
ss <- AddLocalLinearTrend(ss, y) 
ss <- AddSeasonal(ss, y, nseasons = 12) 

set.seed(123)
fit_bayesian <- bsts(y, state.specification = ss, niter = 10000, ping = 0)

fc_bayesian <- predict(fit_bayesian, horizon = h, burn = 1000)
mse_bayesian <- mean((as.numeric(test) - as.numeric(fc_default$mean))^2, na.rm = TRUE)

print(mse_bayesian)

plot(fc_bayesian, main="Pronostico empleabilidad sector manufacturero Mexico 2025 BSTS")

comparar_mse <- function(mse_hw, mse_arima, mse_b) {
  valores <- c(HoltWinters = mse_hw,
               ARIMA       = mse_arima,
               Bayesiano   = mse_b)
  
  menor <- which.min(valores)
  list(
    modelo_ganador = names(valores)[menor],
    mse_minimo     = valores[menor],
    todos_los_mse  = valores
  )
}


resultado <- comparar_mse(mse_hw, mse_arima, mse_default)
resultado
print(mse_bayesian - mse_arima)


#original vs pronosticos
pronostico_media <- as.numeric(fc_bayesian$mean)

tiempo_pronostico <- time(manufactura_ts)[(length(manufactura_ts) - length(pronostico_media) + 1):length(manufactura_ts)]

# --- Generación del Gráfico ---


plot(manufactura_ts, 
     main = "Serie Original de Empleo vs. Pronóstico BSTS",
     xlab = "Tiempo",
     ylab = "Personas Empleadas", # Ajusta la etiqueta si usaste y_sc
     col = "darkblue",
     lwd = 2,
     type = "l",
     xlim = range(time(manufactura_ts)), # Asegura que el eje X cubra toda la serie
     ylim = range(c(manufactura_ts, fc_arima_mean), na.rm = TRUE)) # Asegura que el eje Y cubra ambas series

# Añadir el pronóstico al gráfico. Usamos 'points' o 'lines' con el tiempo correcto.
lines(tiempo_pronostico, 
      fc_arima_mean, 
      col = "red", 
      lwd = 2)


abline(v = time(manufactura_ts)[length(manufactura_ts) - length(fc_arima_mean)], 
       lty = 2, 
       col = "gray40")


#### real vs arima 
plot(manufactura_ts, 
     main = "Serie Original de Empleo vs. Pronóstico ARIMA",
     xlab = "Tiempo",
     ylab = "Personas Empleadas ", # Ajusta la etiqueta si usaste y_sc
     col = "darkblue",
     lwd = 2,
     type = "l",
     xlim = range(time(manufactura_ts)), # Asegura que el eje X cubra toda la serie
     ylim = range(c(manufactura_ts, fc_arima_mean), na.rm = TRUE)) # Asegura que el eje Y cubra ambas series

# Añadir el pronóstico al gráfico. Usamos 'points' o 'lines' con el tiempo correcto.
lines(tiempo_pronostico, 
      fc_arima_mean, 
      col = "green", 
      lwd = 2)


abline(v = time(manufactura_ts)[length(manufactura_ts) - length(fc_arima_mean)], 
       lty = 2, 
       col = "gray40")


# Leyenda
legend("topleft", 
       legend = c("Serie Original", "Pronóstico ARIMA"),
       col = c("darkblue", "green"),
       lwd = 2,
       bg = "white")



#pronostico 2026 modelo final

ss <- list()
ss <- AddLocalLinearTrend(ss_default, manufactura_ts) 
ss <- AddSeasonal(ss_default, manufactura_ts, nseasons = 12) 

set.seed(123)
fit <- bsts(manufactura_ts, state.specification = ss, niter = 10000, ping = 0)

fc_2026 <- predict(fit, horizon = 12, burn = 1000)
plot(fc_2026)














