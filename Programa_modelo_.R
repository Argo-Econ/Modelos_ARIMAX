#------------------------------------------------------------------------------#
# Desarrollo de la met. Box Jenkings ----
## Modelos ARIMA media condicional
## Arturo Yesid Gonzalez ----
# ***************************************************************************** ----

# carga de librerias (funciones para el desarrollo practico)
require(pacman) # library(pacman)

p_load(readxl, sandwich, car, lmtest, TSstudio, lmtest, forecast
       , tseries, TSA, tsoutliers, GGally, xts, ggplot2, dplyr
       , MASS, nortest, nortest, FinTS, rugarch, Metrics )


# Importacion datos ----
#------------------------------------------------------------------------------#
Datos_ent1 <- read_xlsx(path = "Datos_ent/Bases_Modelos_ARIMA.xlsx"
                        ,sheet = "Acciones",range = "a4:bq406"
                        ,col_names = T)

tail(Datos_ent1)

Datos_ent2 <- read_xlsx(path = "Datos_ent/Bases_Modelos_ARIMA.xlsx"
                        ,sheet = "Exogenas",range = "a3:d423"
                        ,col_names = T)
tail(Datos_ent2)

Datos_ent3 <- read_xlsx(path = "Datos_ent/Bases_Modelos_ARIMA.xlsx"
                        ,sheet = "Monedas",range = "a4:w286"
                        ,col_names = T)

tail(Datos_ent3)



# Definicion de objetos de serie de tiempo ----
#------------------------------------------------------------------------------#

# Acciones - variables de interes
Datos_ent1_ts1 <- ts(Datos_ent1[,-1],start = c(1990,1),frequency = 12)
Datos_ent1_ts2 <- xts(Datos_ent1[,-1]
                      ,order.by = as.Date(Datos_ent1$Fecha))

# Exogenas
Datos_ent2_ts1 <- ts(Datos_ent2[,-1],start = c(1990,1),frequency = 12)
Datos_ent2_ts2 <- xts(Datos_ent2[,-1]
                      ,order.by = as.Date(Datos_ent2$Fecha))

# Monedas
Datos_ent3_ts1 <- ts(Datos_ent3[,-c(1:3)],start = c(2000,1),frequency = 12)
Datos_ent3_ts2 <- xts(Datos_ent3[,-c(1:3)]
                      ,order.by = as.Date(Datos_ent3$Fecha))


## Creacion de la base de modelacion ----
#------------------------------------------------------------------------------#

# Objeto ts
Base_modelo_ts <- ts.union(Datos_ent3_ts1[ , 1],Datos_ent2_ts1)
tail(Base_modelo_ts)
colnames(Base_modelo_ts) <- c("Colombia","Brent","IP_Index","IPC_EEUU")
View(Base_modelo_ts)

Base_exo_pronos_ts <- tail(Base_modelo_ts[,-1],19)

Base_modelo_dep_ts <- Base_modelo_ts %>% na.omit()
head(Base_modelo_dep_ts)
tail(Base_modelo_dep_ts)  

# Base modelo con objetos xts (reto)
Base_modelo_xts <- cbind.xts(Datos_ent3_ts2[,1],Datos_ent2_ts2)
tail(Base_modelo_xts)
colnames(Base_modelo_xts) <- c("Colombia","Brent","IP_Index","IPC_EEUU")
View(Base_modelo_xts)

Base_exo_pronos_xts <- tail(Base_modelo_xts[,-1],19)

Base_modelo_dep_xts <- Base_modelo_xts %>% na.omit()
head(Base_modelo_dep_xts)
tail(Base_modelo_dep_xts)  



# Identificación ----

## Elementos gráficos
#------------------------------------------------------------------------------#
ts_plot(Base_modelo_dep_ts
        ,type = "multiple"
        ,slider = T)

ts_plot(Base_modelo_dep_xts
        ,type = "multiple"
        ,slider = T)

# Comportamiento estacional de la variable objetivo
ts_seasonal(Base_modelo_dep_xts$Colombia, type = "all")
ts_cor(Base_modelo_dep_xts$Colombia, lag.max = 60) # con objetos xts no funciona
ts_cor(Base_modelo_dep_ts[,1], lag.max = 60)    # funciona con objetos ts


windows()
tsdisplay(Base_modelo_dep_ts[,1], main = "Tasa de Cambio Peso-Dólar"
          , xlab = "Fecha", ylab = "TRM")

ts_lags(Base_modelo_dep_ts[,1], lags = 1:18)
ts_lags(Base_modelo_dep_xts$Colombia, lags = 1:18)

adf.test(x = Base_modelo_dep_ts[,1],alternative = "stationary")

## Transformación Box-Cox ----
#------------------------------------------------------------------------------#

boxCox(lm(Base_modelo_dep_xts$Colombia~1),    # regresión var. de interes como regresor constante
       lambda = seq(-3, 3, 1/100), # secuencia de valores para lambda
       plotit = TRUE,  # Crear el emento grafico de contraste
       eps = 1/50,     # tolerancia sobre valor de lambda
       xlab = expression(lambda), # Valores para lambda
       ylab = "log-Likelihood",
       main ="Valor de lambda")

lambda_colombia <- BoxCox.lambda(Base_modelo_dep_xts$Colombia, method = "loglik")
lambda_Brent <- BoxCox.lambda(Base_modelo_dep_xts$Brent, method = "loglik")
lambda_IP_index <- BoxCox.lambda(Base_modelo_dep_xts$IP_Index, method = "loglik")
lambda_IPC_EEUU <- BoxCox.lambda(Base_modelo_dep_xts$IPC_EEUU, method = "loglik")

# forma optima
lamdas_buff <- apply(Base_modelo_dep_xts, 2, function(x)  BoxCox.lambda(x,method = "loglik") )


# serie_transformada Box-Cox manual
Colombia_BoxCox <- BoxCox(Base_modelo_dep_xts$Colombia,lambda = lamdas_buff[1])
Brent_BoxCox <- BoxCox(Base_modelo_dep_xts$Brent,lambda = lambda_Brent)
IP_Index_BoxCox <- BoxCox(Base_modelo_dep_xts$IP_Index,lambda = lambda_IP_index)
IPC_EEUU_BoxCox <- BoxCox(Base_modelo_dep_xts$IPC_EEUU,lambda = lambda_IPC_EEUU)

Base_modelo_dep_xts_BoxCox <- cbind.xts(Base_modelo_dep_xts,Colombia_BoxCox$Colombia
                                 ,Brent_BoxCox$Brent,IP_Index_BoxCox$IP_Index
                                 ,IPC_EEUU_BoxCox$IPC_EEUU)

names(Base_modelo_dep_xts_BoxCox)
colnames(Base_modelo_dep_xts_BoxCox) <- c("Colombia","Brent","IP_Index","IPC_EEUU"
                                          ,"Colombia_BoxCox","Brent_BoxCox"
                                          ,"IP_Index_BoxCox","IPC_EEUU_BoxCox")

tail(Base_modelo_dep_xts_BoxCox)
# variable IP_Index se transformó para mal ¿?

# forma optima
Base_modelo_dep_xts_bx <- apply(Base_modelo_dep_xts
                                , 2, function(y) BoxCox(y
                                                        ,BoxCox.lambda(y,method = "loglik")) )
ts_plot(Base_modelo_dep_xts_BoxCox)

# Mensajes:
# la transformación en BoxCox debe mirarse con cuidado evaluando su ajuste
# en términos de la varianza.
# Mayor detalle ver: https://onlinestatbook.com/2/transformations/box-cox.html
# -----------------------------------------------------------------------------#

# Se perdió el atributo de serie de tiempo al efectuar transformación optima
class(Base_modelo_dep_xts_bx)
head(Base_modelo_dep_xts)

f_ini <- as.Date("2000-01-01")
f_end <- as.Date("2023-06-1")
fechas <- seq(f_ini, f_end, by = "month")

Base_modelo_dep_xts_bx <- xts(Base_modelo_dep_xts_bx,order.by = fechas)
tail(Base_modelo_dep_xts_bx)

ts_plot(Base_modelo_dep_xts_bx
        ,type = "multiple"
        ,slider = T
        ,title = "Base con transformación BoxCox")



# la transformacion Box Cox atenua la varianza
var(Base_modelo_dep_xts[,1])
var(Base_modelo_dep_xts_bx[,1])

# Comprobar que la serie se estacionaria (prueba de raiz unitaria)
adf.test(Base_modelo_dep_xts[,1]) # sobre el indice de Colombia la H0 no se rechaza
adf.test(Base_modelo_dep_xts_bx[,1]) # sobre chile boxcox h0 no se rechaza


# aplicar diferencias a la informacion
Base_modelo_dep_xts_bx_diff <- Base_modelo_dep_xts_bx %>% diff() %>% na.omit()
tail(Base_modelo_dep_xts_bx_diff)


# vuelvo a testear estacionariedad
adf.test(Base_modelo_dep_xts_bx_diff[,1],k=1) # Se rechaza H0 -> serie estacionaria I(1)
                                              # se aplicó una diferencia, entonces d=1
kpss.test(Base_modelo_dep_xts_bx_diff[,1]) # H0: serie estacionaria
pp.test(Base_modelo_dep_xts_bx_diff[,1])   # H0: serie no estacionaria


## Transformación retornos log ----
# -----------------------------------------------------------------------------#

Base_modelo_dep_ts_dlx <- Base_modelo_dep_ts %>% log() %>% diff()
#Base_modelo_dep_ts_dlx <- diff(log(Base_modelo_dep_ts))

# probar estacionariedad
adf.test(Base_modelo_dep_ts_dlx[,1])  # Rechaza H0
kpss.test(Base_modelo_dep_ts_dlx[,1]) # No Rechaza H0
pp.test(Base_modelo_dep_ts_dlx[,1])   # Rechaza H0

# -----------------------------------------------------------------------------#
# 1. que puedo aplicar transformaciones BoxCox para estabilizar
#    la varianza, y aplicar la diferencia para estabilizar 
#    la tendencia o media de la serie
# 2. valor del parametro d=? es uno porque se aplicó una diferencia para
#    convertir la serie en estacionaria

# atajo tanto para diferencias directas como diferencias estacionales
ndiffs(Base_modelo_dep_ts[,1]) # el numero d para colocarlo en el modelo ARIMA(p,d,q)
nsdiffs(Base_modelo_dep_ts[,1])

# -----------------------------------------------------------#
# Segundo paso en la identificacion (graficos de FAC y PACF)
# estructura AR y MA

windows()
tsdisplay(Base_modelo_dep_ts_dlx[,1])
# posible modelo AR=1


grafico1 <- autoplot(Base_modelo_dep_ts[,1]) +ylab("indice")+xlab("fecha")
grafico2 <- autoplot(Base_modelo_dep_ts_dlx[,1]) +ylab("retornos")+xlab("fecha")
grafico3 <- Acf(Base_modelo_dep_ts[,1]) %>% autoplot() + labs(x='rezago'
                                                          ,y='FAC serie original')
grafico4 <- Pacf(Base_modelo_dep_ts[,1]) %>% autoplot() + labs(x='rezago'
                                                           ,y='FACP serie original')
grafico5 <- Acf(Base_modelo_dep_ts_dlx[,1]) %>% autoplot() + labs(x='rezago'
                                                          ,y='FAC serie en retornos')
grafico6 <- Pacf(Base_modelo_dep_ts_dlx[,1]) %>% autoplot() + labs(x='rezago'
                                                           ,y='FACP serie en retornos')
windows()
gridExtra::grid.arrange(grafico1,grafico3
                        ,grafico4,grafico2
                        ,grafico5,grafico6,
                        ncol=3)

eacf(Base_modelo_dep_ts_dlx[,1],ar.max = 10, ma.max = 10)


# Conclusion:
# 1. existen unos posibles candidatos a modelar
#     MA(1) - ARMA(0,1) , ARMA(3,3), ARMA(8,6)



# Estimacion modelos -----
# -----------------------------------------------------------------------------#

## modelo 1 ----
mod1 <- Arima(y = Base_modelo_dep_ts_dlx[,1],order = c(0,0,1),method = "CSS")
summary(mod1)
lmtest::coeftest(mod1)

## Carga de función eval residuales ----
source("Funciones/Funcion_Prueba_Residuales.r")

### Chequeo mod1 ----
windows()
checkresiduals(mod1)

windows()
prueba_residuales(mod1$residuals)

## modelo 2 ----
mod2 <- Arima(y = Base_modelo_dep_ts_dlx[,1],order = c(3,0,3), method = "CSS-ML")
summary(mod2)

### Chequeo mod2 ----
windows()
checkresiduals(mod2)

windows()
prueba_residuales(mod2$residuals)


## modelo 3 con exogenas ----
mod2a <- Arima(y = Base_modelo_dep_ts_dlx[,1],order = c(3,0,3)
              ,xreg = Base_modelo_dep_ts_dlx[,-1])
summary(mod2a)

### Chequeo mod3 con exogenas ----
windows()
checkresiduals(mod2a)

windows()
prueba_residuales(mod2a$residuals)


## modelo 4 niveles y exogenas ----
mod3b <- Arima(y = log(Base_modelo_dep_ts[,1]),order = c(3,1,3)    # ndiffs(log(Base_modelo_dep_ts[,1])) para saver que colocar en d c(x,d,x)
              ,xreg = log(Base_modelo_dep_ts[,-1]))
summary(mod3b)
"ndiffs(log(Base_modelo_dep_ts[,1])) para saber que colocar en d c(x,d,x)"
### Chequeo mod4 niveles y exogenas ----
windows()
checkresiduals(mod3b)

windows()
prueba_residuales(mod4$residuals)


## modelo 4 ajuste manual ----
mod4 <- Arima(y = log(Base_modelo_dep_ts[,1]),order = c(6,1,8)
              ,seasonal = c(0,0,0)    # Seasonal (P,D,Q)
              ,xreg = log(Base_modelo_dep_ts[,-1]))
summary(mod4)
lmtest::coeftest(mod4)
### Chequeo modelo 5 ajuste ----
windows()
checkresiduals(mod4)

windows()
prueba_residuales(mod4$residuals)


## modelo 6 auto.arima en lx ----
mod6 <- auto.arima(y = log(Base_modelo_dep_ts[,1])
                   ,d = 1,max.order = 14,start.p = 2
                   ,trace = T,stepwise = F
                   ,xreg = log(Base_modelo_dep_ts[,-1]))
summary(mod6)

### Chequeo modelo auto.arima en lx ----
windows()
checkresiduals(mod6)

windows()
prueba_residuales(mod6$residuals)


# Mensaje,
# En terminos de la varianza los modelos son pobres, hay outliers 
# por corregir


# Analisis de intervencion ----
# Detección de outliers
# tipos de outliers
# -----------------------------------------------------------------------------#

# - Additive outliers (AO)      - función pulso
# - Level Shift       (LS)
# - Transient change  (TC)      - Cambio de nivel
# - Innovation Ouliers (IO)     - Cambio progresivo
# - Seasonal level Shifts (SLS)

ts_plot(log(Base_modelo_dep_ts[,1]))

outliers_colombia <- tso((Base_modelo_dep_ts[,1])
                    , types = c("TC", "AO", "LS") )
windows()
plot(outliers_colombia)

## Ejemplos outliers ----
tc <- rep(0, nrow(log(Base_modelo_dep_ts)))
tc[110] <- 1

# cambio de nivel
ls <- stats::filter(tc, filter = 1, method = "recursive")
plot(ls)

# pulso
ao <- stats::filter(tc, filter = 0, method = "recursive")
ts_plot(ao)

# Cambio temporal - tracendente
tc_0_4 <- stats::filter(tc, filter = 0.4, method = "recursive")
tc_0_8 <- stats::filter(tc, filter = 0.8, method = "recursive")
tc_all <- cbind("TC_delta_0.4"= tc_0_4, "TC_delta_0.8"= tc_0_8)

ts_plot(tc_all, title = "Cambio transitorio")



# Outliers con serie de trabajo ----
outliers_colombia
outliers_colombia$outliers$coefhat

# Fechas en las que ocurrieron los outlier
outliers_idx <- outliers_colombia$outliers$ind

# Creación de los outliers
n <- length(Base_modelo_dep_ts[,1])
outlier1_tc1 <- outliers("TC", outliers_idx[1])
outlier2_tc2 <- outliers("LS", outliers_idx[2])

outlier1_tc <- outliers.effects(outlier1_tc1, n)
outlier2_tc <- outliers.effects(outlier2_tc2, n)

# Unión de las series de outliers
outliers_gral <- cbind(outlier1_tc,outlier2_tc)
ts_plot(as.ts(outliers_gral), type="multiple")

# Visualización de serie original y de la intervención
comparativo <- cbind("Intervenida"=outliers_colombia$yadj,"Original"=(Base_modelo_dep_ts[,1]))
ts_plot(as.ts(comparativo))

# /------ fin intervencion --------------------------------------------

head(Base_modelo_dep_ts)
Colombia_interv <- ts(outliers_colombia$yadj, start = c(2000,1)
                   ,frequency = 12)

Base_modelo_dep_ts_log <- ts.union((Base_modelo_dep_ts),Colombia_interv)
View(Base_modelo_dep_ts_log)

colnames(Base_modelo_dep_ts_log) <- c("Colombia","Brent","IP_Index"
                                      ,"IPC_EEUU","Colombia_interv")


## modelo 7 intervenida ----
mod7 <- auto.arima(y = Base_modelo_dep_ts_log[,5]
                   ,d = 1,max.order = 14,start.p = 2
                   ,trace = T,stepwise = F, allowdrift = F
                   ,xreg = Base_modelo_dep_ts_log[,c(-1,-5)])
summary(mod7)

### Chequeo modelo auto.arima en lx ----
windows()
checkresiduals(mod7)

windows()
prueba_residuales(mod7$residuals)


# Pronostico (Uso del modelos) -----
# -----------------------------------------------------------------------------#


## Pronosticos libres sin exogenas ----
fore_mod1 <- forecast(mod1, h=19)
autoplot(fore_mod1)

fore_mod2 <- forecast(mod2, h=19)
autoplot(fore_mod2)


## Pronosticos con exogenas ----
Base_exo_pronos_ts
fore_mod3 <- forecast(mod3, xreg = diff(log(Base_exo_pronos_ts)))
autoplot(fore_mod3)

fore_mod2a <- forecast(mod2a, xreg = log(Base_exo_pronos_ts))
autoplot(fore_mod2a)

fore_mod3b <- forecast(mod3b, xreg = log(Base_exo_pronos_ts))
autoplot(fore_mod3b)

fore_mod4 <- forecast(mod4, xreg = log(Base_exo_pronos_ts))
autoplot(fore_mod4)

fore_mod7 <- forecast(mod7, xreg = log(Base_exo_pronos_ts))
autoplot(fore_mod7)



# /------------------------------------------------------------------------  ---- 
# Final de programa ----
# /------------------------------------------------------------------------  ---- 


