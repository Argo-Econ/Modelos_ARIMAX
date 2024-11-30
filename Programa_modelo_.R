#------------------------------------------------------------------------------#
# Desarrollo de la met. Box Jenkings ----
## Modelos ARIMA media condicional
## Arturo Yesid Gonzalez ----
# ***************************************************************************** ----

# carga de librerias (funciones para el desarrollo practico)
require(pacman) # library(pacman)

p_load(readxl, sandwich, car, lmtest, TSstudio, lmtest, forecast
       , tseries, TSA, tsoutliers, GGally, xts, ggplot2, dplyr
       , MASS, nortest, FinTS, rugarch, Metrics)


# Importacion datos ----
#------------------------------------------------------------------------------#
Datos_ent1 <- read_xlsx(path = "Datos_ent/Bases_Modelos_ARIMA.xlsx"
                        ,sheet = "Actividad_Colombia",range = "a4:n236"
                        ,col_names = T)

tail(Datos_ent1)

Datos_ent2 <- read_xlsx(path = "Datos_ent/Bases_Modelos_ARIMA.xlsx"
                        ,sheet = "Exogenas",range = "a3:d435"
                        ,col_names = T)
tail(Datos_ent2)

Datos_ent3 <- read_xlsx(path = "Datos_ent/Bases_Modelos_ARIMA.xlsx"
                        ,sheet = "Monedas",range = "a4:w297"
                        ,col_names = T)

tail(Datos_ent3)



# Definicion de objetos de serie de tiempo ----
#------------------------------------------------------------------------------#

# Actividad comercio - variables de interes
Datos_ent1_ts1 <- ts(Datos_ent1[,-1],start = c(2005,1),frequency = 12)
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
Base_modelo_ts <- ts.union(Datos_ent1_ts1[,6],Datos_ent2_ts1)
tail(Base_modelo_ts)
colnames(Base_modelo_ts) <- c("ISE_Comercio","Brent","IP_Index","IPC_EEUU")
View(Base_modelo_ts)

Base_exo_pronos_ts <- tail(Base_modelo_ts[,-1],19)

Base_modelo_dep_ts <- Base_modelo_ts %>% na.omit()
head(Base_modelo_dep_ts)
tail(Base_modelo_dep_ts)  

# Base modelo con objetos xts (reto)
Base_modelo_xts <- cbind.xts(Datos_ent1_ts2$Comercio,Datos_ent2_ts2)
tail(Base_modelo_xts)
colnames(Base_modelo_xts) <- c("ISE_Comercio","Brent","IP_Index","IPC_EEUU")
View(Base_modelo_xts)

Base_exo_pronos_xts <- tail(Base_modelo_xts[,-1],19)

Base_modelo_dep_xts <- Base_modelo_xts %>% na.omit()
head(Base_modelo_dep_xts)
tail(Base_modelo_dep_xts)  



# 1. Identificación ----

## Elementos gráficos
#------------------------------------------------------------------------------#
ts_plot(Base_modelo_dep_ts
        ,type = "multiple"
        ,slider = T)

ts_plot(Base_modelo_dep_xts
        ,type = "multiple"
        ,slider = T)

# Comportamiento estacional de la variable objetivo
ts_seasonal(Base_modelo_dep_xts$ISE_Comercio, type = "all")
ts_cor(Base_modelo_dep_xts$ISE_Comercio, lag.max = 60) # con objetos xts no funciona
ts_cor(Base_modelo_dep_ts[,1], lag.max = 60)    # funciona con objetos ts


windows()
tsdisplay(Base_modelo_dep_ts[,1], main = "Actividad ISE Sector Comercio"
          , xlab = "Fecha", ylab = "Indice ISE")

ts_lags(Base_modelo_dep_ts[,1], lags = 1:18)
ts_lags(Base_modelo_dep_xts$ISE_Comercio, lags = 1:18)

adf.test(x = Base_modelo_dep_ts[,1],alternative = "stationary")

## Transformación Box-Cox ----
#------------------------------------------------------------------------------#

boxCox(lm(Base_modelo_dep_xts$ISE_Comercio~1),    # regresión var. de interes como regresor constante
       lambda = seq(-3, 3, 1/100), # secuencia de valores para lambda
       plotit = TRUE,  # Crear el emento grafico de contraste
       eps = 1/50,     # tolerancia sobre valor de lambda
       xlab = expression(lambda), # Valores para lambda
       ylab = "log-Likelihood",
       main ="Valor de lambda")

lambda_ISE_Comercio <- BoxCox.lambda(Base_modelo_dep_xts$ISE_Comercio, method = "loglik")
lambda_Brent <- BoxCox.lambda(Base_modelo_dep_xts$Brent, method = "loglik")
lambda_IP_index <- BoxCox.lambda(Base_modelo_dep_xts$IP_Index, method = "loglik")
lambda_IPC_EEUU <- BoxCox.lambda(Base_modelo_dep_xts$IPC_EEUU, method = "loglik")

# forma optima
lamdas_buff <- apply(Base_modelo_dep_xts, 2, function(x)  BoxCox.lambda(x,method = "loglik") )


# serie_transformada Box-Cox manual
ISE_Comercio_BoxCox <- BoxCox(Base_modelo_dep_xts$ISE_Comercio,lambda = lamdas_buff[1])
Brent_BoxCox <- BoxCox(Base_modelo_dep_xts$Brent,lambda = lambda_Brent)
IP_Index_BoxCox <- BoxCox(Base_modelo_dep_xts$IP_Index,lambda = lambda_IP_index)
IPC_EEUU_BoxCox <- BoxCox(Base_modelo_dep_xts$IPC_EEUU,lambda = lambda_IPC_EEUU)

Base_modelo_dep_xts_BoxCox <- cbind.xts(Base_modelo_dep_xts,ISE_Comercio_BoxCox
                                 ,Brent_BoxCox$Brent,IP_Index_BoxCox$IP_Index
                                 ,IPC_EEUU_BoxCox$IPC_EEUU)

names(Base_modelo_dep_xts_BoxCox)
colnames(Base_modelo_dep_xts_BoxCox) <- c("ISE_Comercio","Brent","IP_Index","IPC_EEUU"
                                          ,"ISE_Comercio_BoxCox","Brent_BoxCox"
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
tail(Base_modelo_dep_xts)

f_ini <- as.Date("2005-01-01")
f_end <- as.Date("2024-03-1")
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
adf.test(Base_modelo_dep_xts[,1],k=0) # Sounds weird!!!! 
adf.test(Base_modelo_dep_xts_bx[,1]) 


# aplicar diferencias a la informacion
Base_modelo_dep_xts_bx_diff <- Base_modelo_dep_xts_bx %>% diff(.,lag = 1,differences = 1) %>% na.omit()
tail(Base_modelo_dep_xts_bx_diff)


# vuelvo a testear estacionariedad
adf.test(Base_modelo_dep_xts_bx_diff[,1],k=0) # Se rechaza H0 -> serie estacionaria I(1)
                                              # se aplicó una diferencia, entonces d=1
kpss.test(Base_modelo_dep_xts_bx_diff[,1]) # H0: serie estacionaria
pp.test(Base_modelo_dep_xts_bx_diff[,1])   # H0: serie no estacionaria

plot(Base_modelo_dep_xts_bx_diff[,1])

## Transformación retornos log ----
# -----------------------------------------------------------------------------#

Base_modelo_dep_ts_dlx <- Base_modelo_dep_ts %>% log() %>% diff()
Base_modelo_dep_ts_slx <- Base_modelo_dep_ts %>% log() %>% diff(.,differences = 1,lag = 12) # Retornos log. anuales
ts_plot(Base_modelo_dep_ts_slx) 

# probar estacionariedad
adf.test(Base_modelo_dep_ts_dlx[,1])  # Rechaza H0
kpss.test(Base_modelo_dep_ts_dlx[,1]) # No Rechaza H0
pp.test(Base_modelo_dep_ts_dlx[,1])   # Rechaza H0

# -----------------------------------------------------------------------------#
# 1. que puedo aplicar transformaciones BoxCox para estabilizar
#    la varianza, y aplicar la oper. diferencia para estabilizar 
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
#     ARMA(4,5) , ARMA(6,4), ARMA(5,6) -> MA(6)



# Estimacion modelos -----
# -----------------------------------------------------------------------------#

## modelo 1 ----
mod1 <- Arima(y = Base_modelo_dep_ts_dlx[,1],order = c(4,0,5)
              ,method = "ML")
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
mod2 <- Arima(y = Base_modelo_dep_ts_dlx[,1],order = c(6,0,4), method = "CSS-ML")
summary(mod2)

### Chequeo mod2 ----
windows()
checkresiduals(mod2)

windows()
prueba_residuales(mod2$residuals)


## modelo 3 con exogenas ----
mod3 <- Arima(y = Base_modelo_dep_ts_dlx[,1],order = c(0,0,6))
summary(mod3)
checkresiduals(mod3)
mod3a <- Arima(y = Base_modelo_dep_ts_dlx[,1],order = c(4,0,5),seasonal = c(1,1,1) )
summary(mod3a)
### Chequeo mod3 con diferencia estacional ----
windows()
checkresiduals(mod3a)

windows()
prueba_residuales(mod3a$residuals)




## modelo 3a con exogenas ----
mod3b <- Arima(y = Base_modelo_dep_ts_dlx[,1],order = c(4,0,5)   # order = c(p,d,q)
              ,seasonal = c(1,1,1) # parte estacional (P,D,Q)
              ,xreg = Base_modelo_dep_ts_dlx[,-1])
summary(mod3b)


### Modelo 4 en niveles log con exogenas ----
mod4 <- Arima(y = log(Base_modelo_dep_ts[,1]), order = c(4,1,5)
              , seasonal = c(0,1,1)
              , xreg = log(Base_modelo_dep_ts[,-1])
              , method = "ML")

summary(mod4)
lmtest::coeftest(mod4)




## Modelo 5 auto.arima en logaritmos ----
mod5 <- auto.arima(y = log(Base_modelo_dep_ts[,1])
                   ,d = 1,max.order = 10,start.p = 2
                   ,trace = T,stepwise = F
                   ,xreg = log(Base_modelo_dep_ts[,-1])
                   ,approximation = T
                   ,allowdrift = T
                   ,allowmean = T)
summary(mod5)

### Chequeo modelo auto.arima en lx ----
windows()
checkresiduals(mod5)


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

outliers_ISE_Comercio <- tso((Base_modelo_dep_ts[,1])
                            , types = c("TC", "AO", "LS") )
windows()
plot(outliers_ISE_Comercio)
outliers_ISE_Comercio$yadj
## Ejemplos outliers ----
tc <- rep(0, nrow(log(Base_modelo_dep_ts)))
tc[184] <- 1

# cambio de nivel
ls <- stats::filter(tc, filter = 1, method = "recursive")
plot(ls)

# Cambuio de nivel
ao <- stats::filter(tc, filter = 0, method = "recursive")
ts_plot(ao)

# Cambio temporal - tracendente
tc_0_4 <- stats::filter(tc, filter = 0.4, method = "recursive")
tc_0_8 <- stats::filter(tc, filter = 0.8, method = "recursive")
tc_all <- cbind("TC_delta_0.4"= tc_0_4, "TC_delta_0.8"= tc_0_8)

ts_plot(tc_all, title = "Cambio transitorio")



# Outliers con serie de trabajo ----
outliers_ISE_Comercio
outliers_ISE_Comercio$outliers$coefhat

# Fechas en las que ocurrieron los outlier
outliers_idx <- outliers_ISE_Comercio$outliers$ind

# # Creación de los outliers
# n <- length(Base_modelo_dep_ts[,1])
# outlier1_tc1 <- outliers("TC", outliers_idx[1])


# Visualización de serie original y de la intervención
comparativo <- cbind("Intervenida"=outliers_ISE_Comercio$yadj
                     ,"Original"=(Base_modelo_dep_ts[,1]))
ts_plot(as.ts(comparativo))

# /------ fin intervencion --------------------------------------------

head(Base_modelo_dep_ts)
ISE_Comercio_interv <- ts(outliers_ISE_Comercio$yadj, start = c(2005,1)
                          ,frequency = 12)

Base_modelo_dep_ts_log <- log(ts.union((Base_modelo_dep_ts),ISE_Comercio_interv))
View(Base_modelo_dep_ts_log)

colnames(Base_modelo_dep_ts_log) <- c("ISE_Comercio","Brent","IP_Index"
                                      ,"IPC_EEUU","ISE_Comercio_interv")


## modelo 6 intervenida ----
mod6 <- auto.arima(y = Base_modelo_dep_ts_log[,5]
                   ,d = 1,max.order = 14,start.p = 2
                   ,trace = T,stepwise = F, allowdrift = F
                   ,xreg = Base_modelo_dep_ts_log[,c(-1,-5)])
summary(mod6)
lmtest::coeftest(mod6)
### Chequeo modelo auto.arima en lx ----
windows()
checkresiduals(mod6)

windows()
prueba_residuales(mod6$residuals)



# Pronostico (Uso del modelos) -----
# -----------------------------------------------------------------------------#


## Pronosticos libres sin exogenas ----
fore_mod1 <- forecast(mod1, h=21)
autoplot(fore_mod1)

fore_mod2 <- forecast(mod2, h=21)
autoplot(fore_mod2)


## Pronosticos con exogenas ----
Base_exo_pronos_ts
fore_mod3b <- forecast(mod3b, xreg = diff(log(Base_exo_pronos_ts)))
autoplot(fore_mod3b)

# pronos mod en niveles
fore_mod4 <- forecast(object = mod4,h = 21,level = c(60,70,90)
                      ,xreg = log(Base_exo_pronos_ts))
fore_mod4
windows()
autoplot(fore_mod4)

names(fore_mod4)
fore_mod4 <- fore_mod4$mean %>% as.data.frame() %>% exp()

# pronos mod6
fore_mod6 <- forecast(object = mod6, xreg = log(Base_exo_pronos_ts))

windows()
autoplot(fore_mod6)

# /------------------------------------------------------------------------  ---- 
# Final de programa ----
# /------------------------------------------------------------------------  ---- 


