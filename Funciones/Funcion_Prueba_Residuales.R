#------------------------------------------------------------------------------------
# Funcion para graficar P-Value de la autocorrelacion, Heteroscedasticidad y
# Normalidad en residuales para todos los rezagos posibles de la serie de tiempo,
# junto con sus estadisticos de normalidad
# @arturo.gonzalez
#------------------------------------------------------------------------------------

# Paquetes requeridos:
# FinTS
# nortest 
# 

prueba_residuales<-function(ts,alpha=0.05){
  autocor_pvalue=NULL
  arch_pvalue=NULL
  for (i in 1:(length(ts)-2)){
    autocor_pvalue[i]=FinTS::AutocorTest(ts,lag = i,type = "Ljung-Box")$p.value
    arch_pvalue[i]=FinTS::ArchTest(ts,lags = i)$p.value
  }
  
  par(mfrow=c(2,2))
  #graficos y evaluaciones de los tests
  plot(ts,main="Residuales modelo",type="l",ylab="Residuales",col="darkblue")
  grid(lwd=1, nx=11, ny=11)
  plot(x=1:(length(ts)-2),cex=1.2,col="gray51",y=autocor_pvalue,ylim=c(0,1),xlab="Lags",ylab="P-Value"
       ,main="Autocorrelaci?n Test de Ljung-Box \nH0: No Autocorrelacion")
  grid(lwd=1, nx=11, ny=11)
  lines(x=1:(length(ts)-2),y=rep(alpha,(length(ts)-2)),col="red",type="l",lty=2,lwd=2)
  plot(x=1:(length(ts)-2),cex=1.2,col="gray51",y=arch_pvalue,ylim=c(0,1),xlab="Lags",ylab="P-Value"
       ,main="Heteroced?sticidad efectos ARCH  Test\nH0: No heterocedasticidad")
  grid(lwd=1, nx=11, ny=11)
  lines(x=1:(length(ts)-2),y=rep(alpha,(length(ts)-2)),col="red",type="l",lty=2,lwd=2)
  coord=qqnorm(ts,plot.it = F)
  shapiro=shapiro.test(ts)
  lillie=nortest::lillie.test(ts)
  qqnorm(ts,cex=1.2,col="gray51",main="Q-Q Plot\nH0: Datos comp. normal");qqline(ts,col="red",lwd=2);grid(lwd=1, nx=11, ny=11)
  text(-2,quantile(coord$y,0.85),paste("Shapiro Test:","\nP-Value =",round(shapiro$p.value,6)),font=2)
  text(2,quantile(coord$y,0.15),paste("Lilliefors Test:","\nP-Value =",round(lillie$p.value,6)),font=2)
}

# ------------------------------------------------------------------------------------