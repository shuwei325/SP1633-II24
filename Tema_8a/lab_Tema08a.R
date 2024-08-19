
# Tema VIII: Modelos no lineales: ARCH, GARCH y extensiones -------

# 0. Librerías ---------------------------------------------------

library(forecast)
library(astsa)
library(fGarch)
library(tseries)
library(xts)
library(TSA)
library(car)
library(quantmod)

# 1. Ejemplos simulados ---------------------------------------------------

# ARCH(1)
set.seed(123456)
spec = garchSpec(model = list(gamma=0.01,alpha = c(0.8), beta = 0))
y1<-garchSim(spec, n = 1000)
ts.plot(y1)
acf2(y1)

ts.plot(y1^2)
acf2(y1^2)

# ARCH(2)
spec = garchSpec(model = list(alpha = c(0.2, 0.4), beta = 0))
y1<-garchSim(spec, n = 1000)
ts.plot(y1)
acf2(y1)

ts.plot(y1^2)
acf2(y1^2)


# GARCH(1,1)
spec = garchSpec(model = list(alpha = 0.2, beta = 0.4))
y1<-garchSim(spec, n = 2000)
ts.plot(y1)
acf2(y1)

ts.plot(y1^2)
acf2(y1^2)


# 2. Ejemplo: promedio diario industrial Dow Jone  --------

?djia
data(djia)

y<-djia$Close
plot(y)

ts.plot(y)
retorno<-diff(log(y))
ts.plot(retorno)

acf2(y)
acf2(retorno)

mod1 = Arima(retorno, order=c(1,0,1))
summary(mod1)
acf2(mod1$res)
checkresiduals(mod1,lag=20)

acf2(mod1$res^2)

# GARCH -------------------------------------------------------------------

retorno<-retorno[-1]
garch11 <- garchFit(retorno~garch(1,1), data=retorno)
suppressWarnings(garch11 <- garchFit(~garch(1,1), data=retorno))
str(garch11)
garch11@fit$matcoef

plot(garch11)   

plot(garch11,which=5)

pronostico1<-predict(garch11,plot=TRUE,n.ahead=10)

# ARMA-GARCH --------------------------------------------------------------

garch11 <- garchFit(retorno~arma(1,1)+garch(1,1), data=retorno)
suppressWarnings(garch11 <- garchFit(~arma(1,1)+garch(1,1), data=retorno))
str(garch11)
garch11@fit$matcoef

plot(garch11)    

plot(garch11,which=5)

pronostico1<-predict(garch11,plot=TRUE,n.ahead=10)

# 3. Ejemplo: CREF stock- 26/08/2004-15-08-2006 ---------------------------

data(CREF)
plot(CREF)

r.cref=diff(log(CREF))*100
plot(r.cref); abline(h=0)

plot(r.cref^2)

#Inspección de acf y pacf
acf2(r.cref)
acf2(r.cref^2)

#library(fGarch)
garch11.m <- garchFit(~garch(1,1), data=r.cref)
garch11 <- garchFit(~garch(1,1), data=r.cref,include.mean=FALSE)
garch11.m@fit$matcoef
garch11@fit$matcoef

garch11.m@fit$ics
garch11@fit$ics

plot(garch11)
