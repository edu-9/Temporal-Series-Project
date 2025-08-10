library(forecast)
dados = read.csv("dadostrabalho.csv",header=T, na.strings = ":")
#retirar coluna não importantes e linhas com vc ou vd
dados=dados[,-(3:5)]
dados=dados[,-(4:5)]
dados=dados[-(93:235),]
dados=dados[-(1:3),]

#analise descritiva
summary(dados[,3])
hist(dados[,3],main="Histograma do volume afluente mensal na albufeira da valeira",xlab="volume afluente (dam3)")
boxplot(dados[,3],main="Boxplot do volume afluente mensal na albufeira da valeira")
#série temporal é discreta espacço de estados R+
seriet = ts(dados[,3],start=c(2001,8),frequency=12)
seriet
plot(seriet,main="volume afluente mensal na albufeira da valeira")
#preencher 2 valores NA
seriecompleta=na.interp(seriet)
seriecompleta
plot(seriecompleta,main="Volume afluente mensal na albufeira da Valeira",ylab="volume afluente (dam3)")
#modelo proposto pelo R serie completa
auto.arima(seriecompleta)
#serie log
#serielog=log(seriecompleta)
#plot(serielog)
#plot(decompose(serielog))
#m0=auto.arima(serielog)
#plot(forecast(m0))

#serie com transformação box cox
lambda <- BoxCox.lambda(seriecompleta,lower=-1,method = "loglik")
boxcox=BoxCox(seriecompleta, lambda = lambda)
plot(boxcox)
plot(decompose(boxcox))
m0=auto.arima(boxcox)
plot(forecast(m0))
shapiro.test(m0$residuals)
acf(m0$residuals)
pacf(m0$residuals)
#resumo dados
summary(dados[,3])
#desvio padrao e variância
sd(dados[,3],na.rm = T)
var(dados[,3],na.rm=T)
#hist e boxplot
hist(dados[,3],main="Histograma volume afluente mensal na albufeira da Valeira" ,xlab="volume (dam3)")
boxplot(dados[,3],main="Boxplot volume afluente mensal na albufeira da Valeira" ,ylab="volume(dam3)")
boxplot(boxcox)
#decompose
plot(decompose(seriecompleta)) 
plot(decompose(boxcox))
dec=decompose(boxcox)
#FAC e FACP serie não logaritmizada
acf(seriecompleta,main="FAC",na.action = na.omit) 
acf(seriecompleta, plot=FALSE,na.action = na.omit)
pacf(seriecompleta , main="FACP",na.action = na.omit)
pacf(seriecompleta, plot=FALSE,na.action = na.omit)

#FAC e FACP serie logaritmizada
acf(boxcox,main="FAC",na.action = na.omit) 
acf(boxcox, plot=FALSE,na.action = na.omit)
pacf(boxcox , main="FACP",na.action = na.omit)
pacf(boxcox, plot=FALSE,na.action = na.omit)

#estabilização da série temporal
#variância já foi estabilizada
#d=?
auxiliar = arima(boxcox, order=c(0,0,0), seasonal=list(order=c(0,0,0), period=0))
plot(auxiliar$residuals, main="apos diferenciacao")
#plot(diff(boxcox,0))
#uaremos d=0

#s=?
periodogram=spectrum(boxcox, plot=F)
imax=which.max(periodogram$spec)
periodo=1/periodogram$freq[imax]
periodo
#apesar de propor 3.75 usaremos 12 


#names(dec)
#st=dec$x-dec$trend
#plot(st)
#m0=auto.arima(st)
#plot(forecast(m0))
mm=arima(boxcox, order=c(1,0,0), seasonal=list(order=c(2,1,0), period=12))
acf(mm$residuals)
pacf(mm$residuals)
shapiro.test(mm$residuals)
#determinar D 
arima(boxcox, order=c(0,0,0), seasonal=list(order=c(1,0,0), period=12))
#como coeficiente não está proximo de 1, D=0

#determinar P e Q
arima(boxcox, order=c(0,0,0), seasonal=list(order=c(1,0,0), period=12))
#aic=314 e coeficientes significativos
arima(boxcox, order=c(0,0,0), seasonal=list(order=c(1,0,1), period=12))
#aic=306 e coeficientes significativos
arima(boxcox, order=c(0,0,0), seasonal=list(order=c(0,0,1), period=12))
#aic=310 e coeficientes significativos
arima(boxcox, order=c(0,0,0), seasonal=list(order=c(2,0,1), period=12))
#aic=312 e coeficientes nao significativos
arima(boxcox, order=c(0,0,0), seasonal=list(order=c(1,0,2), period=12))
#aic= 308 nao significativos
#P=1 e Q=1

#determinar p e q
arima(boxcox, order=c(1,0,0), seasonal=list(order=c(1,0,1), period=12))
#aic=244 significativos
arima(boxcox, order=c(0,0,1), seasonal=list(order=c(1,0,1), period=12))
#aic=255
arima(boxcox, order=c(1,0,1), seasonal=list(order=c(1,0,1), period=12))
#aic=246
arima(boxcox, order=c(2,0,1), seasonal=list(order=c(1,0,1), period=12))
#aic=244.88 n significativo
arima(boxcox, order=c(1,0,2), seasonal=list(order=c(1,0,1), period=12))
#aic=248.4
arima(boxcox, order=c(2,0,2), seasonal=list(order=c(1,0,1), period=12))
#aic=246
arima(boxcox, order=c(2,0,3), seasonal=list(order=c(1,0,1), period=12))
#aic=246
#p=1 q=0

#concluindo optamos pelo modelo SARIMA(1,0,0)x(1,0,1)s=12
modelo=arima(boxcox, order=c(1,0,0), seasonal=list(order=c(1,0,1), period=12))
names(modelo)
#intervalos de confiança dos coeficientes
modelo$coef[1] + c(-1,1) * qnorm(0.975) * sqrt(modelo$var.coef[1,1])
modelo$coef[2] + c(-1,1) * qnorm(0.975) * sqrt(modelo$var.coef[2,2])
modelo$coef[3] + c(-1,1) * qnorm(0.975) * sqrt(modelo$var.coef[3,3])

#nenhum intervalo contém 0 logo todos os coeficientes são estatisticamente significativos

# variÃ¢ncia estimada dos erros
sqrt(modelo$sigma2)
modelo
resi=modelo$residuals

layout(matrix(c(1,2,3,4),2,byrow=T))
plot(as.vector(resi), main="Representação gráfica dos resíduos", ylab="")
#residuos são homocedasticos (n dependem de t)
hist(resi, freq=F, main="Histograma dos resíduos", xlab="resíduos", ylab="frequência relativa")
curve(dnorm(x, mean(resi), sd(resi)), add=T)
acf(resi, main="FAC dos resíduos", ylab="FAC")
pacf(resi, main="FACP dos resíduos", ylab="FACP")

#correlação dos residuos
Box.test(resi,lag = 1) #p-value = 0.5016>0.05 nao rejeito H0, os residuos sao independentes
?Box.test

# normalidade
shapiro.test(resi) #p-value = 0.6717>0.05, não rejeito H0


ks.test(resi, "pnorm", mean=mean(resi), sd=sd(resi))
#p-value = 0.988>0.05 não rejeito H0 e logo residuos seguem uma dist normal


# media igual a 0
t.test(resi, mu=0) #p-value = 0.8617>0.05
# não se rejeita H0 e logo a media é igual a 0

#logo os resíduos são Ruido Branco


####PREVISAO####
library(forecast)


#observados vs estimados (boxcox)
est.dad = fitted.values(modelo)
res.dad = boxcox- est.dad
mean(sum(res.dad^2)) #eqm
mean(sum(res.dad1^2))
mean(sum(abs(res.dad))) #eqm
mean(sum(abs(res.dad1)))
ts.plot(boxcox, main = "Dados Transformados com lambda=0.05")
lines(ts(est.dad, start=c(2001,8), frequency=12), col="red")
est.dad1 = fitted.values(mm)
res.dad1 = boxcox- est.dad1
lines(ts(est.dad1, start=c(2001,8), frequency=12), col="blue")
#dados estimados dao um pouco ao lado. normal?
modelo
## observados vs estimados (original)
est = InvBoxCox(est.dad,lambda)
inver<- round(est, digits = 0)
res =  seriecompleta - est
ts.plot(seriecompleta, main = "Dados originais")
lines(ts(inver, start=c(2001,8), frequency=12), col="red")
#problema igual aos valorws com transformação

# Forecasting para 2007 e 2008
modelo1=arima(boxcox[1:65], order=c(1,0,0), seasonal=list(order=c(1,0,1), period=12))
previsoes<- predict(modelo1, n.ahead = 24) #previsões com dados transformados
inv_prev<-InvBoxCox(previsoes$pred,lambda)
inver_prev<- round(inv_prev, digits = 0) #previsões com dados originais

var(seriecompleta[66:89]-inver_prev)
#intervalos de previsão - dados transformados
low = previsoes$pred - 1.96*previsoes$se
upper = previsoes$pred + 1.96*previsoes$se
head(low)
head(upper)
head(previsoes$pred)
length(low)

#intervalos de previsão - dados originais
low2 = round(InvBoxCox(low,lambda), digits = 0)
upper2 = round(InvBoxCox(upper,lambda), digits = 0)


# gráfico de previsão e estimados vs observados -  dados transformados
ts.plot(boxcox, main = "Dados Transformados", xlim=c(2001.8,2009),ylim=c(15,24))
lines(ts(previsoes$pred, start=c(2007,1), frequency=12), col="blue")
lines(ts(low, start=c(2007,1), deltat=1/12), lty=3, col="blue")
lines(ts(upper,start=c(2007,1), deltat=1/12), lty=3, col="blue")
est.dad = fitted.values(modelo1)
lines(ts(est.dad, start=c(2001,8), frequency=12), col="red")

# gráfico de previsão e estimados vs observados -  dados originais
ts.plot(seriecompleta, main = "Dados originais", ylab="volume (dam3)", xlim=c(2001.8,2009),ylim=c(0,3700000))
lines(ts(inver_prev, start=c(2007,1), frequency=12), col="blue")
lines(ts(low2, start=c(2007,1), deltat=1/12), lty=3, col="blue")
lines(ts(upper2, start=c(2007,1), deltat=1/12), lty=3, col="blue")
est1 = InvBoxCox(est.dad,lambda)
lines(ts(est1, start=c(2001,8), frequency=12), col="red")
legend("topleft", legend = c("Série observada", "Estimativas", "Previsões", "Intervalos de previsão (95%)"), col = c("black", "red", "blue", "blue"), lty = c(1,1,1,2), bty="n")


?lines

# Forecasting para 2009 até 2021 (dados apartir de 2008 sao vd ou vc)
plot(boxcox,main="volume afluente mensal na albufeira da valeira")
modelo=arima(boxcox, order=c(1,0,0), seasonal=list(order=c(1,0,1), period=12))
previsoes2009<- predict(modelo, n.ahead = 156) #previsões com dados transformados
inv_prev2009<-InvBoxCox(previsoes2009$pred,lambda)
inver_prev2009<- round(inv_prev2009, digits = 0) #previsões com dados originais

#intervalos de previsão - dados transformados
low2009 = previsoes2009$pred - 1.96*previsoes2009$se
upper2009 = previsoes2009$pred + 1.96*previsoes2009$se

#intervalos de previsão - dados originais
low3 = round(InvBoxCox(low2009,lambda), digits = 0)
upper3 = round(InvBoxCox(upper2009,lambda), digits = 0)

#tornar os dados totais numa serie
dadostotais = read.csv("dadostrabalho.csv",header=T, na.strings = ":")
dadostotais=dadostotais[,-(2:5)]
dadostotais=dadostotais[,-(3:4)]
dadostotais=dadostotais[-(1:3),]
serietotal=ts(dadostotais[,2],start=c(2001,8),frequency=12)
plot(serietotal)
serietotalcompleta=na.interp(serietotal)
serietotalcompleta
plot(serietotalcompleta,main="volume afluente mensal na albufeira da valeira")
# gráfico de previsão e estimados vs observados -  dados transformados
ts.plot(boxcox, main = "Dados Transformados", xlim=c(2001.8,2021))

lines(ts(previsoes2009$pred, start=c(2009,1), frequency=12), col="blue")
lines(ts(low2009, start=c(2009,1), deltat=1/12), lty=3, col="blue")
lines(ts(upper2009,start=c(2009,1), deltat=1/12), lty=3, col="blue")

# gráfico de previsão e estimados vs observados -  dados originais
ts.plot(serietotalcompleta, main = "Dados originais", ylab="volume (dam3)", xlim=c(2001.8,2022))
lines(ts(inver_prev2009, start=c(2009,1), frequency=12), col="blue")
lines(ts(low3, start=c(2009,1), deltat=1/12), lty=3, col="blue")
lines(ts(upper3, start=c(2009,1), deltat=1/12), lty=3, col="blue")
legend("topleft", legend = c("Série observada", "Estimativas", "Previsões", "Intervalos de previsão (95%)"), col = c("black", "red", "blue", "blue"), lty = c(1,1,1,2), bty="n")

