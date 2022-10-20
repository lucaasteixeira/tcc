library(fpp2)
library(ggplot2)
library(forecast)
library(latex2exp)
library(urca)
library(caschrono)
library(DT)
library(forecast)
library(fpp2)
library(dygraphs)
library(BETS)
library(urca)
library(TSA)
library(FinTS)
library(normtest)
library(tseries)
library(astsa)
library(foreign)
library(quantmod)
library(tidyverse)
library(lubridate)
library(maptools)
library(maps)
library(gridGraphics)
library(dplyr)
library(outliers)
library(EnvStats)
library(gapminder)
library(basedosdados)
library(car)
library(boxcoxmix)
library(gridExtra)
library(e1071)
library(moments)




##dados <- read.csv(file = "C:\\Users\\lucaa\\Desktop\\TCC\\tccpy\\********.csv")

dadosout <- serie_temporal_icms_mt_2000_2020_real_igpdi_01_2000

dadosreais <- mediana_sem_outliers_serie_temporal_icms_mt_2000_2020_real_igpdi_01_2000_09_2020

dadosnominais = serie_temporal_icms_mt_2000_2020_nominal

##colocando os dados em serie temporal
icmsdadosout <- ts(dadosout,
        frequency = 12,
        start = c(2000,1))

icms<- ts(dadosreais,
          frequency = 12,
          start = c(2000,1))


icms_nominal<- ts(dadosnominais,
          frequency = 12,
          start = c(2000,1))

icmsdadosout
icms
icms_nominal

#Verificando se há outliers
test <- rosnerTest(icmsdadosout,
                   k = 52
)
test


#Outliers em icmsdadosout
outlier(icmsdadosout)
outlier(icmsdadosout, opposite = TRUE)
boxplot.stats(icmsdadosout)$out

#Outliers em icms
outlier(icms)
outlier(icms, opposite = TRUE)
boxplot.stats(icms)$out


par(mfrow=c(1,2))


boxplot(icmsdadosout, xlin=c(2000,2020), ylin=c(0,19000000), main='Com Outliers',
        xlab='', ylab='icms_reais_igpdi_01_2000')

boxplot(icms, xlin=c(2000,2020), ylin=c(0,19000000), main='Sem Outliers',
        xlab='', ylab='icms_reais_igpdi_01_2000')



BoxCox.lambda(icms)


##plotando o grï¿½fico em ST
ts.plot((icms), main='ICMS Real de Mato Grosso   IGP-DI 01-2000', xlab='Anos', ylab='ICMS em R$')

icms
       
abline(reg = lm(icms~time(icms)))

ggseasonplot(icms)

## observa-se uma tendencia crescente de aumento de arrecadaï¿½ï¿½o e ICMS
## indica a presenï¿½a de saicmsonalidade
## - TENDENCIA: parece haver na arrecadaï¿½ï¿½o de ICMS ao longo praicmso
##   Coerente com a teoria econï¿½mica pois espera-se que ao longo do tempo 
##   ha aumento no desenvolvimento econ., consequentemente, as receitas cresï¿½am
##   e hï¿½ aumento na arrecad de ICMS.

## - VARIANCIA: observa-se que junto do aumento de arrecadaï¿½ï¿½o, a distancia
##   entre os meses com maiores e menores arrecadaï¿½ï¿½es tambem estï¿½ aumentando. 
##   Fato coerente com a teoria economica, pois quando ha aumento de arrecadaï¿½ï¿½o,
##   espera-se maiores oscilaï¿½ï¿½es em relaï¿½ï¿½o ao valor mï¿½dio.

## - SAicmsONALIDADE: nota-se um comportamento saicmsonal da arrecadaï¿½ï¿½o mensal de ICMS.
##   De setembro a deicmsembro observa-se um aumento na arrecadaï¿½ï¿½o de ICMS.
##   Alem disso, parece que a saicmsonalidade ï¿½ crescente.


dygraph(icms)

ggseasonplot(icms, polar = T)

monthplot(icms, labels = month.abb)

boxplot(icms[,1] ~ cycle(icms[,1]), xlab = "Mês", ylab = "icms_real_igpdi_01_2000 (R$)", main = "Arrecadação Mensal de ICMS em Mato Grosso - Boxplot")




## ESTATISTICA DESCRITIVA
logicms <- log(icms)

mean(logicms)
median(logicms)
range(logicms)
quantile(logicms, prob=c(0.75,0.25,0.9,0.1,0.5))


var(logicms)
format(var(logicms), scientific = FALSE)

sd(logicms)

kurtosis.norm.test(logicms)
skewness.norm.test(logicms)



## Decomposiï¿½ï¿½o classica. Decompondo a serie temporal utiliicmsando filtros
## de medias moveis em tres componentes principais:
## - Tendencia + ciclo
## - Saicmsonalidade
## - Residuo

plot(decompose(icms))

par(mfrow=c(1,2))

acf(icms, lag.max = 256, main='ICMS - ACF',
    xlab='defasagens', ylab='' )

pacf(icms,lag.max = 36, main='ICMS - PACF',
     xlab='defasagens', ylab='')

## TESTANDO A ESTACIONARIEDADE SAicmsONAL E Nï¿½O SAicmsONAL
## Hï¿½ 4 maneiras de observar a estacionariedade:
## - Anï¿½lise grï¿½fica ???
## - Comparar a media e a variancia em diferentes tempos da ST
## - Observar a FAC 'Funcao de Autocorrelacao'
## - Teste de Raiicms Unitaria



## Observa-se que o grafico gerado ï¿½ nao estacionï¿½rio condiicms com a literatura,
## na teoria de Box & Jenkins, que diicms que quando as autocorrelacoes decrescem
## de forma exponencial ou senoidal, elas sï¿½o nao-estacionarias.


BETS.corrgram(icms,lag.max = 36)
acf(icms, lag.max = 36, main = "ICMS")
pacf(icms, lag.max = 36, main = "ICMS")
## H0 ---> A ST possui raiicms unitaria (ST ï¿½ nï¿½o-estacionaria)
## H1 ---> A ST ï¿½ estacionaria

## Para tal analise utiliicmsou-se do teste  Dickey Fuller Aumentado (ADF)

## A analise estatistica apresentou um tau2= 0.3967, bem superior ao valor
## critico associado ao nivel de confianca de 95%(-2,88)
## Conclui-se que a serie temporal de icms nï¿½o ï¿½ estacionaria
## Nï¿½O REJEITA A H0

adf.drift <- ur.df(y = icms, type = c("drift"),lags = 24, selectlags = "AIC")
adf.drift
summary(adf.drift)



BETS.corrgram(adf.drift@res,lag.max = 36)




## Como a serie temporal ï¿½ nao estacionaria, feicms-se uma diferenciacï¿½o
## Aplicando uma diferenciacao, a serie temporal apresenta estar estacionaria
## na media, mas a variancia ï¿½ crescente ao longo do tempo.

ts.plot(diff(icms, lag = 1, differences = 1), main='Primeira diferenciação da série do ICMS',
        xlab='Anos', ylab='diff(icms, lag = 1, differences = 1)' )

BETS.corrgram(diff(icms, lag = 1, differences = 1),lag.max = 36)

## Um dos pressupostos da teoria Box & Jenkins ï¿½ que a ST seja tambï¿½m estacionï¿½ria na variï¿½ncia.
## Realiicmsou-se log na serie temporal

ts.plot(diff(log(icms),lag = 1,differences = 1), main='Primeira diferenciação da série do ICMS em Log',
        xlab='Anos', ylab='diff(log(icms), lag = 1, differences = 1)' )

acf(diff(diff(log(icms), lag = 1, differences = 1), lag.max = 250, main='ACF da Primeira diferenciação da série do ICMS em Log',
    xlab='defasagens', ylab='diff(log(icms), lag = 1, differences = 1)'))

pacf(diff(diff(log(icms), lag = 1, differences = 1), lag.max = 250, main='PACF da Primeira diferenciação da série do ICMS em Log',
     xlab='defasagens', ylab='diff(log(icms), lag = 1, differences = 1)'))


BETS.corrgram(diff(log(icms), lag = 1, differences = 1),lag.max=36)

#ts.plot(icms),lag = 1,differences = 1))

## AVALIANDO A ESTACIONARIEDADE DA PARTE SAicmsONAL
## Observe que agora a FAC apresenta cortes bruscos nos lags 1 e 12 e 
## nï¿½o apresenta mais decrescimento lento nem na parte saicmsonal nem na nï¿½o saicmsonal.

######################################VER A PARTE SAZONAL###########################################
diff_icms <-diff((log(icms)),12)
ggtsdisplay(diff_icms)
ndiffs(diff_icms) #NDFIFFS = 0 DIFERENCIAÇAO NA SAZONALIDADE, MAS MESMO ASSIM VAMOS DIFERENCIAR

diff_icms_again <- diff(diff_icms, 1)
adf.test(diff_icms_again)
ggtsdisplay(diff_icms_again)

####################################################################################################

BETS.corrgram(diff(diff(log(icms), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 36)

acf(diff(diff(log(icms), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 250, main='ACF da Primeira diferenciação da série do ICMS em Log',
    xlab='defasagens', ylab='diff(log(icms), lag = 1, differences = 1)')

pacf(diff(diff(log(icms), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 250, main='PACF da Primeira diferenciação da série do ICMS em Log',
     xlab='defasagens', ylab='diff(log(icms), lag = 1, differences = 1)')

ggtsdisplay(diff(diff(log(icms), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 15)
###################################################DUAS FORMAS DE INTERPRETAR ACIMA################################

## Faicms-se novamente o teste de raiicms unitaria para confirmar a estacionalidade
## apos aplicar as transformaï¿½ï¿½es anteriores.

## H0 ---> A ST possui raiicms unitaria (ST ï¿½ nï¿½o-estacionaria)
## H1 ---> A ST ï¿½ estacionaria

## Para tal analise utiliicmsou-se novamente do teste  Dickey Fuller Aumentado (ADF)
## O valor da estatï¿½stica de teste R.U (tau2 = -4.0465) ï¿½ inferior ao valor crï¿½tico (-2,88) ao nï¿½vel de significï¿½ncia de 95%
## Podemos concluir que a sï¿½rie ï¿½ estacionï¿½ria

adf.drift2 <- ur.df(y = diff(diff(log(icms), lag = 1), lag = 12), type = "drift", lags = 24, selectlags = "AIC")
adf.drift2
summary(adf.drift2)

## Alguns lags parecem significativos, mas nï¿½o sï¿½o relevante porque apresentam
## correlaï¿½ï¿½o muito baixa

BETS.corrgram(adf.drift2@res, lag.max = 36)

## MODELANDO A SERIE TEMPORAL


## A metodologia Box & Jenkins para sï¿½ries temporais estacionï¿½rias e 
## construï¿½ï¿½o dos modelos ARIMA segue um ciclo iterativo composto por 4 partes:
## - Especificaï¿½ï¿½o:SARIMA(p,d,q)(P,D,Q) ï¿½ analisada
## - Estimaï¿½ï¿½o:os parï¿½metros do modelo identificado sï¿½o estimados e testados estatisticamente sobre sua significï¿½ncia
## - Diagnï¿½stico: analise dos residuos (hï¿½ ruido branco) e teste Ljung-Box para ver se o modelo sugerido ï¿½ adequado.
##   Em seguida, verifica-s os modelos que apresentam menores valores para os critï¿½rios AIC e BIC
##   Caso haja problemas no diagnï¿½stico, volta-se ï¿½ identificaï¿½ï¿½o
## - Modelo definitivo: para previsï¿½o ou controle
##   Verifica quais modelos tem melhores medidas RMSE e MAPE



## IDENTIFICAï¿½ï¿½O

BETS.corrgram(diff(diff(log(icms), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 48)

BETS.corrgram(diff(diff(log(icms), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 48,type= "partial")






ggtsdisplay(diff(diff(log(icms))))


icms_teste <- auto.arima(icms)

                        
icms_teste
                        
      






############################################sarima1#####################################################                     
sarima_icms <- Arima(icms, order = c(1,1,2), seasonal = c(0,1,1),method = "ML", lambda=0)

sarima_icms

BETS.t_test(sarima_icms)

sarima_icms %>% forecast(h = 12) %>% autoplot()




# amostra treino até julho 2015 (187 meses)
cons <- window(icms, start = c(2000, 1), end = c(2015, 10))
# amostra teste (47 meses até junho 2019)
teste <- window(icms, start = c(2015, 11))


autoplot(icms) + autolayer(cons, series = "Training") + autolayer(teste, 
                                                                      series = "Test")







diag <- tsdiag(sarima_icms, gof.lag = 20)


checkresiduals(sarima_icms)



##TESTE DOS RESIDUOS CASO FAÇA DIFF SAZONAL - TIRAR OS ZEROS
x <- sarima_icms$residuals
plot(x)


x <- window(x, start=time(x)[14])
plot(x)

#par(mfrow=c(2,2))
plot(x)
acf(x)
pacf(x)
qqnorm(x)
qqline(x)
qqp(rstandard(sarima_icms))

#######TESTES LB,ARCH LM-test e Jarque-Bera test for normality

Box.test(x, lag = 24,type = "Ljung-Box", fitdf = 2)

ArchTest(x,lags = 12)

jb.norm.test(x, nrepl=2000)


#################################################
autoplot(icms) +
  autolayer(sarima_icms$fitted,series = "Arima(0,1,2)(0,0,2)")


###############PREVISAO sarima1#########################

plot(forecast(object = sarima_icms, h=6, level = 0.95))
lines(sarima_icms$fitted, col = "blue")
accuracy(sarima_icms)

plot(icms)
lines(sarima_icms$fitted, col = "blue")

write.csv(data.frame(forecast(object = sarima_icms, h=6, level = 0.95)),"previsao2021_2.csv")










############################################sarima2#####################################################                     
sarima2_icms <- Arima(icms, order = c(2,1,0), seasonal = c(0,1,1),method = "ML", lambda=0)

sarima2_icms

BETS.t_test(sarima2_icms)

diag <- tsdiag(sarima2_icms, gof.lag = 20)


checkresiduals(sarima2_icms)



##TESTE DOS RESIDUOS CASO FAÇA DIFF SAZONAL - TIRAR OS ZEROS
x <- sarima2_icms$residuals
plot(x)


x <- window(x, start=time(x)[14])
plot(x)

#par(mfrow=c(2,2))
plot(x)
acf(x)
pacf(x)
qqnorm(x)
qqline(x)
qqp(rstandard(sarima2_icms))

#######TESTES LB,ARCH LM-test e Jarque-Bera test for normality

Box.test(x, lag = 24,type = "Ljung-Box", fitdf = 1)

ArchTest(x,lags = 12)

jb.norm.test(x, nrepl=2000)


#################################################
autoplot(icms) +
  autolayer(sarima2_icms$fitted,series = "Arima(0,1,1)(0,1,1)")


###############PREVISAO sarima2#########################

plot(forecast(object = sarima2_icms, h=6, level = 0.95))
lines(sarima2_icms$fitted, col = "blue")
accuracy(sarima2_icms)

plot(icms)
lines(sarima2_icms$fitted, col = "blue")

write.csv(data.frame(forecast(object = sarima2_icms, h=6, level = 0.95)),"previsao2021.csv")











############################################sarima3#####################################################                     
sarima3_icms <- Arima(icms, order = c(1,1,2), seasonal = c(0,0,2),method = "ML", lambda=0)

sarima3_icms

BETS.t_test(sarima3_icms)

diag <- tsdiag(sarima3_icms, gof.lag = 20)


checkresiduals(sarima3_icms)



##TESTE DOS RESIDUOS CASO FAÇA DIFF SAZONAL - TIRAR OS ZEROS
x <- sarima3_icms$residuals
plot(x)


x <- window(x, start=time(x)[14])
plot(x)

#par(mfrow=c(2,2))
plot(x)
acf(x)
pacf(x)
qqnorm(x)
qqline(x)
qqp(rstandard(sarima3_icms))

#######TESTES LB,ARCH LM-test e Jarque-Bera test for normality

Box.test(x, lag = 24,type = "Ljung-Box", fitdf = 1)

ArchTest(x,lags = 12)

jb.norm.test(x, nrepl=2000)


#################################################



###############PREVISAO sarima3#########################

plot(forecast(object = sarima3_icms, h=6, level = 0.95))
lines(sarima3_icms$fitted, col = "blue")
accuracy(sarima3_icms)

plot(icms)
lines(sarima3_icms$fitted, col = "blue")

write.csv(data.frame(forecast(object = sarima3_icms, h=6, level = 0.95)),"previsao2021.csv")






############################################sarima4#####################################################                     
sarima4_icms <- Arima(icms, order = c(0,1,3), seasonal = c(0,0,2),method = "ML", lambda=0)

sarima4_icms

BETS.t_test(sarima4_icms)

diag <- tsdiag(sarima4_icms, gof.lag = 20)


checkresiduals(sarima4_icms)



##TESTE DOS RESIDUOS CASO FAÇA DIFF SAZONAL - TIRAR OS ZEROS
x <- sarima4_icms$residuals
plot(x)


x <- window(x, start=time(x)[14])
plot(x)

par(mfrow=c(2,2))
plot(x)
acf(x)
pacf(x)
qqnorm(x)
qqline(x)
qqp(rstandard(sarima4_icms))

#######TESTES LB,ARCH LM-test e Jarque-Bera test for normality

Box.test(x, lag = 24,type = "Ljung-Box", fitdf = 1)

ArchTest(x,lags = 12)

jb.norm.test(x, nrepl=2000)


#################################################



###############PREVISAO sarima1#########################

plot(forecast(object = sarima4_icms, h=6, level = 0.95))
lines(sarima4_icms$fitted, col = "blue")
accuracy(sarima4_icms)

plot(icms)
lines(sarima4_icms$fitted, col = "blue")

write.csv(data.frame(forecast(object = sarima4_icms, h=6, level = 0.95)),"previsao2021.csv")
