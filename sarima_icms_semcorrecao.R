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

#Verificando se h� outliers
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


##plotando o gr�fico em ST
ts.plot((icms), main='ICMS Real de Mato Grosso   IGP-DI 01-2000', xlab='Anos', ylab='ICMS em R$')

icms
       
abline(reg = lm(icms~time(icms)))

ggseasonplot(icms)

## observa-se uma tendencia crescente de aumento de arrecada��o e ICMS
## indica a presen�a de saicmsonalidade
## - TENDENCIA: parece haver na arrecada��o de ICMS ao longo praicmso
##   Coerente com a teoria econ�mica pois espera-se que ao longo do tempo 
##   ha aumento no desenvolvimento econ., consequentemente, as receitas cres�am
##   e h� aumento na arrecad de ICMS.

## - VARIANCIA: observa-se que junto do aumento de arrecada��o, a distancia
##   entre os meses com maiores e menores arrecada��es tambem est� aumentando. 
##   Fato coerente com a teoria economica, pois quando ha aumento de arrecada��o,
##   espera-se maiores oscila��es em rela��o ao valor m�dio.

## - SAicmsONALIDADE: nota-se um comportamento saicmsonal da arrecada��o mensal de ICMS.
##   De setembro a deicmsembro observa-se um aumento na arrecada��o de ICMS.
##   Alem disso, parece que a saicmsonalidade � crescente.


dygraph(icms)

ggseasonplot(icms, polar = T)

monthplot(icms, labels = month.abb)

boxplot(icms[,1] ~ cycle(icms[,1]), xlab = "M�s", ylab = "icms_real_igpdi_01_2000 (R$)", main = "Arrecada��o Mensal de ICMS em Mato Grosso - Boxplot")




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



## Decomposi��o classica. Decompondo a serie temporal utiliicmsando filtros
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

## TESTANDO A ESTACIONARIEDADE SAicmsONAL E N�O SAicmsONAL
## H� 4 maneiras de observar a estacionariedade:
## - An�lise gr�fica ???
## - Comparar a media e a variancia em diferentes tempos da ST
## - Observar a FAC 'Funcao de Autocorrelacao'
## - Teste de Raiicms Unitaria



## Observa-se que o grafico gerado � nao estacion�rio condiicms com a literatura,
## na teoria de Box & Jenkins, que diicms que quando as autocorrelacoes decrescem
## de forma exponencial ou senoidal, elas s�o nao-estacionarias.


BETS.corrgram(icms,lag.max = 36)
acf(icms, lag.max = 36, main = "ICMS")
pacf(icms, lag.max = 36, main = "ICMS")
## H0 ---> A ST possui raiicms unitaria (ST � n�o-estacionaria)
## H1 ---> A ST � estacionaria

## Para tal analise utiliicmsou-se do teste  Dickey Fuller Aumentado (ADF)

## A analise estatistica apresentou um tau2= 0.3967, bem superior ao valor
## critico associado ao nivel de confianca de 95%(-2,88)
## Conclui-se que a serie temporal de icms n�o � estacionaria
## N�O REJEITA A H0

adf.drift <- ur.df(y = icms, type = c("drift"),lags = 24, selectlags = "AIC")
adf.drift
summary(adf.drift)



BETS.corrgram(adf.drift@res,lag.max = 36)




## Como a serie temporal � nao estacionaria, feicms-se uma diferenciac�o
## Aplicando uma diferenciacao, a serie temporal apresenta estar estacionaria
## na media, mas a variancia � crescente ao longo do tempo.

ts.plot(diff(icms, lag = 1, differences = 1), main='Primeira diferencia��o da s�rie do ICMS',
        xlab='Anos', ylab='diff(icms, lag = 1, differences = 1)' )

BETS.corrgram(diff(icms, lag = 1, differences = 1),lag.max = 36)

## Um dos pressupostos da teoria Box & Jenkins � que a ST seja tamb�m estacion�ria na vari�ncia.
## Realiicmsou-se log na serie temporal

ts.plot(diff(log(icms),lag = 1,differences = 1), main='Primeira diferencia��o da s�rie do ICMS em Log',
        xlab='Anos', ylab='diff(log(icms), lag = 1, differences = 1)' )

acf(diff(diff(log(icms), lag = 1, differences = 1), lag.max = 250, main='ACF da Primeira diferencia��o da s�rie do ICMS em Log',
    xlab='defasagens', ylab='diff(log(icms), lag = 1, differences = 1)'))

pacf(diff(diff(log(icms), lag = 1, differences = 1), lag.max = 250, main='PACF da Primeira diferencia��o da s�rie do ICMS em Log',
     xlab='defasagens', ylab='diff(log(icms), lag = 1, differences = 1)'))


BETS.corrgram(diff(log(icms), lag = 1, differences = 1),lag.max=36)

#ts.plot(icms),lag = 1,differences = 1))

## AVALIANDO A ESTACIONARIEDADE DA PARTE SAicmsONAL
## Observe que agora a FAC apresenta cortes bruscos nos lags 1 e 12 e 
## n�o apresenta mais decrescimento lento nem na parte saicmsonal nem na n�o saicmsonal.

######################################VER A PARTE SAZONAL###########################################
diff_icms <-diff((log(icms)),12)
ggtsdisplay(diff_icms)
ndiffs(diff_icms) #NDFIFFS = 0 DIFERENCIA�AO NA SAZONALIDADE, MAS MESMO ASSIM VAMOS DIFERENCIAR

diff_icms_again <- diff(diff_icms, 1)
adf.test(diff_icms_again)
ggtsdisplay(diff_icms_again)

####################################################################################################

BETS.corrgram(diff(diff(log(icms), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 36)

acf(diff(diff(log(icms), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 250, main='ACF da Primeira diferencia��o da s�rie do ICMS em Log',
    xlab='defasagens', ylab='diff(log(icms), lag = 1, differences = 1)')

pacf(diff(diff(log(icms), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 250, main='PACF da Primeira diferencia��o da s�rie do ICMS em Log',
     xlab='defasagens', ylab='diff(log(icms), lag = 1, differences = 1)')

ggtsdisplay(diff(diff(log(icms), lag = 1, differences = 1),lag = 12, differences = 1), lag.max = 15)
###################################################DUAS FORMAS DE INTERPRETAR ACIMA################################

## Faicms-se novamente o teste de raiicms unitaria para confirmar a estacionalidade
## apos aplicar as transforma��es anteriores.

## H0 ---> A ST possui raiicms unitaria (ST � n�o-estacionaria)
## H1 ---> A ST � estacionaria

## Para tal analise utiliicmsou-se novamente do teste  Dickey Fuller Aumentado (ADF)
## O valor da estat�stica de teste R.U (tau2 = -4.0465) � inferior ao valor cr�tico (-2,88) ao n�vel de signific�ncia de 95%
## Podemos concluir que a s�rie � estacion�ria

adf.drift2 <- ur.df(y = diff(diff(log(icms), lag = 1), lag = 12), type = "drift", lags = 24, selectlags = "AIC")
adf.drift2
summary(adf.drift2)

## Alguns lags parecem significativos, mas n�o s�o relevante porque apresentam
## correla��o muito baixa

BETS.corrgram(adf.drift2@res, lag.max = 36)

## MODELANDO A SERIE TEMPORAL


## A metodologia Box & Jenkins para s�ries temporais estacion�rias e 
## constru��o dos modelos ARIMA segue um ciclo iterativo composto por 4 partes:
## - Especifica��o:SARIMA(p,d,q)(P,D,Q) � analisada
## - Estima��o:os par�metros do modelo identificado s�o estimados e testados estatisticamente sobre sua signific�ncia
## - Diagn�stico: analise dos residuos (h� ruido branco) e teste Ljung-Box para ver se o modelo sugerido � adequado.
##   Em seguida, verifica-s os modelos que apresentam menores valores para os crit�rios AIC e BIC
##   Caso haja problemas no diagn�stico, volta-se � identifica��o
## - Modelo definitivo: para previs�o ou controle
##   Verifica quais modelos tem melhores medidas RMSE e MAPE



## IDENTIFICA��O

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




# amostra treino at� julho 2015 (187 meses)
cons <- window(icms, start = c(2000, 1), end = c(2015, 10))
# amostra teste (47 meses at� junho 2019)
teste <- window(icms, start = c(2015, 11))


autoplot(icms) + autolayer(cons, series = "Training") + autolayer(teste, 
                                                                      series = "Test")







diag <- tsdiag(sarima_icms, gof.lag = 20)


checkresiduals(sarima_icms)



##TESTE DOS RESIDUOS CASO FA�A DIFF SAZONAL - TIRAR OS ZEROS
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



##TESTE DOS RESIDUOS CASO FA�A DIFF SAZONAL - TIRAR OS ZEROS
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



##TESTE DOS RESIDUOS CASO FA�A DIFF SAZONAL - TIRAR OS ZEROS
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



##TESTE DOS RESIDUOS CASO FA�A DIFF SAZONAL - TIRAR OS ZEROS
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
