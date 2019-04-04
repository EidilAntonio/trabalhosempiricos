#########################################################
# Mini-Curso Produção de Trabalhos Empíricos usando o R #
######################## Aula 02 ########################


## Pacotes utilizados

library(quantmod)
library(xts)
library(sidrar)
library(forecast)
library(ggplot2)
library(scales)
library(astsa)
library(vars)
library(aod)

######## Coleta dos dados ##########

# IBOVESPA
env = new.env()
getSymbols('^BVSP', src='yahoo',
           env=env,
           from=as.Date('2001-01-01'))

# FBCF
fbcf = get_sidra(api='/t/1620/n1/all/v/all/p/all/c11255/93406/d/v583%202')


############ Tratamento e Exploração Inicial dos dados ##############

## Ibovespa
ibovespa = env$BVSP$BVSP.Close
sapply(ibovespa, function(x) sum(is.na(x)))
ibovespa = ibovespa[complete.cases(ibovespa)]
ibovespa = apply.quarterly(ibovespa, FUN=mean)
ibovespa = ts(ibovespa, start=c(2001,01), freq=4)

## FBCF
fbcf = ts(fbcf$Valor, start=c(1996,01), freq=4)
dfbcf = (fbcf/lag(fbcf,-4)-1)*100

## Juntar dados
data = ts.intersect(ibovespa, dfbcf)

## Gráficos
autoplot(data, facet=TRUE)
df = data.frame(fbcf=data[,2], ibovespa=data[,1])
ggplot(df, aes(ibovespa, fbcf))+
  geom_point()+
  geom_smooth(method='lm')

