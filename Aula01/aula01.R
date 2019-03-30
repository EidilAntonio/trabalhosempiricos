#########################################################
# Mini-Curso Produção de Trabalhos Empíricos usando o R #
######################## Aula 01 ########################


## Pacotes utilizados

# Caso não tenha algum instalado, utilize a função 
# install.packages('quantmod') para instalar um ou mais pacotes.

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
