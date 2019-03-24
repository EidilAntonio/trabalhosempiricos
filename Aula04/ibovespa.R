#########################################################
# Mini-Curso Produção de Trabalhos Empíricos usando o R #
#########################################################


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


############## Modelo ############################

### COnstruindo um VEC
### Selecionar Defasagem
def <- VARselect(data,lag.max=12,type="both")
def$selection 
### Teste de Cointegração
jo.eigen <- ca.jo(data, type='eigen', K=2, ecdet='const', 
                  spec='transitory')
summary(jo.eigen)
### VEC
vec <- cajorls(jo.eigen, r=1)
vec$beta # Vetor de Cointegração Normalizado
vec$rlm # VECM com r=1
### VEC para VAR
vec.level <- vec2var(jo.eigen, r=1)
# IRF: Impulso no Ibovespa e resposta na dfbcf
irf = irf(vec.level, impulse='ibovespa', response='dfbcf', 
          n.ahead = 12, boot=T, ortho=T, cumulative=F)
lags = 1:13
df.irf <- data.frame(irf=irf$irf, lower=irf$Lower, upper=irf$Upper,
                     lags=lags)

colnames(df.irf) <- c('irf', 'lower', 'upper', 'lags')

number_ticks <- function(n) {function(limits) pretty(limits, n)}

ggplot(data = df.irf,aes(x=lags,y=irf)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = irf), size=.8)+
  geom_ribbon(aes(x=lags, ymax=upper, 
                  ymin=lower), 
              fill="blue", alpha=.1) +
  xlab("") + ylab("Variação da FBCF") + 
  ggtitle("Resposta ao Impulso em Ibovespa") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  geom_line(colour = 'black')+
  scale_x_continuous(breaks=number_ticks(13))+
  theme_bw()
### Decomposição de Variância
fevd(vec.level, n.ahead=12)
### VAR(2)
var2 <- VAR(data, p=2, type='both')
serial.test(var2)
### Teste de Wald
var3 <- VAR(data, p=3, type='both')
### Wald Test 01: DFBCF não granger causa Ibovespa
wald.test(b=coef(var3$varresult[[1]]), 
          Sigma=vcov(var3$varresult[[1]]), 
          Terms=c(2,4))

### Wald Test 02: Ibovespa não granger causa DFBCF
wald.test(b=coef(var3$varresult[[2]]), 
          Sigma=vcov(var3$varresult[[2]]), 
          Terms= c(1,3))
