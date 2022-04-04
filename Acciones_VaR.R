###########################################################################
### Tesis: Medidas de riesgo de mercado y crédito en la banca comercial ###
###                  Luis Manuel Aguilar Rodríguez                      ###
###########################################################################

library(quantmod)
library(purrr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(faux)
library(lubridate)

# 1.- Selección de acciones y cálculo de rendimientos 
# Fecha inicial: 31/12/2016
# Fecha final 31/12/2021
# Acciones seleccionadas: "Citigroup Inc-C", "JPMorgan Chase & Co-JPM",
# "Pfizer Inc-PFE", "American Airlines Group Inc-AAL"

AC = c("C","JPM","AAL", "PFE")
getSymbols(AC, from = as.Date('2016-12-30'),to = as.Date('2021-12-30'),
           src = "yahoo", periodicity = "daily")

prices <- map(AC, function(x) Ad(get(x)))
prices <- reduce(prices, merge)
colnames(prices) <- AC

### Log returns ###
log_returns <- diff(log(prices), lag = 1)
log_returns <- na.omit(log_returns)

# 2.- Creación de matriz de correlación y cálculo de media y desviación estándar
sd <- c(sd(log_returns[,1]), sd(log_returns[,2]), sd(log_returns[,3]),
        sd(log_returns[,4]))

# Matriz de correlación y covarianza
cor_p <- round(cor(log_returns), 4)
cov_p <- round(cov(log_returns), 5)

# 3.- Creación de Z con correlación 
# Por el 100, probar > 1000, con 10000 notamos una cercanía mayor 
# en la correlación
# Z_n <- rnorm_multi(n = 10000, vars = 4, mu = 0,sd = 1, cor_p, varnames = AC)
# cor(Z_n)
# head(Z_n)

# 4.- Cálculo de precios de las acciones
# se definirá una función para encontrar el precio de las acciones
# se hará primero para una sola acción, para encontrar la manera correcta
# de generalizar el proceso. 

S<-function(S, sigma, z) return(S * exp((r - 1 / 2 * sigma^2) / 250 + sigma * sqrt(1 / 250) * z))

BS <- function(S_0, k, T, s, r, O = 1){
  d1 <- (log(S_0 / k) + (r + (s^(2)) / (2)) * T) / (s * sqrt(T))
  d2 <- d1 - s * sqrt(T)
  if (O == 1){
    BS <- k * exp(-r * T) * pnorm(-d2) - S_0 * pnorm(-d1)
    return(BS)
  } else if (O == 2) {
    BS <- S_0 * pnorm(d1) - k * exp(-r * T) * pnorm(d2)
    return(BS)
  } else {
    print("Invalid parameter")
  }
  
}

# HAREMOS EL PROCESO QUE GENERA TODO DENTRO DE VARIOS FOR
n = 150000
r <- 0.069
T <- 20 # anualizar 20/250 --> 19/250
s_anual <- sd*sqrt(250)

# NÚMERO DE OPCIONES A COMPRAR:
n1 = 2
n2 = 1
n3 = 5
n4 = 2

# MATRICES NECESARIAS PARA EL PROCESO
# añadir seed
prices_ad = c(prices[1258, ])
prices_ad = c(60.06689, 157.6152, 18.05, 57.14552)
S_0_matrix = matrix(0, n, length(AC))
final_port_matrix = matrix(0, n, T)
BS_M = matrix(0, n, length(AC))
BS_I = c()

for(j in 1:3){
  BS_I[j] = BS(prices_ad[j], prices_ad[j], T / 250, s_anual[j], r)
}

BS_I[4] = BS(prices_ad[4], prices_ad[4], T / 250, s_anual[4], r, O = 2)

final_port_matrix[, 1] = n1 * BS_I[1] + n2 * BS_I[2] + n3 * BS_I[3] +
  n4 * BS_I[4]

# BS con prices_ad --> col1 
# CALCULO DENTRO DE UN LOOP

for(k in 2:T){
  Z_n <- as.matrix(rnorm_multi(n = n, vars = 4, mu = 0,
                               sd = 1, cor_p, varnames = AC))
  if(k == 2){
    for(l in 1:4){
      S_0_matrix[, l] = prices_ad[l] 
    }
  }
  for(i in 1:n){
    for(j in 1:3){
      S_0_matrix[i, j] = S(S_0_matrix[i, j], s_anual[j], Z_n[i, j])
      BS_M[i, j] = BS(S_0_matrix[i, j], prices_ad[j], (T - (k - 1)) / 250, 
                      s_anual[j], r)
    }
    S_0_matrix[i, 4] = S(S_0_matrix[i, 4], s_anual[4], Z_n[i, 4])
    BS_M[i, 4] = BS(S_0_matrix[i, 4], prices_ad[4], (T - (k - 1)) / 250, 
                    s_anual[4], r, O = 2)
    final_port_matrix[i, k] = n1 * BS_M[i, 1] + n2 * BS_M[i, 2] +
      n3 * BS_M[i, 3] + n4 * BS_M[i, 4]
  }
}

colnames(final_port_matrix) <- c("T = 20", "T = 19", "T = 18", "T = 17", "T = 16","T = 15",
                                 "T = 14", "T = 13", "T = 12", "T = 11", "T = 10", "T = 9",
                                 "T = 8", "T = 7", "T = 6", "T = 5", "T = 4", "T = 3",
                                 "T = 2", "T = 1")
# 5. Medidas de Riesgo, EE, PFE, PFE max, EPE, EE Efectivo, PFE efectivo
## EE

t = seq(1, 20, 1)

EE = c()

for (i in 1:T){
  EE[i] = mean(final_port_matrix[, i])
}

# EPE
EPE = mean(EE)

# EE Efectivo
EEe = EE
EEe[19] = EEe[18]
EEe[20] = EEe[18]

# EPE efectivo
EPEe = mean(EEe)

cal = data.frame(t, EE, EPE, EEe, EPEe)

brks <- cal$t[seq(1, length(cal$t), )]
lbls <- lubridate::days(brks)

# plot
ggplot(cal, aes(x=t)) + 
  geom_line(aes(y=EE, col="EE")) + 
  geom_line(aes(y=EPE, col="EPE", linetype = "")) + 
  geom_line(aes(y = EEe, col="EEe")) +
  geom_line(aes(y = EPEe, col="EPEe", linetype = "")) +
  scale_linetype_manual(values=c("twodash", "dotted")) +
  labs(title="Medidas de Riesgo", 
       subtitle="EE | EPE | EEe | EPEe", x = "Tiempo", y="Valor") +  # title and caption
  scale_color_manual(name="", values = c("EE"="#32E457", "EPE"="#bc42aa", 
                                         "EEe" = "#1857B2", "EPEe"="#A537E8")) +  # line color
  theme(panel.grid.minor = element_blank()) +
  scale_x_time(labels = c(seq(1, 20, 1)), breaks = brks) 

# PFE
final_port_matrix_order = apply(final_port_matrix, 2, sort)

p = 0.95
PFE = c()

q = p * length(final_port_matrix_order[, 1])

PFE = c(final_port_matrix_order[q, ])
names(PFE) = NULL

# PFE Máximo
PFEM = c()
for(i in 1:T){
  PFEM[i] = max(final_port_matrix[, i])
}

# DATAFRAME
t = seq(1, 20, 1)
df = data.frame(t, PFE, PFEM)

ggplot(df, aes(x=t)) + 
  geom_line(aes(y=PFE, col="PFE")) + 
  geom_line(aes(y=PFEM, col="PFEM")) + 
  labs(title="Medidas de Riesgo", 
       subtitle="PFE | PFEM", x = "Tiempo", y="Valor") +  # title and caption
  scale_color_manual(name="", values = c("PFE"="#1857B2", "PFEM"="#bc42aa")) +  # line color
  theme(panel.grid.minor = element_blank()) +
  scale_x_time(labels = c(seq(1, 20, 1)), breaks = brks) 

ggplot(df, aes(x = t, y = m, group = Metrics, color = Metrics)) + 
  geom_line(linetype = "solid") + 
  geom_point()

# Comprobar con el error 
mn = c()
for(i in 1:4){
  mn[i] = round(mean(S_0_matrix[1:10000, i]), 3)
}

error = c()
for(i in 1:4){
  error[i] = abs(mn[i] - (prices_ad[i] * exp(r * 19 / 250))) / 
    (prices_ad[i] * exp(r * 19 / 250))
}



