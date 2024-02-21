library(readxl)
library(ggplot2)
library(vrtest)
library(rcompanion)
library(nortest)
library(dplyr)
library(tseries)
library(urca)
library(moments)
options(scipen=100)


dane <- read_excel("dane.xlsx")
dane <- dane[as.Date(dane$Data) >= as.Date("2005-01-01"),]

#wykresy
summary(dane$Zamkniecie)
boxplot(dane$Zamkniecie)
plot(y=dane$Zamkniecie, x=dane$Data, type = "l", lty = 1)
hist(dane$Zamkniecie, # histogram
     col="orange", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "temp",
     main = "EUR/USD")
lines(density(dane$Zamkniecie), # density plot
      lwd = 2, # thickness of line
      col = "green")
plotNormalHistogram(dane$Zamkniecie)


#TESTY FORMALNE NA NORMALNOSC
# H0: Zmienna ma rozklad normalny
# H1: Zmienna nie ma rozkladu normalnego

shapiro.test(dane$Zamkniecie) #Test Shapiro-Wilka

sf.test(dane$Zamkniecie) #Test Shapiro-Francia

jarque.bera.test(dane$Zamkniecie) #Test Jarque Bera


##### Przygotowanie danych #####

y = dane$Zamkniecie
nob = length(y)
r <- diff(log(y), differences = 1)
kvec <- c(2, 4, 8, 16)

#Wykresy
summary(r)
boxplot(r)
plot(y=r, x=dane$Data[2:nob], type = "l", lty = 1)
hist(r, # histogram
     col="orange", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "temp",
     main = "EUR/USD")
lines(density(r), # density plot
      lwd = 2, # thickness of line
      col = "green")
plotNormalHistogram(r)

#TESTY FORMALNE NA NORMALNOSC
# H0: Zmienna ma rozklad normalny
# H1: Zmienna nie ma rozkladu normalnego
shapiro.test(r) #Test Shapiro-Wilka
sf.test(r) #Test Shapiro-Francia
jarque.bera.test(r) #Test Jarque Bera





##TESTY WARIANCJI
##### TEST Lo-MacKinlaya #####

Lo.Mac(r,kvec) 

# M1 and M2 are assumed to follow a normal distribution. So if M1 or M2 is greater than 1.96, you can reject the null hypothesis at 5% significant level.
# Zatem u nas:
# Obszar krytyczny (-inf, -1,96] u [1,96, +inf)

#              M1         M2
# k=2  -0.1885845 -0.1470233
# k=4  -0.7842560 -0.6028779
# k=8  -0.7425034 -0.5702950
# k=16 -0.5769344 -0.4447012


# Zarówno dla założenia o homoskedastyczności jak i heteroskedastyczności:
# Żadna statystyka nie wpada do obszaru krytycznego ani dla k=2.4,8,16, zatem możemy stwierdzić, że NIE ma podstaw do odrzucenia H0, które mówi o błądzeniu losowym zmiennej
# Ergo: W teście Lo-MacKinlaya kurs Euro to Random Walk

# TAK SAMO JAK W NASZEJ PRZYKŁADOWEJ PRACY




##### TEST Chow-Denninga #####


Chow.Denning(r,kvec) 

# Obsar krytyczny dla L=4 i alpha = 0,05: [2.49, +inf)
# Nasza statystyka nie wpada do obszaru krytycznego. Kurs = RWH

# TAK SAMO JAK W NASZEJ PRZYKŁADOWEJ PRACY



##### TEST Wright'a (2000) #####


Wright(r, kvec)

# Statystyki
#             R1         R2        S1
# k=2  -1.969353 -0.9999501 -2.856560
# k=4  -2.199729 -1.3506424 -3.137770
# k=8  -2.086350 -1.2887085 -2.505975
# k=16 -1.480112 -0.8828258 -1.850367



# Obszary krytyczne
# nit 10000 jest zalecane przez Wrighta

Wright.crit(nob, k=2, nit=10000)

# Statystyki = Czy wpada do obszaru krytycznego: NNT
#             R1         R2        S1
# k=2  -1.969353* -0.9999501 -2.856560***



# $R1.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.534564 -1.973685 -1.649421  1.630429  1.953276  2.547705 
# 
# $R2.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.598608 -1.942106 -1.646828  1.604674  1.947673  2.542218 
# 
# $S1.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.556503 -1.956544 -1.642354  1.585229  1.899419  2.470672 

Wright.crit(nob, k=4, nit=10000)

# Statystyki = Czy wpada do obszaru krytycznego: TNT
#             R1         R2        S1
# k=4  -2.199729** -1.3506424 -3.137770***



# $R1.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.588329 -1.980641 -1.668585  1.600346  1.935298  2.554753 
# 
# $R2.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.520248 -1.971150 -1.664885  1.613083  1.968609  2.579129 
# 
# $S1.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.572557 -1.969494 -1.656513  1.648879  1.961861  2.595458

Wright.crit(nob, k=8, nit=10000)

# Statystyki = Czy wpada do obszaru krytycznego: NNT
#             R1         R2        S1
# k=8  -2.086350* -1.2887085 -2.505975**



# $R1.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.676234 -2.121235 -1.803991  1.572856  1.990894  2.542120
# 
# $R2.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.614077 -2.021983 -1.731396  1.572338  1.880775  2.566169 
# 
# $S1.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.527662 -1.969874 -1.726001  1.546038  1.921655  2.433637

Wright.crit(nob, k=16, nit=10000)

# Statystyki = Czy wpada do obszaru krytycznego: NNN
#             R1         R2        S1
# k=16 -1.480112 -0.8828258 -1.850367*

# $R1.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.491186 -1.943761 -1.701389  1.584123  1.915572  2.631291 
# 
# $R2.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.506976 -1.969060 -1.688657  1.588805  1.913810  2.653878 
# 
# $S1.crit
#         0.5%      2.5%        5%       95%     97.5%     99.5% 
#   -2.428515 -1.919164 -1.647396  1.649019  2.027835  2.723801 





#TESTY na biały szum
# Sprawdzenie za pomoca statystyki Q, H0: szereg jest bialym szumem
Box.test(r, lag=24, type="Ljung-Box") # test Ljung-Box
Box.test(r, lag=24, type="Box-Pierce") # test Box-Pierce
#Obydwa testy ujawniły p-value>0.05, a więc szereg jest bialym szumem (H0)


#Testy na stacjonarność
# test KPSS W teście tym mamy odwrócone hipotezy: jeśli statystyka testowa jest większa  
# od wartości krytycznej to odrzucamy H0 o stacjonarności szeregu.
# ur.kpss(y, type = c("mu", "tau")) - "mu" - stała w równaniu testowym; "tau" - trend w równaniu testowym
kpss.test <- ur.kpss(r, type = c("mu"))
summary(kpss.test)
#Statystyka testowa większa niż wartości krytyczne, a więc szereg stacjonarny

#ADF
df.test <- ur.df(r, type = c("none"), lags = 10)
summary(df.test)
#Statystyka testowa mniejsza niż wartości krytyczne, a więc szereg stacjonarny


