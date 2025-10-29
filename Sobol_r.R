#biblioteki
#install.packages("hydroTSM")
library(hydroTSM)
packageVersion(hydroTSM)
library(ggplot2)
packageVersion(ggplot2)
library(dplyr)
library(mclust)
library(tidyverse)
#install.packages("mclust")
library(amap)
#install.packages("amap")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("corrplot")
library(corrplot)
library(spatstat)
#install.packages("tseries")
library(tseries)
library(stats)
#install.packages("fmsb")
library(fmsb)
#opady
Zp<-read.table(file = "C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/zakopane/349190625ZAKOPANE_p_poprawione.txt", header=TRUE,  sep = "\t")
View(Zp)

Tp<-read.table(file="C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/tarnow/350200575TARNOW_p_poprawione.txt" , header=TRUE,  sep = "\t")
View(Tp)

Kp<- read.table(file="C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/kasprowy/349190650KASPROWY-WIERCH_p_poprawione.txt" , header=TRUE,  sep = "\t")
View(Kp)

#temperatura
Zt <- read.table(file = "C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/zakopane/349190625ZAKOPANE_t_poprawione.txt", header=TRUE,  sep = "\t")
View(Zt)

Tt<-read.table(file="C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/tarnow/350200575TARNOW_t_poprawione.txt" , header=TRUE,  sep = "\t")
View(Zt)

Kt<- read.table(file="C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/kasprowy/349190650KASPROWY-WIERCH_t_poprawione.txt" , header=TRUE,  sep = "\t")
View(Kt)

#statystyki
smry(Zp)
smry(Tp)
smry(Kp)

#wszystkie w jednym workframe
W= Kt<- read.table(file="C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/wszystkie.txt" , header=TRUE,  sep = "\t")
View(W)
colors <- c("Tarnów" = "steelblue1","Zakopane" = "royalblue", "Kasprowy Wierch" = "navyblue")
Theme=theme(legend.position = "bottom", plot.title = element_text(family = "Helvetica", face = "bold",  hjust=(0.5))) 

PI<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = IZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = IT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = IK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla stycznia",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PI+Theme)

PII<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = IIZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = IIT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = IIK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla lutego",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PII+Theme)


PIII<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = IIIZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = IIIT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = IIIK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla marca",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PIII+Theme)

PIV<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = IVZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = IVT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = IVK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla kwietnia",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PIV+Theme)

PV<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = VZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = VT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = VK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla maja",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PV+Theme)

PVI<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = VIZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = VIT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = VIK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla czerwca",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PVI+Theme)

PVII<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = VIIZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = VIIT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = VIIK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla lipiec",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PVII+Theme)

PVIII<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = VIIIZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = VIIIT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = VIIIK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla sierpnia",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PVIII+Theme)

PIX<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = IXZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = IXT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = IXK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla wrzeœnia",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PIX+Theme)

PX<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = XZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = XT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = XK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla paŸdziernika",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PX+Theme)

PXI<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = XIZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = XIT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = XIK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla listopada",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PXI+Theme)

PXII<-ggplot(W, aes(x=rok))+
  geom_line ( aes( y = XIIZ, color = "Zakopane"), size=1.5) +
  geom_line ( aes( y = XIIT, color = "Tarnów"), size= 1.5) +
  geom_line ( aes( y = XIIK, color = "Kasprowy Wierch"), size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla grudnia",y="Iloœæ opadów [mm]", x = "Rok", color=" ")  +
  scale_color_manual(values = colors)
print(PXII+Theme)

install.packages("graphics")
library(graphics)
par(mfrow=c(6,2))
print(PXII+Theme)
# #wykresy liniowe
# PI <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = I), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = I),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = I),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# stycznia",y="iloœæ opadów [mm]", x = "Rok")
# PI                                       
# 
# PII <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = II), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = II),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = II),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# lutego",y="iloœæ opadów [mm]", x = "Rok")
# 
# PII
# 
# PIII <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = III), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = III),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = III),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# marca",y="iloœæ opadów [mm]", x = "Rok")
# 
# PIII
# 
# PIV <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = IV), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = IV),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = IV),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# kwietnia",y="iloœæ opadów [mm]", x = "Rok")
# 
# PIV
# 
# PV <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = V), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = V),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = V),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# maja",y="iloœæ opadów [mm]", x = "Rok")
# 
# PV
# 
# PVI <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = VI), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = VI),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = VI),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# czerwca",y="iloœæ opadów [mm]", x = "Rok")
# 
# 
# PVI
# 
# PVII <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = VII), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = VII),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = VII),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# lipca",y="iloœæ opadów [mm]", x = "Rok")
# 
# PVII
# 
# PVIII <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = VIII), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = VIII),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = VIII),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# sierpnia",y="iloœæ opadów [mm]", x = "Rok")
# 
# PVIII
# 
# PIX <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = IX), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = IX),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = IX),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# wrzeœnia",y="iloœæ opadów [mm]", x = "Rok")
# 
# PIX
# 
# PX <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = X), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = X),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = X),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# pa¿dziernika",y="iloœæ opadów [mm]", x = "Rok")
# 
# 
# PX
# 
# 
# PXI <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = XI), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = XI),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = XI),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# listopada",y="iloœæ opadów [mm]", x = "Rok")
# 
# 
# PXI
# 
# PXII <- ggplot() +
#   geom_line (data = Zp, aes(x = rok, y = XII), color = "royalblue", size=1.5) +
#   geom_line (data = Tp, aes(x = rok, y = XII),color = "navyblue", size= 1.5) +
#   geom_line (data = Kp, aes(x = rok, y = XII),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
# grudnia",y="iloœæ opadów [mm]", x = "Rok")
# 
# PXII


#korelacja Pearsona
cor.test(Kp$I,Kt$I)
cor.test(Kp$II,Kt$II)
cor.test(Kp$III,Kt$III)
cor.test(Kp$IV,Kt$IV)
cor.test(Kp$V,Kt$V)
cor.test(Kp$VI,Kt$VI)
cor.test(Kp$VII,Kt$VII)
cor.test(Kp$VIII,Kt$VIII)
cor.test(Kp$IX,Kt$IX)
cor.test(Kp$X,Kt$X)
cor.test(Kp$XI,Kt$XI)
cor.test(Kp$XII,Kt$XII)


cor.test(Tp$I,Tt$I)
cor.test(Tp$II,Tt$II)
cor.test(Tp$III,Tt$III)
cor.test(Tp$IV,Tt$IV)
cor.test(Tp$V,Tt$V)
cor.test(Tp$VI,Tt$VI)
cor.test(Tp$VII,Tt$VII)
cor.test(Tp$VIII,Tt$VIII)
cor.test(Tp$IX,Tt$IX)
cor.test(Tp$X,Tt$X)
cor.test(Tp$XI,Tt$XI)
cor.test(Tp$XII,Tt$XII)

cor.test(Zp$I,Zt$I)
cor.test(Zp$II,Zt$II)
cor.test(Zp$III,Zt$III)
cor.test(Zp$IV,Zt$IV)
cor.test(Zp$V,Zt$V)
cor.test(Zp$VI,Zt$VI)
cor.test(Zp$VII,Zt$VII)
cor.test(Zp$VIII,Zt$VIII)
cor.test(Zp$IX,Zt$IX)
cor.test(Zp$X,Zt$X)
cor.test(Zp$XI,Zt$XI)
cor.test(Zp$XII,Zt$XII)

#korelacja spearmana
cor.test(Zp$I, Tp$I, method = "spearman")
cor.test(Zp$II, Tp$II, method = "spearman")
cor.test(Zp$III, Tp$III, method = "spearman")
cor.test(Zp$IV, Tp$IV, method = "spearman")
cor.test(Zp$V, Tp$V, method = "spearman")
cor.test(Zp$VI, Tp$VI, method = "spearman")
cor.test(Zp$VII, Tp$VII, method = "spearman")
cor.test(Zp$VIII, Tp$VIII, method = "spearman")
cor.test(Zp$IX, Tp$IX, method = "spearman")
cor.test(Zp$X, Tp$X, method = "spearman")
cor.test(Zp$XI, Tp$XI, method = "spearman")
cor.test(Zp$XII, Tp$XII, method = "spearman")

cor.test(Kp$I, Tp$I, method = "spearman")
cor.test(Kp$II, Tp$II, method = "spearman")
cor.test(Kp$III, Tp$III, method = "spearman")
cor.test(Kp$IV, Tp$IV, method = "spearman")
cor.test(Kp$V, Tp$V, method = "spearman")
cor.test(Kp$VI, Tp$VI, method = "spearman")
cor.test(Kp$VII, Tp$VII, method = "spearman")
cor.test(Kp$VIII, Tp$VIII, method = "spearman")
cor.test(Kp$IX, Tp$IX, method = "spearman")
cor.test(Kp$X, Tp$X, method = "spearman")
cor.test(Kp$XI, Tp$XI, method = "spearman")
cor.test(Kp$XII, Tp$XII, method = "spearman")


cor.test(Kp$I, Zp$I, method = "spearman")
cor.test(Kp$II, Zp$II, method = "spearman")
cor.test(Kp$III, Zp$III, method = "spearman")
cor.test(Kp$IV, Zp$IV, method = "spearman")
cor.test(Kp$V, Zp$V, method = "spearman")
cor.test(Kp$VI, Zp$VI, method = "spearman")
cor.test(Kp$VII, Zp$VII, method = "spearman")
cor.test(Kp$VIII, Zp$VIII, method = "spearman")
cor.test(Kp$IX, Zp$IX, method = "spearman")
cor.test(Kp$X, Zp$X, method = "spearman")
cor.test(Kp$XI, Zp$XI, method = "spearman")
cor.test(Kp$XII, Zp$XII, method = "spearman")

cor(Kp$I)
cor.test(Kp$I,Kt$I)
ggplot(Kt, aes(I,Kp$I))+geom_point(col="blue")+
  geom_smooth(method="lm")+ggtitle("Zale¿noœæ temperatury od opadów: styczeñ")+
  labs(x = "temperatura",y="opady")


#corrplot 
library(corrplot)
Zp1=select(Zp, I,II,III,IV,V,VI,VII,VIII,IX,X,XI,XII)
Zpcor=cor(Zp1)
View(Zpcor)
corrplot(Zpcor)
corrplot(Zpcor, method='number',sig.level = 0.05)
cor.test(p.mat)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(Zp1)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(Zp1[, i], Zp1[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(Zp1, method='number',p.mat=p.mat,sig.level = 0.05, insig = "blank")
head(p.mat)

cor.test(Kp$IX, Kp$VI)
cor.test(Kp$XI, Kp$II)
cor.test(Kp$VIII, Kp$II)
cor.test(Kp$VIII, Kp$I)
Tp1=select(Tp, I,II,III,IV,V,VI,VII,VIII,IX,X,XI,XII)
Tpcor=cor(Tp1)
View(Tpcor)
corrplot(Zpcor, method='number')

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(Tp1)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(Zp1[, i], Zp1[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(Tp1, method='number',p.mat=p.mat,sig.level = 0.05, insig = "blank")


corrplot(Kpcor)
Kp1=select(Kp, I,II,III,IV,V,VI,VII,VIII,IX,X,XI,XII)
Kpcor=cor(Kp1)
View(Kpcor)
corrplot(Kpcor)
corrplot(Kpcor, method='number',p.mat=p.mat,sig.level = 0.05, insig = "blank")

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(Kp1)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(Zp1[, i], Zp1[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(Kp1, method='number',p.mat=p.mat,sig.level = 0.05, insig = "blank")








z³=cor(z³¹czone)
View(z³)
corrplot(z³)


install.packages("ggcorrplot")
library(ggcorrplot)
ggpcorrplot(data=Kp$I, lag.max = 30, ci= 0.95, large.sample.size = FALSE, horizontal = TRUE)

Kz=read.table(file="C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/kasprowy/349190650KASPROWY-WIERCH_p_transpozycja1.txt" , header=TRUE,  sep = "\t")
View(Kz)

Zz <- read.table(file = "C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/zakopane/349190625ZAKOPANE_p_transpozycja1.txt", header=TRUE,  sep = "\t")
Tz= read.table(file="C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/tarnow/350200575TARNOW_p_transpozycja1.txt" , header=TRUE,  sep = "\t")
View(Tz)
ggplot()+geom_line(data=Zz,y=Wartoœæ)
  #   geom_line (data = Zp, aes(x = rok, y = II), color = "royalblue", size=1.5) +
  #   geom_line (data = Tp, aes(x = rok, y = II),color = "navyblue", size= 1.5) +
  #   geom_line (data = Kp, aes(x = rok, y = II),color = "steelblue1", size= 1.5) +labs(title="Zmiennoœæ opadów na przestrzeni lat dla
  # lutego",y="iloœæ opadów [mm]", x = "Rok")
  # 
ggplot()+geom_line(data=Zz, aes(y=Wartoœæ, x=Atrybut)
acf(Tz$Wartoœæ)
acf(Zz$Wartoœæ)
acf(Kz$Wartoœæ)

pacf(Tz$Wartoœæ)
pacf(Zz$Wartoœæ)
pacf(Kz$Wartoœæ)

ggplot.corr <- function(Kz, lag.max = 24, ci = 0.95, large.sample.size = TRUE, horizontal = TRUE)
ggplot.corr(data = Kz, lag.max = 30, ci= 0.95, large.sample.size = FALSE, horizontal = TRUE)
View(ggplot.corr)
cf(Kp$I, lag.max = 12, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)

install.packages('ProjectionBasedClustering')
library(ica)
Tpmatrix <- data.matrix(Kz$Wartoœæ)
icafast(Tpmatrix,1, plot=TRUE)
Pacf(Kp1, lag.max = 12, plotIt = TRUE, na.action = na.contiguous, demean = TRUE)

z³¹czone<-read.table(file="C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/z³¹czone.txt" , header=TRUE,  sep = "\t")

z³¹czone1=read.table(file="C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/z³¹czone1.txt" , header=TRUE,  sep = "\t")
View(z³¹czone)

mean(z³¹czone$Kasprowy)
mean(z³¹czone$Tarnow)
mean(z³¹czone$Zakopane)

sum(z³¹czone$Kasprowy)/60
sum(z³¹czone$Tarnow)/60
sum(z³¹czone$Zakopane)/60
ggplot(data=z³¹czone1, aes(x=Czas, y=Kasprowy))+geom_line()+geom_smooth(method=lm)
ggplot()
install.packages('factoextra')
library("factoextra")
rad=read.table(file="C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/rad.txt" , header=TRUE,  sep = "\t")
View(rad)
radarchart(rad, axistype=1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = colors,      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,
           vlcex=0.8,
           seg=5,
           axislabcol="grey", , caxislabels=c(0, 50,100,150, 200, 250),cglwd=0.8, cglty = 1 )

colors <- c("steelblue1","royalblue", "navyblue" )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
areas <- c(rgb(0, 0, 128),rgb(0, 1, 0, 0.25),rgb(0, 0, 1, 0.25))
row=c("Tarnów", "Zakopane", "Kasprowy Wierch")
legend(legend=row, x=1.3, y=1.3, pch=20 , col=colors,  bty = "n", 
       text.col = "grey25", pt.cex = 3)


Zesz=read.table(file="C:/Users/sandr/OneDrive/Dokumenty/IN¯YNIERKA/dane/dane_meteo_stacje/malopolskie/Zeszyt1.txt" , header=TRUE,  sep = "\t")
res.pca <- prcomp(z³¹czone, scale = TRUE)
fviz_eig(res.pca)
vars_transformed <- apply(res.pca$x, 2, var)
vars_transformed/sum(vars_transformed)


cor.test(Kp$I,Kp$II)
cor.test(Kp$I,Kp$III)
cor.test(Kp$I,Kp$IV)
cor.test(Kp$I,Kp$V)
cor.test(Kp$I,Kp$VI)
cor.test(Kp$I,Kp$VII)
cor.test(Kp$I,Kp$VIII)
cor.test(Kp$I,Kp$IX)
cor.test(Kp$I,Kp$X)
cor.test(Kp$I,Kp$XI)
cor.test(Kp$I,Kp$XII)


cor.test(Kp$II,Kp$I)
cor.test(Kp$II,Kp$III)
cor.test(Kp$II,Kp$IV)
cor.test(Kp$II,Kp$V)
cor.test(Kp$II,Kp$VI)
cor.test(Kp$II,Kp$VII)
cor.test(Kp$II,Kp$VIII)
cor.test(Kp$II,Kp$IX)
cor.test(Kp$II,Kp$X)
cor.test(Kp$II,Kp$XI)
cor.test(Kp$II,Kp$XII)


cor.test(Kp$III,Kp$I)
cor.test(Kp$III,Kp$II)
cor.test(Kp$III,Kp$IV)
cor.test(Kp$III,Kp$V)
cor.test(Kp$III,Kp$VI)
cor.test(Kp$III,Kp$VII)
cor.test(Kp$III,Kp$VIII)
cor.test(Kp$III,Kp$IX)
cor.test(Kp$III,Kp$X)
cor.test(Kp$III,Kp$XI)
cor.test(Kp$III,Kp$XII)

cor.test(Kp$IV,Kp$I)
cor.test(Kp$IV,Kp$II)
cor.test(Kp$IV,Kp$III)
cor.test(Kp$IV,Kp$V)
cor.test(Kp$IV,Kp$VI)
cor.test(Kp$IV,Kp$VII)
cor.test(Kp$IV,Kp$VIII)
cor.test(Kp$IV,Kp$IX)
cor.test(Kp$IV,Kp$X)
cor.test(Kp$IV,Kp$XI)
cor.test(Kp$IV,Kp$XII)

cor.test(Kp$V,Kp$I)
cor.test(Kp$V,Kp$II)
cor.test(Kp$V,Kp$III)
cor.test(Kp$V,Kp$IV)
cor.test(Kp$V,Kp$VI)
cor.test(Kp$V,Kp$VII)
cor.test(Kp$V,Kp$VIII)
cor.test(Kp$V,Kp$IX)
cor.test(Kp$V,Kp$X)
cor.test(Kp$V,Kp$XI)
cor.test(Kp$V,Kp$XII)


cor.test(Kp$VI,Kp$I)
cor.test(Kp$VI,Kp$II)
cor.test(Kp$VI,Kp$III)
cor.test(Kp$VI,Kp$V)
cor.test(Kp$VI,Kp$IV)
cor.test(Kp$VI,Kp$VII)
cor.test(Kp$VI,Kp$VIII)
cor.test(Kp$VI,Kp$IX)
cor.test(Kp$VI,Kp$X)
cor.test(Kp$VI,Kp$XI)
cor.test(Kp$VI,Kp$XII)
cor.test(Kp$VII,Kp$I)
cor.test(Kp$VII,Kp$II)
cor.test(Kp$VII,Kp$III)
cor.test(Kp$VII,Kp$V)
cor.test(Kp$VII,Kp$IV)
cor.test(Kp$VII,Kp$VI)
cor.test(Kp$VII,Kp$VIII)
cor.test(Kp$VII,Kp$IX)
cor.test(Kp$VII,Kp$X)
cor.test(Kp$VII,Kp$XI)
cor.test(Kp$VII,Kp$XII)

cor.test(Kp$VIII,Kp$I)
cor.test(Kp$VIII,Kp$II)
cor.test(Kp$VIII,Kp$III)
cor.test(Kp$VIII,Kp$V)
cor.test(Kp$VIII,Kp$IV)
cor.test(Kp$VIII,Kp$VI)
cor.test(Kp$VIII,Kp$VII)
cor.test(Kp$VIII,Kp$IX)
cor.test(Kp$VIII,Kp$X)
cor.test(Kp$VIII,Kp$XI)
cor.test(Kp$VIII,Kp$XII)

cor.test(Kp$IX,Kp$I)
cor.test(Kp$IX,Kp$II)
cor.test(Kp$IX,Kp$III)
cor.test(Kp$IX,Kp$V)
cor.test(Kp$IX,Kp$IV)
cor.test(Kp$IX,Kp$VI)
cor.test(Kp$IX,Kp$VIII)
cor.test(Kp$IX,Kp$VII)
cor.test(Kp$IX,Kp$X)
cor.test(Kp$IX,Kp$XI)
cor.test(Kp$IX,Kp$XII)

cor.test(Kp$X,Kp$I)
cor.test(Kp$X,Kp$II)
cor.test(Kp$X,Kp$III)
cor.test(Kp$X,Kp$V)
cor.test(Kp$X,Kp$IV)
cor.test(Kp$X,Kp$VI)
cor.test(Kp$X,Kp$VIII)
cor.test(Kp$X,Kp$VII)
cor.test(Kp$X,Kp$IX)
cor.test(Kp$X,Kp$XI)
cor.test(Kp$X,Kp$XII)

cor.test(Kp$XI,Kp$I)
cor.test(Kp$XI,Kp$II)
cor.test(Kp$XI,Kp$III)
cor.test(Kp$XI,Kp$V)
cor.test(Kp$XI,Kp$IV)
cor.test(Kp$XI,Kp$VI)
cor.test(Kp$XI,Kp$VIII)
cor.test(Kp$XI,Kp$VII)
cor.test(Kp$XI,Kp$IX)
cor.test(Kp$X,Kp$XI)
cor.test(Kp$XI,Kp$XII)

cor.test(Kp$XII,Kp$I)
cor.test(Kp$XII,Kp$II)
cor.test(Kp$XII,Kp$III)
cor.test(Kp$XII,Kp$V)
cor.test(Kp$XII,Kp$IV)
cor.test(Kp$XII,Kp$VI)
cor.test(Kp$XII,Kp$VIII)
cor.test(Kp$XII,Kp$VII)
cor.test(Kp$XII,Kp$IX)
cor.test(Kp$XII,Kp$XI)
cor.test(Kp$XII,Kp$X)

cor.test(Tp$I,Tp$II)
cor.test(Tp$I,Tp$III)
cor.test(Tp$I,Tp$IV)
cor.test(Tp$I,Tp$V)
cor.test(Tp$I,Tp$VI)
cor.test(Tp$I,Tp$VII)
cor.test(Tp$I,Tp$VIII)
cor.test(Tp$I,Tp$IX)
cor.test(Tp$I,Tp$X)
cor.test(Tp$I,Tp$XI)
cor.test(Tp$I,Tp$XII)


cor.test(Tp$II,Tp$I)
cor.test(Tp$II,Tp$III)
cor.test(Tp$II,Tp$IV)
cor.test(Tp$II,Tp$V)
cor.test(Tp$II,Tp$VI)
cor.test(Tp$II,Tp$VII)
cor.test(Tp$II,Tp$VIII)
cor.test(Tp$II,Tp$IX)
cor.test(Tp$II,Tp$X)
cor.test(Tp$II,Tp$XI)
cor.test(Tp$II,Tp$XII)


cor.test(Tp$III,Tp$I)
cor.test(Tp$III,Tp$II)
cor.test(Tp$III,Tp$IV)
cor.test(Tp$III,Tp$V)
cor.test(Tp$III,Tp$VI)
cor.test(Tp$III,Tp$VII)
cor.test(Tp$III,Tp$VIII)
cor.test(Tp$III,Tp$IX)
cor.test(Tp$III,Tp$X)
cor.test(Tp$III,Tp$XI)
cor.test(Tp$III,Tp$XII)

cor.test(Tp$IV,Tp$I)
cor.test(Tp$IV,Tp$II)
cor.test(Tp$IV,Tp$III)
cor.test(Tp$IV,Tp$V)
cor.test(Tp$IV,Tp$VI)
cor.test(Tp$IV,Tp$VII)
cor.test(Tp$IV,Tp$VIII)
cor.test(Tp$IV,Tp$IX)
cor.test(Tp$IV,Tp$X)
cor.test(Tp$IV,Tp$XI)
cor.test(Tp$IV,Tp$XII)

cor.test(Tp$V,Tp$I)
cor.test(Tp$V,Tp$II)
cor.test(Tp$V,Tp$III)
cor.test(Tp$V,Tp$IV)
cor.test(Tp$V,Tp$VI)
cor.test(Tp$V,Tp$VII)
cor.test(Tp$V,Tp$VIII)
cor.test(Tp$V,Tp$IX)
cor.test(Tp$V,Tp$X)
cor.test(Tp$V,Tp$XI)
cor.test(Tp$V,Tp$XII)


cor.test(Tp$VI,Tp$I)
cor.test(Tp$VI,Tp$II)
cor.test(Tp$VI,Tp$III)
cor.test(Tp$VI,Tp$V)
cor.test(Tp$VI,Tp$IV)
cor.test(Tp$VI,Tp$VII)
cor.test(Tp$VI,Tp$VIII)
cor.test(Tp$VI,Tp$IX)
cor.test(Tp$VI,Tp$X)
cor.test(Tp$VI,Tp$XI)
cor.test(Tp$VI,Tp$XII)
cor.test(Tp$VII,Tp$I)
cor.test(Tp$VII,Tp$II)
cor.test(Tp$VII,Tp$III)
cor.test(Tp$VII,Tp$V)
cor.test(Tp$VII,Tp$IV)
cor.test(Tp$VII,Tp$VI)
cor.test(Tp$VII,Tp$VIII)
cor.test(Tp$VII,Tp$IX)
cor.test(Tp$VII,Tp$X)
cor.test(Tp$VII,Tp$XI)
cor.test(Tp$VII,Tp$XII)

cor.test(Tp$VIII,Tp$I)
cor.test(Tp$VIII,Tp$II)
cor.test(Tp$VIII,Tp$III)
cor.test(Tp$VIII,Tp$V)
cor.test(Tp$VIII,Tp$IV)
cor.test(Tp$VIII,Tp$VI)
cor.test(Tp$VIII,Tp$VII)
cor.test(Tp$VIII,Tp$IX)
cor.test(Tp$VIII,Tp$X)
cor.test(Tp$VIII,Tp$XI)
cor.test(Tp$VIII,Tp$XII)

cor.test(Tp$IX,Tp$I)
cor.test(Tp$IX,Tp$II)
cor.test(Tp$IX,Tp$III)
cor.test(Tp$IX,Tp$V)
cor.test(Tp$IX,Tp$IV)
cor.test(Tp$IX,Tp$VI)
cor.test(Tp$IX,Tp$VIII)
cor.test(Tp$IX,Tp$VII)
cor.test(Tp$IX,Tp$X)
cor.test(Tp$IX,Tp$XI)
cor.test(Tp$IX,Tp$XII)

cor.test(Tp$X,Tp$I)
cor.test(Tp$X,Tp$II)
cor.test(Tp$X,Tp$III)
cor.test(Tp$X,Tp$V)
cor.test(Tp$X,Tp$IV)
cor.test(Tp$X,Tp$VI)
cor.test(Tp$X,Tp$VIII)
cor.test(Tp$X,Tp$VII)
cor.test(Tp$X,Tp$IX)
cor.test(Tp$X,Tp$XI)
cor.test(Tp$X,Tp$XII)

cor.test(Tp$XI,Tp$I)
cor.test(Tp$XI,Tp$II)
cor.test(Tp$XI,Tp$III)
cor.test(Tp$XI,Tp$V)
cor.test(Tp$XI,Tp$IV)
cor.test(Tp$XI,Tp$VI)
cor.test(Tp$XI,Tp$VIII)
cor.test(Tp$XI,Tp$VII)
cor.test(Tp$XI,Tp$IX)
cor.test(Tp$X,Tp$XI)
cor.test(Tp$XI,Tp$XII)

cor.test(Tp$XII,Tp$I)
cor.test(Tp$XII,Tp$II)
cor.test(Tp$XII,Tp$III)
cor.test(Tp$XII,Tp$V)
cor.test(Tp$XII,Tp$IV)
cor.test(Tp$XII,Tp$VI)
cor.test(Tp$XII,Tp$VIII)
cor.test(Tp$XII,Tp$VII)
cor.test(Tp$XII,Tp$IX)
cor.test(Tp$XII,Tp$XI)
cor.test(Tp$XII,Tp$X)

cor.test(Zp$I,Zp$II)
cor.test(Zp$I,Zp$III)
cor.test(Zp$I,Zp$IV)
cor.test(Zp$I,Zp$V)
cor.test(Zp$I,Zp$VI)
cor.test(Zp$I,Zp$VII)
cor.test(Zp$I,Zp$VIII)
cor.test(Zp$I,Zp$IX)
cor.test(Zp$I,Zp$X)
cor.test(Zp$I,Zp$XI)
cor.test(Zp$I,Zp$XII)


cor.test(Zp$II,Zp$I)
cor.test(Zp$II,Zp$III)
cor.test(Zp$II,Zp$IV)
cor.test(Zp$II,Zp$V)
cor.test(Zp$II,Zp$VI)
cor.test(Zp$II,Zp$VII)
cor.test(Zp$II,Zp$VIII)
cor.test(Zp$II,Zp$IX)
cor.test(Zp$II,Zp$X)
cor.test(Zp$II,Zp$XI)
cor.test(Zp$II,Zp$XII)


cor.test(Zp$III,Zp$I)
cor.test(Zp$III,Zp$II)
cor.test(Zp$III,Zp$IV)
cor.test(Zp$III,Zp$V)
cor.test(Zp$III,Zp$VI)
cor.test(Zp$III,Zp$VII)
cor.test(Zp$III,Zp$VIII)
cor.test(Zp$III,Zp$IX)
cor.test(Zp$III,Zp$X)
cor.test(Zp$III,Zp$XI)
cor.test(Zp$III,Zp$XII)

cor.test(Zp$IV,Zp$I)
cor.test(Zp$IV,Zp$II)
cor.test(Zp$IV,Zp$III)
cor.test(Zp$IV,Zp$V)
cor.test(Zp$IV,Zp$VI)
cor.test(Zp$IV,Zp$VII)
cor.test(Zp$IV,Zp$VIII)
cor.test(Zp$IV,Zp$IX)
cor.test(Zp$IV,Zp$X)
cor.test(Zp$IV,Zp$XI)
cor.test(Zp$IV,Zp$XII)

cor.test(Zp$V,Zp$I)
cor.test(Zp$V,Zp$II)
cor.test(Zp$V,Zp$III)
cor.test(Zp$V,Zp$IV)
cor.test(Zp$V,Zp$VI)
cor.test(Zp$V,Zp$VII)
cor.test(Zp$V,Zp$VIII)
cor.test(Zp$V,Zp$IX)
cor.test(Zp$V,Zp$X)
cor.test(Zp$V,Zp$XI)
cor.test(Zp$V,Zp$XII)


cor.test(Zp$VI,Zp$I)
cor.test(Zp$VI,Zp$II)
cor.test(Zp$VI,Zp$III)
cor.test(Zp$VI,Zp$V)
cor.test(Zp$VI,Zp$IV)
cor.test(Zp$VI,Zp$VII)
cor.test(Zp$VI,Zp$VIII)
cor.test(Zp$VI,Zp$IX)
cor.test(Zp$VI,Zp$X)
cor.test(Zp$VI,Zp$XI)
cor.test(Zp$VI,Zp$XII)
cor.test(Zp$VII,Zp$I)
cor.test(Zp$VII,Zp$II)
cor.test(Zp$VII,Zp$III)
cor.test(Zp$VII,Zp$V)
cor.test(Zp$VII,Zp$IV)
cor.test(Zp$VII,Zp$VI)
cor.test(Zp$VII,Zp$VIII)
cor.test(Zp$VII,Zp$IX)
cor.test(Zp$VII,Zp$X)
cor.test(Zp$VII,Zp$XI)
cor.test(Zp$VII,Zp$XII)

cor.test(Zp$VIII,Zp$I)
cor.test(Zp$VIII,Zp$II)
cor.test(Zp$VIII,Zp$III)
cor.test(Zp$VIII,Zp$V)
cor.test(Zp$VIII,Zp$IV)
cor.test(Zp$VIII,Zp$VI)
cor.test(Zp$VIII,Zp$VII)
cor.test(Zp$VIII,Zp$IX)
cor.test(Zp$VIII,Zp$X)
cor.test(Zp$VIII,Zp$XI)
cor.test(Zp$VIII,Zp$XII)

cor.test(Zp$IX,Zp$I)
cor.test(Zp$IX,Zp$II)
cor.test(Zp$IX,Zp$III)
cor.test(Zp$IX,Zp$V)
cor.test(Zp$IX,Zp$IV)
cor.test(Zp$IX,Zp$VI)
cor.test(Zp$IX,Zp$VIII)
cor.test(Zp$IX,Zp$VII)
cor.test(Zp$IX,Zp$X)
cor.test(Zp$IX,Zp$XI)
cor.test(Zp$IX,Zp$XII)

cor.test(Zp$X,Zp$I)
cor.test(Zp$X,Zp$II)
cor.test(Zp$X,Zp$III)
cor.test(Zp$X,Zp$V)
cor.test(Zp$X,Zp$IV)
cor.test(Zp$X,Zp$VI)
cor.test(Zp$X,Zp$VIII)
cor.test(Zp$X,Zp$VII)
cor.test(Zp$X,Zp$IX)
cor.test(Zp$X,Zp$XI)
cor.test(Zp$X,Zp$XII)

cor.test(Zp$XI,Zp$I)
cor.test(Zp$XI,Zp$II)
cor.test(Zp$XI,Zp$III)
cor.test(Zp$XI,Zp$V)
cor.test(Zp$XI,Zp$IV)
cor.test(Zp$XI,Zp$VI)
cor.test(Zp$XI,Zp$VIII)
cor.test(Zp$XI,Zp$VII)
cor.test(Zp$XI,Zp$IX)
cor.test(Zp$X,Zp$XI)
cor.test(Zp$XI,Zp$XII)

cor.test(Zp$XII,Zp$I)
cor.test(Zp$XII,Zp$II)
cor.test(Zp$XII,Zp$III)
cor.test(Zp$XII,Zp$V)
cor.test(Zp$XII,Zp$IV)
cor.test(Zp$XII,Zp$VI)
cor.test(Zp$XII,Zp$VIII)
cor.test(Zp$XII,Zp$VII)
cor.test(Zp$XII,Zp$IX)
cor.test(Zp$XII,Zp$XI)
cor.test(Zp$XII,Zp$X)


