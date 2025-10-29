# Analiza_danych_pogodowych
Projekt przedstawia analizę danych meteorologicznych (temperatura i opady) z wybranych stacji w Małopolsce na podstawie danych IMGW.   Wykorzystano dane ze stacji: - **Zakopane** - **Tarnów** - **Kasprowy Wierch**

Analiza obejmuje:  
- przygotowanie i czyszczenie danych,  
- analizę statystyczną (korelacje, testy istotności),  
- wizualizacje czasowe i porównawcze,  
- analizę PCA i wykresy radarowe.  

## ⚙️ Wykorzystane biblioteki R  
Projekt korzysta z następujących pakietów:
```r
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(corrplot)
library(fmsb)
library(PerformanceAnalytics)
