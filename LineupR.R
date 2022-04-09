########################## Lineup Project - R ################################# 


# Definindo o diretorio
setwd('D:/Commodities-Lineup-Maritime/Lineup')
getwd()


# Carregando pacotes
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)
library(readxl)


# Leitura de arquivo Excel - Lineup 07/01.2022
?read_excel
Lineup <- read_excel('Datamar Lineup 2022.01.07.xlsx', col_names = TRUE)
View(Lineup)
summary(Lineup)
str(Lineup)
View(head(Lineup))

## Fonte de dados tirada do sistema interno da minha empresa ##


# Testando subsets, somente grupo de graos
graos <- subset(Lineup, `COMM GROUP` == 'GRAINS')
graos <- na.omit(graos)
summary(Lineup)
nrow(Lineup)
nrow(graos)


# Conversão de datas
graos$ETS <- as.POSIXct(graos$ETS, format = '%Y-%m-%d')
graos$Month <- month(graos$ETS)
graos$Year <- year(graos$ETS)


# O campo de peso "Weight" foi procesado como character pela linguagem R
# Fazemos a conversão de Character para Inteiro
graos$WEIGHT <- as.numeric(graos$WEIGHT)

View(graos)
summary(graos)
class(graos$WEIGHT)
class(graos$Month)
View(head(graos))
head(graos)


# Extraido apenas inforamçoes das commodities: Soja, Milho e Arroz
# Anos de interesse: 2021/2022
soja <- subset(graos, CARGO == 'SOYBEANS')
soja <- subset(soja, Year %in% c(2021, 2022))

milho <- subset(graos, CARGO == 'CORN')
milho <- subset(milho, Year %in% c(2021, 2022))

arroz <- subset(graos, CARGO == 'RICE')
arroz <- subset(arroz, Year %in% c(2021, 2022))


# Plots referentes aos dados solicitados
g_soja <- ggplot(soja, aes(x = (WEIGHT), y = Month, color == as.factor(Year))) + 
  geom_smooth(se = FALSE, fill = NA, size = 2) +
  theme_light(base_size = 20) +
  xlab('Peso Total')+
  ylab('Mes') + 
  scale_color_discrete("") +
  ggtitle('Peso ao longo dos Meses') +
  theme(plot.title = element_text(size = 18))

g_soja


g_milho <- ggplot(milho, aes(x = (WEIGHT), y = Month, color == as.factor(Year))) + 
  geom_smooth(se = FALSE, fill = NA, size = 2) +
  theme_light(base_size = 20) +
  xlab('Peso Total')+
  ylab('Mes') + 
  scale_color_discrete("") +
  ggtitle('Peso ao longo dos Meses') +
  theme(plot.title = element_text(size = 18))

g_milho

g_arroz <- ggplot(arroz, aes(x = (WEIGHT), y = Month, color == as.factor(Year))) + 
  geom_smooth(se = FALSE, fill = NA, size = 2) +
  theme_light(base_size = 20) +
  xlab('Peso Total')+
  ylab('Mes') + 
  scale_color_discrete("") +
  ggtitle('Peso ao longo dos Meses') +
  theme(plot.title = element_text(size = 18))

g_arroz
