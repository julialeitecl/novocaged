library(tidyverse)
library(readxl)
library(psych)
library(scales) #uso de formato de número
library(writexl)

# 1 Tratamento e limpeza ----
setwd('~/TCC/novocaged/memoriaR/portuaria_ma')
df <- readRDS('ma_base_perfil.Rds')

# limpeza do df
df <- subset(df, select = c('sexo','racacor','graudeinstrucao',
                            'cbo2002ocupacao','salario','saldomovimentacao'))

# remover NA em salário
colSums(is.na(df))
df <- na.omit(df)

# 2 Correlação ----
knitr::kable(cor(df))
pairs(df)

# Frequência
prop.table(table(df$sexo))*100
prop.table(table(df$racacor))*100
round(prop.table(table(df$graudeinstrucao))*100, 2)
prop.table(table(df$saldomovimentacao))*100

# DUMMIES
#genero

#cor/raça

#escolaridade

#salario

