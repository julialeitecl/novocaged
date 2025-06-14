library(tidyverse)
library(readxl)
library(writexl)
library(tidytable)
library(stats)

# 1 Tratamento e limpeza ----
setwd("~/TCC/novocaged/memoriaR/portuaria_ma")
data <- readRDS('base_tratada_perfil.Rds')

# limpeza do df
df <- subset(data, select = c("porto", "genero", "cor_raca", "instrucao", 
                              "faixa_salarial", "tipo_trabalhador"))


# 2 Dummies ----
dummies <- get_dummies(df)
dummies <- dummies |>
  select()

#INDEPENDENTES
#genero:    Homem
#cor/raça:  Branca 
#           Parda
#           Preta
#           Amarela
#           Indígena
#escolaridade: 
#           Médio Completo
#           Fundamental Completo
#           Superior Completo
#           Fundamental Incompleto
#           Pós-Graduação Completa
#salario:   <1SM 
#           1-3SM
#           3-5SM
#           5-10SM
#saldo: 1 Admitidos

#DEPENDENTE: porto publico ou privado 

dummies <- dummies |> 
  select(genero_Homem,
         cor_raca_Branca,cor_raca_Parda,cor_raca_Preta,cor_raca_Amarela,cor_raca_Indígena,
         `instrucao_Médio Completo`,`instrucao_Fundamental Completo`,`instrucao_Superior Completo`,`instrucao_Fundamental Incompleto`,`instrucao_Pós-Graduação Completa`,
         `faixa_salarial_<1SM`,`faixa_salarial_1-3SM`,`faixa_salarial_3-5SM`,`faixa_salarial_5-10SM`,
         saldo_Admitidos,
         tipo_trab_manual)

model <- glm(tipo_trab_manual ~ genero_Homem, 
             family = binomial(link = "logit"), data = dummies)

summary(model)

