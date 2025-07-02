library(tidyverse)
library(readxl)
library(writexl)
library(tidytable)
library(stats)
library(broom)        # Para formatar resultados do modelo
library(car)          # Para diagnóstico (opcional)

# 1 Tratamento e limpeza ----
setwd("~/TCC/novocaged/memoriaR/portuaria_ma")
data <- readRDS('base_tratada_perfil.Rds')

# limpeza do df
df <- subset(data, select = c("porto", "genero", "cor_raca", "instrucao", 
                              "tipo_trabalhador", "faixa_salarial"))

# 2 Dummies ----
# 1. Variável dependente: 1 = Porto do Itaqui, 0 = Portos privados
df$porto_publico <- ifelse(df$porto == "Público", 1, 0)

# 2. Variável dependente: 1 = Chefes, 0 = Técnicos
df$trabalhador_chefe <- ifelse(df$tipo_trabalhador == "Chefia", 1, 0)

  # Converter variáveis categóricas em fator
df <- df %>%
  mutate(
    porto = factor(porto, levels = c("Privado", "Público")),
    genero = factor(genero, levels = c("Homem", "Mulher")),
    cor_raca = factor(cor_raca, levels = c("Parda", "Branca", "Preta", "Amarela", "Indígena")),
    # de acordo com a ordem das descritivas
    instrucao = factor(instrucao, levels = c("Médio Completo", "Superior Completo", "Fundamental Completo", 
                                             "Fundamental Incompleto","Pós-Graduação Completa")),
    faixa_salarial = factor(faixa_salarial, levels = c("1-3SM", "<1SM", "3-5SM", "5-10SM", ">10SM"))
  )

# descritiva das variáveis selecionadas
summary(df)

# 3 Estimação dos modelos de regressão logística ----
## Modelo para tipo de porto 
modelo1 <- glm(porto_publico ~ genero + cor_raca + instrucao + faixa_salarial,
                    data = df,
                    family = binomial(link = "logit"))

summary(modelo1)

tabela_resultados1 <- tidy(modelo1, exponentiate = TRUE, conf.int = TRUE)

## Modelo para tipo de trabalhador
modelo2 <- glm(trabalhador_chefe ~ genero + cor_raca + instrucao + faixa_salarial + porto,
               data = df,
               family = binomial(link = "logit"))
summary(modelo2)
