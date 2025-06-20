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
                              "tipo_trabalhador"))

# 2 Dummies ----
# Variável dependente: 1 = Porto do Itaqui, 0 = Portos privados
df$porto_publico <- ifelse(df$porto == "Público", 1, 0)

# Variáveis independentes (exemplo com gênero, raça e escolaridade)
  # Converter variáveis categóricas em fator
df <- df %>%
  mutate(
    genero = factor(genero, levels = c("Homem", "Mulher")),
    cor_raca = factor(cor_raca, levels = c("Parda", "Branca", "Preta", "Outros")),
    instrucao = factor(instrucao, levels = c("Médio Completo", "Superior Completo", "Fundamental Completo", "Pós-Graduação Completa", "Fundamental Incompleto"))
  )

summary(df)

# 3 Estimação do modelo de regressão logística ----
modelo_logit <- glm(porto_publico ~ genero + cor_raca + instrucao,
                    data = df,
                    family = binomial(link = "logit"))

# 4 Resultados do modelo ----
summary(modelo_logit)

  # Odds ratios e intervalos de confiança
exp(cbind(OddsRatio = coef(modelo_logit), confint(modelo_logit)))

  # Resultado arrumado com broom
tidy(modelo_logit, exponentiate = TRUE, conf.int = TRUE)

# 5 Diagnóstico e qualidade do ajuste ----
  # Pseudo R² de McFadden
pR2(modelo_logit)

  # VIF para verificar multicolinearidade
vif(modelo_logit)

# 6 Visualização dos odds ratios ----
  # Criar gráfico dos odds ratios
library(ggplot2)
resultados <- tidy(modelo_logit, exponentiate = TRUE, conf.int = TRUE)

ggplot(resultados, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(title = "Odds Ratios do modelo logístico",
       x = "Variáveis",
       y = "Odds Ratio") +
  coord_flip() +
  theme_minimal()
