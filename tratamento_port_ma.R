library(tidyverse)
library(readxl)
library(psych)
library(RColorBrewer) #cores graph

# 1 Tratamento e limpeza ----
setwd('~/TCC/novocaged/memoriaR/portuaria_ma')
base <- readRDS('ma_base_perfil.Rds')

View(describe(base))
unique(base$municipio)
# result: 211130 (slz) 210173 (belagua)

# conferir valores únicos
# for (coluna in colnames(base)) {
#   cat(coluna, ":", unique(base[[coluna]]), "\n","\n")
# }

# criando ano e mes
base$ano <- as.numeric(substr(base$competenciamov,1,4))
base$mes <- as.numeric(substr(base$competenciamov,5,6))

# limpeza cols
base <- base |>
  subset(select = -c(competenciamov,tipodedeficiencia,tamestabjan)) |>
  select(ano,mes,everything())

## renomear pelo nome das categorias
setwd("~/TCC/novocaged")
dict_cbo <-
  readxl::read_excel("dicionario.xlsx", sheet = 'cbo2002ocupação')
  
base <- base|>
  left_join(dict_cbo, by = c('cbo2002ocupacao' = 'Código')) |>
  rename(cbonome = Descrição, saldomovimentacao = saldo_ajuste) 

View(describe(base))

# 2 Saldo por ano e mes ----
saldo_por_ano <- base |>
  group_by(ano) |>
  summarise(saldo = sum(saldomovimentacao))

saldo_por_mes <- base |>
  group_by(ano,mes) |>
  summarise(saldo = sum(saldomovimentacao))

# 3 Sazonalidade ----
ggplot(saldo_por_mes, aes(x = mes, y = saldo, color = factor(ano), group = ano)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + 
  ggtitle("Saldo de empregos por ano e mês (2020-2023)") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Anos") 

# Trabalho mais "destruído e criado" ----
# 3.1 Por mês 
saz_por_cbo_m <- base |>
  group_by(mes, cbonome) |> 
  summarise(saldo = sum(saldomovimentacao),
            criados = sum(saldomovimentacao == 1),
            destruidos = sum(saldomovimentacao == -1)) |>
  arrange(mes,desc(saldo))

# 3.2 Por ano 
saz_por_cbo_y <- base |>
  group_by(ano, cbonome) |> 
  summarise(saldo = sum(saldomovimentacao),
            criados = sum(saldomovimentacao == 1),
            destruidos = sum(saldomovimentacao == -1)) |>
  arrange(ano,desc(saldo))

# 4 Perfil do trabalhador médio por anos ----
# 4.1 Labels ----
  # renomeando variáveis categóricas e criando classes de salário
base_perfil <- base |>
  mutate('genero' = case_when((sexo == 1) ~ 'Homem',
                            (sexo == 3) ~ 'Mulher',
                            TRUE ~ NA),
         'cor_raca' = case_when((racacor == 1) ~ 'Branca',
                                (racacor == 2) ~ 'Preta',
                                (racacor == 3) ~ 'Parda',
                                (racacor == 4) ~ 'Amarela',
                                (racacor == 5) ~ 'Indígena',
                                (racacor %in% c(6,9)) ~ 'N/A',
                                TRUE ~ NA),
         # PROPOSTA DE UNIFICAÇÃO:
         'instrucao' = case_when((graudeinstrucao %in% c(2,3,4)) ~ 'Fundamental Incompleto',
                                 (graudeinstrucao %in% c(5,6)) ~ 'Fundamental Completo',
                                 (graudeinstrucao %in% c(7,8)) ~ 'Médio Completo',
                                 (graudeinstrucao == 9) ~ 'Superior Completo',
                                 (graudeinstrucao %in% c(10,11,80)) ~ 'Pós-Graduação Completa',
                                 (graudeinstrucao %in% c(1,99)) ~ 'N/A', #Não Identificado e analfabetos
                                 TRUE ~ NA),
         # https://www.dieese.org.br/analisecestabasica/salarioMinimo.html
         # baseado nas classes de rendimento da SIS
         # salario != valorsalariofixo 
         'faixa_salarial' = case_when((ano == 2020 & mes == 1 & salario <= 1039) ~ '<1SM',
                                      (ano == 2020 & mes == 1 & salario > 1039 & salario <= 3*1039) ~ '1-3SM',
                                      (ano == 2020 & mes == 1 & salario > 3*1039 & salario <= 5*1039) ~ '3-5SM',
                                      (ano == 2020 & mes == 1 & salario > 5*1039 & salario <= 10*1039) ~ '5-10SM',
                                      (ano == 2020 & mes == 1 & salario > 10*1039) ~ '>10SM',
           
                                      (ano == 2020 & mes != 1 & salario <= 1045) ~ '<1SM',
                                      (ano == 2020 & mes != 1 & salario > 1045 & salario <= 3*1045) ~ '1-3SM',
                                      (ano == 2020 & mes != 1 & salario > 3*1045 & salario <= 5*1045) ~ '3-5SM',
                                      (ano == 2020 & mes != 1 & salario > 5*1045 & salario <= 10*1045) ~ '5-10SM',
                                      (ano == 2020 & mes != 1 & salario > 10*1045) ~ '>10SM',
                                      
                                      (ano == 2021 & salario <= 1100) ~ '<1SM',
                                      (ano == 2021 & salario > 1100 & salario <= 3*1100) ~ '1-3SM',
                                      (ano == 2021 & salario > 3*1100 & salario <= 5*1100) ~ '3-5SM',
                                      (ano == 2021 & salario > 5*1100 & salario <= 10*1100) ~ '5-10SM',
                                      (ano == 2021 & salario > 10*1100) ~ '>10SM',
                                      
                                      (ano == 2022 & salario <= 1212) ~ '<1SM',
                                      (ano == 2022 & salario > 1212 & salario <= 3*1212) ~ '1-3SM',
                                      (ano == 2022 & salario > 3*1212 & salario <= 5*1212) ~ '3-5SM',
                                      (ano == 2022 & salario > 5*1212 & salario <= 10*1212) ~ '5-10SM',
                                      (ano == 2022 & salario > 10*1212) ~ '>10SM',
                                      
                                      (ano == 2023 & mes < 5 & salario <= 1302) ~ '<1SM',
                                      (ano == 2023 & mes < 5 & salario > 1302 & salario <= 3*1302) ~ '1-3SM',
                                      (ano == 2023 & mes < 5 & salario > 3*1302 & salario <= 5*1302) ~ '3-5SM',
                                      (ano == 2023 & mes < 5 & salario > 5*1302 & salario <= 10*1302) ~ '5-10SM',
                                      (ano == 2023 & mes < 5 & salario > 10*1302) ~ '>10SM',
                                      
                                      (ano == 2023 & mes >= 5 & salario <= 1320) ~ '<1SM',
                                      (ano == 2023 & mes >= 5 & salario > 1320 & salario <= 3*1320) ~ '1-3SM',
                                      (ano == 2023 & mes >= 5 & salario > 3*1320 & salario <= 5*1320) ~ '3-5SM',
                                      (ano == 2023 & mes >= 5 & salario > 5*1320 & salario <= 10*1320) ~ '5-10SM',
                                      (ano == 2023 & mes >= 5 & salario > 10*1320) ~ '>10SM'
                                      )
         ) |>
  select(ano,genero,cor_raca,instrucao,cbonome,faixa_salarial,salario)

# 4.2 Descritivas por ano ----
# obs: rodar dev.off() em caso de estado gráfico inválido
# paletas: display.brewer.all()
  # Gênero
table(base_perfil$genero,base_perfil$ano)
x1<-ggplot(base_perfil, aes(x=ano)) + 
  geom_bar(aes(fill = as.factor(genero)),position = "fill") +
  ggtitle("Distribuição dos trabalhadores por gênero e ano") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name="Gênero") 

  # Cor/Raça
table(base_perfil$cor_raca,base_perfil$ano)
x2<-ggplot(base_perfil, aes(x=ano)) + 
  geom_bar(aes(fill = as.factor(cor_raca)),position = "fill") +
  ggtitle("Distribuição dos trabalhadores por cor/raça e ano") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name="Cor/Raça",
                      breaks=c('Amarela','Branca','Indígena','Parda','Preta','N/A'))

  # Escolaridade
table(base_perfil$instrucao,base_perfil$ano)
x3<-ggplot(base_perfil, aes(x=ano)) + 
  geom_bar(aes(fill = as.factor(instrucao)),position = "fill") +
  labs(title = 'Distribuição dos trabalhadores por escolaridade e ano') +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name="Escolaridade",
                      breaks=c('Fundamental Incompleto','Fundamental Completo',
                               'Médio Completo', 'Superior Completo',
                               'Pós-Graduação Completa', 'N/A'))

  # Ocupação do trabalhador
View(table(base_perfil$cbonome,base_perfil$ano))

  # Salário
table(base_perfil$faixa_salarial,base_perfil$ano)
x4<-ggplot(base_perfil, aes(x=ano)) + 
  geom_bar(aes(fill = as.factor(faixa_salarial)),position = "fill") +
  labs(title = 'Distribuição dos trabalhadores por faixa salarial (SM) e ano') +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name="Faixa Salarial (SM)",
                      breaks=c('<1SM','1-3SM','3-5SM','5-10SM','>10SM'))
  
library(patchwork)
quadro<-x1+x2+x3+x4
setwd("~/TCC/novocaged/graph")
ggsave('perfil_port_ma.png')

    # histograma de salários menores que 30.000
ggplot(base,
       aes(x=salario)) + 
  geom_histogram() +
  xlim(0, 30000)
    # concentração dos valores
Q1 <- quantile(base$salario, probs = 0.25)
Q2 <- quantile(base$salario, probs = 0.50) # mediana
Q3 <- quantile(base$salario, probs = 0.75)

# Salário médio 
  # para cada nível de instrução
  View(data.frame(tapply(base_perfil$salario, base_perfil$instrucao, mean)))
  # por gênero
  View(data.frame(tapply(base_perfil$salario, base_perfil$genero, mean)))
  # por cor/raça
  View(data.frame(tapply(base_perfil$salario, base_perfil$cor_raca, mean)))
  # por cor/raça e gênero
  View(data.frame(tapply(base_perfil$salario, 
                         list(base_perfil$cor_raca,base_perfil$genero), 
                         mean)))

