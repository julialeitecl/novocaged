library(tidyverse)
library(readxl)
library(psych)

# 1 Tratamento e limpeza ----
base_mov <- readRDS('ma_base_mov_port.Rds')
base_for <- readRDS('ma_base_for_port.Rds')
# base de excluídos para atv portuária no MA é vazia

base <- bind_rows(base_mov,base_for)

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
  subset(select = -c(competenciamov,regiao,origemdainformacao,tipodedeficiencia,
                     tipoempregador,uf,horascontratuais,tipoestabelecimento,tipomovimentacao,
                     indicadoraprendiz,tamestabjan,competenciadec,indicadordeforadoprazo,
                     indtrabintermitente,indtrabparcial)) |>
  select(ano,mes,everything())

## renomear pelo nome das categorias
dict_cbo <-
  readxl::read_excel("dicionario.xlsx", sheet = 'cbo2002ocupação')
  
base <- base|>
  left_join(dict_cbo, by = c('cbo2002ocupacao' = 'Código')) |>
  rename(cbonome = Descrição) 

# 2 Saldo por ano ----
saldo_por_ano <- base |>
  group_by(ano) |>
  summarise(saldo = sum(saldomovimentacao))

# 3 Sazonalidade: trabalho mais destruído e criado ----
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

# 4 / Perfil do trabalhador médio por anos ----
## ideia: fazer perfil de admitidos e desligados separadamente
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
                                (racacor == 6) ~ 'Não informada',
                                (racacor == 9) ~ 'Não Identificado',
                              TRUE ~ NA),
         # unificar
         'instrucao' = case_when((graudeinstrucao == 1) ~ 'Analfabeto',
                                 (graudeinstrucao == 2) ~ 'Até 5ª Incompleto',
                                 (graudeinstrucao == 3) ~ '5ª Completo Fundamental',
                                 (graudeinstrucao == 4) ~ '6ª a 9ª Fundamental',
                                 (graudeinstrucao == 5) ~ 'Fundamental Completo',
                                 (graudeinstrucao == 6) ~ 'Médio Incompleto',
                                 (graudeinstrucao == 7) ~ 'Médio Completo',
                                 (graudeinstrucao == 8) ~ 'Superior Incompleto',
                                 (graudeinstrucao == 9) ~ 'Superior Completo',
                                 (graudeinstrucao == 10) ~ 'Mestrado',
                                 (graudeinstrucao == 11) ~ 'Doutorado',
                                 (graudeinstrucao == 80) ~ 'Pós-Graduação completa',
                                 (graudeinstrucao == 99) ~ 'Não Identificado',
                                 TRUE ~ NA),
                                 # PROPOSTA DE UNIFICAÇÃO:
                                 # (graudeinstrucao == 1) ~ 'Analfabeto', #poucos valores
                                 # (graudeinstrucao %in% c(2,3,4)) ~ 'Fundamental Incompleto',
                                 # (graudeinstrucao == 5) ~ 'Fundamental Completo', 
                                 # (graudeinstrucao == 6) ~ 'Médio Incompleto',
                                 # (graudeinstrucao == 7) ~ 'Médio Completo',
                                 # (graudeinstrucao == 8) ~ 'Superior Incompleto',
                                 # (graudeinstrucao == 9) ~ 'Superior Completo',
                                 # (graudeinstrucao %in% c(10,11,80)) ~ 'Pós-Graduação completa',
                                 # (graudeinstrucao == 99) ~ 'Não Identificado' #vazio
         # https://www.dieese.org.br/analisecestabasica/salarioMinimo.html
         # baseado nas classes de rendimento da SIS
         # salario != valorsalariofixo, investigar
         'faixa_salarial' = case_when((ano == 2020 & mes == 1 & salario > 1039/2 & salario <= 1039) ~ '<1SM',
                                      (ano == 2020 & mes == 1 & salario > 1039 & salario <= 2*1039) ~ '1-2SM',
                                      (ano == 2020 & mes == 1 & salario > 2*1039 & salario <= 3*1039) ~ '2-3SM',
                                      (ano == 2020 & mes == 1 & salario > 3*1039 & salario <= 4*1039) ~ '3-4SM',
                                      (ano == 2020 & mes == 1 & salario > 4*1039 & salario <= 5*1039) ~ '4-5SM',
                                      (ano == 2020 & mes == 1 & salario > 5*1039 & salario <= 10*1039) ~ '5-10SM',
                                      (ano == 2020 & mes == 1 & salario > 10*1039) ~ '>10SM',
           
                                      (ano == 2020 & mes != 1 & salario > 1045/2 & salario <= 1045) ~ '<1SM',
                                      (ano == 2020 & mes != 1 & salario > 1045 & salario <= 2*1045) ~ '1-2SM',
                                      (ano == 2020 & mes != 1 & salario > 2*1045 & salario <= 3*1045) ~ '2-3SM',
                                      (ano == 2020 & mes != 1 & salario > 3*1045 & salario <= 4*1045) ~ '3-4SM',
                                      (ano == 2020 & mes != 1 & salario > 4*1045 & salario <= 5*1045) ~ '4-5SM',
                                      (ano == 2020 & mes != 1 & salario > 5*1045 & salario <= 10*1045) ~ '5-10SM',
                                      (ano == 2020 & mes != 1 & salario > 10*1045) ~ '>10SM',
                                      
                                      (ano == 2021 & salario > 1100/2 & salario <= 1100) ~ '<1SM',
                                      (ano == 2021 & salario > 1100 & salario <= 2*1100) ~ '1-2SM',
                                      (ano == 2021 & salario > 2*1100 & salario <= 3*1100) ~ '2-3SM',
                                      (ano == 2021 & salario > 3*1100 & salario <= 4*1100) ~ '3-4SM',
                                      (ano == 2021 & salario > 4*1100 & salario <= 5*1100) ~ '4-5SM',
                                      (ano == 2021 & salario > 5*1100 & salario <= 10*1100) ~ '5-10SM',
                                      (ano == 2021 & salario > 10*1100) ~ '>10SM',
                                      
                                      (ano == 2022 & salario > 1212/2 & salario <= 1212) ~ '<1SM',
                                      (ano == 2022 & salario > 1212 & salario <= 2*1212) ~ '1-2SM',
                                      (ano == 2022 & salario > 2*1212 & salario <= 3*1212) ~ '2-3SM',
                                      (ano == 2022 & salario > 3*1212 & salario <= 4*1212) ~ '3-4SM',
                                      (ano == 2022 & salario > 4*1212 & salario <= 5*1212) ~ '4-5SM',
                                      (ano == 2022 & salario > 5*1212 & salario <= 10*1212) ~ '5-10SM',
                                      (ano == 2022 & salario > 10*1212) ~ '>10SM',
                                      
                                      (ano == 2023 & mes < 5 & salario > 1302/2 & salario <= 1302) ~ '<1SM',
                                      (ano == 2023 & mes < 5 & salario > 1302 & salario <= 2*1302) ~ '1-2SM',
                                      (ano == 2023 & mes < 5 & salario > 2*1302 & salario <= 3*1302) ~ '2-3SM',
                                      (ano == 2023 & mes < 5 & salario > 3*1302 & salario <= 4*1302) ~ '3-4SM',
                                      (ano == 2023 & mes < 5 & salario > 4*1302 & salario <= 5*1302) ~ '4-5SM',
                                      (ano == 2023 & mes < 5 & salario > 5*1302 & salario <= 10*1302) ~ '5-10SM',
                                      (ano == 2023 & mes < 5 & salario > 10*1302) ~ '>10SM',
                                      
                                      (ano == 2023 & mes >= 5 & salario > 1320/2 & salario <= 1320) ~ '<1SM',
                                      (ano == 2023 & mes >= 5 & salario > 1320 & salario <= 2*1320) ~ '1-2SM',
                                      (ano == 2023 & mes >= 5 & salario > 2*1320 & salario <= 3*1320) ~ '2-3SM',
                                      (ano == 2023 & mes >= 5 & salario > 3*1320 & salario <= 4*1320) ~ '3-4SM',
                                      (ano == 2023 & mes >= 5 & salario > 4*1320 & salario <= 5*1320) ~ '4-5SM',
                                      (ano == 2023 & mes >= 5 & salario > 5*1320 & salario <= 10*1320) ~ '5-10SM',
                                      (ano == 2023 & mes >= 5 & salario > 10*1320) ~ '>10SM'
                                      )
         ) |>
  select(ano,genero,cor_raca,instrucao,cbonome,faixa_salarial,salario)

# 4.2 Descritivas por ano ----
# obs: rodar dev.off() em caso de estado gráfico inválido
  # Gênero
table(base_perfil$genero,base_perfil$ano)
round(prop.table(table(base_perfil$genero,base_perfil$ano))*100,2)
ggplot(base_perfil, aes(x=ano)) + 
  geom_bar(aes(fill = as.factor(genero)),position = "fill") +
  labs(title = 'Distribuição dos trabalhadores por gênero e ano')

  # Cor/Raça
table(base_perfil$cor_raca,base_perfil$ano)
round(prop.table(table(base_perfil$cor_raca,base_perfil$ano))*100,2)
ggplot(base_perfil, aes(x=ano)) + 
  geom_bar(aes(fill = as.factor(cor_raca)),position = "fill") +
  labs(title = 'Distribuição dos trabalhadores por cor/raça e ano')

  # Escolaridade
table(base_perfil$instrucao,base_perfil$ano)
round(prop.table(table(base_perfil$instrucao,base_perfil$ano))*100,2)
ggplot(base_perfil, aes(x=ano)) + 
  geom_bar(aes(fill = as.factor(instrucao)),position = "fill") +
  labs(title = 'Distribuição dos trabalhadores por escolaridade e ano')

  # Ocupação do trabalhador
View(table(base_perfil$cbonome,base_perfil$ano))
View(round(prop.table(table(base_perfil$cbonome,base_perfil$ano))*100,2))

  # Salário
round(prop.table(table(base_perfil$faixa_salarial))*100,2)
table(base_perfil$faixa_salarial,base_perfil$ano)
round(prop.table(table(base_perfil$faixa_salarial,base_perfil$ano))*100,2)
ggplot(base_perfil, aes(x=ano)) + 
  geom_bar(aes(fill = as.factor(faixa_salarial)),position = "fill") +
  labs(title = 'Distribuição dos trabalhadores por faixa salarial e ano')
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

# 4.3 / Gráficos de Dispersão ----
  
  
# 5 / Dummies ----

## genero: base homem 
## raça: base branca (+?) 
## escolaridade: base (+?)
## faixa de renda (1 sm de cada ano) (+?)
