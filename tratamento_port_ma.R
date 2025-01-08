library(tidyverse)
library(readxl)
library(psych)
library(RColorBrewer) #cores graph
library(scales) #uso de formato de número
library(writexl)
library(forcats)

# 1 Tratamento e limpeza ----
setwd('~/TCC/novocaged/memoriaR/portuaria_ma')
base <- readRDS('ma_base_perfil.Rds')

View(describe(base))
unique(base$municipio)

# conferir valores únicos
# for (coluna in colnames(base)) {
#   cat(coluna, ":", unique(base[[coluna]]), "\n","\n")
# }

# criando ano e mes
base$ano <- as.numeric(substr(base$competenciamov,1,4))
base$mes <- as.numeric(substr(base$competenciamov,5,6))

# limpeza cols
base <- base |>
  subset(select = -c(competenciamov,tipodedeficiencia)) |>
  select(ano,mes,everything())

## renomear pelo nome das categorias
setwd("~/TCC/novocaged")
dict_cbo <-
  readxl::read_excel("dicionario.xlsx", sheet = 'cbo2002ocupação')
  
base <- base|>
  left_join(dict_cbo, by = c('cbo2002ocupacao' = 'Código')) |>
  rename(cbonome = Descrição) 
rm(dict_cbo)
View(describe(base))

# 2 Saldo por ano e mes ----
saldo_por_ano <- base |>
  group_by(ano) |>
  summarise(saldo = sum(saldomovimentacao))

saldo_por_mes <- base |>
  group_by(ano,mes) |>
  summarise(saldo = sum(saldomovimentacao))

# 3 Sazonalidade ----
## Saldo
ggplot(saldo_por_mes, aes(x = mes, y = saldo, color = factor(ano), group = ano)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + 
  ggtitle("Saldo de empregos do Porto do Itaqui por ano e mês (2020-2023)") +
  labs(x = "Mês",
       y = 'Saldo') +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Anos") 

## Movimentação portuária
setwd('~/TCC/novocaged')
mov_port <- read_excel('mov_port_itaqui.xlsx')
# limpando base 
mov_port <- mov_port |>
  select(-"Nome da Instalação") |>
  rename(ano = Ano, mes = Mês, saldo = 'Total de Movimentação Portuária em toneladas (t)')

# alterando meses 
mov_port <- mov_port |>
  mutate(mes = recode(mes,
                     'jan' = '01',
                     'fev' = '02',
                     'mar' = '03',
                     'abr' = '04',
                     'mai' = '05',
                     'jun' = '06',
                     'jul' = '07',
                     'ago' = '08',
                     'set' = '09',
                     'out' = '10',
                     'nov' = '11',
                     'dez' = '12')) 

# gráfico com anos separados
ggplot(mov_port, aes(x = as.numeric(mes), y = saldo, color = factor(ano), group = ano)) +
  geom_line(size = 1) +
  ggtitle("Movimentação portuária (t) do Porto do Itaqui por ano e mês (2020-2023)") +
  labs(x = "mês",
       y = 'movimentação') +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Anos") +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  scale_y_continuous(labels = scales::number)

setwd("~/TCC/novocaged/graph")
ggsave('mov_port_itaqui_por_ano.png')
# gráfico com série contínua
mov_port <- mov_port |>
  mutate(date = ymd(paste0(ano,mes,'01'))) |>
  mutate(data = format(date, "%Y/%m")) |>
  subset(select = -c(date))

ggplot(mov_port, aes(x=data, y=saldo, group = 1)) +
  geom_line() +
  labs(x = "ano/mês", y = "movimentação") +
  ggtitle('Movimentação portuária (t) do Porto do Itaqui por ano e mês (2020-2023)') +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::number)

ggsave('mov_port_itaqui_serie.png')

## Trabalho mais "destruído e criado" 
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
  # N/I: Não identificado
base_perfil <- base |>
  mutate('genero' = case_when((sexo == 1) ~ 'Homem',
                            (sexo == 3) ~ 'Mulher',
                            (sexo == 9) ~ 'N/I',
                            TRUE ~ NA),
         'cor_raca' = case_when((racacor == 1) ~ 'Branca',
                                (racacor == 2) ~ 'Preta',
                                (racacor == 3) ~ 'Parda',
                                (racacor == 4) ~ 'Amarela',
                                (racacor == 5) ~ 'Indígena',
                                (racacor == 6) ~ 'Não informada',
                                (racacor == 9) ~ 'N/I',
                                TRUE ~ NA),
         # PROPOSTA DE UNIFICAÇÃO:
         'instrucao' = case_when((graudeinstrucao %in% c(2,3,4)) ~ 'Fundamental Incompleto',
                                 (graudeinstrucao %in% c(5,6)) ~ 'Fundamental Completo',
                                 (graudeinstrucao %in% c(7,8)) ~ 'Médio Completo',
                                 (graudeinstrucao == 9) ~ 'Superior Completo',
                                 (graudeinstrucao %in% c(10,11,80)) ~ 'Pós-Graduação Completa',
                                 (graudeinstrucao %in% c(1,99)) ~ 'N/I', #Não Identificado e analfabetos
                                 TRUE ~ NA),
         # https://www.dieese.org.br/analisecestabasica/salarioMinimo.html
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
  select(ano,genero,cor_raca,instrucao,cbo2002ocupacao,cbonome,faixa_salarial,salario,saldomovimentacao)

# limpeza no salário
base_perfil <- na.omit(base_perfil)

setwd("~/TCC/novocaged/memoriaR")
saveRDS(base_perfil, 'ma_base_perfil_tratada.Rds')

# 4.2 Descritivas ----
# obs: rodar dev.off() em caso de estado gráfico inválido
# paletas: display.brewer.all()
  # Saldo de movimentação
table(base_perfil$saldomovimentacao)

  # Gênero
abs_gen<-table(base_perfil$genero)
rel_gen<-round(prop.table(table(base_perfil$genero))*100, 1)
gen_tab <- data.frame(
  Categ = names(abs_gen),
  Frequencia = as.numeric(abs_gen),
  Proporcao = as.numeric(rel_gen)) 

x1<-ggplot(base_perfil, 
       aes(x = fct_rev(fct_infreq(genero)), # reordenar pelos valores
           fill = genero)) + 
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  xlab("Gênero") + 
  ylab("Contagem") +
  labs(title = 'Distribuição dos trabalhadores por gênero (2020-2023)') +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

  # Cor/Raça
abs_cor<-table(base_perfil$cor_raca)
rel_cor<-round(prop.table(table(base_perfil$cor_raca))*100, 1)
cor_tab <- data.frame(
  Categ = names(abs_cor),
  Frequencia = as.numeric(abs_cor),
  Proporcao = as.numeric(rel_cor)) 

x2<-ggplot(base_perfil, 
       aes(x = fct_rev(fct_infreq(cor_raca)), # reordenar pelos valores
           fill = cor_raca)) + 
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  xlab("Cor/raça") + 
  ylab("Contagem") +
  labs(title = 'Distribuição dos trabalhadores por cor/raça (2020-2023)') +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

  # Escolaridade
abs_escol<-table(base_perfil$instrucao)
rel_escol<-round(prop.table(table(base_perfil$instrucao))*100, 1)
escol_tab <- data.frame(
  Categ = names(abs_escol),
  Frequencia = as.numeric(abs_escol),
  Proporcao = as.numeric(rel_escol)) 

x3 <- ggplot(base_perfil, 
       aes(x = fct_rev(fct_infreq(instrucao)), # reordenar pelos valores
           fill = instrucao)) + 
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  xlab("Escolaridade") + 
  ylab("Contagem") +
  labs(title = 'Distribuição dos trabalhadores por escolaridade (2020-2023)') +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

  # Salário
abs_sal<-table(base_perfil$faixa_salarial)
rel_sal<-round(prop.table(table(base_perfil$faixa_salarial))*100, 1)
sal_tab <- data.frame(
  Categ = names(abs_sal),
  Frequencia = as.numeric(abs_sal),
  Proporcao = as.numeric(rel_sal))
x4<- ggplot(base_perfil, 
               aes(x = fct_rev(fct_infreq(faixa_salarial)), # reordenar pelos valores
                   fill = faixa_salarial)) + 
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  xlab("Faixa salarial") + 
  ylab("Contagem") +
  labs(title = 'Distribuição dos trabalhadores por faixa salarial (SM) (2020-2023)') +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
  
library(patchwork)
quadro<-x1+x2+x3+x4
quadro
setwd("~/TCC/novocaged/graph")
ggsave('perfil_port_ma.png')

# Salário médio 
# concentração dos valores
Q1 <- quantile(base$salario, probs = 0.25, na.rm = T)
Q2 <- quantile(base$salario, probs = 0.50, na.rm = T) # mediana
Q3 <- quantile(base$salario, probs = 0.75, na.rm = T)
# histograma de salários menores que 10.000
ggplot(base,
       aes(x=salario)) + 
  geom_histogram() +
  xlim(0, 10000)

  # para cada nível de instrução
  base_perfil |>
    filter(!is.na(salario), !is.na(instrucao)) |>
    group_by(instrucao) |>
    summarise(mean_salario = mean(salario)) |>
    View()
    
  # por gênero
  base_perfil |>
    filter(!is.na(salario), !is.na(genero)) |>
    group_by(genero) |>
    summarise(mean_salario = mean(salario)) |>
    View()

  # por cor/raça
  base_perfil |>
    filter(!is.na(salario), !is.na(cor_raca)) |>
    group_by(cor_raca) |>
    summarise(mean_salario = mean(salario)) |>
    View()
  
  # por cor/raça e gênero
  base_perfil |>
    filter(!is.na(salario), !is.na(genero), !is.na(cor_raca)) |>
    group_by(genero,cor_raca) |>
    summarise(mean_salario = mean(salario)) |>
    View()
