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

base <- subset(base, subclasse %in% c(5211701, 5231102, 5232000))

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

## renomear pelo nome das categorias na CBO
setwd("~/TCC/novocaged/documentacao")
dict_cbo <-
  readxl::read_excel("dicionario.xlsx", sheet = 'cbo2002ocupação')
  
base <- base|>
  left_join(dict_cbo, by = c('cbo2002ocupacao' = 'Código')) |>
  rename(cbonome = Descrição) 
rm(dict_cbo)

estrutura_cbo <-
  readxl::read_excel("estrutura_cbo.xlsx", sheet = 'estrutura')
base <- base |>
  left_join(estrutura_cbo, by = c('cbo2002ocupacao' = 'ocupacao')) 
rm(estrutura_cbo)

# 2 Saldo por ano e mes ----
saldo_por_ano <- base |>
  group_by(ano) |>
  summarise(saldo = sum(saldomovimentacao))

saldo_por_mes <- base |>
  group_by(ano,mes) |>
  summarise(saldo = sum(saldomovimentacao))

# 3 Sazonalidade e saldo geral ----
## Saldo de movimentação de trabalhadores
ggplot(saldo_por_mes, aes(x = mes, y = saldo, color = factor(ano), group = ano)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + 
  ggtitle("Saldo de empregos do Porto do Itaqui por ano e mês (2020-2023)") +
  labs(x = "Mês",
       y = 'Saldo') +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Anos") 

## Admitidos


## Desligados


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


# 5 Perfil do trabalhador médio por anos ----
## 5.1 Labels ----
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
                                      ),
         'tipo_trab' = case_when(cbo2002ocupacao %in% c(311105,311405,311505,311515,311605,311710,312105,312320,313105,313110,313115,
                                                        313120,313125,313130,313205,313215,313220,313410,314110,314115,314120,314210,
                                                        314410,317205,317210,318505,321105,342110,342115,342405,342410,342535,374110,
                                                        391115,511115,511210,511215,512105,513205,513405,513425,513435,513505,513605,
                                                        514120,514225,514310,514320,514325,516345,517110,517310,517330,517410,517415,
                                                        517420,519110,519910,519935,521105,521110,521125,521130,521135,521140,622005,
                                                        622010,622020,622730,623110,632120,632125,641010,641015,642005,710205,711210,
                                                        711325,715115,715125,715130,715135,715210,715405,715615,715705,716610,717020,
                                                        721210,721215,723315,724205,724315,724410,725010,725105,725705,732105,732140,
                                                        761327,771105,773325,781705,782110,782115,782120,782125,782220,782305,782310,
                                                        782315,782410,782505,782510,782515,782705,782710,782715,782720,783105,783110,
                                                        783210,783215,783220,783225,783230,784205,811115,811410,811630,813120,813125,
                                                        813130,821105,823235,823320,828105,848305,848425,848505,861110,861115,861205,
                                                        862150,862155,862160,862305,911110,911305,914105,914405,914410,914425,915105,
                                                        919105,919110,919205,951105,953115,992105,992115,992225) 
                                 ~ 'manual',
                                 cbo2002ocupacao %in% c(111220,121005,122610,123105,123110,123115,123305,123405,
                                                        123805,131120,141205,141305,141405,141410,141415,141420,
                                                        141505,141510,141605,141610,141615,141710,141720,142105,
                                                        142110,142115,142205,142210,142305,142315,142320,142325,
                                                        142330,142335,142405,142410,142415,142505,142510,142530,
                                                        142705,201215,201225,202110,212405,212410,212415,212420,
                                                        213205,214005,214010,214205,214270,214280,214305,214315,
                                                        214320,214325,214405,214530,214905,214910,214915,214935,
                                                        215105,215140,215205,215215,223530,223710,224120,225140,
                                                        241005,241010,241040,251215,251225,251605,252105,252205,
                                                        252210,252305,252405,252545,252550,252705,252710,252715,
                                                        252720,253115,253125,254305,261210,261215,262410,300105,
                                                        300305,314805,322215,342105,342120,342125,342205,342210,
                                                        342305,342310,342605,342610,351305,351425,351430,351505,
                                                        351605,352205,352310,353230,354125,354130,354140,354205,
                                                        373140,375105,375110,391120,391125,391130,391135,391145,
                                                        391205,391210,391215,410105,410205,410215,410235,410240,
                                                        411005,411010,411025,411030,411045,412205,413105,413110,
                                                        413115,413210,414105,414110,414115,414125,414135,414140,
                                                        414210,414215,415210,420105,420110,420135,421105,421125,
                                                        421305,421310,422105,422120,422125,422315,423105,510105,
                                                        510305,510310,516805,520105,520110,521115,630110,710105,
                                                        760405,810110,820210,860110,860115,910105,910115,950105,
                                                        950110,950205,950305,992205) 
                                 ~ 'nao_manual',
                                 cbo2002ocupacao == 999999 ~ 'nao_identificado',
                                 TRUE ~ NA)
         # cnaes cluster itaqui: c(5232000, 5231102, 5211701, 4731800, 3511501)
         # complexo portuário restante: c(5231101,5231102,5231103)
         # separação de cnae 5231102 entre a divisão?
         # 'itaqui' = case_when(subclasse %in% c(5232000, 5231102, 5211701, 4731800, 3511501) ~ 'cluster',
         #                      subclasse %in% c(5231101,5231102,5231103) ~ 'restante')
         ) |>
  select(ano,genero,cor_raca,instrucao,faixa_salarial,salario,saldomovimentacao,
         tipo_trab,
         cbo2002ocupacao,cbonome,grande_grupo,nome_grande_grupo,sub_principal,
         nome_sub_principal,subgrupo,nome_subgrupo,familia,nome_familia)

# limpeza no salário
base_perfil <- na.omit(base_perfil)

# setwd("~/TCC/novocaged/memoriaR")
# saveRDS(base_perfil, 'ma_base_perfil_tratada.Rds')

## 5.2 Descritivas ----
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

ggplot(base_perfil, 
       aes(x = fct_rev(fct_infreq(genero)), # reordenar pelos valores
           fill = genero)) + 
  geom_bar(stat = "count") +  # usará a contagem das barras
  geom_text(aes(label = ..count..), 
            stat = "count", 
            hjust = -0.2, # Ajuste para posicionar o rótulo fora da barra
            color = "black", # Cor preta para o texto
            fontface = "bold") + # Deixar o texto em negrito
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  xlab("Gênero") + 
  ylab("Contagem") +
  labs(title = 'Distribuição dos trabalhadores por gênero (2020-2023)') +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")


  # Cor/Raça
abs_cor<-table(base_perfil$cor_raca)
rel_cor<-round(prop.table(table(base_perfil$cor_raca))*100, 1)
cor_tab <- data.frame(
  Categ = names(abs_cor),
  Frequencia = as.numeric(abs_cor),
  Proporcao = as.numeric(rel_cor)) 

ggplot(base_perfil, 
       aes(x = fct_rev(fct_infreq(cor_raca)), # reordenar pelos valores
           fill = cor_raca)) + 
  geom_bar(stat = "count") +  # usará a contagem das barras
  geom_text(aes(label = ..count..), 
            stat = "count", 
            hjust = -0.2, # Ajuste para posicionar o rótulo fora da barra
            color = "black", # Cor preta para o texto
            fontface = "bold") + # Deixar o texto em negrito
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  xlab("Cor/raça") + 
  ylab("Contagem") +
  labs(title = 'Distribuição dos trabalhadores por cor/raça (2020-2023)') +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")


  # Escolaridade
abs_escol<-table(base_perfil$instrucao)
rel_escol<-round(prop.table(table(base_perfil$instrucao))*100, 1)
escol_tab <- data.frame(
  Categ = names(abs_escol),
  Frequencia = as.numeric(abs_escol),
  Proporcao = as.numeric(rel_escol)) 

ggplot(base_perfil, 
       aes(x = fct_rev(fct_infreq(instrucao)), # reordenar pelos valores
           fill = instrucao)) + 
  geom_bar(stat = "count") +  # usará a contagem das barras
  geom_text(aes(label = ..count..), 
            stat = "count", 
            hjust = -0.2, # Ajuste para posicionar o rótulo fora da barra
            color = "black", # Cor preta para o texto
            fontface = "bold") + # Deixar o texto em negrito
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  xlab("Escolaridade") + 
  ylab("Contagem") +
  labs(title = 'Distribuição dos trabalhadores por escolaridade (2020-2023)') +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")


  # Salário
abs_sal<-table(base_perfil$faixa_salarial)
rel_sal<-round(prop.table(table(base_perfil$faixa_salarial))*100, 1)
sal_tab <- data.frame(
  Categ = names(abs_sal),
  Frequencia = as.numeric(abs_sal),
  Proporcao = as.numeric(rel_sal))


ggplot(base_perfil, 
       aes(x = fct_rev(fct_infreq(faixa_salarial)), # reordenar pelos valores
           fill = faixa_salarial)) + 
  geom_bar(stat = "count") +  # usará a contagem das barras
  geom_text(aes(label = ..count..), 
            stat = "count", 
            hjust = -0.2, # Ajuste para posicionar o rótulo fora da barra
            color = "black", # Cor preta para o texto
            fontface = "bold") + # Deixar o texto em negrito
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  xlab("Faixa salarial") + 
  ylab("Contagem") +
  labs(title = 'Distribuição dos trabalhadores por faixa salarial (SM) (2020-2023)') +
  theme_bw(base_size = 16) +
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
