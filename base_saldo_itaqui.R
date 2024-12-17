setwd("~/TCC/dados")

### pasta origem do novocaged: ftp://ftp.mtps.gov.br/pdet/microdados/

# RODAR

# PACOTES ----
library(tidyverse)
library(archive)
library(readr)
library(dplyr)
library(janitor)

# DEFININDO COMPETÊNCIAS E PERÍODO -----
competencias <- c('MOV', 'FOR', 'EXC')
anos <- 2020:2023 
meses <- formatC(1:12, width = 2, flag = '0')

# DEFININDO CNAES UTILIZADAS PARA O PORTO DO ITAQUI
cnaes <- c(5232000, 5231102, 5211701, 4731800, 3511501)

# MANUSEANDO OS ARQUIVOS ----
for(k in seq_along(competencias)){
  assign(tolower(paste0('caged', competencias[k], '_baixadas')),
         fs::dir_ls(glob = paste0('CAGED', competencias[k], '*.7z$')))
  
  assign(tolower(paste0('caged', competencias[k], '_lista')),
         list())
}

# CONCATENANDO BASE DO MARANHÃO ----
# Func para ler arquivos zipados e filtrar por Maranhão
arquivos_caged <- function(entrada) {
  # assegurando apenas entradas válidas
  if(!any(entrada == c('MOV', 'FOR', 'EXC'))) {
    stop("Competencia deve ser FOR, MOV ou EXC", call. = FALSE)}
  
  # criando lista com caminho de cada arquivo
  caminho_dos_arquivos <- get(paste0('caged', tolower(entrada), '_baixadas'))
  lista_arquivos_periodo <- vector(mode = 'list', length = length(caminho_dos_arquivos))
  
  # lendo cada arquivo da lista e salvando como tibble 
  for(l in seq_along(caminho_dos_arquivos)) {
    cat('Carregando arquivo', caminho_dos_arquivos[l],
        ' | loop', l, 'de', length(caminho_dos_arquivos), '\n')
    arquivo <- readr::read_csv2(archive::archive_read(caminho_dos_arquivos[l])) |>
      janitor::clean_names() %>%
      filter(uf == 21 & subclasse %in% cnaes) |>
      mutate(salario = as.numeric(salario),
             horascontratuais = as.numeric(horascontratuais),
             valorsalariofixo = as.numeric(valorsalariofixo))
    lista_arquivos_periodo[[l]] <- arquivo
  }
  
  # Concatenando os dataframes da lista em um único dataframe
  df_final <- dplyr::bind_rows(lista_arquivos_periodo)
  return(df_final)
}

# Extrair por competências
## manter isso para trabalhar com as bases mais rapidamente
df_mov <- arquivos_caged('MOV')
df_for <- arquivos_caged('FOR')
df_exc <- arquivos_caged('EXC')

setwd("~/TCC/novocaged/memoriaR/portuaria_ma")
# saveRDS(df_mov, 'ma_base_mov_port.Rds')
# saveRDS(df_for, 'ma_base_for_port.Rds')
# saveRDS(df_exc, 'ma_base_exc_port.Rds')

rm(cagedexc_baixadas,cagedexc_lista,cagedfor_baixadas,cagedfor_lista,cagedmov_baixadas,cagedmov_lista,arquivos_caged,competencias,k,meses,anos,cnaes)

# CÁLCULO ATIVIDADE PORTUÁRIA - MARANHÃO ----
montar_saldo <- function(df){
  saldo <- df |>
    group_by(competenciamov,municipio,secao,subclasse,cbo2002ocupacao,graudeinstrucao,
             idade,racacor,sexo,tamestabjan, tipodedeficiencia,salario) |>
    summarise(saldo = sum(saldomovimentacao))
  return(saldo)
}

saldo_mov_port <- montar_saldo(df_mov)
saldo_for_port <- montar_saldo(df_for)
saldo_exc_port <- montar_saldo(df_exc)

# Somar os saldos de movimentação e fora do prazo pela competência
saldo_soma_port <- bind_rows(
  mutate(saldo_mov_port, competencia = as.character(competenciamov)),
  mutate(saldo_for_port, competencia = as.character(competenciamov)),
) %>%
  group_by(competenciamov,municipio,secao,subclasse,cbo2002ocupacao,graudeinstrucao,
           idade,racacor,sexo,tamestabjan,tipodedeficiencia,salario) %>%
  summarise(saldo = sum(saldo, na.rm = TRUE))

## Calcular saldo ajustado
### MERGE
saldo_ajustado_port <- left_join(saldo_soma_port, saldo_exc_port, 
                                 by = c("competenciamov","municipio","secao",
                                        "subclasse","cbo2002ocupacao","graudeinstrucao",
                                        "idade","racacor","sexo","tamestabjan", "tipodedeficiencia","salario")) %>%
  mutate(saldo_ajuste = saldo.x - coalesce(saldo.y, 0)) %>%
  select(competenciamov,municipio, secao,subclasse,cbo2002ocupacao,graudeinstrucao,
         idade,racacor,sexo,tamestabjan,tipodedeficiencia,saldo_ajuste,salario)
rm(saldo_exc_port,saldo_for_port,saldo_mov_port,saldo_soma_port,montar_saldo)

# setwd("~/TCC/novocaged/memoriaR/portuaria_ma")
# saveRDS(saldo_ajustado_port, "ma_base_perfil.Rds")

# Calcular saldo ajustado - serie
saldo_serie_port <- saldo_ajustado_port |>
  group_by(competenciamov) |>
  arrange(competenciamov) |>
  summarise(saldo_serie = sum(saldo_ajuste),
            admissoes = sum(saldo_ajuste > 0),
            desligamentos = sum(saldo_ajuste < 0))
### CALCULO DE ADM-DESL NÃO BATE COM SALDO  

# Gráfico 
library(lubridate)
saldo_serie_port <- saldo_serie_port |>
  mutate(date = ymd(paste0(competenciamov,'01'))) |>
  mutate(data = format(date, "%Y/%m")) |>
  subset(select = -c(date))

ggplot(data=saldo_serie_port,
       mapping = aes(x=data, y=saldo_serie)) +
  geom_bar(stat = 'identity') +
  labs(x = "ano/mês", y = "saldo ajustado") +
  ggtitle('Saldo de Empregos no Porto do Itaqui - Maranhão') +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5)) 

# setwd("~/TCC/novocaged/graph")
# ggsave('saldo_portuario_ma.png')

library(writexl)
# setwd('~/TCC/novocaged/salvo_excel')
# write_xlsx(saldo_serie_port, 'saldo_porto_maranhao.xlsx')
