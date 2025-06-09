# EXTRAIR SALDO MARANHÃO GERAL 
## para os anos de 2020-2024 (jan-dez)

### pasta origem do novocaged: ftp://ftp.mtps.gov.br/pdet/microdados/
setwd("~/TCC/dados")

# RODAR PACOTES ----
library(tidyverse)
library(archive)
library(readr)
library(dplyr)
library(janitor)
library(writexl)

# DEFININDO COMPETÊNCIAS E PERÍODO -----
competencias <- c('MOV', 'FOR', 'EXC')
anos <- 2020:2024
meses <- formatC(1:12, width = 2, flag = '0')

# Utilizar para extrair dados de cada pasta (nomes) e colocar em uma pasta só
# library(fs)
# pasta <- 'C:/Users/NOVO/Documents/TCC/dados'
# 
# for (ano in anos){
#   for (mes in meses){
#     print(paste0(ano,mes))
#     arquivos <- dir_ls(paste0(pasta,'/',ano,'/',ano,mes), recurse = TRUE)
#     for (arquivo in arquivos){
#       file.copy(arquivo, path(pasta, path_file(arquivo)))
#     }}}


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
      filter(uf == 21) |>
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

rm(cagedexc_baixadas,cagedexc_lista,cagedfor_baixadas,cagedfor_lista,cagedmov_baixadas,cagedmov_lista,arquivos_caged,competencias,k,meses,anos)

# CÁLCULO DE ADMITIDOS, DESLIGADOS E SALDO DO PORTO DO ITAQUI - MARANHÃO ----
montar_saldo <- function(df){
  saldo <- df |>
    group_by(competenciamov) |>
    summarise(saldo = sum(saldomovimentacao),
              admitidos = sum(saldomovimentacao > 0),
              desligados = sum(saldomovimentacao < 0))
}

saldo_mov_port <- montar_saldo(df_mov)
saldo_for_port <- montar_saldo(df_for)
saldo_exc_port <- montar_saldo(df_exc)

# Somar os saldos de movimentação e fora do prazo pela competência
saldo_soma_port <- bind_rows(
  mutate(saldo_mov_port, competencia = as.character(competenciamov)),
  mutate(saldo_for_port, competencia = as.character(competenciamov)),
) %>%
  group_by(competenciamov) %>%
  summarise(saldo = sum(saldo, na.rm = TRUE),
            admitidos = sum(admitidos, na.rm = TRUE),
            desligados = sum(desligados, na.rm = TRUE))

## Calcular saldo ajustado
### MERGE
saldo_ajustado_port <- left_join(saldo_soma_port, saldo_exc_port, 
                                 by = c("competenciamov")) |>
  mutate(saldo_ajuste = saldo.x - coalesce(saldo.y, 0),
         admitidos_ajuste = admitidos.x - coalesce(admitidos.y, 0),
         desligados_ajuste = desligados.x - coalesce(desligados.y, 0)) |>
  select(competenciamov,saldo_ajuste,admitidos_ajuste,desligados_ajuste)

rm(saldo_exc_port,saldo_for_port,saldo_mov_port,saldo_soma_port,montar_saldo)


## Gráfico ----
library(lubridate)
saldo_ajustado_port <- saldo_ajustado_port |>
  mutate(date = ymd(paste0(competenciamov,'01'))) |>
  mutate(data = format(date, "%Y/%m")) |>
  subset(select = -c(date))

ggplot(data=saldo_ajustado_port,
       mapping = aes(x=data, y=saldo_ajuste)) +
  geom_bar(stat = 'identity') +
  labs(x = "ano/mês", y = "saldo ajustado") +
  ggtitle('Saldo de Empregos do Maranhão (2020-2025)') +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5)) 

# setwd("~/TCC/novocaged/graph")
# ggsave('saldo_portuario_ma.png')

setwd('C:/Users/NOVO/Documents/TCC/novocaged/salvo_excel')
write_xlsx(saldo_ajustado_port, 'saldo_por_mes_ma.xlsx')
