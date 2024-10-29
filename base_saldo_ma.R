setwd("~/TCC/dados")

### pasta origem do novocaged: ftp://ftp.mtps.gov.br/pdet/microdados/

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

# MANUSEANDO OS ARQUIVOS ----
# Copiando arquivos da pasta original para diretório de tratamento
## rodar apenas se necessário puxar arquivos de outro diretório
# library(fs)
# pasta_original <- '...'
# pasta_destino <- '...'
# 
# for (ano in anos){
#   for (mes in meses){
#     print(paste0(ano,mes))
#     arquivos <- dir_ls(paste0(pasta_original,'/',ano,'/',ano,mes), recurse = TRUE)
#     for (arquivo in arquivos){
#       file.copy(arquivo, path(pasta_destino, path_file(arquivo)))
# }}}

# Unindo os arquivos de mesma natureza
# obs: importante que todos os arquivos estejam na mesma pasta
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
rm(cagedexc_baixadas,cagedexc_lista,cagedfor_baixadas,cagedfor_lista,cagedmov_baixadas,cagedmov_lista,arquivos_caged)

# CÁLCULO GERAL - MARANHÃO ----
## Func para extrair o saldo
montar_saldo <- function(df){
  saldo <- df |>
    group_by(competenciamov,municipio,secao,subclasse,cbo2002ocupacao,graudeinstrucao,
             idade,racacor,sexo,tamestabjan, tipodedeficiencia) |>
    summarise(saldo = sum(saldomovimentacao))
  return(saldo)
}
saldo_mov <- montar_saldo(df_mov)
saldo_for <- montar_saldo(df_for)
saldo_exc <- montar_saldo(df_exc)

# Somar os saldos de movimentação e fora do prazo pela competência
saldo_soma <- bind_rows(saldo_mov, saldo_for) |>
  group_by(competenciamov,municipio,secao,subclasse,cbo2002ocupacao,graudeinstrucao,
           idade,racacor,sexo,tamestabjan,tipodedeficiencia) |>
  summarise(saldo = sum(saldo, na.rm = TRUE))

## Calcular saldo ajustado
saldo_ajustado <- left_join(saldo_soma, saldo_exc, 
                            by = c("competenciamov","municipio","secao",
                                   "subclasse","cbo2002ocupacao","graudeinstrucao",
                                   "idade","racacor","sexo","tamestabjan", "tipodedeficiencia")) |>
  mutate(saldo_ajuste = saldo.x - coalesce(saldo.y, 0)) |>
  select(competenciamov,municipio, secao,subclasse,cbo2002ocupacao,graudeinstrucao,
         idade,racacor,sexo,tamestabjan,tipodedeficiencia,saldo_ajuste)

# Calcular saldo ajustado - serie
saldo_serie <- saldo_ajustado |>
  group_by(competenciamov) |>
  arrange(competenciamov) |>
  summarise(saldo_serie = sum(saldo_ajuste))

# Gráfico 
library(lubridate)
saldo_serie <- saldo_serie |>
  mutate(date = ymd(paste0(competenciamov,'01'))) |>
  mutate(data = format(date, "%Y/%m")) |>
  subset(select = -c(date))
  
ggplot(data=saldo_serie,
       mapping = aes(x=data, y=saldo_serie)) +
  geom_bar(stat = 'identity') +
  labs(x = "ano/mês", y = "saldo ajustado") +
  ggtitle('Saldo Geral - Maranhão') +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5)) 
  
setwd("~/TCC/novocaged/graph")
ggsave('saldo_portuario_ma.png')

library(writexl)
setwd('~/TCC/novocaged/salvo_excel')
write_xlsx(saldo_serie, 'saldo_maranhao.xlsx')
