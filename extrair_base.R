setwd("C:/Users/NOVO/Documents/TCC/dados")

### pasta origem: ftp://ftp.mtps.gov.br/pdet/microdados/

# Pacotes ----
library(tidyverse)
library(archive)
library(readr)
library(dplyr)
library(janitor)

# Definindo competências e período ------
competencias <- c('MOV', 'FOR', 'EXC')
anos <- 2020:2023 
meses <- formatC(1:12, width = 2, flag = '0')

# Copiando arquivos da pasta original para diretório de tratamento ----
## rodar apenas se necessário puxar arquivos de outro diretório
library(fs)
pasta_original <- 'C:/Users/NOVO/Documents/TCC/dados'
pasta_destino <- 'C:/Users/NOVO/Documents/TCC/dados'

for (ano in anos){
  for (mes in meses){
    print(paste0(ano,mes))
    arquivos <- dir_ls(paste0(pasta_original,'/',ano,'/',ano,mes), recurse = TRUE)
    for (arquivo in arquivos){
      file.copy(arquivo, path(pasta_destino, path_file(arquivo)))
}}}

# Unindo os arquivos de mesma natureza ----
for(k in seq_along(competencias)){
  assign(tolower(paste0('caged', competencias[k], '_baixadas')),
         fs::dir_ls(glob = paste0('CAGED', competencias[k], '*.7z$')))
  
  assign(tolower(paste0('caged', competencias[k], '_lista')),
         list())
}

# Lendo arquivos zipados e filtrando por uf e subclasse portuária ----
for(i in seq_along(cagedmov_baixadas)){
  cat('Lendo periodo', i, '\n')
  read_csv2(archive_read(cagedmov_baixadas[i])) |>
    clean_names() |>
    dplyr::filter(uf == 21 & subclasse %in% c(5231101,5231102,5231103)) ->
    cagedfor_lista[[i]]
}

ler<-read_csv2(archive_read('CAGEDEXC202004.7z'))

