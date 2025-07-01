# EXTRAI A BASE ESPECIFICAMENTE PARA O PORTO DO ITAQUI PELAS CNAES SELECIONADAS DE ATUACAO DO PORTO
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

# DEFININDO CNAES UTILIZADAS PARA O PORTO DO ITAQUI
## cluster Itaqui
## cnaes <- c(5232000, 5231102, 5211701)
  # 5232000 – Atividade de Agenciamento Marítimo
  # 5231102 – Atividade do Operador Portuário
  # 5211701 – Armazenagem Gerais  emissão de warrant
## resto do complexo portuário ma 
## cnaes <- c()
  # 5231101 – Administração da Infraestrutura Portuária
  # 5212500 – Carga e Descarga
  # 5231103 – Gestão de Terminais Aquaviários
  # 5239701 – Serviços de Praticagem
  # 5239799 - Atividades Auxiliares dos Transportes Aquaviários não Especificadas Anteriormente
  # 5030101 - Navegação de Apoio Marítimo
  # 5030102 - Navegação de Apoio Portuário
  # 5099801 - Transporte Aquaviário para Passeios Turísticos
  # 5099899 - Outros Transportes Aquaviários não Especificados Anteriormente
cnaes <- c(5231102,5232000,5211701,
           5231101,5231103,5212500,5239701,
           5239799,5030101,5030102,5099801,5099899)
  
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

# setwd("~/TCC/novocaged/memoriaR/portuaria_ma")
# saveRDS(df_mov, 'ma_base_mov_port.Rds')
# saveRDS(df_for, 'ma_base_for_port.Rds')
# saveRDS(df_exc, 'ma_base_exc_port.Rds')

rm(cagedexc_baixadas,cagedexc_lista,cagedfor_baixadas,cagedfor_lista,cagedmov_baixadas,cagedmov_lista,arquivos_caged,competencias,k,meses,anos,cnaes)

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

# SALDO ----
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

ggplot(data = saldo_ajustado_port,
       mapping = aes(x = data, y = saldo_ajuste, group = 1)) +
  geom_line() +  # Modificado para gráfico de linha
  labs(x = "ano/mês", y = "saldo ajustado") +
  ggtitle('Saldo de Empregos no Porto do Itaqui - Maranhão') +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))


# setwd("~/TCC/novocaged/graph")
# ggsave('saldo_portuario_ma.png')

# setwd('C:/Users/NOVO/Documents/TCC/novocaged/salvo_excel')
# write_xlsx(saldo_ajustado_port, 'saldo_por_mes_portuario_ma.xlsx')

# BASE PARA ANÁLISE DE PERFIL ----
# juntando linhas de movimentação e fora do prazo
base_soma_port <- bind_rows(
  mutate(df_mov, competencia = as.character(competenciamov)),
  mutate(df_for, competencia = as.character(competenciamov))
)

nrow(df_mov)+nrow(df_for)-nrow(df_exc)

# exclusão 
base_filtrada <- base_soma_port |>
  # join without matches
  anti_join(df_exc, 
            by = c("competenciamov","regiao","uf","municipio",
                   "secao","subclasse","saldomovimentacao",
                   "cbo2002ocupacao","categoria",
                   "graudeinstrucao","idade","horascontratuais","racacor","sexo",
                   "tipoempregador","tipoestabelecimento","tipomovimentacao",
                   "tipodedeficiencia","indtrabintermitente","indtrabparcial","salario",
                   "tamestabjan","indicadoraprendiz","origemdainformacao","competenciadec",
                   "indicadordeforadoprazo","unidadesalariocodigo","valorsalariofixo")) |>
  # existe 1 a menos do que deveria ser excluído
  select("competenciamov","municipio","secao","subclasse",        
         "cbo2002ocupacao","graudeinstrucao","idade","racacor",          
         "sexo","tipodedeficiencia","salario","saldomovimentacao")

# setwd("~/TCC/novocaged/memoriaR/portuaria_ma")
# saveRDS(base_filtrada, "ma_base_trabalhadores_portuarios.Rds")

rm(base_filtrada,base_soma_port)
