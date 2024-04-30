library(dplyr)
library(psych)

base_mov <- readRDS('ma_base_mov_port.Rds')
base_for <- readRDS('ma_base_for_port.Rds')
# base de excluídos para atv portuária no MA é vazia

base <- bind_rows(base_mov,base_for)

View(describe(base))
unique(base$municipio)
# result: 211130 (slz) 210173 (belagua)

# criando ano e mes
base$ano <- substr(base$competenciamov,1,4)
base$mes <- substr(base$competenciamov,5,6)

# valores únicos
for (coluna in colnames(base)) {
  cat(coluna, ":", unique(base[[coluna]]), "\n","\n")
}

# limpeza cols
base <- base |>
  subset(select = -c(competenciamov,regiao,origemdainformacao,tipodedeficiencia,tipoempregador,uf)) |>
  select(ano,mes,everything())
