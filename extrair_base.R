setwd("C:/Users/julia.leite/Desktop/2020/202001")

# pasta origem: ftp://ftp.mtps.gov.br/pdet/microdados/

## carregar pacotes
library(tidyverse)
library(archive)
library(readr)

data <- readr::read_csv2(archive::archive_read('CAGEDMOV202001.7z'))
