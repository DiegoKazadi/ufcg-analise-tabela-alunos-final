# Carregar bibliotecas necessárias
library(readr)      # Para leitura de arquivos CSV
library(dplyr)      # Para manipulação de dados
library(stringr)    # Para operações com strings
library(ggplot2)    # Para visualizações futuras (opcional)

# Definir o caminho base onde estão os arquivos
caminho_base <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/tabelas"

# Nome do arquivo que será carregado
arquivo_alunos <- file.path(caminho_base, "alunos-final.csv")

# Leitura do arquivo CSV
# Utilizamos read_csv do pacote readr por ser mais rápido e robusto com grandes volumes de dados
alunos <- readr::read_delim(arquivo_alunos, delim = ",", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# Visualizar as primeiras linhas para garantir que foi carregado corretamente
head(alunos)

# Verificar estrutura do dataframe: tipos de variáveis, dimensões, etc.
glimpse(alunos)

# Exemplo de verificação rápida: contagem de linhas e colunas
cat("Total de linhas:", nrow(alunos), "\n")
cat("Total de colunas:", ncol(alunos), "\n")

# Verificar se há valores ausentes por coluna
colSums(is.na(alunos))

# Mostrar nomes das colunas para referência
names(alunos)

################################################################################

# Converte o período para formato numérico se ainda estiver como texto
alunos$`Período de Ingresso` <- as.character(alunos$`Período de Ingresso`)

# Filtra apenas os ingressos entre 2011.1 e 2023.2
alunos_filtrados <- alunos %>%
  filter(`Período de Ingresso` >= "2011.1" & `Período de Ingresso` <= "2023.2")

# Padronizar variaves
names(alunos) <- tolower(gsub(" ", "_", names(alunos)))

# Mostrar nomes das colunas para referência
names(alunos)









