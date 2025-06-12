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

################################################################################

# Padroniza nomes das colunas para minúsculas
alunos <- alunos %>% 
  rename_with(tolower)

# Remove duplicatas com base na coluna 'cpf', mantendo o primeiro registro
alunos_sem_duplicatas <- alunos %>%
  distinct(cpf, .keep_all = TRUE)

# Verifica quantos registros foram removidos
n_antes <- nrow(alunos)
n_depois <- nrow(alunos_sem_duplicatas)
cat("Registros antes:", n_antes, "\nRegistros após remoção de duplicatas:", n_depois, "\nDuplicatas removidas:", n_antes - n_depois, "\n")



###############################################################################

# Ver os valores únicos da coluna status
unique(alunos_sem_duplicatas$status)

# Ver os valores únicos da coluna tipo de evasao
unique(alunos_sem_duplicatas$`tipo de evasao`)

names(alunos_sem_duplicatas)

# Visualizar valores únicos da coluna status
unique(alunos_sem_duplicatas$status)

# Visualizar valores únicos da coluna tipo_de_evasão
unique(alunos_sem_duplicatas$tipo_de_evasão)

################################################################################

# Total de alunos (sem duplicatas)
total_alunos <- nrow(alunos_sem_duplicatas)

# Filtrar alunos evadidos: INATIVO e tipo de evasão não é GRADUADO nem REGULAR
evadidos <- alunos_sem_duplicatas %>%
  filter(status == "INATIVO" & !tipo_de_evasão %in% c("GRADUADO", "REGULAR"))

# Contar número de evadidos
total_evadidos <- nrow(evadidos)

# Calcular porcentagem de evasão
taxa_evasao <- total_evadidos / total_alunos * 100

# Exibir os resultados
cat("Total de alunos analisados:", total_alunos, "\n")
cat("Total de evadidos:", total_evadidos, "\n")
cat("Taxa de evasão (%):", round(taxa_evasao, 2), "\n")

###############################################################################
# Gráfico de linhas dos ingressantes por período:
# Agrupar e contar ingressantes por período
ingressantes_por_periodo <- alunos_sem_duplicatas %>%
  group_by(período_de_ingresso) %>%
  summarise(total_ingressantes = n()) %>%
  arrange(período_de_ingresso)

# Converter o período em fator ordenado para manter a ordem cronológica no gráfico
ingressantes_por_periodo$período_de_ingresso <- factor(
  ingressantes_por_periodo$período_de_ingresso,
  levels = unique(ingressantes_por_periodo$período_de_ingresso)
)

# Criar o gráfico de linhas
ggplot(ingressantes_por_periodo, aes(x = período_de_ingresso, y = total_ingressantes, group = 1)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_point(color = "#D55E00", size = 2) +
  labs(
    title = "Número de Ingressantes por Período (2011.1 a 2023.2)",
    x = "Período de Ingresso", y = "Total de Ingressantes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################
# Agrupar e contar ingressantes por período
ingressantes_por_periodo <- alunos_sem_duplicatas %>%
  group_by(período_de_ingresso) %>%
  summarise(total_ingressantes = n()) %>%
  arrange(período_de_ingresso)

# Converter período em fator ordenado para manter a sequência correta no gráfico
ingressantes_por_periodo$período_de_ingresso <- factor(
  ingressantes_por_periodo$período_de_ingresso,
  levels = unique(ingressantes_por_periodo$período_de_ingresso)
)

# Gráfico de barras com números acima das barras
ggplot(ingressantes_por_periodo, aes(x = período_de_ingresso, y = total_ingressantes)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  geom_text(aes(label = total_ingressantes), vjust = -0.5, color = "black", size = 3.5) +
  labs(
    title = "Número de Ingressantes por Período (2011.1 a 2023.2)",
    x = "Período de Ingresso",
    y = "Total de Ingressantes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, max(ingressantes_por_periodo$total_ingressantes) * 1.1)  # Espaço extra para o texto

###############################################################################

# Agrupar por sexo e contar alunos
sexo_contagem <- alunos_sem_duplicatas %>%
  group_by(sexo) %>%
  summarise(total = n())

# Gráfico de barras com números em cima
ggplot(sexo_contagem, aes(x = sexo, y = total, fill = sexo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total), vjust = -0.5) +
  labs(title = "Distribuição dos Alunos por Sexo (2011.1 a 2023.2)",
       x = "Sexo",
       y = "Número de Alunos") +
  theme_minimal() +
  theme(legend.position = "none")

###############################################################################
# Agrupar por cor e contar alunos
cor_contagem <- alunos_sem_duplicatas %>%
  group_by(cor) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

# Gráfico de barras ordenado decrescentemente
ggplot(cor_contagem, aes(x = reorder(cor, -total), y = total, fill = cor)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total), vjust = -0.5) +
  labs(title = "Distribuição dos Alunos por Cor/Raça (2011.1 a 2023.2)",
       x = "Cor/Raça",
       y = "Número de Alunos",
       fill = "Cor/Raça") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###############################################################################
# Agrupar por idade e contar alunos
idade_contagem <- alunos_sem_duplicatas %>%
  group_by(idade_aproximada_no_ingresso) %>%
  summarise(total = n()) %>%
  arrange(idade_aproximada_no_ingresso)

# Gráfico de barras com idade em ordem crescente
ggplot(idade_contagem, aes(x = factor(idade_aproximada_no_ingresso, levels = idade_contagem$idade_aproximada_no_ingresso), y = total)) +
  geom_bar(stat = "identity", fill = "#56B4E9") +
  geom_text(aes(label = total), vjust = -0.5) +
  labs(title = "Distribuição da Idade Aproximada no Ingresso",
       x = "Idade Aproximada",
       y = "Número de Alunos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##############################################################################
library(dplyr)
library(ggplot2)

# Verificar os valores únicos na coluna status
unique(alunos_sem_duplicatas$status)

# Contar alunos por status
status_count <- alunos_sem_duplicatas %>%
  group_by(status) %>%
  summarise(total_alunos = n()) %>%
  arrange(desc(total_alunos))

# Gráfico de barras da situação acadêmica
ggplot(status_count, aes(x = reorder(status, -total_alunos), y = total_alunos, fill = status)) +
  geom_col() +
  geom_text(aes(label = total_alunos), vjust = -0.5, size = 5) +
  labs(title = "Distribuição da Situação Acadêmica dos Alunos (2011–2023)",
       x = "Situação Acadêmica", y = "Número de Alunos") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))
###############################################################################
# Filtrar alunos inativos
inativos <- alunos_sem_duplicatas %>% filter(status == "INATIVO")

# Valores únicos em tipo_de_evasão para inativos
unique(inativos$tipo_de_evasão)

library(dplyr)
library(ggplot2)

# Agrupar os dados
dados_situacao <- alunos_sem_duplicatas %>%
  mutate(situacao = case_when(
    status == "ATIVO" ~ "Ativo",
    tipo_de_evasão == "GRADUADO" ~ "Graduado",
    tipo_de_evasão == "CANCELAMENTO POR ABANDONO" ~ "Cancelamento por Abandono",
    tipo_de_evasão == "CANCELAMENTO P SOLICITACAO ALUNO" ~ "Cancelamento por Solicitação do Aluno",
    tipo_de_evasão == "CANCELADO 3 REPROV MESMA DISCIPLINA" ~ "Cancelamento por 3 Reprovações",
    tipo_de_evasão == "CANCELADO REPROVOU TODAS POR FALTAS" ~ "Cancelamento por Faltas",
    tipo_de_evasão == "CANCELADO NOVO INGRESSO OUTRO CURSO" ~ "Cancelamento por Novo Ingresso",
    tipo_de_evasão == "CANCELAMENTO DE MATRICULA" ~ "Cancelamento de Matrícula",
    tipo_de_evasão == "CANCELAMENTO P MUDANCA CURSO" ~ "Cancelamento por Mudança de Curso",
    tipo_de_evasão == "TRANSFERIDO PARA OUTRA IES" ~ "Transferido para Outra IES",
    TRUE ~ "Outros Inativos"
  )) %>%
  group_by(situacao) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

# Gráfico de barras
ggplot(dados_situacao, aes(x = reorder(situacao, total), y = total, fill = situacao)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Situação Acadêmica dos Alunos",
    x = "Situação",
    y = "Número de Alunos",
    fill = "Situação"
  ) +
  theme_minimal()


################################################################################
library(dplyr)
library(ggplot2)

# Agrupamento e classificação das situações
dados_situacao <- alunos_sem_duplicatas %>%
  mutate(situacao = case_when(
    status == "ATIVO" ~ "Ativo",
    tipo_de_evasão == "GRADUADO" ~ "Graduado",
    tipo_de_evasão == "CANCELAMENTO POR ABANDONO" ~ "Cancelamento por Abandono",
    tipo_de_evasão == "CANCELAMENTO P SOLICITACAO ALUNO" ~ "Cancelamento por Solicitação do Aluno",
    tipo_de_evasão == "CANCELADO 3 REPROV MESMA DISCIPLINA" ~ "Cancelamento por 3 Reprovações",
    tipo_de_evasão == "CANCELADO REPROVOU TODAS POR FALTAS" ~ "Cancelamento por Faltas",
    tipo_de_evasão == "CANCELADO NOVO INGRESSO OUTRO CURSO" ~ "Cancelamento por Novo Ingresso",
    tipo_de_evasão == "CANCELAMENTO DE MATRICULA" ~ "Cancelamento de Matrícula",
    tipo_de_evasão == "CANCELAMENTO P MUDANCA CURSO" ~ "Cancelamento por Mudança de Curso",
    tipo_de_evasão == "TRANSFERIDO PARA OUTRA IES" ~ "Transferido para Outra IES",
    TRUE ~ "Outros Inativos"
  )) %>%
  group_by(situacao) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

# Gráfico com rótulos numéricos acima das barras
ggplot(dados_situacao, aes(x = reorder(situacao, total), y = total, fill = situacao)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Situação Acadêmica dos Alunos",
    x = "Situação",
    y = "Número de Alunos",
    fill = "Situação"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legenda redundante, pois já está no eixo
#################################################################################

library(dplyr)
library(ggplot2)

# Calcular total geral
total_geral <- nrow(alunos_sem_duplicatas)

# Agrupar e calcular porcentagem
dados_situacao <- alunos_sem_duplicatas %>%
  mutate(situacao = case_when(
    status == "ATIVO" ~ "Ativo",
    tipo_de_evasão == "GRADUADO" ~ "Graduado",
    tipo_de_evasão == "CANCELAMENTO POR ABANDONO" ~ "Cancelamento por Abandono",
    tipo_de_evasão == "CANCELAMENTO P SOLICITACAO ALUNO" ~ "Cancelamento por Solicitação do Aluno",
    tipo_de_evasão == "CANCELADO 3 REPROV MESMA DISCIPLINA" ~ "Cancelamento por 3 Reprovações",
    tipo_de_evasão == "CANCELADO REPROVOU TODAS POR FALTAS" ~ "Cancelamento por Faltas",
    tipo_de_evasão == "CANCELADO NOVO INGRESSO OUTRO CURSO" ~ "Cancelamento por Novo Ingresso",
    tipo_de_evasão == "CANCELAMENTO DE MATRICULA" ~ "Cancelamento de Matrícula",
    tipo_de_evasão == "CANCELAMENTO P MUDANCA CURSO" ~ "Cancelamento por Mudança de Curso",
    tipo_de_evasão == "TRANSFERIDO PARA OUTRA IES" ~ "Transferido para Outra IES",
    TRUE ~ "Outros Inativos"
  )) %>%
  group_by(situacao) %>%
  summarise(total = n()) %>%
  mutate(porcentagem = round((total / total_geral) * 100, 1)) %>%
  arrange(desc(porcentagem))

# Gráfico com porcentagens
ggplot(dados_situacao, aes(x = reorder(situacao, porcentagem), y = porcentagem, fill = situacao)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(porcentagem, "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Distribuição Percentual das Situações Acadêmicas dos Alunos",
    x = "Situação",
    y = "Porcentagem (%)",
    fill = "Situação"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

###############################################################################
# Calcular a mediana das porcentagens
mediana <- median(dados_situacao$porcentagem)

# Dot plot com linha de mediana
ggplot(dados_situacao, aes(x = porcentagem, y = reorder(situacao, porcentagem))) +
  geom_point(size = 4, color = "steelblue") +
  geom_vline(xintercept = mediana, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "Dispersão Percentual das Situações Acadêmicas",
    x = "Porcentagem (%)",
    y = "Situação"
  ) +
  annotate("text", x = mediana + 1, y = 1, label = paste("Mediana:", mediana, "%"), color = "red", hjust = 0) +
  theme_minimal()
