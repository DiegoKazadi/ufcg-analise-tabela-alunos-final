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

#####

# Agrupar por sexo e calcular porcentagem
sexo_dist <- alunos_sem_duplicatas %>%
  group_by(sexo) %>%
  summarise(total = n()) %>%
  mutate(porcentagem = round((total / sum(total)) * 100, 1))

# Gráfico de barras com porcentagem
ggplot(sexo_dist, aes(x = reorder(sexo, -porcentagem), y = porcentagem, fill = sexo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(porcentagem, "%")), vjust = -0.3, size = 4.5) +
  labs(
    title = "Distribuição Percentual dos Alunos por Sexo",
    x = "Sexo",
    y = "Porcentagem (%)",
    fill = "Sexo"
  ) +
  theme_minimal()

####
library(dplyr)
library(ggplot2)

# Calcular quantidade e proporção por período e sexo
sexo_por_periodo <- alunos_sem_duplicatas %>%
  group_by(período_de_ingresso, sexo) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(período_de_ingresso) %>%
  mutate(porcentagem = total / sum(total) * 100)

# Gráfico de barras empilhadas normalizadas
ggplot(sexo_por_periodo, aes(x = período_de_ingresso, y = porcentagem, fill = sexo)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Distribuição Percentual de Alunos por Sexo em Cada Período de Ingresso",
    x = "Período de Ingresso",
    y = "Alunos Matriculados (%)",
    fill = "Sexo"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################################

# Calcular quantidade e proporção por período e tipo de cota
cota_por_periodo <- alunos_sem_duplicatas %>%
  group_by(período_de_ingresso, cota) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(período_de_ingresso) %>%
  mutate(porcentagem = total / sum(total) * 100)

# Gráfico de barras empilhadas normalizadas
ggplot(cota_por_periodo, aes(x = período_de_ingresso, y = porcentagem, fill = cota)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Distribuição Percentual dos Alunos por Tipo de Cota ao Longo dos Períodos",
    x = "Período de Ingresso",
    y = "Porcentagem (%)",
    fill = "Tipo de Cota"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###############################################################################

# Calcular quantidade e proporção por período e forma de ingresso
forma_ingresso_por_periodo <- alunos_sem_duplicatas %>%
  group_by(período_de_ingresso, forma_de_ingresso) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(período_de_ingresso) %>%
  mutate(porcentagem = total / sum(total) * 100)

# Gráfico de barras empilhadas normalizadas
ggplot(forma_ingresso_por_periodo, aes(x = período_de_ingresso, y = porcentagem, fill = forma_de_ingresso)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Distribuição Percentual dos Alunos por Forma de Ingresso",
    x = "Período de Ingresso",
    y = "Porcentagem (%)",
    fill = "Forma de Ingresso"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###############################################################################

# Agrupar por idade e calcular percentual
# Agrupar por cor e calcular percentual
cor_distribuicao <- alunos_sem_duplicatas %>%
  count(cor) %>%
  mutate(percentual = round(n / sum(n) * 100, 1))

# Visualizar com gráfico de barras
ggplot(cor_distribuicao, aes(x = reorder(cor, -percentual), y = percentual, fill = cor)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentual, "%")), vjust = -0.3, size = 3.5) +
  labs(
    title = "Figura 4.6 - Distribuição Percentual por Cor/Raça (2011.1 a 2023.2)",
    x = "Cor/Raça",
    y = "Percentual (%)",
    fill = "Cor/Raça"
  ) +
  theme_minimal()


###############################################################################
# Agrupar por estado civil e calcular percentual
estado_civil <- alunos_sem_duplicatas %>%
  count(estado_civil) %>%
  mutate(percentual = round(n / sum(n) * 100, 1))

# Visualização com gráfico de barras e percentuais
ggplot(estado_civil, aes(x = reorder(estado_civil, -percentual), y = percentual, fill = estado_civil)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentual, "%")), vjust = -0.3, size = 3.5) +
  labs(
    title = "Figura 4.6 - Distribuição Percentual do Estado Civil no Ingresso (2011.1 a 2023.2)",
    x = "Estado Civil",
    y = "Percentual (%)",
    fill = "Estado Civil"
  ) +
  theme_minimal()

###############################################################################

# Filtrar apenas alunos no intervalo de interesse
dados_ingressantes <- alunos_sem_duplicatas %>%
  filter(período_de_ingresso >= "2011.1" & período_de_ingresso <= "2023.2") %>%
  group_by(período_de_ingresso, currículo) %>%
  summarise(total = n(), .groups = "drop")

# Organizar os períodos para ordenação correta no eixo x
dados_ingressantes$período_de_ingresso <- factor(
  dados_ingressantes$período_de_ingresso,
  levels = sort(unique(dados_ingressantes$período_de_ingresso))
)

# Gráfico de linha com dois currículos
ggplot(dados_ingressantes, aes(x = período_de_ingresso, y = total, group = currículo, color = as.factor(currículo))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Figura 4.8 – Número de Ingressantes por Currículo (2011.1 a 2023.2)",
    x = "Período de Ingresso",
    y = "Número de Alunos Ingressantes",
    color = "Currículo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################################################
library(dplyr)
library(ggplot2)

# Calcular totais por situação e percentual
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
  mutate(percentual = round((total / sum(total)) * 100, 1)) %>%
  arrange(desc(percentual))

# Gráfico com porcentagens
ggplot(dados_situacao, aes(x = reorder(situacao, percentual), y = percentual, fill = situacao)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentual, "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Figura 4.9 – Situação Acadêmica dos Alunos (2011.1–2023.2)",
    x = "Situação Acadêmica",
    y = "Porcentagem (%)",
    fill = "Situação"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

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
