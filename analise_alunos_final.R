# Carregar bibliotecas necessárias
library(readr)      # Para leitura de arquivos CSV
library(dplyr)      # Para manipulação de dados
library(stringr)    # Para operações com strings
library(ggplot2)    # Para visualizações futuras (opcional)
install.packages("janitor")
library(janitor)

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
alunos <- alunos %>% clean_names()

# Mostrar nomes das colunas para referência
names(alunos)

################################################################################

# Converte o período para formato numérico se ainda estiver como texto
alunos$`periodo_de_ingresso` <- as.character(alunos$`periodo_de_ingresso`)

# Filtra apenas os ingressos entre 2011.1 e 2023.2
alunos_filtrados <- alunos %>%
  filter(`periodo_de_ingresso` >= "2011.1" & `periodo_de_ingresso` <= "2023.2")

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
unique(alunos_sem_duplicatas$`tipo_de_evasao`)

names(alunos_sem_duplicatas)

# Visualizar valores únicos da coluna status
unique(alunos_sem_duplicatas$status)

# Visualizar valores únicos da coluna tipo_de_evasão
unique(alunos_sem_duplicatas$tipo_de_evasao)

################################################################################

# Total de alunos (sem duplicatas)
total_alunos <- nrow(alunos_sem_duplicatas)

# Filtrar alunos evadidos: INATIVO e tipo de evasão não é GRADUADO nem REGULAR
evadidos <- alunos_sem_duplicatas %>%
  filter(status == "INATIVO" & !tipo_de_evasao %in% c("GRADUADO", "REGULAR"))

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
  group_by(periodo_de_ingresso) %>%
  summarise(total_ingressantes = n()) %>%
  arrange(periodo_de_ingresso)

# Converter o período em fator ordenado para manter a ordem cronológica no gráfico
ingressantes_por_periodo$periodo_de_ingresso <- factor(
  ingressantes_por_periodo$periodo_de_ingresso,
  levels = unique(ingressantes_por_periodo$periodo_de_ingresso)
)

# Criar o gráfico de linhas
ggplot(ingressantes_por_periodo, aes(x = periodo_de_ingresso, y = total_ingressantes, group = 1)) +
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
  group_by(periodo_de_ingresso) %>%
  summarise(total_ingressantes = n()) %>%
  arrange(periodo_de_ingresso)

# Converter período em fator ordenado para manter a sequência correta no gráfico
ingressantes_por_periodo$periodo_de_ingresso <- factor(
  ingressantes_por_periodo$periodo_de_ingresso,
  levels = unique(ingressantes_por_periodo$periodo_de_ingresso)
)

# Gráfico de barras com números acima das barras
ggplot(ingressantes_por_periodo, aes(x = periodo_de_ingresso, y = total_ingressantes)) +
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
###############################################################################

# Calcular quantidade e proporção por período e sexo
sexo_por_periodo <- alunos_sem_duplicatas %>%
  group_by(periodo_de_ingresso, sexo) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(periodo_de_ingresso) %>%
  mutate(porcentagem = total / sum(total) * 100)

# Gráfico de barras empilhadas normalizadas
ggplot(sexo_por_periodo, aes(x = periodo_de_ingresso, y = porcentagem, fill = sexo)) +
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
  group_by(periodo_de_ingresso, cota) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(periodo_de_ingresso) %>%
  mutate(porcentagem = total / sum(total) * 100)

# Gráfico de barras empilhadas normalizadas
ggplot(cota_por_periodo, aes(x = periodo_de_ingresso, y = porcentagem, fill = cota)) +
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
  group_by(periodo_de_ingresso, forma_de_ingresso) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(periodo_de_ingresso) %>%
  mutate(porcentagem = total / sum(total) * 100)

# Gráfico de barras empilhadas normalizadas
ggplot(forma_ingresso_por_periodo, aes(x = periodo_de_ingresso, y = porcentagem, fill = forma_de_ingresso)) +
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
##############################################################################
# Criar variável binária de evasão (1 = evadiu, 0 = não evadiu)
alunos_sem_duplicatas <- alunos_sem_duplicatas %>%
  mutate(evadiu = ifelse(status == "INATIVO" & tipo_de_evasao != "GRADUADO", 1, 0))

# Boxplot de evasão por estado civil
ggplot(alunos_sem_duplicatas, aes(x = estado_civil, y = evadiu, fill = estado_civil)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Figura X - Distribuição da Evasão por Estado Civil (2011.1 a 2023.2)",
    x = "Estado Civil",
    y = "Evasão (0 = Não, 1 = Sim)",
    fill = "Estado Civil"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 1), labels = c("Não Evadiu", "Evadiu"))
###############################################################################

# Criar variável binária de evasão (caso ainda não exista)
alunos_sem_duplicatas <- alunos_sem_duplicatas %>%
  mutate(evadiu = ifelse(status == "INATIVO" & tipo_de_evasao != "GRADUADO", 1, 0))

# Boxplot de evasão por sexo
ggplot(alunos_sem_duplicatas, aes(x = sexo, y = evadiu, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Figura X - Distribuição da Evasão por Sexo (2011.1 a 2023.2)",
    x = "Sexo",
    y = "Evasão (0 = Não, 1 = Sim)",
    fill = "Sexo"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 1), labels = c("Não Evadiu", "Evadiu"))

###############################################################################

# Filtrar apenas alunos no intervalo de interesse
dados_ingressantes <- alunos_sem_duplicatas %>%
  filter(periodo_de_ingresso >= "2011.1" & periodo_de_ingresso <= "2023.2") %>%
  group_by(periodo_de_ingresso, curriculo) %>%
  summarise(total = n(), .groups = "drop")

# Organizar os períodos para ordenação correta no eixo x
dados_ingressantes$periodo_de_ingresso <- factor(
  dados_ingressantes$periodo_de_ingresso,
  levels = sort(unique(dados_ingressantes$periodo_de_ingresso))
)

# Gráfico de linha com dois currículos
ggplot(dados_ingressantes, aes(x = periodo_de_ingresso, y = total, group = curriculo, color = as.factor(curriculo))) +
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
# Calcular totais por situação e percentual
dados_situacao <- alunos_sem_duplicatas %>%
  mutate(situacao = case_when(
    status == "ATIVO" ~ "Ativo",
    tipo_de_evasao == "GRADUADO" ~ "Graduado",
    tipo_de_evasao == "CANCELAMENTO POR ABANDONO" ~ "Cancelamento por Abandono",
    tipo_de_evasao == "CANCELAMENTO P SOLICITACAO ALUNO" ~ "Cancelamento por Solicitação do Aluno",
    tipo_de_evasao == "CANCELADO 3 REPROV MESMA DISCIPLINA" ~ "Cancelamento por 3 Reprovações",
    tipo_de_evasao == "CANCELADO REPROVOU TODAS POR FALTAS" ~ "Cancelamento por Faltas",
    tipo_de_evasao == "CANCELADO NOVO INGRESSO OUTRO CURSO" ~ "Cancelamento por Novo Ingresso",
    tipo_de_evasao == "CANCELAMENTO DE MATRICULA" ~ "Cancelamento de Matrícula",
    tipo_de_evasao == "CANCELAMENTO P MUDANCA CURSO" ~ "Cancelamento por Mudança de Curso",
    tipo_de_evasao == "TRANSFERIDO PARA OUTRA IES" ~ "Transferido para Outra IES",
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
# Filtrar apenas evasões reais (excluindo graduados)
evasoes_reais <- alunos_sem_duplicatas %>%
  filter(status == "INATIVO", tipo_de_evasao != "GRADUADO", !is.na(periodo_de_evasao))

# Evasões que ocorreram no mesmo período de ingresso
evasao_primeiro_periodo <- evasoes_reais %>%
  filter(periodo_de_evasao == periodo_de_ingresso)

# Contar evasões por período de ingresso (ou seja, evasão no 1º período)
distrib_evasao_primeiro <- evasao_primeiro_periodo %>%
  count(periodo_de_ingresso) %>%
  rename(quantidade_evasoes = n)

# Gráfico
ggplot(distrib_evasao_primeiro, aes(x = reorder(periodo_de_ingresso, periodo_de_ingresso), y = quantidade_evasoes)) +
  geom_bar(stat = "identity", fill = "#E7298A") +
  geom_text(aes(label = quantidade_evasoes), vjust = -0.5, size = 3) +
  labs(
    title = "Evasão no Primeiro Período por Período de Ingresso",
    x = "Período de Ingresso",
    y = "Quantidade de Evasões no 1º Período"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################################################################
# Calcular total geral
total_geral <- nrow(alunos_sem_duplicatas)

# Agrupar e calcular porcentagem
dados_situacao <- alunos_sem_duplicatas %>%
  mutate(situacao = case_when(
    status == "ATIVO" ~ "Ativo",
    tipo_de_evasao == "GRADUADO" ~ "Graduado",
    tipo_de_evasao == "CANCELAMENTO POR ABANDONO" ~ "Cancelamento por Abandono",
    tipo_de_evasao == "CANCELAMENTO P SOLICITACAO ALUNO" ~ "Cancelamento por Solicitação do Aluno",
    tipo_de_evasao == "CANCELADO 3 REPROV MESMA DISCIPLINA" ~ "Cancelamento por 3 Reprovações",
    tipo_de_evasao == "CANCELADO REPROVOU TODAS POR FALTAS" ~ "Cancelamento por Faltas",
    tipo_de_evasao == "CANCELADO NOVO INGRESSO OUTRO CURSO" ~ "Cancelamento por Novo Ingresso",
    tipo_de_evasao == "CANCELAMENTO DE MATRICULA" ~ "Cancelamento de Matrícula",
    tipo_de_evasao == "CANCELAMENTO P MUDANCA CURSO" ~ "Cancelamento por Mudança de Curso",
    tipo_de_evasao == "TRANSFERIDO PARA OUTRA IES" ~ "Transferido para Outra IES",
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
install.packages("tidyr")

# Classificar currículo com base no período de ingresso
alunos_sem_duplicatas <- alunos_sem_duplicatas %>%
  mutate(
    curriculo = case_when(
      periodo_de_ingresso < "2018.1" ~ "Currículo 1999",
      TRUE ~ "Currículo 2017"
    ),
    evadiu = ifelse(status == "INATIVO" & tipo_de_evasao != "GRADUADO", 1, 0)
  )

# Agrupar e calcular taxa de evasão por período, sexo e currículo
evasao_por_sexo <- alunos_sem_duplicatas %>%
  group_by(curriculo, periodo_de_ingresso, sexo) %>%
  summarise(
    total_ingressantes = n(),
    total_evasoes = sum(evadiu),
    taxa_evasao = round((total_evasoes / total_ingressantes) * 100, 2),
    .groups = "drop"
  )

# Calcular média e desvio padrão da taxa de evasão por currículo e sexo
resumo_evasao <- evasao_por_sexo %>%
  group_by(curriculo, sexo) %>%
  summarise(
    media_taxa_evasao = round(mean(taxa_evasao, na.rm = TRUE), 2),
    desvio_padrao = round(sd(taxa_evasao, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Visualizar tabela completa
tabela_resultado <- evasao_por_sexo %>%
  left_join(resumo_evasao, by = c("curriculo", "sexo")) %>%
  arrange(curriculo, sexo, periodo_de_ingresso)

# Visualizar
print(tabela_resultado)
table(alunos_sem_duplicatas$sexo, useNA = "ifany")

###############################################################################
names(alunos_sem_duplicatas)

# Padroniza sexo e tipo de evasão
# Primeiro, padroniza melhor os dados
alunos_limpo <- alunos_sem_duplicatas %>%
  mutate(
    sexo = toupper(sexo),
    status = toupper(status),
    periodo_de_evasao = toupper(periodo_de_evasao),
    curriculo = as.character(curriculo)
  )

# Define quem é evadido de forma clara (INATIVO e não graduado)
alunos_limpo <- alunos_limpo %>%
  mutate(
    evadido = if_else(status == "INATIVO" & !(periodo_de_evasao %in% c("GRADUADO", "GRADUAÇÃO", "GRADUACAO")), TRUE, FALSE)
  )

# Currículo 1999
evasao_1999 <- alunos_limpo %>%
  filter(curriculo == "1999") %>%
  group_by(sexo) %>%
  summarise(
    total = n(),
    evadidos = sum(evadido, na.rm = TRUE),
    taxa_evasao = round((evadidos / total) * 100, 2),
    .groups = "drop"
  )

# Currículo 2017
evasao_2017 <- alunos_limpo %>%
  filter(curriculo == "2017") %>%
  group_by(sexo) %>%
  summarise(
    total = n(),
    evadidos = sum(evadido, na.rm = TRUE),
    taxa_evasao = round((evadidos / total) * 100, 2),
    .groups = "drop"
  )

cat("📘 Taxa de evasão por sexo – Currículo 1999 (excluindo graduados):\n")
print(evasao_1999)

cat("\n📗 Taxa de evasão por sexo – Currículo 2017 (excluindo graduados):\n")
print(evasao_2017)
###############################################################################
names(alunos_sem_duplicatas)
# Pacotes necessários
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# Função para calcular próximo período
proximo_periodo <- function(periodo) {
  partes <- str_split_fixed(periodo, "\\.", 2)
  ano <- as.integer(partes[, 1])
  semestre <- as.integer(partes[, 2])
  
  novo_ano <- ifelse(semestre == 2, ano + 1, ano)
  novo_semestre <- ifelse(semestre == 1, 2, 1)
  
  paste0(novo_ano, ".", novo_semestre)
}


# Função para calcular evasão em múltiplos períodos
calcular_evasao_multiplos_periodos <- function(df, inicio = "2011.1", fim = "2017.2") {
  df <- df %>%
    filter(periodo_de_ingresso >= inicio & periodo_de_ingresso <= fim) %>%
    mutate(
      p1 = proximo_periodo(periodo_de_ingresso),
      p2 = proximo_periodo(p1),
      p3 = proximo_periodo(p2),
      p4 = proximo_periodo(p3),
      evadiu_p1 = ifelse(periodo_de_evasao == p1, 1, 0),
      evadiu_p2 = ifelse(periodo_de_evasao == p2, 1, 0),
      evadiu_p3 = ifelse(periodo_de_evasao == p3, 1, 0),
      evadiu_p4 = ifelse(periodo_de_evasao == p4, 1, 0)
    )
  return(df)
}

# Estatísticas por variável e período
estatisticas_por_variavel <- function(df, variavel, periodo) {
  col_evasao <- paste0("evadiu_p", periodo)
  df_periodo <- df %>%
    group_by(.data[[variavel]]) %>%
    summarise(
      total = n(),
      evasoes = sum(.data[[col_evasao]], na.rm = TRUE)
    ) %>%
    mutate(taxa_evasao = evasoes / total)
  
  media <- mean(df_periodo$taxa_evasao, na.rm = TRUE)
  desvio <- sd(df_periodo$taxa_evasao, na.rm = TRUE)
  list(resultado = df_periodo, media = media, desvio = desvio)
}

# Gráfico da evasão por variável
plotar_grafico <- function(df_resultado, media, desvio, var, periodo) {
  df_resultado <- df_resultado %>%
    arrange(desc(taxa_evasao))
  
  ggplot(df_resultado, aes_string(x = var, y = "taxa_evasao", fill = var)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = media, color = "red", linetype = "dashed") +
    annotate("text", x = 1, y = media, label = sprintf("Média: %.1f%%", media * 100), vjust = -1, color = "red") +
    geom_rect(aes(ymin = media - desvio, ymax = media + desvio),
              xmin = -Inf, xmax = Inf, fill = "red", alpha = 0.1, inherit.aes = FALSE) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = paste("Taxa de Evasão por", str_to_title(var), "-", periodo, "º Período"),
      x = str_to_title(var),
      y = "Taxa de Evasão"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Lista de variáveis a serem analisadas
variaveis <- c("sexo", "cor", "estado_civil", "forma_de_ingresso", "cota")

# Aplicação do script
df_evasao <- calcular_evasao_multiplos_periodos(alunos_sem_duplicatas)

for (periodo in 1:4) {
  cat(paste0("\n====== Estatísticas de Evasão - ", periodo, "º Período ======\n"))
  for (var in variaveis) {
    stats <- estatisticas_por_variavel(df_evasao, var, periodo)
    df_resultado <- stats$resultado
    media <- stats$media
    desvio <- stats$desvio
    
    cat(paste0("\n[", toupper(var), "] Média: ", round(media, 4), ", Desvio Padrão: ", round(desvio, 4), "\n"))
    print(df_resultado)
    print(plotar_grafico(df_resultado, media, desvio, var, periodo))
  }
}


##############################################################################
# SALVAMENTO DOS GRÁFICOS
# Pacotes necessários
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# Função para calcular próximo período
proximo_periodo <- function(periodo) {
  partes <- str_split_fixed(periodo, "\\.", 2)
  ano <- as.integer(partes[, 1])
  semestre <- as.integer(partes[, 2])
  
  novo_ano <- ifelse(semestre == 2, ano + 1, ano)
  novo_semestre <- ifelse(semestre == 1, 2, 1)
  
  paste0(novo_ano, ".", novo_semestre)
}

# Função para calcular evasão em múltiplos períodos
calcular_evasao_multiplos_periodos <- function(df) {
  df %>%
    mutate(
      curriculo = as.factor(curriculo),
      p1 = proximo_periodo(periodo_de_ingresso),
      p2 = proximo_periodo(p1),
      p3 = proximo_periodo(p2),
      p4 = proximo_periodo(p3),
      evadiu_p1 = ifelse(periodo_de_evasao == p1, 1, 0),
      evadiu_p2 = ifelse(periodo_de_evasao == p2, 1, 0),
      evadiu_p3 = ifelse(periodo_de_evasao == p3, 1, 0),
      evadiu_p4 = ifelse(periodo_de_evasao == p4, 1, 0)
    )
}

# Estatísticas por variável e currículo
estatisticas_por_variavel <- function(df, variavel, periodo) {
  col_evasao <- paste0("evadiu_p", periodo)
  
  df_periodo <- df %>%
    group_by(.data[[variavel]], curriculo) %>%
    summarise(
      total = n(),
      evasoes = sum(.data[[col_evasao]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(taxa_evasao = evasoes / total)
  
  media <- mean(df_periodo$taxa_evasao, na.rm = TRUE)
  desvio <- sd(df_periodo$taxa_evasao, na.rm = TRUE)
  list(resultado = df_periodo, media = media, desvio = desvio)
}

# Gráfico com salvamento
plotar_grafico <- function(df_resultado, media, desvio, var, periodo) {
  df_resultado <- df_resultado %>%
    arrange(desc(taxa_evasao))
  
  p <- ggplot(df_resultado, aes(x = .data[[var]], y = taxa_evasao, fill = curriculo)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    geom_hline(yintercept = media, color = "red", linetype = "dashed") +
    annotate("text", x = 1, y = media, label = sprintf("Média Geral: %.1f%%", media * 100), vjust = -1, color = "red") +
    annotate("rect", ymin = media - desvio, ymax = media + desvio,
             xmin = -Inf, xmax = Inf, fill = "red", alpha = 0.1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_brewer(palette = "Set2", name = "Currículo", guide = guide_legend()) +
    labs(
      title = paste("Taxa de Evasão por", str_to_title(var), "-", periodo, "º Período"),
      x = str_to_title(var),
      y = "Taxa de Evasão (%)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white")
    )
  
  # Salvar como JPEG
  file_name <- paste0("evasao_", periodo, "p_", var, ".jpeg")
  ggsave(file_name, plot = p, width = 9, height = 5.5, dpi = 320, device = "jpeg", bg = "white")
  
  return(p)
}

# Variáveis categóricas a analisar
variaveis <- c("sexo", "cor", "estado_civil", "forma_de_ingresso", "cota")

# Base tratada
df_evasao <- calcular_evasao_multiplos_periodos(alunos_sem_duplicatas)

# Loop principal
for (periodo in 1:4) {
  cat(paste0("\n====== Estatísticas de Evasão - ", periodo, "º Período ======\n"))
  for (var in variaveis) {
    stats <- estatisticas_por_variavel(df_evasao, var, periodo)
    df_resultado <- stats$resultado
    media <- stats$media
    desvio <- stats$desvio
    
    cat(paste0("\n[", toupper(var), "] Média: ", round(media, 4), ", Desvio Padrão: ", round(desvio, 4), "\n"))
    print(df_resultado)
    
    plotar_grafico(df_resultado, media, desvio, var, periodo)
  }
}


###############################################################################

#  (sem currículo 2023)
# Pacotes necessários
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# Função para calcular próximo período
proximo_periodo <- function(periodo) {
  partes <- str_split_fixed(periodo, "\\.", 2)
  ano <- as.integer(partes[, 1])
  semestre <- as.integer(partes[, 2])
  
  novo_ano <- ifelse(semestre == 2, ano + 1, ano)
  novo_semestre <- ifelse(semestre == 1, 2, 1)
  
  paste0(novo_ano, ".", novo_semestre)
}

# Função para calcular evasão em múltiplos períodos (apenas currículos 1999 e 2017)
calcular_evasao_multiplos_periodos <- function(df) {
  df %>%
    filter(curriculo %in% c(1999, 2017)) %>%  # <- filtro aplicado aqui
    mutate(
      curriculo = as.factor(curriculo),
      p1 = proximo_periodo(periodo_de_ingresso),
      p2 = proximo_periodo(p1),
      p3 = proximo_periodo(p2),
      p4 = proximo_periodo(p3),
      evadiu_p1 = ifelse(periodo_de_evasao == p1, 1, 0),
      evadiu_p2 = ifelse(periodo_de_evasao == p2, 1, 0),
      evadiu_p3 = ifelse(periodo_de_evasao == p3, 1, 0),
      evadiu_p4 = ifelse(periodo_de_evasao == p4, 1, 0)
    )
}

# Estatísticas por variável e currículo
estatisticas_por_variavel <- function(df, variavel, periodo) {
  col_evasao <- paste0("evadiu_p", periodo)
  
  df_periodo <- df %>%
    group_by(.data[[variavel]], curriculo) %>%
    summarise(
      total = n(),
      evasoes = sum(.data[[col_evasao]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(taxa_evasao = evasoes / total)
  
  media <- mean(df_periodo$taxa_evasao, na.rm = TRUE)
  desvio <- sd(df_periodo$taxa_evasao, na.rm = TRUE)
  list(resultado = df_periodo, media = media, desvio = desvio)
}

# Gráfico com salvamento
plotar_grafico <- function(df_resultado, media, desvio, var, periodo) {
  df_resultado <- df_resultado %>%
    arrange(desc(taxa_evasao))
  
  p <- ggplot(df_resultado, aes(x = .data[[var]], y = taxa_evasao, fill = curriculo)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    geom_hline(yintercept = media, color = "red", linetype = "dashed") +
    annotate("text", x = 1, y = media, label = sprintf("Média Geral: %.1f%%", media * 100), vjust = -1, color = "red") +
    annotate("rect", ymin = media - desvio, ymax = media + desvio,
             xmin = -Inf, xmax = Inf, fill = "red", alpha = 0.1) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_brewer(palette = "Set2", name = "Currículo", guide = guide_legend()) +
    labs(
      title = paste("Taxa de Evasão por", str_to_title(var), "-", periodo, "º Período"),
      x = str_to_title(var),
      y = "Taxa de Evasão (%)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white")
    )
  
  # Salvar como JPEG
  file_name <- paste0("evasao_", periodo, "p_", var, ".jpeg")
  ggsave(file_name, plot = p, width = 9, height = 5.5, dpi = 320, device = "jpeg", bg = "white")
  
  return(p)
}

# Variáveis categóricas a analisar
variaveis <- c("sexo", "cor", "estado_civil", "forma_de_ingresso", "cota")

# Base tratada (com filtro de currículo)
df_evasao <- calcular_evasao_multiplos_periodos(alunos_sem_duplicatas)

# Loop principal
for (periodo in 1:4) {
  cat(paste0("\n====== Estatísticas de Evasão - ", periodo, "º Período ======\n"))
  for (var in variaveis) {
    stats <- estatisticas_por_variavel(df_evasao, var, periodo)
    df_resultado <- stats$resultado
    media <- stats$media
    desvio <- stats$desvio
    
    cat(paste0("\n[", toupper(var), "] Média: ", round(media, 4), ", Desvio Padrão: ", round(desvio, 4), "\n"))
    print(df_resultado)
    
    plotar_grafico(df_resultado, media, desvio, var, periodo)
  }
}

