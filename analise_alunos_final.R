# Carregar bibliotecas necessárias
library(readr)      # Para leitura de arquivos CSV
library(dplyr)      # Para manipulação de dados
library(stringr)    # Para operações com strings
library(ggplot2)    # Para visualizações futuras (opcional)
install.packages("janitor")
library(janitor)
library(scales)
library(viridis)  # Paleta moderna e acessível
library(ggthemes) # Tema mais elegante

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
# curva de ingressantes

# Agrupar dados por período e currículo
df_ingressantes <- alunos_sem_duplicatas %>%
  filter(curriculo %in% c(1999, 2017)) %>%
  group_by(periodo_de_ingresso, curriculo) %>%
  summarise(total_ingressantes = n(), .groups = "drop") %>%
  mutate(curriculo = as.factor(curriculo))

# Ordenar períodos para o eixo x
df_ingressantes <- df_ingressantes %>%
  arrange(periodo_de_ingresso) %>%
  mutate(periodo_de_ingresso = factor(periodo_de_ingresso, levels = unique(periodo_de_ingresso)))

# Plotar o gráfico
ggplot(df_ingressantes, aes(x = periodo_de_ingresso, y = total_ingressantes, color = curriculo, group = curriculo)) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  scale_y_continuous(name = "Número de Ingressantes", breaks = pretty_breaks()) +
  scale_color_brewer(palette = "Dark2", name = "Currículo") +
  labs(
    title = "Evolução dos Ingressantes por Currículo",
    x = "Período de Ingresso"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.background = element_rect(fill = "white", color = NA)
  )

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
# VISUALIZAÇÃO DISTRIBUIÇÃO IDADE
library(ggplot2)
library(dplyr)

# Filtrar apenas ingressantes válidos com idade e currículo definidos
df_idade <- alunos_sem_duplicatas %>%
  filter(!is.na(idade_aproximada_no_ingresso), curriculo %in% c(1999, 2017))

# Transformar curriculo em fator (para controlar a ordem)
df_idade$curriculo <- factor(df_idade$curriculo, levels = c(1999, 2017))

# --- BOXPLOT: distribuição por currículo ---
ggplot(df_idade, aes(x = curriculo, y = idade_aproximada_no_ingresso, fill = curriculo)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Distribuição da Idade no Ingresso por Currículo",
    x = "Currículo",
    y = "Idade Aproximada no Ingresso"
  ) +
  theme_minimal(base_size = 14)

# --- DENSIDADE: forma da distribuição ---
ggplot(df_idade, aes(x = idade_aproximada_no_ingresso, fill = curriculo, color = curriculo)) +
  geom_density(alpha = 0.3, size = 1.2) +
  labs(
    title = "Curvas de Densidade da Idade por Currículo",
    x = "Idade Aproximada no Ingresso",
    y = "Densidade"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")




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

# Função para filtrar evasões reais
filtrar_evasoes_reais <- function(df) {
  df %>%
    filter(
      status == "INATIVO",
      !is.na(periodo_de_evasao),
      tipo_de_evasao != "GRADUADO"
    )
}

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
  df <- filtrar_evasoes_reais(df) %>%  # aplica o filtro aqui
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
# SALVAMENTO DOS GRÁFICOS
# Pacotes necessários
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# ---- Funções auxiliares ----

# Filtrar evasões reais
filtrar_evasoes_reais <- function(df) {
  df %>%
    filter(
      status == "INATIVO",
      tipo_de_evasao != "GRADUADO",
      !is.na(periodo_de_evasao)
    )
}

# Calcular próximo período
proximo_periodo <- function(periodo) {
  partes <- str_split_fixed(periodo, "\\.", 2)
  ano <- as.integer(partes[, 1])
  semestre <- as.integer(partes[, 2])
  novo_ano <- ifelse(semestre == 2, ano + 1, ano)
  novo_semestre <- ifelse(semestre == 1, 2, 1)
  paste0(novo_ano, ".", novo_semestre)
}

# Calcular evasões por períodos
calcular_evasao_multiplos_periodos <- function(df) {
  df %>%
    filtrar_evasoes_reais() %>%
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

# Estatísticas por variável e período
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

# Função para gerar e salvar gráfico com cores melhoradas
plotar_grafico <- function(df_resultado, media, desvio, var, periodo) {
  df_resultado <- df_resultado %>% arrange(desc(taxa_evasao))
  
  p <- ggplot(df_resultado, aes(x = .data[[var]], y = taxa_evasao, fill = curriculo)) +
    geom_bar(stat = "identity", position = position_dodge2(width = 0.9, padding = 0.2)) +
    geom_hline(yintercept = media, color = "#264653", linetype = "dashed", size = 1) +
    annotate("text", 
             x = mean(seq_along(unique(df_resultado[[var]]))), 
             y = media, 
             label = sprintf("Média Geral: %.1f%%", media * 100),
             vjust = -1.2, 
             color = "#264653",
             size = 5,
             fontface = "bold") +
    annotate("rect", ymin = media - desvio, ymax = media + desvio,
             xmin = -Inf, xmax = Inf, fill = "gray80", alpha = 0.15, inherit.aes = FALSE) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.1))) +
    scale_fill_brewer(palette = "Set2", name = "Currículo") +
    labs(
      title = paste("Taxa de Evasão por", str_to_title(var), "-", periodo, "º Período"),
      x = str_to_title(var),
      y = "Taxa de Evasão (%)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = "gray80", size = 0.5, linetype = "solid"),
      legend.key = element_rect(fill = "white"),
      legend.title = element_text(face = "bold")
    )
  
  file_name <- paste0("evasao_", periodo, "p_", var, ".jpeg")
  ggsave(file_name, plot = p, width = 9, height = 5.5, dpi = 320, device = "jpeg", bg = "white")
  
  return(p)
}

# ---- Execução ----

variaveis <- c("sexo", "cor", "estado_civil", "forma_de_ingresso", "cota")
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
    
    plotar_grafico(df_resultado, media, desvio, var, periodo)
  }
}

###############################################################################
# Pacotes necessários
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# --- Função para filtrar evasões reais ---
filtrar_evasoes_reais <- function(df) {
  df %>%
    filter(
      curriculo %in% c(1999, 2017),
      status == "INATIVO",
      tipo_de_evasao != "GRADUADO",
      !is.na(periodo_de_evasao)
    )
}

# --- Função para calcular próximo período ---
proximo_periodo <- function(periodo) {
  partes <- str_split_fixed(periodo, "\\.", 2)
  ano <- as.integer(partes[, 1])
  semestre <- as.integer(partes[, 2])
  novo_ano <- ifelse(semestre == 2, ano + 1, ano)
  novo_semestre <- ifelse(semestre == 1, 2, 1)
  paste0(novo_ano, ".", novo_semestre)
}

# --- Calcular evasão múltiplos períodos ---
calcular_evasao_multiplos_periodos <- function(df) {
  df %>%
    filtrar_evasoes_reais() %>%
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

# --- Estatísticas por variável ---
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

# --- Função para gerar e salvar gráfico ---
plotar_grafico <- function(df_resultado, media, desvio, var, periodo) {
  df_resultado <- df_resultado %>% arrange(desc(taxa_evasao))
  
  # Criar posição para erro (desvio) e média
  posicao_x <- seq_along(df_resultado[[var]])
  
  p <- ggplot(df_resultado, aes_string(x = var, y = "taxa_evasao", fill = "curriculo")) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    geom_errorbar(aes(ymin = taxa_evasao - desvio, ymax = taxa_evasao + desvio),
                  width = 0.2, position = position_dodge(width = 0.8), color = "orange", size = 1) +
    geom_hline(yintercept = media, color = "red", linetype = "dashed", size = 0.7) +
    annotate("text", x = length(posicao_x) + 0.3, y = media, 
             label = sprintf("Média Geral: %.1f%%", media * 100), 
             vjust = -0.5, color = "red", size = 4) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.1))) +
    scale_fill_brewer(palette = "Set2", name = "Currículo") +
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
  
  file_name <- paste0("evasao_", periodo, "p_", var, ".jpeg")
  ggsave(file_name, plot = p, width = 9, height = 5.5, dpi = 320, device = "jpeg", bg = "white")
  
  return(p)
}


# --- Execução final ---
variaveis <- c("sexo", "cor", "estado_civil", "forma_de_ingresso", "cota")
df_evasao <- calcular_evasao_multiplos_periodos(alunos_sem_duplicatas)

for (periodo in 1:4) {
  cat(paste0("\n====== Estatísticas de Evasão - ", periodo, "º Período ======\n"))
  for (var in variaveis) {
    stats <- estatisticas_por_variavel(df_evasao, var, periodo)
    df_resultado <- stats$resultado
    media <- stats$media
    desvio <- stats$desvio
    
    cat(paste0("\n[", toupper(var), "] Média: ", round(media, 4), 
               ", Desvio Padrão: ", round(desvio, 4), "\n"))
    print(df_resultado)
    plotar_grafico(df_resultado, media, desvio, var, periodo)
  }
}

###############################################################################
# BOXPLOT DE VARIÁVEIS CONTÍNUAS

# Boxplot para variáveis contínuas estratificadas por currículo e evasão
# Pacotes necessários
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)

# --- Função para filtrar evasões reais ---
filtrar_evasoes_reais <- function(df) {
  df %>%
    filter(
      curriculo %in% c(1999, 2017),
      status == "INATIVO",
      tipo_de_evasao != "GRADUADO",
      !is.na(periodo_de_evasao)
    )
}

# --- Função para calcular próximo período ---
proximo_periodo <- function(periodo) {
  partes <- str_split_fixed(periodo, "\\.", 2)
  ano <- as.integer(partes[, 1])
  semestre <- as.integer(partes[, 2])
  novo_ano <- ifelse(semestre == 2, ano + 1, ano)
  novo_semestre <- ifelse(semestre == 1, 2, 1)
  paste0(novo_ano, ".", novo_semestre)
}

# --- Calcular evasão com variáveis auxiliares ---
calcular_evasao_multiplos_periodos <- function(df) {
  df %>%
    filtrar_evasoes_reais() %>%
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

# --- Função para gerar e salvar boxplot ---
plotar_boxplot_variavel_continua <- function(df, variavel_continua, periodo) {
  col_evasao <- paste0("evadiu_p", periodo)
  
  df_box <- df %>%
    filter(!is.na(.data[[variavel_continua]])) %>%
    select(curriculo, !!sym(variavel_continua), !!sym(col_evasao)) %>%
    mutate(evadido = ifelse(.data[[col_evasao]] == 1, "Evadido", "Não Evadido"))
  
  p <- ggplot(df_box, aes(x = curriculo, y = .data[[variavel_continua]], fill = evadido)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1.5, position = position_dodge(0.75)) +
    scale_fill_brewer(palette = "Set2", name = "Situação") +
    labs(
      title = paste("Boxplot de", str_to_title(gsub("_", " ", variavel_continua)), "-", periodo, "º Período"),
      x = "Currículo",
      y = str_to_title(gsub("_", " ", variavel_continua))
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  
  file_name <- paste0("boxplot_", periodo, "p_", variavel_continua, ".jpeg")
  ggsave(file_name, plot = p, width = 8, height = 5.5, dpi = 300, device = "jpeg", bg = "white")
  
  return(p)
}

# --- Variáveis contínuas a analisar ---
variaveis_continuas <- c("idade_aproximada_no_ingresso")

# --- Base com evasão processada ---
df_evasao <- calcular_evasao_multiplos_periodos(alunos_sem_duplicatas)

# --- Executar boxplots para todos os períodos ---
for (periodo in 1:4) {
  for (var in variaveis_continuas) {
    cat(paste0("\n📊 Gerando boxplot de ", var, " - ", periodo, "º período...\n"))
    print(plotar_boxplot_variavel_continua(df_evasao, var, periodo))
  }
}

###############################################################################

# Combinar todos os períodos em um único dataframe para boxplot conjunto
# --- Preparar dataframe para boxplot combinado de todos os períodos ---
preparar_boxplot_todos_periodos <- function(df, variavel_continua) {
  lista <- list()
  
  for (periodo in 1:4) {
    col_evasao <- paste0("evadiu_p", periodo)
    
    temp <- df %>%
      filter(curriculo %in% c(1999, 2017)) %>%              # filtro de currículos
      filter(status == "INATIVO", tipo_de_evasao != "GRADUADO") %>%  # filtro evasões reais
      select(curriculo, !!sym(variavel_continua), !!sym(col_evasao)) %>%
      filter(!is.na(.data[[variavel_continua]])) %>%
      mutate(
        periodo = paste0("P", periodo),
        evadido = ifelse(.data[[col_evasao]] == 1, "Evadido", "Não Evadido")
      ) %>%
      select(curriculo, periodo, evadido, !!sym(variavel_continua))
    
    lista[[periodo]] <- temp
  }
  
  bind_rows(lista)
}

# --- Gerar boxplot único para todos os períodos ---
plotar_boxplot_todos_periodos <- function(df_box, variavel_continua) {
  ggplot(df_box, aes(x = periodo, y = .data[[variavel_continua]], fill = curriculo)) +
    geom_boxplot(outlier.shape = 21, position = position_dodge(0.75)) +
    scale_fill_brewer(palette = "Set2", name = "Currículo") +
    labs(
      title = paste("Boxplot de", str_to_title(gsub("_", " ", variavel_continua)), "nos 4 Primeiros Períodos"),
      x = "Período de Evasão",
      y = str_to_title(gsub("_", " ", variavel_continua))
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
}

# --- Executar o processo ---
variavel <- "idade_aproximada_no_ingresso"
df_box <- preparar_boxplot_todos_periodos(df_evasao, variavel)

# Plotar e salvar gráfico
grafico_boxplot_final <- plotar_boxplot_todos_periodos(df_box, variavel)
print(grafico_boxplot_final)

ggsave("boxplot_idade_todos_periodos.jpeg", plot = grafico_boxplot_final, width = 10, height = 6, dpi = 320, device = "jpeg", bg = "white")


###
# --- Função para filtrar evasões reais ---
filtrar_evasoes_reais <- function(df) {
  df %>%
    filter(
      curriculo %in% c(1999, 2017),
      status == "INATIVO",
      tipo_de_evasao != "GRADUADO",
      !is.na(periodo_de_evasao)
    )
}

# --- Função para calcular próximo período ---
proximo_periodo <- function(periodo) {
  partes <- str_split_fixed(periodo, "\\.", 2)
  ano <- as.integer(partes[, 1])
  semestre <- as.integer(partes[, 2])
  novo_ano <- ifelse(semestre == 2, ano + 1, ano)
  novo_semestre <- ifelse(semestre == 1, 2, 1)
  paste0(novo_ano, ".", novo_semestre)
}

# --- Calcular evasão múltiplos períodos ---
calcular_evasao_multiplos_periodos <- function(df) {
  df %>%
    filtrar_evasoes_reais() %>%
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

# --- Estatísticas por variável ---
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

# --- Função para gerar e salvar gráfico (visual clean) ---
plotar_grafico <- function(df_resultado, media, desvio, var, periodo) {
  df_resultado <- df_resultado %>% arrange(desc(taxa_evasao))
  
  num_cats <- length(unique(df_resultado[[var]]))
  
  p <- ggplot(df_resultado, aes_string(x = var, y = "taxa_evasao", fill = "curriculo")) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.85) +
    
    # Faixa sombreada para desvio padrão ao redor da média geral
    annotate("rect",
             xmin = 0.5, xmax = num_cats + 0.5,
             ymin = media - desvio, ymax = media + desvio,
             alpha = 0.15, fill = "orange") +
    
    # Linha média
    geom_hline(yintercept = media, color = "red", linetype = "dashed", size = 1) +
    
    # Texto da média fora do gráfico, canto superior direito
    annotate("text",
             x = num_cats + 0.7,
             y = media,
             label = sprintf("Média Geral: %.1f%%", media * 100),
             color = "red",
             size = 5,
             fontface = "bold",
             vjust = 0.5,
             hjust = 0) +
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.1))) +
    scale_fill_brewer(palette = "Set2", name = "Currículo") +
    
    labs(
      title = paste("Taxa de Evasão por", str_to_title(var), "-", periodo, "º Período"),
      x = str_to_title(var),
      y = "Taxa de Evasão (%)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      legend.position = "top"
    )
  
  file_name <- paste0("evasao_", periodo, "p_", var, ".jpeg")
  ggsave(file_name, plot = p, width = 9, height = 5.5, dpi = 320, device = "jpeg", bg = "white")
  
  return(p)
}

# --- Execução final ---
variaveis <- c("sexo", "cor", "estado_civil", "forma_de_ingresso", "cota")
df_evasao <- calcular_evasao_multiplos_periodos(alunos_sem_duplicatas)

for (periodo in 1:4) {
  cat(paste0("\n====== Estatísticas de Evasão - ", periodo, "º Período ======\n"))
  for (var in variaveis) {
    stats <- estatisticas_por_variavel(df_evasao, var, periodo)
    df_resultado <- stats$resultado
    media <- stats$media
    desvio <- stats$desvio
    
    cat(paste0("\n[", toupper(var), "] Média: ", round(media, 4), 
               ", Desvio Padrão: ", round(desvio, 4), "\n"))
    print(df_resultado)
    plotar_grafico(df_resultado, media, desvio, var, periodo)
  }
}


###############################################################################
# CURRICULO 1999 1° PERIODO
# Taxas extraídas da tabela
taxas_evasao <- c(
  0.0886, 0.0919, 0.1477, 0.1047,
  0.0864, 0.2473, 0.1325, 0.1600,
  0.1383, 0.0909, 0.1235, 0.0988
)

# Cálculo
media <- mean(taxas_evasao)
desvio <- sd(taxas_evasao)

# Resultado formatado
sprintf("Média: %.4f (%.1f%%), Desvio Padrão: %.4f (%.1f%%)", 
        media, media * 100, desvio, desvio * 100)

###############################################################################
# CURRICULO 1999 2° PERIODO
# Vetor com taxas de evasão do 2º período - Currículo 1999
taxas_1999_p2 <- c(
  0.0633, 0.1839, 0.0909, 0.0698, 0.0741, 0.0753,
  0.0964, 0.2000, 0.0426, 0.1023, 0.0370, 0.0370
)

# Cálculo da média e do desvio padrão
media_1999_p2 <- mean(taxas_1999_p2)
desvio_1999_p2 <- sd(taxas_1999_p2)

# Resultados formatados
cat("📊 Estatísticas - Currículo 1999 (2º Período):\n")
cat(sprintf("Média da taxa de evasão: %.4f (%.1f%%)\n", media_1999_p2, media_1999_p2 * 100))
cat(sprintf("Desvio padrão: %.4f (%.1f%%)\n", desvio_1999_p2, desvio_1999_p2 * 100))

# CURRICULO 2017 2° PERIODO
# Vetor com taxas de evasão do 2º período - Currículo 2017
# Taxas de evasão do 2º período - Currículo 2017
taxas_2017_p2 <- c(
  0.0538, 0.0568, 0.0316, 0.0294, 0.0404,
  0.0000, 0.0222, 0.1000, 0.0575, 0.0115
)

# Cálculo da média e desvio padrão
media_2017_p2 <- mean(taxas_2017_p2)
desvio_2017_p2 <- sd(taxas_2017_p2)

# Exibir resultados
cat(sprintf("Média da taxa de evasão: %.4f (%.1f%%)\n", media_2017_p2, media_2017_p2 * 100))
cat(sprintf("Desvio padrão: %.4f (%.1f%%)\n", desvio_2017_p2, desvio_2017_p2 * 100))

# CURRICULO 1999 3° PERIODO
# Vetor com taxas de evasão do 3º período - Currículo 1999
# Taxas de evasão do currículo 1999 - 3º período
taxas_1999_p3 <- c(
  0.0886, 0.1149, 0.0795, 0.0698,
  0.1235, 0.0968, 0.0843, 0.0800,
  0.0532, 0.1136, 0.0988, 0.0617
)

# Cálculo da média e do desvio padrão
media_1999_p3 <- mean(taxas_1999_p3)
desvio_1999_p3 <- sd(taxas_1999_p3)

# Exibição formatada
cat(sprintf("Média da taxa de evasão: %.4f (%.1f%%)\n", media_1999_p3, media_1999_p3 * 100))
cat(sprintf("Desvio padrão: %.4f (%.1f%%)\n", desvio_1999_p3, desvio_1999_p3 * 100))


# CURRICULO 2017 3° PERIODO
# Taxas de evasão do currículo 2017 - 3º período
taxas_2017_p3 <- c(
  0.0323, 0.0795, 0.0211, 0.0098,
  0.0000, 0.0115, 0.0333, 0.0222,
  0.0345, 0.0345
)

# Cálculo da média e do desvio padrão
media_2017_p3 <- mean(taxas_2017_p3)
desvio_2017_p3 <- sd(taxas_2017_p3)

# Exibição formatada
cat(sprintf("Média da taxa de evasão: %.4f (%.1f%%)\n", media_2017_p3, media_2017_p3 * 100))
cat(sprintf("Desvio padrão: %.4f (%.1f%%)\n", desvio_2017_p3, desvio_2017_p3 * 100))



# CURRICULO 1999 4° PERIODO
# Taxas de evasão do 4º período - Currículo 1999
evasao_1999_p4 <- c(
  0.1646, 0.1609, 0.1591, 0.1512,
  0.2222, 0.0968, 0.1446, 0.0667,
  0.0426, 0.0682, 0.0741, 0.0000
)

# Cálculo da média e desvio padrão
media_1999_p4 <- mean(evasao_1999_p4)
desvio_1999_p4 <- sd(evasao_1999_p4)

# Exibir os resultados
cat(sprintf("Média da taxa de evasão: %.4f (%.1f%%)\n", media_1999_p4, media_1999_p4 * 100))
cat(sprintf("Desvio padrão: %.4f (%.1f%%)\n", desvio_1999_p4, desvio_1999_p4 * 100))

# CURRICULO 2017 4° PERIODO
# Taxas de evasão do 4º período - Currículo 2017
evasao_2017_p4 <- c(
  0.0645, 0.0909, 0.0211, 0.0196, 
  0.0303, 0.0575, 0.0222, 0.0222, 
  0.0115, 0.0345
)

# Cálculo da média e desvio padrão
media_2017_p4 <- mean(evasao_2017_p4)
desvio_2017_p4 <- sd(evasao_2017_p4)

# Exibir os resultados formatados
cat(sprintf("Média da taxa de evasão: %.4f (%.1f%%)\n", media_2017_p4, media_2017_p4 * 100))
cat(sprintf("Desvio padrão: %.4f (%.1f%%)\n", desvio_2017_p4, desvio_2017_p4 * 100))

###############################################################################
# library(ggplot2)
library(dplyr)
# Curvacomparativa da taxa
# Dados fornecidos
dados <- data.frame(
  periodo = rep(1:4, each = 2),
  curriculo = rep(c("1999", "2017"), times = 4),
  media = c(12.0, 5.3, 8.9, 4.0, 8.9, 2.8, 11.3, 3.7),
  desvio = c(4.6, 1.7, 5.3, 2.8, 2.2, 2.2, 6.4, 2.5)
)

# Converter período para fator para melhor visualização
dados$periodo <- factor(dados$periodo, levels = 1:4, labels = paste0(1:4, "º Período"))

# Plot
ggplot(dados, aes(x = periodo, y = media, group = curriculo, color = curriculo)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = media - desvio, ymax = media + desvio), width = 0.2, size = 0.8) +
  scale_color_manual(values = c("1999" = "#1f78b4", "2017" = "#33a02c")) +
  labs(
    title = "Comparação da Média da Taxa de Evasão por Currículo e Período",
    x = "Período",
    y = "Média da Taxa de Evasão (%)",
    color = "Currículo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

##########################################################################

# Gráfico de pontos com linhas (dot plot com erro)
# Pacotes necessários
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Dados organizados em um data frame
dados <- data.frame(
  Periodo = rep(1:4, each = 2),
  Curriculo = rep(c("1999", "2017"), times = 4),
  Media = c(12.0, 5.3,
            8.9, 4.0,
            8.9, 2.8,
            11.3, 3.7),
  Desvio = c(4.6, 1.7,
             5.3, 2.8,
             2.2, 2.2,
             6.4, 2.5)
)

# Plot
ggplot(dados, aes(x = Periodo, y = Media, color = Curriculo)) +
  geom_point(size = 4) +                          # Pontos das médias
  geom_line(size = 1) +                           # Linhas conectando os pontos
  geom_errorbar(aes(ymin = Media - Desvio, ymax = Media + Desvio), 
                width = 0.1, size = 0.8, alpha = 0.7) +  # Barras de erro (desvio padrão)
  scale_x_continuous(breaks = 1:4, labels = paste0(1:4, "º Período")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Média da Taxa de Evasão por Currículo e Período",
    x = "Período",
    y = "Taxa Média de Evasão (%)",
    color = "Currículo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )


#############################################################################
# Gráfico de radar (spider plot)
# Instalar o pacote caso não tenha
# install.packages("fmsb")

library(fmsb)

# Dados para radar (colunas = períodos, linhas = currículos)
dados_radar <- data.frame(
  `1º Período` = c(12.0, 5.3),
  `2º Período` = c(8.9, 4.0),
  `3º Período` = c(8.9, 2.8),
  `4º Período` = c(11.3, 3.7)
)
rownames(dados_radar) <- c("Currículo 1999", "Currículo 2017")

# Adiciona linhas para valores máximo e mínimo do gráfico
max_val <- ceiling(max(dados_radar))  # máximo para escala
min_val <- 0                         # mínimo

dados_radar <- rbind(
  rep(max_val, ncol(dados_radar)),   # linha de máximo
  rep(min_val, ncol(dados_radar)),   # linha de mínimo
  dados_radar
)

# Parâmetros visuais
cores <- c(rgb(0.2,0.5,0.5,0.5), rgb(0.8,0.2,0.5,0.5))

# Plot do gráfico radar
radarchart(dados_radar, axistype = 1,
           # Customização das linhas do grid
           pcol = c("darkblue", "darkred"),
           pfcol = cores,
           plwd = 2,
           plty = 1,
           # Customização dos eixos
           cglcol = "grey", cglty = 1, axislabcol = "grey", cglwd = 0.8,
           # Labels dos eixos
           vlcex = 1.2,
           title = "Média da Taxa de Evasão por Período e Currículo")

# Legenda
legend(x = "topright", legend = rownames(dados_radar)[-c(1,2)], 
       bty = "n", pch = 20 , col = c("darkblue", "darkred"), text.col = "black", cex = 1.1, pt.cex = 2)

###############################################################################


# Carregar pacotes necessários
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(scales)

# --- Função para filtrar evasões reais ---
filtrar_evasoes_reais <- function(df) {
  df %>%
    filter(
      curriculo %in% c(1999, 2017),
      status == "INATIVO",
      tipo_de_evasao != "GRADUADO",
      !is.na(periodo_de_evasao)
    )
}

# --- Função para calcular próximo período ---
proximo_periodo <- function(periodo) {
  partes <- str_split_fixed(periodo, "\\.", 2)
  ano <- as.integer(partes[, 1])
  semestre <- as.integer(partes[, 2])
  novo_ano <- ifelse(semestre == 2, ano + 1, ano)
  novo_semestre <- ifelse(semestre == 1, 2, 1)
  paste0(novo_ano, ".", novo_semestre)
}

# --- Calcular evasão em múltiplos períodos ---
calcular_evasao_multiplos_periodos <- function(df) {
  df %>%
    filtrar_evasoes_reais() %>%
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

# --- Preparar dados para boxplot geral ---
preparar_dados_boxplot <- function(df) {
  df_long <- df %>%
    select(curriculo, starts_with("evadiu_p")) %>%
    pivot_longer(
      cols = starts_with("evadiu_p"),
      names_to = "periodo",
      names_prefix = "evadiu_p",
      values_to = "evasao"
    ) %>%
    mutate(
      periodo = paste0(as.integer(periodo), "º Período"),
      periodo = factor(periodo, levels = paste0(1:4, "º Período")),
      curriculo = factor(curriculo, levels = c("1999", "2017"))
    )
  return(df_long)
}

# --- Gerar gráfico boxplot geral ---
gerar_boxplot_geral <- function(df_long) {
  p <- ggplot(df_long, aes(x = periodo, y = evasao, fill = curriculo)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("1999" = "orange", "2017" = "steelblue"), name = "Currículo") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Distribuição das Taxas de Evasão por Currículo e Período",
      x = "Período",
      y = "Taxa de Evasão"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  
  ggsave("boxplot_evasao_curriculos.jpeg", plot = p, width = 9, height = 6, dpi = 320, bg = "white")
  return(p)
}

# --- Executar ---
df_evasao <- calcular_evasao_multiplos_periodos(alunos_sem_duplicatas)
df_boxplot <- preparar_dados_boxplot(df_evasao)
gerar_boxplot_geral(df_boxplot)

###############################################################################

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

# Função para calcular evasão por sexo, currículo e período
evasao_por_sexo_periodo <- function(df, periodo) {
  col_evasao <- paste0("evadiu_p", periodo)
  
  df %>%
    group_by(periodo = paste0(periodo, "º Período"), sexo, curriculo) %>%
    summarise(
      total = n(),
      evasoes = sum(.data[[col_evasao]], na.rm = TRUE),
      taxa_evasao = evasoes / total,
      .groups = "drop"
    )
}

# Aplicar para os quatro períodos
df_evasao_periodos <- bind_rows(
  lapply(1:4, function(p) evasao_por_sexo_periodo(df_evasao, p))
)

# Organizar nomes mais legíveis para o gráfico
df_evasao_periodos$periodo <- factor(df_evasao_periodos$periodo,
                                     levels = c("1º Período", "2º Período", "3º Período", "4º Período"))

# Tabela final organizada
tabela_5_4_2 <- df_evasao_periodos %>%
  arrange(periodo, sexo, curriculo) %>%
  mutate(`Taxa de Evasão (%)` = round(taxa_evasao * 100, 1)) %>%
  select(`Período` = periodo, Sexo = sexo, Currículo = curriculo, `Taxa de Evasão (%)`)

print(tabela_5_4_2)

### Evasão por Sexo, Currículo e Período

ggplot(df_evasao_periodos, aes(x = as.factor(periodo), y = taxa_evasao, 
                               fill = sexo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~curriculo, labeller = label_both) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Dark2", name = "Sexo") +
  labs(
    title = "Figura 5.4.5 – Taxa de Evasão por Sexo, Currículo e Período",
    x = "Período",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
    legend.position = "right",
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )


ggsave("figura_5_4_2_barras_evasao_sexo_curriculo.jpeg", width = 10, height = 6, dpi = 320, device = "jpeg", bg = "white")


################################################################################
### média e desvio padrão por Sexo
# Pacotes
library(dplyr)

# Dados brutos conforme informado
dados_evasao <- data.frame(
  Periodo = c("1º Período", "1º Período", "1º Período",
              "2º Período", "2º Período", "2º Período", "2º Período",
              "3º Período", "3º Período", "3º Período", "3º Período",
              "4º Período", "4º Período", "4º Período", "4º Período"),
  Sexo = c("FEMININO", "FEMININO", "MASCULINO",
           "FEMININO", "FEMININO", "MASCULINO", "MASCULINO",
           "FEMININO", "FEMININO", "MASCULINO", "MASCULINO",
           "FEMININO", "FEMININO", "MASCULINO", "MASCULINO"),
  Curriculo = c(1999, 2017, 1999,
                1999, 2017, 1999, 2017,
                1999, 2017, 1999, 2017,
                1999, 2017, 1999, 2017),
  Taxa = c(32.1, 16.4, 23.3,
           14.8, 14.5, 14.1, 11.8,
           12.3, 12.7, 15.2, 6.2,
           7.4, 12.7, 15.0, 11.1)
)

# Calcular média e desvio padrão por Sexo e Currículo
tabela_5_4_6 <- dados_evasao %>%
  group_by(Sexo, Curriculo) %>%
  summarise(
    `Média (%)` = round(mean(Taxa), 1),
    `Desvio Padrão (%)` = round(sd(Taxa), 1),
    .groups = "drop"
  ) %>%
  arrange(Sexo, Curriculo)

# Exibir tabela formatada
print(tabela_5_4_6)


###############################################################################

# Função para organizar dados por período de evasão
idade_evasao_estatisticas <- function(df, periodo) {
  col_evasao <- paste0("evadiu_p", periodo)
  nome_periodo <- paste0(periodo, "º Período")
  
  df_filtrado <- df %>%
    filter(curriculo %in% c(1999, 2017)) %>%
    filter(!is.na(idade_aproximada_no_ingresso)) %>%
    filter(tipo_de_evasao != "GRADUADO") %>%
    filter(periodo_de_ingresso >= "2011.1" & periodo_de_ingresso <= "2023.2") %>%
    mutate(evadiu = .data[[col_evasao]] == 1)
  
  tabela <- df_filtrado %>%
    group_by(curriculo) %>%
    summarise(
      Ingressantes = n(),
      Evadiram = sum(evadiu, na.rm = TRUE),
      `Taxa de Evasão` = round(Evadiram / Ingressantes * 100, 1),
      `Média Idade` = round(mean(idade_aproximada_no_ingresso[evadiu], na.rm = TRUE), 1),
      `Desvio Padrão` = round(sd(idade_aproximada_no_ingresso[evadiu], na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    mutate(Período = nome_periodo) %>%
    select(Período, everything())
  
  return(tabela)
}

# Aplicar para todos os 4 períodos
tabela_estatisticas <- bind_rows(
  lapply(1:4, function(p) idade_evasao_estatisticas(df_evasao, p))
)

# Visualizar tabela
print(tabela_estatisticas)

# Salvar tabela em CSV
write.csv(tabela_estatisticas, "tabela_evasao_idade_por_curriculo.csv", row.names = FALSE)

# 🎨 Boxplot somente dos evadidos
df_box <- bind_rows(
  lapply(1:4, function(p) {
    col_evasao <- paste0("evadiu_p", p)
    df_evasao %>%
      filter(curriculo %in% c(1999, 2017)) %>%
      filter(!is.na(idade_aproximada_no_ingresso)) %>%
      filter(tipo_de_evasao != "GRADUADO") %>%
      filter(.data[[col_evasao]] == 1) %>%
      mutate(
        Período = paste0(p, "º Período")
      ) %>%
      select(curriculo, Período, idade_aproximada_no_ingresso)
  })
)

# Plotar gráfico
ggplot(df_box, aes(x = curriculo, y = idade_aproximada_no_ingresso, fill = as.factor(curriculo))) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5) +
  facet_wrap(~Período, labeller = label_both) +
  scale_fill_brewer(palette = "Set2", name = "Currículo") +
  labs(
    title = "Boxplot da Idade dos Alunos Evadidos por Currículo e Período",
    x = "Currículo",
    y = "Idade Aproximada no Ingresso"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 12),
    legend.position = "right",
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave("boxplot_idade_evasao_curriculo.jpeg", width = 10, height = 6, dpi = 320, device = "jpeg", bg = "white")




#############################################################################
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)

# Função auxiliar: transforma "2011.1" em 20111, "2020.2" em 20202 etc.
periodo_to_num <- function(p) {
  as.numeric(gsub("\\.", "", p))
}

# Função principal
idade_evasao_estatisticas <- function(df, periodo) {
  col_evasao <- paste0("evadiu_p", periodo)
  nome_periodo <- paste0(periodo, "º Período")
  
  df_filtrado <- df %>%
    mutate(periodo_ord = periodo_to_num(periodo_de_ingresso)) %>%
    filter(periodo_ord >= 20111 & periodo_ord <= 20232) %>%  # Filtragem correta por valor comparável
    filter(curriculo %in% c(1999, 2017)) %>%
    filter(!is.na(idade_aproximada_no_ingresso)) %>%
    filter(tipo_de_evasao != "GRADUADO") %>%
    mutate(evadiu = .data[[col_evasao]] == 1)
  
  tabela <- df_filtrado %>%
    group_by(curriculo) %>%
    summarise(
      Ingressantes = n(),
      Evadiram = sum(evadiu, na.rm = TRUE),
      `Taxa de Evasão` = round(Evadiram / Ingressantes * 100, 1),
      `Média Idade` = round(mean(idade_aproximada_no_ingresso[evadiu], na.rm = TRUE), 1),
      `Desvio Padrão` = round(sd(idade_aproximada_no_ingresso[evadiu], na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    mutate(Período = nome_periodo) %>%
    select(Período, everything())
  
  return(tabela)
}

# Aplicar para todos os 4 períodos
tabela_estatisticas <- bind_rows(
  lapply(1:4, function(p) idade_evasao_estatisticas(df_evasao, p))
)

# Visualizar tabela
print(tabela_estatisticas)

# Salvar tabela em CSV
write.csv(tabela_estatisticas, "tabela_evasao_idade_por_curriculo.csv", row.names = FALSE)

# 🎨 Boxplot somente dos evadidos, com filtragem corrigida
df_box <- bind_rows(
  lapply(1:4, function(p) {
    col_evasao <- paste0("evadiu_p", p)
    df_evasao %>%
      mutate(periodo_ord = periodo_to_num(periodo_de_ingresso)) %>%
      filter(periodo_ord >= 20111 & periodo_ord <= 20232) %>%
      filter(curriculo %in% c(1999, 2017)) %>%
      filter(!is.na(idade_aproximada_no_ingresso)) %>%
      filter(tipo_de_evasao != "GRADUADO") %>%
      filter(.data[[col_evasao]] == 1) %>%
      mutate(
        Período = paste0(p, "º Período")
      ) %>%
      select(curriculo, Período, idade_aproximada_no_ingresso)
  })
)

# Gráfico
ggplot(df_box, aes(x = curriculo, y = idade_aproximada_no_ingresso, fill = as.factor(curriculo))) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5) +
  facet_wrap(~Período, labeller = label_both) +
  scale_fill_brewer(palette = "Set2", name = "Currículo") +
  labs(
    title = "Boxplot da Idade dos Alunos Evadidos por Currículo e Período (2011.1 a 2023.2)",
    x = "Currículo",
    y = "Idade Aproximada no Ingresso"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 12),
    legend.position = "right",
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave("boxplot_idade_evasao_curriculo.jpeg", width = 10, height = 6, dpi = 320, device = "jpeg", bg = "white")

## 









# --- Pacotes necessários ---
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# --- Função para filtrar evasões reais ---
filtrar_evasoes_reais <- function(df) {
  df %>%
    filter(
      curriculo %in% c(1999, 2017),
      status == "INATIVO",
      tipo_de_evasao != "GRADUADO",
      !is.na(periodo_de_evasao)
    )
}

# --- Função para calcular próximo período ---
proximo_periodo <- function(periodo) {
  partes <- str_split_fixed(periodo, "\\.", 2)
  ano <- as.integer(partes[, 1])
  semestre <- as.integer(partes[, 2])
  novo_ano <- ifelse(semestre == 2, ano + 1, ano)
  novo_semestre <- ifelse(semestre == 1, 2, 1)
  paste0(novo_ano, ".", novo_semestre)
}

# --- Calcular evasão múltiplos períodos ---
calcular_evasao_multiplos_periodos <- function(df) {
  df %>%
    filtrar_evasoes_reais() %>%
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

# --- Estatísticas por variável e currículo ---
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
  
  # Médias por currículo
  medias_curriculo <- df_periodo %>%
    group_by(curriculo) %>%
    summarise(
      media = mean(taxa_evasao, na.rm = TRUE),
      desvio = sd(taxa_evasao, na.rm = TRUE),
      .groups = "drop"
    )
  
  media_geral <- mean(df_periodo$taxa_evasao, na.rm = TRUE)
  desvio_geral <- sd(df_periodo$taxa_evasao, na.rm = TRUE)
  
  list(
    resultado = df_periodo,
    medias_curriculo = medias_curriculo,
    media_geral = media_geral,
    desvio_geral = desvio_geral
  )
}

# --- Gráfico com visual limpo e comparativo ---
plotar_grafico <- function(df_resultado, medias_curriculo, media_geral, desvio_geral, var, periodo) {
  df_resultado <- df_resultado %>% arrange(desc(taxa_evasao))
  num_cats <- length(unique(df_resultado[[var]]))
  
  p <- ggplot(df_resultado, aes_string(x = var, y = "taxa_evasao", fill = "curriculo")) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.85) +
    
    annotate("rect",
             xmin = 0.5, xmax = num_cats + 0.5,
             ymin = media_geral - desvio_geral, ymax = media_geral + desvio_geral,
             alpha = 0.15, fill = "orange") +
    
    geom_hline(yintercept = media_geral, color = "red", linetype = "dashed", size = 1) +
    
    annotate("text",
             x = num_cats + 0.7,
             y = media_geral,
             label = sprintf("Média Geral: %.1f%%", media_geral * 100),
             color = "red", size = 5,
             fontface = "bold", vjust = 0.5, hjust = 0) +
    
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.1))) +
    scale_fill_brewer(palette = "Set2", name = "Currículo") +
    
    labs(
      title = paste("Taxa de Evasão por", str_to_title(var), "-", periodo, "º Período"),
      x = str_to_title(var),
      y = "Taxa de Evasão (%)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      legend.position = "top"
    )
  
  file_name <- paste0("evasao_", periodo, "p_", var, ".jpeg")
  ggsave(file_name, plot = p, width = 9, height = 5.5, dpi = 320, device = "jpeg", bg = "white")
  
  return(p)
}
##

# Variáveis atualizadas
variaveis <- c("cor", "cota", "estado_civil", "forma_de_ingresso", "tipo_de_evasao")

# Calcular base com marcações
df_evasao <- calcular_evasao_multiplos_periodos(alunos_sem_duplicatas)

# Loop principal
for (periodo in 1:4) {
  cat(paste0("\n====== Estatísticas de Evasão - ", periodo, "º Período ======\n"))
  for (var in variaveis) {
    stats <- estatisticas_por_variavel(df_evasao, var, periodo)
    df_resultado <- stats$resultado
    medias_curriculo <- stats$medias_curriculo
    media_geral <- stats$media_geral
    desvio_geral <- stats$desvio_geral
    
    cat(paste0("\n[", toupper(var), "]\n"))
    print(df_resultado)
    print(medias_curriculo)
    
    plotar_grafico(df_resultado, medias_curriculo, media_geral, desvio_geral, var, periodo)
  }
}
#
#
#
#

# Pacotes
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# --- Função para filtrar evasões reais ---
filtrar_evasoes_reais <- function(df) {
  df %>%
    filter(
      curriculo %in% c(1999, 2017),
      status == "INATIVO",
      tipo_de_evasao != "GRADUADO",
      !is.na(periodo_de_evasao)
    )
}

# --- Função para calcular próximo período ---
proximo_periodo <- function(periodo) {
  partes <- str_split_fixed(periodo, "\\.", 2)
  ano <- as.integer(partes[, 1])
  semestre <- as.integer(partes[, 2])
  novo_ano <- ifelse(semestre == 2, ano + 1, ano)
  novo_semestre <- ifelse(semestre == 1, 2, 1)
  paste0(novo_ano, ".", novo_semestre)
}

# --- Calcular evasão múltiplos períodos ---
calcular_evasao_multiplos_periodos <- function(df) {
  df %>%
    filtrar_evasoes_reais() %>%
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

# --- Estatísticas por variável e período ---
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
  
  medias_curriculo <- df_periodo %>%
    group_by(curriculo) %>%
    summarise(
      media = mean(taxa_evasao, na.rm = TRUE),
      desvio = sd(taxa_evasao, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(resultado = df_periodo, medias = medias_curriculo)
}

# --- Gráfico comparando currículos com desvio padrão ---
plotar_grafico <- function(df_resultado, medias, var, periodo) {
  df_resultado <- df_resultado %>% arrange(desc(taxa_evasao))
  num_cats <- length(unique(df_resultado[[var]]))
  
  p <- ggplot(df_resultado, aes_string(x = var, y = "taxa_evasao", fill = "curriculo")) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.85) +
    
    geom_hline(data = medias, aes(yintercept = media, color = curriculo), linetype = "dashed", size = 1) +
    geom_rect(data = medias, aes(
      ymin = media - desvio, ymax = media + desvio,
      xmin = -Inf, xmax = Inf,
      fill = NULL
    ), inherit.aes = FALSE, fill = "orange", alpha = 0.12) +
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.1))) +
    scale_fill_brewer(palette = "Set2", name = "Currículo") +
    scale_color_manual(values = c("1999" = "#1B9E77", "2017" = "#D95F02")) +
    
    labs(
      title = paste("Taxa de Evasão por", str_to_title(var), "-", periodo, "º Período"),
      x = str_to_title(var),
      y = "Taxa de Evasão (%)",
      color = "Média por Currículo"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  file_name <- paste0("evasao_", periodo, "p_", var, ".jpeg")
  ggsave(file_name, plot = p, width = 9, height = 5.5, dpi = 320, device = "jpeg", bg = "white")
  return(p)
}

# --- Execução principal ---
variaveis <- c("sexo", "cor", "estado_civil", "forma_de_ingresso", "cota")
df_evasao <- calcular_evasao_multiplos_periodos(alunos_sem_duplicatas)

for (periodo in 1:4) {
  cat(paste0("\n====== Estatísticas de Evasão - ", periodo, "º Período ======\n"))
  for (var in variaveis) {
    stats <- estatisticas_por_variavel(df_evasao, var, periodo)
    df_resultado <- stats$resultado
    medias <- stats$medias
    
    print(df_resultado)
    print(medias)
    
    plotar_grafico(df_resultado, medias, var, periodo)
  }
}

