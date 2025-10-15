
if(!file.exists("Mall_Customers.csv")) {
  download.file(
    "https://www.dropbox.com/scl/fi/fml5gqy2i7qk5pwe42byz/Mall_Customers.csv?rlkey=v6lk7jrg7e3cfwuyme9m9i1z8&st=pvywb3ms&dl=1",
    destfile = "Mall_Customers.csv",
    method = "libcurl"
  )
}

df <- read.csv("Mall_Customers.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Preparação dos Dados (Limpeza e Transformação)
# Supondo que 'df' é o seu dataframe original com a coluna 'Cluster'
# Se você já rodou o código anterior, 'df' já tem a coluna 'Cluster'.

df_final <- df %>%
  mutate(Cluster = as.factor(clusters)) %>% # Garante que Cluster é fator para o eixo X
  
  # Seleciona as variáveis a serem plotadas
  select(Cluster, Age, Annual.Income..k.., Spending.Score..1.100.) 

# 2. Transformar para o formato longo (para facilitar o facetamento no ggplot)
df_long <- df_final %>%
  pivot_longer(
    cols = c(Age, Annual.Income..k.., Spending.Score..1.100.),
    names_to = "Variavel",
    values_to = "Valor"
  ) %>%
  # Limpa os nomes das variáveis para o gráfico
  mutate(
    Variavel_Label = case_when(
      Variavel == "Age" ~ "Idade",
      Variavel == "Annual.Income..k.." ~ "Renda Anual (k$)",
      Variavel == "Spending.Score..1.100." ~ "Score de Gasto (1-100)"
    )
  )

# 3. Criação do Boxplot Facetado
ggplot(df_long, aes(x = Cluster, y = Valor, fill = Cluster)) +
  
  geom_boxplot(alpha = 0.7) +
  
  # Faceta (divide) o gráfico em 3 painéis, um para cada variável
  # 'scales = "free_y"' permite que cada painel tenha seu próprio eixo Y (crucial!)
  facet_wrap(~ Variavel_Label, scales = "free_y", ncol = 3) + 
  
  labs(
    title = "Comparação da Distribuição das Variáveis por Cluster (k=5)",
    subtitle = "Linha central = Mediana | Altura = Distância Interquartil (50% dos dados)",
    x = "Número do Cluster",
    y = "Valor da Variável"
  ) +
  
  theme_bw() +
  theme(
    legend.position = "none", # Remove a legenda de preenchimento
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    # Ajusta o texto do eixo X
    axis.text.x = element_text(vjust = 0.5) 
  )