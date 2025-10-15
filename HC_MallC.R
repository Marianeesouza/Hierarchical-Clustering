# ======================================
# 📦 0️⃣ Instalação / Carregamento de pacotes
# ======================================
required_packages <- c("ggplot2", "tidyr", "dplyr")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(ggplot2)
library(tidyr)
library(dplyr)

# ======================================
# 📥 1️⃣ Baixar e carregar o dataset
# ======================================
if(!file.exists("Mall_Customers.csv")) {
  download.file(
    "https://www.dropbox.com/scl/fi/fml5gqy2i7qk5pwe42byz/Mall_Customers.csv?rlkey=v6lk7jrg7e3cfwuyme9m9i1z8&st=pvywb3ms&dl=1",
    destfile = "Mall_Customers.csv",
    method = "libcurl"
  )
}

df <- read.csv("Mall_Customers.csv", header = TRUE, sep = ",", stringsAsFactors = T, fileEncoding = "UTF-8")

# ======================================
# 🧹 2️⃣ Seleção e preparação dos dados
# ======================================
df_num <- df[, c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]
df_scaled <- scale(df_num)  # Padroniza os dados

# ======================================
# 🌳 3️⃣ Clustering Hierárquico
# ======================================
dist_matrix <- dist(df_scaled, method = "euclidean")   # Distâncias Euclidianas
hc <- hclust(dist_matrix, method = "ward.D2")          # Método de ligação Ward

# (opcional) Visualizar dendrograma
# plot(hc, labels = FALSE, hang = -1, main = "Dendrograma - Clustering Hierárquico")

# ======================================
# ✂️ 4️⃣ Definir o número de grupos
# ======================================
k <- 5  # número de clusters desejado
df$Cluster <- cutree(hc, k = k)  # Adiciona a coluna de cluster ao dataframe

# ======================================
# 📊 5️⃣ Preparação para visualização
# ======================================
df_final <- df %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  select(Cluster, Age, Annual.Income..k.., Spending.Score..1.100.)

df_long <- df_final %>%
  pivot_longer(
    cols = c(Age, Annual.Income..k.., Spending.Score..1.100.),
    names_to = "Variavel",
    values_to = "Valor"
  ) %>%
  mutate(
    Variavel_Label = case_when(
      Variavel == "Age" ~ "Idade",
      Variavel == "Annual.Income..k.." ~ "Renda Anual (k$)",
      Variavel == "Spending.Score..1.100." ~ "Score de Gasto (1-100)"
    )
  )

# ======================================
# 🎨 6️⃣ Criação do Boxplot Facetado
# ======================================
ggplot(df_long, aes(x = Cluster, y = Valor, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Variavel_Label, scales = "free_y", ncol = 3) +
  labs(
    title = paste("Comparação das Variáveis por Cluster Hierárquico (k =", k, ")"),
    subtitle = "Linha central = Mediana | Altura = Distância Interquartil (50% dos dados)",
    x = "Número do Cluster",
    y = "Valor da Variável"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(vjust = 0.5)
  )
