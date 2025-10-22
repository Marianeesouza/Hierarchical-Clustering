# ======================================
# 📦 0️⃣ Instalação / Carregamento de pacotes
# ======================================
required_packages <- c("ggplot2", "tidyr", "dplyr", "cluster", "factoextra")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(ggplot2)
library(tidyr)
library(dplyr)
library(cluster)
library(factoextra)

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

df <- read.csv("Mall_Customers.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, fileEncoding = "UTF-8")

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

plot(hc, labels = FALSE, hang = -1, main = "Dendrograma - Clustering Hierárquico")

# ======================================
# ✂️ 4️⃣ Definir o número de grupos
# ======================================
k <- 6  # número de clusters desejado
df$Cluster <- cutree(hc, k = k)  # Adiciona a coluna de cluster ao dataframe

# ======================================
# 📈 4️⃣ Gráfico da Silhueta Média — Escolha do K Ideal
# ======================================
fviz_nbclust(
  x = df_scaled,
  FUN = hcut,                  # Hierarchical clustering
  method = "silhouette",       # Critério da silhueta
  k.max = 10,                  # Testar até 10 clusters
  hc_method = "ward.D2"        # Mesmo método do clustering
) +
  labs(
    title = "Silhueta Média vs Número de Clusters (k)",
    subtitle = "Avaliação do número ideal de clusters com base na silhueta média",
    x = "Número de Clusters (k)",
    y = "Silhueta Média"
  )

# ======================================
# 📊 6️⃣ Boxplot das variáveis por cluster
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

ggplot(df_long, aes(x = Cluster, y = Valor, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Variavel_Label, scales = "free_y", ncol = 3) +
  labs(
    title = paste("Comparação das Variáveis por Cluster Hierárquico (k =", k, ")"),
    subtitle = "Linha central = Mediana | Altura = Intervalo Interquartil (50% dos dados)",
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

# ======================================
# 🧭 7️⃣ PCA + Gráfico de Dispersão dos Clusters
# ======================================
pca <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

df_pca <- data.frame(
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  Cluster = factor(df$Cluster)
)

ggplot(df_pca, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = sprintf("Clusterização Hierárquica (PCA) - Mall Customers (k=%d)", k),
    subtitle = "Visualização nas duas primeiras Componentes Principais",
    x = "Componente Principal 1 (PC1)",
    y = "Componente Principal 2 (PC2)",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
