# ======================================
# 📦 0️⃣ Pacotes necessários
# ======================================
required_packages <- c("cluster", "factoextra", "ggplot2", "dplyr", "tidyr")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(cluster)
library(factoextra)
library(ggplot2)
library(dplyr)
library(tidyr)

# ======================================
# 🧹 1️⃣ Preparação dos Dados
# ======================================
data(CO2)
df_co2 <- CO2

# Selecionar apenas colunas numéricas
co2_data <- df_co2[, sapply(df_co2, is.numeric)]

# Padronizar os dados
co2_scaled <- scale(co2_data)

# ======================================
# 🌳 2️⃣ Clustering Hierárquico
# ======================================
co2_dist <- dist(co2_scaled, method = "euclidean")
co2_hc <- hclust(co2_dist, method = "ward.D2")

# Visualizar o Dendrograma
plot(co2_hc, labels = FALSE, hang = -1, main = "Dendrograma do Dataset CO2 (Ward.D2)")

# ======================================
# ✂️ 3️⃣ Encontrar o K Ideal (Silhueta)
# ======================================
k_ideal_plot <- fviz_nbclust(
  x = co2_scaled,
  FUN = hcut,
  method = "silhouette",
  k.max = 10,
  hc_method = "ward.D2"
) +
  labs(title = "Silhueta Média vs. Número de Clusters (k)")

print(k_ideal_plot)

# ======================================
# 📊 4️⃣ Corte e Avaliação
# ======================================
k_final <- 3  # Ajuste conforme o gráfico de silhueta
df_co2$Cluster_HC <- cutree(co2_hc, k = k_final)

# Silhueta média
sil_obj <- silhouette(df_co2$Cluster_HC, co2_dist)
sil_avg <- mean(sil_obj[, 3])
cat(sprintf("\n✅ Silhueta Média para k=%d: %.4f\n", k_final, sil_avg))

# Comparação com variáveis categóricas (Type e Treatment)
cat("\n--- Tabela de Concordância: Cluster vs. Type ---\n")
print(table(Cluster = df_co2$Cluster_HC, Type = df_co2$Type))

cat("\n--- Tabela de Concordância: Cluster vs. Treatment ---\n")
print(table(Cluster = df_co2$Cluster_HC, Treatment = df_co2$Treatment))

# ===================================================
# 🎨 5️⃣  PCA + Visualização de Gráfico de Dispersão
# ===================================================
pca <- prcomp(co2_scaled)
df_plot <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  Cluster = factor(df_co2$Cluster_HC)
)

# Scatter plot dos clusters na PCA
ggplot(df_plot, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = sprintf("CO2: Clusterização Hierárquica (k=%d)", k_final),
    x = "Componente Principal 1 (PC1)",
    y = "Componente Principal 2 (PC2)",
    color = "Cluster"
  ) +
  theme_minimal()