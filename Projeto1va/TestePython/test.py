# ======================================
# 1️⃣ Importar bibliotecas
# ======================================
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os

from scipy.cluster.hierarchy import linkage, dendrogram, fcluster
from sklearn.preprocessing import StandardScaler
# ======================================
# 2️⃣ Carregar dataset da Pokédex
# ======================================

df = pd.read_csv("pokemon.csv", encoding="utf-16", sep="\t", quotechar='"')
print(df.shape)
df.head()

print("Primeiras linhas:")
print(df.head())
# ======================================
# 3️⃣ Selecionar colunas numéricas de interesse
# ======================================
# Vamos focar em atributos de batalha
cols = ["hp", "attack", "defense", "sp_attack", "sp_defense", "speed", "weight_kg"]
data = df[cols].dropna()

# Guardar os nomes dos Pokémon (para o dendrograma)
names = df.loc[data.index, "english_name"]

print(f"Usando {len(data)} Pokémon para clustering.")
# ======================================
# 4️⃣ Padronizar os dados
# ======================================
scaler = StandardScaler()
data_scaled = scaler.fit_transform(data)
# ======================================
# 5️⃣ Aplicar o Hierarchical Clustering
# ======================================
# Método "ward" minimiza a variância dentro dos clusters
Z = linkage(data_scaled, method="ward")
# ======================================
# 6️⃣ Plotar o Dendrograma
# ======================================
plt.figure(figsize=(14, 6))
plt.title("Hierarchical Clustering - Pokémon (Ward Linkage)")
plt.xlabel("Pokémon")
plt.ylabel("Distância")

# Dendrograma truncado para não ficar gigante
dendrogram(Z, labels=names.values, leaf_rotation=90, leaf_font_size=6, truncate_mode="level", p=5)
plt.tight_layout()
plt.show()
# ======================================
# 7️⃣ Definir clusters (por exemplo, 5 grupos)
# ======================================
clusters = fcluster(Z, t=5, criterion="maxclust")

df_clusters = df.loc[data.index, ["english_name", "primary_type"]].copy()
df_clusters["cluster"] = clusters

print("\nAmostra dos grupos formados:")
print(df_clusters.groupby("cluster")["primary_type"].value_counts().head(15))
# ======================================
# 8️⃣ Visualização 2D dos clusters (opcional)
# ======================================
from sklearn.decomposition import PCA

pca = PCA(n_components=2)
reduced = pca.fit_transform(data_scaled)

plt.figure(figsize=(8, 6))
sns.scatterplot(
    x=reduced[:, 0],
    y=reduced[:, 1],
    hue=clusters,
    palette="tab10",
    legend="full"
)
plt.title("Pokémon agrupados por similaridade (Hierarchical Clustering + PCA)")
plt.xlabel("Componente Principal 1")
plt.ylabel("Componente Principal 2")
plt.show()