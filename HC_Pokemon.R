# ======================================
# 0️⃣ Pacotes necessários
# ======================================
# Instalar pacotes se necessário
required_packages <- c("tidyr", "dplyr", "jsonlite", "stringr", "ape", "ggplot2", "ggtree", "tidytree", "treeio", "yaml")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(new_packages)) {
  install.packages(new_packages)
  
  if(!"ggtree" %in% installed.packages()[,"Package"]) {
    if(!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
    BiocManager::install("ggtree")
  }
}

# Carregar pacotes
library(tidyr)
library(dplyr)
library(jsonlite)
library(stringr)
library(ape)
library(ggplot2)
library(ggtree)
library(tidytree)
library(treeio)
library(yaml)

# ======================================
# 1️⃣ Carregar dados com verificação
# ======================================
if(!file.exists("pokedex.csv")) {
  download.file(
    "https://www.dropbox.com/scl/fi/ucy4v2xutxjccp448gv3h/pokedex.csv?rlkey=yhr79mt29sfzoawxwgmrbv402&st=zk1z07vz&dl=1",
    destfile = "pokedex.csv",
    method = "libcurl"
  )
}

df <- read.csv("pokedex.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Salvar tipos originais para análise posterior
df_with_types <- df %>% select(Name, Type.1, Type.2)

# Selecionar apenas os atributos relevantes
required_cols <- c("Name", "Type.1", "Type.2", "Abilities", "Egg.Steps", "Moves")
df <- df[, required_cols]

# ======================================
# 2️⃣ Codificação de Habilidades
# ======================================
df_abilities <- df %>%
  filter(!is.na(Abilities) & Abilities != "") %>%
  mutate(Abilities = str_squish(Abilities)) %>%
  separate_rows(Abilities, sep = ",") %>%
  mutate(Abilities = str_squish(Abilities)) %>%
  filter(Abilities != "") %>%
  distinct(Name, Abilities) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = Abilities,
    values_from = value,
    values_fill = 0
  ) %>%
  rename_with(~ paste0("Ability_", .x), -Name)

# ======================================
# 3️⃣ Codificação de Tipos
# ======================================
df_types <- df %>%
  select(Name, Type.1, Type.2) %>%
  mutate(Type.2 = ifelse(Type.2 == "None" | Type.2 == "", NA, Type.2)) %>%
  pivot_longer(cols = c(Type.1, Type.2), names_to = "slot", values_to = "Type") %>%
  filter(!is.na(Type)) %>%
  distinct(Name, Type) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = Type, values_from = value, values_fill = 0) %>%
  rename_with(~ paste0("Type_", .x), -Name)

# ======================================
# 4️⃣ Codificação detalhada de moves (Média de Nível por Tipo de Move)
# ======================================

# Função de Parse
parse_moves_name_yaml <- function(x) {
  if (is.na(x) || x == "" || x == "[]") {
    return(tibble(Move_Name = character(0)))
  }
  x_clean <- str_replace_all(x, "None", "null")
  
  tryCatch({
    moves_list <- yaml::yaml.load(x_clean)
    if (length(moves_list) == 0) return(tibble(Move_Name = character(0)))
    
    # Extrai nomes das chaves
    move_names <- names(moves_list)
    if (is.null(move_names)) move_names <- unlist(moves_list)
    
    tibble(Move_Name = unique(move_names))
  },
  error = function(e) {
    message(paste("⚠️ Erro ao processar Moves:", e$message))
    return(tibble(Move_Name = character(0)))
  })
}

# Aplica e transforma (Cria 1 coluna por Move, valor 1 se o Pokémon aprende)
df_moves <- df %>%
  mutate(moves_parsed = lapply(Moves, parse_moves_name_yaml)) %>%
  select(Name, moves_parsed) %>%
  unnest(moves_parsed) %>%
  filter(!is.na(Move_Name) & Move_Name != "") %>%
  
  # Agregação final (Codificação Binária/One-Hot)
  mutate(value = 1) %>%
  pivot_wider(
    names_from = Move_Name,
    values_from = value,
    values_fill = 0
  )
# Renomeia colunas para evitar conflito com nomes de Tipos/Habilidades
names(df_moves) <- c("Name", paste0("Move_", names(df_moves)[-1]))


# ======================================
# 5️⃣ Combinar todos os dados
# ======================================
df_final <- df %>%
  select(Name, Egg.Steps) %>% # Inclui EggSteps aqui
  left_join(df_types, by = "Name") %>%
  left_join(df_abilities, by = "Name") %>%
  left_join(df_moves, by = "Name")

# Substituir NAs por 0 apenas nas colunas numéricas
numeric_cols <- sapply(df_final, is.numeric)
df_final[numeric_cols] <- lapply(df_final[numeric_cols], function(x) ifelse(is.na(x), 0, x))

# Remover colunas com apenas zeros (se houver)
df_final <- df_final[, colSums(df_final[, -1], na.rm = TRUE) > 0 | colnames(df_final) == "Name"]

# Garante uma linha única por Pokémon
df_final <- df_final %>% distinct(Name, .keep_all = TRUE)

# ======================================
# 🎯 FILTRAGEM, PESAGEM E CLUSTERING (SIMPLIFICADO)
# ======================================

df_final_completo <- df_final %>% filter(!is.na(Name))
cat("Utilizando", nrow(df_final_completo), "Pokémon para clustering.\n")

# 🔧 CONFIGURAR SEUS PESOS AQUI (AJUSTE CONFORME SUA PREFERÊNCIA)
# PESO MANUAL: O valor multiplica a importância da feature na distância.
my_weights <- list(
  abilities = 2.0,   # Habilidades (Traço Genético Forte)
  moves = 2.5,       # Moves (Comportamento/Conjunto de Aprendizagem)
  types = 1.0,       # Tipos (Base, peso neutro 1.0)
  egg_steps = 0.5    # Passos para Ovo (Baixa importância)
)

df_weighted <- df_final_completo

# --- Habilidades ---
ability_cols <- grep("^Ability_", names(df_weighted), value = TRUE)
df_weighted[ability_cols] <- df_weighted[ability_cols] * my_weights$abilities

# --- Moves: apenas raros ---
move_cols <- grep("^Move_", names(df_weighted), value = TRUE)
move_means <- colMeans(df_weighted[, move_cols], na.rm = TRUE)
rare_moves <- names(move_means[move_means <= 0.05])
# Substituir todas as colunas de moves pelas raras
df_weighted <- df_weighted[, c(setdiff(names(df_weighted), move_cols), rare_moves)]
# Aplicar peso
df_weighted[rare_moves] <- df_weighted[rare_moves] * my_weights$moves

# --- Tipos ---
type_cols <- grep("^Type_", names(df_weighted), value = TRUE)
df_weighted[type_cols] <- df_weighted[type_cols] * my_weights$types

# --- Egg.Steps ---
df_weighted$Egg.Steps <- df_weighted$Egg.Steps * my_weights$egg_steps

# Escalonar tudo, exceto Name
data_scaled <- scale(df_weighted[,-1])

# Substituir NAs ou Infs
data_scaled[is.na(data_scaled) | is.infinite(data_scaled)] <- 0

# Clustering hierárquico
hc_completo <- hclust(dist(data_scaled), method = "average")
phylo_tree_completo <- as.phylo(hc_completo)
phylo_tree_completo$tip.label <- df_weighted$Name

# ======================================
# 📊 ANÁLISE DE CLUSTERS RESULTANTES
# ======================================

# Função para análise dos clusters (Usando tipos originais para legibilidade)
analyze_clusters_with_types <- function(hc_object, df_original, df_types_original, n_clusters = 10) {
  clusters <- cutree(hc_object, n_clusters)
  
  cat(sprintf("\n=== ANÁLISE DOS %d CLUSTERS PRINCIPAIS ===\n", n_clusters))
  
  for(i in 1:n_clusters) {
    pokemon_in_cluster <- df_original$Name[clusters == i]
    if(length(pokemon_in_cluster) > 0) {
      cat(sprintf("\n🧩 CLUSTER %d (%d Pokémon):\n", i, length(pokemon_in_cluster)))
      cat("    ", paste(head(pokemon_in_cluster, 8), collapse = ", "))
      if(length(pokemon_in_cluster) > 8) cat(" ...")
      
      # Usar dataframe de tipos originais
      cluster_pokemon <- df_types_original[df_types_original$Name %in% pokemon_in_cluster, ]
      
      # Limpar tipos inválidos (None, "")
      valid_types_1 <- cluster_pokemon$Type.1
      valid_types_2 <- cluster_pokemon$Type.2
      
      valid_types_2 <- valid_types_2[valid_types_2 != "None" & valid_types_2 != ""]
      
      all_types <- c(valid_types_1, valid_types_2)
      type_table <- table(all_types)
      common_types <- names(sort(type_table, decreasing = TRUE))[1:3]
      
      if(length(common_types) > 0) {
        cat(sprintf("\n    Tipos comuns: %s\n", paste(common_types, collapse = ", ")))
      }
    }
  }
  return(clusters)
}

# Executar análise
clusters <- analyze_clusters_with_types(hc_completo, df_final_completo, df_with_types, 12)

# ======================================
# 🎨 VISUALIZAÇÃO MELHORADA
# ======================================

n_visualizacao <- 151

# Função para plotar com cores de cluster (Método seguro com %<+%)
plot_tree_colored_clusters <- function(phylo_tree, clusters, df_final, n_display) {
  
  # 1. Criar um SUBSET da árvore e dos clusters para exibição
  
  # Apenas os n_display primeiros
  pokemons_display <- head(df_final$Name, n_display) 
  
  # Subconjunto da árvore
  phylo_subset <- ape::keep.tip(phylo_tree, pokemons_display)
  
  # Criar um dataframe de cluster (Tip-Label to Cluster ID) APENAS PARA O SUBSET
  # Garantir que o vetor de clusters corresponda à ordem dos tip.label do subset!
  
  # Criar o DF de dados completo (usando a ordem do df_final_completo original)
  cluster_df_full <- data.frame(
    label = df_final$Name,
    cluster_id = as.factor(clusters)
  )
  
  # Fazer a junção com o objeto ggtree, que está na ordem do subset
  p <- ggtree(phylo_subset, layout = "circular") %<+% cluster_df_full
  
  # 3. Plotar usando o novo dado 'cluster_id'
  p <- p +
    geom_tree(aes(color = cluster_id), size = 0.8) +
    geom_tiplab2(
      size = 1.5, 
      aes(label = label, color = cluster_id), 
      offset = 0.02,
      align = TRUE,
      fontface = "bold",
      show.legend = FALSE
    ) +
    geom_tippoint(aes(color = cluster_id), size = 1.5, alpha = 0.7) +
    scale_color_discrete(name = "Cluster") +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.margin = unit(c(3, 3, 3, 3), "cm")
    ) +
    ggtitle(paste("Árvore Filogenética Clusterizada -", length(phylo_tree$tip.label), "Pokémon"))
  
  return(p)
}

# Gerar e salvar plots
cat("\nGerando visualizações...\n")

# Tamanhos maiores para acomodar todos os Pokémon
plot_width <- 16 
plot_height <- 12 

tryCatch({
  p_colored <- plot_tree_colored_clusters(phylo_tree_completo, clusters, df_final_completo, n_visualizacao)
  print(p_colored)
  ggsave("pokemon_tree_CLUSTERED_HD.png", p_colored, width = plot_width, height = plot_height, dpi = 300, bg = "white")
  cat("Plot colorido salvo em: pokemon_tree_CLUSTERED_HD.png\n")
  
}, error = function(e) {
  cat("Erro fatal na geração de plots:", e$message, "\n")
  # Plot de fallback
  p_fallback <- ggtree(phylo_tree_completo, layout = "circular") + 
    geom_tiplab2(size = 1.5) +
    ggtitle("Árvore Filogenética dos Pokémon (Fallback)")
  print(p_fallback)
  ggsave("pokemon_tree_FALLBACK_HD.png", p_fallback, width = 12, height = 10, dpi = 300, bg = "white")
})


# ======================================
# 💡 RELATÓRIO FINAL
# ======================================

cat(paste0("\n", strrep("=", 50), "\n"))
cat("✅ ANÁLISE FILOGENÉTICA CONCLUÍDA!\n")
cat(paste0("\n", strrep("=", 50), "\n"))

cat("\n📊 RESULTADOS OBTIDOS:\n")
cat("• Total de Pokémon processados:", nrow(df_final_completo), "\n")
cat("• Número de características (colunas):", ncol(df_final_completo) - 1, "\n")
cat("• Número de clusters identificados:", length(unique(clusters)), "\n")
cat("• Método de Clustering: Average Linkage (UPGMA) com Distância Euclidiana em dados escalonados\n")

cat("\n🔧 CONFIGURAÇÃO APLICADA (FATOR DE RARIDADE É APENAS BOOST > 1.0):\n")
cat("• Moves: Média de Nível por Tipo de Move (Level-Up apenas)\n")
cat(sprintf("• Moves (Peso Base): %.1f. Raridade aumenta se for raro.\n", my_weights$moves_types_avg))
cat(sprintf("• Habilidades (Peso Base): %.1f. Raridade aumenta se for rara.\n", my_weights$abilities)) 
cat(sprintf("• Tipos (Peso Base): %.1f. Raridade aumenta se for raro.\n", my_weights$types))
cat(sprintf("• Egg.Steps (Peso Manual): %.1f\n", my_weights$egg_steps))

cat("\n💡 PRÓXIMOS PASSOS SUGERIDOS:\n")
cat("1. Visualize o plot salvo para interpretar os clusters.\n")
cat("2. Modifique os pesos em 'my_weights' para afinar o modelo (e.g., aumentar o peso de Tipos ou Habilidades).\n")