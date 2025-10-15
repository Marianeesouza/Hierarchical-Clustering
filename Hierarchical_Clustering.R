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
# 🚀 SISTEMA DE PESOS AUTOMÁTICOS (CORRIGIDO)
# ======================================

calculate_dual_weights <- function(df) {
  feature_sets <- list()
  
  # === 1️⃣ Identificar grupos de features ===
  feature_sets$types <- names(df)[names(df) %in% c(
    "Normal", "Fire", "Water", "Electric", "Grass", "Ice", 
    "Fighting", "Poison", "Ground", "Flying", "Psychic", 
    "Bug", "Rock", "Ghost", "Dragon", "Dark", "Steel", "Fairy"
  )]
  
  feature_sets$abilities <- names(df)[sapply(names(df), function(x) {
    x != "Name" && 
      !x %in% feature_sets$types &&
      grepl("^[A-Z][a-z]+$", x) # Assume que habilidades são capitalizadas
  })]
  
  # NOVO: Move Features são as colunas MoveType_xxx
  feature_sets$moves_types_avg <- names(df)[grepl("^MoveType_", names(df))] 
  
  # === 2️⃣ Função para calcular Fator de RARIDADE (Boost Apenas) ===
  calculate_rarity_boost <- function(features, df) {
    if(length(features) == 0) return(numeric(0))
    
    frequencies <- colSums(df[features] > 0, na.rm = TRUE)
    total_pokemon <- nrow(df)
    
    # Raridade base: Inverso da frequência normalizado
    rarity_base <- total_pokemon / frequencies
    
    # Média para normalização
    mean_rarity <- mean(rarity_base)
    
    # Fator de Boost: Será 1.0 ou > 1.0. Aumenta o peso apenas se for mais raro que a média.
    rarity_boost <- ifelse(
      rarity_base > mean_rarity, 
      # Aplica boost logarítmico (para não explodir o valor)
      pmin(rarity_base / mean_rarity, 5.0), # Limita o boost máximo a 5x
      1.0 # Sem boost para features na média ou mais comuns
    )
    
    names(rarity_boost) <- features
    return(rarity_boost)
  }
  
  # Calcular Boosts para Habilidades e Tipos (se necessário)
  rarity_boosts <- list(
    types = calculate_rarity_boost(feature_sets$types, df),
    abilities = calculate_rarity_boost(feature_sets$abilities, df),
    moves_types_avg = calculate_rarity_boost(feature_sets$moves_types_avg, df)
  )
  
  feature_sets_final <- list(
    types = feature_sets$types,
    abilities = feature_sets$abilities,
    moves_types_avg = feature_sets$moves_types_avg
  )
  
  return(list(feature_sets = feature_sets_final, rarity_boosts = rarity_boosts))
}

apply_custom_weights <- function(df, set_weights = NULL) {
  # Definir sets de pesos se não fornecidos
  if(is.null(set_weights)) {
    set_weights <- list(types = 1.0, abilities = 1.0, moves_types_avg = 1.0, egg_steps = 1.0)
  }
  
  weight_info <- calculate_dual_weights(df)
  df_weighted <- df
  
  # Loop para aplicar pesos (Peso Base * Fator de Raridade)
  for(set_name in names(set_weights)) {
    
    # 1. Tratar Egg.Steps manualmente (sem raridade)
    if(set_name == "egg_steps" && "Egg.Steps" %in% names(df_weighted)) {
      df_weighted$Egg.Steps <- df_weighted$Egg.Steps * set_weights[[set_name]]
      next
    }
    
    features <- weight_info$feature_sets[[set_name]]
    
    if(length(features) > 0) {
      
      # Peso Manual para o SET (ex: types=1.0, abilities=2.5)
      manual_weight <- set_weights[[set_name]]
      
      for(feature in features) {
        # Fator de raridade: 1.0 (comum) ou > 1.0 (raro)
        rarity_factor <- weight_info$rarity_boosts[[set_name]][[feature]]
        
        # Peso final = Peso Manual * Fator de Raridade
        final_weight <- manual_weight * rarity_factor
        
        # Aplica o peso final
        df_weighted[[feature]] <- df_weighted[[feature]] * final_weight
      }
    }
  }
  
  return(list(weighted_df = df_weighted, weight_info = weight_info, set_weights = set_weights))
}

print_weight_report <- function(weight_result) {
  cat("=== RELATÓRIO DE PESOS ===\n")
  weight_info <- weight_result$weight_info
  set_weights <- weight_result$set_weights
  
  for(set_name in names(set_weights)) {
    if(set_name == "egg_steps") {
      cat(sprintf("\n%s (Peso: %.1f):\n", "EGG.STEPS (MANUAL)", set_weights[[set_name]]))
      next
    }
    
    cat(sprintf("\n%s (Peso Base: %.1f):\n", toupper(set_name), set_weights[[set_name]]))
    features <- weight_info$feature_sets[[set_name]]
    
    if(length(features) > 0) {
      # Selecionar os 5 mais raros (maior boost)
      rarity_scores <- weight_info$rarity_boosts[[set_name]][features]
      features_sorted <- features[order(rarity_scores, decreasing = TRUE)]
      
      for(feature in head(features_sorted, 5)) { 
        rarity_factor <- weight_info$rarity_boosts[[set_name]][[feature]]
        final_w <- set_weights[[set_name]] * rarity_factor
        freq <- sum(weight_result$weighted_df[[feature]] > 0)
        
        # Nota: A frequência é count de Pokémon que a possuem (para raridade)
        cat(sprintf("  %s: freq=%d, boost=%.2fx, final=%.2f\n", 
                    feature, freq, rarity_factor, final_w))
      }
      if(length(features) > 5) cat(sprintf("  ... e mais %d features\n", length(features)-5))
    }
  }
}

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
  )

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
  pivot_wider(names_from = Type, values_from = value, values_fill = 0)

# ======================================
# 4️⃣ Codificação detalhada de moves (Média de Nível por Tipo de Move)
# ======================================

# Função de Parse Corrigida
parse_moves_yaml <- function(x) {
  if (is.na(x) || x == "" || x == "[]") {
    return(tibble(Move_Name = NA, Move_Type = NA, Move_Level = 0))
  }
  x_clean <- str_replace_all(x, "None", "null")
  
  tryCatch({
    moves_list <- read_yaml(text = x_clean)
    
    if (length(moves_list) == 0) {
      return(tibble(Move_Name = NA, Move_Type = NA, Move_Level = 0))
    }
    
    moves_df <- bind_rows(moves_list) %>% 
      mutate(Move_Name = names(moves_list), .before = 1) %>% 
      select(Move_Name, Type, Level) %>%
      rename(Move_Type = Type, Move_Level_Text = Level) %>%
      
      mutate(
        Move_Level_Text = as.character(Move_Level_Text),
        # CORREÇÃO: Usar 0 para moves não Level-Up (TM, Egg)
        Move_Level = suppressWarnings(
          ifelse(
            Move_Level_Text %in% c("—", "--", "-", ""), 0, # 0 para TM/Egg Moves
            ifelse(
              is.na(as.numeric(Move_Level_Text)),
              0, as.numeric(Move_Level_Text)
            )
          )
        )
      ) %>%
      select(Move_Name, Move_Type, Move_Level)
    
    return(moves_df)
  }, error = function(e) {
    message(paste("Erro ao processar Moves. Detalhe:", e$message))
    return(tibble(Move_Name = NA, Move_Type = NA, Move_Level = 0))
  })
}

# Aplica e transforma
df_moves_detailed <- df %>%
  mutate(moves_parsed = lapply(Moves, parse_moves_yaml)) %>%
  select(Name, moves_parsed) %>%
  unnest(moves_parsed)

# NOVO: Matriz de Média de Nível por Tipo de Move
df_moves <- df_moves_detailed %>%
  filter(!is.na(Move_Type) & Move_Level > 0) %>% # Apenas Level-Up Moves (Level > 0)
  group_by(Name, Move_Type) %>%
  # Calcula a média dos níveis para cada tipo de move
  summarise(
    avg_level = round(mean(Move_Level, na.rm = TRUE), 1),
    .groups = 'drop'
  ) %>%
  # Cria a coluna com o nome 'MoveType_Tipo'
  mutate(col_name = paste0("MoveType_", Move_Type)) %>%
  select(Name, col_name, avg_level) %>%
  pivot_wider(
    names_from = col_name,
    values_from = avg_level,
    values_fill = 0
  )


# ======================================
# 5️⃣ Combinar todos os dados
# ======================================
df_final <- df %>%
  select(Name, Egg.Steps) %>% # Inclui EggSteps aqui
  left_join(df_types, by = "Name") %>%
  left_join(df_abilities, by = "Name") %>%
  left_join(df_moves, by = "Name") # Moves: Média de Nível por Tipo

# Substituir NAs por 0 apenas nas colunas numéricas
numeric_cols <- sapply(df_final, is.numeric)
df_final[numeric_cols] <- lapply(df_final[numeric_cols], function(x) ifelse(is.na(x), 0, x))

# Remover colunas com apenas zeros (se houver)
df_final <- df_final[, colSums(df_final[, -1], na.rm = TRUE) > 0 | colnames(df_final) == "Name"]

# Garante uma linha única por Pokémon
df_final <- df_final %>% distinct(Name, .keep_all = TRUE)


# ======================================
# 🎯 FILTRAGEM E CLUSTERING
# ======================================
# Usar n Pokémon (Ex: 1ª Geração)
df_final_completo <- df_final %>% filter(!is.na(Name))

cat("Utilizando", nrow(df_final_completo), "Pokémon para clustering.\n")

# 🔧 CONFIGURAR SEUS PESOS AQUI (AJUSTE CONFORME SUA PREFERÊNCIA)
# Peso Base: Valor numérico que a feature terá. A raridade só aumenta este valor.
my_weights <- list(
  types = 4.0,               # Tipos: Peso Base. Raridade aumenta se for raro.
  abilities = 2.5,           # Habilidades: Genética forte (alto peso). Raridade aumenta se for rara.
  moves_types_avg = 2.0,     # Moves (Média de Nível por Tipo): Peso Base. Raridade aumenta se for raro.
  egg_steps = 0.5            # Passos para Ovo: Peso manual, sem raridade.
)

cat("Aplicando sistema de pesos automáticos...\n")

# Aplicar pesos automáticos e transformação (Nível^2)
weighted_result <- apply_custom_weights(df_final_completo, my_weights)
df_weighted <- weighted_result$weighted_df

# Mostrar relatório dos pesos
print_weight_report(weighted_result)

# Aplica peso manual para Egg.Steps (já embutido na apply_custom_weights, mas reconfirmação)
# Nada mais precisa ser feito aqui, pois a função apply_custom_weights já cuida disso.

# ➡️ CORREÇÃO: Usar DADOS PONDERADOS para o clustering
data_for_clustering <- df_weighted[, -1]

# Escalonar dados (MUITO IMPORTANTE!)
data_scaled <- scale(data_for_clustering)

# Verificar e remover NAs/Infs resultantes do scale
if(any(is.na(data_scaled) | is.infinite(data_scaled))) {
  data_scaled[is.na(data_scaled) | is.infinite(data_scaled)] <- 0
  warning("NAs ou valores infinitos encontrados após escalonamento. Substituídos por 0.")
}

# Clustering hierárquico com dados ponderados
hc_completo <- hclust(dist(data_scaled), method = "ward.D2")
phylo_tree_completo <- as.phylo(hc_completo)

# Garantir que os labels são os nomes dos Pokémon
phylo_tree_completo$tip.label <- df_final_completo$Name

cat("✅ Clustering com pesos aplicados concluído!\n")

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

n_visualizacao <- 386

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
cat("• Método de Clustering: Ward.D2 com Distância Euclidiana em dados escalonados\n")

cat("\n🔧 CONFIGURAÇÃO APLICADA (FATOR DE RARIDADE É APENAS BOOST > 1.0):\n")
cat("• Moves: Média de Nível por Tipo de Move (Level-Up apenas)\n")
cat(sprintf("• Moves (Peso Base): %.1f. Raridade aumenta se for raro.\n", my_weights$moves_types_avg))
cat(sprintf("• Habilidades (Peso Base): %.1f. Raridade aumenta se for rara.\n", my_weights$abilities)) 
cat(sprintf("• Tipos (Peso Base): %.1f. Raridade aumenta se for raro.\n", my_weights$types))
cat(sprintf("• Egg.Steps (Peso Manual): %.1f\n", my_weights$egg_steps))

cat("\n💡 PRÓXIMOS PASSOS SUGERIDOS:\n")
cat("1. Visualize o plot salvo para interpretar os clusters.\n")
cat("2. Modifique os pesos em 'my_weights' para afinar o modelo (e.g., aumentar o peso de Tipos ou Habilidades).\n")