# ======================================
# 0️⃣ Pacotes necessários
# ======================================
# Instalar pacotes se necessário
required_packages <- c("tidyr", "dplyr", "jsonlite", "stringr", "ape", "ggplot2", "ggtree", "tidytree", "treeio")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(new_packages)) {
  install.packages(new_packages)
  
  # ggtree pode precisar do BiocManager
  if(!"ggtree" %in% installed.packages()[,"Package"]) {
    if(!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
    BiocManager::install("ggtree")
  }
}

install.packages("yaml")
library(yaml)

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
# 4️⃣ Codificação detalhada de moves
# ======================================

# ======================================
# 1️⃣ Função de Parse Corrigida para a Coluna 'Moves'
#    - Usa yaml::read_yaml para maior robustez com aspas simples.
#    - Transforma a lista aninhada em um dataframe longo.
#    - Converte níveis ('--') para 0.
# ======================================

parse_moves_yaml <- function(x) {
  # 1. Tratamento de valores NA/vazios
  if (is.na(x) || x == "" || x == "[]") {
    return(tibble(Move_Name = NA, Move_Type = NA, Move_Level = 0))
  }
  
  # 2. Pré-processamento: Apenas a correção de 'None' para 'null'
  # O yaml::read_yaml lida com aspas simples, evitando erros léxicos.
  x_clean <- str_replace_all(x, "None", "null")
  
  tryCatch({
    # Usa yaml::read_yaml para maior tolerância à sintaxe
    moves_list <- read_yaml(text = x_clean)
    
    if (length(moves_list) == 0) {
      return(tibble(Move_Name = NA, Move_Type = NA, Move_Level = 0))
    }
    
    # 3. EXTRAÇÃO CORRIGIDA: Transforma a lista nomeada em DataFrame
    moves_df <- bind_rows(moves_list) %>% 
      # Adiciona o nome do move (que era o nome da lista/chave)
      mutate(Move_Name = names(moves_list), .before = 1) %>% 
      
      # 4. Seleção e Limpeza (Tratamento dos Levels)
      select(Move_Name, Type, Level) %>%
      rename(Move_Type = Type, Move_Level_Text = Level) %>%
      
      mutate(
        Move_Level_Text = as.character(Move_Level_Text),
        # Converte para numérico, tratando '—', '--', etc. como 0
        Move_Level = suppressWarnings(
          ifelse(
            Move_Level_Text %in% c("—", "--", "-", ""), 1, 
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
    # Mensagem de erro para ajudar a identificar problemas em Pokémon específicos
    message(paste("Erro ao processar Moves para um Pokémon. Detalhe:", e$message))
    return(tibble(Move_Name = NA, Move_Type = NA, Move_Level = 0))
  })
}

# ======================================
# 2️⃣ Aplicação e Transformação para Matriz de Características (Formato Largo)
# ======================================

# Aplica o parser à coluna 'Moves' (que contém as strings JSON/Yaml)
df_moves_detailed <- df %>%
  mutate(moves_parsed = lapply(Moves, parse_moves_yaml)) %>%
  # Mantém apenas 'Name' e a coluna com os dados de moves parseados
  select(Name, moves_parsed) %>%
  # Expande as listas aninhadas em novas linhas (Formato Longo)
  unnest(moves_parsed)

# 3. Conversão para o Formato Largo (WIDE) - REQUERIDO PARA CLUSTERING
# Este é o 'df_moves' que será unido ao 'df_final'
df_moves <- df_moves_detailed %>%
  # Remove linhas não parseadas
  filter(!is.na(Move_Name), !is.na(Move_Type)) %>%
  
  # Cria nomes de colunas únicos
  mutate(
    col_name_level = paste0(Move_Name, "_Level"),
    col_name_type = paste0("MoveType_", Move_Type)
  )

# 1️⃣ Matriz de níveis dos moves
df_moves_levels <- df_moves %>%
  select(Name, col_name_level, Move_Level) %>%
  pivot_wider(
    names_from = col_name_level,
    values_from = Move_Level,
    values_fill = 0
  )

# 2️⃣ Matriz de tipos aprendidos (binária)
df_moves_types <- df_moves %>%
  mutate(value = 1) %>%
  select(Name, col_name_type, value) %>%
  distinct() %>%  # evita duplicação caso o Pokémon aprenda vários moves do mesmo tipo
  pivot_wider(
    names_from = col_name_type,
    values_from = value,
    values_fill = 0
  )

# 3️⃣ Combinar níveis e tipos
df_moves <- df_moves_levels %>%
  left_join(df_moves_types, by = "Name")

colnames(df_moves)

# ======================================
# 5️⃣ Combinar todos os dados
# ======================================
df_final <- df %>%
  select(Name) %>%
  left_join(df_types, by = "Name") %>%
  left_join(df_abilities, by = "Name") %>%
  left_join(df_moves_levels, by = "Name")

# Substituir NAs por 0 apenas nas colunas numéricas
numeric_cols <- sapply(df_final, is.numeric)
df_final[numeric_cols] <- lapply(df_final[numeric_cols], function(x) ifelse(is.na(x), 0, x))

# Remover colunas com apenas zeros (se houver)
df_final <- df_final[, colSums(df_final[, -1], na.rm = TRUE) > 0 | colnames(df_final) == "Name"]

# Converter para tibble
df_final <- as_tibble(df_final)

# ======================================
# 🎯 USAR TODOS OS POKÉMON - SEM FILTRAGEM
# ======================================
cat("Número total de Pokémon:", nrow(df_final), "\n")

#Usar n POkemon
n <- 151
df_final_completo <- df_final[1:min(n, nrow(df_final)), ]

cat("Utilizando TODOS os Pokémon:", nrow(df_final_completo), "\n")

# ======================================
# 6️⃣ Clustering com todos os Pokémon
# ======================================
# Remover coluna Name e escalar
data_for_clustering <- df_final_completo[, -1]
data_scaled <- scale(data_for_clustering)

# Verificar e remover NAs/Infs resultantes do scale
if(any(is.na(data_scaled) | is.infinite(data_scaled))) {
  data_scaled[is.na(data_scaled) | is.infinite(data_scaled)] <- 0
  warning("NAs ou valores infinitos encontrados após escalonamento. Substituídos por 0.")
}

# Clustering hierárquico com TODOS os Pokémon
hc_completo <- hclust(dist(data_scaled), method = "ward.D2")
phylo_tree_completo <- as.phylo(hc_completo)

# ======================================
# 🔤 GARANTIR QUE OS LABELS SÃO OS NOMES DOS POKÉMON
# ======================================

# Atribuir os nomes dos Pokémon aos labels da árvore
nomes_pokemon <- df_final_completo$Name
phylo_tree_completo$tip.label <- nomes_pokemon

# Verificar se os labels estão corretos
cat("\nVerificação dos labels:\n")
cat("Primeiros 10 labels da árvore:", head(phylo_tree_completo$tip.label, 10), "\n")
cat("Total de Pokémon na árvore:", length(phylo_tree_completo$tip.label), "\n")

# ======================================
# 7️⃣ Plot Circular - Tree of Life COMPLETA
# ======================================

# VERSÃO 1 - Fan Layout com ajustes para muitos Pokémon
p2_completo <- ggtree(phylo_tree_completo, layout = "fan", size = 0.3, open.angle = 25) + 
  geom_tiplab2(
    size = 1.8,  # Fonte menor para muitos Pokémon
    aes(label = label),
    offset = 0.05,  # Offset menor
    align = TRUE,
    color = "#2C3E50",
    family = "sans",
    fontface = "bold",
    hjust = 0.5
  ) +
  geom_tippoint(
    size = 0.8,
    color = "#E74C3C",
    alpha = 0.6
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 20)),
    plot.margin = unit(c(3, 3, 3, 3), "cm")  # Margens maiores
  ) +
  ggtitle(paste("ÁRVORE FILOGENÉTICA COMPLETA -", nrow(df_final_completo), "Pokémon"))

print(p2_completo)

# VERSÃO 2 - Circular Layout
p1_completo <- ggtree(phylo_tree_completo, layout = "circular", size = 0.3) + 
  geom_tiplab2(
    size = 1.6,
    aes(label = label),
    offset = 0.04,
    align = TRUE,
    color = "darkblue",
    family = "sans",
    fontface = "bold"
  ) +
  geom_tippoint(
    size = 0.7,
    color = "#27AE60",
    alpha = 0.6
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = unit(c(3, 3, 3, 3), "cm")
  ) +
  ggtitle(paste("ÁRVORE FILOGENÉTICA COMPLETA -", nrow(df_final_completo), "Pokémon (Circular)"))

print(p1_completo)

# ======================================
# 8️⃣ Versão Colorida por Tipo
# ======================================
if("Type.1" %in% colnames(df)) {
  # Juntar informações de tipo
  type_info <- df %>% 
    select(Name, Type.1) %>% 
    filter(Name %in% df_final_completo$Name) %>%
    distinct()
  
  # Criar um vetor para associar tipos aos nomes dos Pokémon
  tipo_por_pokemon <- type_info$Type.1
  names(tipo_por_pokemon) <- type_info$Name
  
  # Cores para os tipos
  type_colors <- c(
    "Normal" = "#A8A878", "Fire" = "#F08030", "Water" = "#6890F0",
    "Electric" = "#F8D030", "Grass" = "#78C850", "Ice" = "#98D8D8",
    "Fighting" = "#C03028", "Poison" = "#A040A0", "Ground" = "#E0C068",
    "Flying" = "#A890F0", "Psychic" = "#F85888", "Bug" = "#A8B820",
    "Rock" = "#B8A038", "Ghost" = "#705898", "Dragon" = "#7038F8",
    "Dark" = "#705848", "Steel" = "#B8B8D0", "Fairy" = "#EE99AC"
  )
  
  p_colorido <- ggtree(phylo_tree_completo, layout = "fan", size = 0.3, open.angle = 25) + 
    geom_tiplab2(
      size = 1.8,
      aes(label = label, color = tipo_por_pokemon[label]),
      offset = 0.05,
      align = TRUE,
      fontface = "bold",
      show.legend = TRUE
    ) +
    geom_tippoint(
      size = 0.8,
      aes(color = tipo_por_pokemon[label]),
      alpha = 0.6
    ) +
    scale_color_manual(values = type_colors, name = "Tipo Principal") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.margin = unit(c(3, 3, 3, 3), "cm"),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
    ggtitle(paste("ÁRVORE FILOGENÉTICA COMPLETA -", nrow(df_final_completo), "Pokémon (Colorida por Tipo)"))
  
  print(p_colorido)
}

# ======================================
# 9️⃣ Versão Simplificada - Sem pontos para menos poluição visual
# ======================================
p_simples <- ggtree(phylo_tree_completo, layout = "fan", size = 0.2, open.angle = 30) + 
  geom_tiplab2(
    size = 1.5,
    aes(label = label),
    offset = 0.03,
    align = TRUE,
    color = "#2C3E50",
    family = "sans"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.margin = unit(c(4, 4, 4, 4), "cm")
  ) +
  ggtitle(paste("ÁRVORE FILOGENÉTICA -", nrow(df_final_completo), "Pokémon (Layout Simplificado)"))

print(p_simples)

# ======================================
# 🔟 Versão com Zoom Interativo (se necessário)
# ======================================
# Para muitos Pokémon, podemos criar uma versão com zoom
if(nrow(df_final_completo) > 150) {
  cat("\n⚠️  Muitos Pokémon detectados. Criando versão com zoom...\n")
  
  # Plot base para zoom
  p_base <- ggtree(phylo_tree_completo, layout = "circular", size = 0.2) + 
    geom_tiplab2(
      size = 1.2,
      aes(label = label),
      offset = 0.02,
      color = "#2C3E50",
      family = "sans"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      plot.margin = unit(c(2, 2, 2, 2), "cm")
    ) +
    ggtitle(paste("ÁRVORE FILOGENÉTICA COMPLETA -", nrow(df_final_completo), "Pokémon (Zoom necessário)"))
  
  print(p_base)
}

# ======================================
# 💾 Salvar os plots em alta qualidade
# ======================================
cat("Salvando plots...\n")

# Tamanhos maiores para acomodar todos os Pokémon
plot_width <- max(20, nrow(df_final_completo) / 8)
plot_height <- max(18, nrow(df_final_completo) / 10)

ggsave("pokemon_tree_COMPLETA_fan_HD.png", p2_completo, 
       width = plot_width, height = plot_height, dpi = 300, bg = "white")

ggsave("pokemon_tree_COMPLETA_circular_HD.png", p1_completo, 
       width = plot_width, height = plot_height, dpi = 300, bg = "white")

ggsave("pokemon_tree_COMPLETA_simples_HD.png", p_simples, 
       width = plot_width, height = plot_height, dpi = 300, bg = "white")

if(exists("p_colorido")) {
  ggsave(
    "pokemon_tree_COMPLETA_colorida_HD.png",
    p_colorido,
    width = plot_width,
    height = plot_height,
    dpi = 300,
    bg = "white"
  )
}

if(exists("p_base")) {
  ggsave("pokemon_tree_COMPLETA_base_HD.png", p_base, 
         width = plot_width, height = plot_height, dpi = 300, bg = "white")
}

# ======================================
# ℹ️ Informações finais
# ======================================
cat("\n=== ANÁLISE CONCLUÍDA ===\n")
cat("Número total de Pokémon analisados:", nrow(df_final_completo), "\n")
cat("Número de características:", ncol(df_final_completo) - 1, "\n")
cat("Labels utilizados: Nomes dos Pokémon\n")
cat("Arquivos salvos:\n")
cat("- pokemon_tree_COMPLETA_fan_HD.png\n")
cat("- pokemon_tree_COMPLETA_circular_HD.png\n")
cat("- pokemon_tree_COMPLETA_simples_HD.png\n")
if(exists("p_colorido")) cat("- pokemon_tree_COMPLETA_colorida_HD.png\n")
if(exists("p_base")) cat("- pokemon_tree_COMPLETA_base_HD.png\n")

# Verificação final
cat("\n✅ TODOS OS POKÉMON INCLUÍDOS:\n")
cat("Total na árvore:", length(phylo_tree_completo$tip.label), "\n")
cat("Exemplo de Pokémon incluídos:\n")
cat(paste(head(phylo_tree_completo$tip.label, 10), collapse = ", "), "\n")
cat("... e", length(phylo_tree_completo$tip.label) - 10, "mais\n")

cat("\n💡 DICAS PARA VISUALIZAÇÃO:\n")
cat("- Os plots salvos são grandes para acomodar todos os Pokémon\n")
cat("- Abra as imagens em um visualizador que permita zoom\n")
cat("- A versão 'simples' tem menos elementos visuais para melhor legibilidade\n")
cat("- Se ainda estiver muito denso, considere agrupar por geração ou tipo\n")