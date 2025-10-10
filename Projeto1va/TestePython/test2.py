# ======================================
# Bibliotecas
# ======================================
import pandas as pd
import numpy as np
from scipy.cluster.hierarchy import linkage, dendrogram
from sklearn.preprocessing import StandardScaler
import plotly.figure_factory as ff
import dash
from dash import dcc, html, Input, Output, State, callback_context
import dash_bootstrap_components as dbc
import plotly.express as px
from jupyter_dash import JupyterDash

# ======================================
# 1️⃣ Carregar e preparar dados
# ======================================
df = pd.read_csv("pokemon.csv", encoding="utf-16", sep="\t", quotechar='"')

#df = df.head(151)  # Usar apenas os primeiros 151 Pokémons
# Selecionar colunas numéricas
cols_numeric = ["base_egg_steps"]

data_numeric = df[cols_numeric].fillna(0)  # Preencher NaNs com 0

# One-Hot Encoding para tipos
type_primary = pd.get_dummies(df['primary_type'], prefix='type')
type_secondary = pd.get_dummies(df['secondary_type'].fillna('none'), prefix='type2')

# One-hot encoding das habilidades
abilities_0 = pd.get_dummies(df['abilities_0'], prefix='ability0')
abilities_1 = pd.get_dummies(df['abilities_1'].fillna('none'), prefix='ability1')
abilities_2 = pd.get_dummies(df['abilities_2'].fillna('none'), prefix='ability2')
abilities_hidden = pd.get_dummies(df['abilities_hidden'].fillna('none'), prefix='abilityH')

# Garantir que os índices batem
type_primary = type_primary.loc[data_numeric.index]
type_secondary = type_secondary.loc[data_numeric.index]
abilities_0 = abilities_0.loc[data_numeric.index]
abilities_1 = abilities_1.loc[data_numeric.index]
abilities_2 = abilities_2.loc[data_numeric.index]
abilities_hidden = abilities_hidden.loc[data_numeric.index]

# Concatenar números + tipos codificados
data_full = pd.concat([data_numeric, type_primary, type_secondary, abilities_0, abilities_1, abilities_2, abilities_hidden], axis=1)

# Guardar nomes e tipos originais
names = df.loc[data_numeric.index, "english_name"]
types = df.loc[data_numeric.index, "primary_type"]
full_data = df.loc[data_numeric.index]

print(f"Usando {len(data_full)} Pokémon para clustering.")
# Padronizar todos os dados
scaler = StandardScaler()
data_scaled = scaler.fit_transform(data_full)

# ======================================
# 2️⃣ Clustering hierárquico
# ======================================
Z = linkage(data_scaled, method="ward")
# ======================================
# 3️⃣ Função para dendrograma completo
# ======================================
def create_full_dendrogram():
    fig = ff.create_dendrogram(
        data_scaled,
        labels=names.values,
        linkagefun=lambda x: Z,
        orientation='left',
        color_threshold=0
    )
    fig.update_layout(
        width=1200,
        height=max(600, len(names) * 3),
        title="Dendrograma Completo - Pokémon (Clique em qualquer ramo para explorar)",
        xaxis_title="Distância",
        yaxis_title="Pokémon"
    )
    return fig

# ======================================
# 4️⃣ Dashboard Interativo com Dash
# ======================================
app = dash.Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])

app.layout = dbc.Container([
    dbc.Row([
        dbc.Col([html.H1("🌳 Explorador de Dendrograma - Pokémon", className="text-center mb-4")])
    ]),
    dbc.Row([
        dbc.Col([
            dbc.Card([
                dbc.CardHeader("Controles de Navegação"),
                dbc.CardBody([
                    dbc.Button("🏠 Vista Completa", id="btn-full-view", color="primary", className="me-2"),
                ])
            ], className="mb-3")
        ])
    ]),
    dbc.Row([
        dbc.Col([
            dcc.Graph(id="dendrogram-plot", figure=create_full_dendrogram(),
                      config={'displayModeBar': True}, style={'height': '80vh'})
        ])
    ])
], fluid=True)

@app.callback(
    Output('dendrogram-plot', 'figure'),
    Input('btn-full-view', 'n_clicks')
)
def show_full_dendrogram(n_clicks):
    return create_full_dendrogram()

# ======================================
# 5️⃣ Rodar o app
# ======================================
if __name__ == '__main__':
    app.run(debug=True, port=8050)