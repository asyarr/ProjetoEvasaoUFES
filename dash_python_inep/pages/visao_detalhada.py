import dash
from dash import html

dash.register_page(__name__, path='/detalhada', name='Visão Detalhada')

# Layout simples de placeholder
layout = html.Div([
    html.H1('Página de Visão Detalhada'),
    html.P('Esta página está em construção.'),
    html.P('Não tem nada aqui ainda :p')
])