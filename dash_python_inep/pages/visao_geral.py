import dash
import re
from dash import dcc, html, Input, Output
import plotly.graph_objects as go
import pandas as pd
import dash_bootstrap_components as dbc

dash.register_page(__name__, path='/', name='Visão Geral')

df = pd.read_csv(r'dados/cursos_filtrados.csv', sep=";", encoding="latin1")
cursos_desejados = ["Física", "Matemática", "Química", "Estatística"]
df['NO_CURSO_NORMALIZED'] = df['NO_CURSO'].str.extract(f'({"|".join(cursos_desejados)})', expand=False, flags=re.IGNORECASE)
df.dropna(subset=['NO_CURSO_NORMALIZED'], inplace=True)
df['NO_CURSO_NORMALIZED'] = df['NO_CURSO_NORMALIZED'].str.title()
lista_cursos = df['NO_CURSO_NORMALIZED'].unique()
df_grouped = df.groupby(["NU_ANO_CENSO", "NO_CURSO_NORMALIZED"]).agg({
    "QT_VG_TOTAL": "sum", "QT_ING": "sum", "QT_MAT": "sum",
    "QT_SIT_DESVINCULADO": "sum", "QT_CONC": "sum"
}).reset_index()
df_grouped.rename(columns={
    'NU_ANO_CENSO': 'Ano', 'NO_CURSO_NORMALIZED': 'Curso', 'QT_VG_TOTAL': 'Vagas',
    'QT_ING': 'Ingressantes', 'QT_MAT': 'Matriculados',
    'QT_SIT_DESVINCULADO': 'Evadidos', 'QT_CONC': 'Diplomados'
}, inplace=True)

def calculate_indicators(dff):
    dff = dff.copy()
    ingressantes_safe = dff["Ingressantes"].clip(lower=1)
    evadidos_safe = dff["Evadidos"].clip(lower=1)
    matriculados_safe = dff["Matriculados"].clip(lower=1)
    vagas_safe = dff["Vagas"].clip(lower=1)
    dff.loc[:, "Taxa de Preenchimento de Vagas (%)"] = (100 * dff["Ingressantes"] / vagas_safe).round(1)
    dff.loc[:, "Taxa de Ocupação (%)"] = (100 * dff["Matriculados"] / vagas_safe).round(1)
    dff.loc[:, "Taxa de Conclusão de Curso (%)"] = (100 * dff["Diplomados"] / ingressantes_safe).round(1)
    dff.loc[:, "Taxa de Evasão (%)"] = (100 * dff["Evadidos"] / matriculados_safe).round(1)
    dff.loc[:, "Taxa de Retenção (%)"] = (100 - dff["Taxa de Evasão (%)"]).round(1)
    dff.loc[:, "Relação Diplomados/Evadidos"] = (dff["Diplomados"] / evadidos_safe).round(2)
    return dff

df_grouped = calculate_indicators(df_grouped)

def create_diplomados_evadidos_chart(df):
    fig = go.Figure()
    if not df.empty:
        fig.add_trace(go.Bar(x=df['Ano'], y=df['Diplomados'], name='Diplomados', marker=dict(color='#5A4FCF', line=dict(width=0), cornerradius=15), hovertemplate='%{y} diplomados<extra></extra>'))
        fig.add_trace(go.Bar(x=df['Ano'], y=df['Evadidos'], name='Evadidos', marker=dict(color='#FFA07A', line=dict(width=0), cornerradius=15), hovertemplate='%{y} evadidos<extra></extra>'))
    fig.update_layout(title={'text': "<b>Diplomados vs Evadidos por Ano</b>", 'font': {'size': 20, 'color': '#212529'}, 'x': 0.05, 'xanchor': 'left'}, barmode='group', plot_bgcolor='rgba(0,0,0,0)', paper_bgcolor='rgba(0,0,0,0)', font=dict(family="Inter, sans-serif", size=12, color="#6C757D"), margin=dict(l=40, r=20, t=80, b=40), legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1, font={'size': 14}), xaxis=dict(title='Ano', gridcolor='#E9ECEF', tickmode='linear'), yaxis=dict(title='Quantidade', gridcolor='#E9ECEF'), hoverlabel={'font': {'family': "Inter, sans-serif"}})
    return fig

def create_line_chart_indicators(df):
    fig = go.Figure()
    if not df.empty:
        taxas = {
            'Taxa de Preenchimento de Vagas (%)': {'color': '#1E90dd', 'name': 'Taxa de Preenchimento de Vagas'},
            'Taxa de Conclusão de Curso (%)': {'color': '#191970', 'name': 'Taxa de Conclusão de Curso'},
            'Taxa de Evasão (%)': {'color': '#FF6347', 'name': 'Taxa de Evasão'}
        }
        
        for col, settings in taxas.items():
            fig.add_trace(go.Scatter(
                x=df['Ano'],
                y=df[col],
                mode='lines+markers+text',
                name=settings['name'],
                line=dict(color=settings['color'], width=2),
                marker=dict(size=8),
                text=df[col].apply(lambda x: f'{x:.1f}%'.replace('.', ',')),
                textposition='top center',
                textfont=dict(size=10, color=settings['color']),
                hovertemplate='%{y:.1f}%<extra></extra>'
            ))

    fig.update_layout(
        title={'text': "<b>Evolução dos Indicadores por Ano</b>", 'font': {'size': 20, 'color': '#212529'}, 'x': 0.05, 'xanchor': 'left'},
        plot_bgcolor='rgba(0,0,0,0)',
        paper_bgcolor='rgba(0,0,0,0)',
        font=dict(family="Inter, sans-serif", size=12, color="#6C757D"),
        margin=dict(l=40, r=20, t=80, b=40),
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1, font={'size': 14}),
        xaxis=dict(title='Ano', gridcolor='#E9ECEF', tickmode='linear'),
        yaxis=dict(title='Percentual (%)', gridcolor='#E9ECEF', range=[0, df[list(taxas.keys())].max().max() * 1.2 if not df.empty else 110]),
        hoverlabel={'font': {'family': "Inter, sans-serif"}}
    )
    return fig

layout = html.Div(className='dashboard-container', children=[
    html.Div(className='filter-section', children=[
        html.Div(className='filter-control', children=[
            html.Label("Selecione o Ano:"),
            dcc.Dropdown(id='year-dropdown', options=[{'label': 'Geral (Todos os Anos)', 'value': 'geral'}] + [{'label': str(ano), 'value': ano} for ano in sorted(df_grouped['Ano'].unique())], value='geral', clearable=False)
        ]),
        html.Div(className='filter-control', children=[
            html.Label("Selecione o Curso:"),
            dcc.Dropdown(id='course-dropdown', options=[{'label': 'Todos os Cursos', 'value': 'todos'}] + [{'label': curso, 'value': curso} for curso in lista_cursos], value='todos', clearable=False)
        ])
    ]),
    html.Div(id='kpi-cards-row1', className='kpi-card-row'),
    html.Div(id='kpi-cards-row2', className='kpi-card-row'),
    html.Div(className='charts-section', children=[
        html.Div(className='chart', children=[dcc.Graph(id='diplomados-evadidos-chart', config={'displayModeBar': False})]),
        html.Div(className='chart', children=[dcc.Graph(id='indicators-line-chart', config={'displayModeBar': False})])
    ])
])

@dash.callback(
    [Output('kpi-cards-row1', 'children'),
     Output('kpi-cards-row2', 'children'),
     Output('diplomados-evadidos-chart', 'figure'),
     Output('indicators-line-chart', 'figure')],
    [Input('year-dropdown', 'value'),
     Input('course-dropdown', 'value')]
)
def update_dashboard(selected_year, selected_course):
    if selected_course == 'todos':
        dff_by_course = df_grouped.copy()
    else:
        dff_by_course = df_grouped[df_grouped['Curso'] == selected_course].copy()
    
    if selected_year == 'geral':
        dff_cards = dff_by_course.copy()
    else:
        dff_cards = dff_by_course[dff_by_course['Ano'] == selected_year].copy()
    
    if selected_course == 'todos':
        dff_yearly_agg = dff_by_course.groupby('Ano').agg({
            'Vagas': 'sum', 'Ingressantes': 'sum', 'Matriculados': 'sum', 
            'Evadidos': 'sum', 'Diplomados': 'sum'
        }).reset_index()
    else:
        dff_yearly_agg = dff_by_course.copy()
        
    dff_line_bar = calculate_indicators(dff_yearly_agg)

    if selected_year != 'geral':
        dff_bar_filtered = dff_line_bar[dff_line_bar['Ano'] == selected_year]
    else:
        dff_bar_filtered = dff_line_bar

    card_data_agg = dff_cards[['Vagas', 'Ingressantes', 'Matriculados', 'Evadidos', 'Diplomados']].sum()
    
    if card_data_agg.empty or card_data_agg.sum() == 0:
        card_data = pd.Series(0, index=['Vagas', 'Ingressantes', 'Matriculados', 'Evadidos', 'Diplomados', 'Taxa de Preenchimento de Vagas (%)', 'Taxa de Conclusão de Curso (%)', 'Taxa de Evasão (%)', 'Taxa de Retenção (%)', 'Relação Diplomados/Evadidos'])
    else:
        card_data = calculate_indicators(pd.DataFrame([card_data_agg])).iloc[0]

    icon_map = {
        "Vagas": "fas fa-layer-group", "Ingressantes": "fas fa-user-plus",
        "Matriculados": "fas fa-users", "Evadidos": "fas fa-user-minus",
        "Diplomados": "fas fa-user-graduate", "Relação Diplomados/Evadidos": "fas fa-balance-scale",
        "Taxa de Conclusão de Curso (%)": "fas fa-check-circle", "Taxa de Evasão (%)": "fas fa-sign-out-alt",
        "Taxa de Preenchimento de Vagas (%)": "fas fa-percent"
    }
    tooltip_texts = {
        "Vagas": "Total de vagas ofertadas no período selecionado.",
        "Ingressantes": "Total de novos alunos que entraram no curso.",
        "Matriculados": "Total de alunos frequentando o curso (calouros e veteranos).",
        "Evadidos": "Total de alunos que se desvincularam do curso (abandono, transferência, etc.).",
        "Diplomados": "Total de alunos que concluíram o curso e receberam o diploma.",
        "Taxa de Preenchimento de Vagas": "Percentual de vagas que foram preenchidas por novos alunos. (Ingressantes / Vagas)",
        "Taxa de Conclusão de Curso": "Percentual de alunos ingressantes que efetivamente se formaram. (Diplomados / Ingressantes)",
        "Taxa de Evasão": "Percentual de alunos que deixaram o curso em relação ao total de matriculados. (Evadidos / Matriculados)",
        "Diplomados / Evadidos": "Razão entre o número de alunos diplomados e o número de evadidos. Valores > 1 indicam mais sucesso."
    }

    def create_card(title, value, icon_class, card_id, tooltip_text, suffix=""):
        if pd.isna(value) or value == 0:
            formatted_value = "0" if suffix != "%" else "0,0%"
        elif suffix == "%":
            formatted_value = f"{value:.1f}{suffix}".replace(".", ",")
        elif isinstance(value, float):
            formatted_value = f"{value:.2f}".replace(".", ",")
        else:
            formatted_value = f"{int(value):,}".replace(",", ".")
        card_div = html.Div(id=card_id, className='card card-style', children=[
            html.I(className=f"card-icon {icon_class}"),
            html.Div(className='card-text', children=[
                html.H3(formatted_value, className='card-value'),
                html.P(title, className='card-title')
            ])
        ])
        tooltip = dbc.Tooltip(tooltip_text, target=card_id, placement='bottom')
        return card_div, tooltip

    cards1_components = []
    cards_info_row1 = ['Vagas', 'Ingressantes', 'Matriculados', 'Evadidos', 'Diplomados']
    for col in cards_info_row1:
        card, tooltip = create_card(col, card_data[col], icon_map.get(col, "fas fa-question-circle"),
            card_id=f"card-{col.replace(' ', '-')}", tooltip_text=tooltip_texts.get(col, "Sem descrição."))
        cards1_components.extend([card, tooltip])

    cards2_components = []
    card, tooltip = create_card("Taxa de Preenchimento de Vagas", card_data['Taxa de Preenchimento de Vagas (%)'], 
        icon_map['Taxa de Preenchimento de Vagas (%)'], card_id="card-taxa-preenchimento",
        tooltip_text=tooltip_texts["Taxa de Preenchimento de Vagas"], suffix="%")
    cards2_components.extend([card, tooltip])
    card, tooltip = create_card("Taxa de Conclusão de Curso", card_data['Taxa de Conclusão de Curso (%)'], 
        icon_map['Taxa de Conclusão de Curso (%)'], card_id="card-taxa-conclusao",
        tooltip_text=tooltip_texts["Taxa de Conclusão de Curso"], suffix="%")
    cards2_components.extend([card, tooltip])
    card, tooltip = create_card("Taxa de Evasão", card_data['Taxa de Evasão (%)'], icon_map['Taxa de Evasão (%)'], 
        card_id="card-taxa-evasao", tooltip_text=tooltip_texts["Taxa de Evasão"], suffix="%")
    cards2_components.extend([card, tooltip])
    card, tooltip = create_card("Diplomados / Evadidos", card_data['Relação Diplomados/Evadidos'],
        icon_map['Relação Diplomados/Evadidos'], card_id="card-relacao-dipl-evad",
        tooltip_text=tooltip_texts["Diplomados / Evadidos"])
    cards2_components.extend([card, tooltip])
    
    bar_fig = create_diplomados_evadidos_chart(dff_bar_filtered)
    line_fig = create_line_chart_indicators(dff_line_bar) 
    
    return cards1_components, cards2_components, bar_fig, line_fig