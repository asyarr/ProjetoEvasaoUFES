import dash
from dash import html
import dash_bootstrap_components as dbc

font_awesome_stylesheet = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"
app = dash.Dash(__name__, use_pages=True, external_stylesheets=[dbc.themes.BOOTSTRAP, font_awesome_stylesheet])
server = app.server

header = dbc.NavbarSimple(
    children=[
        dbc.Nav(
            [
                dbc.NavLink(page["name"], href=page["relative_path"], className="nav-link-custom")
                for page in dash.page_registry.values()
            ],
        ),
    ],
    brand="Análise de Evasão",
    brand_href="/",
    color="white",
    dark=False,
    className="mb-4"
)

app.layout = html.Div([
    header,

    html.Div(
        className="content-container",
        children=[
            dash.page_container
        ]
    )
])

if __name__ == '__main__':
    app.run(debug=True)