from flask import Flask, jsonify
from dash import Dash, html, dcc, Output, Input
import pandas as pd
import plotly.express as px
import datetime as dt
import psycopg2
import statsmodels.api as sm

#flask
server = Flask(__name__)

def get_data_from_db():
    conn = psycopg2.connect(
        dbname="sales_data",
        user="postgres",
        password="Meowmeow2025*",
        host="localhost",
        port="5432"
    )
    df = pd.read_sql("SELECT * FROM sales;", conn)
    conn.close()
    return df

@server.route("/data")
def serve_data():
    df = get_data_from_db()
    return jsonify(df.to_dict(orient="records"))

#dash
app = Dash(__name__, server=server, url_base_pathname="/dashboard/")

# Minimal layout, dashboard content will be populated by callback
app.layout = html.Div([
    html.H1("Sales Dashboard", style={'textAlign': 'center'}),
    html.Div(id="dashboard-content"),
    dcc.Interval(id="load-trigger", n_intervals=0, max_intervals=1)  # fire once after load
])


@app.callback(
    Output("dashboard-content", "children"),
    Input("load-trigger", "n_intervals")
)
def load_dashboard(_):
    import requests
    try:
        api_data = requests.get("http://127.0.0.1:5051/data").json()
        df = pd.DataFrame(api_data)
    except Exception as e:
        return html.Div([
            html.H3("Error fetching API data"),
            html.Pre(str(e))
        ])

    #cleaning
    df["salesdate"] = pd.to_datetime(df["salesdate"], errors="coerce")
    df["discount"] = pd.to_numeric(df["discount"], errors="coerce")
    df["itemssold"] = pd.to_numeric(df["itemssold"], errors="coerce")
    df["freeship"] = df["freeship"].fillna(0).astype(int)

    df_clean = df.dropna(subset=["discount", "freeship", "itemssold"])
    if len(df_clean) < 5:
        return html.Div([
            html.H3("Not enough data to run regression"),
            html.P(f"Rows available after cleaning: {len(df_clean)}")
        ])

    # summary
    summary = {
        "total_items_sold": int(df["itemssold"].sum()),
        "avg_discount": round(df["discount"].mean(), 3),
        "free_ship_pct": round(df["freeship"].mean() * 100, 2)
    }

    # regression
    X = sm.add_constant(df_clean[["discount", "freeship"]])
    y = df_clean["itemssold"]
    model = sm.OLS(y, X).fit()
    df["predicted"] = model.predict(sm.add_constant(df[["discount", "freeship"]]))

    reg = {
        "intercept": round(model.params.get("const", 0), 4),
        "coef_discount": round(model.params.get("discount", 0), 4),
        "coef_freeship": round(model.params.get("freeship", 0), 4),
        "p_discount": round(model.pvalues.get("discount", 1.0), 4),
        "p_freeship": round(model.pvalues.get("freeship", 1.0), 4),
        "r2": round(model.rsquared, 4)
    }

    # charts
    fig_product = px.bar(
        df.groupby("productid")["itemssold"].sum().reset_index(),
        x="productid", y="itemssold",
        title="Items Sold per Product"
    )

    fig_region = px.bar(
        df.groupby("region")["itemssold"].sum().reset_index(),
        x="region", y="itemssold",
        title="Items Sold per Region"
    )

    fig_discount = px.scatter(
        df, x="discount", y="itemssold", color="freeship",
        opacity=0.4, title="Discount vs Items Sold (Regression Line)"
    )
    fig_discount.add_traces(
        px.line(df.sort_values("discount"), x="discount", y="predicted").data
    )

    heatmap_data = df.groupby(["region", "productid"])["itemssold"].sum().reset_index()
    fig_heatmap = px.density_heatmap(
        heatmap_data, x="productid", y="region", z="itemssold",
        title="Heatmap of Items Sold by Product and Region"
    )

    # dash layout
    return html.Div([

        # KPI cards
        html.Div([
            html.Div([
                html.H4("Total Items Sold"),
                html.P(summary["total_items_sold"])
            ], style={'display': 'inline-block', 'width': '30%', 'textAlign': 'center'}),

            html.Div([
                html.H4("Average Discount"),
                html.P(summary["avg_discount"])
            ], style={'display': 'inline-block', 'width': '30%', 'textAlign': 'center'}),

            html.Div([
                html.H4("% Free Shipping Orders"),
                html.P(f"{summary['free_ship_pct']}%")
            ], style={'display': 'inline-block', 'width': '30%', 'textAlign': 'center'}),
        ], style={'marginBottom': 50}),

        # Regression block
        html.H2("Regression Analysis", style={'textAlign': 'center'}),
        html.Div([
            html.Div([
                html.H4("Intercept"),
                html.P(reg["intercept"])
            ], style={'width': '25%', 'display': 'inline-block', 'textAlign': 'center'}),

            html.Div([
                html.H4("Discount Coefficient"),
                html.P(f"{reg['coef_discount']} (p={reg['p_discount']})")
            ], style={'width': '25%', 'display': 'inline-block', 'textAlign': 'center'}),

            html.Div([
                html.H4("Free Shipping Coefficient"),
                html.P(f"{reg['coef_freeship']} (p={reg['p_freeship']})")
            ], style={'width': '25%', 'display': 'inline-block', 'textAlign': 'center'}),

            html.Div([
                html.H4("R²"),
                html.P(reg["r2"])
            ], style={'width': '25%', 'display': 'inline-block', 'textAlign': 'center'}),
        ], style={'marginBottom': 50}),

        # Charts
        html.Div([
            html.Div(dcc.Graph(figure=fig_product), style={'width': '48%', 'display': 'inline-block'}),
            html.Div(dcc.Graph(figure=fig_region), style={'width': '48%', 'display': 'inline-block'})
        ]),

        html.Div([
            html.Div(dcc.Graph(figure=fig_discount), style={'width': '48%', 'display': 'inline-block'}),
            html.Div(dcc.Graph(figure=fig_heatmap), style={'width': '48%', 'display': 'inline-block'})
        ])
    ])

if __name__ == "__main__":
    app.run(debug=True, port=5051)
