import pandas as pd
import plotly as px
from ipywidgets import interact


country_df = pd.read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases_country.csv')
country_df.columns = map(str.lower, country_df.columns)
country_df = country_df.rename(columns={'country_region': 'country'})

sorted_country_df = country_df.sort_values('confirmed', ascending= False)

def bubble_chart(n):
    fig = px.scatter(sorted_country_df.head(n), x="country", y="confirmed", size="confirmed", color="country",
               hover_name="country", size_max=60)
    fig.update_layout(
    title=str(n) +" Worst hit countries",
    xaxis_title="Countries",
    yaxis_title="Confirmed Cases",
    width = 700
    )
    fig.show()
    
interact(bubble_chart, n=10)