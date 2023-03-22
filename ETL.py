import glob
from IPython.display import display
import pandas as pd
from datetime import datetime

#df = pd.read_json('C:\\Users\\hp\\Desktop\\ETL\\a.json')
#display(df)
#
#
#
#
columns            = ['Name','Market Cap (US$ Billion)']
market_cap_file    = 'C:\\Users\\hp\\Desktop\\ETL\\a.json'
exchange_rate_file = 'C:\\Users\\hp\\Desktop\\ETL\\c.csv'
load_to_file       = 'C:\\Users\\hp\\Desktop\\ETL\\d.csv'


def extract(filename):
     extracted_data = pd.read_json(filename) 
     df = pd.DataFrame(extracted_data, columns=columns)
     return df

rates = pd.read_csv(exchange_rate_file, index_col=0)
exchange_rate = rates.at['GBP', 'Rates']
#print(exchange_rate) 0.7323984208000001

def transform(extracted_df, exchange_rate):
    transformed_df = extracted_df.rename(columns={'Market Cap (US$ Billion)': 'Dollar'})
    transformed_df['Dollar'] = round(transformed_df.Dollar * exchange_rate, 3)
    transformed_df = transformed_df.rename(columns={'Dollar': 'Market Cap (GBP$ Billion)'})
    
    return transformed_df

def load(df_to_load, filename):
    df_to_load.to_csv(filename)



x=extract(market_cap_file)
display(x)
y=transform(x,exchange_rate)
display(y)
load(y, load_to_file)
