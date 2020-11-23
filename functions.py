import pandas as pd
import datetime as dt

def sus_con_rec(status):
    for old, new in zip(['C19_Con', 'C19_Rec', 'C19_Sus', 'NonC19'], ['Confirmed', 'Recovering', 'Suspected', 'Occupied']):
        if old in status:
            return status.replace(old, new)
    return status

def sunburst_data(df: pd.DataFrame, date: str) -> pd.DataFrame:
    # validate date
    try: 
        date_df = dt.datetime.strptime(date, "%Y-%m-%d")
    except ValueError:
        raise ValueError("Incorrect date format supplied. Provide date in form 'YYYY-mm-dd'")
    temp_df = df.copy()[df['date'] == date_df]
    temp_df['indicator'] = temp_df['indicator'].str.replace('CO_', '')
    temp_df['indicator'] = temp_df['indicator'].str.replace('Bed', '')
    temp_df['bed_type'] = temp_df['indicator'].map(lambda x: 'GA' if 'GA' in x else 'IV')
    temp_df['bed_status'] = temp_df['indicator'].apply(lambda x: x.split('_')[0]) + '_' + temp_df['indicator'].apply(lambda x: x.split('_')[1].replace('Spare', 'NonC19'))
    temp_df['indicator'] = temp_df['indicator'].apply(sus_con_rec)
    return temp_df[['bed_type', 'bed_status', 'indicator', 'count']]


def string_datediff(date:str, diff:int) -> str:
    try:
        date_asdate = dt.datetime.strptime(date, "%Y-%m-%d")
    except ValueError:
        raise ValueError("Incorrect date format supplied. Provide date in form 'YYYY-mm-dd'")    
    datediff = date_asdate + dt.timedelta(days = diff)
    return datediff.strftime("%Y-%m-%d")