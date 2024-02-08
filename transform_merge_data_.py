
import pandas as pd
mp = pd.read_csv("MPDataset_MPDS2022a.csv")
merged_wb_df_evs= pd.read_csv("wb_data.csv")
mpd = mp[mp['countryname'].isin(['Denmark', 'France', 'Germany', 'Hungary','Italy', 'Netherlands', 'Norway', 'Poland', 'Spain', 'Sweden'])]

def get_unique_values_df(df, columns):
    """
    This function returns unique combinations of values for the specified columns in a DataFrame.
    
    Parameters:
    - df: pandas.DataFrame, the DataFrame to analyze.
    - columns: list of str, the column names to find unique combinations of values.
    
    Returns:
    - pandas.DataFrame with unique combinations of values for the specified columns.
    """
    # Drop duplicates based on the specified columns to get unique combinations
    unique_values_df = df.drop_duplicates(subset=columns)
    
    # Sort the DataFrame for better readability, if desired
    unique_values_df = unique_values_df.sort_values(by=columns).reset_index(drop=True)
    
    return unique_values_df


import numpy as np
import math
mpd.dropna(subset=['coderyear'], inplace=True)
mpd['coderyear'] = mpd['coderyear'].astype(int)
merged_wb_df_evs.rename(columns={'year': 'coderyear', 'country': 'countryname'}, inplace=True)
merged_mpd = merged_wb_df_evs.merge(mpd, on=['coderyear', 'countryname'], how='left')
print("Status: Merged Manifesto Project with World Bank and EVS Data")
mp_year_countries = get_unique_values_df(merged_mpd, ["countryname", "coderyear"])
print("These are the countries and years in the final merged_dataframe",mp_year_countries)

merged_mpd.to_csv('merged_mpd.csv', index=False)
print('Saving Final Merged Dataframe to csv: merged_mpd.csv')