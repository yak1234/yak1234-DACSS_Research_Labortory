import pandas as pd
wb_df = pd.read_csv('wb_data.csv')

za_7503_subset = pd.read_csv('ZA7503_subset.csv')

unique_s009 = za_7503_subset['S009'].unique()

filtered_evs2 = za_7503_subset[za_7503_subset['S009'].isin(['DE','DK','ES','FR','HU','IT','NO,''NL' ,'PL','SE'])]
filtered_evs2

# Convert 'S012' column to string data type
filtered_evs2['S012'] = filtered_evs2['S012'].astype(str)

# Extract year from 'S012' column and create new column 'year'
filtered_evs2['year'] = filtered_evs2['S012'].str[:4]

# Drop rows with '-4' and '-5' values in 'S012' column
filtered_evs2 = filtered_evs2[~filtered_evs2['S012'].isin(['-4','-2', '-5'])]
country_dict2 = {'DK': 'Denmark', 'FR': 'France', 'DE': 'Germany', 'HU': 'Hungary', 'IT': 'Italy','NO': 'Norway', 'NL': 'Netherlands', 'PL': 'Poland', 'ES': 'Spain', 'SE': 'Sweden'}

# Use map() function to replace country codes with country names
filtered_evs2['S009'] = filtered_evs2['S009'].map(country_dict2)

filtered_evs2 = filtered_evs2.rename(columns={'S009': 'country'})

# Convert 'year' column in filtered_evs2 dataframe to int64 data type
filtered_evs2['year'] = filtered_evs2['year'].astype('int64')

# Convert 'year' column in merged_gdp_migrant dataframe to int64 data type
wb_df['year'] = wb_df['year'].astype('int64')


merged_wb_df_evs = wb_df.merge(filtered_evs2, on=['country', 'year'], how='left')

import pandas as pd
def get_unique_values_df(data, column_names):
    unique_values = []
    for column in column_names:
        column_unique = data[column].unique().tolist()
        unique_values.append(column_unique)
    return unique_values

unique_values = get_unique_values_df(merged_wb_df_evs, ["country", "year"])

print("Status:Merged Together World Bank and EVS data")
print("Here are the Country and Year Values we will be studying:", unique_values)

print("Last Dataset, Manifesto Project will be merged with World Bank and EVS Data")
import pandas as pd
mp = pd.read_csv("MPDataset_MPDS2022a.csv")
mpd = mp[mp['countryname'].isin(['Denmark', 'France', 'Germany', 'Hungary','Italy', 'Netherlands', 'Norway', 'Poland', 'Spain', 'Sweden'])]

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