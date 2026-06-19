# default_series
default series


# -*- coding: utf-8 -*-
"""
Created on Wed Dec 24 13:35:24 2025

"""

echo "# default_series" >> README.md
git init
git add README.md
git commit -m "first commit"
git branch -M main
git remote add origin https://github.com/alexisdalmeida/default_series.git
git push -u origin main

import numpy as np
import pandas as pd
from datetime import datetime
from scipy.interpolate import interp1d


#Import files
path1 = "C:/Users/alexis.dalmeida/Documents/Python/Historical_credit_risk/data/Bank4"


file = "/Perf_Corp_Bank4.csv"
temp = path1 + file
data = pd.read_csv(temp)

# Get headers as a list

headers = list(data.columns)

# Convert to date type and sort by dates
data['Reporting date (DD/MM/YYYY)']  = pd.to_datetime(data['Reporting date (DD/MM/YYYY)'] ,format = "%d/%m/%Y" )
data = data.sort_values(by='Reporting date (DD/MM/YYYY)')

# Create a vector of dates
dates_vect = data['Reporting date (DD/MM/YYYY)']
dates_vect = pd.to_datetime(dates_vect,format = "%d/%m/%Y" )
dates_vect.sort_values()
dates_unique = dates_vect.unique()
segment_vect = data['Portfolio Segmentation used by the Bank at Reporting Date'].unique()


# ===========  Segment analysis =========

#==  Clean the data- replace NaN by zero

data['Government Ownership (<50%, >=50%) (if Public Sector)'] = data[
     'Government Ownership (<50%, >=50%) (if Public Sector)'].replace(np.nan, 0)

data['Total Outstanding Balance at Reporting Date (for Non-Revolving facilities only)']= data[
     'Total Outstanding Balance at Reporting Date (for Non-Revolving facilities only)'].replace(np.nan, 0)

data['Public Sector or Government-related entities indicator ']= data[
     'Public Sector or Government-related entities indicator '].replace(np.nan, 0)

data['Government Ownership (<50%, >=50%) (if Public Sector)']= data[
     'Government Ownership (<50%, >=50%) (if Public Sector)'].replace(np.nan, 0)


# == Time series of segment - Customer count 
segments_pivot1 =  pd.pivot_table(data, 
                   values='CustomerID', 
                   index= 'Reporting date (DD/MM/YYYY)', 
                   columns = ['Portfolio Segmentation used by the Bank at Reporting Date', 
                              'CRE vs. Non-CRE indicator',
                              ], 
                   aggfunc= 'count',    
                   fill_value=0)  
segments_pivot1.to_clipboard()

# == Time series of segment - Exposure

segments_pivot2 =   pd.pivot_table(data, 
                   values='Total Outstanding Balance at Reporting Date (for Non-Revolving facilities only)', 
                   index= 'Reporting date (DD/MM/YYYY)', 
                   columns = ['Portfolio Segmentation used by the Bank at Reporting Date', 
                              'CRE vs. Non-CRE indicator'], 
                   aggfunc= lambda x: x.sum() / 1_000_000,    
                   fill_value=0)  
segments_pivot2.to_clipboard()


# === Segment pivots as of Dec 2023 - exposure
data_cut_date = data[ data['Reporting date (DD/MM/YYYY)']== '2023-12-31']

# = Segments x CRE
segments_pivot3 =   pd.pivot_table(data_cut_date, 
                   values='Total Outstanding Balance at Reporting Date (for Non-Revolving facilities only)', 
                   index= 'CRE vs. Non-CRE indicator', 
                   columns = 'Portfolio Segmentation used by the Bank at Reporting Date', 
                   aggfunc= lambda x: x.sum() / 1_000_000,   
                   fill_value=0)  

segments_pivot3.to_clipboard()

# = Segments x Gov
segments_pivot4 =   pd.pivot_table(data_cut_date, 
                   values='Total Outstanding Balance at Reporting Date (for Non-Revolving facilities only)', 
                   index= 'Public Sector or Government-related entities indicator ', 
                   columns = 'Portfolio Segmentation used by the Bank at Reporting Date', 
                   aggfunc= lambda x: x.sum() / 1_000_000,    
                   fill_value=0) 
segments_pivot4.to_clipboard() 

# = Segments x Gov ownership
segments_pivot5 =   pd.pivot_table(data_cut_date, 
                   values='Total Outstanding Balance at Reporting Date (for Non-Revolving facilities only)', 
                   index= 'Government Ownership (<50%, >=50%) (if Public Sector)', 
                   columns = 'Portfolio Segmentation used by the Bank at Reporting Date', 
                   aggfunc= lambda x: x.sum() / 1_000_000,    
                   fill_value=0) 
segments_pivot5.to_clipboard()

# = Segments x Industry
segments_pivot6 =   pd.pivot_table(data_cut_date, 
                   values='Total Outstanding Balance at Reporting Date (for Non-Revolving facilities only)', 
                   index= 'Industry', 
                   columns = 'Portfolio Segmentation used by the Bank at Reporting Date', 
                   aggfunc= lambda x: x.sum() / 1_000_000,    
                   fill_value=0) 
segments_pivot6.to_clipboard()



# ==== Choose the type of defaults ================
# =============  Work with original default data ==========

# select a given segment only
selected_segment = 'LARGE CORPORATE'
data_sub = data[data['Portfolio Segmentation used by the Bank at Reporting Date']== selected_segment ]
# get total list of clients
customer_vect_unique = data_sub['CustomerID'].unique()
type(customer_vect_unique)
customer_total=len(customer_vect_unique)
dates_unique
# = Create a default flag based on DPD
data['DPD derived default'] =  (data['Maximum Current Delinquency status in DPD at Reporting Date '] >89).astype(int)



# == Option 1 - use the original default flag
# Create a matrix (clients x dates) to be populated with defaults flags
df_def = pd.DataFrame(None, index=customer_vect_unique, columns=dates_unique)

# populate the matrix with 0 and 1
for i in range(0,len(dates_unique)-1):
    # cut data at a given date
    data_cut_date = data_sub[ (data_sub['Reporting date (DD/MM/YYYY)']== dates_unique[i])]
    # create a small df just with ID and default
    a = data_cut_date['CustomerID']
    b = data_cut_date['Default Flag at Reporting Date (if any of the facilities is in default, then Customer should be tagged as 1, else 0)']
    df_default_flag = pd.DataFrame({'CustomerID': a, 'Def_flag': b})
    # populate the matrix with the default at a given date
    x= df_default_flag['CustomerID']
    df_def.loc[ x, dates_unique[i] ] = df_default_flag[ 'Def_flag'].values

print(df_def)


# == Option 2 - use the synthetic defaults based on DPD
# Create a matrix (clients x dates) to be populated with defaults flags
df_def = pd.DataFrame(None, index=customer_vect_unique, columns=dates_unique)

# populate the matrix with 0 and 1
for i in range(0,len(dates_unique)-1):
    # cut data at a given date
    data_cut_date = data_sub[ (data_sub['Reporting date (DD/MM/YYYY)']== dates_unique[i])]
    # create a small df just with ID and default
    a = data_cut_date['CustomerID']
    b = data_cut_date['DPD derived default'] #===== NEW default flag
    df_default_flag = pd.DataFrame({'CustomerID': a, 'Def_flag': b})
    # populate the matrix with the default at a given date
    x= df_default_flag['CustomerID']
    df_def.loc[ x, dates_unique[i] ] = df_default_flag[ 'Def_flag'].values

print(df_def)


# === Adjust the defaultflags to retain the FIRST default event only

# Duplicate the data frame. Create a adjusted df. 
df_def_adj = df_def.copy()
N = len(dates_unique)-1 # used as position reference for the end of time steps
# 
nCure = 12 #number of cure months assumed
# loop through the matrix
for iCust in range(0,customer_total-1):
        for jDate in range(0,N-1): # stop at N-1 because of the forward loop
            
        # loop forward over the cure, in case of default
            if df_def.iloc[iCust,jDate ] == 1: #current step in default
                
                for k in range(1,min(nCure, N-jDate ) +1):  
                    if df_def.iloc[ iCust,jDate+k ]  == 1:  # following dates in default
                        df_def_adj.iloc[ iCust,jDate+k ]  = 0

# check with one client
jDate = 0
iCust = 4
df_def_adj.iloc[ iCust, ].values

# == export the default matrices - original and adjusted
df_def_out = df_def.reset_index() #move the index to the first col
df_def_out.to_clipboard()

df_def_adj_out = df_def.reset_index() #move the index to the first col
df_def_adj_out.to_clipboard()

#path2 = "C:/Users/alexis.dalmeida/Documents/Python/Historical_credit_risk/output"
#file = "/Perf_Corp_out.csv"
#temp = path2 + file
#data = df_def.to_csv(temp, index=False)

#path2 = "C:/Users/alexis.dalmeida/Documents/Python/Historical_credit_risk/output"
#file = "/Perf_Corp_out_2.csv"
#temp = path2 + file
#data = df_def_adj.to_csv(temp, index=False)


# ====== Create a time series of default by using the matrices above

# first, get the exposure
for i in range(0,len(dates_unique)):
    # Subset of the data - surrent date
    data_sub = data[ (data['Reporting date (DD/MM/YYYY)']== dates_unique[i]) &
                     (data['Portfolio Segmentation used by the Bank at Reporting Date']== 'LARGE CORPORATE') ]

    exposure_total = np.sum( data_sub['Total Outstanding Balance at Reporting Date (for Non-Revolving facilities only)'])
 
    # create df with the sums
    data_temp = {
        'date': [dates_unique[i]],
        'exposure_total': [exposure_total],
    }
    df_temp = pd.DataFrame(data_temp)

    # concatenate
    if i == 0:   
        df_exp = df_temp
    else:
        df_exp = pd.concat([df_exp, df_temp], axis=0)

# Align the index
df_exp.index = dates_unique


# == compute default rates
df_def_adj.sum(axis=0)
a = (df_def_adj == 0).sum(axis=0)
b = (df_def_adj == 1).sum(axis=0)
c = df_def_adj.isna().sum(axis=0)
# Check
a+b+c

# ======= prepare a df
data_temp = {
    'Defaults': b,
    'Performing': a,
    'Customers': a+b,
    'Total Exposure': df_exp['exposure_total'].values,
}
df_series = pd.DataFrame(data_temp)
print(df_series)

# == export
df_series = df_series.reset_index() #move the index to the first col
df_series.to_clipboard()



# =======================================================================




