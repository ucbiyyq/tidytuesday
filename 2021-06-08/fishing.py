# -*- coding: utf-8 -*-
"""
Created on Mon Jun  7 19:45:30 2021

@author: yangy
"""

#%% gets data
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

#%% 
fishing = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv")

#%% eda
fishing.columns
fishing.info()
fishing["year"].value_counts()
fishing["lake"].value_counts()
fishing["species"].value_counts()
fishing["grand_total"].value_counts()
fishing["comments"].value_counts()
fishing["region"].value_counts()
fishing["values"].value_counts()
fishing[["year", "grand_total", "values"]].describe()
fishing[["year", "grand_total", "values"]].isna().sum()

#%% yearly
# each row is an observation per region, so if we want yearly totals, we need to sum
# note, there are lots of missing values in the data files, but read_csv already replaced them with 0

yearly = fishing.groupby(["year", "lake"]).agg({"grand_total" : "sum", "values" : "sum"}).reset_index()

yearly["grand_total"].hist()
yearly["values"].hist()

colors = {'Erie':'red', 'Huron':'green', 'Ontario':'blue', 'Superior':'yellow', 'Saint Clair':'grey', 'Michigan':'black'}

fig, ax = plt.subplots()
ax.scatter(yearly['grand_total'], yearly['values'], c=yearly['lake'].map(colors))
plt.show()

fig, ax = plt.subplots()
ax.scatter(yearly['year'], yearly['grand_total'], c=yearly['lake'].map(colors))
plt.show()

fig, ax = plt.subplots()
ax.scatter(yearly['year'], yearly['values'], c=yearly['lake'].map(colors))
plt.show()

# TODO figure out how to do facetted plots