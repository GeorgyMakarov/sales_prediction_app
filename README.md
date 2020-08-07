# Sales volume prediction Shiny app

The goal of this project is to create a shiny web application that could predict
sales volume of a certain product from historic sales data and forecasted price.
Prediction like this could be useful for marketing campaigns in FMCG companies.
The result of the project is practically implemented in brewing company.

The application works on **ARIMAX** model. **Time series** of the model is the 
mean of weekly sales volumes in retail shops. **External regressor** is the mean 
sales price of a product at a given week. Forecasting period is 52 weeks. The 
application predicts sales volume for each product.

The application is built using `R Shiny` and `forecast` library. The model 
showed **91%** of *RMSE*. It generates new prediction in under **20ms** after
the change in price forecast. Forecast visualization uses `ggplot2` package. 
Visualization shows *4-D* graph with actual and forecasted sales volumes, 
corresponding prices and revenue. Final outputs show sums for forecasted period.

# Project development plan

## Prerequisite libraries

Download prerequisite libraries;  
Setup working directory;  

## Data download and transformation

Download data from .csv;  
Quickly explore the data through `str`, `head`, `dim`;  
Transform data column from character to date format;  
Multiply sales by thousands;  
Delete SalesItem as not relevant to analysis;  
Filter *NA* values from all columns;  
Filter rows with sales = 0;  
Filter rows with volume = 0;  
Compute price as a fraction of sales / volume;  
Reduce map - need: week, shop, sku, sales, price, volume;  
Rename columns for simplicity;  

## Remove outliers from the dataset

Detect outliers with Cook's distance;  
Detect outliers for each feature using IQ range;  
Detect outliers with Mahalanobis distance;  

## Choose top items and shops

Apply Pareto principles for items;  
Apply Pareto principles to shops;  
Group the data by weeks and sku;  
Smooth extreme volumes with k-nearest neighbors mean;  



