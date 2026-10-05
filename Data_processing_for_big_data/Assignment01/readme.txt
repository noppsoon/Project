# Australian Property Market Analysis with PySpark

An academic assignment analysing approximately **4.85 million NSW property transaction records** using PySpark RDDs, the DataFrame API, and Spark SQL.

## Objective
Practise large-scale data preparation and querying, explore property market patterns, and compare implementations using the DataFrame API and Spark SQL.

## Analysis
- Load property transaction data and council, property purpose, and zoning lookup tables.
- Clean invalid identifiers, transform dates, and inspect RDD partitions.
- Calculate monthly sales counts and identify councils with the most unique houses.
- Standardise land area units and categorise property purposes.
- Identify properties with the largest increase between first and last sale prices.
- Visualise median house prices by season and year.
- Explore associations between property characteristics and sale prices.
- Compare DataFrame and SQL queries grouping transactions by year, price range, and settlement period, including combinations with zero transactions.

## Tools
Python, PySpark, Spark SQL, pandas, Matplotlib, and seaborn.

## Files
- `Assignment 1_Pyspark.ipynb`: Assignment instructions, code, explanations, and saved outputs.

## Running the Notebook
1. Set up Python, Java, PySpark, Jupyter Notebook, pandas, Matplotlib, and seaborn.
2. Place the following input files in a `dataset/` folder beside the notebook:
   - `nsw_property_price.csv`
   - `council.json`
   - `property_purpose.json`
   - `zoning.json`
3. Open the notebook and run the cells in order.

Spark is configured to run locally with four cores and the `Australia/Melbourne` session timezone.

## Status
This notebook preserves the original assignment submission. Code issues identified during review require correction before a complete run from a fresh session can be verified.
