# Victorian Property Market Analysis in R

An academic project exploring Victorian property transactions through data cleaning, exploratory analysis, text analysis, and basic forecasting.

## Analysis
- Compare monthly transaction counts across selected suburbs.
- Explore description keywords associated with higher property prices using a 10% sample.
- Calculate price–land size correlations by suburb and property type.
- Analyse price changes between first and last recorded sales within five years.
- Compare variability in monthly median prices during 2022.
- Estimate September 2025 median prices for four-bedroom, two-bathroom houses in six suburbs using separate linear regression models.

## Tools
R, R Markdown, tidyverse, tidytext, and ggplot2.

## Files
- `Assignment4_R_programming (1).Rmd`: Analysis code and written explanations.
- Required input: `property_transaction_victoria.csv`.

## Running
Place the CSV beside the R Markdown file, install the packages listed in the script, and open the file in RStudio. Use Knit to generate the HTML report.

## Limitations
Keyword results describe associations rather than causal effects. Price forecasts are exploratory and have not been validated on held-out data. Renovation status and proximity to schools or shopping centres are not included in the models.
