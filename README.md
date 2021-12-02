# Yelp-reviews-analysis
Analysis of Yelp data, business insights, NLP analysis and ML model for stars prediction based on sentiment data.
The output is in form of .html presentation rendered by R markdown.

Data can be found on this URL adress: **https://www.yelp.com/dataset**

1. Firstly, the data has to be downloaded.
2. Script named **read_json** uses spark to parallelly read .json files and convert them to several .csv files. This was done because of shortage of internal computation memory and only one .csv file was used from each dataset.
3. **main_yelp** contains all the analytical coding, NLP analysis and ML models respectively.
4. **yelp_data_analysis** renders the .html presentation, using images from previous script.
