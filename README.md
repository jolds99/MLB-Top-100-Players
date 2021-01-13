# MLB Top 100 Players 
## Jonathan Olds - Fall 2020 ##

*This honors thesis project acts as an attempt to better understand the mindset of baseball analysts. Specifically, I was curious as to whether analysts exhibit preferences towards conventional baseball statistics or sabermetrics in their evaluation of players. This question was examined using logistic regression models in conjunction with two variable selection methods - principal components analysis and LASSO penalization.*

This repository contains the csv data files relevant to the project, the R and Rmd scripts used in the data organization, exploration and analysis process, and the pdf version of the final paper (Final Report.pdf) and slides (Presentation Slides.pdf) used in my presentation of the thesis project on December 10, 2020. 

## R/Rmd Scripts Descriptions ##
1. DataScraping.R: Includes several functions created to scrape and clean data from Baseball Reference, as well as the code necessary to organize and merge data into one large dataset
2. DataExploration.R: Includes the plots used to facilitate better understanding of the data and explore initial relationships. This script also includes some code that drops unnecessary/duplicate variables from the dataset, based on the examination of the plots and the data. 
3. Logistic Regression & Results.R: Includes all code relevant to conducting principal components analysis and LASSO penalization within the context of logistic regression models, as well as the tabular and graphical results from the models. 
