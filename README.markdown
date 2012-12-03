# HI SCORE 
[Description](http://bit.ly/MIxhwc)

Thanks for taking an interest in the HI Score!
If you're here, then probably want to experiment with our data and code. While we tried to document our work as much as possible, if you haven't worked with R or any other programming language before, you will most likely have a hard time.

# FILES

In this folder you will find the following files:

* FilmCodes.txt - A simple text file with the id's we used for the films
* HiScoreData.csv - A spreadsheet containing twitter, news, and search data for 14 
* documentary films.
* HiScore.R - Our code to generate HiScores.  This file also contains an overview of the data.
* HiScoreEquation.pdf - The equation we used to generate HiScores.
* InsightsAPI.R - A program to download data from Google Insights
* LexisNexisParser - A Python program to parse text files from LexisNexis.

# SCORES	

How to generate HI Scores:
- If you don't have R, download the current version from: http://cran.r-project.org/
- Open "HiScore.R" and run the code by selecting all and pressing run. This will take the data from HiScoreData.csv and generate scores over time for the 14 films.  
- Note the arguments at the top of the code; these can be used to modify the weights, lags, and scales that make up HI Score (see code for explanation).  
-  This program will also generate a line plot for each film which you can use to assess how modifications affect the score.

For any additional questions, please feel free to email me at brian@harmony-institute.org