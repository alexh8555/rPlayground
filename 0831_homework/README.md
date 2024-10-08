# Q1
Case for R Summer School (RSM), on Rstudio
Turing Students Rotterdam
In this case we are going to do some basic data analysis on a movie data set so that we can have some insight into the trends of popular movies. This dataset contains information on the budget, genre, language, popularity, production company, release date, revenue, vote average and vote count of the movie according to top 5000 movies according to imdb.

You have received the dataset in your email, it is important to first upload this into your coding environment. This can be done in Rstudio by using the following code;
install.packages('readxl')
library(readxl)
## Set working directory
setwd("/Users/ajoonibedi/Desktop/R workshop data")
      ## fill in your own folder name between the quotation marks (where you saved the excel file)
      ## You can also go to the menu bar at the top: Session > Set Working Directory > Choose Directory


## Load the dataset
Movie_Data <- read_excel("R summer workshop - movie.xlsx")

Now before you can start analysing the data you of course need to clean the data. Your first assignment is to figure out what to do with missing data. Find your missing data and figure out if it is better to delete the movie with missing data or if there is a way for you to figure out what the missing value could be.

You are not yet done cleaning your data. It is also important to correct wrong data in the data set. Not all mistakes in the data are easy to find, but sometimes you have data that is obviously wrong, like a movie budget of 1. To investigate your data and find some mistakes you could use the function summary(), make a histogram, or barplots.

After you have found all your mistakes it is important to decide what to do with them. Write down 3 different ways you can deal with data that seems incorrect. Decide for yourself which of these 3 methods you think is best for your data.

It is also important to make sure that the data in the same column uses the same unit, so for example that a movie budget is always in dollars or euros. It is not always easy to check if this is the case but if you look at the histogram of vote_average, then you see something weird. What do you think happened here and how can you fix it?




Now that you have cleaned the data to the best of your abilities, it is time to start the analysis. In our analysis we are only interested in movies that have your favourite genre. So pick your favourite genre and remove all the movies that do not have this genre or make a new dataset that only has movies with this genre.

First calculate the average run time of the movies (the movies that have your favourite genre). Then plot a histogram of movie ratings. Next identify the top 5 highest-grossing movies. Lastly calculate and visualise the distribution of movie languages.


Hints and coding tips
Use the following code to change the release dates from a character data type to a time data type.        data$release_date <- as.Date(data$release_date, format = "%Y-%m-%d")
To make a histogram of column ‘X’ from data frame ‘data’ the following code could be used.           hist(data$X)
To change a character type like ‘2023-08-28’ to a data type of date can be done with the following code.       as.Date("2023-08-28")
To look at the frequency of a character type data it might be good to look at a barplot and the function table().