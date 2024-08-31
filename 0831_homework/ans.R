# Import library
library(readxl)

# Set working directory in R-terminal first
setwd("/Users/alex/Documents/Github/rPlayground/0831_homework")

# Load the dataset
movie_data <- read_excel("R summer workshop - movie.xlsx")

print(movie_data)

# nolint start
# Now before you can start analysing the data you of course need to clean the data. Your first assignment is to figure out what to do with missing data.
# Find your missing data and figure out if it is better to delete the movie with missing data or if there is a way for you to figure out what the missing value could be.
# You are not yet done cleaning your data. It is also important to correct wrong data in the data set.
# Not all mistakes in the data are easy to find, but sometimes you have data that is obviously wrong, like a movie budget of 1.
# To investigate your data and find some mistakes you could use the function summary(), make a histogram, or barplots.
# After you have found all your mistakes it is important to decide what to do with them.
# Write down 3 different ways you can deal with data that seems incorrect.
# Decide for yourself which of these 3 methods you think is best for your data.
# It is also important to make sure that the data in the same column uses the same unit, so for example that a movie budget is always in dollars or euros.
# It is not always easy to check if this is the case but if you look at the histogram of vote_average, then you see something weird.
# What do you think happened here and how can you fix it?


# Now that you have cleaned the data to the best of your abilities, it is time to start the analysis.
# In our analysis we are only interested in movies that have your favourite genre.
# So pick your favourite genre and remove all the movies that do not have this genre or make a new dataset that only has movies with this genre.
# First calculate the average run time of the movies (the movies that have your favourite genre).
# Then plot a histogram of movie ratings.
# Next identify the top 5 highest-grossing movies.
# Lastly calculate and visualise the distribution of movie languages.
# nolint end

# Hints and coding tips
# Use the following code to change the release dates from a character data type to a time data type.
data$release_date <- as.Date(data$release_date, format = "%Y-%m-%d")

# To make a histogram of column ‘X’ from data frame ‘data’ the following code could be used.
hist(data$X)

# To change a character type like ‘2023-08-28’ to a data type of date can be done with the following code.
as.Date("2023-08-28")

# To look at the frequency of a character type data it might be good to look at a barplot and the function table().
