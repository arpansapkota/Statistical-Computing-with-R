library(rvest)
library(dplyr)

#Getting Data from Json File from API
json_data <- jsonlite::fromJSON("https://www.mohfw.gov.in/data/datanew.json")
class(json_data)
head(json_data)

# convert the data into a tibble
covid_data <- as_tibble(json_data)

# separate the state_name column into two columns for state name and union territory name
library(tidyr)
covid_data <- covid_data %>% 
  separate(state_name, into = c("state", "union_territory"), sep = " and ")
head(covid_data)

#Transforming columns to the correct data type
covid_data$sno <- as.integer(covid_data$sno)
covid_data$active <- as.integer(covid_data$active)
covid_data$positive <- as.integer(covid_data$positive)
covid_data$cured <- as.integer(covid_data$cured)
covid_data$death <- as.integer(covid_data$death)
covid_data$new_active <- as.integer(covid_data$new_active)
covid_data$new_positive <- as.integer(covid_data$new_positive)
covid_data$new_cured <- as.integer(covid_data$new_cured)
covid_data$new_death <- as.integer(covid_data$new_death)
covid_data$death_reconsille <- as.integer(covid_data$death_reconsille)
covid_data$total <- as.integer(covid_data$total)
covid_data$actualdeath24hrs<- as.integer(covid_data$actualdeath24hrs)

# Remove unnecessary columns for further analysis
tidy_covid_data <- covid_data %>% 
  select(state, active, cured, death)

head(tidy_covid_data)

# Get this data in R, make it tidy and do the necessary transformation: https://covid19.who.int/table
json_data <- jsonlite::fromJSON("https://covid19.who.int/page-data/index/page-data.json")
class(json_data)
head(json_data)


#Part II
library(tidyverse)

#5. Download the title.ratings.tsv.gz and title.basics.tsv.gz files in your computer from https://datasets.imdbws.com/
#6. Copy these files by creating a new folder "imdb" inside the "documents" folder of your computer and unzip them there

#7. Set the working directory to the "imdb" folder
setwd("/Users/arpan/Desktop/MDS/01 MDS I-I/MDS 503 - Statistical Computing with R/Lab/imdb")

#8. Import the "title.ratings.tsv" file
df_ratings <- read_tsv("title.ratings.tsv")

#9. Check the structure and get summary of all the variables
str(df_ratings)
summary(df_ratings)
#Interpretation: The "df_ratings" data frame has 3 columns - "tconst", "averageRating", and "numVotes". "tconst" is a character variable containing the unique identifier for each title in the dataset. "averageRating" is a numeric variable representing the average rating of the title, and "numVotes" is a numeric variable representing the number of votes the title has received on IMDb. The dataset contains 1,120,239 observations (rows).

#10. Is this dataset "tidy"? Why?
#The dataset is tidy because each observation represents a single row, and each variable has its own column.

#11. Get scatterplot of "averageRating" and "numVotes"
plot(df_ratings$averageRating,df_ratings$numVotes)
#Interpretation: The scatterplot shows a positive relationship between the number of votes and the average rating. Titles with a higher number of votes tend to have higher average ratings. However, there are also some titles with a low number of votes that have high average ratings.

#12. Import the "title.basics.tsv" file 
df_basics <- read_tsv("title.basics.tsv")

#13. Check the structure and get summary of all the variables 
str(df_basics)
summary(df_basics)
#Interpretation: The "df_basics" data frame has 9 columns - "tconst", "titleType", "primaryTitle", "originalTitle", "isAdult", "startYear", "endYear", "runtimeMinutes", and "genres". "tconst" is a character variable containing the unique identifier for each title in the dataset. "titleType" is a categorical variable indicating the type of title (e.g., movie, tv series). "primaryTitle" and "originalTitle" are character variables containing the primary and original titles of the title, respectively. "isAdult" is a binary variable indicating whether the title is classified as "adult" or not. "startYear" and "endYear" are numeric variables indicating the year the title was released or the range of years if the title was a tv series. "runtimeMinutes" is a numeric variable indicating the duration of the title in minutes. "genres" is a character variable indicating the genre(s) of the title. The dataset contains 9770643 observations (rows).
head(df_basics)

#14. Is this dataset "tidy"? Why?
#The "title.basics.tsv" dataset is not tidy because it contains multiple values in the "genres" column separated by commas. This violates the tidy data principles because each cell should only contain a single value.

#15. Join the "df_basics" variables in "df_ratings" using "left_join" function
##df_ratings <- left_join(df_ratings, df_basics[, c("tconst", "startYear", "runtimeMinutes", "genres")], by = "tconst")
df_ratings<- left_join(df_ratings,df_basics)
head(df_ratings)

#16. Get a scatterplot of "runtimeMinutes" and "averageRating" variables in "df_ratings"
plot(df_ratings$runtimeMinutes,df_ratings$averageRating)

#17. Get the frequency table of "genres" variable using the plyr package
library(plyr)
count(df_ratings, "genres")

#18. Create a "by_genres" object using group_by function with df_ratings data and genres variable:
by_genres <- group_by(df_ratings, genres)

#19. Get mean of "runtimeMinutes" variable using summarise function by genres, removing NA in "runtimeMinutes"
str(by_genres)
#non-numric field so making it numeric
by_genres$runtimeMinutes <- as.numeric(by_genres$runtimeMinutes)

by_genres %>%
  summarise(mean_runtime = mean(runtimeMinutes, na.rm = TRUE))

#20. Get mean of "runtimeMinutes" variable using summarise by genres without creating "by_genres" object:
df_ratings$runtimeMinutes <- as.numeric(df_ratings$runtimeMinutes)

df_ratings %>%
  group_by(genres) %>%
  summarise(mean_runtime = mean(runtimeMinutes, na.rm = TRUE))


#21. What is difference between step 19 and step 20? Which one do you prefer? Why? Interpret the result of your choice.
#In step 19, we use the summarise() function with the by_genres object to group the data by genre and calculate the mean runtime for each group. We also include the na.rm = TRUE argument to remove any NA values in the runtimeMinutes variable.
#In step 20, we use the summarise() function directly on the df_ratings dataset to group the data by genre and calculate the mean runtime for each group. We do not need to create a separate by_genres object for this.
#Both approaches give us the same result, which is the mean runtime for each genre. However, step 19 removes NA values in the runtimeMinutes variable, whereas step 20 does not. If there are many NA values in the runtimeMinutes variable, step 19 may be preferred because it ensures that these values do not affect the calculation of the mean.
#In summary, both approaches are valid and give the same result, but the choice between them may depend on the specific characteristics of the dataset, such as the number of NA values in the variable being summarized.


#22. Filter df_ratings data with runtimeMinutes less than 150 minutes
df_ratings_movie150m <- df_ratings %>%
  filter(runtimeMinutes < 150)

#23. Get scatterplot of runtimeMinutes and averageRating variable for df_ratings_movie150m
plot(df_ratings_movie150m$runtimeMinutes,df_ratings_movie150m$averageRating)

#24. Arrange df_ratings_movie150m data in descending order by averageRating
best_worst_movies <- df_ratings_movie150m %>%
  arrange(desc(averageRating))

#25. Show the top 6 and last 6 movies based on the arranged dataset above
# Top 6 movies
head(best_worst_movies, 6)

# Last 6 movies
tail(best_worst_movies, 6)

#26. Get the averageRating of adult movies (isAdult variable) using mutate function and interpret it carefully
df_ratings <- df_ratings %>%
  mutate(adult_rating = ifelse(isAdult == 1, averageRating, NA))

mean(df_ratings$adult_rating, na.rm = TRUE)
#This code first adds a new column "adult_rating" to the "df_ratings" data, which assigns the value of averageRating to adult movies (isAdult == 1) and NA to non-adult movies (isAdult == 0). Then, it calculates the mean of adult_rating while removing any missing values. The result is the average rating of adult movies.
#The result of the code mean(df_ratings$adult_rating, na.rm = TRUE) is 6.338142. This means that the average rating of movies intended for adult audiences (as determined by the "isAdult" variable) is 6.338142. This value falls between the range of 1 and 10, where 1 represents the lowest possible rating and 10 represents the highest possible rating. Therefore, we can interpret this as the average quality of adult movies, as rated by the users in the IMDb dataset.

#27. Divide the "df_ratings_movies150m" into training and testing dataset with 80% and 20% split with slice function
library(dplyr)

set.seed(123)
train <- df_ratings_movie150m %>% slice_sample(n = round(nrow(.) * 0.8))
test <- df_ratings_movie150m %>% slice(-which(row.names(.) %in% row.names(train)))

#28. Get mean, standard deviation, median and interquartile range of averageRating, numVotes and runtimeMinutes variable of training and testing data and interpret them carefully

# Mean, standard deviation, median, and interquartile range of training data
train_summary <- sapply(train[, c("averageRating", "numVotes", "runtimeMinutes")], function(x) c(mean = mean(x, na.rm = TRUE), 
                                                                                                 sd = sd(x, na.rm = TRUE), 
                                                                                                 median = median(x, na.rm = TRUE),
                                                                                                 IQR = IQR(x, na.rm = TRUE)))
print(train_summary)

# Mean, standard deviation, median, and interquartile range of testing data
test_summary <- sapply(test[, c("averageRating", "numVotes", "runtimeMinutes")], function(x) c(mean = mean(x, na.rm = TRUE), 
                                                                                               sd = sd(x, na.rm = TRUE), 
                                                                                               median = median(x, na.rm = TRUE),
                                                                                               IQR = IQR(x, na.rm = TRUE)))
print(test_summary)


#29. Get histogram of averageRating, numVotes and runtimeMinutes variables of training and testing data; compare them and interpret them carefully

# Histograms for the variables in the training dataset
par(mfrow=c(1,3))
hist(train$averageRating, main = "Training - Average Rating")
hist(train$numVotes, main = "Training - Number of Votes")
hist(train$runtimeMinutes, main = "Training - Runtime (minutes)")

# Histograms for the variables in the testing dataset
par(mfrow=c(1,3))
hist(test$averageRating, main = "Testing - Average Rating")
hist(test$numVotes, main = "Testing - Number of Votes")
hist(test$runtimeMinutes, main = "Testing - Runtime (minutes)")

#30. Get boxplot of averageRating, numVotes and runtimeMinutes variables of training and testing data; compare them and interpret them carefully
# Boxplots for training data
par(mfrow = c(1,3))
boxplot(train$averageRating, main = "Training Data - averageRating", ylab = "averageRating")
boxplot(train$numVotes, main = "Training Data - numVotes", ylab = "numVotes")
boxplot(train$runtimeMinutes, main = "Training Data - runtimeMinutes", ylab = "runtimeMinutes")

# Boxplots for testing data
par(mfrow = c(1,3))
boxplot(test$averageRating, main = "Testing Data - averageRating", ylab = "averageRating")
boxplot(test$numVotes, main = "Testing Data - numVotes", ylab = "numVotes")
boxplot(test$runtimeMinutes, main = "Testing Data - runtimeMinutes", ylab = "runtimeMinutes")

