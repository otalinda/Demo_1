#############################################################
                      # PREPPING #
#############################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(plyr)
library(tidyverse)
library(caret)



# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#Inspecting the total number of users and movies in the training data:

stats <- edx %>% summarize(users = n_distinct(userId),
                                 movies = n_distinct(movieId))


#Finding the number of ratings for movies falling within a particular genre (training data):
chars <- "Romance"
vector <- grepl(chars, edx$genres)
length(vector[vector == TRUE])


#Finding which movie has the greatest number of ratings (training data):

frequency <- count(edx, "title") %>% arrange(desc(freq))
head(frequency)

  #OR, the code below also works:

    edx %>% group_by(movieId, title) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    Submit

#Finding the five most given ratings in order of frequency:

frequency <- count(edx, "rating") %>% arrange(desc(freq))
head(frequency)

  #OR, the below code also works:

    edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
      arrange(desc(count))  

#Verifying that half star ratings are generally less common:

edx %>% group_by(rating) %>% summarize(count = dplyr :: n())        
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()


############################################################################
              #Generating the model using the training set
              #Goal: RMSE <= 0.87750
##########################################################################

#1) Defining a function to compute the RMSE:

RMSE <- function(true, predicted){
        sqrt(mean((true - predicted)^2))
  }

#2) Generating the model parameters: mean, regularized movie effect, regularized user effect.
#Using cross validation to optimize/tune lambda (the penalization term applied to the movie and user effects)

lambdas <- seq(0, 10, .25)

results <- sapply(lambdas, function(l){

mu <- mean(edx$rating)  
    
movie_effect <- edx %>%
  group_by(movieId) %>%
  summarize(f1 = sum(rating - mu)/(n()+l))


user_effect <- edx %>%
  left_join(movie_effect, by = "movieId") %>% 
              group_by(userId) %>%
              summarize(f2 = sum(rating - mu - f1)/(n()+l))
            
      
predicted_ratings <- edx %>% 
  left_join(movie_effect, by = "movieId") %>% 
  left_join(user_effect, by = "userId") %>% 
  mutate(prediction = mu + f1 + f2) %>%
  .$prediction


return(RMSE(edx$rating, predicted_ratings))

})

# 3) Making a quick plot of the results and finding the optimal lambda

plot(lambdas, results)

lambdas[which.min(results)] #gives a lambda of 0.5
min(results)

#The final model uses a lambda of 0.5 and yields an RSME of 0.8566952 on the training data.

###########################################################
        #Testing the model on the validation set
##########################################################


mu <- mean(edx$rating)
  
final_movie_effect <- edx %>%
  group_by(movieId) %>%
  summarize(f1 = sum(rating - mu)/(n()+0.5))


final_user_effect <- edx %>%
  left_join(final_movie_effect, by = "movieId") %>% 
  group_by(userId) %>%
  summarize(f2 = sum(rating - mu - f1)/(n()+0.5))


final_predictions <- validation %>% 
  left_join(final_movie_effect, by = "movieId") %>% 
  left_join(final_user_effect, by = "userId") %>% 
  mutate(rating_hat = mu + f1 + f2) %>%
  .$rating_hat

expected_loss <- RMSE(validation$rating, final_predictions)
expected_loss

############################################################
        # Final model gives an RMSE of 0.8652226
############################################################



