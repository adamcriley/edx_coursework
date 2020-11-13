if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(reshaper2)) install.packages("reshaper2", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)
library(lubridate)
library(data.table)
library(reshaper2)
library(ggplot2)

############################################################
# Create edx set, validation set (final hold-out test set)
############################################################

# Run time on my laptop: ~30 minutes
# R version 3.6.1

# Start of course provided material
# Note: This block of information was provided by the course and has not been malipulated or altered

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                            title = as.character(title),
                                            genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
      semi_join(edx, by = "movieId") %>%
      semi_join(edx, by = "userId")

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# End of course provided material

#########################################################################
# Clean provided data, shape it, and split into testing and training sets
#########################################################################

#
# Convert the dates into usable formats. 
#
edx <- edx %>% mutate(
ID = seq.int(nrow(edx)), # Add an index column for reference to split training and testing sets
reviewyear = year(as_datetime(timestamp)), # Take the timestamp data and split it / format it into a year
reviewday = format(as_datetime(timestamp), "%Y-%m-%d"), # Also convert timestamp into a more friendly date format
releaseyear = str_extract(str_extract(title, regex("\\((\\d{4})\\)")), regex("(\\d{4})")), # Extract release year information from move title
reviewdelta = (as.numeric(reviewyear) - as.numeric(releaseyear)) #Finally with both the review year and release year, added a column for the different between the two
)

# Note: # I was extra caustion with the release year conversion and broke it into two steps because I was worried about incorrectly catching 4 digit numbers contained in the movie title itself

# Repeat steps on validation set to prep data in the same way
validation <- validation %>% mutate(
reviewyear = year(as_datetime(timestamp)),
reviewday = format(as_datetime(timestamp), "%Y-%m-%d"),
releaseyear = str_extract(str_extract(title, regex("\\((\\d{4})\\)")), regex("(\\d{4})")),
reviewdelta = (as.numeric(reviewyear) - as.numeric(releaseyear))
)

#
# Extract a complete list of all genre listed
#
movie_genres_temp <- edx %>% separate_rows(genres, sep = "[|]") # Split the genre column on the "pipe" character and re-collected the parsed terms into a huge list.
movie_genres <- unique(movie_genres_temp$genres) # Deduplicate and collect the genre list into a small list
counting <- 1:as.numeric(length(movie_genres)) # Create an index for the genre list

# Take each genre and append it as an individual column joined to the orginal data
# I did this using a for loop and using the counting variable above to walk through the columns
# I then checked to see if that string of genre text for the newly appended column was detected in the original genre column
# If it was, I marked it as true, and if not as false
# The boolean values were treated as numeric and recorded for each movie 
for(i in seq_along(counting)) {
    edx <- add_column(edx, !!movie_genres[i] := as.numeric(str_detect(edx$genres, movie_genres[i])))
    validation <- add_column(validation, !!movie_genres[i] := as.numeric(str_detect(validation$genres, movie_genres[i])))
}

# Split data into training and testing sets
set.seed(61, sample.kind = 'Rounding')
train_index <- createDataPartition(edx$ID, times = 1, p = 0.2, list = FALSE)
edx_train_set <- edx[-train_index,]
temp_test_set <- edx[train_index,]

# Validate the created traing and testing sets and adjust splits
# This step ensures that no movie or user present in the testing set is missing from the training set
edx_test_set <- temp_test_set %>% 
      semi_join(edx_train_set, by = "movieId") %>%
      semi_join(edx_train_set, by = "userId")
removed <- anti_join(temp_test_set, edx_test_set)
edx_train_set <- rbind(edx_train_set, removed)

# Select variables for traing and clean up unused columns
tidy_validation_set <- select(validation, userId, movieId, releaseyear, rating, reviewday, reviewyear, reviewdelta, genres, movie_genres[1:length(movie_genres)])
tidy_test_set <- select(edx_test_set, userId, movieId, releaseyear, rating, reviewday, reviewyear, reviewdelta,  genres, movie_genres[1:length(movie_genres)])
tidy_train_set <- select(edx_train_set, userId, movieId, releaseyear, rating, reviewday, reviewyear, reviewdelta,  genres, movie_genres[1:length(movie_genres)])

# Format cleaned data into tidier format
tidy_validation_set <- tidy_validation_set %>% mutate(userId=as.integer(userId), movieId=as.integer(movieId), releaseyear=as.integer(releaseyear), reviewday=as.Date(reviewday))
tidy_test_set <- tidy_test_set %>% mutate(userId=as.integer(userId), movieId=as.integer(movieId), releaseyear=as.integer(releaseyear), reviewday=as.Date(reviewday))
tidy_train_set <- tidy_train_set %>% mutate(userId=as.integer(userId), movieId=as.integer(movieId), releaseyear=as.integer(releaseyear), reviewday=as.Date(reviewday))

# Remove unused collections, pick up the garbage and taking out the trash
rm(movie_genres_temp, counting, i, edx, train_index, temp_test_set, removed, validation, edx_test_set, edx_train_set)

############################################################
# Train the Data
############################################################

# Define testing function
RMSE <- function(true_ratings, predicted_ratings) {
        sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Baseline model using only the mean (you have to start somewhere)
mu <- mean(tidy_train_set$rating) # Takes ne mean of all the ratings
baseline_rmse <- RMSE(mu,tidy_test_set$rating) # Test the value
rmse_results <- tibble(method = "Training mean", RMSE = baseline_rmse) # Record the results

# Note: I won't add redundant comments, but the process below is very redundant. I'll add comments when I'm doing new things but won't go crazy

# user bias
user_avgs <- tidy_train_set %>% group_by(userId) %>% summarize(b_u = mean(rating - mu)) # Similar to above, now grouping the data by user and taking the average per user
predicted_ratings <- mu + tidy_test_set %>% left_join(user_avgs, by='userId') %>% pull(b_u) # Attach the new averages to matched users for the testing set
baseline_w_user_bias <- RMSE(predicted_ratings, tidy_test_set$rating)
rmse_results <- add_row(rmse_results, method = "User bias", RMSE = baseline_w_user_bias)

# regularized user bias
lambdas <- seq(0, 10, .25) # set up lamdas
sum <- tidy_train_set %>% group_by(userId) %>% summarize(s = sum(rating - mu), n_u = n()) # agin, group by user and record, this time take note of the number of observations for the user as well
rmses <- sapply(lambdas, function(lu){ # Apply regularization function across all lamdas
     predicted_ratings <- tidy_test_set %>%  # Save ratings
          left_join(sum, by='userId') %>%  # attach averages to test set
          mutate(e_u = s/(n_u+lu)) %>% # calculate error
          mutate(pred = mu + e_u) %>% # adjust prediction
          .$pred # select prediction
     return(RMSE(predicted_ratings, tidy_test_set$rating)) # return all saved predictions ans ratings
})
regularized_w_user_bias <- rmses[which.min(rmses)] # selected best value
rmse_results <- add_row(rmse_results, method = "Regularized user bias", RMSE = regularized_w_user_bias)

# movie bias
movie_b_i <- tidy_train_set %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + tidy_test_set %>% left_join(movie_b_i, by='movieId') %>% pull(b_i)
baseline_w_movie_bias <- RMSE(predicted_ratings, tidy_test_set$rating)
rmse_results <- add_row(rmse_results, method = "Movie bias", RMSE = baseline_w_movie_bias)

# regularized movie bias
sum <- tidy_train_set %>% group_by(movieId) %>% summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(lm){
     predicted_ratings <- tidy_test_set %>% 
          left_join(sum, by='movieId') %>% 
          mutate(e_i = s/(n_i+lm)) %>%
          mutate(prediction = mu + e_i) %>%
          .$prediction
     return(RMSE(predicted_ratings, tidy_test_set$rating))
})
regularized_w_movie_bias <- rmses[which.min(rmses)]
rmse_results <- add_row(rmse_results, method = "Regularized movie bias", RMSE = regularized_w_movie_bias)

# Bias from how old the movie is when it was reviewed
date_d_i <- tidy_train_set %>% group_by(reviewdelta) %>% summarize(n_d = mean(rating - mu))
predicted_ratings <- mu + tidy_test_set %>% left_join(date_d_i, by='reviewdelta') %>% pull(n_d)
baseline_w_date_bias <- RMSE(predicted_ratings, tidy_test_set$rating)
rmse_results <- add_row(rmse_results, method = "Review date bias", RMSE = baseline_w_date_bias)

# regularized date bias
sum <- tidy_train_set %>% group_by(reviewdelta) %>% summarize(s = sum(rating - mu), n_d = n())
rmses <- sapply(lambdas, function(ld){
     predicted_ratings <- tidy_test_set %>% 
          left_join(sum, by='reviewdelta') %>% 
          mutate(e_i = s/(n_d+ld)) %>%
          mutate(prediction = mu + e_i) %>%
          .$prediction
     return(RMSE(predicted_ratings, tidy_test_set$rating))
})
regularized_w_date_bias <- rmses[which.min(rmses)]
rmse_results <- add_row(rmse_results, method = "Regularized review date bias", RMSE = regularized_w_date_bias)

# genre bias
genre_g_i <- tidy_train_set %>% group_by(genres) %>% summarize(g_i = mean(rating - mu))
predicted_ratings <- mu + tidy_test_set %>% left_join(genre_g_i, by='genres') %>% pull(g_i)
baseline_w_genre_bias <- RMSE(predicted_ratings, tidy_test_set$rating)
rmse_results <- add_row(rmse_results, method = "Genre bias", RMSE = baseline_w_genre_bias)

# regularized genre bias
sum <- tidy_train_set %>% group_by(genres) %>% summarize(s = sum(rating - mu), n_g = n())
rmses <- sapply(lambdas, function(lg){
     predicted_ratings <- tidy_test_set %>% 
          left_join(sum, by='genres') %>% 
          mutate(e_i = s/(n_g+lg)) %>%
          mutate(prediction = mu + e_i) %>%
          .$prediction
     return(RMSE(predicted_ratings, tidy_test_set$rating))
})
regularized_w_genre_bias <- rmses[which.min(rmses)]
rmse_results <- add_row(rmse_results, method = "Regularized grenre bias", RMSE = regularized_w_genre_bias)

# Bias from how old the movie is
date_r_i <- tidy_train_set %>% group_by(releaseyear) %>% summarize(n_r = mean(rating - mu))
predicted_ratings <- mu + tidy_test_set %>% left_join(date_r_i, by='releaseyear') %>% pull(n_r)
baseline_w_date_bias <- RMSE(predicted_ratings, tidy_test_set$rating)
rmse_results <- add_row(rmse_results, method = "Release date bias", RMSE = baseline_w_date_bias)

# regularized release date bias
sum <- tidy_train_set %>% group_by(releaseyear) %>% summarize(s = sum(rating - mu), r_d = n())
rmses <- sapply(lambdas, function(lr){
     predicted_ratings <- tidy_test_set %>% 
          left_join(sum, by='releaseyear') %>% 
          mutate(e_i = s/(r_d+lr)) %>%
          mutate(prediction = mu + e_i) %>%
          .$prediction
     return(RMSE(predicted_ratings, tidy_test_set$rating))
})
regularized_r_date_bias <- rmses[which.min(rmses)]
rmse_results <- add_row(rmse_results, method = "Regularized release date bias", RMSE = regularized_r_date_bias)

# movie + user bias
movie_b_i <- tidy_train_set %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu)) # this is the same as above
user_movie_avgs <- tidy_train_set %>% left_join(movie_b_i, by='movieId') %>% group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i))  # but now we do it twice for both factors
predicted_ratings <- tidy_test_set %>% left_join(movie_b_i, by='movieId') %>% left_join(user_movie_avgs, by='userId') %>% mutate(pred = mu + b_i + b_u) %>% pull(pred) # and use both factors in the prediction
baseline_w_user_movie_bias <- RMSE(predicted_ratings, tidy_test_set$rating)
rmse_results <- add_row(rmse_results, method = "User + movie bias", RMSE = baseline_w_user_movie_bias)

# regularized movie + user bias
rmses <- sapply(lambdas, function(lnu){
     mu <- mean(tidy_train_set$rating)
     b_i <- tidy_train_set %>% # same as the other combo, it's the same first step
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu)/(n()+lnu)) # we've consolidate this step to take the averages in the Sapply to be more compact instead of saving it separately
     b_u <- tidy_train_set %>%  # we just add another factor and repeat the step above for both
          left_join(b_i, by="movieId") %>%
          group_by(userId) %>%
          summarize(b_u = sum(rating - b_i - mu)/(n()+lnu))
     predicted_ratings <- 
          tidy_test_set %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          mutate(prediction = mu + b_i + b_u) %>% # then use both results in the prediction
          .$prediction
     return(RMSE(predicted_ratings, tidy_test_set$rating))
})
regularized_w_user_movie_bias <- rmses[which.min(rmses)]
rmse_results <- add_row(rmse_results, method = "Regularized user + movie bias", RMSE = regularized_w_user_movie_bias )

# regularized multi bias (all previous factors)
rmses <- sapply(lambdas, function(llm){
     mu <- mean(tidy_train_set$rating)
     b_i <- tidy_train_set %>%
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu)/(n()+llm))
     b_u <- tidy_train_set %>% 
          left_join(b_i, by="movieId") %>%
          group_by(userId) %>%
          summarize(b_u = sum(rating - b_i - mu)/(n()+llm))
     d_i <- tidy_train_set %>%
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          group_by(reviewdelta) %>%
          summarize(d_i = sum(rating - b_u - b_i - mu)/(n()+llm))
     g_i <- tidy_train_set %>%
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(d_i, by = "reviewdelta") %>%
          group_by(genres) %>%
          summarize(g_i = sum(rating - d_i - b_u - b_i - mu)/(n()+llm))
     r_i <- tidy_train_set %>%
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(d_i, by = "reviewdelta") %>%
          left_join(g_i, by = "genres") %>%    
          group_by(releaseyear) %>%
          summarize(r_i = sum(rating - g_i - d_i - b_u - b_i - mu)/(n()+llm))
     predicted_ratings <- 
          tidy_test_set %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(d_i, by = "reviewdelta") %>%
          left_join(g_i, by = "genres") %>%
          left_join(r_i, by = "releaseyear") %>%
          mutate(prediction = mu + b_i + b_u + d_i + g_i + r_i) %>%
          .$prediction
     return(RMSE(predicted_ratings, tidy_test_set$rating))
})
regularized_multi_bias <- rmses[which.min(rmses)]
rmse_results <- add_row(rmse_results, method = "Regularized 5 factor bias", RMSE = regularized_multi_bias)

# validation testing - multi bias above only calculated against the validation set
rmses <- sapply(lambdas, function(llm){
     mu <- mean(tidy_train_set$rating)
     b_i <- tidy_train_set %>%
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu)/(n()+llm))
     b_u <- tidy_train_set %>% 
          left_join(b_i, by="movieId") %>%
          group_by(userId) %>%
          summarize(b_u = sum(rating - b_i - mu)/(n()+llm))
     d_i <- tidy_train_set %>%
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          group_by(reviewdelta) %>%
          summarize(d_i = sum(rating - b_u - b_i - mu)/(n()+llm))
     g_i <- tidy_train_set %>%
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(d_i, by = "reviewdelta") %>%
          group_by(genres) %>%
          summarize(g_i = sum(rating - d_i - b_u - b_i - mu)/(n()+llm))
     r_i <- tidy_train_set %>%
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(d_i, by = "reviewdelta") %>%
          left_join(g_i, by = "genres") %>%    
          group_by(releaseyear) %>%
          summarize(r_i = sum(rating - g_i - d_i - b_u - b_i - mu)/(n()+llm))
     predicted_ratings <- 
          tidy_validation_set %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(d_i, by = "reviewdelta") %>%
          left_join(g_i, by = "genres") %>%
          left_join(r_i, by = "releaseyear") %>%
          mutate(prediction = mu + b_i + b_u + d_i + g_i + r_i) %>%
          .$prediction
     return(RMSE(predicted_ratings, tidy_validation_set$rating))
})
regularized_valid_bias <- rmses[which.min(rmses)]
rmse_results <- add_row(rmse_results, method = "Validation", RMSE = regularized_valid_bias)

as.data.frame(rmse_results)