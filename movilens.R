### "MovieLens Project"


#Load libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")

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
set.seed(1)
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

##Dimensions


dim(edx)

##Structure

str(edx)

##Genre distribution

percentage <- prop.table(table(edx$genres)) * 100
genre_freq<-(cbind(freq=table(edx$genres), percentage=percentage))
head(genre_freq[order(genre_freq[,2],decreasing=TRUE),],10)

rm(percentage,genere_freq)


##Number of users and movies in the dataset


edx%>%summarize(num_users = n_distinct(userId),num_movies = n_distinct(movieId))


#DATA CLEAN AND TRANSFORMATIONS


sapply(edx,function(x)(sum(is.na(x))))


#Transformation of timestamp from integer to Datetime
edx$timestamp<-as.POSIXct(edx$timestamp,origin = lubridate::origin)

#DATA VISUALIZATION

#Evolution of number of ratings

ttime<-edx%>%group_by(day=as.Date(timestamp))%>%summarise(n = n())
ttime%>%ggplot(aes(x=day,y=n))+
  geom_line( col="darkgreen",
             alpha = .6)+labs(x="Day",y="Number of ratings")+
  ggtitle("Number of ratings per day")+
  theme(plot.title = element_text(hjust = 0.5)) 

rm(ttime) 


#Evolution of number of active users

ttime_user<-edx%>%mutate(year=year(timestamp))%>%select(userId,year)%>%unique()
ttime_user2<-ttime_user%>%group_by(year)%>%summarise(n=n())

ttime_user2%>%ggplot(aes(x=year,y=n))+
  geom_line( col="darkgreen")+
  scale_x_continuous(breaks = unique(ttime_user$year))+
  labs(x="Year",y="Number of active users")+
  ggtitle("Number of active users per year")+
  theme(plot.title = element_text(hjust = 0.5)) 

rm(ttime_user,ttime_user2)

#Histogram rating per user

edx%>%count(userId)%>%ggplot(aes(x=n))+
  geom_histogram( binwidth = 0.1,
                  col="red",
                  fill="green",
                  alpha = .2)+scale_x_log10()+
  ggtitle("Number of ratings per user")+
  theme(plot.title = element_text(hjust = 0.5)) 


#Histogram of ratings per movie

edx%>%count(movieId)%>%ggplot(aes(x=n))+
  geom_histogram( binwidth = 0.15,
                  col="red",
                  fill="green",
                  alpha = .2)+scale_x_log10() +
  ggtitle("Number of ratings per movie")+
  theme(plot.title = element_text(hjust = 0.5)) 

#Histogram ratings

edx%>%ggplot(aes(x=rating))+geom_histogram(center = 0.5,
                                           binwidth=0.5,                         
                                           col="red",
                                           fill="green",
                                           alpha = .2) +
                                           coord_flip()+
                                     labs(x="Stars", y="Number of ratings") +
                                      ggtitle("Number of ratings per star")+
                                      theme(plot.title = element_text(hjust = 0.5)) 



#Boxplot ratings per gender(only genres with mora than 50000 ratings)
edx %>% group_by(genres) %>%
  mutate(n=n()) %>%
  filter(n >= 50000) %>%ungroup() %>%
  ggplot(aes(x=genres,y=rating, color=genres))+
  geom_boxplot(outlier.colour="darkgreen", outlier.shape=1,
               outlier.size=1)+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Genres", y="Stars") +
  ggtitle("Ratings per gender")+
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        plot.title = element_text(hjust = 0.5),legend.position="none")


#Evolution of ratings 

ttime_rating<-edx%>%select(timestamp,rating)%>% mutate(month = round_date(as_datetime(timestamp), unit = "month"))

ttime_rating2<-ttime_rating%>%group_by(month)%>%summarise(media=mean(rating))

ttime_rating2%>%ggplot(aes(x=month,y=media))+
  geom_line( col="darkgreen")+
  labs(x="Month",y="Ratings")+
  ggtitle("Average ratings per Month")+
  theme(plot.title = element_text(hjust = 0.5)) 

rm(ttime_rating,ttime_rating2)


#RESULTS

## Split-out validation dataset

set.seed(7)


validationIndex <- createDataPartition(y = edx$rating, times = 1, p = 0.3, list = FALSE)

testset1 <- edx[validationIndex,]

trainset <- edx[-validationIndex,]

testset <- testset1 %>% 
  semi_join(trainset, by = "movieId") %>%
  semi_join(trainset, by = "userId")

rm(validationIndex)
rm(testset1)

##Users-effect recommendation model
#training

mu <- mean(trainset$rating) 
user_avgs <- trainset %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

#validation

predicted_ratings <- mu + testset %>% 
  left_join(user_avgs, by='userId') %>%
  pull(b_u)

model_user_RMSE <- RMSE(predicted_ratings, testset$rating)


sprintf("User-effect model  RMSE: %f",model_user_RMSE)



## Item-effect recommendation model

#training
mu <- mean(trainset$rating) 
movie_avgs <- trainset %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#validation
predicted_ratings <- mu + testset %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_item_RMSE <- RMSE(predicted_ratings, testset$rating)


sprintf("Item-effect model  RMSE: %f",model_item_RMSE)



## Hybrid-effect recommendation model


#validation

predicted_ratings <- testset %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu +(b_i + b_u)) %>%
  pull(pred)


model_hybrid_RMSE <- RMSE(predicted_ratings, testset$rating)
sprintf("Hybrid model RMSE: %f",model_hybrid_RMSE)

## Hybrid-effect recommendation model with regularization
#Applying cross-validation to calculate the best lambda
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  #training
  mu <- mean(trainset$rating) 
  
  b_i <- trainset %>%  group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- trainset %>% left_join(b_i, by="movieId") %>% group_by(userId) %>%  summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  #validation
  predicted_ratings <- testset %>% left_join(b_i, by = "movieId") %>%left_join(b_u, by = "userId")%>%mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, testset$rating))
  
})

qplot(lambdas, rmses) 


#The best lambda is:


lambda <- lambdas[which.min(rmses)]
sprintf("Lambda: %f",lambda)


#The RMSE for the chosen lambda
model_hybrid_recom_RMSE<-min(rmses)
sprintf("Hybrid model with regularization RMSE: %f",model_hybrid_recom_RMSE)

##  Appliying the model to de Validation dataset


mu <- mean(edx$rating) 


b_i <- edx %>%  group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>%  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

predicted_ratings <- validation %>% left_join(b_i, by = "movieId") %>%left_join(b_u, by = "userId") %>%mutate(pred = mu + b_i + b_u) %>% pull(pred)


RMSE_validation<-RMSE(predicted_ratings, validation$rating)

sprintf("RMSE_validation: %f", RMSE_validation)






