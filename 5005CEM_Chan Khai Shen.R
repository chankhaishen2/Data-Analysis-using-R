# Question 1
movie <- read.csv("C:/Users/user/Documents/BCSCUN/Data Science/Lab Report/movies.csv")


# Question 2
library(dplyr)
movie %>% head(10)
movie %>% summarize(Total = n())


# Question 3
library(ggplot2)

# Score
ggplot(data = movie, mapping = aes(x = score)) +
  geom_histogram(binwidth = 0.5, na.rm = TRUE)

movie$score = 
  ifelse(is.na(movie$score), 
         ave(movie$score, FUN = function(x) mean(movie$score, na.rm = 'TRUE')), 
         movie$score)

# Budget
ggplot(data = movie, mapping = aes(x = budget)) +
  geom_histogram(binwidth = 25000000, na.rm = TRUE)

movie$budget = 
  ifelse(is.na(movie$budget), 
         ave(movie$budget, FUN = function(x) median(movie$budget, na.rm = 'TRUE')), 
         movie$budget)

# Votes
ggplot(data = movie, mapping = aes(x = votes)) +
  geom_histogram(binwidth = 250000, na.rm = TRUE)

movie$votes = 
  ifelse(is.na(movie$votes), 
         ave(movie$votes, FUN = function(x) median(movie$votes, na.rm = 'TRUE')), 
         movie$votes)

# Gross
ggplot(data = movie, mapping = aes(x = gross)) +
  geom_histogram(binwidth = 250000000, na.rm = TRUE)

movie$gross = 
  ifelse(is.na(movie$gross), 
         ave(movie$gross, FUN = function(x) median(movie$gross, na.rm = 'TRUE')), 
         movie$gross)

# Runtime
ggplot(data = movie, mapping = aes(x = runtime)) +
  geom_histogram(binwidth = 25, na.rm = TRUE)

movie$runtime = 
  ifelse(is.na(movie$runtime), 
         ave(movie$runtime, FUN = function(x) mean(movie$runtime, na.rm = 'TRUE')), 
         movie$runtime)


# Question 4
# Score
min(movie$score)
mean(movie$score)
median(movie$score)
sd(movie$score)

# Budget
min(movie$budget)
mean(movie$budget)
median(movie$budget)
sd(movie$budget)


# Question 5
movie %>% group_by(name, genre) %>% 
  filter(score > mean(score)) %>% 
  summarize(name, genre, score)


# Question 6
library(ggpubr)
ggstripchart(movie, x = "genre", y = "runtime", 
             xlab = "Genre of movie", ylab = "Runtime of movie", 
             title = "Runtime of movie from each genre", 
             color = "genre", add = "max", na.rm = TRUE)


# Question 7
movie$genre = 
  factor(movie$genre, 
         levels = c("Drama", "Adventure", "Action", "Comedy", 
                    "Horror", "Biography", "Crime", "Fantasy", 
                    "Family", "Sci-Fi", "Animation", "Romance", 
                    "Music", "Western", "Thriller", "History", 
                    "Mystery", "Sport", "Musical"), 
         labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))


# Question 8
mosthighlycorrelated <- function(mydataframe, numtoreport)
{
  cormatrix <- cor(mydataframe)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  fm <- as.data.frame(as.table(cormatrix))
  names(fm) <- c("First.Variable", "Second.Variable", "Correlation")
  head(fm[order(abs(fm$Correlation), decreasing=T),], n = numtoreport)
}

movienumeric <- movie %>% 
  summarize(movie$scores, movie$votes, movie$budget, 
            movie$gross, movie$runtime)

mosthighlycorrelated(movienumeric, 1)
