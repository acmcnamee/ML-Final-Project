## Final R Code
## Team 4: Tyler Crist, Dave Hogarth, Avery McNamee, and Nasiba Radjabova
rm(list=ls())

## Loading in CSV Files
games <- read.csv('games.csv')
players <- read.csv('players.csv')
plays <- read.csv('plays.csv')

## Evaluating the Data
str(games)
summary(games)

str(players)
summary(players)

str(plays)
summary(plays)

# Combining the Games and Plays csv
nfl <- inner_join(games, plays, by = c("gameId"))

# Removing NA's
nfl <- na.omit(nfl)

# Cleaning up the Data
nfl$offensePlayResult <- as.numeric(nfl$offensePlayResult)
nfl$offenseFormation <- as.factor(nfl$offenseFormation)
nfl$passResult <- as.factor(nfl$passResult)
nfl$yardlineNumber <- as.numeric(nfl$yardlineNumber)


## Creating Train and Test Data: 75% split
trainIndex <- createDataPartition(nfl$offensePlayResult, p = .75, list=FALSE)

train.nfl <- nfl[trainIndex,] 
test.nfl <- nfl[-trainIndex,]

#View(train.nfl)

## Creating Our Model
set.seed(1234)

nfl.model <- lm(offensePlayResult ~ yardlineNumber + yardsToGo + passResult + 
                  epa + offenseFormation, data = train.nfl)
summary(nfl.model)
# Adjusted R-squared: 0.6478 


## Checking Assumptions
# 1) Linearity
plot(offensePlayResult ~ yardlineNumber + yardsToGo + passResult + epa + offenseFormation, data = train.nfl)

# 2) Normality of Errors (residuals)
hist(nfl.model$residuals)
# rightly-skewed
mean(nfl.model$residuals)
# mean close to 0:  1.921563e-16

plot(nfl.model) 
# Graph 1: Residuals appear to follow somewhat of a pattern, particularly in the middle of the dataset
# Graph 2: the right tail of the Q-Q plot is skewed far from the line

# 3) Homoscedasticity
library(lmtest)
bptest(nfl.model)
# p-value < 2.2e-16 -- highly heteroscedastic; indicates problematic outliers (large dataset)


# 4) Multicollinearity
car::vif(nfl.model)
  # no multicollinearity present with a threshold value of 10


## Assessing Training Accuracy
preds.train <- predict(nfl.model, train.nfl)
(rmse <- RMSE(train.nfl$offensePlayResult, preds.train))
# 6.058252
mse <- rmse^2
mse
# 36.70242

## Assessing Testing Accuracy
preds.test <- predict(nfl.model, test.nfl)
(rmse.test <- RMSE(test.nfl$offensePlayResult, preds.test))
# 6.06463
mse.test <- rmse.test^2
mse.test
# 36.77973

