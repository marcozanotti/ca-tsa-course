# Exam Script
 
# Marco Zanotti
# zanottimarco17@gmail.com



# Install & Load ----------------------------------------------------------

# install.packages("tidyverse")
# install.packages("modeltime")
# install.packages("modeltime.resample")
# install.packages("modeltime.ensemble")
# install.packages("timetk")

library(tidyverse)
library(modeltime)
library(modeltime.resample)
library(modeltime.ensemble)
library(timetk)



# Import data -------------------------------------------------------------

data_train <- read_csv("data/train.csv")
str(data_train)
data_train <- data_train |> mutate(date = ymd_h(date)) # convert date to dttm format

data_test <- read_csv("data/test.csv")
str(data_test)
data_test <- data_test |> mutate(date = ymd_h(date)) # convert date to dttm format



# Explorative analysis ----------------------------------------------------












