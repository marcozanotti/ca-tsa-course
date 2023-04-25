# Exam Script
 
# Marco Zanotti
# zanottimarco17@gmail.com


# install.packages("tidyverse")
# install.packages("tidymodels")
# install.packages("modeltime")
# install.packages("modeltime.resample")
# install.packages("modeltime.h2o")
# install.packages("timetk")

library(tidyverse)
library(tidymodels)
library(modeltime)
library(modeltime.resample)
library(modeltime.h2o)
library(timetk)
library(h2o)

source("R/utils.R")

Sys.setenv(JAVA_HOME = "/usr/lib/jvm/jdk-17/") # h2o jvm

# Data
train_dtt <- ymd_hms(c("2009-07-01 00:00:00", "2011-01-01 00:00:00"))

data_train <- read_csv("data/train.csv") |> 
	mutate(date = ymd_h(date)) |> 
	filter(between(date, train_dtt[1], train_dtt[2]))
data_test <- read_csv("data/test.csv") |> mutate(date = ymd_h(date)) 

# Compute forecasts
res <- map(
	paste0("wp", 1:7), 
	~ auto_forecast(train = data_train, test = data_test, var_name = .x)
) |> collect_results()
res

write_csv(res$forecast_tbl, "exam/marcozanotti_forecasts.csv")
write_csv(res$evaluation_tbl, "exam/marcozanotti_metrics.csv")

