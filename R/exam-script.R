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

data_train <- read_csv("data/train.csv", show_col_types = FALSE) |> 
	mutate(date = ymd_h(date)) |> 
	filter(between(date, train_dtt[1], train_dtt[2]))
data_test <- read_csv("data/test.csv", show_col_types = FALSE) |> 
	mutate(date = ymd_h(date)) 

# Compute forecasts
res <- map(
	paste0("wp", 1:7), 
	~ auto_forecast(train = data_train, test = data_test, var_name = .x)
) |> collect_results()
res

write_csv(res$forecast_tbl, "exam/marcozanotti_forecasts.csv")
write_csv(res$evaluation_tbl, "exam/marcozanotti_metrics.csv")


# ts_plot <- read_csv("data/train.csv", show_col_types = FALSE) |> 
# 	mutate(date = ymd_h(date), type = "actual") |>  
# 	bind_rows(
# 		read_csv("exam/marcozanotti_forecasts.csv") |> mutate(id = NULL, type = "forecast")
# 	) |> 
# 	pivot_longer(cols = wp1:wp7) |> 
# 	arrange(name, date)
# 
# ts_plot |>
# 	filter(name == "wp1") |> 
# 	plot_time_series(.date_var = date, .value = value, .color_var = type, .facet_vars = name)

# read_csv("exam/marcozanotti_forecasts.csv") |> 
# 	mutate(
# 		date = as.character(date) |> 
# 			str_remove_all(":00:00") |> 
# 			str_remove_all("-") |> 
# 			str_remove_all("\\s")
# 	) |> 
# 		write_csv("exam/marcozanotti_forecasts.csv") |> 
# 		mutate(across(where(is.numeric), ~ round(.x, 3)))
