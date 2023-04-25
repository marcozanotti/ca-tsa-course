# function to prepare windforcast dataset
clean_windfor_dataset <- function(data) {
	
	# function to convert a vector to a 4 column vector
	convert_to_4colums <- function(x) {
		
		if (length(x) == 0) {
			res <- c(NA_real_, NA_real_, NA_real_, NA_real_)
		} else if (length(x) == 1) {
			res <- c(x, NA_real_, NA_real_, NA_real_)
		} else if (length(x) == 2) {
			res <- c(x, NA_real_, NA_real_)
		} else if (length(x) == 3) {
			res <- c(x, NA_real_)
		} else if (length(x) == 4) {
			res <- x
		} else {
			stop("Length of x is greater than 4")
		}
		
		return(t(res))
		
	}
	
	data <- data |> 
		mutate(date = ymd_h(date)) |>
		mutate(dtt = date + hours(hors), .after = date) |> 
		arrange(date, dtt)
	
	vrs <- c(rep("u", 4), rep("v", 4), rep("ws", 4), rep("wd", 4))
	dates <- unique(data$dtt)
	
	res <- matrix(data = NA_real_, nrow = length(dates), ncol = length(vrs) + 1) |> 
		as_tibble() |> 
		set_names(c("date", paste0(vrs, 1:4)))
	res$date <- dates
	
	for (d in dates) {
		res[res$date == d, 2:ncol(res)] <-
			t(c(
				data[data$dtt == d,]$u |> convert_to_4colums(),
				data[data$dtt == d,]$v |> convert_to_4colums(),
				data[data$dtt == d,]$ws |> convert_to_4colums(),
				data[data$dtt == d,]$wd |> convert_to_4colums()
			))
	}
	
	idx_first_na <- which(rowSums(is.na(res[37:nrow(res), ])) > 0)[1] + 36
	for (i in idx_first_na:nrow(res)) {
		res[i, -1] <- t(unlist(c(
			rev(res[i, 2:5]), 
			rev(res[i, 6:9]), 
			rev(res[i, 10:13]), 
			rev(res[i, 14:17])
		)))
	}
	
	return(res)
	
}

# function to compute forecasts
auto_forecast <- function(train, test, var_name) {
	
	h2o.init() # initialize h2o vm
	
	data_prep_tbl <- train |> 
		select(date, all_of(var_name)) |> 
		rename("value" = var_name)
	# out-of-sample dataset
	forecast_tbl <- test |> select(date) 
	# xregs dataset
	data_xregs <- read_csv(
		paste0("data/windforecasts_wf", grep("\\d", var_name) ,".csv"), 
		show_col_types = FALSE
	) |> 
		clean_windfor_dataset()
	data_prep_tbl <- data_prep_tbl |> left_join(data_xregs, by = "date") 
	forecast_tbl <- forecast_tbl |> left_join(data_xregs, by = "date")
	
	# * Train / Test Sets 
	splits <- time_series_split(
		data_prep_tbl, 
		date_var = date,
		assess = "3 month", 
		cumulative = TRUE
	)
	
	# * Recipe - Feature Engineering
	# calendar features + holidays + 5 period fourier series + xregs
	rcp_spec <- recipe(value ~ ., data = training(splits)) |> 
		step_timeseries_signature(date) |>
		step_rm(matches("(iso)|(xts)|(year)|(half)|(quarter)|(qday)|(hour12)|(minute)|(second)")) |>
		step_normalize(matches("(index.num)|(yday)")) |> 
		step_holiday_signature(date, locale_set = "World", exchange_set = "none") |> 
		step_rm(matches("(_US_)|(_CA_)|(_GB_)|(_FR_)|(_IT_)|(_JP_)|(_CH_)|(_DE_)")) |> 
		step_fourier(date, period = c(7, 14, 30, 90, 365), K = 2)
	
	# * Engine
	model_spec_h2o <- automl_reg(mode = 'regression') |>
		set_engine(
			engine = "h2o",
			max_runtime_secs = 30,
			max_runtime_secs_per_model = 30,
			project_name = "ca_exam",
			max_models = 1000,
			nfolds = 10,
			sort_metric = "rmse",
			verbosity = NULL
		)
	
	# * Fitting
	wrkfl_fit_h2o <- workflow() |>
		add_model(model_spec_h2o) |>
		add_recipe(rcp_spec) |>
		fit(training(splits))
	
	# * Evaluating
	calibration_tbl <- modeltime_table(wrkfl_fit_h2o) |>
		modeltime_calibrate(testing(splits)) 
	eval_tbl <- modeltime_accuracy(calibration_tbl) |> 
		select(.model_desc, rmse) |> 
		rename("model" = ".model_desc")
	
	# * Refitting
	refit_tbl <- calibration_tbl |> modeltime_refit(data = data_prep_tbl)
	
	# * Forecasting out-of-sample
	res_tbl <- refit_tbl |> 
		modeltime_forecast(new_data = forecast_tbl, actual_data = data_prep_tbl) |> 
		filter(.model_desc != "ACTUAL") |> 
		select(.index, .value) |> 
		mutate(
			.value = ifelse(.value < 0, 0, .value), 
			.value = ifelse(.value > 1, 1, .value)
		) |> 
		set_names(c("date", var_name))
	
	res <- list(
		"forecast_tbl" = left_join(test, res_tbl, by = "date") |> arrange(date),
		"evaluation_tbl" = eval_tbl
	)
	
	h2o.shutdown(prompt = FALSE) # close h2o vm
	Sys.sleep(30)
	
	return(res)
	
}

# function to collect results of different wp forecasts
collect_results <- function(res_list) {
	
	forecast_tbl <- res_list[[1]]$forecast_tbl |> 
		select(1, 2) |> 
		bind_cols(
			map(res_list, "forecast_tbl") |> map(~ select(.x, 3)) |> bind_cols()
		)
	
	evaluation_tbl <- map(res_list, "evaluation_tbl") |> 
		bind_rows() |>
		mutate(var = paste0("wp", 1:7), .before = 1)
	
	res <- list(
		"forecast_tbl" = forecast_tbl,
		"evaluation_tbl" = evaluation_tbl
	)
	return(res)
	
}