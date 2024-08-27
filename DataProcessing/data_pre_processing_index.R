library(data.table)
library(ggplot2)
library(data.table)
library(gridExtra)
library(MASS)
library(bizdays)
library(dplyr)
library(RANN)



################################################################################
# FUNCTIONS & DEFINITIONS
################################################################################

################################################################################
# INDEX SYNTHETIC CONTRACT GENERATION

#' Generate a Range of Noisy Spreads: 
#'
#' a sequence of spreads from the specified start_bps to end_bps in basis points
#' with a increment gap_bps, adds Gaussian noise to the sequence, and converts 
#' the spreads in bps to percentages.
#'
#' @param start_bps numeric value: starting spread in basis points.
#' @param end_bps numeric value: ending spread in basis points.
#' @param gap_bps numeric value: gap between consecutive spread increments 
#' @return numeric vector: noisy spreads converted from bips to percentage.
#' 
generate_spread_range <- function(start_bps, end_bps, gap_bps, noisy=T) {
  all_spreads <- seq(start_bps, end_bps, gap_bps)
  
  if (noisy){
    set.seed(123)
    # Generate Gaussian noise
    noise <- rnorm(length(all_spreads), mean = 0, sd = 1)
    # Scale the noise to be within [0, 1]
    noise <- (noise - min(noise)) / (max(noise) - min(noise))
    # Add the noise to the sequence
    noisy_spreads <- all_spreads + noise
    out <- noisy_spreads
  }else{
    out <- all_spreads
  }
  # convert spread from bips to percentage
  return(out / 10000)
}

#' Generate Valuation Dates Excluding Weekends
#'
#' generates a sequence of valuation dates from the specified start date to the 
#' end date, with a specified interval in business days, excluding weekends.
#'
#' @param start_date character string or Date object: start date.
#' @param end_date character string or Date object: end date.
#' @param interval integer: interval in business days. Defaults to 5.
#' @return vector of Date objects representing the valuation dates.
#' 
generate_valuation_dates <- function(start_date, end_date, interval = 5) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  out <- out <- start_date
  current_date <- start_date
  
  # Iterate until the end date is reached or surpassed
  while (current_date < end_date) {
    # Advance by interval business days
    current_date <- bizdays::add.bizdays(current_date, interval, "weekends")
    # Append the next date to the result if it's before or on the end date
    if (current_date <= end_date) {
      out <- c(out, current_date)
    }
  }
  
  return(out)
}

#' Generate a Range of index contract specifications
#' Algorithm: 
#' For each valuation date
#'   For each unique {coupon, recovery}
#'     generate spreads from start_bps to end_bps in increments of gap_bps
#'     create a data table block of contracts for {val_date, coupon, recovery}
#'     append to previous block for previous val_date
#'
#' Each contract includes valuation date, currency, coupon, recovery, spread, and maturity.
#'
#' @param index_data data table: should include columns for `coupon`, `recovery`, `maturity`.
#' @param noisy_spreads numeric vector: noisy spreads.
#' @param all_val_dates character vector: valuation dates.
#' @return data table containing the generated index contracts.
#' 
generate_index_contracts <- function(index_data, noisy_spreads, all_val_dates) {
  unique_cpn_rec <- unique(index_data[, .(coupon, recovery)])
  max_maturity <- max(index_data$maturity)
  result_list <- list()
  
  # increment valuation date
  for (d in 1:length(all_val_dates)) {
    val_date <- all_val_dates[d]
    
    for (i in 1:nrow(unique_cpn_rec)) {
      cpns <- rep(unique_cpn_rec$coupon[i], length(noisy_spreads))
      recs <- rep(unique_cpn_rec$recovery[i], length(noisy_spreads))
      
      dt <- data.table(
        val_date = val_date,
        currency = 'EUR', # for now
        coupon = cpns,
        recovery = recs,
        spread = noisy_spreads,
        maturity = max_maturity
      )
      result_list[[length(result_list) + 1]] <- dt
    }
  }
  
  out <- rbindlist(result_list)
  return(out)
}


#' Calculate Year Fraction Between Two Dates
#'
#' calculates year fraction between two dates for a given day count convention.
#'
#' @param start_date character string: or Date object: start date.
#' @param end_date character string: or Date object: end date.
#' @param dcc character string: day count convention. Defaults to "ACT/360".
#' @return numeric value representing the year fraction between the two dates.
year_frac <- function(start_date, end_date, dcc = "ACT/360") {
  # Convert the dcc to uppercase
  dcc <- toupper(dcc)
  # Calculate the difference in days between the end_date and start_date
  num <- as.numeric(difftime(as.Date(end_date), as.Date(start_date), units = "days"))
  # Determine the denominator based on the day count convention
  if (dcc == "ACT/360") {
    denom <- 360.0
  } else if (dcc == "ACT/365") {
    denom <- 365.0
  } else {
    stop("Unsupported day count convention")
  }
  
  return(num / denom)
}


#' Calculate First Order Predictions
#'
#' Calculate first-order predictions for a given data frame
#' Uses the cs01 and theta to predict the next price 
#'
#' @param df data frame: contains: `val_date`, `coupon`, `recovery`, `spread`, `mat_duration`, `mtm`, `cs01`, and `theta`.
#' @return data table with additional columns `ds`, `dt`, `mtm_fo_`, and `mtm_fo`. (fo = first_order)
first_order_predictions <- function(df){
  out <- df %>%
    arrange(coupon, recovery, mat_duration, spread) %>%
    group_by(coupon, recovery) %>%
    mutate(
      ds = lead(spread) - spread,
      dt = lead(mat_duration) - mat_duration,
      mtm_fo_ = mtm + (cs01 * ds + theta * dt),
      mtm_fo = lag(mtm_fo_)
    ) %>% 
    as.data.table() 
  
  out <- na.omit(out)
  return(out)
}


################################################################################
# FIRST ORDER ERROR BAR ESTIMATION : 
# relies on lookup logic for matching unseen test data with closest training points 
################################################################################

# Taking a test data set, find the nerest neighbours in the training set and 
# create a combined data frame. The gradients and training labels in the 
# training set can be used to give a first order estimation of what the test 
# label should be in order to compare with the NN prediction 

# Eaxample 
# df_train <- idx_priced_fd
# df_test <- df_train[sample(nrow(df_train), 200000), ]
# df_test[, spread := spread + 0.0001][, mat_duration := mat_duration + 2/360]
# result <- find_nearest_neighbors(df_train, df_test)

#' @param df_train data frame: of training data 
#' @param df_test data frame: of test data 
first_order_estimate <- function(df_train, df_test) {
  
  # Ensure necessary libraries are loaded
  library(data.table)
  library(RANN)
  
  # Calculate errors
  df_train[, cs01_error := abs(cs01 - cs01_pred)]
  df_train[, theta_error := abs(theta - theta_pred)]
  
  # Extract relevant columns as matrices
  train_matrix <- as.matrix(df_train[, .(spread, mat_duration, coupon, recovery)])
  test_matrix <- as.matrix(df_test[, .(spread, mat_duration, coupon, recovery)])
  
  # Find the nearest neighbors
  result <- nn2(train_matrix, test_matrix, k = 1)
  
  # Extract the indices of the nearest neighbors
  nearest_indices <- result$nn.idx
  
  # Create a data table to store the results
  results_dt <- cbind(
    df_train[nearest_indices, ],
    df_test[, .(spread_t = spread,
                mat_duration_t = mat_duration,
                coupon_t = coupon,
                recovery_t = recovery,
                mtm_t = mtm,
                mtm_t_pred = mtm_pred)]
  )
  
  ### New metrics to enable the error bar calculation ###
  results_dt[, ds := spread_t - spread][, dt := mat_duration_t - mat_duration]
  
  # fo (ground truth) estimate of price
  results_dt[, mtm_gt_fo := mtm + (cs01 * ds + theta * dt)]
  
  # foplus estimate of price
  results_dt[, mtm_gt_foplus := mtm + ((cs01 + cs01_error) * ds + (theta - theta_error) * dt)]
  
  # Construct error bar
  results_dt[, error_bar_size := abs(mtm_gt_foplus - mtm_t_pred) + abs(mtm_gt_foplus - mtm_gt_fo)]
  results_dt[, mtm_t_pred_up := mtm_t_pred + error_bar_size]
  results_dt[, mtm_t_pred_dn := mtm_t_pred - error_bar_size]
  results_dt[, error_bar_size_gt := abs(mtm_gt_fo - mtm_t)]
  results_dt[, mtm_t_up := mtm_t + error_bar_size_gt]
  results_dt[, mtm_t_dn := mtm_t - error_bar_size_gt]
  
  ### Define some error metrics for info only ###
  results_dt[, gt_nn_error := (mtm_t - mtm_t_pred) / mtm_t * 100]
  results_dt[, gt_fo_error := (mtm_gt_fo - mtm_t) / mtm_t * 100]
  results_dt[, nn_fo_error := (mtm_gt_fo - mtm_t_pred) / mtm_t_pred * 100]
  
  # Check if mtm_t prediction falls within the error bars
  results_dt[, mtm_t_pred_within_nn_error := mtm_t_pred <= mtm_t_pred_up & mtm_t_pred >= mtm_t_pred_dn]
  results_dt[, mtm_t_within_nn_error := mtm_t <= mtm_t_pred_up & mtm_t >= mtm_t_pred_dn]
  results_dt[, mtm_t_pred_within_gt_error := mtm_t_pred <= mtm_t_up & mtm_t_pred >= mtm_t_dn]
  results_dt[, mtm_t_within_gt_error := mtm_t <= mtm_t_up & mtm_t >= mtm_t_dn]
  
  # How close to the error bar are the points that are not within the error bar
  results_dt[mtm_t_within_nn_error == F, true_misses := pmin(abs(mtm_t - mtm_t_pred_up), abs(mtm_t - mtm_t_pred_dn)) / (2*error_bar_size) * 100]
  
  # Size of error bars as a percentage of the prediction price
  results_dt[, error_bar_percent_of_price := (2*error_bar_size) / abs(mtm_t_pred) * 100]
  results_dt[, rel_size := abs(mtm_t_pred_up - mtm_t_pred_dn) / abs(mtm_t_up - mtm_t_dn)]
  
  # Return the results data table
  return(results_dt)
}



################################################################################
# PROCESS DATA INDEX - main execution function 
################################################################################

#' Process Index Data and Generate Index Contracts
#'
#' Uses raw index data to generate index contracts. The contracts will later be
#' used to calculate 'exact' prices and then used for training a deep learning model
#' @param config list: list of configuration parameters 
#' @param data_path string: path to index market data 
#'
#' @return data table of index contracts 
#' 
process_data_index <-function(config, data_path){
  
  # Load raw index data for spreads as of "2024-05-30" 
  index_data <- fread(paste0(data_path, 'index_data.csv'))
  # view index with largest spread
  #index_data[order(-spread)][1:10] 
  
  # Load 7D historic returns (2008-2024) for iTraxx Europe Crossover -1 10Y
  # This is the index with largest spread as of "2024-05-30" 
  xo_10y_minus_1 <- fread(paste0(data_path, 'xo_otr_minus_1.csv'))
  
  # Fit Student-t distribution to 7D returns 
  suppressWarnings(
    fit <- fitdistr(xo_10y_minus_1$return7D, "t")
  )
  print(fit)
  
  df <- fit$estimate["df"] # degrees of freedom 
  mean <- fit$estimate["m"]
  sd <- fit$estimate["s"]
  
  # Generate index contracts for training deep learning model 
  
  # largest 1 in 30Y 7D spread return (= 99.987% quantile)
  q_99987_30Y <- qt(1 - config$Q30Y, df = df, lower.tail = FALSE) * sd + mean
  relative_return_1_30Y <- exp(q_99987_30Y)  - 1
  largest_spread_1_in_30Y <- max(index_data$spread) * ( 1 + relative_return_1_30Y)
  print(paste0('Largest likely 1 in 30Y spread (bps) over 7D returns: ', round(largest_spread_1_in_30Y * 10000,0)))
  
  # Create a calendar excluding weekends and generate all valuation dates 
  create.calendar("weekends", weekdays = c("saturday", "sunday"))
  start_date <- config$DATE_START
  end_date <- max(index_data$maturity)
  all_val_dates <- generate_valuation_dates(start_date, end_date)
  #print(all_val_dates)
  
  system.time({
    noisy_spreads <- generate_spread_range(config$SPREAD_START, 
                                           config$SPREAD_END, 
                                           config$SPREAD_INC)
    out <- generate_index_contracts(index_data, noisy_spreads, all_val_dates)
    
    out <- out %>%
      mutate(val_date = as.Date(val_date),
             maturity = as.Date(maturity),
             indexFactor = 1.0,
             flat_ir = 0.03,
             mat_duration = year_frac(val_date, maturity, config$DCC),
             smc = spread - (coupon/10000),
             contractId = paste0("CDS_", row_number()))
    })
  
  # remove non-sensical spreads for coupon==100 indices 
  max_feasible_100 <- (0.06  / max(index_data[coupon==500]$spread)) * max(index_data[coupon==100]$spread) 
  out <- out[!(coupon == 100 & spread > max_feasible_100)]
  
  out <- out[,.(val_date, contractId, currency, coupon, recovery, maturity, 
                indexFactor, flat_ir, spread, smc, mat_duration)]
  return(out)
}



