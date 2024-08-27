library(data.table)
library(ggplot2)
library(data.table)
library(gridExtra)
library(MASS)
library(bizdays)
library(dplyr)

source('data_pre_processing_index.R')

################################################################################
# FUNCTIONS & DEFINITIONS
################################################################################

################################################################################
# OPTION SYNTHETIC CONTRACT GENERATION


#' Perform a Cross Join of Two Data Tables
#'
#' Performs a cross join between two data tables, `X` and `Y`. 
#' A cross join returns the Cartesian product of the two tables, i.e., every 
#' combination of rows from `X` and `Y`.
#'
#' @param X data frame or data table
#' @param Y data frame or data table
#' @return data table: representing the cross join of `X` and `Y`. If either 
#' `X` or `Y` is empty, the result is an empty data table with the combined 
#' column names of `X` and `Y`.
crossjoin <- function(X, Y) {
  X <- as.data.table(X)
  Y <- as.data.table(Y)
  if (nrow(X) > 0 && nrow(Y) > 0) {
    return(setkey(copy(X)[, c(k = 1, .SD)], k)[Y[, c(k = 1, .SD)], 
      allow.cartesian = TRUE][, `:=`(k, NULL)])
  } else {
    return(cbind(head(X, 0), head(Y, 0)))
  }
}

#' Generate Base Combinations of Coupon, Recovery, Maturity, and Expiry
#'
#' generates all unique combinations of coupon, recovery, maturity, expiry date 
#'
#' @param opt_data data table: containing option data with columns for `coupon`, `recovery`, `maturity`, and `expiryDate`.
#' @return data table containing all unique combinations of `coupon`, `recovery`, `maturity`, and `expiryDate`.
generate_base_combinations_cpn_rec_mat_exp <- function(opt_data){
  
  # 3 unique {coupon, recovery} combinations
  cpn_rec <- unique(opt_data[, .(coupon, recovery)])
  # unique maturity dates, 3 unique option expiry dates 
  mats <- data.table(maturity = unique(opt_data$maturity))
  exps <- data.table(expiryDate = unique(opt_data$expiryDate))
  # 18 unique {coupon, recovery, maturity, expiry} combinations
  out <- crossjoin(crossjoin(cpn_rec, mats), exps)
  
  return(out)
}

#' Extend Data Table Block with New Values
#'
#' extends a given data table block by creating copies of it with new values 
#' for a specified column, then concatenates these copies into a single table.
#'
#' @param block data table: to be extended.
#' @param extensions vector of values: to be used to extend the data table block. 
#' @param colname character string: specifying the name of the column to be extended with new values.
#' @return data table containing the extended blocks with new values in the specified column.
extend_dt_block <- function(block, extensions, colname){
  
  out <- data.table()
  for (i in 1:length(extensions)){
    new_vals <- rep(extensions[i], nrow(block))
    new_block <- copy(block)
    new_block[, (colname) := new_vals]
    
    if (i == 1){
      out <- new_block
    }else{
      out <- rbind(out, new_block)
    }
  }
  return(out)
}


#' Generate Strikes Based on Log Moneyness
#'
#' randomly choose n strikes between logM = -1 to logM = +1
#' back out the strikes from logM = log(strike /spread)

#' @param spread numeric: representing base spread used to calculate the strike 
#' @param n integer: specifying the number of strike prices to generate.
#' @param seed optional integer: to set the random seed for reproducibility.
#' @return numeric vector: of length `n` containing the generated strike prices.
choose_strikes <- function(spread, n, seed=123){
  set.seed(seed)
  logMs <- runif(n, min = -1, max = 1)
  strikes <- spread * exp(logMs) 
  
  return(strikes)
}

#' Generate Skewed Strikes Based on Log Moneyness
#'
#' generates `n` strike prices with a skewed distribution of log Moneyness 
#' 80% of the log Moneyness values are uniformly distributed between -0.5 and 0.5, 
#' and the remaining 20% are either between -1 and -0.5 or between 0.5 and 2. 
#'
#' @param spread numeric: representing base spread used to calculate the strike 
#' @param n integer: specifying the number of strike prices to generate.
#' @param seed optional integer: to set the random seed for reproducibility.
#' @return numeric vector: of length `n` containing the generated strike prices.
choose_strikes_skewed <- function(spread, n, seed=123) {
  set.seed(seed)
  
  # Generate 80% of the logM values within -0.5 to 0.5
  logMs_80 <- runif(floor(n * 0.8), min = -0.5, max = 0.5)
  
  # Generate the remaining 20% of the logM values within -1 to -0.5 or 0.5 to 2
  logMs_20 <- runif(ceiling(n * 0.2), min = 0, max = 1)
  logMs_20 <- ifelse(logMs_20 < 0.5, 
                     runif(length(logMs_20), min = -1, max = -0.5), 
                     runif(length(logMs_20), min = 0.5, max = 1))
  
  # Combine the two sets of logM values
  logMs <- c(logMs_80, logMs_20)
  # Shuffle the combined logM values to ensure randomness
  logMs <- sample(logMs, n)
  # Calculate the strikes
  strikes <- spread * exp(logMs)
  
  return(strikes)
}


#' Extend Data Table Block with Generated Strikes
#'
#' extends a given data table block by generating a specified number of strikes
#' for each unique spread value in the block. The strikes can be generated with 
#' either a uniform or skewed distribution based on the `skewed` parameter.
#'
#' @param block data table: to be extended.
#' @param colname character string: specifying the name of the column to be extended with new strike values.
#' @param num_strikes integer: specifying the number of strike prices to generate for each unique spread value.
#' @param skewed logical value: indicating whether to use a skewed distribution for generating strikes (`TRUE`) or a uniform distribution (`FALSE`).
#' @return data table containing the extended blocks with new strike values and a column indicating the strike in spread format.
#' @examples
extend_dt_block_strikes <- function(block, colname, num_strikes, skewed){
  
  out <- data.table()
  noisy_spreads <- unique(block$spread)
  
  for (i in 1:length(noisy_spreads)){
    if (skewed){
      strike_selection <- choose_strikes_skewed(noisy_spreads[i], num_strikes, seed = i)
    }else{
      strike_selection <- choose_strikes(noisy_spreads[i], num_strikes)
    }
    new_block <- extend_dt_block(block[spread == noisy_spreads[i]], strike_selection, 'strike')
    new_block[, strike_in_spread_format := strike]
    
    if (i == 1){
      out <- new_block
    }else{
      out <- rbind(out, new_block)
    }
  }
  return(out)
}


#' Generate Option Contracts
#'
#' Generate a Range of option contract specifications
#' Algorithm: 
#' Start with block of 18 combinations of {coupon, recovery, maturity, expiryDate}
#' Multiply by all noisy_ spreads
#' Multiply by all (relevant) strikes per noisy_spread 
#' Multiply by all implied vols
#' Multiply by 2 (payer receiver option types)
#' Multiply by all value dates (relevant for expiry date)
#'
#' @param config list: of configuration parameters. 
#' @param option_data data table: base option data with columns for `coupon`, `recovery`, `maturity`, and `expiryDate`.
#' @param noisy_spreads vector: noisy spread values to extend the option data.
#' @param implied_vols vector: of implied volatility values to extend the option data.
#' @param option_types vector: of option types to extend the option data.
#' @param val_dates vector: of valuation dates to extend the option data.
#' @return data table containing the fully extended option contracts with all combinations of the specified attributes.
generate_option_contracts <- function(config,
                                      option_data,
                                      noisy_spreads,
                                      implied_vols,
                                      option_types,
                                      val_dates){
  
  # generate base combinations of possible {coupon, recovery, maturity, expiry}
  cpn_rec_mat_exp <- generate_base_combinations_cpn_rec_mat_exp(option_data)
  # extend by noisy_spreads 
  out <- extend_dt_block(cpn_rec_mat_exp, noisy_spreads, 'spread')
  # extend by implied vols
  out <- extend_dt_block(out, implied_vols, 'impliedVol')
  # extend by option types 
  out <- extend_dt_block(out, option_types, 'optionType')
  # extend by valuation dates 
  out <- extend_dt_block(out, val_dates, 'val_date')
  out <- out[!(val_date >= expiryDate)] # remove expired contracts
  # extend by strikes 
  out <- extend_dt_block_strikes(out, 'strike', config$NUM_STRIKES, config$LOGM_SKEWED)
  
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
first_order_estimate_opt <- function(df_train, df_test) {
  
  
  # Calculate errors
  df_train[, delta_error := abs(delta - delta_pred)]
  df_train[, theta_error := abs(theta - theta_pred)]
  df_train[, deltastrike_error := abs(deltastrike - deltastrike_pred)]
  df_train[, vega_error := abs(vega - vega_pred)]
  
  # Extract relevant columns as matrices
  train_matrix <- as.matrix(df_train[, .(spread, exp_duration, strike, impliedVol)])
  test_matrix <- as.matrix(df_test[, .(spread, exp_duration, strike, impliedVol)])
  
  # Find the nearest neighbors
  result <- nn2(train_matrix, test_matrix, k = 1)
  
  # Extract the indices of the nearest neighbors
  nearest_indices <- result$nn.idx
  
  # Create a data table to store the results
  results_dt <- cbind(
    df_train[nearest_indices, ],
    df_test[, .(spread_t = spread,
                exp_duration_t = exp_duration,
                mat_duration_t = mat_duration,
                strike_t = strike,
                impliedVol_t = impliedVol,
                mtm_t = mtm,
                mtm_t_pred = mtm_pred)]
  )
  
  ### New metrics to enable the error bar calculation ###
  # KEY:
  # gt = ground truth,
  # nn = neural network,
  # fo = shorthand for fo_gt: first order ground truth gradient (from a training point)
  # foplus = first order ground truth gradient + diff of nn gradient on the same point
  # ds = change in spread between the prediction point and the nearest training point in n dimensions
  # dt = change in exp_duration between the prediction point and the nearest training point in n dimensions
  # dk = change in strike between the prediction point and the nearest training point in n dimensions
  # dv = change in impliedVol between the prediction point and the nearest training point in n dimensions
  # _t refers to the variable being from the test set
  
  results_dt[, ds := spread_t - spread][, dt := exp_duration_t - exp_duration]
  results_dt[, dk := strike_t - strike][, dv := impliedVol_t - impliedVol]
  # fo (ground truth) estimate of price
  results_dt[, mtm_gt_fo := mtm + (delta * ds + theta * dt + deltastrike * dk + vega * dv)]
  # foplus estimate of price
  results_dt[, mtm_gt_foplus := mtm + ((delta + delta_error) * ds +
                                         (theta + theta_error) * dt +
                                         (deltastrike + deltastrike_error) * dk +
                                         (vega + vega_error) * dv)]
  
  # Construct error bar
  results_dt[, error_bar_size := abs(mtm_gt_foplus - mtm_t_pred) + abs(mtm_gt_foplus - mtm_gt_fo)] # 2nd term is to account for linear shift
  results_dt[, mtm_t_pred_up := mtm_t_pred + error_bar_size]
  results_dt[, mtm_t_pred_dn := mtm_t_pred - error_bar_size]
  results_dt[, error_bar_size_gt := abs(mtm_gt_fo - mtm_t)]
  results_dt[, mtm_t_up := mtm_t + error_bar_size_gt]
  results_dt[, mtm_t_dn := mtm_t - error_bar_size_gt]
  
  ### Define the error metrics ###
  # The 'real error' in prediction between the real gt price and the nn prediction
  results_dt[, gt_nn_error := (mtm_t - mtm_t_pred) / mtm_t * 100]
  # Error between gt price and the gt fo estimate of the price
  results_dt[, gt_fo_error := (mtm_gt_fo - mtm_t) / mtm_t * 100]
  # Error between nn prediction and gt fo estimate (gt fo estimate could be used as a floor/cap of mtm_t_pred)
  results_dt[, nn_fo_error := (mtm_gt_fo - mtm_t_pred) / mtm_t_pred * 100]
  
  # Remove rows where ds = dt = dk = dv = 0
  results_dt <- results_dt[!(ds == 0 & dt == 0 & dk == 0 & dv == 0)]
  
  # Check if mtm_t prediction falls within the error bars
  results_dt[, mtm_t_pred_within_nn_error := mtm_t_pred <= mtm_t_pred_up & mtm_t_pred >= mtm_t_pred_dn] # Should be 100% by construction
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
# PROCESS DATA OPTION - main execution function 
################################################################################

#' Process Option Data and Generate Option Contracts
#'
#' Uses raw option data to generate option contracts. The contracts will later be
#' used to calculate 'exact' prices and then used for training a deep learning model
#'
#' @param config list: list of configuration parameters 
#' @param data_path string: path to index market data 
#' @return data table of index contracts 
#' 
process_data_option <-function(config, data_path){
  
  # Load raw option data for spreads as of "2024-05-30" 
  opt_data <- fread(paste0(data_path, 'option_data.csv'))
  
  # Create a calendar excluding weekends and generate all valuation dates
  # Only interested in moving the valuation date over 3 month period for options 
  create.calendar("weekends", weekdays = c("saturday", "sunday"))
  start_date <- config$DATE_START
  end_date <- max(opt_data$expiryDate)
  val_dates <- generate_valuation_dates(start_date, end_date)
  
  # generate noisy spreads over similar range as for index_contracts 
  noisy_spreads <- generate_spread_range(config$SPREAD_START, 
                                         config$SPREAD_END, 
                                         config$SPREAD_INC, noisy=T)
  # generate full range of implied vols 
  # TODO maybe generate on the fly denser to atm in certain min max range 
  implied_vols <- generate_spread_range(config$IVOL_START, 
                                        config$IVOL_END, 
                                        config$IVOL_INC, noisy=T) * 100
  # option_types: 1 = payer, -1 = receiver 
  option_types <- c(1,-1)
  
  # generate option contracts 
  system.time({
    out <- generate_option_contracts(config, 
                                     opt_data,
                                     noisy_spreads,
                                     implied_vols,
                                     option_types,
                                     val_dates)
    
      out <- out %>%
        mutate(logM = log(strike_in_spread_format / spread),
               indexFactor = 1.0,
               indexLoss = 0.0,
               flat_ir = 0.03,
               currency = 'EUR',
               mat_duration = year_frac(val_date, maturity, config$DCC),
               exp_duration = year_frac(val_date, expiryDate, config$DCC),
               smc = spread - (coupon/10000),
               contractId = paste0("CDSO_", row_number()))
      
  })
  
  # remove non-sensical spreads for coupon==100 indices 
  max_feasible_100 <- (0.06  / max(opt_data[coupon==500]$spread)) * max(opt_data[coupon==100]$spread) 
  min_feasible_500 <- 1/(0.06  / max(opt_data[coupon==500]$spread)) * max(opt_data[coupon==500]$spread) 
  out <- out[!(coupon == 100 & spread > max_feasible_100)]
  out <- out[!(coupon == 500 & spread < min_feasible_500)]
  
  out <- out[,.(val_date, contractId, optionType, currency, coupon, recovery, 
                indexFactor, indexLoss, maturity, expiryDate, flat_ir, strike, spread, 
                strike_in_spread_format, smc, logM, impliedVol, exp_duration, mat_duration)]
  
  
  return(out)
}




