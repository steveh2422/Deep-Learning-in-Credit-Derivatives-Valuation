################################################################################
#  main entry point 
#  SYNTHETIC CONTRACT GENERATION 

# Instructions: 
# - set working directory to the location of this code file 
# - source the file to exectur all code and generate contracts 
# - un-comment write.csv code lines if you wish to save updates  

source('plotting.R')
source('data_pre_processing_index.R')
source('data_pre_processing_option.R')

################################################################################
# INDEX SYNTHETIC CONTRACT GENERATION

idx_config <- list(
  SPREAD_START = 5,
  SPREAD_END = 600,
  SPREAD_INC = 0.5,
  DATE_START = '2024-05-30',
  Q30Y = 0.99987,
  DCC = "ACT/360"
)

opt_config <- list(
  SPREAD_START = 50,
  SPREAD_END = 600,
  SPREAD_INC = 2,
  IVOL_START = 10,
  IVOL_END = 200,
  IVOL_INC = 20,
  NUM_STRIKES = 10,
  DATE_START = '2024-05-30', # Date of collected real market data 
  LOGM_SKEWED = T,
  DCC = "ACT/360"
) 



# Process the raw data and generate index contracts for training 
data_path <- './../../data/'
index_contracts <- process_data_index(idx_config, data_path)
index_contracts
#write.csv(index_contracts, './../../data/index_contracts_dense.csv', row.names = F)

################################################################################
# OPTION SYNTHETIC CONTRACT GENERATION

opt_contracts <- process_data_option(opt_config, data_path)
opt_contracts
#write.csv(opt_contracts, './../../data/option_contracts_dense.csv', row.names = F)



################################################################################
# DATA EXPLORATION

### INDEX ######################################################################
idx_data <- fread(paste0(getwd(), '/../../data/index_data.csv'))
idx_priced <- fread(paste0(getwd(), '/../../data/index_priced.csv'))
idx_priced_fd <- fread(paste0(getwd(), '/../../data/index_priced_fd.csv'))
idx_priced_dense <- fread(paste0(getwd(), '/../../data/index_priced_dense.csv'))
idx_contracts <- fread(paste0(getwd(), '/../../data/index_contracts.csv'))
idx_contracts_dense <- fread(paste0(getwd(), '/../../data/index_contracts_dense.csv'))
data_path <- paste0(getwd(), '/../../data/index_priced.csv')
grad <- plot_mtm_vs_duration_increment(data_path)


### OPTION ######################################################################
opt_data <- fread(paste0(getwd(), '/../../data/option_data.csv'))
opt_priced <- fread(paste0(getwd(), '/../../data/option_priced.csv'))
opt_priced_fd <- fread(paste0(getwd(), '/../../data/option_priced_fd.csv'))
opt_priced_dense <- fread(paste0(getwd(), '/../../data/option_priced_dense.csv'))
opt_priced_dense_fd <- fread(paste0(getwd(), '/../../data/option_priced_dense_fd.csv'))
opt_contracts <- fread(paste0(getwd(), '/../../data/option_contracts.csv'))
opt_contracts_dense <- fread(paste0(getwd(), '/../../data/option_contracts_dense.csv'))


### ERROR BAR ANALYSIS #########################################################
# INDEX
pytrain <-  fread(paste0(getwd(), '/../../data/py_train_data.csv'))
pytrue <-  fread(paste0(getwd(), '/../../data/py_true_data.csv'))
pyfo <- first_order_estimate(pytrain, pytrue)
nrow(pyfo[mtm_t_within_nn_error == T])/ nrow(pyfo)
nrow(pyfo[mtm_t_pred_within_nn_error == T])/ nrow(pyfo)
# OPTION
pytrainopt <-  fread(paste0(getwd(), '/../../data/py_opt_train_data.csv'))
pytrueopt <-  fread(paste0(getwd(), '/../../data/py_opt_true_data.csv'))
pyfoopt <- first_order_estimate_opt(pytrainopt, pytrueopt)
nrow(pyfoopt[mtm_t_within_nn_error == T])/ nrow(pyfoopt)
nrow(pyfoopt[mtm_t_pred_within_nn_error == T])/ nrow(pyfoopt)


