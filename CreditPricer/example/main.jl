## FILE: main.jl 

# DESCRIPTION: 
# Main entry point for pricing CDS INdex and CDS Option contracts  


using CreditPricer
using DataFrames
using CSV 

################################################################################
### PRICE INDEX CONTRACTS ######################################################
###

# load index contracts
data_path = "./../../data/"
index_contracts = load_contracts(_index_, data_path, "index_contracts.csv")
index_contracts_dense = load_contracts(_index_, data_path, "index_contracts_dense.csv")
# Price all index contracts 
residx_dd = run_measure(_mtm_, _index_, index_contracts)
residx_dense_dd = run_measure(_mtm_, _index_, index_contracts_dense)
#CSV.write(data_path * "index_priced.csv", residx_dd[:mtm_index])
#CSV.write(data_path * "index_priced_dense.csv", residx_dense_dd[:mtm_index])

### PRICE INDEX CS01 ###########################################################
residxcs01_dd = run_measure(_cs01_, _index_, index_contracts)
residxcs01_dense_dd = run_measure(_cs01_, _index_, index_contracts_dense)

### PRICE INDEX THETA  #########################################################
residxtheta_dd = run_measure(_theta_, _index_, index_contracts)
residxtheta_dense_dd = run_measure(_theta_, _index_, index_contracts_dense)

### COMBINE MTM, CS01, THETA   #################################################
dfmtm = residx_dd[:mtm_index]
dfcs01 = select(residxcs01_dd[:cs01_index], :contractId, :cs01)
dftheta = select(residxtheta_dd[:theta_index], :contractId, :theta)
dfcomb = leftjoin(leftjoin(dfmtm, dfcs01, on=:contractId), dftheta, on=:contractId)
#CSV.write(data_path * "index_priced_fd.csv", dfcomb)  # first derivatives 



################################################################################
### PRICE OPTION CONTRACTS #####################################################
###

# load option contracts 
option_contracts = load_contracts(_option_, data_path, "option_contracts.csv")
option_contracts_dense = load_contracts(_option_, data_path, "option_contracts_dense.csv")
# Price all option contracts
resopt_dd = run_measure(_mtm_, _option_, option_contracts)
resopt_dense_dd = run_measure(_mtm_, _option_, option_contracts_dense)
# remove economically worthless contracts (side effect of contract generation process)
resopt_dd[:mtm_option] = resopt_dd[:mtm_option][resopt_dd[:mtm_option].mtm .> 1e-8,:]
resopt_dense_dd[:mtm_option] = resopt_dense_dd[:mtm_option][resopt_dense_dd[:mtm_option].mtm .> 1e-8,:]
#CSV.write(data_path * "option_priced.csv", resopt_dd[:mtm_option])
#CSV.write(data_path * "option_priced_dense.csv", resopt_dense_dd[:mtm_option])



### PRICE OPTION DELTA ###########################################################
resoptdelta_dd = run_measure(_delta_, _option_, option_contracts)

### PRICE OPTION THETA  ##########################################################
resopttheta_dd = run_measure(_theta_, _option_, option_contracts)

### PRICE OPTION VEGA  ###########################################################
resoptvega_dd = run_measure(_vega_, _option_, option_contracts)

### PRICE OPTION DELTASTRIKE  ####################################################
resoptdeltastrike_dd = run_measure(_deltastrike_, _option_, option_contracts)

### PRICE OPTION DELTALOGM  ######################################################
resoptdeltalogm_dd = run_measure(_deltalogm_, _option_, option_contracts)


### COMBINE MTM, DELTA, THETA, VEGA, DELTASTRIKE   ###############################
dfmtm_opt = resopt_dd[:mtm_option]
dfdelta_opt = select(resoptdelta_dd[:delta_option], :contractId, :delta)
dftheta_opt = select(resopttheta_dd[:theta_option], :contractId, :theta)
dfvega_opt = select(resoptvega_dd[:vega_option], :contractId, :vega)
dfdeltastrike_opt = select(resoptdeltastrike_dd[:deltastrike_option], :contractId, :deltastrike)
dfdeltalogm_opt = select(resoptdeltalogm_dd[:deltalogm_option], :contractId, :deltalogm)
dfcomb_opt = leftjoin(leftjoin(leftjoin(leftjoin(leftjoin(dfmtm_opt, dfdelta_opt, on=:contractId), dftheta_opt, on=:contractId), dfdeltastrike_opt, on=:contractId), dfvega_opt, on=:contractId), dfdeltalogm_opt, on=:contractId)
#CSV.write(data_path * "option_priced_fd.csv", dfcomb_opt)  # first derivatives 


### PRICE OPTION MTM & GREEKS FOR DENSE DATA   ####################################
resopt_dense_dd = run_measure(_mtm_, _option_, option_contracts_dense)
resoptdelta_dense_dd = run_measure(_delta_, _option_, option_contracts_dense)
resopttheta_dense_dd = run_measure(_theta_, _option_, option_contracts_dense)
resoptvega_dense_dd = run_measure(_vega_, _option_, option_contracts_dense)
resoptdeltastrike__dense_dd = run_measure(_deltastrike_, _option_, option_contracts_dense)
resoptdeltalogm_dense_dd = run_measure(_deltalogm_, _option_, option_contracts_dense)

dfmtm_dopt = resopt_dense_dd[:mtm_option]
dfdelta_dopt = select(resoptdelta_dense_dd[:delta_option], :contractId, :delta)
dftheta_dopt = select(resopttheta_dense_dd[:theta_option], :contractId, :theta)
dfvega_dopt = select(resoptvega_dense_dd[:vega_option], :contractId, :vega)
dfdeltastrike_dopt = select(resoptdeltastrike__dense_dd[:deltastrike_option], :contractId, :deltastrike)
dfdeltalogm_dopt = select(resoptdeltalogm_dense_dd[:deltalogm_option], :contractId, :deltalogm)
dfcomb_dense_opt = leftjoin(leftjoin(leftjoin(leftjoin(leftjoin(dfmtm_dopt, dfdelta_dopt, on=:contractId), dftheta_dopt, on=:contractId), dfdeltastrike_dopt, on=:contractId), dfvega_dopt, on=:contractId), dfdeltalogm_dopt, on=:contractId)

#CSV.write(data_path * "option_priced_dense_fd.csv", dfcomb_dense_opt)  # first derivatives 