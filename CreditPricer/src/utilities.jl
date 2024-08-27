## FILE: utilities.jl 

# DESCRIPTION: 
# Utility / Helper functions 

# DESCRIPTION
#   Load the index contract csv
# ARGS
#   type::Index,  expects _index_ to call this function using multiple dispatch 
#   data_path::String  path to the csv file 
# RETURNS
#    DataFrame of the un-priced index contracts 
function load_contracts(type::Index, data_path, name)
 
    contracts = DataFrame(CSV.File(data_path * name))
    return(contracts)
end

# DESCRIPTION
#   Load the option contract csv
# ARGS
#   type::Option,  expects _option_ to call this function using multiple dispatch 
#   data_path::String  path to the csv file 
# RETURNS
#    DataFrame of the un-priced option contracts 
function load_contracts(type::Option, data_path, name)
 
    contracts = DataFrame(CSV.File(data_path * name))
    contracts[!, :indexLoss] = Float64.(contracts[!, :indexLoss])
    contracts[!, :indexFactor] = Float64.(contracts[!, :indexFactor])
    return(contracts)
end


