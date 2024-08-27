## FILE: pricer_idx.jl 

# AUTHOR: Stephen Hope 

# DESCRIPTION: 
# CDS Index pricing using the ISDA standard library 



# PRIVATE: NO DOC STRING
# DESCRIPTION
#   price a dataframe of index contracts
#   This is a wrapper to price_cds 
# ARGS
#   type: expects _index_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   conmkt::DataFrame: merged index contracts and market data dataframe 
#   ircurves: ir rate curves as read from the RData data dictionary _dd_["IRcurves"] = (DictoVec{DataFrame}) 
#   out_col: name of the results column, default = "MtM"
# RETURNS
#    Merged dataframe of contracts and market data with extra results column
function pricer!(type::Index, 
    tradedate::Date,
    conmkt::DataFrame,
    ircurves::Dict{String, DataFrame}; 
    out_col::String = "MtM")   

    # Cache a DataFrame of pointers to zero curves 'ptr_zc' from the ircurves DataFrames
    ir_ptrs = cache_ir_curves(ircurves, tradedate)

    # Cache a DataFrame of pointers to credit curves 'ptr_cc' required by this set of contracts & market data (input)  
    cc_cache = cache_credit_curves(type, tradedate, conmkt, ir_ptrs)

    # Merge the ptr_cc with the conmkt
    merged_df = innerjoin(conmkt, cc_cache, on = intersect(names(conmkt), names(cc_cache)))

    # Merge the ptr_zc with the conmkt
    merged_df = innerjoin(merged_df, ir_ptrs[:, [:currency, :ptr_zc]], on = [:currency])

    # Now that we have a credit curve for the index, re-use the single name pricer 
    pricer!(_singlename_, tradedate, merged_df, out_col=out_col)

    # Apply the index factor 
    merged_df[:, out_col] .= merged_df[:, out_col] .* merged_df.indexFactor

    # Remove unwanted columns
    remove_cols = [:ptr_zc, :ptr_cc]
    filtered_df = merged_df[:, Not(remove_cols)]

    # Ensure the original DataFrame is updated in place
    for col in names(filtered_df)
        conmkt[!, col] = filtered_df[!, col]
    end

    return conmkt
end

# DESCRIPTION
#    This function computes the risky annuity
# ARGS
#    trade_date::Date,          Valuation date (eod)
#    maturity::Date,            Swap maturity date
#    spread::Float64,           Par spread e.g. 0.01 = 100bps
#    recovery::Float64,         Recovery rate
#    ir::Float64,               Interest rate from trade date to maturity
#    dcc::String = "ACT/365",   Day count convention - defaults to "ACT/365"
# RETURNS
#    risky annuity Float64
function cds_risky_annuity(trade_date::Date,
                           maturity::Date,
                           spread::Float64,
                           recovery::Float64,
                           ir::Float64;
                           dcc::String = "ACT/365")

    # risky annuity = (1 - exp (- (λ + ir) Δt)) / (λ + ir) 
    # write as ...          
    # risky annuity = Δt (1 - exp (- (λ + ir) Δt)) / (λ + ir) Δt
    # and call x = (λ + ir) Δt

    Δt_mat = year_frac(trade_date, maturity, dcc=dcc)
    λ = spread / (1 - recovery)
    x = (λ + ir) * Δt_mat 

    # To avoid numerical instability use an exp expansion for small values of x 
    if x < 1e-6
    x = 1 - 1/2*x + 1/6*x^2 - 1/24*x^3
    ra = Δt_mat * x
    else
    ra = Δt_mat * ( 1 - exp(-x)) / x
    end

    return(ra)
end




# PRIVATE: NO DOC STRING
# DESCRIPTION
#   Run a MtM measure on index contracts
# ARGS
#   measure::MtM, mtm measure type expects _mtm_ 
#   type::Index,  expects _index_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   contracts::DataFrame: merged index contracts and market data dataframe 
# RETURNS
#    DataDict containing priced contracts and elapsed time 
function run_measure(measure::MtM, type::Index, contracts::DataFrame)
    resultsDD = Dict([])
    res_key = "mtm_index"
    res_key_elapsed = res_key * "_elapsed"

    # Get unique valuation dates
    val_dates = unique(contracts.val_date)
    # Create the interest rate curves for the subset (move in loop when ir not flat)
    ir_curves = create_flat_ir_curves(contracts.flat_ir[1])
    # Initialize the final results DataFrame
    priced_contracts = DataFrame()
    
    elapsed = @elapsed begin 
        # Loop through each valuation date
        for eod in val_dates
            println("Pricing index contracts on date $eod")
            # Filter the subset of contracts for the current valuation date
            contracts_sub = filter(row -> row.val_date == eod, contracts)
            # Call the pricer! function to get the marked-to-market (mtm) values
            pricer!(_index_, eod, contracts_sub, ir_curves, out_col="mtm")
            
            # Append the results to the final DataFrame
            priced_contracts = vcat(priced_contracts, contracts_sub)
        end
    end
    resultsDD[Symbol(res_key)] = priced_contracts
    resultsDD[Symbol(res_key_elapsed)] = elapsed
    
    return(resultsDD)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
#   Run a CS01 measure on index contracts
# ARGS
#   measure::CS01, measure type expects _cs01_ 
#   type::Index,  expects _index_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   contracts::DataFrame: merged index contracts and market data dataframe 
# RETURNS
#    DataDict containing priced & CS01 values for contracts and elapsed time 
function run_measure(measure::CS01, type::Index, contracts::DataFrame)
    resultsDD = Dict([])
    res_key = "cs01_index"
    res_key_elapsed = res_key * "_elapsed"

    # Get unique valuation dates
    val_dates = unique(contracts.val_date)
    # Create the interest rate curves for the subset (move in loop when ir not flat)
    ir_curves = create_flat_ir_curves(contracts.flat_ir[1])
    # Initialize the final results DataFrame
    priced_contracts = DataFrame()

    elapsed = @elapsed begin 
        # Loop through each valuation date
        for eod in val_dates
            println("Pricing CS01 index contracts on date $eod")
            # Filter the subset of contracts for the current valuation date
            contracts_sub = filter(row -> row.val_date == eod, contracts)
            # perturb spreads up & down & price 
            contracts_sub.spread = contracts_sub.spread .+ 1e-4
            # Call the pricer! function to get the marked-to-market (mtm) values
            pricer!(_index_, eod, contracts_sub, ir_curves, out_col="mtm_up")
            contracts_sub.spread = contracts_sub.spread .- (2 * 1e-4)
            pricer!(_index_, eod, contracts_sub, ir_curves, out_col="mtm_dn")
            contracts_sub.cs01 = (contracts_sub.mtm_up .- contracts_sub.mtm_dn) / (2 * 1e-4)
            # Append the results to the final DataFrame
            priced_contracts = vcat(priced_contracts, contracts_sub)
        end
    end
    resultsDD[Symbol(res_key)] = priced_contracts
    resultsDD[Symbol(res_key_elapsed)] = elapsed
    
    return(resultsDD)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
#   Run a Theta measure on index contracts
# ARGS
#   measure::Theta, measure type expects _theta_ 
#   type::Index,  expects _index_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   contracts::DataFrame: merged index contracts and market data dataframe 
# RETURNS
#    DataDict containing priced & Theta values for contracts and elapsed time 
function run_measure(measure::Theta, type::Index, contracts::DataFrame)

    year_frac_1d = 0.002777778  # (assumes ACT/360 day count convention)
    resultsDD = Dict([])
    res_key = "theta_index"
    res_key_elapsed = res_key * "_elapsed"

    # Get unique valuation dates
    val_dates = unique(contracts.val_date)
    # Create the interest rate curves for the subset (move in loop when ir not flat)
    ir_curves = create_flat_ir_curves(contracts.flat_ir[1])
    # Initialize the final results DataFrame
    priced_contracts = DataFrame()

    elapsed = @elapsed begin 
        # Loop through each valuation date
        for eod in val_dates
            println("Pricing Theta index contracts on date $eod")
            # Filter the subset of contracts for the current valuation date
            contracts_sub = filter(row -> row.val_date == eod, contracts)
            pricer!(_index_, eod, contracts_sub, ir_curves, out_col="mtm")
            # shift time forward one day and re-price 
            eod_plus_one = CreditPricer.advancebdays(:WeekendsOnly, eod, 1)
            # Call the pricer! function to get the marked-to-market (mtm) values
            pricer!(_index_, eod_plus_one, contracts_sub, ir_curves, out_col="mtm_fwd")
            contracts_sub.theta = (contracts_sub.mtm_fwd .- contracts_sub.mtm) / (-1* year_frac_1d)
            # Append the results to the final DataFrame
            priced_contracts = vcat(priced_contracts, contracts_sub)
        end
    end
    resultsDD[Symbol(res_key)] = priced_contracts
    resultsDD[Symbol(res_key_elapsed)] = elapsed
    
    return(resultsDD)
end