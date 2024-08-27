## FILE: pricer_sn.jl 

# AUTHOR: Stephen Hope 

# DESCRIPTION: 
# CDS single name pricer usig ISDA standasrd library 

# PRIVATE: NO DOC STRING

# DESCRIPTION
#   Price a single name CDS (wrapper to ISDA pricer)
# ARGS 
#   ptr_zc::Ptr{TCurve} : pointer to an isda zero curve
#   ptr_cc::Ptr{TCurve} : pointer to an isda credit curve built @ index level
#   isdaconv::ISDApricerconv : struct (pricer conventions)
#   tradedate::Date : pricing trade date
#   matdate::Date : maturity date 
#   coupon::Float64 : coupon expressed in bps (100bp = 100)
#   recovery::Float64 : recovery expressed as percentage (40% = 0.4)
# RETURNS
#   pv::Float64 
function price_cds(ptr_zc::Ptr{TCurve},
                   ptr_cc::Ptr{TCurve},
                   isdaconv::ISDApricerconv,
                   tradedate::Date,
                   matdate::Date,
                   coupon::Float64,
                   recovery::Float64,
                   conmktrow::DataFrameRow)

    pv = isda_cds_price(ptr_zc,
                        ptr_cc,
                        isdaconv,
                        tradedate,
                        tradedate, # cashsettle
                        tradedate + Day(isdaconv.stepin_days), # step_in date
                        tradedate + Day(1), # accstart accrual at expiry date + 1
                        matdate,
                        coupon,
                        recovery,
                        Cint(1), # is clean = true
                        conmktrow) # only used if approx pricing needed 

    return(pv)

end


# PRIVATE: NO DOC STRING

# DESCRIPTION
#   MUTATING FUNCTION:
#   Price a DataFrame of single name CDS contracts 
#   Wrapper function which creates the credit curve cache
# ARGS 
#   type::Singlename : expects _singlename_ to call this function using multiple dispatch 
#   tradedate::Date : pricing trade date
#   conmkt::DataFrame: merged single name contracts and market data dataframe 
#   ir_ptrs::DataFrame: dataframe of pointers to zero curves 
#   out_col::String: name of the results column, default = "MtM"
# RETURNS
#   conmkt DataFrame updated with the an MtM col named according to out_col
function pricer!(type::Singlename, 
                 tradedate::Date,
                 conmkt::DataFrame, 
                 ir_ptrs::DataFrame; 
                 out_col::String = "MtM")

    # Cache a dataframe of pointers to credit curves 'ptr_cc' required by this set of contracts & market data (input)  
    cc_cache = cache_credit_curves(type, tradedate, conmkt, ir_ptrs)

    # call the pricer version that merges ir_ptrs and cc_cache into the conmkt ready for pricing
    conmkt = pricer!(type, tradedate, conmkt, ir_ptrs, cc_cache, out_col=out_col)

    return(conmkt)
end


# PRIVATE: NO DOC STRING

# DESCRIPTION
#   MUTATING FUNCTION:
#   Price a DataFrame of single name CDS contracts 
#   Wrapper function which merges ptr_cc and ptr_zc into the conmkt
# ARGS 
#   type::Singlename : expects _singlename_ to call this function using multiple dispatch 
#   tradedate::Date : pricing trade date
#   conmkt::DataFrame: merged single name contracts and market data dataframe 
#   ir_ptrs::DataFrame: dataframe of pointers to zero curves 
#   cc_cache::DataFrame: dataframe of pointers to creditcurves 
#   out_col::String: name of the results column, default = "MtM"
# RETURNS
#   conmkt DataFrame updated with the an MtM col named according to out_col
function pricer!(type::Singlename, 
                 tradedate::Date,
                 conmkt::DataFrame, 
                 ir_ptrs::DataFrame, 
                 cc_cache::DataFrame;
                 out_col::String = "MtM")

    # merge the ptr_cc with the conmkt 
    conmkt_ = innerjoin(conmkt, cc_cache, on = intersect(names(conmkt), names(cc_cache)))
    # merge the ptr_zc with the conmkt 
    conmkt_ = innerjoin(conmkt_, ir_ptrs, on = [:currency])
    # Call the main pricing routine 
    pricer!(type, tradedate, conmkt_, out_col=out_col)
    conmkt[!, Symbol(out_col)] = conmkt_[:, Symbol(out_col)]

    return(conmkt_)
end


# DESCRIPTION
#   MUTATING FUNCTION:
#   Price a DataFrame of single name CDS contracts 
#   Main pricing logic wrapper calling the actual price_cds function
# ARGS 
#   type::Singlename : expects _singlename_ to call this function using multiple dispatch 
#   tradedate::Date : pricing trade date
#   conmkt::DataFrame: merged single name contracts and market data dataframe 
#   out_col::String: name of the results column, default = "MtM"
# RETURNS
#   conmkt DataFrame updated with the an MtM col named according to out_col
function pricer!(type::Singlename, 
                 tradedate::Date,
                 conmkt::DataFrame; 
                 out_col::String = "MtM")

    # Check we have cols for ptr_zc and ptr_cc
    if !("ptr_zc" in names(conmkt) && "ptr_cc" in names(conmkt) )
        @error "Columns :ptr_zc and :ptr_cc must exist in conmkt DataFrame "
    end

    # Add a results column to the conmkt dataframe and initialise to zeros
    conmkt[!, Symbol(out_col)] .= 0.0

    if _config_[:SN_EXPERIMENTAL_OPTIMISATION] && nrow(conmkt) > 10000 # only unique pricing combinations 
        conmkt_ = conmkt
        conmkt_[!, Symbol(out_col)] .= 0.0
        # Rather than unique we use combine(groupby) which is much slower as we need to add back in s1 .. s10 
        # for approx pricer backup calls 
        conmkt_=  combine(groupby(conmkt_, [:coupon, :maturity, :recovery, :ptr_zc, :ptr_cc]), first) 
    else
        conmkt_ = conmkt
    end

    # Decide on MULTITHREADED or not and Loop over each contract and call price_option 
    if _config_[:MULTITHREADING] && nrow(conmkt) > 100

        _config_[:LOGGING] ? @info(" :Multithreading ON ") : nothing
        Threads.@threads for i in 1:nrow(conmkt_)

            # call the pricer
            conmkt_[i, Symbol(out_col)] = price_cds(conmkt_[i, :ptr_zc],
                                                    conmkt_[i, :ptr_cc],
                                                    _isdapricerconv_,
                                                    tradedate,
                                                    Date(conmkt_[i, :maturity]),
                                                    conmkt_[i, :coupon] / 10000,
                                                    conmkt_[i, :recovery],
                                                    conmkt_[i, :])

        end

    else
        for i in 1:nrow(conmkt_)

        # call the pricer
        conmkt_[i, Symbol(out_col)] = price_cds(conmkt_[i, :ptr_zc],
                                                conmkt_[i, :ptr_cc],
                                                _isdapricerconv_,
                                                tradedate,
                                                Date(conmkt_[i, :maturity]),
                                                conmkt_[i, :coupon] / 10000,
                                                conmkt_[i, :recovery],
                                                conmkt_[i, :])
        end 

    end

    if _config_[:SN_EXPERIMENTAL_OPTIMISATION] && nrow(conmkt) > 10000
        conmktout = leftjoin(conmkt[:, Not(Symbol(out_col))], conmkt_, on=([:coupon, :maturity, :recovery, :ptr_zc, :ptr_cc]), makeunique=true)
        conmkt[!, Symbol(out_col)] = conmktout[:, Symbol(out_col)]
    end

    return(conmkt)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
#   price a dataframe of sn contracts
#   This is a wrapper to price_cds above
# ARGS
#   type: expects _singlename_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   contracts: sn contracts dataframe in same format as R datatable 'unique_SingleName_contracts'
#   market: entity market data in same format as R datatable'optionMarketData'
#   ircurves: ir rate curves as read from the RData data dictionary _dd_["IRcurves"] = (DictoVec{DataFrame}) 
#   out_col: name of the results column, default = "MtM"
# RETURNS
#    Merged dataframe of contracts and market data with extra results column

function pricer(type::Singlename, 
                tradedate::Date,
                contracts::DataFrame, 
                market::DataFrame, 
                ircurves::DictoVec{DataFrame}; 
                out_col::String = "MtM")

    # Cache a dataframe of pointers to zero curves 'ptr_zc' from the ircurves dataframes
    ir_ptrs = cache_ir_curves(ircurves, tradedate)
    # Merge the contracts and market data 
    conmkt = innerjoin(contracts, market, on = intersect(names(contracts), names(market)))    
    # Cache a dataframe of pointers to credit curves 'ptr_cc' required by this set of contracts & market data (input)  
    cc_cache = cache_credit_curves(type, tradedate, conmkt, ir_ptrs)
    out = pricer!(type, tradedate, conmkt, ir_ptrs, cc_cache, out_col=out_col)

    return(out)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
#   price a dataframe of sn contracts
#   This is a wrapper to price_cds above
# ARGS
#   type: expects _singlename_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   conmkt::DataFrame: merged single name contracts and market data dataframe 
#   ircurves: ir rate curves as read from the RData data dictionary _dd_["IRcurves"] = (DictoVec{DataFrame}) 
#   out_col: name of the results column, default = "MtM"
# RETURNS
#    Merged dataframe of contracts and market data with extra results column

function pricer!(type::Singlename, 
                 tradedate::Date,
                 conmkt::DataFrame,  
                 ircurves::DictoVec{DataFrame}; 
                 out_col::String = "MtM")

    # Cache a dataframe of pointers to zero curves 'ptr_zc' from the ircurves dataframes
    ir_ptrs = cache_ir_curves(ircurves, tradedate)   
    # Cache a dataframe of pointers to credit curves 'ptr_cc' required by this set of contracts & market data (input)  
    cc_cache = cache_credit_curves(type, tradedate, conmkt, ir_ptrs)
    pricer!(type, tradedate, conmkt, ir_ptrs, cc_cache, out_col=out_col)

    return(conmkt)
end