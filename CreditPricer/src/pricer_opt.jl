## FILE: pricer_opt.jl 

# AUTHOR: Stephen Hope 

# DESCRIPTION: 
# CDS Option standard implementation of Bloomberg CDSO pricer 




## Methods: cdsio_calc_m

# PRIVATE: NO DOC STRING
# DESCRIPTION
# Calculate the adjusted spread of the index
# ARGS
#    Δte :  time to expiry in years
#    m :    the coefficient
#    x :    the standard normal r.v.
#    σ :    annualised volatility
# RETURNS
#   index adjusted spread
cdsio_adjidxspread(Δte::Float64, m::Float64, x::Float64, σ::Float64) = m*exp(-0.5Δte*σ^2 + σ*x*√Δte)::Float64



## Methods: cdsio_calc_m

# PRIVATE: NO DOC STRING
# DESCRIPTION
# CDSIO calculate the index spread coefficient
# The constant 'm' is chosen to satisfy the requirement that the current
# expected upfront of the adjusted spread matches the clean adjusted forward
# price at delivery of the underlying swap
# ARGS
#    tradedate::Date :     valuation (end of day) date
#    idxmat::Date :        maturity date of the underlying index
#    exdate::Date :        option expiry date (exercise  date)
#    σ::Float64 :          index spread volatility
#    coupon::Float64 :     swap coupon rate of the underlying index
#    recovery::Float64 :   index recovery rate
#    fwdrate::Float64 :    fwd IR rate from option expiry to index maturity
#    fwdprice::Float64 :   knock-in (clean) forward price
# RETURNS
#    m coefficient
function cdsio_calc_m(tradedate::Date,
                      idxmat::Date,
                      exdate::Date,
                      σ::Float64,
                      coupon::Float64,
                      recovery::Float64,
                      fwdrate::Float64,
                      fwdprice::Float64)::Float64

    # Time to expiry
    Δte = year_frac(tradedate, exdate)

    # The function that expresses the condition for constant m that satisfies
    # the requirement that the expected upfront of the adjusted spread
    # matches the clean adjusted forward price at delivery
    function f0(x::Float64)
        # Integrand of the upfront of the adjusted spread

        function integrand(y::Float64, m::Float64 = x)
            adjs = cdsio_adjidxspread(Δte, m, y, σ)
            ra = cds_risky_annuity(exdate,idxmat,adjs,recovery,fwdrate,dcc="ACT/360")
            pdf(Normal(), y)*(adjs - coupon) * ra
        end

        # numerical integration
        if _config_[:NUMERICAL_INT] == "simpson"
            return(simps(integrand, -10.0, 10.0, 100) - fwdprice)
        else
            return(quadgk(integrand, -10.0, 10.0)[1] - fwdprice)
        end
    end

    # Find zero root
    zero = fzero(f0, 0.25) # looks twice as fast as find_zero

    return(zero)
end



## Methods: pricer

function price_option(ptr_zc::Any,
                      ptr_cc::Any,
                      ptr_cck::Any,
                      isdaconv::Any,
                      tradedate::Any,
                      exdate::Any,
                      idxmat::Any,
                      strike::Any,
                      vol::Any,
                      coupon::Any,
                      recovery::Any,
                      payrec::Any,
                      conmktrow::Any;
                      lossamt::Any = 0)

    # something wrong with inputs if this was called 
    return(-999.0)
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#   Bloombeg implementation of Option pricer
# ARGS
#   ptr_zc: pointer to an isda zero curve
#   ptr_cc: pointer to an isda credit curve built @ index level
#   ptr_cck: pointer to an isda credit curve built @ option strike level
#   isdaconv::ISDApricerconv struct (pricer conventions)
#   tradedate: pricing trade date
#   exdate:  exercise date 
#   idxmat: index maturity date 
#   strike: option strike expressed as spread percentage (50bp = 0.005)
#   vol: option implied vol expressed as (50% = 0.5)
#   coupon: index coupon expressed in bps (100bp = 100)
#   recovery: index recovery expressed as percentage (40% = 0.4)
#   payrec: option type (Payer = 1, Receiver = -1)
#   conmktrow::DataFrameRow : Only used if isda_cds_price pricer fails and we need approx backup
#   lossamt: option loss amount
# RETURNS
#   option value 
function price_option(ptr_zc::Ptr{TCurve},
                      ptr_cc::Ptr{TCurve},
                      ptr_cck::Ptr{TCurve},
                      isdaconv::ISDApricerconv,
                      tradedate::Date,
                      exdate::Date,
                      idxmat::Date,
                      strike::Float64,
                      vol::Float64,
                      coupon::Float64,
                      recovery::Float64,
                      payrec::Int8,
                      conmktrow::DataFrameRow;
                      lossamt::Float64 = 0.0)

    # Check if already expired
    if tradedate > exdate
        return(0.0)
    end  

    idxFactor = conmktrow.indexFactor  
    # Discount factor
    P_te = isda_df(ptr_zc, exdate)
    P_T = isda_df(ptr_zc, idxmat)
    Δte = year_frac(tradedate, exdate)
    ΔT = year_frac(tradedate, idxmat)

    # Forward rate between expiry to index maturity
    fwdIR = log(P_te/P_T) / (ΔT - Δte)

    # Value of default-adjusted index at expiry date with protection knocked-in

    #cashsettle = tradedate  #previous setting 
    cashsettle = tradedate + Day(isdaconv.cash_settle_days)

    # Calculate the risky annuity for knock-in fwd
    #stepin_date = tradedate
    stepin_date = tradedate + Day(isdaconv.stepin_days)  

    # fee leg cpn date begins accrual at expiry date + 1
    fwdacc_start = exdate + Day(1)

    fwd_riskyannuity = idxFactor * isda_cds_price(ptr_zc,
                                      ptr_cc,
                                      isdaconv,
                                      tradedate,
                                      cashsettle,
                                      stepin_date,
                                      fwdacc_start,
                                      idxmat,
                                      1.0,  # coupon
                                      1.0,  # recovery
                                      Cint(1),
                                      conmktrow) / P_te

    # Contingent leg protection starting as of trade date
    accstart = prev_imm_date(tradedate)
    #accstart = tradedate
    fwd_contleg = idxFactor * isda_cds_price(ptr_zc,
                                 ptr_cc,
                                 isdaconv,
                                 tradedate,
                                 cashsettle,
                                 stepin_date,
                                 accstart,
                                 idxmat,
                                 0.0,  # coupon
                                 recovery,
                                 Cint(1),
                                 conmktrow) / P_te

    # Forward value
    fwd_knockin =  fwd_riskyannuity * coupon + fwd_contleg

    # Solve the unknown coefficient m to match the forward value
    m = cdsio_calc_m(tradedate,
                     idxmat,
                     exdate,
                     vol,
                     coupon,
                     recovery,
                     fwdIR,
                     fwd_knockin)

    # Using the forward starting credit curve at the contractual strike - get the risky df 
    Q_te = isda_df(ptr_cck, exdate)

    strike_ra = idxFactor * isda_cds_price(ptr_zc,
                               ptr_cck,
                               isdaconv,
                               tradedate,
                               cashsettle,
                               stepin_date,
                               fwdacc_start,
                               idxmat,
                               1.0,  # coupon
                               1.0,  # recovery
                               Cint(1),
                               conmktrow) # isclean

                             
    # Strike adjustment term 
    fwdstrike = (coupon - strike) * strike_ra / Q_te / P_te

    # First, in order to acheive higher integral precision, search for ATM
    # exercise point x0
    function f0(x::Float64)
        adjs = cdsio_adjidxspread(Δte, m, x, vol)
        (adjs - coupon) * cds_risky_annuity(exdate,
                                            idxmat,
                                            adjs,
                                            recovery,
                                            fwdIR,
                                            dcc=isdaconv.paydcc) - fwdstrike + lossamt
    end

    # Limit of 10 deemed sufficient to balance between precision and speed
    #lobound = -10.0
    #upbound = 10.0
    lobound = -150.0
    upbound = 150.0
    
    x0 = try
        fzero(f0, 0.0)
    catch
        x0 = payrec == 1 ? lobound : upbound  # previous setting
        #x0 = payrec == 1 ? upbound : lobound
    end
        
    
    # Solve for the option value

    # Due to monotonicity, a unique solution known to exist
    # define option value
    function integrand(x::Float64)
        adjs = cdsio_adjidxspread(Δte, m, x, vol)
        ra = cds_risky_annuity(exdate,
                               idxmat,
                               adjs,
                               recovery,
                               fwdIR,
                               dcc=isdaconv.paydcc)
        pdf(Normal(), x)*payrec*((adjs - coupon) * ra - fwdstrike + lossamt)
    end

    
    lolimit = payrec == 1 ? x0 : lobound
    uplimit = payrec == 1 ? upbound : x0
    #lolimit = lobound  
    #uplimit = upbound

    if _config_[:NUMERICAL_INT] == "simpson"
        # 100 integration elements found to be sufficient for convergence
        #optval = P_te * simps(integrand, lolimit, uplimit, 100)
        optval = P_te * simps(integrand, lolimit, uplimit, 500)
    else
        optval = P_te * quadgk(integrand, lolimit, uplimit)[1]
    end

    # For logging purposes 
    out = (mtm=optval, 
           P_te=P_te, 
           P_T=P_T, 
           del_te=Δte, 
           del_T=ΔT,  
           fwdIR=fwdIR, 
           fwd_ra=fwd_riskyannuity,
           fwd_def=fwd_contleg,
           Fo=fwd_knockin,
           m=m,
           strike_ra=strike_ra,
           strike_adj=fwdstrike)
    
    return(out)
end

# Simpson's Rule numerical integration 
function simps(f::Function, a::Number, b::Number, n::Number)
    iseven(n) || throw("n must be even, and $n was given")
    h = (b-a)/n
    s = f(a) + f(b)
    s += 4 * sum(f.(a .+ collect(1:2:n) .* h))
    s += 2 * sum(f.(a .+ collect(2:2:n-1) .* h))
    h/3 * s
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#   MUTATING FUNCTION: convert HY strike in price to strike in spread format
# ARGS
#   conmkt::DataFRame : merged option contracts and market data dataframe 
#   ir_ptrs::DataFRame : dataframe of pointers to zero curves 
#   valuedate::Date : the bizdate 
#   contract_signature : the contract substring to search for in the contractId
#   ccy : currency of target contracts to convert 
#   assume_converted : if col 'strike_in_spread_format' exists then assume conversion already done
# RETURNS
#   conmkt DataFrame updated with a :strike_in_spread_format col 

function convert_strike_in_price_to_strike_in_spread!(conmkt::DataFrame, ir_ptrs::DataFrame, valuedate::Date;
                                                      assume_converted=false, contract_signature=".HY", ccy="USD")

    # if no target ccy found, then no converting to do 
    if isempty(ir_ptrs[ir_ptrs.currency .== ccy,:])
        conmkt[!, :strike_in_spread_format] = conmkt[:, :strike]
        return(conmkt)
    # Else if assume_converted = true and we find :strike_in_spread col - then nothing to do 
    elseif assume_converted && in("strike_in_spread_format", names(conmkt)) 
        return(conmkt)
    end 
 
    # initialise the conversion column 
    conmkt[!, :strike_in_spread_format] = conmkt[:, :strike]

    # Get the appropriate ccy ptr_zc
    ptr_zc = ir_ptrs[ir_ptrs.currency .== ccy, :ptr_zc][1]

    # Loop over contracts and call isda_upfront_to_flatspread where required
    for i in 1:nrow(conmkt)

        if  match(Regex(contract_signature), conmkt[i, :contractId]) !== nothing         
            
            startdate = Date(conmkt[i, :expiryDate])  # exercise date of the option when protection starts om the new CDS
            enddate = Date(conmkt[i, :maturity]) # enddate is the maturity of the index we exercise into 
            coupon = conmkt[i, :coupon] / 10000
            recovery = conmkt[i, :recovery]
            upfront = (100 - conmkt[i, :strike]) / 100
            
            flat = isda_upfront_to_flatspread(ptr_zc, _isdapricerconv_, valuedate,startdate,enddate,coupon,recovery,upfront)
            conmkt[i, :strike_in_spread_format] = flat
        end

    end

    return(conmkt)

end




# PRIVATE: NO DOC STRING
# DESCRIPTION
#   MUTATING FUNCTION: price a dataframe of option contracts  
#   This is a wrapper to price_option above
# ARGS
#   type: expects _option_ to call this function using multiple dispatch 
#   tradedate: trade date
#   conmkt: merged option contracts and market data dataframe 
#   out_col: name of the results column, default = "MtM"
#   assume_converted : if col 'strike_in_spread_format' exists in conmkt then assume conversion already done
# RETURNS
#    Merged dataframe of contracts and market data with extra results column

function pricer!(type::Option, 
                 tradedate::Date,
                 conmkt::DataFrame; 
                 out_col::String = "MtM",
                 assume_converted = false)

    ir_ptrs = conmkt[:,[:currency, :ptr_zc]]
    # Convert strike in price to strike in spread if necessary
    convert_strike_in_price_to_strike_in_spread!(conmkt, ir_ptrs, tradedate, assume_converted=assume_converted)

    # Add a results column to the conmkt dataframe and initialise to zeros
    conmkt[!, Symbol(out_col)] .= 0.0

    if _config_[:LOGGING]
        initialise_option_logging_results!(conmkt)
    end

    res = nothing                                 
    # Decide on MULTITHREADED or not and Loop over each contract and call price_option 
    if _config_[:OPTION_MULTITHREADING] && nrow(conmkt) > 10
        
        _config_[:LOGGING] ? @info(" :Option Multithreading ON ") : nothing
        Threads.@threads for i in 1:nrow(conmkt)

                res = price_option(conmkt[i, :ptr_zc],
                                   conmkt[i, :ptr_cc],
                                   conmkt[i, :ptr_cck],
                                   _isdapricerconv_,
                                   tradedate,
                                   Date(conmkt[i, :expiryDate]),
                                   Date(conmkt[i, :maturity]),
                                   conmkt[i, :strike_in_spread_format],
                                   conmkt[i, :impliedVol],
                                   conmkt[i, :coupon] / 10000,
                                   conmkt[i, :recovery],
                                   Int8(conmkt[i, :optionType]),
                                   conmkt[i, :],
                                   lossamt=conmkt[i, :indexLoss])
            
            conmkt[i, Symbol(out_col)] = typeof(res) == Float64 ? -999.0 : res.mtm

            if _config_[:LOGGING]
                add_option_logging_results!(conmkt, i, res)
            end
        end

    else
        for i in 1:nrow(conmkt)

            res = price_option(conmkt[i, :ptr_zc],
                               conmkt[i, :ptr_cc],
                               conmkt[i, :ptr_cck],
                               _isdapricerconv_,
                               tradedate,
                               Date(conmkt[i, :expiryDate]),
                               Date(conmkt[i, :maturity]),
                               conmkt[i, :strike_in_spread_format],
                               conmkt[i, :impliedVol],
                               conmkt[i, :coupon] / 10000,
                               conmkt[i, :recovery],
                               Int8(conmkt[i, :optionType]),
                               conmkt[i, :],
                               lossamt=conmkt[i, :indexLoss])

            conmkt[i, Symbol(out_col)] = typeof(res) == Float64 ? -999.0 : res.mtm

            if _config_[:LOGGING]
                add_option_logging_results!(conmkt, i, res)
            end
        end

    end

    return(conmkt)

end

function initialise_option_logging_results!(conmkt)
    conmkt[!, :P_te] .= 0.0
    conmkt[!, :P_T] .= 0.0
    conmkt[!, :del_te] .= 0.0
    conmkt[!, :del_T] .= 0.0
    conmkt[!, :fwdIR] .= 0.0
    conmkt[!, :fwd_ra] .= 0.0
    conmkt[!, :fwd_def] .= 0.0
    conmkt[!, :Fo] .= 0.0
    conmkt[!, :m] .= 0.0
    conmkt[!, :strike_ra] .= 0.0
    conmkt[!, :strike_adj] .= 0.0

    return(conmkt)
end

function add_option_logging_results!(conmkt, i, res)
    conmkt[i, :P_te] = res.P_te
    conmkt[i, :P_T] = res.P_T
    conmkt[i, :del_te] = res.del_te
    conmkt[i, :del_T] = res.del_T
    conmkt[i, :fwdIR] = res.fwdIR
    conmkt[i, :fwd_ra] = res.fwd_ra
    conmkt[i, :fwd_def] = res.fwd_def
    conmkt[i, :Fo] = res.Fo
    conmkt[i, :m] = res.m
    conmkt[i, :strike_ra] = res.strike_ra
    conmkt[i, :strike_adj] = res.strike_adj

    return(conmkt)
end

# PRIVATE: NO DOC STRING

# DESCRIPTION
#   MUTATING FUNCTION:
#   Price a DataFrame of option contracts 
#   Wrapper function which creates the credit curve caches
# ARGS 
#   type::Option : expects _option_ to call this function using multiple dispatch 
#   tradedate::Date : pricing trade date
#   conmkt::DataFrame: merged option contracts and market data dataframe 
#   ir_ptrs::DataFrame: dataframe of pointers to zero curves 
#   out_col::String: name of the results column, default = "MtM"
#   assume_converted::Bool : Do we need to convert price to spread for USD HY
# RETURNS
#   conmkt DataFrame updated with the an MtM col named according to out_col
function pricer!(type::Option, 
                 tradedate::Date,
                 conmkt::DataFrame, 
                 ir_ptrs::DataFrame; 
                 out_col::String = "MtM",
                 assume_converted::Bool = false)

    # Cache a dataframe of pointers to credit curves 'ptr_cc' required by this set of contracts & market data (input)  
    cc_cache = cache_credit_curves(_index_, tradedate, conmkt, ir_ptrs)

    # Cache a dataframe of pointers to credit curves 'ptr_cck' for strike levels
    convert_strike_in_price_to_strike_in_spread!(conmkt, ir_ptrs, tradedate, assume_converted=assume_converted)
    cck_cache = cache_credit_curves(_option_, tradedate, conmkt, ir_ptrs)

    # call the pricer version that merges ir_ptrs and cc_cache into the conmkt ready for pricing
    conmkt_ = pricer!(type, tradedate, conmkt, ir_ptrs, cc_cache, cck_cache, out_col=out_col)

    return(conmkt_)
end



# PRIVATE: NO DOC STRING

# DESCRIPTION
#   MUTATING FUNCTION:
#   Price a DataFrame of options contracts 
#   Wrapper function which merges ptr_cc, ptr_cck and ptr_zc into the conmkt
# ARGS 
#   type::Option : expects _option_ to call this function using multiple dispatch 
#   tradedate::Date : pricing trade date
#   conmkt::DataFrame: merged option contracts and market data dataframe 
#   ir_ptrs::DataFrame: dataframe of pointers to zero curves 
#   cc_cache::DataFrame: dataframe of pointers to creditcurves 
#   cck_cache::DataFrame: dataframe of pointers to creditcurves for strike levels 
#   out_col::String: name of the results column, default = "MtM"
# RETURNS
#   conmkt DataFrame updated with the an MtM col named according to out_col
function pricer!(type::Option, 
                 tradedate::Date,
                 conmkt::DataFrame, 
                 ir_ptrs::DataFrame, 
                 cc_cache::DataFrame,
                 cck_cache::DataFrame;
                 out_col::String = "MtM")

    # merge the ptr_cc with the conmkt 
    conmkt_ = innerjoin(conmkt, cc_cache, on = intersect(names(conmkt), names(cc_cache)))

    # merge the ptr_cck with the conmkt 
    conmkt_.strike = conmkt.strike_in_spread_format
    conmkt_ = innerjoin(conmkt_, cck_cache, on = intersect(names(conmkt_), names(cck_cache)))

    # merge the ptr_zc with the conmkt 
    conmkt_ = innerjoin(conmkt_, ir_ptrs, on = [:currency])

    # Call the main pricing routine 
    pricer!(type, tradedate, conmkt_, out_col=out_col, assume_converted=true)
    conmkt[!, Symbol(out_col)] = conmkt_[:, Symbol(out_col)]
    
    return(conmkt_)

end




# PRIVATE: NO DOC STRING
# DESCRIPTION
#   price a dataframe of option contracts
#   This is a wrapper to price_option above
# ARGS
#   type: expects _option_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   contracts: option contracts dataframe in same format as R datatable 'unique_Option_contracts'
#   market: option market data in same format as R datatable'optionMarketData'
#   ircurves: ir rate curves as read from the RData data dictionary _dd_["IRcurves"] = (DictoVec{DataFrame}) 
#   out_col: name of the results column, default = "MtM"
# RETURNS
#    Merged dataframe of contracts and market data with extra results column

function pricer(type::Option, 
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
    cc_cache = cache_credit_curves(_index_, tradedate, conmkt, ir_ptrs)

    # same but for strikes 
    convert_strike_in_price_to_strike_in_spread!(conmkt, ir_ptrs, tradedate, assume_converted=false)
    cck_cache = cache_credit_curves(_option_, tradedate, conmkt, ir_ptrs)

    out = pricer!(type, tradedate, conmkt, ir_ptrs, cc_cache, cck_cache, out_col=out_col)

    return(out)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
#   price a dataframe of option contracts
#   This is a wrapper to price_option above
# ARGS
#   type:Option, expects _option_ to call this function using multiple dispatch 
#   tradedate:Date, trade date 
#   conmkt::DataFrame, merged option contracts and market data dataframe 
#   ircurves:Dict{String, DataFrame}, ir rate curves for EUR and US 
#   out_col:String,  name of the results column, default = "MtM"
# RETURNS
#    Merged dataframe of contracts and market data with extra results column

function pricer!(type::Option, 
                 tradedate::Date,
                 conmkt::DataFrame,
                 ircurves::Dict{String, DataFrame}; 
                 out_col::String = "MtM")

    # Cache a dataframe of pointers to zero curves 'ptr_zc' from the ircurves dataframes
    ir_ptrs = cache_ir_curves(ircurves, tradedate)  

    # Cache a dataframe of pointers to credit curves 'ptr_cc' required by this set of contracts & market data (input)  
    cc_cache = cache_credit_curves(_index_, tradedate, conmkt, ir_ptrs)

    # same but for strikes 
    convert_strike_in_price_to_strike_in_spread!(conmkt, ir_ptrs, tradedate, assume_converted=true)
    cck_cache = cache_credit_curves(_option_, tradedate, conmkt, ir_ptrs)

    pricer!(type, tradedate, conmkt, ir_ptrs, cc_cache, cck_cache, out_col=out_col)

    return(conmkt)
end



# PRIVATE: NO DOC STRING
# DESCRIPTION
#   Run a MtM measure on option contracts
# ARGS
#   measure::MtM, mtm measure type expects _mtm_ 
#   type::Option,  expects _option_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   contracts::DataFrame: merged option contracts and market data dataframe 
# RETURNS
#    DataDict containing priced contracts and elapsed time 
function run_measure(measure::MtM, type::Option, contracts::DataFrame)
    resultsDD = Dict([])
    res_key = "mtm_option"
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
            println("Pricing option contracts on date $eod")
            # Filter the subset of contracts for the current valuation date
            contracts_sub = filter(row -> row.val_date == eod, contracts)
            # Call the pricer! function to get the marked-to-market (mtm) values
            pricer!(_option_, eod, contracts_sub, ir_curves, out_col="mtm")
            
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
#   Run a DELTA measure on option contracts
# ARGS
#   measure::Delta, measure type expects _delta_ 
#   type::Option,  expects _option_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   contracts::DataFrame: merged option contracts and market data dataframe 
# RETURNS
#    DataDict containing priced & Delta values for contracts and elapsed time 
function run_measure(measure::Delta, type::Option, contracts::DataFrame)
    resultsDD = Dict([])
    res_key = "delta_option"
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
            println("Pricing Delta option contracts on date $eod")
            # Filter the subset of contracts for the current valuation date
            contracts_sub = filter(row -> row.val_date == eod, contracts)
            # perturb spreads up & down & price 
            contracts_sub.spread = contracts_sub.spread .+ 1e-4
            # Call the pricer! function to get the marked-to-market (mtm) values
            pricer!(_option_, eod, contracts_sub, ir_curves, out_col="mtm_up")
            contracts_sub.spread = contracts_sub.spread .- (2 * 1e-4)
            pricer!(_option_, eod, contracts_sub, ir_curves, out_col="mtm_dn")
            contracts_sub.delta = (contracts_sub.mtm_up .- contracts_sub.mtm_dn) / (2 * 1e-4)
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
#   Run a THETA (expiry) measure on option contracts
# ARGS
#   measure::Theta, measure type expects _theta_ 
#   type::Option,  expects _option_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   contracts::DataFrame: merged option contracts and market data dataframe 
# RETURNS
#    DataDict containing priced & Theta values for contracts and elapsed time 
function run_measure(measure::Theta, type::Option, contracts::DataFrame)
    resultsDD = Dict([])
    res_key = "theta_option"
    res_key_elapsed = res_key * "_elapsed"

    year_frac_1d = 0.002777778  # (assumes ACT/360 day count convention)
    # Get unique valuation dates
    val_dates = unique(contracts.val_date)
    # Create the interest rate curves for the subset (move in loop when ir not flat)
    ir_curves = create_flat_ir_curves(contracts.flat_ir[1])
    # Initialize the final results DataFrame
    priced_contracts = DataFrame()

    elapsed = @elapsed begin 
        # Loop through each valuation date
        for eod in val_dates
            println("Pricing Theta option contracts on date $eod")
            # Filter the subset of contracts for the current valuation date
            contracts_sub = filter(row -> row.val_date == eod, contracts)
            pricer!(_option_, eod, contracts_sub, ir_curves, out_col="mtm")
            # shift time forward one day and re-price 
            eod_plus_one = CreditPricer.advancebdays(:WeekendsOnly, eod, 1)
            # Call the pricer! function to get the marked-to-market (mtm) values
            pricer!(_option_, eod_plus_one, contracts_sub, ir_curves, out_col="mtm_fwd")
            contracts_sub.theta = (contracts_sub.mtm_fwd .- contracts_sub.mtm) / (-1*year_frac_1d)
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
#   Run a VEGA measure on option contracts
# ARGS
#   measure::Vega, measure type expects _vega_ 
#   type::Option,  expects _option_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   contracts::DataFrame: merged option contracts and market data dataframe 
# RETURNS
#    DataDict containing priced & Vega values for contracts and elapsed time 
function run_measure(measure::Vega, type::Option, contracts::DataFrame)
    resultsDD = Dict([])
    res_key = "vega_option"
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
            println("Pricing Vega option contracts on date $eod")
            # Filter the subset of contracts for the current valuation date
            contracts_sub = filter(row -> row.val_date == eod, contracts)
            # perturb spreads up & down & price 
            contracts_sub.impliedVol = contracts_sub.impliedVol .+ 1e-2
            # Call the pricer! function to get the marked-to-market (mtm) values
            pricer!(_option_, eod, contracts_sub, ir_curves, out_col="mtm_up")
            contracts_sub.impliedVol = contracts_sub.impliedVol .- (2 * 1e-2)
            pricer!(_option_, eod, contracts_sub, ir_curves, out_col="mtm_dn")
            contracts_sub.vega = (contracts_sub.mtm_up .- contracts_sub.mtm_dn) / (2 * 1e-2)
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
#   Run a DELTASTRIKE measure on option contracts
# ARGS
#   measure::DeltaStrike, measure type expects _deltastrike_ 
#   type::Option,  expects _option_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   contracts::DataFrame: merged option contracts and market data dataframe 
# RETURNS
#    DataDict containing priced & Vega values for contracts and elapsed time 
function run_measure(measure::DeltaStrike, type::Option, contracts::DataFrame)
    resultsDD = Dict([])
    res_key = "deltastrike_option"
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
            println("Pricing DeltaStrike option contracts on date $eod")
            # Filter the subset of contracts for the current valuation date
            contracts_sub = filter(row -> row.val_date == eod, contracts)
            # perturb spreads up & down & price 
            contracts_sub.strike = contracts_sub.strike .+ 1e-4
            contracts_sub.strike_in_spread_format = contracts_sub.strike_in_spread_format .+ 1e-4
            # Call the pricer! function to get the marked-to-market (mtm) values
            pricer!(_option_, eod, contracts_sub, ir_curves, out_col="mtm_up")
            contracts_sub.strike = contracts_sub.strike .- (2 * 1e-4)
            contracts_sub.strike_in_spread_format = contracts_sub.strike_in_spread_format .- (2 * 1e-4)
            pricer!(_option_, eod, contracts_sub, ir_curves, out_col="mtm_dn")
            contracts_sub.deltastrike = (contracts_sub.mtm_up .- contracts_sub.mtm_dn) / (2 * 1e-4)
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
#   Run a DELTALOGM measure on option contracts
# ARGS
#   measure::DeltaLogm, measure type expects _deltalogm_ 
#   type::Option,  expects _option_ to call this function using multiple dispatch 
#   tradedate: trade date 
#   contracts::DataFrame: merged option contracts and market data dataframe 
# RETURNS
#    DataDict containing priced & DeltaLogm values for contracts and elapsed time 
function run_measure(measure::DeltaLogm, type::Option, contracts::DataFrame)
    resultsDD = Dict([])
    res_key = "deltalogm_option"
    res_key_elapsed = res_key * "_elapsed"

    logm_shift = 1e-2
    # Get unique valuation dates
    val_dates = unique(contracts.val_date)
    # Create the interest rate curves for the subset (move in loop when ir not flat)
    ir_curves = create_flat_ir_curves(contracts.flat_ir[1])
    # Initialize the final results DataFrame
    priced_contracts = DataFrame()

    # logM not used directly in pricing so find equivalent spread shift which gives constant logM shift 
    # log(k/s) + logm_shift = log(k/s')
    # s' = s exp(- logm_shift)
    elapsed = @elapsed begin 
        # Loop through each valuation date
        for eod in val_dates
            println("Pricing DeltaLogm option contracts on date $eod")
            # Filter the subset of contracts for the current valuation date
            contracts_sub = filter(row -> row.val_date == eod, contracts)
            # perturb spreads up & down for equivalent og logm_sgift & price 
            contracts_sub.spread = contracts_sub.spread .* exp(-1* logm_shift)
            # Call the pricer! function to get the marked-to-market (mtm) values
            pricer!(_option_, eod, contracts_sub, ir_curves, out_col="mtm_up")
            contracts_sub.spread = contracts_sub.spread .* exp(2* logm_shift)
            pricer!(_option_, eod, contracts_sub, ir_curves, out_col="mtm_dn")
            # logM gradient is negative 
            contracts_sub.deltalogm = (contracts_sub.mtm_up .- contracts_sub.mtm_dn) / (-2 * logm_shift)
            # Append the results to the final DataFrame
            priced_contracts = vcat(priced_contracts, contracts_sub)
        end
    end
    resultsDD[Symbol(res_key)] = priced_contracts
    resultsDD[Symbol(res_key_elapsed)] = elapsed
    
    return(resultsDD)
end
