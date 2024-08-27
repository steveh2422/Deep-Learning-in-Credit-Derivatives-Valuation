## FILE: isda_creditcurve.jl 

# AUTHOR: Stephen Hope 

# DESCRIPTION: 
# Code for building the creditcurve via the ISDA C library 


## Methods: credit curve caching 

# PRIVATE: NO DOC STRING
# DESCRIPTION
#   Build a cache of credit curve ptrs for each unique index {spread, recovery, ccy}
# ARGS
#   type: trade type - should be _index_ to call this function through multiple dispatch
#   eodddate: end of day date
#   conmkt: merged contracts and market data dataframe 
#   ir_ptrs: dataframe of pointers to zero curves 
# RETURNS
#    dataframe of contracts and pointers to corresponding isda credit curve (ptr_cc)

function cache_credit_curves(type::Index,
                             eoddate::Date,
                             conmkt::DataFrame,
                             ir_ptrs::DataFrame)

    # Initialise unique cache
    # NOTE: we key on :spread / :strike (not contractId) because this caching needs to work during pricematrix building
    # where the same contract is in the conmkt many times with different bumped spreads
    cc_cache = unique(select(conmkt, ([:spread, :currency, :recovery])))
    cc_cache.ptr_cc = fill(Ptr{TCurve}(C_NULL), nrow(cc_cache))

    # For each unique {spread/strike, currency, recovery} - create a ptr_cc to an isda credit curve
    if _config_[:MULTITHREADING] && nrow(cc_cache) > 100    # CRASHING FOR quadgk integration 
        Threads.@threads for i in 1:nrow(cc_cache)

            ccy = cc_cache[i, :currency]
            ptr_zc = ir_ptrs[ir_ptrs.currency .== ccy, :ptr_zc][1]
            idxlevel = cc_cache[i, :spread]
            rec = cc_cache[i, :recovery]

            cc_cache[i, :ptr_cc] = isda_build_credit_curve(eoddate, ptr_zc, idxlevel, rec, _index_)
        end
    else
        for i in 1:nrow(cc_cache)

            ccy = cc_cache[i, :currency]
            ptr_zc = ir_ptrs[ir_ptrs.currency .== ccy, :ptr_zc][1]
            idxlevel = cc_cache[i, :spread]
            rec = cc_cache[i, :recovery]

            cc_cache[i, :ptr_cc] = isda_build_credit_curve(eoddate, ptr_zc, idxlevel, rec, _index_)
        end
    end
    return(cc_cache)
end

function cache_credit_curves(type::Option, 
                             eoddate::Date,
                             conmkt::DataFrame,
                             ir_ptrs::DataFrame)

    # Initialise unique cache
    cck_cache = unique(select(conmkt, ([:strike_in_spread_format, :currency, :recovery])))
    rename!(cck_cache, [:strike_in_spread_format] .=> [:strike])
    cck_cache.ptr_cck = fill(Ptr{TCurve}(C_NULL), nrow(cck_cache))


    # For each unique {contractId, strike} - create a ptr_cc to an isda credit curve
    if _config_[:MULTITHREADING] && nrow(cck_cache) > 100    # CRASHING FOR QUADGK ??? 
        Threads.@threads for i in 1:nrow(cck_cache)

            strike = cck_cache[i, :strike]
            rec = cck_cache[i, :recovery]
            ccy = cck_cache[i, :currency]
            ptr_zc = ir_ptrs[ir_ptrs.currency .== ccy, :ptr_zc][1]
            cck_cache[i, :ptr_cck] = isda_build_credit_curve(eoddate, ptr_zc, strike, rec, _index_)
        end
    else
        for i in 1:nrow(cck_cache)

            strike = cck_cache[i, :strike]
            rec = cck_cache[i, :recovery]
            ccy = cck_cache[i, :currency]
            ptr_zc = ir_ptrs[ir_ptrs.currency .== ccy, :ptr_zc][1]
            cck_cache[i, :ptr_cck] = isda_build_credit_curve(eoddate, ptr_zc, strike, rec, _index_)
        end
    end

    return(cck_cache)
end




# PRIVATE: NO DOC STRING
# DESCRIPTION
#   Build a cache of credit curve ptrs for each unique single name contract 
# ARGS
#   type: trade type - should be _singlename_ to call this function through multiple dispatch
#   eodddate: end of day date
#   conmkt: merged contracts and market data dataframe 
#   ir_ptrs: dataframe of pointers to zero curves 
# RETURNS
#    dataframe of contracts and pointers to corresponding isda credit curve (ptr_cc)

function cache_credit_curves(type::Singlename,
                             eoddate::Date,
                             conmkt::DataFrame,
                             ir_ptrs::DataFrame)

    # Initialise unique cache
    # NOTE: we key on :s1, :s3 ... etc because this caching needs to work during pricematrix building
    # where the same contract is in the conmkt many times with different bumped spreads
    cc_cache = unique(select(conmkt, ([:s1, :s3, :s5, :s7, :s10, :currency, :recovery])))
    cc_cache.ptr_cc = fill(Ptr{TCurve}(C_NULL), nrow(cc_cache))

    # For each unique {cdskey, s1, s3, s5, s7, s10} - create a ptr_cc to an isda credit curve

    if _config_[:MULTITHREADING]  && nrow(conmkt) > 10
        Threads.@threads for i in 1:nrow(cc_cache)

            ccy = cc_cache[i, :currency]
            rec = cc_cache[i, :recovery]
            s1 = cc_cache[i, :s1]
            s3 = cc_cache[i, :s3]
            s5 = cc_cache[i, :s5]
            s7 = cc_cache[i, :s7]
            s10 = cc_cache[i, :s10]
            spreads = [s1,s3,s5,s7,s10]
            ptr_zc = ir_ptrs[ir_ptrs.currency .== ccy, :ptr_zc][1] 

            cc_cache[i, :ptr_cc] = isda_build_credit_curve(eoddate, ptr_zc, spreads, rec, _singlename_)
        end

    else    
        for i in 1:nrow(cc_cache)

            ccy = cc_cache[i, :currency]
            rec = cc_cache[i, :recovery]
            s1 = cc_cache[i, :s1]
            s3 = cc_cache[i, :s3]
            s5 = cc_cache[i, :s5]
            s7 = cc_cache[i, :s7]
            s10 = cc_cache[i, :s10]
            spreads = [s1,s3,s5,s7,s10]
            ptr_zc = ir_ptrs[ir_ptrs.currency .== ccy, :ptr_zc][1] 

            cc_cache[i, :ptr_cc] = isda_build_credit_curve(eoddate, ptr_zc, spreads, rec, _singlename_)
        end
    end

    return(cc_cache)
end


## Methods: isda_build_credit_curve

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Bootstraps a clean spread curve from par spread inputs
#    Convenience wrapper for _option_ trade type to automatically generate
#    the appropriate maturity benchmarks and coupons
#    For more info see isda cds.h JpmcdsCleanSpreadCurve function description
# ARGS
#    valuedate::Date :      value date (usually end of day)
#    ptr_zc::Ptr{TCurve} :  pointer to isda zero curve, 'isda_build_zero_curve'
#    spread::Float64 :      single index ref level
#    recovery::Float64 :    index recovery level
#    tradetype::Option :  the index singleton struct _index_
# RETURNS
#    pointer to an isda credit curve
function isda_build_credit_curve(valuedate::Date,
                                 ptr_zc::Ptr{TCurve},
                                 spread::Float64,
                                 recovery::Float64,
                                 tradetype::Option)

    # For Option build credit curve as for _indexflat_
    ptr_cc = isda_build_credit_curve(valuedate, ptr_zc,spread,recovery, _index_)
    return(ptr_cc)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Bootstraps a clean spread curve from par spread inputs
#    Convenience wrapper for _index_ trade type to automatically generate
#    the appropriate maturity benchmarks and coupons
#    For more info see isda cds.h JpmcdsCleanSpreadCurve function description
# ARGS
#    valuedate::Date :      value date (usually end of day)
#    ptr_zc::Ptr{TCurve} :  pointer to isda zero curve, 'isda_build_zero_curve'
#    spread::Float64 :      single index ref level
#    recovery::Float64 :    index recovery level
#    tradetype::Index :     the index singleton struct _index_
# RETURNS
#    pointer to an isda credit curve
function isda_build_credit_curve(valuedate::Date,
                                 ptr_zc::Ptr{TCurve},
                                 spread::Float64,
                                 recovery::Float64,
                                 tradetype::Index)

    # Index IM convention, flat term curve is generated for INDEX pricing
    # using tenors ["6M","1Y","2Y","3Y","4Y","5Y","7Y","10Y"]
    benchmark_start = benchmark_end_date(valuedate, tenor="0M")
    spread_curve_dates = get_spread_maturities(benchmark_start,
                        tradetype).maturity

    spreads = fill(spread, length(spread_curve_dates))

    ptr_cc = isda_build_credit_curve(valuedate,
                                     ptr_zc,
                                     spreads,
                                     spread_curve_dates,
                                     recovery)

    return(ptr_cc)
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Bootstraps a clean spread curve from par spread inputs
#    Convenience wrapper for _singlename_ trade type to automatically generate
#    the appropriate maturity benchmarks and coupons
#    For more info see isda cds.h JpmcdsCleanSpreadCurve function description
#    src code in cdsbootstrap.c
# ARGS
#    valuedate::Date :            value date (usually end of day)
#    ptr_zc::Ptr{TCurve} :        ptr to isda zero curve, 'isda_build_zero_curve'
#    spreads::Array{Float64,1} :  spreads / coupon rates
#    recovery::Float64 :          recovery level
#    tradetype::ISinglename :     single name singleton struct _singlename_
# RETURNS
#    pointer to an isda credit curve
function isda_build_credit_curve(valuedate::Date,
                                 ptr_zc::Ptr{TCurve},
                                 spreads::Array{Float64,1},
                                 recovery::Float64,
                                 tradetype::Singlename)

    # Singlename is currently using tenors ["1Y","3Y","5Y","7Y","10Y"]
    benchmark_start = benchmark_end_date(valuedate, tenor="0M")
    spread_curve_dates = get_spread_maturities(benchmark_start,
                                               tradetype).maturity

    ptr_cc = isda_build_credit_curve(valuedate,
                                     ptr_zc,
                                     spreads,
                                     spread_curve_dates,
                                     recovery)

    return(ptr_cc)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Bootstraps a clean spread curve from par spread inputs
#    Convenience wrapper passing tenors of form {"1M", "3M", "1Y" ... }
#    For more info see isda cds.h JpmcdsCleanSpreadCurve function description
# ARGS
#    valuedate::Date :            value date (usually end of day)
#    ptr_zc::Ptr{TCurve} :        ptr to isda zero curve, 'isda_build_zero_curve'
#    coupons::Array{Float64,1} :  spreads / coupon rates
#    tenors::Array{String,1} :    tenors for each benchmark
#    recovery::Float64 :          recovery level
# RETURNS
#    pointer to an isda credit curve
function isda_build_credit_curve(valuedate::Date,
                                 ptr_zc::Ptr{TCurve},
                                 coupons::Array{Float64,1},
                                 tenors::Array{String,1},
                                 recovery::Float64)


    benchmark_start = benchmark_end_date(valuedate, tenor="0M")
    spread_curve_dates = get_spread_maturities(benchmark_start, tenors).maturity

    ptr_cc = isda_build_credit_curve(valuedate,
                ptr_zc,
                coupons,
                spread_curve_dates,
                recovery)

    return(ptr_cc)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Bootstraps a clean spread curve from par spread inputs
#    For more info see isda cds.h JpmcdsCleanSpreadCurve function description
# ARGS
#    valuedate::Date :            value date (usually end of day)
#    ptr_zc::Ptr{TCurve} :        ptr to isda zero curve, 'isda_build_zero_curve'
#    coupons::Array{Float64,1} :  spreads / coupon rates
#    maturities::Array{Date,1} :  maturity dates for each benchmark
#    recovery::Float64 :          recovery level
# RETURNS
#    pointer to an isda credit curve
function isda_build_credit_curve(valuedate::Date,
                                 ptr_zc::Ptr{TCurve},
                                 coupons::Array{Float64,1},
                                 maturities::Array{Date,1},
                                 recovery::Float64)

    routine = "isda_build_credit_curve"

    if length(coupons) != length(maturities)
        @error "instrument spreads and maturities must have the same number of entries"
    end

    # ISDA pricer conventions
    isdaconv = _isdapricerconv_

    benchmark_start = benchmark_end_date(valuedate, tenor="0M")

    # Type conversions 'xyz_' underscore means type ready for ccalls

    # number of benchmark dates
    nbdates_ = Clong(length(maturities))

    # dates when protection ends for each bnchmark (eod)
    maturities_ = Vector{Clong}(map(Dates.value, maturities))

    # coupon rates for each benchmark instrument
    coupons_ = Vector{Cdouble}(coupons)

    # holidays used for adjusting coupon dates; None is weekends only
    holidays_ = "None"

    # risk starts at end of today
    zerodate_ = Clong(Dates.value(valuedate))

    # effective date of benchmark CDS
    accstart_ = Clong(Dates.value(benchmark_start))

    # protection start date used in the Model
    stepindate_ = JpmcdsDtFwdAny_(valuedate, isdaconv.stepin_days)

    # cash settle date adjusted for working days
    cashsettledate_ = JpmcdsDateFwdThenAdjust_(valuedate,
    isdaconv.cash_settle_days,
    isdaconv.bdc,
    holidays_)

    # flags to denote benchmarks to include . This makes it
    # can pass this as an input in the future if need be
    includes = fill(Cint(1), nbdates_)
    includes_ = Vector{Cint}(includes)

    # recovery rate in case of default
    recovery_ = Cdouble(recovery)

    # should accrued interest be paid on default - usuall set to true
    payacc_ondefault_ = Cint(isdaconv.payacc_ondefault)

    # interval between coupon payments
    payivl = JpmcdsStringToDateInterval_(isdaconv.payfreq)
    payivl_ = Ref(payivl)

    # day count convention for coupon payment - norally ACT_360
    paydcc_ = JpmcdsStringToDayCountConv_(isdaconv.paydcc)

    # if the startDate and endDate are not on cycle, then this parameter
    # determines location of coupon dates
    stubmethod = JpmcdsStringToStubMethod_(isdaconv.stubtype)
    stubmethod_ = Ref(stubmethod)

    # bad day convention for adjusting coupon payment dates
    bdc_ = Clong(isdaconv.bdc)

    # Now build the credit curve
    # C definition in cdsbootstrap.h

    out_ptr = ccall((:JpmcdsCleanSpreadCurve, :libisda),
                        Ptr{TCurve},         # return ptr
                        (Clong,              # today
                        Ptr{TCurve},         # ptr to zero curve
                        Clong,               # startDate
                        Clong,               # stepindate
                        Clong,               # cashSettleDate
                        Clong,               # nbDate
                        Ref{Clong},          # *endDates
                        Ref{Cdouble},        # *couponRates
                        Ref{Cint},           # *includes
                        Cdouble,             # recoveryRate
                        Cint,               # payAccOnDefault
                        Ref{TDateInterval},  # *couponInterval
                        Clong,               # paymentDCC
                        Ref{TStubMethod},    # *stubType
                        Clong,               # badDayConv
                        Cstring),            # *calendar
                        zerodate_,
                        ptr_zc,
                        accstart_,
                        stepindate_,
                        cashsettledate_,
                        nbdates_,
                        maturities_,
                        coupons_,
                        includes_,
                        recovery_,
                        payacc_ondefault_,
                        payivl_,
                        paydcc_,
                        stubmethod_,
                        bdc_,
                        holidays_)

    return(out_ptr)
end
