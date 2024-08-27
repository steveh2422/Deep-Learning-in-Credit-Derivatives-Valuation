## FILE: isda_cds.jl 

# AUTHOR: Stephen Hope 

# DESCRIPTION: 
# Code for calling the ISDA CDS pricer


## Methods: isda_cds_price

# PRIVATE: NO DOC STRING
# DESCRIPTION
# Computes the price (upfront charge) for a vanilla CDS (see cds.h)
# ARGS
#    ptr_zc{Ptr::TCurve} : pointer to an isda zero curve
#    ptr_cc{Ptr::TCurve} : pointer to an isda credit curve built
#    isdaconv::ISDApricerconv : ISDApricerconv struct (pricer conventions)
#    cashsettle::Date : Date for which the PV is calculated and cash settled
#    stepin_date::Date : Step in date of the benchmark CDS
#    accstart::Date : Effective date of the benchmark CDS
#    idxmat::Date : index maturity 
#    cpn::Float64 : spread level
#    recovery::Float64 : recovery
#    isclean::UInt8 : boolean 'isClean' price
#    conmktrow::DataFrameRow : Only used if isda_cds_price pricer fails and we need approx backup
# RETURNS
#    cds pv
function isda_cds_price(ptr_zc::Ptr{TCurve},
                        ptr_cc::Ptr{TCurve},
                        isdaconv::ISDApricerconv,
                        valuedate::Date,
                        cashsettle::Date,
                        stepin_date::Date,
                        accstart::Date,
                        idxmat::Date,
                        cpn::Float64,
                        recovery::Float64,
                        isclean::Cint,
                        conmktrow::DataFrameRow)::Float64


    routine = "isda_cds_pricer"

    # Calendar used when adjusting coupon dates. Can be NULL which equals
    # a calendar with no holidays and including weekends
    hols_ = "None"

    # Risk starts at the end of today
    today_ = Clong(Dates.value(valuedate))

    # Date for which the PV is calculated and cash settled
    cashsettle_ = Clong(Dates.value(cashsettle))

    # Step in date of the benchmark CDS
    stepin_date_ = Clong(Dates.value(stepin_date))

    # Effective date of the benchmark CDS
    accstart_ = Clong(Dates.value(accstart))

    # Date when protection ends (end of day)
    idxmat_ = Clong(Dates.value(idxmat))

    # Fixed coupon rate (a.k.a. spread) for the fee leg
    cpn_ = cpn

    # Should accrued interest be paid on default. Usually set to TRUE
    payacc_ondefault_ = Cint(isdaconv.payacc_ondefault)

    # Interval between coupon payments. Can be NULL when 3M is assumed
    payivl = JpmcdsStringToDateInterval_(isdaconv.payfreq)
    payivl_ = Ref(payivl)

    # If the startDate and endDate are not on cycle, then this parameter
    # determines location of coupon dates
    stubmethod = JpmcdsStringToStubMethod_(isdaconv.stubtype)
    stubmethod_ = Ref(stubmethod)

    # Day count convention for coupon payment. Normal is ACT_360
    paydcc_ = JpmcdsStringToDayCountConv_(isdaconv.paydcc)

    # Bad day convention for adjusting coupon payment dates
    bdc_ = Clong(isdaconv.bdc)

    # recovery rate in case of default
    recovery_ = Cdouble(recovery)

    # Is the price expressed as a clean price (removing accrued interest)
    isclean_ = isclean

    # Call isda pricer
    price_ = Ref{Cdouble}(0.0)

    status = ccall((:JpmcdsCdsPrice, :libisda),
                    Cint,
                    (Clong,               # today
                    Clong,                # valueDate (really cashsettle date)
                    Clong,                # stepinDate
                    Clong,                # startDate
                    Clong,                # endDate
                    Cdouble,              # couponRate
                    Cint,                 # payAccOnDefault
                    Ref{TDateInterval},   # *couponInterval
                    Ref{TStubMethod},     # *stubType
                    Clong,                # paymentDcc
                    Clong,                # badDayConv
                    Cstring,              # *calendar
                    Ptr{TCurve},          # *discCurve
                    Ptr{TCurve},          # *spreadCurve
                    Cdouble,              # recoveryRate
                    Cint,                 # isPriceClean
                    Ref{Cdouble}),        # *price (output)
                    today_,
                    cashsettle_,
                    stepin_date_,
                    accstart_,
                    idxmat_,
                    cpn_,
                    payacc_ondefault_,
                    payivl_,
                    stubmethod_,
                    paydcc_,
                    bdc_,
                    hols_,
                    ptr_zc,
                    ptr_cc,
                    recovery_,
                    isclean_,
                    price_)

    out = price_.x

    # APPROX Pricing fallback 
    if status !=0  

        # assume SN contract has failed to bootstrap
        if ":s1" in names(conmktrow)

            # Calculate an approximate price 
            benchmark_start = benchmark_end_date(valuedate, tenor="0M")
            spread_maturities = get_spread_maturities(benchmark_start, _singlename_)
            conmkt_interp = DataFrame(conmktrow)[:, [:s1, :s3, :s5, :s7, :s10, :maturity]]
            conmkt_interp = unique(conmkt_interp) 

            #if _config_[:APPROX_MODEL] == "hazard_curve"
            build_hazard_curve!(valuedate, conmkt_interp, spread_maturities, "spread")
            #else
            #    lin_interp_spread!(conmkt_interp, spread_maturities, "spread")
            #end

            # TODO - check this ... 
            ir_rate = (1 ./ isda_df(ptr_zc, idxmat)) .- 1

            out = cds_pv(valuedate,
                        idxmat,
                        conmkt_interp[1, :spread],
                        cpn,
                        recovery,
                        ir_rate,
                        _isdapricerconv_.paydcc)

            # TODO record / log failure to bootstrap                     

        end
    end

    return(out)
end




# PRIVATE: NO DOC STRING
# DESCRIPTION
# Computes the flat spread required to match the upfron charge
# ARGS
#    ptr_zc{Ptr::TCurve} :
#    isdaconv::ISDApricerconv :
#    valuedate::Date :
#    startdate::Date :
#    enddate::Date :
#    cpn::Float64 :
#    recovery::Float64 :
#    upfront::Float64 :
# RETURNS
#    flat spread corresponding to upfront 
function isda_upfront_to_flatspread(ptr_zc::Ptr{TCurve},
                                    isdaconv::ISDApricerconv,
                                    valuedate::Date,
                                    startdate::Date,
                                    enddate::Date,
                                    cpn::Float64,
                                    recovery::Float64,
                                    upfront::Float64)::Float64


    routine = "isda_upfront_to_flatspread"

    # Calendar used when adjusting coupon dates. Can be NULL which equals
    # a calendar with no holidays and including weekends
    calendar_ = "None"

    # Risk starts at the end of today
    today_ = Clong(Dates.value(valuedate))

    # Effective date of the benchmark CDS / benchmarkStartDate_
    benchmarkStartDate_ = Clong(Dates.value(startdate))
    startdate_ = Clong(Dates.value(startdate))
    stepin_date_ = startdate_


    # Date when protection ends (end of day)
    enddate_ = Clong(Dates.value(enddate))

    # Fixed coupon rate (a.k.a. spread) for the fee leg
    cpn_ = cpn

    # Should accrued interest be paid on default. Usually set to TRUE
    payacc_ondefault_ = Cint(isdaconv.payacc_ondefault)

    # Interval between coupon payments. Can be NULL when 3M is assumed
    dateInterval = JpmcdsStringToDateInterval_(isdaconv.payfreq)
    dateInterval_ = Ref(dateInterval)

    # If the startDate and endDate are not on cycle, then this parameter
    # determines location of coupon dates
    stubtype = JpmcdsStringToStubMethod_(isdaconv.stubtype)
    stubtype_ = Ref(stubtype)

    # Day count convention for coupon payment. Normal is ACT_360
    accrueDCC_ = JpmcdsStringToDayCountConv_(isdaconv.paydcc)

    # Bad day convention for adjusting coupon payment dates
    badDayConv_ = Clong(isdaconv.bdc)

    # upfront charge
    upfrontCharge_ = Cdouble(upfront)

    # recovery rate in case of default
    recovery_ = Cdouble(recovery)

    # pay accrues at startDate
    payAccruedAtStart_ = Cint(1)  # set to true for our purposes

    # Call isda pricer
    onespread_ = Ref{Cdouble}(0.0)

    status = ccall((:JpmcdsCdsoneSpread, :libisda),
                    Cint,
                    (Clong,               # today
                    Clong,                # valueDate (really cashsettle date)
                    Clong,                # benchmarkStartDate
                    Clong,                # stepinDate
                    Clong,                # startDate                
                    Clong,                # endDate                
                    Cdouble,              # couponRate
                    Cint,                 # payAccOnDefault
                    Ref{TDateInterval},   # *dateInterval
                    Ref{TStubMethod},     # *stubType
                    Clong,                # accrueDCC
                    Clong,                # badDayConv
                    Cstring,              # *calendar
                    Ptr{TCurve},          # *discCurve
                    Cdouble,              # upfrontCharge
                    Cdouble,              # recoveryRate
                    Cint,                 # pay accrued at start
                    Ref{Cdouble}),        # *onespread(output) 
                    today_,
                    today_,
                    benchmarkStartDate_,
                    stepin_date_,
                    startdate_,
                    enddate_,
                    cpn_,
                    payacc_ondefault_,
                    dateInterval_,
                    stubtype_,
                    accrueDCC_,
                    badDayConv_,
                    calendar_,
                    ptr_zc,
                    upfrontCharge_,
                    recovery_,
                    payAccruedAtStart_,
                    onespread_)

    if status !=0
    # TODO throw some exception
    end

    return(onespread_.x)
end


