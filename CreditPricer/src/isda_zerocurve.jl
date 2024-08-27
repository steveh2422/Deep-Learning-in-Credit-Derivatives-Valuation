## FILE: isda_zerocurve.jl 

# AUTHOR: Stephen Hope 

# DESCRIPTION: 
# Code for building the zerocurve via the ISDA C Library 



## Methods: isda_build_zero_curve

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Build zero curve from money market, and swap instruments.
# ARGS
#    valuedate::Date :           end of day date
#    ccy::String :               currency
#    ircurve::DataFrame :        data frame of interest rates, tenors and
#                                instrument types {MM, S}
#    [settle_days::Cint = 2] :   settlement days
# RETURNS
#    Returns a pointer to the isda zero cruve
function isda_build_zero_curve(valuedate::Date,
    ccy::String,
    ircurve::DataFrame)

    routine = "isda_build_zero_curve"

    sttldays::Cint = 2

    # Load the ISDA conventions for this currency
    isdaconv = ccy == "EUR" ? _isdaeurconv_ : _isdausdconv_

    # Type conversions 'xyz_' underscore means type ready for ccalls

    # holidays : currently "None" i.e weekends only
    hols_ = isdaconv.holiday

    # value date
    basedate_ = JpmcdsDateFwdThenAdjust_(valuedate,
                sttldays,
                isdaconv.fwdadjbdc,
                hols_)

    # instrument names : array of M or S converted to UInt8
    if typeof(ircurve.type) == Vector{String1}
        inst = Vector{String}(ircurve.type)
        inst = Vector{Char}(join(inst))
    elseif typeof(ircurve.type) == Vector{String}
        inst = Vector{Char}(join(ircurve.type))
    else
        inst = ircurve.type
    end
    inst_ = Vector{UInt8}(inst)

    # number of benchmark instruments
    n_ = Clong(length(inst_))

    # bad day convention
    mmbdc_ = Clong(isdaconv.mmbdc)

    # array of swap dates from the expiries
    #dates_ = Vector{Int64}(undef, n_)
    dates_ = Vector{Clong}(undef, n_)
    for i in 1:n_
        ivl = JpmcdsStringToDateInterval_(ircurve.tenor[i])
        # ISDA convention: MM instruments bus day adjusted but swaps are not
        inst_bdc = Char(inst_[i]) == 'S' ? isdaconv.swapbdc : mmbdc_
        adjdate = JpmcdsDateFwdThenAdjust_(basedate_, ivl, inst_bdc, hols_)
        dates_[i] = adjdate
    end

    # array of swap rates
    rates_ = Vector{Cdouble}(ircurve.rate)

    # DCC of MM instruments
    mmdcc_ = JpmcdsStringToDayCountConv_(isdaconv.mmdcc)

    # DCC of fixed leg
    fixeddcc_ = JpmcdsStringToDayCountConv_(isdaconv.fixeddcc)

    # DCC of floating leg
    floatdcc_ = JpmcdsStringToDayCountConv_(isdaconv.floatdcc)

    # fixed leg frequency
    fixedivl = JpmcdsStringToDateInterval_(isdaconv.fixedfreq)
    fixedfreq_ = JpmcdsDateIntervalToFreq_(fixedivl)

    # float leg frequency
    floativl = JpmcdsStringToDateInterval_(isdaconv.floatfreq)
    floatfreq_ = JpmcdsDateIntervalToFreq_(floativl)



    # Now build the zero curve
    # C definition in zerocurve.h
    out_ptr = ccall((:JpmcdsBuildIRZeroCurve, :libisda),
                    Ptr{TCurve},      # return ptr
                    (Clong,           # valueDate
                    Ref{UInt8},       # *instrNames
                    Ref{Clong},       # *dates    assign memory in Julia
                    Ref{Cdouble},     # *rates
                    Clong,            # nInstr
                    Clong,            # mmDCC
                    Clong,            # fixedSwapFreq
                    Clong,            # floatSwapFreq
                    Clong,            # fixedSwapDCC
                    Clong,            # floatSwapDCC
                    Clong,            # badDayConv
                    Cstring),         # holidayFile
                    basedate_,
                    inst_,
                    dates_,
                    rates_,
                    n_,
                    mmdcc_,
                    fixedfreq_,
                    floatfreq_,
                    fixeddcc_,
                    floatdcc_,
                    mmbdc_,
                    hols_)


    return(out_ptr)

end


## Methods: isda_df

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    isda calculated discount factor
# ARGS
#    ptr_zc::Ptr{TCurve} :   pointer to isda zero curve
#    date::Date :            date
# RETURNS
#    discount factor value Float64
function isda_df(ptr_zc::Ptr{TCurve}, date::Date)::Float64

    date_utd = Clong(Dates.value(date))
    df = isda_df(ptr_zc, date_utd)
    return(df)
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    isda calculated discount factor
# ARGS
#    ptr_zc::Ptr{TCurve} :    pointer to isda zero curve
#    dates::Array{Date,1} :   array of dates
# RETURNS
#    array of discount factors corresponding to each date
function isda_df(ptr_zc::Ptr{TCurve}, dates::Array{Date,1})::Vector{Float64}

    num_dates = length(dates)
    dfs = Vector{Float64}(undef, num_dates)
    for i = 1:num_dates
        date_utd = Clong(Dates.value(dates[i]))
        df = isda_df(ptr_zc, date_utd)
        dfs[i] = df
    end

    return(dfs)
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    isda calculated discount factor
# ARGS
#    ptr_zc::Ptr{TCurve} :    pointer to isda zero curve
#    dates::Array{String,1} : array of dates as strings 
# RETURNS
#    array of discount factors corresponding to each date
function isda_df(ptr_zc::Ptr{TCurve}, dates::Array{String,1}, fmt="yyyy-mm-dd")::Vector{Float64}

    date_array = Date.(dates, fmt)
    dfs = isda_df(ptr_zc, date_array)
    return(dfs)
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    isda calculated discount factor
# ARGS
#    ptr_zc::Ptr{TCurve} :    pointer to isda zero curve
#    dates::Array{Clong,1} :  array of dates as UTD Clong
# RETURNS
#    array of discount factors corresponding to each date
function isda_df(ptr_zc::Ptr{TCurve}, dates::Array{Clong,1})::Vector{Float64}

    num_dates = length(dates)
    dfs = Vector{Float64}(undef, num_dates)
    for i = 1:num_dates
        date_utd = Clong(dates[i])
        df = isda_df(ptr_zc, date_utd)
        dfs[i] = df
    end

    return(dfs)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
#    isda calculated discount factor
# ARGS
#    ptr_zc::Ptr{TCurve} :   pointer to isda zero curve
#    date::Clong :           UTD date Clong
# RETURNS
#    discount factor value Float64
function isda_df(ptr_zc::Ptr{TCurve}, date_utd::Clong)
    # C definition in cxzerocurve.h
    #date_utd_ = convert(Clong, date_utd)
    df = ccall((:JpmcdsZeroPrice, :libisda),
                Cdouble,
                (Ptr{TCurve},
                Clong),
                ptr_zc,
                date_utd)
    return(df)
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Create a dataframe of ptrs to zero curves from the DictoVec of ircurve dataframes 
# ARGS
#    ircurves:Dict{String, DataFrame}  data frames of the IRcurves
#    endofdaydate:Date  end of day date
# RETURNS
#    dataframe of ptr_zc for each ccy 

function cache_ir_curves(ircurves::Dict{String, DataFrame}, endofdaydate::Date)

    # Make a dataframe of ptrs_zc for each ccy from the ircurves dataframes
    ir_ptrs = DataFrame()
    ir_ptrs.ccy = collect(keys(ircurves))
    ir_ptrs.ptr_zc = fill(Ptr{TCurve}(C_NULL), nrow(ir_ptrs))

    for i in 1:nrow(ir_ptrs)
        ccy = ir_ptrs[i, :ccy]
        ir_ptrs[i, :ptr_zc] = isda_build_zero_curve(endofdaydate, ccy, ircurves[ccy])
    end

    rename!(ir_ptrs, [:ccy] .=> [:currency])
    return(ir_ptrs)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Make a dataframe of ccy & ptr_zc from the ircurve dataframe of a single currency
# ARGS
#    ircurve:   dataframe of the {type, tenor, rate} 
#    endofdaydate:  end of day date
# RETURNS
#    dataframe of ptr_zc for a single ccy
function cache_ir_curves(ircurve::DataFrame, endofdaydate::Date, ccy::String)

    # Make a dataframe of ptr_zc from the ircurve dataframe of a single currency
    ir_ptrs = DataFrame()
    ir_ptrs.ccy = [ccy]
    ir_ptrs.ptr_zc = fill(Ptr{TCurve}(C_NULL), nrow(ir_ptrs))
    ir_ptrs[ir_ptrs.ccy .== ccy, :ptr_zc] .= isda_build_zero_curve(endofdaydate, ccy, ircurve)
    rename!(ir_ptrs, [:ccy] .=> [:currency])
    return(ir_ptrs)
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Make a dictionary to flat IR_CURVE data frames 
# ARGS
#    ir:float   flat interest rate 
# RETURNS
#    dataframe of ptr_zc for a single ccy
function create_flat_ir_curves(ir)
    # Define the structure
    eur_tenors = ["1M", "3M", "6M", "1Y", "2Y", "3Y", "4Y", "5Y", "6Y", "7Y", "8Y", "9Y", "10Y", "12Y", "15Y", "20Y", "30Y"]
    usd_tenors = ["1M", "3M", "6M", "1Y", "2Y", "3Y", "4Y", "5Y", "6Y", "7Y", "8Y", "9Y", "10Y", "12Y", "15Y", "20Y", "25Y", "30Y"]
    # Create DataFrames
    eur_df = DataFrame(type = vcat(repeat(["M"], 3), repeat(["S"], 14)), tenor = eur_tenors, rate = fill(ir, length(eur_tenors)))
    usd_df = DataFrame(type = vcat(repeat(["M"], 3), repeat(["S"], 15)), tenor = usd_tenors, rate = fill(ir, length(usd_tenors)))
    # Create the final structure
    ir_curves = Dict("EUR" => eur_df, "USD" => usd_df)
    
    return ir_curves
end
