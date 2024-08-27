## FILE: date.jl 

# AUTHOR: Stephen Hope 

# DESCRIPTION: 
# Code for date utility functions  


# USEFUL INFO
# Convert an Int64 to Date type like this - clumsy but no convert method yet
# Date(Dates.UTD(735685))


function date_intervals(futuredate::Date, pastdate::Date)

    out = futuredate - pastdate
    out = out.value / 365
    return(out)
end



# PRIVATE: NO DOC STRING
# DESCRIPTION
#   Determine the reference expiry in order to control ATM calendar arbitrages 
# ARGS
#   expiry_dates::Vector{Date} : expiry dates vector of size 1-3
# RETURNS
#    reference expiry date

# Don't add ::Type to argument or groupby used with transform complains about ::SubArray etc
function get_ref_expiry(expiry_dates)

    # WARNING: this function's logic is not valid if we start clearing more than 3 expiries 

    imm_months = Int64[3,6,9,12]

    # extract the expiry months from the input 
    expiry_months = Dates.month.(expiry_dates)

    # How many dates provided as an input ?
    n = length(expiry_dates)

    # If only one expiry date available, then return it
    if n == 1
        out = expiry_dates[1]

    # If 2 dates available, then either return imm if available, or the max
    elseif n == 2
        refmonth = intersect(expiry_months, imm_months)
        if length(refmonth) == 0
            out = maximum(expiry_dates)
        else
            out = expiry_dates[findall(expiry_months .== refmonth)][1]
        end

    # If 3 dates, then certainly one of them is imm
    elseif n == 3
        refmonth = intersect(expiry_months, imm_months)
        out = expiry_dates[findall(expiry_months .== refmonth)][1]
    else
        @error "this function must be passed 1-3 dates : passed $n"
    end

    return(out)
end

# If only 1 date selected per red9 by calling code groupby - this function may be called with a single date
# rather than a Vector{Date}. So in that case just retrn the date as for n == 1 case above
function get_ref_expiry(expiry_dates::Date) 
    return(expiry_dates)
end


## Methods: make_date_array

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Helper function to create an array of dates i.e Array{Date,1}
#    Useful for when the built in Date range functionality is of no use
#    because we need date increments of varying amounts
# ARGS
#    year::Array{Int64} :    vector of years
#    month::Array{Int64} :   vector of months
#    day::Array{Int64}) :    vector of days
# RETURNS
#    Array{Date,1}

function make_date_array(year::Array{Int64},
                         month::Array{Int64},
                         day::Array{Int64})

    num_dates = length(year)
    dates = Array{Date,1}(undef, num_dates)

    for i in 1:num_dates
        dates[i] = Date(year[i], month[i], day[i])
    end

    return(dates)
end

## Methods: year_frac

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Calculates the date interval in years for a given day count method
# ARGS
#    start_date::Date :      Beginning of period
#    end_date::Date :        End of period
#    [dcc::String] :         (Opt) Day count convention "ACT/360"(default)
# RETURNS
#    year fraction Float64
function year_frac(start_date::Date, end_date::Date; dcc::String = "ACT/360")

    dcc = uppercase(dcc)
    num = end_date - start_date
    if (dcc == "ACT/360")
        denom = 360.0
    elseif (dcc == "ACT/365")
        denom = 365.0
    end

    return(num.value / denom)
    #return(((num.value)+1) / denom)
end

## Methods: get_imm_date

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Calculate the Credit IMM date for a given set of dates
# ARGS
#    refdates::Array{Date,1} :  vector of reference dates
#    request::String :          request type: "N" next IMM, "P" previous IMM
# RETURNS
#    array of IMM dates Array{Date,1}
function get_imm_date(refdates::Array{Date,1}, request::String)

    num_dates = length(refdates)
    out = Array{Date,1}(undef, num_dates)
    for i in 1:num_dates
        out[i] = get_imm_date(refdates[i], request)
    end

    return(out)

end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Calculate the Credit IMM date for a given date
# ARGS
#    refdate::Date :    reference date
#    request::String :  request type: "N" next IMM, "P" previous IMM
# RETURNS
#    single IMM date tpye Date
function get_imm_date(refdate::Date, request::String)

    cat = uppercase(request)
    immday = 20
    immmonths = [3,6,9,12]

    # get ref day, month and year
    refday = day(refdate)
    refmonth = month(refdate)
    refyear = year(refdate)

    # Determine request
    if cat == "N"
        nextimm = true
        # Check if the next IMM is the 20th March of the next year
        out = refdate ≥ Date(refyear, 12, 20) ?
                        Date(refyear + 1, 3, 20) : nothing
        if isnothing(out)  # Then IMM is within current year
            for i in immmonths
                if refdate < Date(refyear, i, immday)
                    out = Date(refyear, i, immday) ; break
                end
            end
        end
    elseif cat == "P"
        nextimm = false
        # Check if the prev IMM is the 20th Dec of last year
        out = refdate < Date(refyear, 3, 20) ?
                        Date(refyear - 1, 12, 20) : nothing
        if isnothing(out)  # Then IMM is within current year
            for i in reverse(immmonths)
                if refdate > Date(refyear, i, immday)
                    out = Date(refyear, i, immday) ; break
                end
            end
        end
    else
        @error "request type must be either N or P : passed $request"
    end

    return(out)
end


## Methods: next_imm_date

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Wrapper to get_imm_date with request set to "N" for next
# ARGS
#    refdate::Date :  reference date
# RETURNS
#    single IMM date type Date
function next_imm_date(refdate::Date)
    # set request to "N" for next
    return(get_imm_date(refdate, "N"))
end


## Methods: prev_imm_date

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Wrapper to get_imm_date with request set to "P" for previous
# ARGS
#    refdate::Date :  reference date
# RETURNS
#    single IMM date type Date
function prev_imm_date(refdate::Date)
    # set request to "N" for next
    return(get_imm_date(refdate, "P"))
end


## Methods: split_tenor_term

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Split standard ternor into numeric and string parts e.g. "5Y" => 5 and "Y"
# ARGS
#    tenor::String :   standard tenor e.g "1M"
# RETURNS
#    tuple(number::Int64, ten::String)
function split_tenor_term(tenor::String)

    num = parse(Int64, split(tenor, r"[aA-zZ]+")[1])
    ten = split(tenor, r"[0-9]+")[2]

    return(number = num, string = ten)
end


## Methods: get_tenor_term

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Convert standard ternor into term maturity in years
# ARGS
#    tenor::String :   standard tenor e.g "1M"
# RETURNS
#    fraction Float64
function get_tenor_term(tenor::String)

    tenor_dict = Dict("Y" => 1, "M" => 12, "D" => 360)
    num = parse(Int64, split(tenor, r"[aA-zZ]+")[1])
    ten = tenor_dict[split(tenor, r"[0-9]+")[2]]

    return(num / ten)
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Convert standard ternors into term maturity in years
# ARGS
#    tenor::Array{String,1} :   vector of tenors e.g {"1M", "3M", "5Y" ...}
# RETURNS
#    array of tenor terms 
function get_tenor_term(tenors::Array{String,1})

    num_tenors = length(tenors)
    tenor_terms = Array{Float64,1}(undef, num_tenors)
    for i in 1:num_tenors
        tenor_terms[i] = get_tenor_term(tenors[i])
    end
    return(tenor_terms)
end

## Methods: benchmark_end_date

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Calculate benchmark maturity dates from a trade dates and tenor
# ARGS
#    trade_dates::Array{Date,1} :  vector of trade dates
#    [tenor::String] :             (Opt) tenor
# RETURNS
#    array of end dates Array{Date,1}
function benchmark_end_date(trade_dates::Array{Date,1}; tenor::String = "0M")

    num_dates = length(trade_dates)
    out = Array{Date,1}(undef, num_dates)
    ten = tenor
    for i in 1:num_dates
        out[i] = benchmark_end_date(trade_dates[i], tenor=ten)
    end

    return(out)
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Calculate benchmark maturity date from a trade date and tenor
# ARGS
#    trade_date::Date :  trade dates
#    [tenor::String] :   (Opt) tenor
# RETURNS
#    single benchmark eend date of type Date
function benchmark_end_date(trade_date::Date; tenor::String = "0M")

    trade_year = year(trade_date)

    year_mar20 = Date(trade_year, 3,20)
    year_jun20 = Date(trade_year, 6,20)
    year_sep20 = Date(trade_year, 9,20)
    year_dec20 = Date(trade_year, 12,20)

    # Adjustment of 3 months if the trade date does NOT fall between
    # 20-Mar and 19-Jun or 20-Sep and 19-Dec
    adj = !( (trade_date ≥ year_mar20 && trade_date < year_jun20) ||
        (trade_date ≥ year_sep20 && trade_date < year_dec20))

    out = next_imm_date(trade_date) +
            Dates.Month(round(get_tenor_term(tenor)* 12.0) - 3*adj)

    return(out)
end



## Methods: get_spread_maturities

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Return absolute dates to which relative spread tenors correspond
#    Helper wrapper for type Singlename using the tenor set
#    ["1Y","3Y","5Y","7Y","10Y"]
# ARGS
#    keydate::Date :          reference date to which tenors are relative
#    tradetype::Singlename :  _singlename_ singleton
# RETURNS
#    maturities for standard single name tenors
function get_spread_maturities(keydate::Date, tradetype::Singlename)
    # Current tenors used in pricing for singlename
    tenors = ["1Y","3Y","5Y","7Y","10Y"]
    return(get_spread_maturities(keydate, tenors))
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Return absolute dates to which relative spread tenors correspond
#    Helper wrapper for type Index using the tenor set
#    ["6M","1Y","2Y","3Y","4Y","5Y","7Y","10Y"]
# ARGS
#    keydate::Date :     reference date to which tenors are relative
#    tradetype::Index :  _index_ singleton
# RETURNS
#    maturities for standard index tenors
function get_spread_maturities(keydate::Date, tradetype::Index)
    # Current tenors used in pricing for index
    tenors = ["6M","1Y","2Y","3Y","4Y","5Y","7Y","10Y"]
    return(get_spread_maturities(keydate, tenors))
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Return absolute dates to which relative spread tenors correspond
#    Helper wrapper for type Indexflat using a fixed tenor of 5Y
# ARGS
#    keydate::Date :             reference date to which tenors are relative
#    tradetype::Indexflat :      _indexflat_ singleton
# RETURNS
#    5Y maturity for index flat
function get_spread_maturities(keydate::Date, tradetype::Indexflat)
    # tenor used in pricing for indexflat
    tenors = ["5Y"]
    return(get_spread_maturities(keydate, tenors))
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Return absolute dates to which relative spread tenors correspond
# ARGS
#    keydate::Date :             reference date to which tenors are relative
#    tenors::Array{String,1} :   vector of standard tenors
# RETURNS
#    Dataframe of tenors and maturities
function get_spread_maturities(keydate::Date, tenors::Array{String,1})

    num_tenors = length(tenors)
    maturities = Array{Date,1}(undef, num_tenors)
    df = DataFrame()

    for i in 1:num_tenors
        split = split_tenor_term(tenors[i])
        num = split.number
        ten = split.string

        if ten == "D"
            mat = keydate + Dates.Day(num)
        elseif ten == "M"
            mat = keydate + Dates.Month(num)
        elseif ten == "Y"
            mat = keydate + Dates.Year(num)
        end
        maturities[i] = mat
    end
    df.tenor = tenors
    df.maturity = maturities

    return(df)
end
