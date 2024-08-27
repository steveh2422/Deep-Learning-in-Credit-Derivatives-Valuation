## FILE: CreditPricer.jl 

# DESCRIPTION: 
# Creditpricer module definitions and support 


module CreditPricer

# MODULE DEPENDENCIES 
# To add more, enter package manager in CreditPricer
# julia> ] 
# pkg> activate . 
# pkg> add XYZ
# then package appears in project.toml  
using BusinessDays 
using CSV
using Dates
using DataFrames
using DataFramesMeta
using Distributions
using GLM 
using Markdown
using Pipe
using QuadGK
using RData
using Roots
using StatsBase
using Suppressor


## CONFIG DEFAULT VALUES  (pricing config) 
_config_ = Dict([
    (:LOGGING, false),
    (:MULTITHREADING, false),                # switch on multithreading for sn & index
    (:NUMERICAL_INT, "simpson"),             # {"simpson" or "quadgk"} quadgk doesn't work with multithreading
    (:OPTION_MULTITHREADING, false),          # switch on option multithreading 
    (:OPTION_MODEL, "numerical_int"),        # {"numerical_int" or "taylor"}
    (:SN_MODEL, "isda"),                     # {"isda" or "approx"}
    (:SN_EXPERIMENTAL_OPTIMISATION, false),  # WIP 
    (:INDEX_MODEL, "isda"),                  # {"isda" or "approx"}
    (:APPROX_MODEL, "hazard_curve")          # {"hazard_curve" or "interp_spread"}
])

# ISDA IR CONVENTIONS (for a given currency)
struct ISDAirconv
    holiday::String    # holiday file
    mmbdc::Clong       # money market bad day convention
    swapbdc::Clong     # swap bad day convention
    fwdadjbdc::Clong   # value date adjustment bad day convention
    mmdcc::String      # money market day count convention
    floatdcc::String   # float leg day count convention
    fixeddcc::String   # fixed leg day count convention
    floatfreq::String  # float leg day count convention
    fixedfreq::String  # fixed leg day count convention
end

# ISDA PRICING CONVENTIONS
struct ISDApricerconv
    cash_settle_days::Cint  # trade settlement as of value date
    stepin_days::Cint       # protection start as of value date
    payacc_ondefault::Bool  # flag for accrued interest payment on default
    payfreq::String         # fee leg payment interval
    paydcc::String          # day count convention for periodic fees
    stubtype::String        # short or partial periods : Front/Short
    bdc::Clong              ## bad day convention JPMCDS_BAD_DAY_FOLLOWING
end

const _isdapricerconv_ = ISDApricerconv(3,1,true,"Q","Act/360","F/S",70)
# RFR cross over date 2022-04-04
# 77 is MODIFIED 'M'
const _isdaeurconvpreRFR_ = ISDAirconv("None", 77, 77, 77, "Act/360", "Act/360", "30/360", "6M", "1Y")
const _isdausdconvpreRFR_ = ISDAirconv("None", 77, 77, 77, "Act/360", "Act/360", "30/360", "3M", "6M")
const _isdaeurconv_ = ISDAirconv("None", 77, 77, 77, "Act/360", "Act/360", "Act/360", "1Y", "1Y")
const _isdausdconv_ = ISDAirconv("None", 77, 77, 77, "Act/360", "Act/360", "Act/360", "1Y", "1Y")

# TRADE SINGLETONS (for multiple dispatch control)
struct Singlename end; const _singlename_ = Singlename()
struct Index end; const _index_ = Index()
struct Indexflat end; const _indexflat_ = Indexflat()
struct Option end; const _option_ = Option()
struct SinglenameApprox end; const _singlenameapprox_ = SinglenameApprox()
struct IndexApprox end; const _indexapprox_ = IndexApprox()

# MEASURE SINGLETONS (for multiple dispatch control)
struct MtM end ; const _mtm_ = MtM()
struct CS01 end; const _cs01_ = CS01()
struct Delta end; const _delta_ = Delta()
struct Theta end; const _theta_ = Theta()    
struct Vega end; const _vega_ = Vega()    
struct DeltaStrike end; const _deltastrike_ = DeltaStrike() 
struct DeltaLogm end; const _deltalogm_ = DeltaLogm() 


# EXPORTS
#date
export Date
# trade singletons
export _singlename_, _index_, _option_ 
export _indexflat_, _singlenameapprox_, _indexapprox_
# measure singletons
export _mtm_, _cs01_, _delta_, _theta_, _vega_, _deltastrike_, _deltalogm_ 
# main
export run_measure, load_contracts, create_flat_ir_curves
# pricer files
export pricer!

# INCLUDES
include("isda_type_conversions.jl")
include("date.jl")
include("isda_zerocurve.jl")
include("isda_creditcurve.jl")
include("isda_cds.jl")
include("pricer_sn.jl")
include("pricer_opt.jl")
include("pricer_idx.jl")
include("utilities.jl")

end # module CreditPricer
