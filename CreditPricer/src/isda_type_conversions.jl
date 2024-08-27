
## FILE: isda_type_conversions.jl 

# DESCRIPTION
# Code for converting Julia types to C types
# Some coding conventions:
# variables created ready to be used in ccall are appended with '_'
# e.g.
# Julia valuedate::Date becomes valuedate_ once converted to an Clong
# Similarly for clarity...
# when calling ISDA lib functions Jpmcdsxyz... 
# chosen convention is to wrap the Jpmcdsxyz in a Julia function. 
# If leaving the name the same as the ISDA name then append "_"
# e.g.
# function JpmcdsDtFwdAny_()
#    ccall((:JpmcdsDtFwdAny, :libisda)), etc ...
#    etc ...
#    return(ccall_compatible_type)
# end
#
# Standard Julia functions follow the usual lower case rules with "_"
# between words for readability where deemed appropriate


## BUILD INSTRUCTIONS
# Interface to the ISDA CDS Model functionality
# Julia calls functions in
# libisda.dylib (MACOSX)
# libisda.so (LINUX)
# libisda.dll (WIN)

# LINUX / MACOSX 
# To build libisda.so
# Download ISDA C libraries from https://www.cdsmodel.com/cdsmodel/download.html
# navigate to .../isda_cds_model_c_v1.8.2/lib/build/linux
# execute 'make'  > make
# libcds.so is created --> rename it to libisda.so  / .dylib 
# drop libisda.so into the replicjpricer root directory (no headers required)
# ... nothing else to do, you can now start using 'ccall' functionality

## TYPE MAPPING INSTRUCTIONS
# https://docs.julialang.org/en/v1/manual/calling-c-and-fortran-code/
# reproduced here for lookup convenience

# C Type           Julia Alias    Julia Type
# ------           -----------    ----------

# unsigned char		  Cuchar	    UInt8
# bool 		          Cuchar	    UInt8  Note JPMs TBoolean is Cint
# short		          Cshort	    Int16
# unsigned short	  Cushort	    UInt16
# int              	  Cint	        Int32
# unsigned int		  Cuint	        UInt32
# long long	     	  Clonglong	    Int64
# long int            Clong         Int32
# unsigned long long  Culonglong	UInt64
# intmax_t		      Cintmax_t	    Int64
# uintmax_t		      Cuintmax_t	UInt64
# float	         	  Cfloat	    Float32
# double		      Cdouble	    Float64
# complex float		  ComplexF32	Complex{Float32}
# complex double	  ComplexF64	Complex{Float64}
# ptrdiff_t		      Cptrdiff_t	Int
# ssize_t		      Cssize_t	    Int
# size_t		      Csize_t	    UInt
# void			                    Cvoid
# void*                			    Ptr{Cvoid}
# struct T                          T where T is a Julia leaf type (struct)
# struct T*                         Ptr{T} where T is the corresponding
#                                   struct in Julia
# T*                                Ref{T}
# (T represents
# an appropriately
# defined type)
# char*                            Cstring if NUL-terminated,
#                                  or Ptr{UInt8} if not
# (or char[]
# e.g. a string)
# char** (or *char[])			   Ptr{Ptr{UInt8}}
# T[] or T*                        Ptr{T}
# enum                             Cint
# a_arg			                   Not supported

# C functions declared to return Cvoid will return the value nothing in Julia

# ISDA Library usefule typedefs
#
# JPM typedef     C Type     Julia Type
# -----------     ------     ----------
# TDate           long int   Clong



# Use Ref{T}    For C code accepting pointers
# Use Ptr{T}    For C code returning ptrs or ptrs that will be filled by C
# Use Ptr{T}    Pointers contained in C structs should be represented as fields
#               of type Ptr{T} within the corresponding Julia struct types
#               designed to mimic the internal structure of the corresponding
#               C structs.
# Example
# width = Ref{Cint}(0)
# range = Ref{Cfloat}(0)
# ccall(:foo, Cvoid, (Ref{Cint}, Ref{Cfloat}), width, range)
# Also see C Wrapper examples section in url above ...

##


# Julia struct types to mirror the corresponding C structs
# If C returns a ptr to a struct that will be passed to another C function
# then you don't need to define the internals of the struct in Julia
# see (TCurve)

# C definition in bastypes.h
mutable struct TRatePt
    fDate::Clong      # equivalent to TDate
    fRate::Cdouble    # equivalent to double 
end

# C definition in bastypes.h
mutable struct TCurve 
    fNumItems::Cint
    fArray::Ptr{TRatePt}
    fBaseDate::Clong
    fBasis::Cdouble
    fDayCountConv::Clong
end

# C definition in bastypes.h
#mutable struct TCurve end

# C definition in stub.h
mutable struct TStubMethod
    stubAtEnd::Cint  # equivalent to TBoolean
    longStub::Cint   # equivalent to TBoolean
end

# C definition in cdate.h
mutable struct TDateInterval
    prd::Cint
    prd_type::Cchar
    flag::Cint
end


## Methods: jpm_bad_day

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Maps bad day conventions of the form {"N", "M", "P", "F"}
#    to the JPM integer forms defined in busday.h as #define long
#    JPMCDS_BAD_DAY_NONE  78
#    JPMCDS_BAD_DAY_MODIFIED 77
#    JPMCDS_BAD_DAY_PREVIOUS 80
#    JPMCDS_BAD_DAY_FOLLOWING 70
#    This function exists as no Jpmcdsxyz function to create them
# ARGS
#    bdc::Char : bad day convention as one of {"N", "M", "P", "F"}
function jpm_bad_day(bdc::Char)::Clong
    # JPM definitions - no C function call to create these from a Julia String!
    # That I have noticed anyway ...
    jpmbdc = Dict('N' => Clong('N'),  # JPMCDS_BAD_DAY_NONE
                  'M' => Clong('M'),  # JPMCDS_BAD_DAY_MODIFIED
                  'P' => Clong('P'),  # JPMCDS_BAD_DAY_PREVIOUS,
                  'F' => Clong('F'))  # JPMCDS_BAD_DAY_FOLLOWING

    try
        bdc_ = jpmbdc[bdc]
        return(bdc_)
    catch e
        @error "bad day convention $bdc not recognised!"
    end
end



## Methods: JpmcdsDtFwdAny_

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Wrapper to isda function JpmcdsDtFwdAny (ldate.h)
#    adjusts date forward, no holidays
# ARGS
#    basedate::Date :  date to adjust
#    ivl::Cint :       interval
#    [tenor::Char] :   tenor of interval defaults to 'D'
# RETURNS
#   adjusted date UTD Clong
function JpmcdsDtFwdAny_(basedate::Date, ivl::Cint, tenor::Char = 'D')::Clong
    # No memory management in Julia required for this function

    out_date_ = Ref{Clong}(0)
    ivl_ = TDateInterval(ivl,Cchar(tenor),Cint(0))
    basedate_ = Clong(Dates.value(basedate))

    status = ccall((:JpmcdsDtFwdAny, :libisda),
                    Clong,
                    (Clong, Ref{TDateInterval}, Ref{Clong}),
                    basedate_, ivl_, out_date_)

    if status !=0
        # TODO throw some exception
    end

    # dereference the reference to the object that C modified
    out_ = out_date_.x

    return(out_)
end



## Methods: JpmcdsDateFwdThenAdjust_

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Wrapper to isda function JpmcdsDateFwdThenAdjust (ldate.h)
#    adjusts date forward, taking into account holidays and DCC
# ARGS
#    basedate::Clong:    UTD date to adjust Clong
#    ivl::Cint :         interval
#    bdc::Clong :        bad day converted already adjusted via jpm_bad_day()
#    holidays::String :  holiday file - usually "None"
#    [tenor::Char] :     tenor of interval defaults to 'D'
# RETURNS
#    adjusted date UTD Clong
function JpmcdsDateFwdThenAdjust_(basedate::Clong,
                                  ivl::Cint,
                                  bdc::Clong,
                                  holidays::String,
                                  tenor::Char = 'D'  )::Clong

    #basedate_asdate = Date(Dates.UTD(basedate))
    return(JpmcdsDateFwdThenAdjust_(basedate, ivl, bdc, holidays))
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Wrapper to isda function JpmcdsDateFwdThenAdjust (ldate.h)
#    adjusts date forward, taking into account holidays and DCC
# ARGS
#    basedate::Date:      date to adjust type Date
#    ivl::Cint :          interval
#    bdc::Clong :         bad day converted already adjusted via jpm_bad_day()
#    holidays::String :   holiday file - usually "None"
#    [tenor::Char] :      tenor of interval defaults to 'D'
# RETURNS
#    adjusted date UTD Clong
function JpmcdsDateFwdThenAdjust_(basedate::Date,
                                  ivl::Cint,
                                  bdc::Clong,
                                  holidays::String;
                                  tenor::Char = 'D' )::Clong
    # No memory management in Julia required for this function

    out_date_ = Ref{Clong}(0)
    basedate_ = Clong(Dates.value(basedate))
    ivl_ = TDateInterval(ivl,Cchar(tenor),Cint(0))
    bdc_ = bdc
    holidays_ = holidays # no conversion necessary it seems a Cstring will be assumed

    status = ccall((:JpmcdsDateFwdThenAdjust, :libisda),
                    Clong,
                    (Clong, Ref{TDateInterval}, Clong, Cstring, Ref{Clong}),
                    basedate_, ivl_, bdc_, holidays_, out_date_)

    if status !=0
        # TODO throw some exception
    end

    # dereference the reference to the object that C modified
    out_ = out_date_.x

    return(out_)
end

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Wrapper to isda function JpmcdsDateFwdThenAdjust (ldate.h)
#    adjusts date forward, taking into account holidays and DCC
# ARGS
#    basedate::Clong:     UTD date to adjust Clong
#    ivl::TDateInterval : interval type TDateInterval
#    bdc::Clong :         bad day converted already adjusted via jpm_bad_day()
#    holidays::String :   holiday file - usually "None"
# RETURNS
#    adjusted date UTD Clong
function JpmcdsDateFwdThenAdjust_(basedate::Clong,
                                  ivl::TDateInterval,
                                  bdc::Clong,
                                  holidays::String)::Clong
    # No memory management in Julia required for this function

    out_date_ = Ref{Clong}(0)
    basedate_ = basedate
    ivl_ = ivl
    bdc_ = bdc
    holidays_ = holidays

    status = ccall((:JpmcdsDateFwdThenAdjust, :libisda),
                    Clong,
                    (Clong, Ref{TDateInterval}, Clong, Cstring, Ref{Clong}),
                    basedate_, ivl_, bdc_, holidays_, out_date_)

    if status !=0
        # TODO throw some exception
    end

    # dereference the reference to the object that C modified
    out_ = out_date_.x

    return(out_)
end



## Methods: JpmcdsStringToDateInterval_

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Wrapper to isda function JpmcdsStringToDateInterval (convert.h)
# ARGS
#    str::String :   string ivl to convert e.g.  1Y, 3M, 4D, etc 
# RETURNS
#    interval struct TDateInterval
function JpmcdsStringToDateInterval_(str::String)::TDateInterval
    # No memory management in Julia required for this function

    # conversion to Cstring for NUL-terminates strings is implicit 
    instr_ = str  
    out_ivl_ = TDateInterval(Cint(0),Cchar('0'),Cint(0))   # Empty TDateInterval: mod by C

    status = ccall((:JpmcdsStringToDateInterval, :libisda),
                    Cint,
                    (Cstring, Cstring, Ref{TDateInterval}),
                    instr_, "JpmcdsStringToDateInterval_", Ref(out_ivl_))

    if status !=0
        # TODO throw some exception
    end

    return(out_ivl_)
end


## Methods: JpmcdsStringToDayCountConv_

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Wrapper to isda function JpmcdsStringToDayCountConv (yearfrac.h)
#    String dcc e.g. "Act/360" converted to JPM DCC Clong
# ARGS
#    dcc::String :   str dcc 
# RETURNS
#    JPM DCC Clong
function JpmcdsStringToDayCountConv_(dcc::String)::Clong
    # No memory management in Julia required for this function
    in_dcc_ = dcc
    out_dcc = Ref{Clong}(0)
    status = ccall((:JpmcdsStringToDayCountConv, :libisda),
                    Cint,
                    (Cstring, Ref{Clong}),
                    in_dcc_, out_dcc)

    if status !=0
        # TODO throw some exception
    end

    # dereference the reference to the object that C modified
    out_dcc_ = out_dcc.x

    return(out_dcc_)
end


## Methods: JpmcdsStringToStubMethod_

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Wrapper to isda function JpmcdsStringToStubMethod (stub.h)
#    String to stub method
# ARGS
#    stub::String :   stub as string e.g. "F/S"
# RETURNS
#    stubmethod struct TStubMethod
function JpmcdsStringToStubMethod_(stub::String)::TStubMethod
    # No memory management in Julia required for this function
    instub_ = stub
    stubmethod_ = TStubMethod(Cint(0),Cint(0))

    status = ccall((:JpmcdsStringToStubMethod, :libisda),
                    Cint,
                    (Cstring, Ref{TStubMethod}),
                    instub_, stubmethod_)

    if status !=0
        # TODO throw some exception
    end

    return(stubmethod_)
end


## Methods: JpmcdsDateIntervalToFreq_

# PRIVATE: NO DOC STRING
# DESCRIPTION
#    Wrapper to isda function JpmcdsDateIntervalToFreq (date_sup.h)
#    date interval to payment frequency
# ARGS
#    ivl::TDateInterval :    interval
# RETURNS
#    payment frequency Cdouble:   times per year 
function JpmcdsDateIntervalToFreq_(ivl::TDateInterval)::Cdouble
    # No memory management in Julia required for this function
    ivl_ = ivl
    out_freq_ = Ref{Cdouble}(0)

    status = ccall((:JpmcdsDateIntervalToFreq, :libisda),
                    Cint,
                    (Ref{TDateInterval}, Ref{Cdouble}),
                    ivl_,
                    out_freq_)

    if status !=0
        # TODO throw some exception
    end

    # dereference the reference to the object that C modified
    out_ = out_freq_.x

    return(out_)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
#    creates the isda error file for logging errors
#    Tells JpmcdsErrMsg to write to the provided file name.
#    Routine opens the file if doesn't exist; for appending
# ARGS
#    filename::String :    full path of file with no extension attached
# RETURNS
#    nothing
function JpmcdsErrMsgFileName_(filename::String)
    status = ccall((:JpmcdsErrMsgFileName, :libisda),
                    Cint,
                    (Cstring, Cint),
                    filename, 1)

    if status !=0
        # TODO throw some exception
    end
    return(nothing)
end


# PRIVATE: NO DOC STRING
# DESCRIPTION
# Tells JpmcdsErrMsg to actually write something (either to log file or to
# standard out, depending on how USE_PRINTF is set.)
# ARGS
#    void:   pass Cvoid
# RETURNS
#    nothing
function JpmcdsErrMsgOn_()
    status = ccall((:JpmcdsErrMsgOn, :libisda),
                    Cvoid,
                    ())
                    

    if status !=0
        # TODO throw some exception
    end
    return(nothing)
end


